"""
NECBL Hitting Stats Scraper - Playwright version
Playwright bundles its own Chromium so no Chrome/ChromeDriver mismatch issues.
"""

import re
import sys
import time
import pandas as pd
from datetime import datetime
from playwright.sync_api import sync_playwright, TimeoutError as PlaywrightTimeout

TEAM_SLUGS = {
    "UPP_VAL": ("Upper Valley Nighthawks",  "UPP", "uppervalleynighthawks"),
    "VAL_BLU": ("Valley Blue Sox",           "VAL", "valleybluesox"),
    "KEE_SWA": ("Keene Swampbats",           "KEE", "keeneswampbats"),
    "BRI_B":   ("Bristol Blues",             "BRI", "bristolblues"),
    "MAR_VIN": ("Martha's Vineyard Sharks",  "MAR", "marthasvineyardsharks"),
    "OCE_STA": ("Ocean State Waves",         "OCE", "oceanstatewaves"),
    "NOR_ADA": ("North Adams Steeplecats",   "NOR", "northadamssteeplecats"),
    "MYS_SCH": ("Mystic Schooners",          "MYS", "mysticschooners"),
    "NEW_GUL": ("Newport Gulls",             "NEW", "newportgulls"),
    "SAN_MAI": ("Sanford Mainers",           "SAN", "sanfordmainers"),
    "VER_MOU": ("Vermont Mountaineers",      "VER", "vermontmountaineers"),
    "DAN_WES": ("Danbury Westerners",        "DAN", "danburywesterners"),
    "NSN":     ("North Shore Navigators",    "NSN", "northshorenavigators"),
}

WOBA_WEIGHTS = {
    "single": 0.888,
    "double": 1.271,
    "triple": 1.616,
    "hr":     2.101,
    "bb_hbp": 0.690,
}

def safe_num(val, default=0):
    try:
        v = re.sub(r"[^0-9.]", "", str(val))
        return float(v) if v else default
    except:
        return default


def scrape_team(page, season, team_code, team_name, team_abbrev, slug):
    url = f"https://www.necbl.com/sports/bsb/{season}/teams/{slug}?view=lineup"
    print(f"  Scraping {team_name} ({season})...", flush=True)

    try:
        page.goto(url, wait_until="domcontentloaded", timeout=45000)
        # Wait for the hitting stats table - look for a th containing AB
        try:
            page.wait_for_selector("th:has-text('AB')", timeout=30000)
        except PlaywrightTimeout:
            # Try scrolling to trigger lazy load
            try:
                page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
                time.sleep(2)
                page.wait_for_selector("th:has-text('AB')", timeout=15000)
            except PlaywrightTimeout:
                print(f"    No AB column found after scroll for {team_name}", flush=True)
                return []
        time.sleep(1)
    except PlaywrightTimeout:
        print(f"    Timeout for {team_name}", flush=True)
        return []
    except Exception as e:
        print(f"    Error loading {team_name}: {e}", flush=True)
        return []

    # Get all tables from the page
    tables = page.query_selector_all("table")
    hitting_tbl = None

    for tbl in tables:
        headers = [th.inner_text().strip().upper() for th in tbl.query_selector_all("th")]
        if "AB" in headers and "H" in headers:
            hitting_tbl = tbl
            break

    if hitting_tbl is None:
        print(f"    No hitting table found for {team_name}", flush=True)
        return []

    headers = [th.inner_text().strip().upper() for th in hitting_tbl.query_selector_all("th")]

    def col(name, *alts):
        for n in [name] + list(alts):
            for i, h in enumerate(headers):
                if h == n or h.startswith(n):
                    return i
        return None

    name_col = col("NAME", "PLAYER")
    ab_col   = col("AB")
    h_col    = col("H")
    d_col    = col("2B")
    t_col    = col("3B")
    hr_col   = col("HR")
    bb_col   = col("BB")
    hbp_col  = col("HBP")
    so_col   = col("K", "SO")

    if name_col is None or ab_col is None or h_col is None:
        print(f"    Missing required columns. Headers: {headers}", flush=True)
        return []

    rows = hitting_tbl.query_selector_all("tr")
    players = []

    for row in rows[1:]:  # skip header
        cells = row.query_selector_all("td, th")
        if len(cells) < 3:
            continue

        def cell(idx):
            if idx is None or idx >= len(cells):
                return ""
            return cells[idx].inner_text().strip()

        raw_name = cell(name_col)
        if not raw_name or len(raw_name) < 2:
            continue
        if re.match(r"^(Total|Opponent|Name|Player|---)", raw_name, re.I):
            continue

        ab_val = safe_num(cell(ab_col))
        if ab_val <= 0:
            continue

        h_val   = safe_num(cell(h_col))
        d_val   = safe_num(cell(d_col))
        t_val   = safe_num(cell(t_col))
        hr_val  = safe_num(cell(hr_col))
        bb_val  = safe_num(cell(bb_col))
        hbp_val = safe_num(cell(hbp_col))
        so_val  = safe_num(cell(so_col))

        singles = max(0, h_val - d_val - t_val - hr_val)
        pa      = ab_val + bb_val + hbp_val
        bip     = max(1, ab_val - so_val)

        if pa <= 0:
            continue

        woba = (
            singles * WOBA_WEIGHTS["single"] +
            d_val   * WOBA_WEIGHTS["double"] +
            t_val   * WOBA_WEIGHTS["triple"] +
            hr_val  * WOBA_WEIGHTS["hr"] +
            (bb_val + hbp_val) * WOBA_WEIGHTS["bb_hbp"]
        ) / pa

        wobacon = (
            singles * WOBA_WEIGHTS["single"] +
            d_val   * WOBA_WEIGHTS["double"] +
            t_val   * WOBA_WEIGHTS["triple"] +
            hr_val  * WOBA_WEIGHTS["hr"]
        ) / bip

        clean = re.sub(r"\s+", " ", raw_name).strip()
        parts = clean.split()
        if len(parts) >= 2:
            first_initial = parts[0][0].upper()
            last_name     = " ".join(parts[1:]).upper()
        elif len(parts) == 1:
            first_initial = "X"
            last_name     = parts[0].upper()
        else:
            continue

        composite_key = f"{last_name}_{first_initial}_{team_abbrev}_{season}"

        players.append({
            "Player":                  raw_name,
            "Last_Name":               last_name,
            "First_Initial":           first_initial,
            "Team":                    team_name,
            "Team_Code":               team_code,
            "Team_Abbrev":             team_abbrev,
            "Season":                  season,
            "AB":                      int(ab_val),
            "H":                       int(h_val),
            "Singles":                 int(singles),
            "Doubles":                 int(d_val),
            "Triples":                 int(t_val),
            "HR":                      int(hr_val),
            "BB":                      int(bb_val),
            "HBP":                     int(hbp_val),
            "SO":                      int(so_val),
            "PA":                      int(pa),
            "Batted_Balls":            int(bip),
            "wOBA":                    round(woba, 3),
            "wOBACON":                 round(wobacon, 3),
            "Player_Team_Season_Key":  composite_key,
        })

    print(f"    Got {len(players)} players", flush=True)
    return players


def main():
    current_year = str(datetime.now().year)
    seasons = [str(y) for y in range(int(current_year), 2020, -1)]
    print(f"Scraping NECBL seasons: {seasons}", flush=True)

    all_rows = []

    with sync_playwright() as p:
        browser = p.chromium.launch(
            headless=True,
            args=["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"]
        )
        context = browser.new_context(
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/120.0.0.0 Safari/537.36"
            )
        )
        page = context.new_page()

        for season in seasons:
            print(f"\n=== Season {season} ===", flush=True)
            for code, (name, abbrev, slug) in TEAM_SLUGS.items():
                try:
                    rows = scrape_team(page, season, code, name, abbrev, slug)
                    all_rows.extend(rows)
                except Exception as e:
                    print(f"    Unexpected error for {name}: {e}", flush=True)
                time.sleep(0.5)

        browser.close()

    if all_rows:
        df = pd.DataFrame(all_rows)
        df.to_csv("necbl_stats.csv", index=False)
        print(f"\nSaved necbl_stats.csv with {len(df)} rows", flush=True)
        print(f"Seasons: {df['Season'].unique().tolist()}", flush=True)
        print(f"Teams:   {df['Team_Code'].nunique()} teams", flush=True)
    else:
        pd.DataFrame(columns=[
            "Player","Last_Name","First_Initial","Team","Team_Code",
            "Team_Abbrev","Season","AB","H","Singles","Doubles","Triples",
            "HR","BB","HBP","SO","PA","Batted_Balls","wOBA","wOBACON",
            "Player_Team_Season_Key"
        ]).to_csv("necbl_stats.csv", index=False)
        print("No data scraped - wrote empty necbl_stats.csv", flush=True)
        sys.exit(1)


if __name__ == "__main__":
    main()
