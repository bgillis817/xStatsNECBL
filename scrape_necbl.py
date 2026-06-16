"""
NECBL Hitting Stats Scraper
Uses Selenium headless Chrome to scrape PrestoSports pages
(which require JavaScript rendering).
Outputs necbl_stats.csv with one row per player.
"""

import time
import re
import sys
import pandas as pd
from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup

TEAM_SLUGS = {
    "UPP_VAL": ("Upper Valley Nighthawks",   "UPP", "uppervalleynighthawks"),
    "VAL_BLU": ("Valley Blue Sox",            "VAL", "valleybluesox"),
    "KEE_SWA": ("Keene Swampbats",            "KEE", "keeneswampbats"),
    "BRI_B":   ("Bristol Blues",              "BRI", "bristolblues"),
    "MAR_VIN": ("Martha's Vineyard Sharks",   "MAR", "marthasvineyardsharks"),
    "OCE_STA": ("Ocean State Waves",          "OCE", "oceanstatewaves"),
    "NOR_ADA": ("North Adams Steeplecats",    "NOR", "northadamssteeplecats"),
    "MYS_SCH": ("Mystic Schooners",           "MYS", "mysticschooners"),
    "NEW_GUL": ("Newport Gulls",              "NEW", "newportgulls"),
    "SAN_MAI": ("Sanford Mainers",            "SAN", "sanfordmainers"),
    "VER_MOU": ("Vermont Mountaineers",       "VER", "vermontmountaineers"),
    "DAN_WES": ("Danbury Westerners",         "DAN", "danburywesterners"),
    "NSN":     ("North Shore Navigators",     "NSN", "northshorenavigators"),
}

WOBA_WEIGHTS = {
    "single": 0.888,
    "double": 1.271,
    "triple": 1.616,
    "hr":     2.101,
    "bb_hbp": 0.690,
}

def make_driver():
    opts = Options()
    opts.add_argument("--headless=new")
    opts.add_argument("--no-sandbox")
    opts.add_argument("--disable-dev-shm-usage")
    opts.add_argument("--disable-gpu")
    opts.add_argument("--disable-extensions")
    opts.add_argument("--disable-software-rasterizer")
    opts.add_argument("--disable-background-networking")
    opts.add_argument("--disable-default-apps")
    opts.add_argument("--disable-sync")
    opts.add_argument("--metrics-recording-only")
    opts.add_argument("--mute-audio")
    opts.add_argument("--no-first-run")
    opts.add_argument("--safebrowsing-disable-auto-update")
    opts.add_argument("--window-size=1920,1080")
    opts.add_argument("--memory-pressure-off")
    opts.add_argument(
        "user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    )
    from webdriver_manager.chrome import ChromeDriverManager
    from selenium.webdriver.chrome.service import Service
    service = Service(ChromeDriverManager().install())
    return webdriver.Chrome(service=service, options=opts)


def safe_num(val, default=0):
    try:
        v = re.sub(r"[^0-9.]", "", str(val))
        return float(v) if v else default
    except:
        return default


def scrape_team(driver, season, team_code, team_name, team_abbrev, slug):
    url = (
        f"https://www.necbl.com/sports/bsb/{season}"
        f"/teams/{slug}?view=lineup"
    )
    print(f"  Scraping {team_name} ({season})...", flush=True)

    try:
        driver.get(url)
        # Wait up to 20s for the hitting table to appear
        WebDriverWait(driver, 30).until(
            EC.presence_of_element_located((By.TAG_NAME, "table"))
        )
        time.sleep(3)  # Extra wait for JS data population
    except Exception as e:
        print(f"    Timeout/error loading {team_name}: {e}", flush=True)
        # Retry once
        try:
            print(f"    Retrying {team_name}...", flush=True)
            driver.get(url)
            WebDriverWait(driver, 30).until(
                EC.presence_of_element_located((By.TAG_NAME, "table"))
            )
            time.sleep(3)
        except Exception as e2:
            print(f"    Retry failed: {e2}", flush=True)
            return []

    soup = BeautifulSoup(driver.page_source, "html.parser")
    tables = soup.find_all("table")

    hitting_tbl = None
    for tbl in tables:
        headers = [th.get_text(strip=True).upper() for th in tbl.find_all("th")]
        if "AB" in headers and "H" in headers:
            hitting_tbl = tbl
            break

    if hitting_tbl is None:
        print(f"    No hitting table found for {team_name}", flush=True)
        return []

    headers = [th.get_text(strip=True).upper() for th in hitting_tbl.find_all("th")]

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
        print(f"    Missing required columns for {team_name}. Headers: {headers}", flush=True)
        return []

    rows = hitting_tbl.find_all("tr")[1:]  # skip header
    players = []

    for row in rows:
        cells = row.find_all(["td", "th"])
        if len(cells) < 3:
            continue

        def cell(idx):
            if idx is None or idx >= len(cells):
                return ""
            return cells[idx].get_text(strip=True)

        raw_name = cell(name_col)
        if not raw_name or len(raw_name) < 2:
            continue
        if re.match(r"^(Total|Opponent|Name|Player|---)", raw_name, re.I):
            continue

        ab_val  = safe_num(cell(ab_col))
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

        # Name parsing: PrestoSports is "First Last"
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
            "Player":                raw_name,
            "Last_Name":             last_name,
            "First_Initial":         first_initial,
            "Team":                  team_name,
            "Team_Code":             team_code,
            "Team_Abbrev":           team_abbrev,
            "Season":                season,
            "AB":                    int(ab_val),
            "H":                     int(h_val),
            "Singles":               int(singles),
            "Doubles":               int(d_val),
            "Triples":               int(t_val),
            "HR":                    int(hr_val),
            "BB":                    int(bb_val),
            "HBP":                   int(hbp_val),
            "SO":                    int(so_val),
            "PA":                    int(pa),
            "Batted_Balls":          int(bip),
            "wOBA":                  round(woba, 3),
            "wOBACON":               round(wobacon, 3),
            "Player_Team_Season_Key": composite_key,
        })

    print(f"    Got {len(players)} players", flush=True)
    return players


def main():
    import os
    from datetime import datetime

    current_year = str(datetime.now().year)
    seasons      = [str(y) for y in range(int(current_year), 2020, -1)]

    print(f"Scraping NECBL seasons: {seasons}", flush=True)


    all_rows = []

    for season in seasons:
        print(f"\n=== Season {season} ===", flush=True)
        for code, (name, abbrev, slug) in TEAM_SLUGS.items():
            # Fresh driver per team - prevents one crash cascading to others
            driver = make_driver()
            try:
                rows = scrape_team(driver, season, code, name, abbrev, slug)
                all_rows.extend(rows)
            except Exception as e:
                print(f"    Driver-level error for {name}: {e}", flush=True)
            finally:
                try:
                    driver.quit()
                except:
                    pass
            time.sleep(1)
    if all_rows:
        df = pd.DataFrame(all_rows)
        df.to_csv("necbl_stats.csv", index=False)
        print(f"\nSaved necbl_stats.csv with {len(df)} rows", flush=True)
        print(f"Seasons: {df['Season'].unique().tolist()}", flush=True)
        print(f"Teams:   {df['Team_Code'].nunique()} teams", flush=True)
    else:
        # Write empty CSV so pipeline doesn't crash
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
