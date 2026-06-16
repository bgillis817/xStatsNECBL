"""
NECBL Hitting Stats Scraper
Uses the PrestoSports static monospace print template which returns
plain HTML with no JavaScript required.
URL: https://newenglandcollegiateleague.prestosports.com/sports/bsb/YEAR/teams/SLUG
     ?tmpl=teaminfo-network-monospace-template&sort=ab&pos=h
"""

import re
import sys
import time
import requests
import pandas as pd
from bs4 import BeautifulSoup
from datetime import datetime

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.5",
}

BASE = "https://newenglandcollegiateleague.prestosports.com"

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
        v = re.sub(r"[^0-9.]", "", str(val).strip())
        return float(v) if v else default
    except:
        return default


def scrape_team(session, season, team_code, team_name, team_abbrev, slug):
    url = (
        f"{BASE}/sports/bsb/{season}/teams/{slug}"
        f"?tmpl=teaminfo-network-monospace-template&sort=ab&pos=h"
    )
    print(f"  Scraping {team_name} ({season})...", flush=True)

    try:
        resp = session.get(url, headers=HEADERS, timeout=20)
        if resp.status_code != 200:
            print(f"    HTTP {resp.status_code} - skipping", flush=True)
            return []
    except Exception as e:
        print(f"    Request error: {e}", flush=True)
        return []

    soup = BeautifulSoup(resp.text, "html.parser")
    tables = soup.find_all("table")

    hitting_tbl = None
    for tbl in tables:
        headers = [th.get_text(strip=True).upper() for th in tbl.find_all("th")]
        if "AB (AT BATS)" in headers or "AB" in headers:
            hitting_tbl = tbl
            break

    if hitting_tbl is None:
        print(f"    No hitting table found", flush=True)
        return []

    headers = [th.get_text(strip=True).upper() for th in hitting_tbl.find_all("th")]

    def col(name, *alts):
        for n in [name] + list(alts):
            for i, h in enumerate(headers):
                if h == n or h.startswith(n):
                    return i
        return None

    name_col = col("PLAYER", "NAME")
    ab_col   = col("AB")
    h_col    = col("H (HITS)", "H")
    d_col    = col("2B")
    t_col    = col("3B")
    hr_col   = col("HR")
    bb_col   = col("BB")
    hbp_col  = col("HBP")
    so_col   = col("SO", "K")

    if name_col is None or ab_col is None or h_col is None:
        print(f"    Missing columns. Headers: {headers[:10]}", flush=True)
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
        # Strip trailing dots used for monospace padding
        raw_name = re.sub(r"\.+$", "", raw_name).strip()

        if not raw_name or len(raw_name) < 2:
            continue
        if re.match(r"^(Total|Opponent|Name|Player|---|#)", raw_name, re.I):
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

        # PrestoSports monospace template: "First Last......."
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
    session = requests.Session()

    for season in seasons:
        print(f"\n=== Season {season} ===", flush=True)
        for code, (name, abbrev, slug) in TEAM_SLUGS.items():
            rows = scrape_team(session, season, code, name, abbrev, slug)
            all_rows.extend(rows)
            time.sleep(0.5)

    if all_rows:
        df = pd.DataFrame(all_rows)
        df.to_csv("necbl_stats.csv", index=False)
        print(f"\nSaved necbl_stats.csv with {len(df)} rows", flush=True)
        print(f"Seasons: {sorted(df['Season'].unique().tolist(), reverse=True)}", flush=True)
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
