"""
NECBL Hitting Stats Scraper + Auto-Upload to Google Drive
Scrapes all 13 NECBL teams, calculates wOBA/wOBACON, uploads to Drive.

Setup (once):
  pip install requests beautifulsoup4 pandas google-auth google-api-python-client

Run:
  python scrape_necbl_local.py

Schedule daily via Windows Task Scheduler or Mac/Linux cron.

Requirements:
  - service_account.json in the same folder as this script
  - NECBL_STATS_FOLDER_ID set below
"""

import re
import sys
import os
import time
import requests
import pandas as pd
from bs4 import BeautifulSoup
from datetime import datetime

# ============================================================
#  CONFIGURATION — edit these two values
# ============================================================
SERVICE_ACCOUNT_JSON = r"C:\Users\bengi\OneDrive\NECBLScraper\service_account.json"
NECBL_STATS_FOLDER_ID = "1UFkClomCviloJrq4X7cUGeNVMCy7ENQb"
# ============================================================

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    )
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
    print(f"  {team_name} ({season})...", flush=True, end=" ")

    try:
        resp = session.get(url, headers=HEADERS, timeout=20)
        if resp.status_code != 200:
            print(f"HTTP {resp.status_code} - skipped")
            return []
    except Exception as e:
        print(f"Error: {e}")
        return []

    soup = BeautifulSoup(resp.text, "html.parser")
    tables = soup.find_all("table")

    hitting_tbl = None
    for tbl in tables:
        hdrs = [th.get_text(strip=True).upper() for th in tbl.find_all("th")]
        if "AB (AT BATS)" in hdrs or "AB" in hdrs:
            hitting_tbl = tbl
            break

    if hitting_tbl is None:
        print("no stats table")
        return []

    hdrs = [th.get_text(strip=True).upper() for th in hitting_tbl.find_all("th")]

    def col(name, *alts):
        for n in [name] + list(alts):
            for i, h in enumerate(hdrs):
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
        print("missing columns")
        return []

    rows = hitting_tbl.find_all("tr")[1:]
    players = []

    for row in rows:
        cells = row.find_all(["td", "th"])
        if len(cells) < 3:
            continue

        def cell(idx):
            if idx is None or idx >= len(cells):
                return ""
            return cells[idx].get_text(strip=True)

        raw_name = re.sub(r"\.+$", "", cell(name_col)).strip()
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

    print(f"{len(players)} players")
    return players


def upload_to_drive(csv_path, folder_id, sa_json):
    """Upload necbl_stats.csv to Google Drive, replacing any existing file."""
    from google.oauth2 import service_account
    from googleapiclient.discovery import build
    from googleapiclient.http import MediaFileUpload

    print("\nUploading to Google Drive...", flush=True)

    creds = service_account.Credentials.from_service_account_file(
        sa_json,
        scopes=["https://www.googleapis.com/auth/drive"]
    )
    service = build("drive", "v3", credentials=creds)

    # Check if file already exists in folder
    query = (
        f"name = 'necbl_stats.csv' "
        f"and '{folder_id}' in parents "
        f"and trashed = false"
    )
    existing = service.files().list(q=query, fields="files(id, name)").execute()
    existing_files = existing.get("files", [])

    media = MediaFileUpload(csv_path, mimetype="text/csv", resumable=True)

    if existing_files:
        # Update existing file (service account has permission, no quota needed)
        file_id = existing_files[0]["id"]
        service.files().update(
            fileId=file_id,
            media_body=media
        ).execute()
        print(f"  Updated necbl_stats.csv (id: {file_id})")
    else:
        print("  No existing necbl_stats.csv found in Drive folder.")
        print("  Please upload necbl_stats.csv to the NECBL Stats folder manually once.")
        print(f"  File is saved locally at: {csv_path}")

    print("  Upload complete.")


def main():
    current_year = str(datetime.now().year)
    seasons = [str(y) for y in range(int(current_year), 2020, -1)]

    print("=" * 60)
    print("NECBL Stats Scraper")
    print(f"Scraping seasons: {seasons}")
    print("=" * 60)

    all_rows = []
    session = requests.Session()

    for season in seasons:
        print(f"\n--- Season {season} ---")
        for code, (name, abbrev, slug) in TEAM_SLUGS.items():
            rows = scrape_team(session, season, code, name, abbrev, slug)
            all_rows.extend(rows)
            time.sleep(1.5)

    print("\n" + "=" * 60)

    if not all_rows:
        print("No data scraped — check your internet connection.")
        sys.exit(1)

    df = pd.DataFrame(all_rows)
    csv_path = os.path.join(os.path.dirname(__file__), "necbl_stats.csv")
    df.to_csv(csv_path, index=False)

    print(f"Saved {csv_path}")
    print(f"  Total rows:  {len(df)}")
    print(f"  Seasons:     {sorted(df['Season'].unique().tolist(), reverse=True)}")
    print(f"  Teams:       {df['Team_Code'].nunique()} / 13")

    # Upload to Drive
    if not NECBL_STATS_FOLDER_ID or "REPLACE" in NECBL_STATS_FOLDER_ID:
        print("\nSkipping Drive upload — set NECBL_STATS_FOLDER_ID in the script first.")
        return

    if not os.path.exists(SERVICE_ACCOUNT_JSON):
        print(f"\nSkipping Drive upload — {SERVICE_ACCOUNT_JSON} not found.")
        return

    try:
        upload_to_drive(csv_path, NECBL_STATS_FOLDER_ID, SERVICE_ACCOUNT_JSON)
    except Exception as e:
        print(f"Drive upload failed: {e}")
        print("CSV saved locally — upload manually if needed.")


if __name__ == "__main__":
    main()
