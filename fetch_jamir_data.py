#!/usr/bin/env python3
"""
fetch_jamir_data.py
Fetches NBA Stats data for the Jamir Watkins analysis.

"""

import os
import re
import time
import pandas as pd
from curl_cffi import requests as cf_requests

# ── Constants ─────────────────────────────────────────────────────────────────
WIZARDS_ID  = 1610612764
JAMIR_ID    = 1642364
SEASON      = "2025-26"
SEASON_TYPE = "Regular Season"
SLEEP       = 1.5
BASE_URL    = "https://stats.nba.com/stats"

NBA_HEADERS = {
    "Connection":          "keep-alive",
    "Accept":              "application/json, text/plain, */*",
    "x-nba-stats-token":   "true",
    "X-NewRelic-ID":       "VQECWF5UChAHUlNTBwgBVw==",
    "User-Agent":          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) "
                           "AppleWebKit/537.36 (KHTML, like Gecko) "
                           "Chrome/78.0.3904.87 Safari/537.36",
    "x-nba-stats-origin":  "stats",
    "Sec-Fetch-Site":      "same-origin",
    "Sec-Fetch-Mode":      "cors",
    "Referer":             "https://www.nba.com/",
    "Accept-Encoding":     "gzip, deflate, br",
    "Accept-Language":     "en-US,en;q=0.9",
}

# Chrome 120 TLS fingerprint — the key to bypassing stats.nba.com bot detection
session = cf_requests.Session(impersonate="chrome120")


# ── Helpers ───────────────────────────────────────────────────────────────────

def snooze():
    time.sleep(SLEEP)


def safe_gid(gid):
    return str(gid).zfill(10)


def convert_minutes(val):
    """Convert 'PT07M25.00S' → '7:25' so R's lubridate::ms() can parse it."""
    if not val or (isinstance(val, float) and pd.isna(val)):
        return "0:00"
    m = re.match(r"PT(\d+)M([\d.]+)S", str(val))
    if m:
        return f"{int(m.group(1))}:{round(float(m.group(2))):02d}"
    return str(val)


def fetch_result_sets(endpoint, params, rs_index=0):
    """Fetch a resultSets-format endpoint. Returns a DataFrame."""
    resp = session.get(
        f"{BASE_URL}/{endpoint}",
        params=params,
        headers=NBA_HEADERS,
        timeout=30,
    )
    resp.raise_for_status()
    data = resp.json()
    rs = data.get("resultSets") or data.get("resultSet")
    if not rs:
        raise ValueError(f"No resultSets in response for {endpoint}")
    headers = rs[rs_index]["headers"]
    rows    = rs[rs_index]["rowSet"]
    return pd.DataFrame(rows, columns=headers) if rows else pd.DataFrame(columns=headers)


def fetch_box_v3(game_id, box_type="traditional"):
    """Fetch v3 box score; returns flat DataFrame of all players."""
    resp = session.get(
        f"{BASE_URL}/boxscore{box_type}v3",
        params={
            "GameID": game_id, "StartPeriod": 1, "EndPeriod": 10,
            "StartRange": 0, "EndRange": 28800, "RangeType": 0, "LeagueID": "00",
        },
        headers=NBA_HEADERS,
        timeout=30,
    )
    resp.raise_for_status()
    data = resp.json()
    # v3 box score key varies by box_type: "boxScoreTraditional", "boxScoreAdvanced", etc.
    game_key = "boxScore" + box_type.capitalize()
    game = data.get(game_key) or data.get("game") or data
    if not game:
        return pd.DataFrame()

    rows = []
    for team_key in ("homeTeam", "awayTeam"):
        team = game[team_key]
        for p in team.get("players", []):
            stats = p.pop("statistics", {})
            rows.append({**p, **stats, "teamId": team["teamId"], "game_id": game_id})

    if not rows:
        return pd.DataFrame()

    df = pd.DataFrame(rows)
    if "minutes" in df.columns:
        df["minutes"] = df["minutes"].apply(convert_minutes)
    # drop duplicate gameId from API if present
    df = df.drop(columns=["gameId"], errors="ignore")
    return df


def fetch_pbp_v3(game_id):
    """Fetch play-by-play v3 for one game."""
    resp = session.get(
        f"{BASE_URL}/playbyplayv3",
        params={"GameID": game_id, "StartPeriod": 1, "EndPeriod": 10, "LeagueID": "00"},
        headers=NBA_HEADERS,
        timeout=30,
    )
    resp.raise_for_status()
    actions = resp.json()["game"]["actions"]
    if not actions:
        return pd.DataFrame()
    df = pd.DataFrame(actions)
    df["game_id"] = game_id
    df = df.drop(columns=["gameId"], errors="ignore")
    return df


# ── 1. Game dates ──────────────────────────────────────────────────────────────
if os.path.exists("jamir_game_dates.csv"):
    print("jamir_game_dates.csv: loading cache")
    game_dates = pd.read_csv("jamir_game_dates.csv", dtype={"game_id": str})
else:
    print("Fetching game dates...")
    df = fetch_result_sets("leaguegamefinder", {
        "PlayerOrTeam": "T", "TeamID": WIZARDS_ID,
        "Season": SEASON, "SeasonType": SEASON_TYPE, "LeagueID": "00",
    })
    df.columns = [c.lower() for c in df.columns]
    df["game_id"] = df["game_id"].astype(str).str.zfill(10)
    game_dates = df
    game_dates.to_csv("jamir_game_dates.csv", index=False)
    print(f"  {len(game_dates)} games saved.")
    snooze()

game_ids = game_dates["game_id"].unique().tolist()
print(f"Game IDs: {len(game_ids)} total\n")


# ── 2. Traditional box scores ──────────────────────────────────────────────────
if os.path.exists("jamir_boxscore_traditional.csv"):
    print("jamir_boxscore_traditional.csv: loading cache")
else:
    print(f"Fetching traditional box scores ({len(game_ids)} games)...")
    frames = []
    for i, gid in enumerate(game_ids):
        print(f"  [{i+1}/{len(game_ids)}] {gid}", end="\r")
        try:
            df = fetch_box_v3(gid, "traditional")
            df = df[df["teamId"] == WIZARDS_ID]
            frames.append(df)
        except Exception as e:
            print(f"\n  Error {gid}: {e}")
        snooze()
    out = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    out.to_csv("jamir_boxscore_traditional.csv", index=False)
    print(f"\n  {len(out)} rows saved.")


# ── 3. Advanced box scores ─────────────────────────────────────────────────────
if os.path.exists("jamir_boxscore_advanced.csv"):
    print("jamir_boxscore_advanced.csv: loading cache")
else:
    print(f"Fetching advanced box scores ({len(game_ids)} games)...")
    frames = []
    for i, gid in enumerate(game_ids):
        print(f"  [{i+1}/{len(game_ids)}] {gid}", end="\r")
        try:
            df = fetch_box_v3(gid, "advanced")
            df = df[df["teamId"] == WIZARDS_ID]
            frames.append(df)
        except Exception as e:
            print(f"\n  Error {gid}: {e}")
        snooze()
    out = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    out.to_csv("jamir_boxscore_advanced.csv", index=False)
    print(f"\n  {len(out)} rows saved.")


# ── 4. League-wide advanced stats ─────────────────────────────────────────────
if os.path.exists("jamir_league_advanced.csv"):
    print("jamir_league_advanced.csv: loading cache")
else:
    print("Fetching league advanced stats...")
    df = fetch_result_sets("leaguedashplayerstats", {
        "Season": SEASON, "SeasonType": SEASON_TYPE,
        "PerMode": "PerGame", "MeasureType": "Advanced", "LeagueID": "00",
        "College": "", "Conference": "", "Country": "",
        "DateFrom": "", "DateTo": "", "Division": "",
        "DraftPick": "", "DraftYear": "", "GameScope": "", "GameSegment": "",
        "Height": "", "LastNGames": 0, "Location": "", "Month": 0,
        "OpponentTeamID": 0, "Outcome": "", "PORound": 0,
        "PaceAdjust": "N", "Period": 0, "PlayerExperience": "",
        "PlayerPosition": "", "PlusMinus": "N", "Rank": "N",
        "SeasonSegment": "", "ShotClockRange": "", "StarterBench": "",
        "TeamID": 0, "TwoWay": 0, "VsConference": "", "VsDivision": "", "Weight": "",
    })
    df.to_csv("jamir_league_advanced.csv", index=False)
    print(f"  {len(df)} players saved.")
    snooze()


# ── 4b. League-wide traditional stats (for STL, BLK, PLAYER_POSITION) ─────────
if os.path.exists("jamir_league_traditional.csv"):
    print("jamir_league_traditional.csv: loading cache")
else:
    print("Fetching league traditional stats...")
    df = fetch_result_sets("leaguedashplayerstats", {
        "Season": SEASON, "SeasonType": SEASON_TYPE,
        "PerMode": "PerGame", "MeasureType": "Base", "LeagueID": "00",
        "College": "", "Conference": "", "Country": "",
        "DateFrom": "", "DateTo": "", "Division": "",
        "DraftPick": "", "DraftYear": "", "GameScope": "", "GameSegment": "",
        "Height": "", "LastNGames": 0, "Location": "", "Month": 0,
        "OpponentTeamID": 0, "Outcome": "", "PORound": 0,
        "PaceAdjust": "N", "Period": 0, "PlayerExperience": "",
        "PlayerPosition": "", "PlusMinus": "N", "Rank": "N",
        "SeasonSegment": "", "ShotClockRange": "", "StarterBench": "",
        "TeamID": 0, "TwoWay": 0, "VsConference": "", "VsDivision": "", "Weight": "",
    })
    df.to_csv("jamir_league_traditional.csv", index=False)
    print(f"  {len(df)} players saved.")
    snooze()


# ── 5. On/off data ─────────────────────────────────────────────────────────────
if os.path.exists("jamir_on_off.csv"):
    print("jamir_on_off.csv: loading cache")
else:
    print("Fetching on/off data...")
    oo_params = {
        "TeamID": WIZARDS_ID, "Season": SEASON,
        "SeasonType": SEASON_TYPE, "MeasureType": "Advanced",
        "PerMode": "PerGame", "LeagueID": "00",
        "DateFrom": "", "DateTo": "", "GameSegment": "",
        "LastNGames": 0, "Location": "", "Month": 0,
        "OpponentTeamID": 0, "Outcome": "", "Period": 0, "PORound": 0,
        "VsConference": "", "VsDivision": "",
    }
    try:
        on_df  = fetch_result_sets("teamplayeronoffdetails", oo_params, rs_index=0)
        on_df["STATUS"] = "ON"
        off_df = fetch_result_sets("teamplayeronoffdetails", oo_params, rs_index=1)
        off_df["STATUS"] = "OFF"
        on_off = pd.concat([on_df, off_df], ignore_index=True)
        on_off.to_csv("jamir_on_off.csv", index=False)
        print(f"  {len(on_off)} rows saved ({len(on_df)} ON + {len(off_df)} OFF).")
    except Exception as e:
        print(f"  on/off fetch failed: {e}")
    snooze()


# ── 6. Jamir shot chart ────────────────────────────────────────────────────────
if os.path.exists("jamir_shot_chart.csv"):
    print("jamir_shot_chart.csv: loading cache")
else:
    print("Fetching Jamir shot chart...")
    shot_base = {
        "Season": SEASON, "SeasonType": SEASON_TYPE,
        "GameID": "", "Outcome": "", "Location": "", "Month": 0,
        "SeasonSegment": "", "DateFrom": "", "DateTo": "",
        "OpponentTeamID": 0, "VsConference": "", "VsDivision": "",
        "RookieYear": "", "GameSegment": "", "Period": 0, "LastNGames": 0,
        "AheadBehind": "", "PointDiff": "", "RangeType": 0,
        "StartPeriod": 1, "EndPeriod": 10, "StartRange": 0, "EndRange": 28800,
        "ContextFilter": "", "ContextMeasure": "FGA", "LeagueID": "00",
    }
    df = fetch_result_sets("shotchartdetail", {"PlayerID": JAMIR_ID, "TeamID": 0, **shot_base})
    df.to_csv("jamir_shot_chart.csv", index=False)
    print(f"  {len(df)} shots saved.")
    snooze()


# ── 7. Team shot chart ─────────────────────────────────────────────────────────
if os.path.exists("jamir_shot_chart_team.csv"):
    print("jamir_shot_chart_team.csv: loading cache")
else:
    print("Fetching Wizards team shot chart...")
    df = fetch_result_sets("shotchartdetail", {"PlayerID": 0, "TeamID": WIZARDS_ID, **shot_base})
    df.to_csv("jamir_shot_chart_team.csv", index=False)
    print(f"  {len(df)} shots saved.")
    snooze()


# ── 8. Five-man lineup data ────────────────────────────────────────────────────
if os.path.exists("jamir_lineups.csv"):
    print("jamir_lineups.csv: loading cache")
else:
    print("Fetching 5-man lineup data...")
    df = fetch_result_sets("leaguedashlineups", {
        "GroupQuantity": 5, "TeamID": WIZARDS_ID,
        "Season": SEASON, "SeasonType": SEASON_TYPE,
        "MeasureType": "Advanced", "PerMode": "PerGame", "LeagueID": "00",
        "DateFrom": "", "DateTo": "", "GameSegment": "",
        "LastNGames": 0, "Location": "", "Month": 0,
        "OpponentTeamID": 0, "Outcome": "", "PORound": 0, "Period": 0,
        "SeasonSegment": "", "ShotClockRange": "", "VsConference": "", "VsDivision": "",
    })
    df.to_csv("jamir_lineups.csv", index=False)
    print(f"  {len(df)} lineups saved.")
    snooze()


# ── 9. Play-by-play ────────────────────────────────────────────────────────────
if os.path.exists("jamir_pbp.csv"):
    print("jamir_pbp.csv: loading cache")
else:
    print(f"Fetching play-by-play ({len(game_ids)} games)...")
    frames = []
    for i, gid in enumerate(game_ids):
        print(f"  [{i+1}/{len(game_ids)}] {gid}", end="\r")
        try:
            frames.append(fetch_pbp_v3(gid))
        except Exception as e:
            print(f"\n  Error {gid}: {e}")
        snooze()
    out = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    out.to_csv("jamir_pbp.csv", index=False)
    print(f"\n  {len(out)} PBP rows saved.")


# ── 10. League hustle stats ────────────────────────────────────────────────────
if os.path.exists("jamir_hustle_stats.csv"):
    print("jamir_hustle_stats.csv: loading cache")
else:
    print("Fetching league hustle stats...")
    df = fetch_result_sets("leaguehustlestatsplayer", {
        "Season": SEASON, "SeasonType": SEASON_TYPE,
        "PerMode": "PerGame", "LeagueID": "00",
        "College": "", "Conference": "", "Country": "",
        "DateFrom": "", "DateTo": "", "Division": "",
        "DraftPick": "", "DraftYear": "", "GameScope": "", "GameSegment": "",
        "Height": "", "LastNGames": 0, "Location": "", "Month": 0,
        "OpponentTeamID": 0, "Outcome": "", "PORound": 0,
        "PlayerExperience": "", "PlayerPosition": "",
        "SeasonSegment": "", "TeamID": 0,
        "VsConference": "", "VsDivision": "", "Weight": "",
    })
    df.to_csv("jamir_hustle_stats.csv", index=False)
    print(f"  {len(df)} players saved.")
    snooze()


# ── Summary ────────────────────────────────────────────────────────────────────
print("\nStatus:")
for f in [
    "jamir_game_dates.csv", "jamir_boxscore_traditional.csv",
    "jamir_boxscore_advanced.csv", "jamir_league_advanced.csv",
    "jamir_on_off.csv", "jamir_shot_chart.csv", "jamir_shot_chart_team.csv",
    "jamir_lineups.csv", "jamir_pbp.csv",
    "jamir_league_traditional.csv", "jamir_hustle_stats.csv",
]:
    exists = os.path.exists(f)
    size   = os.path.getsize(f) if exists else 0
    print(f"  {'OK' if exists else 'MISSING':6s} {f} ({size:,} bytes)")
