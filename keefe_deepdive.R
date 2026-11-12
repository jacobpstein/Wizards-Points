###########################################################
# What is Brian Keefe actually in charge of?
# Possessions, timeouts, rotations and defense: what the data can and
# can't say about coaching.  wizardspoints.substack.com
#
# ONE self-contained script. Run it top to bottom:
#   Rscript keefe_deepdive.R            # uses cached data if present
#   REFRESH=1 Rscript keefe_deepdive.R  # re-pulls the current season first
#
# Data comes from stats.nba.com, which blocks R's TLS fingerprint, so the
# pull is done by a small Python program embedded below (section 0): the
# script writes it to a temp file, makes itself a local virtualenv with
# curl_cffi + pandas the first time (./.venv-nba, git-ignored), runs it,
# and reads the CSVs it leaves in ./possession_data (also git-ignored).
# Nothing in the data folder is meant to be committed. (The league-wide box scores for 2024-25 and
# 2025-26 were first seeded from the full-season cdn.nba.com pull made for tank_vs_talent on 2026-11-07,
# converted to this script's columns; the fetcher tops up anything missing either way. Play-by-play is
# NOT seeded that way: the cdn feed labels actions differently from playbyplayv3, so it is always pulled here.) Figures go to
# ./possession_figs.
#
# Sections: 0 data  1 possession length & shot timing  2 shot quality vs
# shot making  3 the coaching-change panel (offense)  4 defense & rotation
# 5 does he move minutes on last night's box score  6 timeouts, after-timeout
# plays, challenges  7 data art (tapestry, timeout clock, constellation)
# 8 Keefe vs Keefe: four fingerprints per season, how far a November rank can be
# trusted, and which changes since 2025-26 are bigger than a normal offseason
#
# R version 4.5.3; tidyverse, janitor, scales, patchwork, ragg.
###########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(scales)
  library(patchwork)
})
set.seed(202)

# ===========================================================================
# House style (same conventions as wizards_rankings.R)
# ===========================================================================

font <- "Gill Sans"
if (requireNamespace("systemfonts", quietly = TRUE) && !font %in% systemfonts::system_fonts()$family) font <- "sans"

# palette: colorhunt.co/palette/2b2a4cb31312ea906ceee2de (navy, crimson, peach, cream)
pal <- c(red = "#B31312", teal = "#2B2A4C", teal_dark = "#1C1B33", navy = "#2B2A4C", grey = "#4D4D4D", gold = "#EA906C", ink = "#1C1B33", cream = "#EEE2DE")
wiz_blue <- pal[["navy"]]; wiz_red <- pal[["red"]]; teal <- pal[["teal"]]; teal_dark <- pal[["teal_dark"]]
gold <- pal[["gold"]]; ink <- pal[["ink"]]; grey_text <- pal[["grey"]]; cream <- pal[["cream"]]
caption_text <- "Data: nba.com/stats\nwizardspoints.substack.com"
WIZ_ID <- 1610612764; CUR <- "2026-27"

theme_wiz <- function(base_size = 18) {
  ggplot2::theme_minimal(base_size = base_size, base_family = font) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.6), colour = "black", margin = ggplot2::margin(b = 10))
      , plot.subtitle = ggplot2::element_text(colour = pal[["grey"]], size = ggplot2::rel(1.05), lineheight = 1.1, margin = ggplot2::margin(b = 18))
      , plot.caption = ggplot2::element_text(hjust = 1, colour = "black", size = ggplot2::rel(0.9), lineheight = 1.1, margin = ggplot2::margin(t = 14))
      , plot.title.position = "plot", plot.caption.position = "plot"
      , panel.grid = ggplot2::element_blank()
      , axis.text = ggplot2::element_text(colour = pal[["grey"]])
      , axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.1), margin = ggplot2::margin(t = 8))
      , legend.text = ggplot2::element_text(colour = pal[["grey"]])
      , strip.text = ggplot2::element_text(face = "bold", colour = "black")
      , plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
# the coaching scripts were written against a smaller base size; keep their proportions
theme_wp <- function(base = 14) theme_wiz(base_size = base) + ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.35)))
# dark variant for the data-art pieces
theme_ink <- function(base_size = 15) {
  ggplot2::theme_void(base_size = base_size, base_family = font) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = ink, colour = NA), text = ggplot2::element_text(colour = "grey85", family = font)
                   , plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.7), colour = "white", margin = ggplot2::margin(b = 8))
                   , plot.subtitle = ggplot2::element_text(colour = "grey70", lineheight = 1.1, margin = ggplot2::margin(b = 14))
                   , plot.caption = ggplot2::element_text(colour = "grey55", size = ggplot2::rel(0.8), hjust = 1)
                   , plot.title.position = "plot", plot.caption.position = "plot", plot.margin = ggplot2::margin(14, 14, 10, 14))
}

ordinal <- function(n) { suf <- ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[(n %% 10) + 1]); paste0(n, suf) }
dir.create("possession_figs", showWarnings = FALSE)

# "Through <date>" and "first N games" in titles follow the latest game in the Wizards game log,
# so a data refresh updates every chart's labels without touching the scripts.
.gl_path <- "possession_data/wizards_gamelog_2026-27.csv"
if (file.exists(.gl_path)) {
  .gl <- utils::read.csv(.gl_path, stringsAsFactors = FALSE)
  .gl <- .gl[!is.na(.gl$WL) & nzchar(.gl$WL), ]   # a game pulled while still in progress has no result yet; don't count it
  .d <- as.Date(.gl$GAME_DATE, format = "%b %d, %Y")
  through_day <- format(max(.d, na.rm = TRUE), "%b %e") |> trimws() |> gsub(pattern = "  ", replacement = " ")
  n_games <- nrow(.gl)
} else { through_day <- "Nov 5"; n_games <- 8 }
fix_dates <- function(x) {
  if (is.null(x) || !is.character(x)) return(x)
  x <- gsub("Nov 5", through_day, x, fixed = TRUE)
  x <- gsub("Nov 4", through_day, x, fixed = TRUE)
  x <- gsub("first 8 games", paste("first", n_games, "games"), x, fixed = TRUE)
  x <- gsub("(8 games)", paste0("(", n_games, " games)"), x, fixed = TRUE)
  gsub("eight games", paste(tolower(english_number(n_games)), "games"), x, fixed = TRUE)
}
english_number <- function(n) { w <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"); if (n <= 15) w[n] else as.character(n) }
save_plot <- function(name, p, width = 11, height = 7, bg = "white") {
  if (inherits(p, "ggplot")) {
    for (k in c("title", "subtitle", "caption")) if (!is.null(p$labels[[k]])) p$labels[[k]] <- fix_dates(p$labels[[k]])
  } else if (inherits(p, "patchwork")) {
    p <- p + patchwork::plot_annotation(title = fix_dates(p$patches$annotation$title), subtitle = fix_dates(p$patches$annotation$subtitle))
  }
  # figures built at phone width keep their size; the rest (the appendix) are saved on a canvas 15% smaller at the same
  # font sizes, so their text reads about 18% larger in a newsletter
  phone <- c("player_development.png", "same_player_habits.png", "keefe_evolution_fans.png", "change_baseline.png", "coach_change_moves.png", "roster_implied_change.png", "keefe_own_decisions.png", "timeout_map.png")
  ggplot2::ggsave(file.path("possession_figs", name), p, width = width, height = height, dpi = 300, device = ragg::agg_png, bg = bg
                  , scale = if (name %in% phone) 1 else 0.85)
}
pt <- function(x, digits = 3) print(as.data.frame(x), digits = digits, row.names = FALSE)


# ===========================================================================
# 0. Data: the embedded fetcher, a one-time virtualenv, and a cache folder
# ===========================================================================
DATA_DIR <- "possession_data"
REFRESH  <- nzchar(Sys.getenv("REFRESH"))
dir.create(DATA_DIR, showWarnings = FALSE)
for (ig in c(DATA_DIR, ".venv-nba")) {           # keep the cache and the venv out of git
  gi <- ".gitignore"
  cur <- if (file.exists(gi)) readLines(gi, warn = FALSE) else character()
  if (!paste0(ig, "/") %in% cur) writeLines(c(cur, paste0(ig, "/")), gi)
}

nba_fetch_py <- r"---(
#!/usr/bin/env python3
"""
nba_fetch_all.py  (embedded in keefe_deepdive.R; also runnable on its own)

Pulls everything the coaching post needs from stats.nba.com into OUT_DIR and never
prints a row of data, only counts. Files that already exist are skipped, so re-runs
are cheap; pass --refresh-current to re-pull the current season (box scores and
play-by-play are appended game by game and resume automatically).

    python nba_fetch_all.py <out_dir> [--refresh-current] [--season 2026-27]

Also pulls the same team tables for the five seasons before CUR cut at the current point in the season
(early_*.csv), to check how much a November rank says about where a team finishes.

Endpoints: leaguedashteamstats (Advanced / Defense / Four Factors / Opponent / Bench),
leaguedashptstats (Possessions, Passing), leaguedashteamptshot and leaguedashoppptshot
(by shot-clock bucket), leaguedashteamshotlocations (own and opponent), leaguehustlestatsteam,
leaguedashlineups (5-man per team, 2-man for the Wizards), commonteamroster (coaches),
shotchartdetail, teamgamelog, leaguegamelog, playbyplayv3, boxscoretraditionalv3.
"""

import os
import sys
import time
import pandas as pd
from curl_cffi import requests as cf_requests

OUT_DIR = sys.argv[1] if len(sys.argv) > 1 else "possession_data"
REFRESH_CURRENT = "--refresh-current" in sys.argv
CUR = sys.argv[sys.argv.index("--season") + 1] if "--season" in sys.argv else "2026-27"
WIZ = 1610612764
SEASON_TYPE = "Regular Season"
SLEEP = 1.0
BASE = "https://stats.nba.com/stats"
PANEL_SEASONS = [f"{y}-{str(y + 1)[-2:]}" for y in range(2013, int(CUR[:4]) + 1)]
KEEFE_SEASONS = [f"{y}-{str(y + 1)[-2:]}" for y in range(int(CUR[:4]) - 2, int(CUR[:4]) + 1)]
PREV = KEEFE_SEASONS[-2]
# the early-season calibration: the same team tables for recent completed seasons, cut at the point in the
# season the current one has reached, so a November rank can be compared with where that team finished
EARLY_SEASONS = [f"{y}-{str(y + 1)[-2:]}" for y in range(int(CUR[:4]) - 5, int(CUR[:4]))]
CLOCK_RANGES = ["24-22", "22-18 Very Early", "18-15 Early", "15-7 Average", "7-4 Late", "4-0 Very Late"]

HEADERS = {
    "Connection": "keep-alive", "Accept": "application/json, text/plain, */*", "x-nba-stats-token": "true",
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36",
    "x-nba-stats-origin": "stats", "Sec-Fetch-Site": "same-origin", "Sec-Fetch-Mode": "cors",
    "Referer": "https://www.nba.com/", "Accept-Encoding": "gzip, deflate, br", "Accept-Language": "en-US,en;q=0.9",
}
session = cf_requests.Session(impersonate="chrome120")   # Chrome TLS fingerprint: the part R cannot do


def path(name):
    return os.path.join(OUT_DIR, name)


def get_json(endpoint, params, tries=3):
    for attempt in range(tries):
        try:
            r = session.get(f"{BASE}/{endpoint}", params=params, headers=HEADERS, timeout=45)
            r.raise_for_status()
            time.sleep(SLEEP)
            return r.json()
        except Exception as e:  # noqa
            print(f"  ! {endpoint} attempt {attempt + 1}: {type(e).__name__}", file=sys.stderr, flush=True)
            time.sleep(4 * (attempt + 1))
    raise RuntimeError(f"gave up on {endpoint}")


def fetch(endpoint, params, rs_name=None, rs_index=0):
    data = get_json(endpoint, params)
    rs = data.get("resultSets") or [data.get("resultSet")]
    sel = next(r for r in rs if r["name"] == rs_name) if rs_name else rs[rs_index]
    return pd.DataFrame(sel["rowSet"], columns=sel["headers"])


def save(df, name):
    df.to_csv(path(name), index=False)
    print(f"  wrote {name}: {len(df)} rows", flush=True)


def have(name):
    return os.path.exists(path(name))


def team_stats(season, **over):
    p = dict(Conference="", DateFrom="", DateTo="", Division="", GameScope="", GameSegment="", LastNGames=0, LeagueID="00",
             Location="", MeasureType="Advanced", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N",
             PerMode="PerGame", Period=0, PlayerExperience="", PlayerPosition="", PlusMinus="N", Rank="N", Season=season,
             SeasonSegment="", SeasonType=SEASON_TYPE, ShotClockRange="", StarterBench="", TeamID=0, TwoWay=0,
             VsConference="", VsDivision="")
    p.update(over)
    return fetch("leaguedashteamstats", p)


def pt_stats(season, measure, date_to="", who="Team"):
    return fetch("leaguedashptstats", dict(
        College="", Conference="", Country="", DateFrom="", DateTo=date_to, Division="", DraftPick="", DraftYear="", GameScope="",
        Height="", LastNGames=0, LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PerMode="PerGame",
        PlayerExperience="", PlayerOrTeam=who, PlayerPosition="", PtMeasureType=measure, Season=season, SeasonSegment="",
        SeasonType=SEASON_TYPE, StarterBench="", TeamID=0, VsConference="", VsDivision="", Weight=""))


def clock_shots(season, endpoint, date_to=""):
    frames = []
    for rng in CLOCK_RANGES:
        d = fetch(endpoint, dict(
            CloseDefDistRange="", DateFrom="", DateTo=date_to, DribbleRange="", GameSegment="", GeneralRange="Overall", LastNGames=0,
            LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N", PerMode="PerGame",
            Period=0, PlusMinus="N", Rank="N", Season=season, SeasonSegment="", SeasonType=SEASON_TYPE, ShotClockRange=rng,
            ShotDistRange="", TeamID=0, TouchTimeRange="", VsConference="", VsDivision=""))
        d["SHOT_CLOCK_RANGE"] = rng
        frames.append(d)
    return pd.concat(frames, ignore_index=True)


def shot_locations(season, measure="Base", date_to="", endpoint="leaguedashteamshotlocations"):
    data = get_json(endpoint, dict(
        Conference="", DateFrom="", DateTo=date_to, DistanceRange="By Zone", Division="", GameScope="", GameSegment="", LastNGames=0,
        LeagueID="00", Location="", MeasureType=measure, Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N",
        PerMode="Totals", Period=0, PlayerExperience="", PlayerPosition="", PlusMinus="N", Rank="N", Season=season,
        SeasonSegment="", SeasonType=SEASON_TYPE, ShotClockRange="", StarterBench="", TeamID=0, VsConference="", VsDivision=""))
    rs = data["resultSets"]
    zones, cols = rs["headers"][0], rs["headers"][1]["columnNames"]
    skip, span = zones["columnsToSkip"], zones["columnSpan"]
    out, rest = cols[:skip], cols[skip:]
    for i, z in enumerate(zones["columnNames"]):
        zc = z.upper().replace(" ", "_").replace("(", "").replace(")", "").replace("-", "_")
        out += [f"{c}_{zc}" for c in rest[i * span:(i + 1) * span]]
    return pd.DataFrame(rs["rowSet"], columns=out)


def hustle(season, date_to=""):
    return fetch("leaguehustlestatsteam", dict(
        College="", Conference="", Country="", DateFrom="", DateTo=date_to, Division="", DraftPick="", DraftYear="", GameScope="",
        Height="", LastNGames=0, LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N",
        PerMode="PerGame", PlayerExperience="", PlayerPosition="", Season=season, SeasonSegment="", SeasonType=SEASON_TYPE,
        TeamID=0, VsConference="", VsDivision="", Weight=""))


def lineups(season, team_id, n=5, date_to=""):
    return fetch("leaguedashlineups", dict(
        Conference="", DateFrom="", DateTo=date_to, Division="", GameSegment="", GroupQuantity=n, LastNGames=0, LeagueID="00",
        Location="", MeasureType="Advanced", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N", PerMode="Totals",
        Period=0, PlusMinus="N", Rank="N", Season=season, SeasonSegment="", SeasonType=SEASON_TYPE, ShotClockRange="",
        TeamID=team_id, VsConference="", VsDivision=""))


def coaches(season, team_ids):
    rows = []
    for tid in team_ids:
        r = fetch("commonteamroster", dict(TeamID=int(tid), Season=season, LeagueID="00"), rs_name="Coaches")
        r = r[r["COACH_TYPE"] == "Head Coach"].copy()
        r["TEAM_ID"] = int(tid)
        rows.append(r[["TEAM_ID", "COACH_ID", "COACH_NAME", "COACH_TYPE"]])
    return pd.concat(rows, ignore_index=True)


def shots(season, team_id):
    return fetch("shotchartdetail", dict(
        AheadBehind="", ClutchTime="", ContextFilter="", ContextMeasure="FGA", DateFrom="", DateTo="", EndPeriod=10, EndRange=28800,
        GameID="", GameSegment="", LastNGames=0, LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0,
        Period=0, PlayerID=0, PlayerPosition="", PointDiff="", Position="", RangeType=0, RookieYear="", Season=season,
        SeasonSegment="", SeasonType=SEASON_TYPE, StartPeriod=1, StartRange=0, TeamID=team_id, VsConference="", VsDivision=""),
        rs_name="Shot_Chart_Detail")


def game_ids(season, scope):
    gl = fetch("leaguegamelog", dict(Counter=5000, DateFrom="", DateTo="", Direction="ASC", LeagueID="00", PlayerOrTeam="T",
                                     Season=season, SeasonType=SEASON_TYPE, Sorter="DATE"))
    if scope == "wizards":
        gl = gl[gl["TEAM_ID"] == WIZ]
    return sorted(gl["GAME_ID"].astype(str).str.zfill(10).unique().tolist())


def append_by_game(name, season, scope, fetch_one):
    p = path(name)
    done = set(pd.read_csv(p, usecols=["gameId"], dtype=str)["gameId"].unique()) if os.path.exists(p) else set()
    todo = [g for g in game_ids(season, scope) if g not in done]
    print(f"  {name}: {len(todo)} games to fetch", flush=True)
    for i, gid in enumerate(todo, 1):
        for attempt in range(3):
            try:
                df = fetch_one(gid)
                df.to_csv(p, mode="a", header=not os.path.exists(p), index=False)
                break
            except Exception as e:  # noqa
                print(f"  ! {gid} attempt {attempt + 1}: {type(e).__name__}", file=sys.stderr, flush=True)
                time.sleep(5 * (attempt + 1))
        if i % 100 == 0:
            print(f"    {i}/{len(todo)}", flush=True)


PBP_KEEP = ["gameId", "actionNumber", "clock", "period", "teamId", "teamTricode", "personId", "playerName", "scoreHome",
            "scoreAway", "description", "actionType", "subType", "isFieldGoal", "shotResult", "shotValue"]


def pbp_game(gid):
    data = get_json("playbyplayv3", dict(GameID=gid, StartPeriod=0, EndPeriod=14))
    acts = pd.DataFrame(data["game"]["actions"])
    acts["gameId"] = gid
    for c in PBP_KEEP:
        if c not in acts.columns:
            acts[c] = None
    return acts[PBP_KEEP]


def box_game(gid):
    box = get_json("boxscoretraditionalv3", dict(GameID=gid, LeagueID="00", StartPeriod=0, EndPeriod=14, StartRange=0,
                                                  EndRange=28800, RangeType=0))["boxScoreTraditional"]
    rows = []
    for side in ["homeTeam", "awayTeam"]:
        t = box[side]
        for p in t["players"]:
            s = p["statistics"]
            rows.append(dict(gameId=gid, home=side == "homeTeam", teamId=t["teamId"], teamTricode=t["teamTricode"],
                             personId=p["personId"], name=f'{p["firstName"]} {p["familyName"]}', starter=bool(p.get("position")),
                             minutes=s.get("minutes"), plusMinus=s.get("plusMinusPoints"), points=s.get("points"),
                             comment=p.get("comment", "")))
    return pd.DataFrame(rows)


def season_block(season):
    """Everything that is one row per team for a season, as one dict of frames."""
    out = {}
    out["advanced"] = team_stats(season)
    out["possessions"] = pt_stats(season, "Possessions")
    out["passing"] = pt_stats(season, "Passing")
    out["shotclock"] = clock_shots(season, "leaguedashteamptshot")
    out["shotzones"] = shot_locations(season, "Base")
    out["defense"] = team_stats(season, MeasureType="Defense")
    out["fourfactors"] = team_stats(season, MeasureType="Four Factors")
    out["oppclock"] = clock_shots(season, "leaguedashoppptshot")
    out["opploc"] = shot_locations(season, "Opponent")
    if int(season[:4]) >= 2016:
        out["hustle"] = hustle(season)
    team_ids = out["advanced"]["TEAM_ID"].tolist()
    out["coaches"] = coaches(season, team_ids)
    out["lineups"] = pd.concat([lineups(season, int(t)) for t in team_ids], ignore_index=True)
    for k in out:
        out[k]["SEASON"] = season
    return out


def player_base(season, date_to=""):
    return fetch("leaguedashplayerstats", dict(
        College="", Conference="", Country="", DateFrom="", DateTo=date_to, Division="", DraftPick="", DraftYear="", GameScope="",
        GameSegment="", Height="", LastNGames=0, LeagueID="00", Location="", MeasureType="Base", Month=0, OpponentTeamID=0,
        Outcome="", PORound=0, PaceAdjust="N", PerMode="Totals", Period=0, PlayerExperience="", PlayerPosition="", PlusMinus="N",
        Rank="N", Season=season, SeasonSegment="", SeasonType=SEASON_TYPE, ShotClockRange="", StarterBench="", TeamID=0,
        TwoWay=0, VsConference="", VsDivision="", Weight=""))


def stale_check(name, n_games):
    d = pd.read_csv(path(name), low_memory=False)
    return "N_GAMES" not in d.columns or int(d["N_GAMES"].iloc[0]) != n_games


def early_cut(season, n_games):
    """The date by which the median team had played n_games, as MM/DD/YYYY."""
    gl = fetch("leaguegamelog", dict(Counter=5000, DateFrom="", DateTo="", Direction="ASC", LeagueID="00", PlayerOrTeam="T",
                                     Season=season, SeasonType=SEASON_TYPE, Sorter="DATE"))
    gl["GAME_DATE"] = pd.to_datetime(gl["GAME_DATE"])
    nth = gl.sort_values("GAME_DATE").groupby("TEAM_ID")["GAME_DATE"].apply(lambda d: d.iloc[min(n_games, len(d)) - 1])
    return nth.sort_values().iloc[len(nth) // 2].strftime("%m/%d/%Y")


def early_block(season, n_games):
    """The team tables the fingerprints use, for one season, through its first n_games."""
    cut = early_cut(season, n_games)
    out = {"advanced": team_stats(season, DateTo=cut), "passing": pt_stats(season, "Passing", cut)
           , "shotclock": clock_shots(season, "leaguedashteamptshot", cut), "shotzones": shot_locations(season, "Base", cut)
           , "fourfactors": team_stats(season, MeasureType="Four Factors", DateTo=cut)
           , "oppclock": clock_shots(season, "leaguedashoppptshot", cut), "opploc": shot_locations(season, "Opponent", cut)
           , "hustle": hustle(season, cut)}
    out["lineups"] = pd.concat([lineups(season, int(t), date_to=cut) for t in out["advanced"]["TEAM_ID"]], ignore_index=True)
    for k in out:
        out[k]["SEASON"] = season; out[k]["CUT_DATE"] = cut; out[k]["N_GAMES"] = n_games
    return out


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    print(f"== data folder {OUT_DIR}; current season {CUR}; refresh {REFRESH_CURRENT}", flush=True)

    # 1. the 14-season panel, one file per table, cached season by season
    tables = ["advanced", "possessions", "passing", "shotclock", "shotzones", "defense", "fourfactors", "oppclock", "opploc",
              "hustle", "coaches", "lineups"]
    existing = {t: (pd.read_csv(path(f"panel_{t}.csv"), low_memory=False) if have(f"panel_{t}.csv") else None) for t in tables}
    for season in PANEL_SEASONS:
        done = existing["advanced"] is not None and season in set(existing["advanced"]["SEASON"])
        if done and not (REFRESH_CURRENT and season == CUR):
            continue
        print(f"== panel {season}", flush=True)
        block = season_block(season)
        for t in tables:
            if t not in block:
                continue
            old = existing[t]
            if old is not None:
                old = old[old["SEASON"] != season]
                existing[t] = pd.concat([old, block[t]], ignore_index=True)
            else:
                existing[t] = block[t]
            save(existing[t], f"panel_{t}.csv")

    # 2. shots: Wizards for the three Keefe seasons, whole league for the current one
    for season in KEEFE_SEASONS:
        n = f"wizards_shots_{season}.csv"
        if not have(n) or (REFRESH_CURRENT and season == CUR):
            d = shots(season, WIZ); d["SEASON"] = season; save(d, n)
    if not have(f"league_shots_{CUR}.csv") or REFRESH_CURRENT:
        d = shots(CUR, 0); d["SEASON"] = CUR; save(d, f"league_shots_{CUR}.csv")

    # 3. Wizards game log (dates / results), 2-man lineups for the constellation
    for season in KEEFE_SEASONS:
        n = f"wizards_gamelog_{season}.csv"
        if not have(n) or (REFRESH_CURRENT and season == CUR):
            d = fetch("teamgamelog", dict(TeamID=WIZ, Season=season, SeasonType=SEASON_TYPE, LeagueID="00", DateFrom="", DateTo=""))
            d["SEASON"] = season; save(d, n)
    if not have(f"wizards_pairs_{CUR}.csv") or REFRESH_CURRENT:
        save(lineups(CUR, WIZ, 2), f"wizards_pairs_{CUR}.csv")
    # the Wizards' five-man lineups with their own and their opponents' shooting, for the three-point-luck adjustment by lineup type
    for m in ["Base", "Opponent"]:
        n = f"wizards_lineups_{m.lower()}_{CUR}.csv"
        if not have(n) or REFRESH_CURRENT:
            d = fetch("leaguedashlineups", dict(
                Conference="", DateFrom="", DateTo="", Division="", GameSegment="", GroupQuantity=5, LastNGames=0, LeagueID="00",
                Location="", MeasureType=m, Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N", PerMode="Totals",
                Period=0, PlusMinus="N", Rank="N", Season=CUR, SeasonSegment="", SeasonType=SEASON_TYPE, ShotClockRange="",
                TeamID=WIZ, VsConference="", VsDivision=""))
            d["SEASON"] = CUR; save(d, n)

    # 3b. every team's game dates (game ids are not in date order), for ordering games within a season
    for season in KEEFE_SEASONS:
        n = f"game_dates_{season}.csv"
        if not have(n) or season == CUR:
            gl = fetch("leaguegamelog", dict(Counter=5000, DateFrom="", DateTo="", Direction="ASC", LeagueID="00", PlayerOrTeam="T",
                                             Season=season, SeasonType=SEASON_TYPE, Sorter="DATE"))[["GAME_ID", "TEAM_ID", "GAME_DATE"]]
            gl["GAME_ID"] = gl["GAME_ID"].astype(str).str.zfill(10); save(gl, n)

    # 4. play-by-play and box scores, appended game by game (always resumes; picks up new games)
    # league-wide for all three Keefe seasons, so his in-game decisions can be ranked each year;
    # "all" seasons always top up, which also fills in a file first pulled Wizards-only
    plan = {KEEFE_SEASONS[0]: "all", PREV: "all", CUR: "all"}
    for season, scope in plan.items():
        if season == CUR or scope == "all" or not have(f"pbp_{season}.csv"):
            append_by_game(f"pbp_{season}.csv", season, scope, pbp_game)
    plan = {KEEFE_SEASONS[0]: "all", PREV: "all", CUR: "all"}
    for season, scope in plan.items():
        if season == CUR or scope == "all" or not have(f"boxscores_{season}.csv"):
            append_by_game(f"boxscores_{season}.csv", season, scope, box_game)

    # 5. player habits for the seasons before the last two (shot zones, fouls, touches), to ask how much of a
    #    team's change its new roster predicts. One row per player, whatever teams he played for.
    n = f"player_base_{CUR}.csv"
    if not have(n) or REFRESH_CURRENT:
        d = player_base(CUR); d["SEASON"] = CUR; save(d, n)
    for season in KEEFE_SEASONS[:2]:
        for name, fn in {"player_base": lambda s: player_base(s)
                         , "player_zones": lambda s: shot_locations(s, "Base", endpoint="leaguedashplayershotlocations")
                         , "player_poss": lambda s: pt_stats(s, "Possessions", who="Player")}.items():
            n = f"{name}_{season}.csv"
            if not have(n):
                d = fn(season); d["SEASON"] = season; save(d, n)

    # 6. early-season calibration: recent seasons cut at the Wizards' current games played (the cut date is
    #    when the median team reached that many). Re-pulled whenever the current season moves on.
    adv_cur = pd.read_csv(path("panel_advanced.csv"), low_memory=False)
    n_games = int(adv_cur.loc[(adv_cur["SEASON"] == CUR) & (adv_cur["TEAM_ID"] == WIZ), "GP"].iloc[0])
    # the previous season's player table cut at the same point, for the development measure's noise check
    n = f"player_base_early_{PREV}.csv"
    if not have(n) or stale_check(n, n_games):
        cut = early_cut(PREV, n_games); d = player_base(PREV, cut); d["SEASON"] = PREV; d["CUT_DATE"] = cut; d["N_GAMES"] = n_games; save(d, n)
    old = pd.read_csv(path("early_advanced.csv"), low_memory=False) if have("early_advanced.csv") else None
    stale = old is None or int(old["N_GAMES"].iloc[0]) != n_games
    for season in EARLY_SEASONS:
        if not stale and season in set(old["SEASON"]):
            continue
        print(f"== early {season} (first {n_games} games)", flush=True)
        block = early_block(season, n_games)
        for t, d in block.items():
            prev = None if stale and season == EARLY_SEASONS[0] else (pd.read_csv(path(f"early_{t}.csv"), low_memory=False) if have(f"early_{t}.csv") else None)
            if prev is not None:
                prev = prev[prev["SEASON"] != season]
            save(pd.concat([prev, d], ignore_index=True) if prev is not None else d, f"early_{t}.csv")
    print("done", flush=True)


if __name__ == "__main__":
    main()
)---"

find_python <- function() {
  venv_py <- file.path(".venv-nba", "bin", "python")
  if (file.exists(venv_py) && system2(venv_py, c("-c", shQuote("import curl_cffi, pandas")), stdout = FALSE, stderr = FALSE) == 0) return(venv_py)
  cands <- c("python3.12", "python3.13", "python3.11", "python3")
  for (p in cands) {
    exe <- Sys.which(p); if (!nzchar(exe)) next
    if (system2(exe, c("-c", shQuote("import curl_cffi, pandas")), stdout = FALSE, stderr = FALSE) == 0) return(exe)
  }
  base <- NULL
  for (p in cands) { exe <- Sys.which(p); if (nzchar(exe)) { base <- exe; break } }
  if (is.null(base)) stop("No python3 found; install Python 3.11+ (brew install python) and re-run.")
  message("Setting up ./.venv-nba with curl_cffi + pandas (one time) ...")
  if (nzchar(Sys.which("uv"))) {
    system2("uv", c("venv", "-q", ".venv-nba", "-p", "3.12"))
    system2("uv", c("pip", "install", "-q", "-p", venv_py, "curl_cffi", "pandas"))
  } else {
    system2(base, c("-m", "venv", ".venv-nba"))
    system2(venv_py, c("-m", "pip", "install", "-q", "curl_cffi", "pandas"))
  }
  if (system2(venv_py, c("-c", shQuote("import curl_cffi, pandas")), stdout = FALSE, stderr = FALSE) != 0) stop("virtualenv setup failed")
  venv_py
}

run_fetch <- function() {
  py <- find_python()
  f <- tempfile("nba_fetch_", fileext = ".py")
  writeLines(nba_fetch_py, f)
  args <- c(f, DATA_DIR, "--season", CUR, if (REFRESH) "--refresh-current")
  message("Pulling data from stats.nba.com (first run takes ~45 minutes; later runs only add new games) ...")
  status <- system2(py, args, stdout = "", stderr = "")
  if (status != 0) stop("the data pull failed; see messages above")
}
needed <- c("panel_advanced.csv", "panel_lineups.csv", "panel_coaches.csv", "early_advanced.csv", sprintf("league_shots_%s.csv", CUR), sprintf("pbp_%s.csv", CUR), sprintf("boxscores_%s.csv", CUR))
if (REFRESH || !all(file.exists(file.path(DATA_DIR, needed)))) run_fetch() else message("Using cached data in ", DATA_DIR, " (set REFRESH=1 to pull the latest games).")

# readers ------------------------------------------------------------------
rd <- function(stem) read_csv(file.path(DATA_DIR, paste0(stem, ".csv")), show_col_types = FALSE, guess_max = 5000) |> clean_names()
rd_season <- function(table, season) rd(paste0("panel_", table)) |> filter(season == !!season)
rd_games <- function(stem, season) read_csv(file.path(DATA_DIR, sprintf("%s_%s.csv", stem, season)), show_col_types = FALSE
                                            , col_types = cols(gameId = col_character(), .default = col_guess()), guess_max = 5000) |> clean_names()
# Coach of record. For a past season, commonteamroster returns the staff as of the FOLLOWING offseason
# (it lists Scott Brooks for 2015-16, Randy Wittman's last season, and Wes Unseld Jr. for 2020-21, Brooks's
# last), so the coach who started season s is the one listed for s-1. The current season uses today's listing.
# Mid-season firings are invisible this way: such a season is credited to the coach who started it.
# 2013-14 has no prior listing and stays NA, as do seasons whose prior listing is missing.
coach_tbl <- function() {
  raw <- rd("panel_coaches") |> group_by(team_id, season) |> slice(1) |> ungroup() |> select(team_id, listed = season, coach_name)
  next_season <- \(s) sprintf("%d-%02d", as.integer(substr(s, 1, 4)) + 1, (as.integer(substr(s, 1, 4)) + 2) %% 100)
  shifted <- raw |> filter(listed != CUR) |> transmute(team_id, season = next_season(listed), coach_name) |> filter(season != CUR)
  bind_rows(shifted, raw |> filter(listed == CUR) |> transmute(team_id, season = listed, coach_name))
}
SEASONS <- c(sprintf("%d-%02d", as.integer(substr(CUR, 1, 4)) - 2:0, (as.integer(substr(CUR, 1, 4)) - 2:0 + 1) %% 100))
PREV <- SEASONS[2]


# ===========================================================================
# 1. How long is a possession, when and where do the Wizards shoot
# ===========================================================================

read_all <- function(stem) {
  tbl <- c(team_advanced = "advanced", team_tracking_possessions = "possessions", team_tracking_passing = "passing", team_shotclock = "shotclock")[stem]
  if (!is.na(tbl)) return(rd(paste0("panel_", tbl)) |> filter(season %in% SEASONS))
  map_dfr(SEASONS, \(s) read_csv(file.path(DATA_DIR, sprintf("%s_%s.csv", stem, s)), show_col_types = FALSE)) |> clean_names()
}

adv  <- read_all("team_advanced")
poss <- read_all("team_tracking_possessions")
pass <- read_all("team_tracking_passing")
clk  <- read_all("team_shotclock")
gl   <- read_all("wizards_gamelog")
wshot <- read_all("wizards_shots")
lshot <- rd(paste0("league_shots_", CUR))

# ---------------------------------------------------------------------------
# 1. Possession length and passes per possession, all 30 teams, each season
# ---------------------------------------------------------------------------
team <- adv |>
  select(season, team_id, team_name, gp, w, l, off_rating, def_rating, net_rating
         , efg_pct, ts_pct, ast_pct, tm_tov_pct, pace, poss) |>
  left_join(poss |> select(season, team_id, team_abbreviation, time_of_poss, touches
                           , avg_sec_per_touch, avg_drib_per_touch, pts_per_touch
                           , paint_touches), by = c("season", "team_id")) |>
  left_join(pass |> select(season, team_id, passes_made, potential_ast, ast_points_created
                           , secondary_ast), by = c("season", "team_id")) |>
  # POSS comes back as a season total even with PerMode=PerGame; tracking stats are per game
  mutate(poss_pg = poss / gp
         , sec_per_poss = time_of_poss * 60 / poss_pg    # minutes of possession per game -> seconds per possession
         , passes_per_poss = passes_made / poss_pg
         , touches_per_poss = touches / poss_pg
         , is_wiz = team_id == WIZ_ID) |>
  group_by(season) |>
  mutate(across(c(sec_per_poss, passes_per_poss, pace, off_rating, net_rating, efg_pct, avg_sec_per_touch)
                , ~ rank(-.x), .names = "{.col}_rk_desc")) |>
  ungroup()

cat("\n== League possession length (seconds per offensive possession) ==\n")
pt(team |> group_by(season) |>
     summarise(teams = n(), mean_sec = mean(sec_per_poss), min_sec = min(sec_per_poss)
               , max_sec = max(sec_per_poss), mean_passes_per_poss = mean(passes_per_poss)
               , mean_pace = mean(pace)), digits = 3)

cat("\n== Wizards, by Keefe season (rank 1 = longest / most / highest) ==\n")
pt(team |> filter(is_wiz) |>
     transmute(season, gp, w, l, sec_per_poss, sec_rank = sec_per_poss_rk_desc
               , passes_per_poss, pass_rank = passes_per_poss_rk_desc
               , avg_sec_per_touch, pace, pace_rank = pace_rk_desc
               , off_rating, off_rank = off_rating_rk_desc, efg_pct, efg_rank = efg_pct_rk_desc
               , net_rating, net_rank = net_rating_rk_desc), digits = 3)

cat("\n== 2026-27, all teams sorted by possession length ==\n")
pt(team |> filter(season == "2026-27") |>
     arrange(desc(sec_per_poss)) |>
     transmute(team = team_abbreviation, gp, w, l, sec_per_poss, passes_per_poss, pace
               , off_rating, efg_pct, net_rating), digits = 3)

# ---------------------------------------------------------------------------
# 2. Does possession length / ball movement relate to outcomes? (full seasons)
# ---------------------------------------------------------------------------
cat("\n== Cross-team correlations with offensive rating, full seasons only ==\n")
pt(team |> filter(season != "2026-27") |>
     group_by(season) |>
     summarise(r_secpos_ortg = cor(sec_per_poss, off_rating)
               , r_passes_ortg = cor(passes_per_poss, off_rating)
               , r_pace_ortg = cor(pace, off_rating)
               , r_secpos_net = cor(sec_per_poss, net_rating)
               , r_passes_net = cor(passes_per_poss, net_rating)), digits = 2)

# ---------------------------------------------------------------------------
# 3. Shot-clock profile: share of FGA and eFG% by clock bucket
# ---------------------------------------------------------------------------
clk_lvls <- c("24-22", "22-18 Very Early", "18-15 Early", "15-7 Average", "7-4 Late", "4-0 Very Late")
clk <- clk |> mutate(shot_clock_range = factor(shot_clock_range, clk_lvls), is_wiz = team_id == WIZ_ID)

clk_sum <- clk |>
  group_by(season, shot_clock_range) |>
  summarise(league_efg = weighted.mean(efg_pct, fga)
            , wiz_freq = fga_frequency[is_wiz], wiz_efg = efg_pct[is_wiz]
            , wiz_freq_rank = rank(-fga_frequency)[is_wiz]
            , wiz_efg_rank = rank(-efg_pct)[is_wiz], .groups = "drop")
league_freq <- clk |> group_by(season) |> mutate(tot = sum(fga)) |>
  group_by(season, shot_clock_range) |> summarise(league_freq = sum(fga) / first(tot), .groups = "drop")
clk_sum <- clk_sum |> left_join(league_freq, by = c("season", "shot_clock_range")) |>
  relocate(league_freq, .after = shot_clock_range)

cat("\n== Shot-clock buckets: Wizards vs league (freq = share of FGA; rank 1 = highest of 30) ==\n")
pt(clk_sum |> arrange(season, shot_clock_range), digits = 3)

cat("\n== 2026-27 early-offense share (shots with 18+ on the clock), all teams ==\n")
pt(clk |> filter(season == "2026-27") |>
     group_by(team = team_abbreviation) |>
     summarise(early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
               , late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")])
               , early_efg = weighted.mean(efg_pct[shot_clock_range %in% c("24-22", "22-18 Very Early")]
                                           , fga[shot_clock_range %in% c("24-22", "22-18 Very Early")])
               , late_efg = weighted.mean(efg_pct[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]
                                          , fga[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")])
               , .groups = "drop") |>
     arrange(desc(early_share)), digits = 3)

p_clk <- clk_sum |>
  filter(season == "2026-27") |>
  pivot_longer(c(league_freq, wiz_freq), names_to = "who", values_to = "freq") |>
  mutate(who = if_else(who == "wiz_freq", "Wizards", "League")) |>
  ggplot(aes(shot_clock_range, freq, fill = who)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_fill_manual(values = c(Wizards = wiz_blue, League = "grey70"), name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(title = "When in the shot clock the Wizards shoot, 2026-27 (first 8 games)"
       , subtitle = "Share of field-goal attempts by seconds left on the shot clock"
       , x = "Shot clock remaining", y = "Share of FGA", caption = caption_text) +
  theme_wiz(16) + theme(legend.position = "top")
save_plot("shot_clock_profile_2026-27.png", p_clk)

# ---------------------------------------------------------------------------
# 4. Where in the quarter: seconds elapsed in period at the time of each shot
# ---------------------------------------------------------------------------
prep_shots <- function(d) {
  d |>
    mutate(period_len = if_else(period <= 4, 720, 300)
           , elapsed = period_len - (minutes_remaining * 60 + seconds_remaining)
           , elapsed_min = pmin(floor(elapsed / 60), 11)
           , pts_value = if_else(shot_type == "3PT Field Goal", 3, 2)
           , efg_pts = shot_made_flag * pts_value / 2
           , zone = case_when(shot_zone_basic == "Restricted Area" ~ "Restricted area"
                              , shot_zone_basic == "In The Paint (Non-RA)" ~ "Paint (non-RA)"
                              , shot_zone_basic == "Mid-Range" ~ "Mid-range"
                              , shot_zone_basic %in% c("Left Corner 3", "Right Corner 3") ~ "Corner 3"
                              , shot_zone_basic == "Above the Break 3" ~ "Above-break 3"
                              , TRUE ~ "Heave"))
}
wshot <- prep_shots(wshot)
lshot <- prep_shots(lshot) |> mutate(is_wiz = team_id == WIZ_ID)

cat("\n== 2026-27 Wizards: shots by minute of quarter (regulation periods) ==\n")
pt(wshot |> filter(season == "2026-27", period <= 4) |>
     count(elapsed_min) |> mutate(share = n / sum(n)), digits = 3)

cat("\n== 2026-27: Wizards vs rest of league, share of FGA and eFG by quarter segment ==\n")
seg <- function(m) case_when(m < 3 ~ "0-3 min", m < 6 ~ "3-6 min", m < 9 ~ "6-9 min", TRUE ~ "9-12 min")
pt(lshot |> filter(period <= 4) |>
     mutate(who = if_else(is_wiz, "Wizards", "Rest of league"), segment = seg(elapsed_min)) |>
     group_by(who, segment) |>
     summarise(fga = n(), efg = mean(efg_pts), .groups = "drop_last") |>
     mutate(share = fga / sum(fga)) |> ungroup() |>
     pivot_wider(names_from = who, values_from = c(fga, share, efg)), digits = 3)

cat("\n== 2026-27: Wizards vs rest of league, by period ==\n")
pt(lshot |>
     mutate(who = if_else(is_wiz, "Wizards", "Rest of league")) |>
     group_by(who, period) |>
     summarise(fga = n(), efg = mean(efg_pts), three_share = mean(pts_value == 3), .groups = "drop_last") |>
     mutate(share = fga / sum(fga)) |> ungroup() |>
     pivot_wider(names_from = who, values_from = c(fga, share, efg, three_share)), digits = 3)

p_min <- lshot |> filter(period <= 4) |>
  mutate(who = if_else(is_wiz, "Wizards", "Rest of league")) |>
  count(who, elapsed_min) |> group_by(who) |> mutate(share = n / sum(n)) |>
  ggplot(aes(elapsed_min + 0.5, share, colour = who)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_colour_manual(values = c(Wizards = wiz_blue, `Rest of league` = "grey60"), name = NULL) +
  scale_x_continuous(breaks = 0:12) + scale_y_continuous(labels = percent) +
  labs(title = "When in the quarter shots go up, 2026-27"
       , subtitle = "Share of each team's field-goal attempts by minute of the quarter (regulation only)"
       , x = "Minute of quarter", y = "Share of FGA", caption = caption_text) +
  theme_wiz(16) + theme(legend.position = "top")
save_plot("shots_by_minute_of_quarter_2026-27.png", p_min)

# ---------------------------------------------------------------------------
# 5. Where on the court
# ---------------------------------------------------------------------------
zone_lvls <- c("Restricted area", "Paint (non-RA)", "Mid-range", "Corner 3", "Above-break 3", "Heave")
cat("\n== Shot location: Wizards by season (share of FGA, eFG) ==\n")
pt(wshot |> mutate(zone = factor(zone, zone_lvls)) |>
     group_by(season, zone) |> summarise(fga = n(), efg = mean(efg_pts), .groups = "drop_last") |>
     mutate(share = fga / sum(fga)) |> ungroup() |>
     pivot_wider(names_from = season, values_from = c(fga, share, efg)), digits = 3)

cat("\n== Shot location 2026-27: Wizards vs rest of league ==\n")
pt(lshot |> mutate(zone = factor(zone, zone_lvls), who = if_else(is_wiz, "Wizards", "Rest of league")) |>
     group_by(who, zone) |> summarise(fga = n(), efg = mean(efg_pts), .groups = "drop_last") |>
     mutate(share = fga / sum(fga)) |> ungroup() |>
     pivot_wider(names_from = who, values_from = c(fga, share, efg)), digits = 3)

cat("\n== Shot location 2026-27: Wizards rank among 30 teams (1 = highest share) ==\n")
pt(lshot |> mutate(zone = factor(zone, zone_lvls)) |>
     count(team_id, zone) |> group_by(team_id) |> mutate(share = n / sum(n)) |>
     group_by(zone) |> mutate(rk = rank(-share)) |> ungroup() |>
     filter(team_id == WIZ_ID) |> select(zone, share, rank = rk), digits = 3)

# shot chart: Wizards 2026-27 density, half court (LOC_X/Y in tenths of feet, hoop at 0,0)
court <- function() {
  list(annotate("rect", xmin = -250, xmax = 250, ymin = -47.5, ymax = 422.5, fill = NA, colour = "grey40")
       , annotate("rect", xmin = -80, xmax = 80, ymin = -47.5, ymax = 142.5, fill = NA, colour = "grey40")
       , annotate("path", x = 237.5 * cos(seq(0.38, pi - 0.38, length.out = 100))
                  , y = 237.5 * sin(seq(0.38, pi - 0.38, length.out = 100)), colour = "grey40")
       , annotate("segment", x = c(-220, 220), xend = c(-220, 220), y = -47.5, yend = 92.5, colour = "grey40")
       , annotate("path", x = 7.5 * cos(seq(0, 2 * pi, length.out = 50)), y = 7.5 * sin(seq(0, 2 * pi, length.out = 50)), colour = "grey40")
       , annotate("segment", x = -30, xend = 30, y = -7.5, yend = -7.5, colour = "grey40")
       , annotate("path", x = 60 * cos(seq(0, pi, length.out = 50)), y = 142.5 + 60 * sin(seq(0, pi, length.out = 50)), colour = "grey40")
       , coord_fixed(xlim = c(-250, 250), ylim = c(-47.5, 422.5))
       , theme_void(base_size = 14, base_family = font))
}
p_court <- wshot |> filter(season == "2026-27", loc_y < 400) |>
  ggplot(aes(loc_x, loc_y)) +
  court() +
  geom_point(aes(colour = factor(shot_made_flag, labels = c("Missed", "Made"))), alpha = 0.6, size = 2) +
  scale_colour_manual(values = c(Missed = wiz_red, Made = wiz_blue), name = NULL) +
  labs(title = "Every Wizards shot, 2026-27 (first 8 games)", caption = caption_text) +
  theme(legend.position = "top")
save_plot("wizards_shot_chart_2026-27.png", p_court, width = 8, height = 8)

# ---------------------------------------------------------------------------
# 6. Keefe trend: pace, possession length, passes, early offense, results
# ---------------------------------------------------------------------------
early <- clk |> filter(is_wiz) |>
  group_by(season) |>
  summarise(early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]))
early_rk <- clk |>
  group_by(season, team_id) |>
  summarise(early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]), .groups = "drop") |>
  group_by(season) |> mutate(early_rank = rank(-early_share), late_rank = rank(-late_share)) |>
  ungroup() |> filter(team_id == WIZ_ID) |> select(season, early_rank, late_rank)

cat("\n== Keefe era summary ==\n")
pt(team |> filter(is_wiz) |>
     select(season, gp, w, l, pace, pace_rk = pace_rk_desc, sec_per_poss, sec_rk = sec_per_poss_rk_desc
            , passes_per_poss, pass_rk = passes_per_poss_rk_desc, off_rating, off_rk = off_rating_rk_desc
            , net_rating, net_rk = net_rating_rk_desc) |>
     left_join(early, by = "season") |> left_join(early_rk, by = "season"), digits = 3)

cat("\n== 2026-27 game-by-game ==\n")
pt(gl |> filter(season == "2026-27") |> select(game_date, matchup, wl, pts, fga, fg_pct, fg3a, fg3_pct, ast, tov), digits = 3)


# ===========================================================================
# 2. Shot quality vs shot making, the 24-second dial, the court difference map
# ===========================================================================
set.seed(202)

clk_lvls <- c("24-22", "22-18 Very Early", "18-15 Early", "15-7 Average", "7-4 Late", "4-0 Very Late")
clk_width <- c(2, 4, 3, 8, 3, 4)   # seconds in each bucket

# ===========================================================================
# PART A: 2026-27 — shot quality vs shot making
# ===========================================================================
lshot <- rd(paste0("league_shots_", CUR)) |>
  mutate(pts_value = if_else(shot_type == "3PT Field Goal", 3, 2)
         , efg_pts = shot_made_flag * pts_value / 2
         , cell = paste(shot_zone_basic, shot_zone_area, shot_zone_range)) |>
  filter(shot_zone_basic != "Backcourt")
abbrev <- rd_season("passing", CUR) |>
  select(team_id, team = team_abbreviation)
adv_cur <- rd_season("advanced", CUR)

# League eFG in each location cell -> expected eFG from location alone
cell_efg <- lshot |> group_by(cell) |> summarise(league_efg = mean(efg_pts), n = n(), .groups = "drop")
loc_quality <- lshot |>
  left_join(cell_efg, by = "cell") |>
  group_by(team_id) |>
  summarise(fga = n(), actual_efg = mean(efg_pts), expected_efg_loc = mean(league_efg), .groups = "drop") |>
  mutate(shot_making = actual_efg - expected_efg_loc) |>
  left_join(abbrev, by = "team_id")

# League eFG in each shot-clock bucket -> expected eFG from timing alone
clk_cur <- rd_season("shotclock", CUR) |>
  mutate(shot_clock_range = factor(shot_clock_range, clk_lvls))
bucket_efg <- clk_cur |> group_by(shot_clock_range) |> summarise(league_efg = weighted.mean(efg_pct, fga), .groups = "drop")
clk_quality <- clk_cur |>
  left_join(bucket_efg, by = "shot_clock_range") |>
  group_by(team_id) |>
  summarise(expected_efg_clock = sum(fga_frequency * league_efg), .groups = "drop")

quality <- loc_quality |> left_join(clk_quality, by = "team_id") |>
  left_join(adv_cur |> select(team_id, off_rating, w, l), by = "team_id") |>
  mutate(is_wiz = team_id == WIZ_ID)

cat("\n== 2026-27: shot quality (expected eFG from where / when) vs shot making (actual - expected) ==\n")
pt(quality |> arrange(desc(expected_efg_loc)) |>
     select(team, w, l, fga, expected_efg_loc, expected_efg_clock, actual_efg, shot_making, off_rating))
cat("\n== correlations across the 30 teams ==\n")
pt(tibble(r_loc_quality_ortg = cor(quality$expected_efg_loc, quality$off_rating)
          , r_clock_quality_ortg = cor(quality$expected_efg_clock, quality$off_rating)
          , r_making_ortg = cor(quality$shot_making, quality$off_rating)
          , r_loc_vs_clock_quality = cor(quality$expected_efg_loc, quality$expected_efg_clock)))
# the possession and shot numbers Appendix B quotes, kept under their own name for the write-up
appx_b <- list(
  league_sec = team |> filter(season == CUR) |> summarise(v = mean(sec_per_poss)) |> pull(v)
  , wiz = team |> filter(is_wiz) |> transmute(season, sec_per_poss, sec_rank = sec_per_poss_rk_desc, passes_per_poss, pass_rank = passes_per_poss_rk_desc, pace, pace_rank = pace_rk_desc)
  , r_secpos_ortg = team |> filter(season != CUR) |> group_by(season) |> summarise(r = cor(sec_per_poss, off_rating)) |> pull(r)
  , zones = wshot |> mutate(zone = factor(zone, zone_lvls)) |> count(season, zone) |> group_by(season) |> mutate(share = n / sum(n)) |> ungroup() |>
      left_join(prep_shots(rd(paste0("league_shots_", CUR))) |> count(team_id, zone) |> group_by(team_id) |> mutate(share = n / sum(n)) |> group_by(zone) |> mutate(rk = rank(-share)) |> ungroup() |>
                  filter(team_id == WIZ_ID) |> transmute(season = CUR, zone = as.character(zone), rank_now = rk), by = c("season", "zone"))
  , quality = quality |> mutate(rk_loc = rank(-expected_efg_loc)) |> filter(is_wiz) |> select(expected_efg_loc, rk_loc, shot_making, actual_efg)
  , r_loc_ortg = cor(quality$expected_efg_loc, quality$off_rating), r_making_ortg = cor(quality$shot_making, quality$off_rating)
  , clock_efg = clk_sum |> filter(season == CUR) |> select(shot_clock_range, league_efg) |> mutate(shot_clock_range = as.character(shot_clock_range)))

# --- Viz 1: the quadrant ---------------------------------------------------
qx <- mean(quality$expected_efg_loc); qy <- 0
p_quad <- ggplot(quality, aes(expected_efg_loc, shot_making)) +
  annotate("rect", xmin = qx, xmax = Inf, ymin = 0, ymax = Inf, fill = teal, alpha = 0.07) +
  annotate("rect", xmin = -Inf, xmax = qx, ymin = -Inf, ymax = 0, fill = wiz_red, alpha = 0.07) +
  geom_hline(yintercept = 0, colour = "grey60") + geom_vline(xintercept = qx, colour = "grey60") +
  annotate("text", x = Inf, y = Inf, label = "good shots, made them", hjust = 1.05, vjust = 1.5, colour = "grey40", fontface = "italic") +
  annotate("text", x = -Inf, y = Inf, label = "bad shots, made them anyway", hjust = -0.05, vjust = 1.5, colour = "grey40", fontface = "italic") +
  annotate("text", x = Inf, y = -Inf, label = "good shots, missed them", hjust = 1.05, vjust = -1, colour = "grey40", fontface = "italic") +
  annotate("text", x = -Inf, y = -Inf, label = "bad shots, missed them", hjust = -0.05, vjust = -1, colour = "grey40", fontface = "italic") +
  geom_point(aes(size = off_rating, colour = is_wiz), alpha = 0.85) +
  geom_text(aes(label = team, colour = is_wiz), size = 3.6, vjust = -1.1, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = wiz_blue, `FALSE` = "grey55"), guide = "none") +
  scale_size_continuous(range = c(2, 9), name = "Offensive rating") +
  scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1, style_positive = "plus")) +
  labs(title = "Shot selection is the coach's. Shot making is the roster's."
       , subtitle = "x: eFG% a league-average shooter would post from each team's shot locations (the quality of the shots taken)\ny: how far above or below that the team actually shot (the quality of the shooters). 2026-27, through Nov 5."
       , x = "Expected eFG% from shot location", y = "Actual minus expected eFG%", caption = caption_text) +
  theme_wp() + theme(legend.position = "bottom")
save_plot("shot_quality_vs_making_2026-27.png", p_quad, width = 11, height = 8.5)

# --- Viz 2: the 24-second dial ----------------------------------------------
dial <- clk_cur |>
  mutate(who = if_else(team_id == WIZ_ID, "Wizards", "League")) |>
  group_by(who, shot_clock_range) |>
  summarise(fga = sum(fga), fgm = sum(fgm), fg3m = sum(fg3m), .groups = "drop_last") |>
  mutate(share = fga / sum(fga), efg = (fgm + 0.5 * fg3m) / fga) |> ungroup() |>
  mutate(width = clk_width[as.integer(shot_clock_range)]
         , share_per_sec = share / width
         , end = cumsum(width)[as.integer(shot_clock_range)] - width + width   # seconds elapsed at bucket end
         , start = end - width
         , r0 = if_else(who == "League", 1, 2.15)
         , height = share_per_sec / max(share_per_sec) * 0.95)   # tallest bar ~ ring width
dial_lab <- tibble(sec = c(0, 6, 12, 18), lab = c("24", "18", "12", "6"))
ring_lab <- tibble(x = 12, y = c(1, 2.15), lab = c("league", "Wizards"), col = c("grey25", wiz_blue))
p_dial <- ggplot(dial) +
  geom_rect(aes(xmin = start, xmax = end, ymin = r0, ymax = r0 + height, fill = efg)
            , colour = "white", linewidth = 0.6) +
  geom_hline(yintercept = c(1, 2.15), colour = "grey75", linewidth = 0.3) +
  geom_text(data = dial_lab, aes(x = sec, y = 0.45, label = lab), size = 4.5, colour = "grey30", fontface = "bold") +
  annotate("text", x = 12, y = 0.05, label = "seconds left", size = 2.8, colour = "grey45") +
  geom_text(data = ring_lab, aes(x, y, label = lab, colour = col), size = 3.4, vjust = -0.3, fontface = "bold.italic", show.legend = FALSE) +
  scale_colour_identity() +
  coord_radial(start = 0, expand = FALSE, inner.radius = 0.15, r.axis.inside = FALSE) +
  scale_x_continuous(limits = c(0, 24), breaks = NULL) +
  scale_y_continuous(limits = c(0, 3.3), breaks = NULL) +
  scale_fill_gradient2(low = wiz_red, mid = "grey88", high = teal, midpoint = 0.53
                       , labels = label_percent(accuracy = 1), name = "eFG% in bucket") +
  labs(title = "The 24-second clock, as the Wizards use it"
       , subtitle = paste0("Rings run clockwise from 24 seconds left to 0. Bar height: shots per second of clock\n"
                           , "(how crowded that part of the clock is). Colour: eFG% on those shots.\n"
                           , "Inner ring: league. Outer ring: Wizards, 2026-27 through Nov 5.")
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp() + theme(legend.position = "right", panel.grid = element_blank(), axis.text = element_blank())
save_plot("shot_clock_dial_2026-27.png", p_dial, width = 10, height = 9)

# --- Viz 3: where the Wizards shoot more / less than everyone else ---------
court_layer <- function(col = "grey35") {
  list(annotate("rect", xmin = -250, xmax = 250, ymin = -47.5, ymax = 422.5, fill = NA, colour = col)
       , annotate("rect", xmin = -80, xmax = 80, ymin = -47.5, ymax = 142.5, fill = NA, colour = col)
       , annotate("path", x = 237.5 * cos(seq(0.38, pi - 0.38, length.out = 100))
                  , y = 237.5 * sin(seq(0.38, pi - 0.38, length.out = 100)), colour = col)
       , annotate("segment", x = c(-220, 220), xend = c(-220, 220), y = -47.5, yend = 92.5, colour = col)
       , annotate("path", x = 7.5 * cos(seq(0, 2 * pi, length.out = 50)), y = 7.5 * sin(seq(0, 2 * pi, length.out = 50)), colour = col)
       , annotate("path", x = 40 * cos(seq(0, pi, length.out = 50)), y = 40 * sin(seq(0, pi, length.out = 50)), colour = col, linetype = "dashed")
       , annotate("path", x = 60 * cos(seq(0, pi, length.out = 50)), y = 142.5 + 60 * sin(seq(0, pi, length.out = 50)), colour = col)
       , coord_fixed(xlim = c(-250, 250), ylim = c(-47.5, 350))
       , theme_void(base_size = 14))
}
bin <- 50   # tenths of feet -> 5 ft cells
court_diff <- lshot |>
  filter(loc_y < 350) |>
  mutate(who = if_else(team_id == WIZ_ID, "wiz", "lg")
         , bx = floor(loc_x / bin) * bin + bin / 2, by = floor(loc_y / bin) * bin + bin / 2) |>
  count(who, bx, by) |> group_by(who) |> mutate(share = n / sum(n)) |> ungroup() |>
  select(-n) |> pivot_wider(names_from = who, values_from = share, values_fill = 0) |>
  mutate(diff = (wiz - lg) * 100, ratio = log2((wiz + 1e-4) / (lg + 1e-4))) |>
  filter(wiz > 0 | lg > 0.001)
p_diff <- ggplot(court_diff, aes(bx, by)) +
  geom_tile(aes(fill = diff), width = bin, height = bin, colour = "white", linewidth = 0.3) +
  court_layer() +
  scale_fill_gradient2(low = wiz_red, mid = "grey95", high = wiz_blue, midpoint = 0
                       , name = "Wizards minus league,\n% of all shots", limits = c(-max(abs(court_diff$diff)), max(abs(court_diff$diff)))) +
  labs(title = "Where the Wizards shoot more (blue) and less (red) than the rest of the NBA"
       , subtitle = "Each 5-ft cell: Wizards' share of their shots minus the league's share of its shots, 2026-27 through Nov 5"
       , caption = caption_text) +
  theme(legend.position = "right", plot.title = element_text(face = "bold", size = 15)
        , plot.subtitle = element_text(colour = "grey30"), plot.title.position = "plot")
save_plot("court_difference_map_2026-27.png", p_diff, width = 10, height = 9)

cat("\nPart A figures written.\n")


# ===========================================================================
# 3. The coaching-change panel: what moves with the coach (offense), fingerprints
# ===========================================================================
set.seed(202)



p_adv <- rd("panel_advanced")
p_pas <- rd("panel_passing")
p_clk <- rd("panel_shotclock")
p_zon <- rd("panel_shotzones")
p_cch <- coach_tbl()   # one head coach of record per team-season

cat("zone columns:", paste(names(p_zon)[str_starts(names(p_zon), "fga_")], collapse = ", "), "\n")

zone_fga <- p_zon |>
  transmute(team_id, season
            , fga_total = rowSums(across(starts_with("fga_") & !matches("backcourt|^fga_corner_3$")))   # fga_corner_3 repeats left + right
            , rim_share = fga_restricted_area / fga_total
            , mid_share = fga_mid_range / fga_total
            , three_share = (fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3) / fga_total
            , corner_share = (fga_left_corner_3 + fga_right_corner_3) / fga_total)
clock_sum <- p_clk |>
  group_by(team_id, season) |>
  summarise(early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]), .groups = "drop")

panel <- p_adv |>
  select(team_id, team_name, season, gp, w, l, off_rating, def_rating, net_rating, efg_pct, pace, poss) |>
  left_join(p_pas |> select(team_id, season, team = team_abbreviation, passes_made), by = c("team_id", "season")) |>
  left_join(clock_sum, by = c("team_id", "season")) |>
  left_join(zone_fga, by = c("team_id", "season")) |>
  left_join(p_cch |> select(team_id, season, coach = coach_name), by = c("team_id", "season")) |>
  mutate(passes_per_poss = passes_made / (poss / gp)
         , season_start = as.integer(substr(season, 1, 4))) |>
  arrange(team_id, season_start)

metrics <- c(pace = "Pace", early_share = "Early-clock share (18+ s)", late_share = "Late-clock share (<7 s)"
             , passes_per_poss = "Passes per possession", rim_share = "Rim share", three_share = "3PA share"
             , mid_share = "Mid-range share", efg_pct = "eFG%", off_rating = "Offensive rating"
             , net_rating = "Net rating")
# standardise within season so league-wide trends (the 3PA boom) don't masquerade as coaching
panel_z <- panel |>
  group_by(season) |>
  mutate(across(all_of(names(metrics)), ~ (.x - mean(.x)) / sd(.x), .names = "z_{.col}")
         , across(all_of(names(metrics)), ~ rank(-.x), .names = "rk_{.col}")) |>   # league rank, 1 = most
  ungroup()

cat("\n== coaches of record, sanity check (Wizards) ==\n")
pt(panel |> filter(team_id == WIZ_ID) |> select(season, coach, w, l))

# --- B1. What moves when the coach changes? ---------------------------------
full <- panel_z |> filter(season != CUR, gp >= 60)
changes <- full |>
  group_by(team_id) |> arrange(season_start, .by_group = TRUE) |>
  mutate(prev_coach = lag(coach), prev_season = lag(season_start)
         , across(starts_with("z_"), ~ .x - lag(.x), .names = "d{.col}")) |>
  ungroup() |>
  filter(!is.na(prev_coach), season_start - prev_season == 1) |>
  mutate(new_coach = coach != prev_coach) |>
  filter(!is.na(new_coach))

cat("\n== team-seasons with no head coach of record from the roster endpoint ==\n")
pt(panel |> filter(is.na(coach)) |> select(team, season))
cat("\n== team-season pairs: ", nrow(changes), " (", sum(changes$new_coach), " with a new coach)\n", sep = "")

sens <- changes |>
  pivot_longer(starts_with("dz_"), names_to = "metric", values_to = "d") |>
  mutate(metric = str_remove(metric, "dz_")) |>
  group_by(metric, new_coach) |>
  summarise(mean_abs_d = mean(abs(d)), n = n(), .groups = "drop") |>
  pivot_wider(names_from = new_coach, values_from = c(mean_abs_d, n)) |>
  mutate(ratio = mean_abs_d_TRUE / mean_abs_d_FALSE, label = metrics[metric])

# bootstrap the ratio by resampling teams
boot <- map_dfr(1:1000, \(i) {
  ids <- sample(unique(changes$team_id), replace = TRUE)
  map_dfr(ids, \(id) changes |> filter(team_id == id)) |>
    pivot_longer(starts_with("dz_"), names_to = "metric", values_to = "d") |>
    group_by(metric, new_coach) |> summarise(m = mean(abs(d)), .groups = "drop") |>
    pivot_wider(names_from = new_coach, values_from = m) |>
    transmute(metric = str_remove(metric, "dz_"), ratio = `TRUE` / `FALSE`, i = i)
})
sens <- sens |> left_join(boot |> group_by(metric) |> summarise(lo = quantile(ratio, 0.05), hi = quantile(ratio, 0.95)), by = "metric")
cat("\n== Year-over-year change (|dz|) with a new coach vs the same coach ==\n")
pt(sens |> arrange(desc(ratio)) |> select(label, same_coach = mean_abs_d_FALSE, new_coach = mean_abs_d_TRUE, ratio, lo, hi))

p_sens <- sens |>
  mutate(label = fct_reorder(label, ratio), kind = if_else(metric %in% c("efg_pct", "off_rating", "net_rating"), "Outcome", "Process")) |>
  ggplot(aes(ratio, label)) +
  geom_vline(xintercept = 1, colour = "grey60", linetype = "dashed") +
  geom_segment(aes(x = lo, xend = hi, yend = label, colour = kind), linewidth = 1.2, alpha = 0.5) +
  geom_point(aes(colour = kind), size = 4.5) +
  geom_text(aes(label = sprintf("%.2fx", ratio)), vjust = -1.1, size = 3.4, colour = "grey30") +
  scale_colour_manual(values = c(Process = wiz_blue, Outcome = wiz_red), name = NULL) +
  labs(title = "A new coach changes the shape of the offense more than its output"
       , subtitle = paste0("How much each team stat moves from one season to the next when the head coach changes, relative to when he stays.\n"
                           , "1.0x = a coaching change is no more disruptive than a normal offseason. Stats standardised within season.\n"
                           , min(full$season), " to ", max(full$season), ", 60+ game seasons; bands are 90% bootstrap intervals.")
       , x = "Season-to-season change with a new coach / change with the same coach", y = NULL, caption = caption_text) +
  theme_wp() + theme(legend.position = "top")
save_plot("coach_change_sensitivity.png", p_sens, width = 11, height = 7.5)

# --- B2. Does the new coach actually move the outcome, net of regression to the mean?
cat("\n== Next-season value on this-season value + coaching change (z units) ==\n")
for (m in c("z_off_rating", "z_net_rating", "z_efg_pct", "z_early_share", "z_late_share", "z_pace", "z_three_share")) {
  d <- changes |> mutate(y = .data[[m]], x = .data[[m]] - .data[[paste0("d", m)]])
  fit <- lm(y ~ x + new_coach, data = d)
  cat(sprintf("%-16s persistence %.2f   new-coach effect %+.2f (se %.2f)\n", m, coef(fit)["x"]
              , coef(fit)["new_coachTRUE"], sqrt(diag(vcov(fit)))["new_coachTRUE"]))
}

# --- B3. Do tendencies travel with the coach? -------------------------------
travel <- full |>
  filter(!is.na(coach)) |>
  group_by(coach) |> filter(n_distinct(team_id) >= 2) |>
  group_by(coach, team_id) |>
  summarise(across(c(z_pace, z_early_share, z_late_share, z_passes_per_poss, z_rim_share, z_three_share, z_mid_share, z_efg_pct, z_off_rating), mean)
            , seasons = n(), .groups = "drop")
travel_cor <- travel |>
  pivot_longer(starts_with("z_"), names_to = "metric", values_to = "z") |>
  group_by(coach, metric) |>
  mutate(others = (sum(z) - z) / (n() - 1)) |> ungroup() |>
  group_by(metric) |>
  summarise(r_between_stints = cor(z, others), coaches = n_distinct(coach), stints = n(), .groups = "drop") |>
  mutate(metric = str_remove(metric, "z_"), label = metrics[metric])
cat("\n== Does a coach's tendency on one team predict it on his next team? (coaches with 2+ teams) ==\n")
pt(travel_cor |> arrange(desc(r_between_stints)) |> select(label, r_between_stints, coaches, stints))

p_travel <- travel |>
  pivot_longer(c(z_pace, z_early_share, z_late_share, z_three_share, z_off_rating), names_to = "metric", values_to = "z") |>
  mutate(metric = str_remove(metric, "z_"), label = factor(metrics[metric], levels = metrics[c("pace", "early_share", "late_share", "three_share", "off_rating")])
         , coach = fct_reorder(coach, z, .fun = \(v) mean(v))) |>
  ggplot(aes(z, coach)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_line(aes(group = coach), colour = "grey60") +
  geom_point(aes(colour = label), size = 2.6, alpha = 0.85, show.legend = FALSE) +
  facet_wrap(~ label, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = c(wiz_blue, teal, wiz_red, "#E1AF00", "grey30")) +
  labs(title = "Tendencies that travel with the coach, and one that doesn't"
       , subtitle = "Coaches who have led 2+ teams since 2013-14. Each dot is one stint (team-average z-score vs that season's league); the line joins a coach's stints.\nA tight line means the trait followed the coach. Offensive rating, on the right, mostly doesn't."
       , x = "z-score vs league (season-standardised)", y = NULL, caption = caption_text) +
  theme_wp(12) + theme(strip.text = element_text(face = "bold"), axis.text.y = element_text(size = 7.5))
save_plot("coach_traveling_tendencies.png", p_travel, width = 13, height = 9)

# --- B4. Coach fingerprints, 2026-27 ----------------------------------------
dims <- c(rk_pace = "Pace", rk_early_share = "Early\nshots", rk_late_share = "Few late\nshots", rk_passes_per_poss = "Passes\nper poss."
          , rk_rim_share = "Rim\nshots", rk_three_share = "Three-pt\nshots")
fp <- panel_z |> filter(season == CUR) |>
  select(team, coach, off_rating, w, l, all_of(names(dims))) |>
  mutate(rk_late_share = 31 - rk_late_share) |>    # flip so rank 1 = fewest late shots, the analytics-friendly end
  pivot_longer(all_of(names(dims)), names_to = "dim", values_to = "rank") |>
  mutate(dim = factor(dims[dim], levels = dims), r = 31 - rank
         , coach_lab = paste0(coach, "\n", team, " ", w, "-", l)
         , coach_lab = fct_reorder(coach_lab, -off_rating)
         , is_wiz = team == "WAS")
p_fp <- ggplot(fp, aes(dim, r, fill = is_wiz)) +
  geom_col(width = 1, colour = "white", linewidth = 0.4, alpha = 0.9) +
  geom_hline(yintercept = 15.5, colour = "grey40", linewidth = 0.3, linetype = "dotted") +
  coord_radial(expand = FALSE, inner.radius = 0.05) +
  scale_y_continuous(limits = c(0, 31), breaks = NULL) +
  scale_fill_manual(values = c(`TRUE` = wiz_blue, `FALSE` = "grey65"), guide = "none") +
  facet_wrap(~ coach_lab, ncol = 5) +
  labs(title = "Thirty offensive fingerprints, 2026-27"
       , subtitle = paste0("Each petal is a coach-controlled tendency as a league rank out of 30: a petal that reaches the edge is 1st in the NBA, one that barely shows is 30th (dotted ring = 15th).\n"
                           , "Pace; Early = share of shots with 18+ s on the clock; No late = fewer shots with <7 s (flipped so outward is good); Passes per possession;\n"
                           , "Rim = share of shots in the restricted area; Threes = 3PA share. Ordered by offensive rating. Through Nov 5.")
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp(11) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(size = 7)
        , strip.text = element_text(face = "bold", size = 8.5, lineheight = 0.9), panel.spacing.x = unit(2.2, "lines"), panel.spacing.y = unit(0.4, "lines"))
save_plot("coach_fingerprints_2026-27.png", p_fp, width = 11, height = 14)

# --- B5. Keefe's three fingerprints -----------------------------------------
keefe <- panel_z |> filter(team_id == WIZ_ID, season %in% c("2024-25", "2025-26", CUR)) |>
  select(season, coach, off_rating, w, l, all_of(names(dims))) |>
  mutate(rk_late_share = 31 - rk_late_share) |>
  pivot_longer(all_of(names(dims)), names_to = "dim", values_to = "rank") |>
  mutate(dim = factor(dims[dim], levels = dims), r = 31 - rank
         , lab = paste0(season, "  (", w, "-", l, ", ORtg ", round(off_rating, 1), ")"))
p_keefe <- ggplot(keefe, aes(dim, r)) +
  geom_col(width = 1, colour = "white", fill = wiz_blue, alpha = 0.9) +
  geom_hline(yintercept = 15.5, colour = wiz_red, linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(y = 38, label = paste0(dim, "\n", scales::ordinal(rank))), size = 3.1, colour = "grey15", family = font, lineheight = 0.85) +
  coord_radial(expand = FALSE, inner.radius = 0.05, clip = "off") +
  scale_y_continuous(limits = c(0, 40), breaks = NULL) +
  facet_wrap(~ lab) +
  labs(title = "Brian Keefe's three offenses"
       , subtitle = "Same petals as the league chart, as league ranks out of 30 (red ring = 15th). Every petal points outward\nwhen the offense does more of the thing its name says."
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp(13) + theme(panel.grid = element_blank(), axis.text.x = element_blank(), strip.text = element_text(face = "bold"), panel.spacing.x = unit(3, "lines"), plot.margin = margin(6, 20, 6, 20))
save_plot("keefe_fingerprints.png", p_keefe, width = 12, height = 5.5)

cat("\n== Keefe league ranks by season, 1 = most (late-clock not flipped here) ==\n")
pt(panel_z |> filter(team_id == WIZ_ID, season_start >= 2024) |>
     select(season, rk_pace, rk_early_share, rk_late_share, rk_passes_per_poss, rk_rim_share, rk_three_share, rk_mid_share, rk_efg_pct, rk_off_rating))

cat("\nPart B figures written.\n")


# ===========================================================================
# 4. Defense and the rotation: the same three tests, plus Keefe's defensive dials
# ===========================================================================
set.seed(202)
wiz_blue <- pal[["navy"]]; wiz_red <- pal[["red"]]; teal <- pal[["teal"]]; gold <- pal[["gold"]]

# ---------------------------------------------------------------------------
# Build the team-season panel
# ---------------------------------------------------------------------------
adv  <- rd("panel_advanced");   pas <- rd("panel_passing");  clk <- rd("panel_shotclock"); zon <- rd("panel_shotzones")
cch  <- coach_tbl()
dfn  <- rd("panel_defense");   ff  <- rd("panel_fourfactors")
oclk <- rd("panel_oppclock");  oloc <- rd("panel_opploc");   lu  <- rd("panel_lineups")
hus  <- if (file.exists(file.path(DATA_DIR, "panel_hustle.csv"))) rd("panel_hustle") else NULL

off_clock <- clk |> group_by(team_id, season) |>
  summarise(early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]), .groups = "drop")
def_clock <- oclk |> group_by(team_id, season) |>
  summarise(opp_early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , opp_late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]), .groups = "drop")
off_zone <- zon |> transmute(team_id, season, tot = rowSums(across(starts_with("fga_") & !matches("backcourt|^fga_corner_3$")))
                             , rim_share = fga_restricted_area / tot, mid_share = fga_mid_range / tot
                             , three_share = (fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3) / tot) |> select(-tot)
def_zone <- oloc |> transmute(team_id, season, tot = rowSums(across(starts_with("opp_fga_") & !matches("backcourt|^opp_fga_corner_3$")))
                              , opp_rim_share = opp_fga_restricted_area / tot, opp_mid_share = opp_fga_mid_range / tot
                              , opp_three_share = (opp_fga_left_corner_3 + opp_fga_right_corner_3 + opp_fga_above_the_break_3) / tot) |> select(-tot)

# lineup concentration: Herfindahl index of 5-man lineup minutes, top-lineup share, lineups with 5%+ of minutes
rot <- lu |> group_by(team_id, season) |>
  mutate(share = min / sum(min)) |>
  summarise(lineup_hhi = sum(share^2), top_lineup_share = max(share), n_lineups_5pct = sum(share >= 0.05)
            , lineups_used = n(), .groups = "drop")

panel <- adv |>
  select(team_id, team_name, season, gp, w, l, off_rating, def_rating, net_rating, efg_pct, pace, poss, min_pg = min) |>
  left_join(pas |> select(team_id, season, team = team_abbreviation, passes_made), by = c("team_id", "season")) |>
  left_join(off_clock, by = c("team_id", "season")) |> left_join(off_zone, by = c("team_id", "season")) |>
  left_join(def_clock, by = c("team_id", "season")) |> left_join(def_zone, by = c("team_id", "season")) |>
  left_join(dfn |> select(team_id, season, stl, blk, opp_pts_paint, opp_pts_fb, opp_pts_off_tov), by = c("team_id", "season")) |>
  left_join(ff |> select(team_id, season, opp_efg_pct, opp_fta_rate, opp_tov_pct, opp_oreb_pct), by = c("team_id", "season")) |>
  left_join(rot, by = c("team_id", "season")) |>
  left_join(cch |> select(team_id, season, coach = coach_name), by = c("team_id", "season")) |>
  mutate(passes_per_poss = passes_made / (poss / gp)
         , season_start = as.integer(substr(season, 1, 4))) |>
  arrange(team_id, season_start)
if (!is.null(hus)) panel <- panel |> left_join(hus |> select(team_id, season, deflections, contested_shots, charges_drawn), by = c("team_id", "season"))


groups <- tribble(
  ~metric, ~label, ~group,
  "pace", "Pace", "Offense: process",
  "early_share", "Early-clock share", "Offense: process",
  "late_share", "Late-clock share", "Offense: process",
  "passes_per_poss", "Passes per possession", "Offense: process",
  "three_share", "3PA share", "Offense: process",
  "rim_share", "Rim share", "Offense: process",
  "opp_three_share", "Opp. 3PA share allowed", "Defense: process",
  "opp_rim_share", "Opp. rim share allowed", "Defense: process",
  "opp_mid_share", "Opp. mid-range share allowed", "Defense: process",
  "opp_late_share", "Opp. forced to late clock", "Defense: process",
  "opp_early_share", "Opp. early-clock share", "Defense: process",
  "opp_fta_rate", "Opp. FTA rate (fouling)", "Defense: process",
  "opp_tov_pct", "Opp. turnover rate", "Defense: process",
  "opp_oreb_pct", "Opp. off. rebound rate", "Defense: process",
  "opp_pts_paint", "Opp. points in paint", "Defense: process",
  "stl", "Steals", "Defense: process",
  "blk", "Blocks", "Defense: process",
  "deflections", "Deflections", "Defense: process",
  "contested_shots", "Contested shots", "Defense: process",
  "lineup_hhi", "Lineup concentration (HHI)", "Rotation",
  "top_lineup_share", "Top lineup's minute share", "Rotation",
  "n_lineups_5pct", "Lineups with 5%+ of minutes", "Rotation",
    "efg_pct", "eFG%", "Outcome",
  "off_rating", "Offensive rating", "Outcome",
  "opp_efg_pct", "Opp. eFG%", "Outcome",
  "def_rating", "Defensive rating", "Outcome",
  "net_rating", "Net rating", "Outcome")
groups <- groups |> filter(metric %in% names(panel))
mets <- groups$metric

# League rank within each season (1 = the most of that thing among the 30 teams). Ranks rather than
# z-scores so every number reads as "Nth in the league" and a change reads as "places moved".
panel_z <- panel |> group_by(season) |>
  mutate(across(all_of(mets), ~ rank(-.x, na.last = "keep"), .names = "rk_{.col}")) |> ungroup()

# ---------------------------------------------------------------------------
# 1. What moves when the coach changes (ratio of |rank change| new coach / same coach)
# ---------------------------------------------------------------------------
full <- panel_z |> filter(season != CUR, gp >= 60, !is.na(coach))
changes <- full |> group_by(team_id) |> arrange(season_start, .by_group = TRUE) |>
  mutate(prev_coach = lag(coach), prev_season = lag(season_start), across(starts_with("rk_"), ~ .x - lag(.x), .names = "d{.col}")) |>
  ungroup() |> filter(!is.na(prev_coach), season_start - prev_season == 1) |> mutate(new_coach = coach != prev_coach)
cat("\npairs:", nrow(changes), " new-coach pairs:", sum(changes$new_coach), "\n")

ratio_fn <- function(d) d |>
  pivot_longer(starts_with("drk_"), names_to = "metric", values_to = "d") |>
  filter(!is.na(d)) |> mutate(metric = str_remove(metric, "drk_")) |>
  group_by(metric, new_coach) |> summarise(m = mean(abs(d)), .groups = "drop") |>
  pivot_wider(names_from = new_coach, values_from = m) |> transmute(metric, ratio = `TRUE` / `FALSE`)
sens <- ratio_fn(changes)
boot <- map_dfr(1:500, \(i) { ids <- sample(unique(changes$team_id), replace = TRUE)
  ratio_fn(map_dfr(ids, \(id) changes |> filter(team_id == id))) |> mutate(i = i) })
sens <- sens |> left_join(boot |> group_by(metric) |> summarise(lo = quantile(ratio, 0.05), hi = quantile(ratio, 0.95)), by = "metric") |>
  left_join(groups, by = "metric")

# ---------------------------------------------------------------------------
# 2. Does it travel with the coach?  (coaches with 2+ teams)
# ---------------------------------------------------------------------------
travel <- full |> group_by(coach) |> filter(n_distinct(team_id) >= 2) |>
  group_by(coach, team_id) |> summarise(across(starts_with("rk_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(starts_with("rk_"), names_to = "metric", values_to = "z") |> filter(!is.na(z)) |>
  group_by(coach, metric) |> filter(n() >= 2) |> mutate(others = (sum(z) - z) / (n() - 1)) |> ungroup() |>
  group_by(metric) |> summarise(r_travel = cor(z, others), stints = n(), .groups = "drop") |>
  mutate(metric = str_remove(metric, "rk_")) |> left_join(groups, by = "metric")

# ---------------------------------------------------------------------------
# 3. New-coach effect on next-season value, net of regression to the mean
# ---------------------------------------------------------------------------
nc <- map_dfr(mets, \(m) {
  d <- changes |> transmute(y = .data[[paste0("rk_", m)]], x = y - .data[[paste0("drk_", m)]], new_coach) |> filter(!is.na(y), !is.na(x))
  fit <- lm(y ~ x + new_coach, data = d)
  tibble(metric = m, persistence = coef(fit)["x"], new_coach_effect = coef(fit)["new_coachTRUE"], se = sqrt(diag(vcov(fit)))["new_coachTRUE"])
}) |> left_join(groups, by = "metric")

summary_tbl <- sens |> select(metric, label, group, ratio, lo, hi) |>
  left_join(travel |> select(metric, r_travel), by = "metric") |>
  left_join(nc |> select(metric, new_coach_effect, se), by = "metric") |>
  arrange(group, desc(ratio))
cat("\n== Coach sensitivity (ratio), travel correlation, and new-coach effect, by metric ==\n")
pt(summary_tbl)

# --- the big chart: three panels -------------------------------------------
grp_cols <- c(`Offense: process` = wiz_blue, `Defense: process` = "#7A6F8C", Rotation = gold, Outcome = wiz_red)
sens_long <- summary_tbl |> mutate(label = fct_reorder(label, ratio), group = factor(group, names(grp_cols)))
p1 <- ggplot(sens_long, aes(ratio, label, colour = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_segment(aes(x = lo, xend = hi, yend = label), linewidth = 1.1, alpha = 0.45) + geom_point(size = 3.6) +
  scale_colour_manual(values = grp_cols, name = NULL) +
  labs(title = "1. Moves when the coach changes", x = "Rank change, new coach vs same coach", y = NULL) +
  theme_wp(9) + theme(legend.position = "top", plot.title = element_text(size = rel(1.2)))
p2 <- ggplot(sens_long, aes(r_travel, label, colour = group)) +
  geom_vline(xintercept = 0, colour = "grey60") + geom_point(size = 3.6, show.legend = FALSE) +
  scale_colour_manual(values = grp_cols, guide = "none") +
  labs(title = "2. Travels with the coach", x = "Correlation between a coach's stints", y = NULL) +
  theme_wp(9) + theme(axis.text.y = element_blank(), plot.title = element_text(size = rel(1.2)))
# ranks run 1 = most; for outcomes flip to "places gained" so + is good (lower DRtg / opp eFG% is good, so those flip the other way)
sens_long <- sens_long |> mutate(dir = case_when(metric %in% c("off_rating", "net_rating", "efg_pct") ~ -1, metric %in% c("def_rating", "opp_efg_pct") ~ 1, TRUE ~ -1)
                                 , places = dir * new_coach_effect, places_lo = places - 1.64 * se, places_hi = places + 1.64 * se)
p3 <- ggplot(sens_long, aes(places, label, colour = group)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_segment(aes(x = places_lo, xend = places_hi, yend = label), linewidth = 1.1, alpha = 0.45) +
  geom_point(size = 3.6, show.legend = FALSE) + scale_colour_manual(values = grp_cols, guide = "none") +
  labs(title = "3. The year after a change", x = "League places moved\n(outcomes: + = better; process: + = more)", y = NULL) +
  theme_wp(9) + theme(axis.text.y = element_blank(), plot.title = element_text(size = rel(1.2)))
p_all <- (p1 | p2 | p3) + plot_layout(widths = c(1.6, 1, 1)) +
  plot_annotation(title = "What a head coach actually controls: offense, defense and the rotation"
                  , subtitle = paste0("Three tests on 30 teams x ", n_distinct(full$season), " seasons (", min(full$season), " to ", max(full$season)
                                      , ", 60+ games, everything as a league rank out of 30).\n"
                                      , "1: how much more a team's rank moves in a coaching-change offseason than in a normal one (1.0 = no more; bands are 90% bootstrap intervals).\n"
                                      , "2: whether a coach's rank on one team predicts his rank on his next team. 3: how many places a team moves the year after a change, once you account for bad teams bouncing back anyway.")
                  , caption = caption_text, theme = theme_wp(12))
save_plot("coach_control_offense_defense_rotation.png", p_all, width = 17, height = 11)
# phone-width version for the body of the post: just the first test, which carries the argument
p_moves <- ggplot(sens_long, aes(ratio, label, colour = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
  geom_segment(aes(x = lo, xend = hi, yend = label), linewidth = 1.4, alpha = 0.4) + geom_point(size = 4) +
  scale_colour_manual(values = grp_cols, name = NULL) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(title = "What moves when the coach changes"
       , subtitle = paste0("How much more a team's league rank moves in an offseason\nwith a new coach than with the same one. 1.0 = no more.\n"
                           , "Bars are 90% intervals. ", min(full$season), " to ", max(full$season), ", 30 teams.")
       , x = "New coach / same coach", y = NULL, caption = caption_text) +
  theme_wp(17) + theme(legend.position = "top", legend.justification = "left", axis.text.y = element_text(colour = "grey15", size = rel(0.8)))
save_plot("coach_change_moves.png", p_moves, width = 7.5, height = 11)

# ---------------------------------------------------------------------------
# 4. Keefe and the 2026-27 Wizards: defense and rotation
# ---------------------------------------------------------------------------
cat("\n== Wizards, Keefe seasons: defense process league ranks (1 = most of the thing; for def_rating 1 = worst) ==\n")
pt(panel_z |> filter(team_id == WIZ_ID, season_start >= 2023) |>
     select(season, w, l, def_rating, rk_def_rating, rk_opp_efg_pct, rk_opp_three_share, rk_opp_rim_share, rk_opp_late_share
            , rk_opp_fta_rate, rk_opp_tov_pct, rk_opp_oreb_pct, rk_opp_pts_paint, any_of(c("rk_deflections", "rk_contested_shots"))))
cat("\n== Wizards, Keefe seasons: rotation ==\n")
pt(panel_z |> filter(team_id == WIZ_ID, season_start >= 2023) |>
     select(season, lineups_used, n_lineups_5pct, top_lineup_share, lineup_hhi, rk_lineup_hhi, rk_top_lineup_share))

cat("\n== 2026-27 ranks among 30 (1 = most) ==\n")
pt(panel |> filter(season == CUR) |>
     mutate(across(c(def_rating, opp_efg_pct, opp_three_share, opp_rim_share, opp_late_share, opp_fta_rate, opp_tov_pct, opp_oreb_pct
                     , lineup_hhi, top_lineup_share, any_of(c("deflections", "contested_shots"))), ~ rank(-.x))) |>
     filter(team_id == WIZ_ID) |>
     select(def_rating_rank_worst_first = def_rating, opp_efg_pct, opp_three_share, opp_rim_share, opp_late_share, opp_fta_rate, opp_tov_pct, opp_oreb_pct
            , lineup_hhi, top_lineup_share, any_of(c("deflections", "contested_shots"))))

# defensive fingerprint: Wizards three seasons vs league, the coach-controlled defensive dials
ddims <- c(rk_opp_three_share = "Fewer 3s\nallowed", rk_opp_rim_share = "Fewer rim\nshots", rk_opp_late_share = "Opp. shoots\nlate"
           , rk_opp_fta_rate = "Fewer\nfouls", rk_opp_tov_pct = "Force\nturnovers", rk_opp_oreb_pct = "Few opp.\noff. rebs")
dfp <- panel_z |> filter(team_id == WIZ_ID, season_start >= 2024) |>
  select(season, w, l, def_rating, all_of(names(ddims))) |>
  # flip the ones where "more" is bad so every petal's rank 1 = the scheme-friendly end
  mutate(across(c(rk_opp_three_share, rk_opp_rim_share, rk_opp_fta_rate, rk_opp_oreb_pct), ~ 31 - .x)) |>
  pivot_longer(all_of(names(ddims)), names_to = "dim", values_to = "rank") |>
  mutate(dim = factor(ddims[dim], levels = ddims), r = 31 - rank, lab = paste0(season, "  (", w, "-", l, ", DRtg ", round(def_rating, 1), ")"))
p_dfp <- ggplot(dfp, aes(dim, r)) +
  geom_col(width = 1, colour = "white", fill = teal, alpha = 0.9) +
  geom_hline(yintercept = 15.5, colour = wiz_red, linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(y = 38, label = paste0(dim, "\n", ordinal(rank))), size = 3.1, colour = "grey15", family = font, lineheight = 0.85) +
  coord_radial(expand = FALSE, inner.radius = 0.05, clip = "off") + scale_y_continuous(limits = c(0, 40), breaks = NULL) +
  facet_wrap(~ lab) +
  labs(title = "Brian Keefe's three defenses", subtitle = "Six things a defensive scheme decides, as league ranks out of 30 (red ring = 15th). Every petal points outward when the defense is doing\nthe thing coaches ask for: a petal at the rim is 1st in the NBA at it, one that barely shows is 30th."
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp(13) + theme(panel.grid = element_blank(), axis.text.x = element_blank(), strip.text = element_text(face = "bold"), panel.spacing.x = unit(3, "lines"), plot.margin = margin(6, 20, 6, 20))
save_plot("keefe_defense_fingerprints.png", p_dfp, width = 13, height = 5.8)

# rotation: Wizards 2026-27 lineup minutes vs league distribution
lu_cur <- lu |> filter(season == CUR) |> group_by(team_id, team_abbreviation) |> arrange(desc(min), .by_group = TRUE) |>
  mutate(rank = row_number(), share = min / sum(min), cum = cumsum(share)) |> ungroup()
p_rot <- lu_cur |> filter(rank <= 15) |>
  ggplot(aes(rank, cum, group = team_abbreviation)) +
  geom_line(colour = "grey75", linewidth = 0.6) +
  geom_line(data = lu_cur |> filter(team_id == WIZ_ID, rank <= 15), colour = wiz_blue, linewidth = 1.6) +
  geom_point(data = lu_cur |> filter(team_id == WIZ_ID, rank <= 15), colour = wiz_blue, size = 2.5) +
  geom_text(data = lu_cur |> filter(rank == 15) |> mutate(lab = if_else(team_id == WIZ_ID | cum == max(cum) | cum == min(cum), team_abbreviation, "")),
            aes(label = lab), hjust = -0.2, size = 3.3) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) + scale_x_continuous(breaks = c(1, 5, 10, 15)) +
  labs(title = "How settled is the rotation?"
       , subtitle = "Share of all minutes taken by each team's most-used 5-man lineups, 2026-27.\nEach grey line is a team; blue is the Wizards.\nA steep line means the coach leans on a few lineups; a flat one means he is still searching.", x = "Lineups, ranked by minutes", y = "Share of all minutes", caption = caption_text) +
  theme_wp()
save_plot("rotation_concentration_2026-27.png", p_rot, width = 11, height = 7)

cat("\nDefense/rotation figures written.\n")


# ===========================================================================
# 5. Does the coach move minutes on last night's box score?
# ===========================================================================
box <- map_dfr(SEASONS, \(s) {
  if (file.exists(file.path(DATA_DIR, sprintf("boxscores_%s.csv", s)))) rd_games("boxscores", s) |> mutate(season = s)
}) |>
  mutate(mins = as.numeric(str_match(minutes, "^(\\d+)")[, 2]) + as.numeric(str_match(minutes, ":(\\d+)")[, 2]) / 60
         , mins = replace_na(mins, 0), played = mins > 0, dnp = !played) |>
  arrange(season, team_id, game_id)
coaches <- coach_tbl() |>
  select(team_id, season, coach = coach_name)

# game dates: nba.com game ids are not in date order, so every "game N" and every "previous game" uses the date
game_dates <- map_dfr(SEASONS, \(s) read_csv(file.path(DATA_DIR, sprintf("game_dates_%s.csv", s)), show_col_types = FALSE, col_types = cols(GAME_ID = col_character(), .default = col_guess())) |>
  clean_names() |> transmute(season = s, game_id, team_id, game_date = as.Date(game_date)))

# game order and result per team
games <- box |> group_by(season, game_id, team_id, team_tricode, home) |> summarise(pts = sum(points, na.rm = TRUE), .groups = "drop") |>
  group_by(season, game_id) |> mutate(opp_pts = sum(pts) - pts, won = pts > opp_pts) |> ungroup() |>
  left_join(game_dates, by = c("season", "game_id", "team_id")) |>
  group_by(season, team_id) |> arrange(game_date, game_id, .by_group = TRUE) |> mutate(g = row_number()) |> ungroup()
stopifnot(!any(is.na(games$game_date)))
box <- box |> left_join(games |> select(season, game_id, team_id, g, won), by = c("season", "game_id", "team_id"))
box_league <- box   # section 8 re-cuts these by games played; section 7 reuses the name box

# ---------------------------------------------------------------------------
# 1. Minutes next game vs plus-minus this game
# ---------------------------------------------------------------------------
pm <- box |> filter(played) |>
  group_by(season, team_id, person_id) |> arrange(g, .by_group = TRUE) |>
  mutate(avg_mins = cummean(mins), next_mins = lead(mins), next_g = lead(g), next_played = lead(played)) |> ungroup() |>
  filter(!is.na(next_mins), next_g == g + 1, mins >= 8) |>
  mutate(d_mins = next_mins - mins, above_usual = mins - avg_mins)   # how far this game's minutes sat above the player's running average

# within-team regression: d_mins ~ plus_minus + above_usual
fit_team <- function(d) {
  if (nrow(d) < 25) return(tibble(slope = NA_real_, se = NA_real_, n = nrow(d)))
  f <- lm(d_mins ~ plus_minus + above_usual, data = d)
  tibble(slope = coef(f)[["plus_minus"]], se = sqrt(diag(vcov(f)))[["plus_minus"]], n = nrow(d))
}
resp <- pm |> group_by(season, team_id, team_tricode) |> group_modify(~ fit_team(.x)) |> ungroup() |>
  left_join(coaches, by = c("season", "team_id")) |>
  mutate(per10 = slope * 10, lo = (slope - 1.64 * se) * 10, hi = (slope + 1.64 * se) * 10)   # minutes next game per +10 plus-minus
league_fit <- lm(d_mins ~ plus_minus + above_usual, data = pm |> filter(season == CUR))
cat("\n== League 2026-27: minutes next game per +10 plus-minus (all teams pooled):", round(coef(league_fit)[["plus_minus"]] * 10, 2), "\n")
cat("\n== 2026-27 by coach: minutes added next game per +10 plus-minus tonight (90% interval) ==\n")
pt(resp |> filter(season == CUR) |> arrange(desc(per10)) |> select(team_tricode, coach, n, per10, lo, hi))
cat("\n== Wizards, Keefe seasons ==\n")
pt(resp |> filter(team_id == WIZ_ID) |> select(season, coach, n, per10, lo, hi))

# ---------------------------------------------------------------------------
# 2. Starting lineups: how many, how often they change, and after what
# ---------------------------------------------------------------------------
starts <- box |> filter(starter) |> group_by(season, team_id, team_tricode, game_id, g, won) |>
  summarise(five = paste(sort(person_id), collapse = "-"), .groups = "drop") |>
  group_by(season, team_id) |> arrange(g, .by_group = TRUE) |>
  mutate(changed = five != lag(five), prev_won = lag(won)) |> ungroup()
start_sum <- starts |> group_by(season, team_id, team_tricode) |>
  summarise(games = n(), distinct_fives = n_distinct(five), changes = sum(changed, na.rm = TRUE)
            , change_after_loss = mean(changed[!prev_won], na.rm = TRUE), change_after_win = mean(changed[prev_won], na.rm = TRUE), .groups = "drop") |>
  mutate(changes_per_10 = 10 * changes / pmax(games - 1, 1)) |> left_join(coaches, by = c("season", "team_id"))
cat("\n== 2026-27 starting fives by team ==\n")
pt(start_sum |> filter(season == CUR) |> arrange(desc(changes_per_10)) |> select(team_tricode, coach, games, distinct_fives, changes, changes_per_10, change_after_loss, change_after_win))
cat("\n== Wizards, Keefe seasons ==\n")
pt(start_sum |> filter(team_id == WIZ_ID) |> select(season, games, distinct_fives, changes, changes_per_10, change_after_loss, change_after_win))

# rotation size: players averaging 10+ minutes over the games they played in
rot <- box |> filter(played) |> group_by(season, team_id, team_tricode, person_id) |> summarise(mpg = mean(mins), gp = n(), .groups = "drop") |>
  group_by(season, team_id, team_tricode) |> summarise(rotation_10 = sum(mpg >= 10), rotation_20 = sum(mpg >= 20), .groups = "drop")
cat("\n== Wizards rotation size, Keefe seasons (players averaging 10+ / 20+ minutes) ==\n")
pt(rot |> filter(team_id == WIZ_ID))
cat("\n== 2026-27 rotation size, league ranks (1 = deepest) ==\n")
pt(rot |> filter(season == CUR) |> mutate(rk = rank(-rotation_10, ties.method = "min")) |> arrange(rk) |> select(team_tricode, rotation_10, rotation_20, rk))

# ---------------------------------------------------------------------------
# Viz 1: the coach's hand — minutes responsiveness by coach, 2026-27, with Keefe's history
# ---------------------------------------------------------------------------
rp <- resp |> filter(season == CUR, !is.na(slope)) |>
  mutate(lab = paste0(coach, " (", team_tricode, ")"), is_wiz = team_id == WIZ_ID, lab = fct_reorder(lab, per10))
keefe_hist <- resp |> filter(team_id == WIZ_ID, season != CUR)
p_resp <- ggplot(rp, aes(per10, lab)) +
  geom_vline(xintercept = 0, colour = grey_text) +
  geom_vline(xintercept = coef(league_fit)[["plus_minus"]] * 10, colour = teal, linetype = "dashed") +
  geom_segment(aes(x = lo, xend = hi, yend = lab, colour = is_wiz), linewidth = 1.1, alpha = 0.5) +
  geom_point(aes(colour = is_wiz), size = 3.4) +
  geom_point(data = keefe_hist |> mutate(lab = rp$lab[rp$is_wiz][1]), aes(per10, lab), colour = wiz_red, shape = 1, size = 4, stroke = 1.2) +
  geom_text(data = keefe_hist |> mutate(lab = rp$lab[rp$is_wiz][1], vj = if_else(season == "2024-25", 2.2, -1.2)), aes(per10, lab, label = season, vjust = vj), colour = wiz_red, size = 3.2, family = font) +
  scale_colour_manual(values = c(`TRUE` = wiz_blue, `FALSE` = "grey55"), guide = "none") +
  scale_x_continuous(labels = label_number(style_positive = "plus", suffix = " min")) +
  labs(title = "Who plays tomorrow depends on tonight. How much?"
       , subtitle = paste0("Minutes a player gains in his next game for every +10 in plus-minus tonight, after allowing for the usual drift back to his normal minutes.\n"
                           , "Positive = the coach rewards a good night (and benches a bad one). 2026-27 by head coach, players with 8+ minutes; bars are 90% intervals.\n"
                           , "Dashed line = league as a whole. Red rings: Keefe's two previous Wizards seasons.")
       , x = "Minutes next game, per +10 plus-minus tonight", y = NULL, caption = caption_text) +
  theme_wp(12) + theme(axis.text.y = element_text(colour = "black", size = rel(0.85)))
save_plot("minutes_responsiveness_2026-27.png", p_resp, width = 12, height = 10)

# ---------------------------------------------------------------------------
# Viz 2: the Wizards' minutes ledger — every player, every game, with that night's plus-minus
# ---------------------------------------------------------------------------
wb <- box |> filter(season == CUR, team_id == WIZ_ID) |>
  group_by(person_id) |> mutate(tot = sum(mins)) |> ungroup() |> filter(tot >= 20) |>
  mutate(short = paste0(str_sub(word(name, 1), 1, 1), ". ", word(name, -1)), short = fct_reorder(short, tot)) |>
  group_by(person_id) |> arrange(g, .by_group = TRUE) |> mutate(d_prev = mins - lag(mins), cut = d_prev <= -6, bump = d_prev >= 6) |> ungroup() |>
  left_join(games |> filter(season == CUR, team_id == WIZ_ID) |> transmute(game_id, glab = paste0("G", g, "\n", if_else(won, "W", "L"))), by = "game_id") |>
  mutate(glab = fct_reorder(glab, g))
p_ledger <- ggplot(wb, aes(glab, short)) +
  geom_tile(aes(fill = mins), colour = "white", linewidth = 1) +
  geom_tile(data = wb |> filter(cut), fill = NA, colour = wiz_red, linewidth = 1.6) +
  geom_tile(data = wb |> filter(bump), fill = NA, colour = teal_dark, linewidth = 1.6) +
  geom_text(aes(label = if_else(played, sprintf("%.0f'\n%+d", mins, as.integer(plus_minus)), "DNP")
                , colour = if_else(mins > 22, "white", "black")), size = 3.1, lineheight = 0.85, family = font) +
  scale_colour_identity() +
  scale_fill_gradient(low = cream, high = wiz_blue, name = "Minutes", limits = c(0, 42)) +
  scale_x_discrete(position = "top") +
  labs(title = "The minutes ledger"
       , subtitle = paste0("Every Wizard, every game: minutes (top) and plus-minus that night (bottom). A red frame means he lost 6+ minutes from his previous game;\n"
                           , "a teal frame means he gained 6+. If Keefe were managing by last night's box score, red frames would follow negative numbers. 2026-27 through Nov 5.")
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp(12) + theme(legend.position = "right", axis.text.y = element_text(colour = "black"), axis.text.x.top = element_text(colour = "black", lineheight = 0.9))
save_plot("minutes_ledger_2026-27.png", p_ledger, width = 13, height = 9)

cat("\nLineup-response figures written.\n")


# ===========================================================================
# 6. Timeouts, after-timeout plays and coach's challenges, from play-by-play
# ===========================================================================
pbp <- map_dfr(SEASONS, \(s) {
  if (file.exists(file.path(DATA_DIR, sprintf("pbp_%s.csv", s)))) rd_games("pbp", s) |> mutate(season = s)
})

# game clock -> seconds elapsed in game
pbp <- pbp |>
  mutate(clock_sec = as.numeric(str_match(clock, "PT(\\d+)M")[, 2]) * 60 + as.numeric(str_match(clock, "M([\\d.]+)S")[, 2])
         , period_len = if_else(period <= 4, 720, 300)
         , elapsed = if_else(period <= 4, (period - 1) * 720, 2880 + (period - 5) * 300) + (period_len - clock_sec)
         , score_home = as.numeric(score_home), score_away = as.numeric(score_away)) |>
  arrange(season, game_id, action_number) |>
  group_by(season, game_id) |> fill(score_home, score_away, .direction = "down") |>
  mutate(score_home = replace_na(score_home, 0), score_away = replace_na(score_away, 0)) |> ungroup()

# timeout rows have teamId = 0 and no tricode in playbyplayv3; the team id rides in personId (team ids are ~1.61e9).
# Fallback: the team nickname that opens the description ("WIZARDS Timeout: ...").
nick <- rd("panel_advanced") |>
  distinct(team_id, team_name) |> mutate(nick = str_to_lower(word(team_name, -1))) |> distinct(nick, tid = team_id)
pbp <- pbp |>
  mutate(team_id = if_else((is.na(team_id) | team_id == 0) & !is.na(person_id) & person_id > 1e9, person_id, team_id)
         , nick = if_else(action_type == "Timeout", str_to_lower(word(description, 1)), NA_character_)) |>
  left_join(nick, by = "nick") |>
  mutate(team_id = if_else((is.na(team_id) | team_id == 0) & !is.na(tid), tid, team_id)) |> select(-tid, -nick)
tri_map <- pbp |> filter(!is.na(team_id), team_id > 0, !is.na(team_tricode)) |> distinct(season, game_id, tid = team_id, team_tricode) |>
  group_by(season, game_id, tid) |> slice(1) |> ungroup()
pbp <- pbp |> left_join(tri_map, by = c("season", "game_id", "team_id" = "tid"), suffix = c("", "_fill")) |>
  mutate(team_tricode = coalesce(team_tricode, team_tricode_fill)) |> select(-team_tricode_fill)
cat("timeout rows with a team id:", mean(pbp$team_id[pbp$action_type == "Timeout"] > 0, na.rm = TRUE), "\n")

# home / away team ids: the team whose made shots move scoreHome is home
home_map <- pbp |> filter(action_type %in% c("Made Shot", "Free Throw"), !is.na(team_id), team_id > 0) |>
  group_by(season, game_id) |> arrange(action_number, .by_group = TRUE) |>
  mutate(dh = score_home - lag(score_home), da = score_away - lag(score_away)) |>
  filter(!is.na(dh)) |>
  summarise(home_id = as.numeric(names(which.max(table(team_id[dh > 0])))), away_id = as.numeric(names(which.max(table(team_id[da > 0])))), .groups = "drop")
pbp <- pbp |> left_join(home_map, by = c("season", "game_id"))
# shots and free throws by team and game, with and without the last two minutes of the fourth quarter and overtime
# (where intentional fouls live), for the fouling check in section 8
ft_split <- pbp |> filter(action_type %in% c("Made Shot", "Missed Shot", "Free Throw"), !is.na(team_id), team_id > 0) |>
  mutate(late = period >= 4 & clock_sec <= 120, is_ft = action_type == "Free Throw") |>
  group_by(season, game_id, team_id) |>
  summarise(fga = sum(!is_ft), fta = sum(is_ft), fga_early = sum(!is_ft & !late), fta_early = sum(is_ft & !late), .groups = "drop")

# every scoring event, with points for each side, to compute runs around a timestamp
scores <- pbp |> filter(action_type %in% c("Made Shot", "Free Throw"), !is.na(team_id), team_id > 0) |>
  group_by(season, game_id) |> arrange(action_number, .by_group = TRUE) |>
  mutate(pts = (score_home + score_away) - lag(score_home + score_away, default = 0)) |> ungroup() |>
  filter(pts > 0) |> select(season, game_id, elapsed, team_id, pts)

# --- Timeouts -----------------------------------------------------------------
# Coach's challenges: the challenger must spend a timeout, logged a few actions before the "Instant Replay" row
# (sub_type "Coach Challenge" when the challenge wins, usually "Regular" when it loses). Link each replay row to
# the nearest preceding timeout, at most 3 actions back, and credit that timeout's team. (The call on the floor is
# no guide: after an overturn the preceding foul usually belongs to the challenger's opponent.) Those timeouts
# are spent on a challenge, not called by choice, so they also come out of the timeout counts below.
pbp_i <- pbp |> group_by(season, game_id) |> mutate(i = row_number()) |> ungroup()
chal_link <- pbp_i |> filter(action_type == "Instant Replay", str_starts(sub_type, "Coach Challenge")) |>
  select(season, game_id, r_i = i, r_action = action_number) |>
  inner_join(pbp_i |> filter(action_type == "Timeout", !is.na(team_id), team_id > 0) |> select(season, game_id, t_i = i, t_action = action_number, chal_team = team_id)
             , by = c("season", "game_id"), relationship = "many-to-many") |>
  filter(t_i < r_i, t_i >= r_i - 3) |>
  group_by(season, game_id, r_action) |> slice_max(t_i, n = 1, with_ties = FALSE) |> ungroup()
rm(pbp_i)

to <- pbp |> filter(action_type == "Timeout", sub_type == "Regular", !is.na(team_id), team_id > 0) |>
  anti_join(chal_link, by = c("season", "game_id", "action_number" = "t_action")) |>
  mutate(margin = if_else(team_id == home_id, score_home - score_away, score_away - score_home)
         , opp_id = if_else(team_id == home_id, away_id, home_id)
         , time_left = (if_else(period <= 4, 2880, 2880 + (period - 4) * 300)) - elapsed) |>
  select(season, game_id, action_number, team_id, team_tricode, opp_id, period, clock_sec, elapsed, time_left, margin, description)

W <- 180   # window, seconds of game clock
window_net <- function(d, before = TRUE) {
  # net points (team - opp) scored within W seconds before / after each timeout
  d |> select(season, game_id, action_number, team_id, elapsed) |>
    left_join(scores, by = c("season", "game_id"), relationship = "many-to-many", suffix = c("", "_s")) |>
    filter(if (before) elapsed_s < elapsed & elapsed_s >= elapsed - W else elapsed_s > elapsed & elapsed_s <= elapsed + W) |>
    group_by(season, game_id, action_number) |>
    summarise(own = sum(pts[team_id_s == team_id]), opp = sum(pts[team_id_s != team_id]), .groups = "drop") |>
    mutate(net = own - opp)
}
to <- to |>
  left_join(window_net(to, TRUE) |> rename(own_before = own, opp_before = opp, net_before = net), by = c("season", "game_id", "action_number")) |>
  left_join(window_net(to, FALSE) |> rename(own_after = own, opp_after = opp, net_after = net), by = c("season", "game_id", "action_number")) |>
  mutate(across(c(own_before, opp_before, net_before, own_after, opp_after, net_after), ~ replace_na(.x, 0))
         , opp_run = opp_before - own_before                      # how big an opponent run preceded it
         , stop_the_bleeding = opp_run >= 6                       # the classic "timeout to stop a run"
         , possibly_mandatory = clock_sec <= 420 & clock_sec > 360 | clock_sec <= 180 & clock_sec > 120   # under 7:00 / 3:00 TV windows
         , swing = net_after - net_before)

cat("\n== timeouts parsed by season ==\n")
pt(to |> count(season, name = "timeouts"))

cat("\n== League, 2025-26 full season: what precedes a timeout and what follows ==\n")
pt(to |> filter(season == PREV) |>
     summarise(n = n(), share_after_run6 = mean(stop_the_bleeding), mean_opp_run_before = mean(opp_run)
               , n_run = sum(stop_the_bleeding), net_after_when_run = mean(net_after[stop_the_bleeding]), net_before_when_run = mean(net_before[stop_the_bleeding])
               , net_before_all = mean(net_before), net_after_all = mean(net_after), mean_swing = mean(swing)))

# The after-a-run swing is mostly regression to the mean: compare to the same window after *any* dead ball following a run.
# Cheap control: opponent 6-0 runs where no timeout was called in the next 30 s.
runs_ctl <- scores |> filter(season == PREV) |>
  group_by(season, game_id) |> arrange(elapsed, .by_group = TRUE) |> ungroup()
ctl_pts <- to |> filter(season == PREV) |> select(season, game_id, t_elapsed = elapsed)
# sample dead-ball points: every made basket by the opponent that completes a 6-0 (or more) run in the prior 3 minutes
ctl <- scores |> filter(season == PREV) |>
  select(season, game_id, elapsed, scorer = team_id) |>
  left_join(home_map, by = c("season", "game_id")) |>
  mutate(team_id = if_else(scorer == home_id, away_id, home_id), action_number = elapsed) |>      # the team being run on
  select(season, game_id, action_number, team_id, elapsed)
ctl <- ctl |> left_join(window_net(ctl, TRUE) |> rename(net_before = net), by = c("season", "game_id", "action_number")) |>
  filter(replace_na(net_before, 0) <= -6) |>
  anti_join(ctl_pts |> mutate(lo = t_elapsed - 30, hi = t_elapsed + 30), by = join_by(season, game_id, between(elapsed, lo, hi))) |>
  left_join(window_net(ctl, FALSE) |> rename(net_after = net), by = c("season", "game_id", "action_number")) |>
  mutate(net_after = replace_na(net_after, 0))
cat("\n== Control: 6-0+ runs with NO timeout in the next 30 s (2025-26) ==\n")
pt(ctl |> summarise(n = n(), net_before = mean(net_before), net_after = mean(net_after)))
cat("    vs timeouts called on a 6-0+ run:\n")
pt(to |> filter(season == PREV, stop_the_bleeding) |> summarise(n = n(), net_before = mean(net_before), net_after = mean(net_after)))

# --- Timeouts per run faced ------------------------------------------------------
# The share of a coach's timeouts that come on an opponent run mostly measures how often his team gets run on
# (bad teams get run on more). The coach's own choice is the response: of the runs his team faces, how many
# draw a timeout? A "run" here is the opponent reaching a net 6+ over the previous 3 minutes; triggers less than
# 60 s apart for the same team are one run. Likely mandatory timeouts are dropped: when nobody calls one before
# 7:00, the first stoppage after it is charged to the home team, so the first timeout of the 7:00-3:00 window,
# charged to the home team in a quarter with no earlier timeout, is treated as the TV's, not the coach's.
to_mand <- to |> select(season, game_id, action_number, team_id, period, clock_sec, elapsed) |>
  left_join(home_map, by = c("season", "game_id")) |>
  group_by(season, game_id, period) |> arrange(elapsed, .by_group = TRUE) |>
  mutate(seg = case_when(clock_sec > 420 ~ 1, clock_sec > 180 ~ 2, TRUE ~ 3), none_before = !any(seg == 1)) |>
  group_by(season, game_id, period, seg) |> mutate(first_in_seg = row_number() == 1) |> ungroup() |>
  mutate(likely_mandatory = coalesce(period <= 4 & team_id == home_id & first_in_seg & seg == 2 & none_before & clock_sec > 360, FALSE))
cat("\n== likely mandatory timeouts dropped from the run response, by season ==\n")
pt(to_mand |> group_by(season) |> summarise(timeouts = n(), likely_mandatory = sum(likely_mandatory)))
run_trig <- scores |> left_join(home_map, by = c("season", "game_id")) |>
  mutate(victim = if_else(team_id == home_id, away_id, home_id)) |>
  group_by(season, game_id) |> arrange(elapsed, .by_group = TRUE) |>
  mutate(run = map_dbl(row_number(), \(i) { win <- elapsed > elapsed[i] - W & elapsed <= elapsed[i]
    sum(pts[win & team_id == team_id[i]]) - sum(pts[win & team_id != team_id[i]]) })) |> ungroup() |>
  filter(run >= 6) |> select(season, game_id, elapsed, victim) |>
  group_by(season, game_id, victim) |> arrange(elapsed, .by_group = TRUE) |>
  filter(elapsed - lag(elapsed, default = -Inf) > 60) |> ungroup()
run_resp <- run_trig |>
  left_join(to_mand |> filter(!likely_mandatory) |> select(season, game_id, victim = team_id, t_el = elapsed), by = c("season", "game_id", "victim"), relationship = "many-to-many") |>
  group_by(season, game_id, team_id = victim, elapsed) |> summarise(called = any(!is.na(t_el) & t_el >= elapsed & t_el <= elapsed + 30), .groups = "drop")
cat("\n== Timeouts per run faced (league), and Keefe ==\n")
pt(run_resp |> group_by(season, team_id) |> summarise(runs = n(), p_call = mean(called), .groups = "drop") |>
     group_by(season) |> mutate(league = mean(p_call), rank_most_eager = rank(-p_call)) |> ungroup() |> filter(team_id == WIZ_ID))

# --- team / coach level, 2025-26 and 2026-27 ---------------------------------------
coaches <- coach_tbl()
games_per_team <- pbp |> distinct(season, game_id, home_id, away_id) |>
  pivot_longer(c(home_id, away_id), values_to = "team_id") |> count(season, team_id, name = "games")

team_to <- to |> group_by(season, team_id, team_tricode) |>
  summarise(timeouts = n(), share_on_run = mean(stop_the_bleeding), mean_margin = mean(margin)
            , share_trailing = mean(margin < 0), net_after_run = mean(net_after[stop_the_bleeding]), swing_run = mean(swing[stop_the_bleeding])
            , share_q4_last3 = mean(period >= 4 & time_left <= 180), .groups = "drop") |>
  left_join(games_per_team, by = c("season", "team_id")) |>
  mutate(timeouts_pg = timeouts / games) |>
  left_join(coaches |> select(team_id, season, coach = coach_name), by = c("team_id", "season"))

# timeouts left entering the final 2 minutes of close games (|margin| <= 5)
# NBA: 7 per game, max 4 in Q4, max 2 in the last 3 minutes. Count used before the 2:00 mark of Q4.
close_end <- pbp |> filter(period == 4, clock_sec <= 120) |> group_by(season, game_id) |> slice(1) |>
  transmute(season, game_id, margin_home = score_home - score_away) |> filter(abs(margin_home) <= 5)
to_left <- to |> semi_join(close_end, by = c("season", "game_id")) |>
  filter(period < 4 | clock_sec > 120) |>
  count(season, game_id, team_id, name = "used_before_2min") |>
  right_join(close_end |> left_join(home_map, by = c("season", "game_id")) |> pivot_longer(c(home_id, away_id), values_to = "team_id") |> select(season, game_id, team_id), by = c("season", "game_id", "team_id")) |>
  mutate(used_before_2min = replace_na(used_before_2min, 0), left = pmax(7 - used_before_2min, 0))
to_left_game <- to_left    # one row per close game and team; section 8 re-cuts it by games played
to_left <- to_left |> group_by(season, team_id) |> summarise(close_games = n(), mean_to_left_2min = mean(left), .groups = "drop")
team_to <- team_to |> left_join(to_left, by = c("season", "team_id"))

cat("\n== 2026-27: Wizards vs league, timeout behaviour ==\n")
pt(team_to |> filter(season == CUR) |> arrange(desc(share_on_run)) |>
     select(team_tricode, coach, games, timeouts_pg, share_on_run, share_trailing, net_after_run, swing_run, mean_to_left_2min))

cat("\n== 2025-26 full season, by coach: timeout tendencies ==\n")
pt(team_to |> filter(season == PREV) |> arrange(desc(share_on_run)) |>
     select(team_tricode, coach, timeouts_pg, share_on_run, share_trailing, net_after_run, swing_run, close_games, mean_to_left_2min))

# --- Challenges ---------------------------------------------------------------
# per-game action lists, used by the challenge attribution and the after-timeout scoring below
ev <- pbp |>
  select(season, game_id, action_number, period, team_id, action_type, sub_type, description, score_home, score_away, home_id, away_id) |>
  mutate(team_id = replace_na(team_id, 0), action_type = replace_na(action_type, ""), sub_type = replace_na(sub_type, ""), description = replace_na(description, "")) |>
  arrange(season, game_id, action_number)
ev_split <- ev |> group_by(season, game_id) |> group_split()
names(ev_split) <- map_chr(ev_split, \(g) paste(g$season[1], g$game_id[1]))

# Every challenge produces an "Instant Replay" row whose subType starts "Coach Challenge" and names the ruling;
# the team comes from chal_link (the timeout spent on it), built with the timeouts above.
replay_rows <- pbp |> filter(action_type == "Instant Replay", str_starts(sub_type, "Coach Challenge")) |>
  select(season, game_id, action_number, period, clock_sec, elapsed, ruling = sub_type, score_home, score_away, home_id, away_id)
chal <- replay_rows |> left_join(chal_link |> select(season, game_id, action_number = r_action, team_id = chal_team), by = c("season", "game_id", "action_number")) |>
  filter(!is.na(team_id)) |>
  mutate(margin = if_else(team_id == home_id, score_home - score_away, score_away - score_home)
         , won = str_detect(ruling, regex("overturn", ignore_case = TRUE))
         , late_close = period >= 4 & clock_sec <= 300 & abs(margin) <= 8) |>
  left_join(to |> distinct(team_id, team_tricode), by = "team_id")
cat("\n== challenges found and attributed:", nrow(chal), "of", nrow(replay_rows), "replay rows ==\n")
# sanity: a coach who loses a challenge has no second one, so a team challenging again after a loss in the same game is an attribution error
chal_seq <- chal |> group_by(season, game_id, team_id) |> arrange(action_number, .by_group = TRUE) |> summarise(bad = any(lag(!won, default = FALSE)), .groups = "drop")
cat("team-games with a challenge after a lost one (should be ~0):", sum(chal_seq$bad), "of", nrow(chal_seq), "\n")
pt(chal |> count(season, ruling))

team_ch <- chal |> group_by(season, team_id, team_tricode) |>
  summarise(challenges = n(), win_rate = mean(won), wins = sum(won), share_late_close = mean(late_close), .groups = "drop") |>
  left_join(games_per_team, by = c("season", "team_id")) |> mutate(challenges_pg = challenges / games) |>
  left_join(coaches |> select(team_id, season, coach = coach_name), by = c("team_id", "season"))
cat("\n== Challenges 2025-26 by coach ==\n")
pt(team_ch |> filter(season == PREV) |> arrange(desc(win_rate)) |> select(team_tricode, coach, challenges, challenges_pg, win_rate, share_late_close))
cat("\n== Challenges 2026-27 ==\n")
pt(team_ch |> filter(season == CUR) |> arrange(desc(win_rate)) |> select(team_tricode, coach, challenges, challenges_pg, win_rate, share_late_close))
cat("\n== Wizards challenges, all three seasons ==\n")
pt(team_ch |> filter(team_id == WIZ_ID))

# ---------------------------------------------------------------------------
# After-timeout (ATO) plays and first-possession-of-the-quarter plays
# The two possessions a coach gets to draw up on a whiteboard. Score the calling
# team's first offensive possession after each regular timeout, and each team's
# first possession of each period, as points per possession.
# ---------------------------------------------------------------------------
# walk forward from an action index until `team`'s first offensive possession ends; return points scored on it
score_first_poss <- function(g, start_idx, team, max_steps = 60) {
  n <- nrow(g); i <- start_idx + 1
  is_home <- team == g$home_id[1]
  pts_at <- function(k) if (is_home) g$score_home[k] else g$score_away[k]
  base <- pts_at(start_idx); started <- FALSE; last_miss_team <- NA; steps <- 0
  while (i <= n && steps < max_steps) {
    at <- g$action_type[i]; tm <- g$team_id[i]
    if (at == "period" && g$sub_type[i] == "end") return(if (started) pts_at(i) - base else NA_real_)
    if (tm == team) {
      if (at %in% c("Made Shot", "Missed Shot", "Turnover", "Free Throw")) started <- TRUE
      if (at == "Missed Shot") last_miss_team <- team
      if (at == "Turnover") return(pts_at(i) - base)
      if (at == "Made Shot") {
        # and-one: swallow immediately following free throws by the same team
        j <- i + 1
        while (j <= n && g$team_id[j] == team && g$action_type[j] %in% c("Free Throw", "Foul")) j <- j + 1
        return(pts_at(j - 1) - base)
      }
      if (at == "Free Throw" && str_detect(g$description[i], "(\\d) of \\1")) {  # last FT of the trip
        if (!str_detect(g$description[i], "MISS")) return(pts_at(i) - base)
        last_miss_team <- team
      }
    } else if (tm > 0) {
      if (at == "Rebound" && identical(last_miss_team, team)) return(pts_at(i) - base)
      if (at == "Missed Shot") last_miss_team <- tm
      if (at %in% c("Made Shot", "Turnover")) { last_miss_team <- NA }
    }
    i <- i + 1; steps <- steps + 1
  }
  NA_real_
}

ato <- to |> mutate(key = paste(season, game_id)) |>
  mutate(pts = pmap_dbl(list(key, action_number, team_id), \(k, a, t) {
    g <- ev_split[[k]]; idx <- match(a, g$action_number); if (is.na(idx)) return(NA_real_)
    score_first_poss(g, idx, t) })) |>
  filter(!is.na(pts), pts >= 0, pts <= 5)   # a handful of feed glitches (score corrections) give impossible values
q1 <- ev |> filter(action_type == "period", sub_type == "start") |> mutate(key = paste(season, game_id)) |>
  select(season, game_id, key, action_number, period, home_id, away_id) |>
  pivot_longer(c(home_id, away_id), values_to = "team_id") |>
  mutate(pts = pmap_dbl(list(key, action_number, team_id), \(k, a, t) {
    g <- ev_split[[k]]; idx <- match(a, g$action_number); if (is.na(idx)) return(NA_real_)
    score_first_poss(g, idx, t) })) |>
  filter(!is.na(pts), pts >= 0, pts <= 5)   # a handful of feed glitches (score corrections) give impossible values

adv_all <- rd("panel_advanced") |>
  select(season, team_id, off_rating) |> mutate(team_ppp = off_rating / 100)
lg_ppp <- adv_all |> group_by(season) |> summarise(league_ppp = mean(team_ppp), .groups = "drop")

summarise_plays <- function(d, label, k = 60) {
  # k = strength of the pull toward the league ATO rate, in possessions (empirical-Bayes style shrinkage)
  lg <- d |> group_by(season) |> summarise(league_rate = mean(pts), league_sd = sd(pts), .groups = "drop")
  d |> group_by(season, team_id) |>
    summarise(n = n(), raw = mean(pts), sd = sd(pts), .groups = "drop") |>
    left_join(lg, by = "season") |>
    mutate(shrunk = (n * raw + k * league_rate) / (n + k), lo = raw - 1.64 * league_sd / sqrt(n), hi = raw + 1.64 * league_sd / sqrt(n), play = label) |>
    left_join(adv_all, by = c("season", "team_id")) |>
    mutate(vs_own = shrunk - team_ppp, vs_league = shrunk - league_rate)
}
plays <- bind_rows(summarise_plays(ato, "After timeout"), summarise_plays(q1, "First possession of a quarter")) |>
  left_join(coaches |> select(team_id, season, coach = coach_name), by = c("team_id", "season")) |>
  left_join(to |> distinct(team_id, team_tricode), by = "team_id")

cat("\n== League: points per possession on drawn-up plays vs all possessions ==\n")
pt(plays |> group_by(season, play) |> summarise(possessions = sum(n), league_rate = first(league_rate), league_all_poss = first(team_ppp) * 0 + mean(team_ppp), .groups = "drop") |>
     left_join(lg_ppp, by = "season") |> select(-league_all_poss))
cat("\n== 2026-27 by team: ATO and first-of-quarter PPP (shrunk toward league; 90% interval on the raw rate) ==\n")
pt(plays |> filter(season == CUR) |> arrange(play, desc(shrunk)) |> select(play, team_tricode, coach, n, raw, shrunk, lo, hi, team_ppp, vs_own))
cat("\n== 2025-26 by team ==\n")
pt(plays |> filter(season == PREV) |> arrange(play, desc(shrunk)) |> select(play, team_tricode, coach, n, raw, shrunk, lo, hi, team_ppp, vs_own))
cat("\n== Wizards, three seasons ==\n")
pt(plays |> filter(team_id == WIZ_ID) |> arrange(play, season) |> select(play, season, coach, n, raw, shrunk, lo, hi, team_ppp, vs_own, vs_league))

# Viz: ATO and first-of-quarter, 2025-26 full season, with the Wizards' current season as the overlay
pl_plot <- plays |> filter(season %in% c(PREV, CUR)) |>
  mutate(lab = if_else(season == CUR, paste0(team_tricode, " '27"), team_tricode), is_wiz = team_id == WIZ_ID)
p_ato <- ggplot(pl_plot |> filter(season == PREV), aes(shrunk, fct_reorder(team_tricode, shrunk))) +
  facet_wrap(~ play, scales = "free_y") +
  geom_vline(data = pl_plot |> filter(season == PREV) |> distinct(play, league_rate), aes(xintercept = league_rate), colour = "grey55", linetype = "dashed") +
  geom_segment(aes(x = lo, xend = hi, yend = fct_reorder(team_tricode, shrunk)), colour = "grey80", linewidth = 1) +
  geom_point(aes(x = team_ppp), colour = "grey45", shape = 124, size = 3.5) +
  geom_point(aes(colour = is_wiz), size = 3) +
  geom_point(data = pl_plot |> filter(season == CUR, is_wiz), aes(x = shrunk, y = "WAS"), colour = wiz_red, size = 4, shape = 18) +
  scale_colour_manual(values = c(`TRUE` = wiz_blue, `FALSE` = "grey30"), guide = "none") +
  labs(title = "The plays a coach draws up: after a timeout, and out of a quarter break"
       , subtitle = "Points per possession on each team's first possession after its own timeout (left) and first possession of each quarter (right), 2025-26.\nDot = rate after pulling small samples toward the league average; grey bar = 90% interval on the raw rate; tick = that team's rate on all its possessions.\nWizards in blue; the red diamond is Keefe's 2026-27 so far. Dashed line = league average on these plays."
       , x = "Points per possession", y = NULL, caption = caption_text) +
  theme_wp(12) + theme(strip.text = element_text(face = "bold"))
save_plot("ato_and_quarter_start_plays.png", p_ato, width = 13, height = 10)

# ---------------------------------------------------------------------------
# Viz 1: the timeout map — when in the game and at what margin coaches call them
# ---------------------------------------------------------------------------
to_plot <- to |> filter(season %in% c(PREV, CUR), period <= 4) |>
  mutate(who = if_else(team_id == WIZ_ID, "Wizards", "League"), game_min = elapsed / 60)
p_map <- ggplot() +
  geom_bin2d(data = to_plot |> filter(who == "League"), aes(game_min, margin), binwidth = c(1, 2)) +
  scale_fill_gradient(low = "grey95", high = "grey35", guide = "none") +
  geom_vline(xintercept = c(12, 24, 36), colour = "white", linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = wiz_red, linewidth = 0.4, linetype = "dashed") +
  geom_point(data = to_plot |> filter(who == "Wizards", season == CUR), aes(game_min, margin, colour = stop_the_bleeding), size = 3.4, alpha = 0.9) +
  scale_colour_manual(values = c(`TRUE` = wiz_red, `FALSE` = wiz_blue), guide = "none") +
  scale_x_continuous(breaks = seq(0, 48, 12), limits = c(0, 48)) +
  coord_cartesian(ylim = c(-30, 30)) +
  labs(title = "When coaches call timeout, and when Keefe does"
       , subtitle = paste0("Grey: every league timeout, 2025-26 and 2026-27 (darker = more).<br>"
                           , "<span style='color:", wiz_red, "'>**Red dots**</span>: Keefe's timeouts this season during an opponent run.<br>"
                           , "<span style='color:", wiz_blue, "'>**Blue dots**</span>: his other timeouts.")
       , x = "Minute of game", y = "Margin for the team calling timeout", caption = caption_text) +
  theme_wp(17) + theme(plot.subtitle = ggtext::element_markdown(lineheight = 1.3), plot.title = element_text(size = rel(1.15)))
save_plot("timeout_map.png", p_map, width = 7.5, height = 7.5)

# ---------------------------------------------------------------------------
# Viz 2: does the timeout stop the run?  before/after net points, timeout vs no timeout
# ---------------------------------------------------------------------------
swing_df <- bind_rows(
  to |> filter(season == PREV, stop_the_bleeding) |> transmute(what = "Timeout called", net_before, net_after)
  , ctl |> transmute(what = "No timeout (same run, play on)", net_before, net_after)
  , to |> filter(season == CUR, stop_the_bleeding, team_id == WIZ_ID) |> transmute(what = "Keefe, 2026-27", net_before, net_after)
) |> group_by(what) |> summarise(n = n(), before = mean(net_before), after = mean(net_after), se = sd(net_after) / sqrt(n), .groups = "drop") |>
  mutate(what = fct_relevel(what, "Timeout called", "No timeout (same run, play on)", "Keefe, 2026-27"))
p_swing <- swing_df |> pivot_longer(c(before, after), names_to = "when", values_to = "net") |>
  mutate(when = factor(when, c("before", "after"), c("3 min before", "3 min after"))) |>
  ggplot(aes(when, net, group = what, colour = what)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(linewidth = 1.4) + geom_point(size = 4) +
  geom_text(data = swing_df |> arrange(after) |> mutate(y_lab = after + (row_number() - 2) * 0.55), aes(x = 2.08, y = y_lab, label = sprintf("%+.1f  (n=%s)", after, comma(n))), hjust = 0, size = 3.6, show.legend = FALSE, family = font) +
  scale_colour_manual(values = c(`Timeout called` = teal, `No timeout (same run, play on)` = "grey50", `Keefe, 2026-27` = wiz_blue), name = NULL) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.6))) +
  labs(title = "Does a timeout stop the run? Mostly the run stops itself"
       , subtitle = "Net points for the team being run on, in the 3 minutes before and after an opponent 6-0+ run, 2025-26.\nRuns end whether or not the coach calls time; the timeout's edge is whatever gap remains between the lines on the right."
       , x = NULL, y = "Net points (team minus opponent)", caption = caption_text) +
  theme_wp() + theme(legend.position = "top")
save_plot("timeout_stops_the_run.png", p_swing, width = 11, height = 7)

# ---------------------------------------------------------------------------
# Viz 3: challenges — rate vs success, 2025-26, coach labels, Wizards both seasons
# ---------------------------------------------------------------------------
ch_plot <- team_ch |> filter(season %in% c(PREV, CUR)) |>
  mutate(lab = if_else(season == CUR, paste0(team_tricode, " '27"), team_tricode), is_wiz = team_id == WIZ_ID)
p_ch <- ggplot(ch_plot |> filter(season == PREV), aes(challenges_pg, win_rate)) +
  geom_hline(yintercept = mean(chal$won[chal$season == PREV]), colour = "grey60", linetype = "dashed") +
  geom_vline(xintercept = mean(ch_plot$challenges_pg[ch_plot$season == PREV]), colour = "grey60", linetype = "dashed") +
  geom_point(aes(size = challenges), colour = "grey55", alpha = 0.8) +
  geom_text(aes(label = coach), size = 2.9, vjust = -1.1, colour = "grey35") +
  geom_point(data = ch_plot |> filter(is_wiz), aes(size = challenges, colour = season)) +
  geom_text(data = ch_plot |> filter(is_wiz), aes(label = paste("Keefe", season)), size = 3.4, fontface = "bold", vjust = 2, colour = wiz_blue) +
  scale_colour_manual(values = c(`2025-26` = wiz_blue, `2026-27` = wiz_red), guide = "none") +
  scale_size_continuous(range = c(2, 8), name = "Challenges") +
  scale_y_continuous(labels = percent) +
  labs(title = "Coach's challenges: how often, and how often they win"
       , subtitle = "2025-26 full season by head coach; dashed lines are league averages. Keefe's 2026-27 (8 games) added in red. 'Win' = ruling overturned."
       , x = "Challenges per game", y = "Share overturned", caption = caption_text) +
  theme_wp() + theme(legend.position = "bottom")
save_plot("challenges_by_coach.png", p_ch, width = 12, height = 8)

cat("\nTimeout/challenge figures written.\n")


# ===========================================================================
# 7. Data art: the rotation tapestry, the timeout clock, the lineup constellation
# ===========================================================================
box <- rd_games("boxscores", CUR) |>
  mutate(mins = as.numeric(str_match(minutes, "^(\\d+)")[, 2]) + as.numeric(str_match(minutes, ":(\\d+)")[, 2]) / 60
         , mins = replace_na(mins, 0), surname = str_to_lower(word(name, -1)))
wiz_box <- box |> filter(team_id == WIZ_ID)
gl <- read_csv(file.path(DATA_DIR, sprintf("wizards_gamelog_%s.csv", CUR)), show_col_types = FALSE, col_types = cols(Game_ID = col_character())) |> clean_names() |>
  mutate(game_id = str_pad(game_id, 10, pad = "0"), opp = str_sub(matchup, -3), home = str_detect(matchup, "vs"), date = mdy(game_date)) |>
  arrange(date) |> mutate(game_no = row_number(), label = paste0("G", game_no, "  ", if_else(home, "vs ", "@ "), opp, "  ", wl, " ", pts))

players <- wiz_box |> group_by(person_id, name, surname) |> summarise(mins = sum(mins), .groups = "drop") |> arrange(desc(mins)) |>
  mutate(short = paste0(str_sub(word(name, 1), 1, 1), ". ", word(name, -1)))
player_pal <- c("#2B2A4C", "#B31312", "#EA906C", "#7A6F8C", "#C9A79A", "#F21A00", "#8C6D31", "#7570B3", "#66A61E", "#E7298A"
         , "#A6761D", "#1B9E77", "#D95F02", "#999999", "#F781BF", "#4DAF4A", "#984EA3", "#FF7F00")
player_cols <- setNames(player_pal[seq_len(nrow(players))], players$short)

# ===========================================================================
# 1. Rotation tapestry
# ===========================================================================
if (file.exists(file.path(DATA_DIR, sprintf("pbp_%s.csv", CUR)))) {
pbp <- rd_games("pbp", CUR) |>
  filter(game_id %in% gl$game_id) |>
  mutate(clock_sec = as.numeric(str_match(clock, "PT(\\d+)M")[, 2]) * 60 + as.numeric(str_match(clock, "M([\\d.]+)S")[, 2])
         , period_len = if_else(period <= 4, 720, 300)
         , period_start = if_else(period <= 4, (period - 1) * 720, 2880 + (period - 5) * 300)
         , elapsed = period_start + (period_len - clock_sec)) |>
  arrange(game_id, action_number)

# substitutions: "SUB: In FOR Out"; match surnames to the box score of that game
subs <- pbp |> filter(action_type == "Substitution", team_id == WIZ_ID) |>
  mutate(in_name = str_to_lower(str_match(description, "SUB:\\s*(.+?)\\s+FOR\\s+")[, 2]), out_name = str_to_lower(str_match(description, "FOR\\s+(.+)$")[, 2])) |>
  select(game_id, period, elapsed, action_number, in_name, out_name)
roster <- wiz_box |> select(game_id, person_id, surname)
match_name <- function(nm, gid) {
  r <- roster |> filter(game_id == gid)
  hit <- r$person_id[r$surname == nm | str_detect(nm, fixed(r$surname))]
  if (length(hit) == 1) hit else NA_real_
}
subs <- subs |> mutate(in_id = map2_dbl(in_name, game_id, match_name), out_id = map2_dbl(out_name, game_id, match_name))
cat("subs matched:", mean(!is.na(subs$in_id) & !is.na(subs$out_id)), "\n")

# who starts each period: Q1 from the box score; later periods inferred from who acts before being subbed in
acts <- pbp |> filter(team_id == WIZ_ID, !is.na(person_id), person_id > 0, action_type %in% c("Made Shot", "Missed Shot", "Rebound", "Foul", "Turnover", "Free Throw", "Violation", "Jump Ball")) |>
  select(game_id, period, elapsed, person_id)
stints <- list()
for (gid in gl$game_id) {
  periods <- sort(unique(pbp$period[pbp$game_id == gid]))
  on <- wiz_box |> filter(game_id == gid, starter) |> pull(person_id)
  for (p in periods) {
    p_start <- if (p <= 4) (p - 1) * 720 else 2880 + (p - 5) * 300
    p_end <- p_start + if (p <= 4) 720 else 300
    s_p <- subs |> filter(game_id == gid, period == p) |> arrange(action_number)
    if (p > 1) {
      first_in <- s_p |> group_by(in_id) |> summarise(t = min(elapsed), .groups = "drop")
      a_p <- acts |> filter(game_id == gid, period == p) |> left_join(first_in, by = c("person_id" = "in_id")) |>
        filter(is.na(t) | elapsed < t) |> distinct(person_id)
      out_first <- s_p |> group_by(out_id) |> summarise(t = min(elapsed), .groups = "drop") |> left_join(first_in, by = c("out_id" = "in_id"), suffix = c("_out", "_in")) |>
        filter(is.na(t_in) | t_out < t_in) |> pull(out_id)
      cand <- unique(c(a_p$person_id, out_first))
      on_new <- cand[seq_len(min(5, length(cand)))]
      if (length(on_new) < 5) on_new <- unique(c(on_new, setdiff(on, on_new)))[1:5]
      on <- on_new
    }
    t <- p_start
    for (i in seq_len(nrow(s_p))) {
      stints[[length(stints) + 1]] <- tibble(game_id = gid, person_id = on, start = t, end = s_p$elapsed[i])
      t <- s_p$elapsed[i]
      if (!is.na(s_p$out_id[i]) && !is.na(s_p$in_id[i])) on <- c(setdiff(on, s_p$out_id[i]), s_p$in_id[i])
    }
    stints[[length(stints) + 1]] <- tibble(game_id = gid, person_id = on, start = t, end = p_end)
  }
}
stints <- bind_rows(stints) |> filter(end > start, !is.na(person_id)) |> left_join(players |> select(person_id, short), by = "person_id")
chk <- stints |> group_by(game_id, person_id) |> summarise(mins = sum(end - start) / 60, .groups = "drop") |>
  left_join(wiz_box |> select(game_id, person_id, box_mins = mins), by = c("game_id", "person_id"))
cat("reconstructed vs box-score minutes, correlation:", cor(chk$mins, chk$box_mins), " mean abs diff:", mean(abs(chk$mins - chk$box_mins)), "\n")

# slice to 15-second cells; 5 lanes ordered by the player's season minutes so the weave is stable
grid <- stints |> mutate(cell_s = map2(floor(start / 15), ceiling(end / 15) - 1, ~ seq(.x, .y))) |>
  select(game_id, short, cell_s) |> unnest(cell_s) |> distinct() |>
  left_join(players |> select(short, mins), by = "short") |>
  group_by(game_id, cell_s) |> arrange(desc(mins), .by_group = TRUE) |> mutate(lane = row_number()) |> ungroup() |> filter(lane <= 5) |>
  left_join(gl |> select(game_id, game_no, label), by = "game_id") |>
  mutate(minute = cell_s / 4 + 0.125)
tap_lab <- gl |> mutate(y = 0) |> select(label, game_no)
p_tap <- ggplot(grid, aes(minute, lane, fill = short)) +
  geom_tile(width = 0.25, height = 0.96) +
  geom_vline(xintercept = c(12, 24, 36, 48), colour = ink, linewidth = 0.5) +
  facet_wrap(~ fct_reorder(label, game_no), ncol = 1, strip.position = "left") +
  scale_fill_manual(values = player_cols, name = NULL) +
  scale_x_continuous(breaks = seq(0, 48, 12), expand = c(0, 0), limits = c(0, max(grid$minute) + 0.125)) +
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  labs(title = "The rotation tapestry", subtitle = "Every Wizards game this season as a strip of cloth: five threads for the five men on the floor, woven minute by minute. Threads are stacked by season minutes,\nso the starters sit at the top of each strip and the bench rises into them. Vertical lines are quarter breaks. Through Nov 5, 2026."
       , x = "Minute of game", y = NULL, caption = caption_text) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(colour = NA))) +
  theme_wiz(15) +
  theme(legend.position = "bottom", panel.grid = element_blank(), panel.spacing.y = unit(0.35, "lines")
        , strip.text.y.left = element_text(angle = 0, hjust = 1, size = 9, colour = "grey20"), strip.placement = "outside"
        , plot.title = element_text(face = "bold", size = 20), plot.subtitle = element_text(colour = "grey30", lineheight = 1.1)
        , plot.title.position = "plot", plot.caption.position = "plot", plot.caption = element_text(colour = "grey45", size = 8)
        , plot.background = element_rect(fill = "white", colour = NA))
save_plot("rotation_tapestry_2026-27.png", p_tap, width = 14, height = 11)

# ===========================================================================
# 2. The timeout clock
# ===========================================================================
pbp_all <- bind_rows(rd_games("pbp", PREV) |> mutate(season = PREV), rd_games("pbp", CUR) |> mutate(season = CUR)) |>
  mutate(clock_sec = as.numeric(str_match(clock, "PT(\\d+)M")[, 2]) * 60 + as.numeric(str_match(clock, "M([\\d.]+)S")[, 2])
         , elapsed = if_else(period <= 4, (period - 1) * 720 + (720 - clock_sec), 2880 + (period - 5) * 300 + (300 - clock_sec))
         , score_home = as.numeric(score_home), score_away = as.numeric(score_away)) |>
  arrange(season, game_id, action_number) |> group_by(season, game_id) |> fill(score_home, score_away, .direction = "down") |> ungroup()
# timeout rows carry the team id in personId (team ids ~1.61e9), with teamId = 0
pbp_all <- pbp_all |> mutate(team_id = if_else((is.na(team_id) | team_id == 0) & !is.na(person_id) & person_id > 1e9, person_id, team_id))
home_map <- pbp_all |> filter(action_type %in% c("Made Shot", "Free Throw"), team_id > 0) |> group_by(season, game_id) |>
  mutate(dh = score_home - lag(score_home)) |> filter(!is.na(dh), dh > 0) |> summarise(home_id = as.numeric(names(which.max(table(team_id)))), .groups = "drop")
tos <- pbp_all |> filter(action_type == "Timeout", sub_type == "Regular", team_id > 0, period <= 4) |> left_join(home_map, by = c("season", "game_id")) |>
  mutate(margin = if_else(team_id == home_id, score_home - score_away, score_away - score_home), minute = elapsed / 60)
league_halo <- tos |> filter(season == PREV) |> mutate(bin = floor(minute)) |> count(bin) |> mutate(dens = n / max(n))
wiz_to <- tos |> filter(season == CUR, team_id == WIZ_ID) |> left_join(gl |> select(game_id, game_no, opp, wl), by = "game_id") |>
  mutate(ring = 1.6 + game_no * 0.55, state = case_when(margin <= -6 ~ "trailing by 6+", margin >= 6 ~ "leading by 6+", TRUE ~ "within 5"))
p_clock <- ggplot() +
  geom_rect(data = league_halo, aes(xmin = bin, xmax = bin + 1, ymin = 0.75, ymax = 1.35, alpha = dens), fill = "white") +
  scale_alpha_continuous(range = c(0.04, 0.85), guide = "none") +
  geom_hline(yintercept = 1.6 + (1:8) * 0.55, colour = "#2A3566", linewidth = 0.25) +
  geom_vline(xintercept = c(0, 12, 24, 36), colour = "#3C4A80", linewidth = 0.4) +
  geom_segment(data = wiz_to, aes(x = minute, xend = minute, y = ring - 0.22, yend = ring + 0.22, colour = state), linewidth = 1.6, lineend = "round") +
  geom_text(data = tibble(x = c(0, 12, 24, 36), lab = c("tip", "Q2", "Q3", "Q4")), aes(x, 0.45, label = lab), colour = "grey70", size = 3.4) +
  geom_text(data = gl, aes(x = 47.4, y = 1.6 + game_no * 0.55, label = paste0(if_else(home, "vs ", "@ "), opp)), colour = "grey75", size = 2.6, hjust = 1) +
  scale_colour_manual(values = c(`trailing by 6+` = wiz_red, `leading by 6+` = teal, `within 5` = gold), name = "Score when Keefe called it") +
  coord_radial(start = 0, expand = FALSE, inner.radius = 0.08) +
  scale_x_continuous(limits = c(0, 48), breaks = NULL) + scale_y_continuous(limits = c(0, 6.6), breaks = NULL) +
  labs(title = "The timeout clock", subtitle = "The 48 minutes of a game as a dial. The inner halo is when NBA coaches called timeout last season (brighter = more often).\nEach outer ring is one Wizards game this season, with a tick for each of Keefe's timeouts, coloured by the score at that moment."
       , x = NULL, y = NULL, caption = caption_text) +
  theme_ink() + theme(legend.position = "bottom")
save_plot("timeout_clock_2026-27.png", p_clock, width = 11, height = 12, bg = ink)
}

# ===========================================================================
# 3. The lineup constellation
# ===========================================================================
pairs <- rd(paste0("wizards_pairs_", CUR)) |>
  mutate(ids = str_extract_all(group_id, "\\d+"), a = map_dbl(ids, ~ as.numeric(.x[1])), b = map_dbl(ids, ~ as.numeric(.x[2]))) |>
  select(a, b, min, net_rating, poss) |> filter(min >= 15)
nodes <- players |> mutate(k = row_number(), ang = 2 * pi * (k - 1) / n(), x = cos(ang), y = sin(ang)
                           , r_lab = 1.18, lx = r_lab * cos(ang), ly = r_lab * sin(ang), hj = if_else(cos(ang) < -0.1, 1, if_else(cos(ang) > 0.1, 0, 0.5)))
edges <- pairs |> left_join(nodes |> select(a = person_id, xa = x, ya = y), by = "a") |> left_join(nodes |> select(b = person_id, xb = x, yb = y), by = "b") |>
  filter(!is.na(xa), !is.na(xb)) |> mutate(net = pmin(pmax(net_rating, -25), 25))
p_con <- ggplot() +
  geom_curve(data = edges, aes(x = xa, y = ya, xend = xb, yend = yb, linewidth = min, colour = net), curvature = 0.18, alpha = 0.85, lineend = "round") +
  geom_point(data = nodes, aes(x, y, size = mins), colour = ink, fill = "white", shape = 21, stroke = 0.8) +
  geom_point(data = nodes, aes(x, y, size = mins * 0.25), colour = gold, alpha = 0.9) +
  geom_text(data = nodes, aes(lx, ly, label = paste0(short, "\n", round(mins), " min"), hjust = hj), colour = "grey85", size = 3.2, lineheight = 0.85) +
  scale_colour_gradient2(low = wiz_red, mid = "grey70", high = teal, midpoint = 0, name = "Net rating\ntogether", limits = c(-25, 25), labels = label_number(style_positive = "plus")) +
  scale_linewidth_continuous(range = c(0.3, 7), name = "Minutes\ntogether") +
  scale_size_continuous(range = c(2, 16), guide = "none") +
  coord_equal(xlim = c(-1.6, 1.6), ylim = c(-1.45, 1.45)) +
  labs(title = "The lineup constellation", subtitle = "Every Wizard is a star, placed clockwise from the top by minutes played. A line joins two players who have shared the floor for 15+ minutes;\nthicker = more time together, colour = how the pair has fared (points per 100 possessions, net). Through Nov 5, 2026."
       , caption = caption_text) +
  theme_ink() + theme(legend.position = "right")
save_plot("lineup_constellation_2026-27.png", p_con, width = 12, height = 11, bg = ink)

cat("\n== pairs with the most minutes ==\n")
pt(pairs |> arrange(desc(min)) |> left_join(nodes |> select(a = person_id, pa = short), by = "a") |> left_join(nodes |> select(b = person_id, pb = short), by = "b") |>
     select(pa, pb, min, net_rating) |> head(10))
cat("\nArt figures written.\n")


# ===========================================================================
# 8. Keefe vs Keefe: how the coach has changed since his earlier Wizards seasons
# ===========================================================================
# The fingerprints above compare Keefe with himself on things the coaching-change panel says belong to
# the coach, as league ranks, which survives the roster turning over every summer. This section puts
# all four areas he runs (offense, defense, the rotation, in-game decisions) on the same petals for each
# of his full seasons and this one, then asks two questions of every change since 2025-26:
#   1. Is the sample big enough? Recent seasons cut at the same number of games say how far a
#      November rank moves by April, which gives every 2026-27 petal a plausible final range.
#   2. Is the change bigger than an ordinary offseason? Coaches who stayed put supply the yardstick.
set.seed(202)
KEEFE <- SEASONS                                  # 2024-25, 2025-26 and the current season
N_TEAMS <- 30

# --- dial definitions ---------------------------------------------------------
# dir = "more": a petal reaches the edge when the team does the most of it in the league.
# dir = "less": flipped, so outward is the end the label names (e.g. fewer fouls).
# Offense and defense petals are the same six as the fingerprints in sections 3 and 4.
dials <- tribble(
  ~dial, ~area, ~petal, ~dir, ~long,
  "pace", "Offense", "Pace", "more", "Pace",
  "early_share", "Offense", "Early\nshots", "more", "Early-clock share (18+ s)",
  "late_share", "Offense", "Few late\nshots", "less", "Fewest late-clock shots (<7 s)",
  "passes_per_poss", "Offense", "Passes\nper poss.", "more", "Passes per possession",
  "rim_share", "Offense", "Rim\nshots", "more", "Rim share of shots",
  "three_share", "Offense", "Three-pt\nshots", "more", "3PA share of shots",
  "opp_three_share", "Defense", "Fewer 3s\nallowed", "less", "Fewest opponent threes",
  "opp_rim_share", "Defense", "Fewer rim\nshots", "less", "Fewest opponent rim shots",
  "opp_late_share", "Defense", "Opp. shoots\nlate", "more", "Opponents forced to late clock",
  "opp_fta_rate", "Defense", "Fewer\nfouls", "less", "Fewest opponent free throws",
  "opp_tov_pct", "Defense", "Force\nturnovers", "more", "Opponent turnover rate",
  "opp_oreb_pct", "Defense", "Few opp.\noff. rebs", "less", "Fewest opponent offensive rebounds",
  "top_lineup_share", "Rotation", "Top lineup\nminutes", "more", "Top lineup's share of minutes",
  "starter_share", "Rotation", "Starters'\nminutes", "more", "Starters' share of minutes",
  "top3_share", "Rotation", "Top-3\nminutes", "more", "Top three players' share of minutes",
  "rotation_size", "Rotation", "Few players\nused", "less", "Fewest players at 10+ min a game",
  "changes_per_10", "Rotation", "Same\nstarters", "less", "Fewest starting-lineup changes",
  "per10", "Rotation", "Minutes not\nfrom +/-", "less", "Least minutes response to last night's plus-minus",
  "to_per_run", "Decisions", "Few timeouts\non runs", "less", "Fewest timeouts per opponent run faced",
  "short_on_timeouts", "Decisions", "Timeouts\nleft late", "less", "Fewest close games entering the last 2 min with under 2 timeouts",
  "challenges_pg", "Decisions", "Challenges\nused", "more", "Challenges per game",
  "win_rate", "Decisions", "Challenge\nwins", "more", "Challenge success rate",
  "ato_ppp", "Decisions", "Scores after\ntimeouts", "more", "After-timeout points per possession",
  "q1_ppp", "Decisions", "Scores to\nopen qtrs", "more", "First-possession-of-quarter points per possession")
# counted = FALSE: dials too noisy to judge at this point of the season (after-timeout and quarter-start points per
# possession have split-half reliability around 0.1-0.3, and minutes response to plus-minus settles slowest of all).
# They stay on the fans for completeness but never count toward a verdict.
# cluster: dials that move together (pace and late-clock share; starters' and top three's minutes) count once.
dials <- dials |> mutate(counted = !dial %in% c("ato_ppp", "q1_ppp", "per10")
                         , cluster = case_when(dial %in% c("pace", "late_share") ~ "tempo", dial %in% c("starter_share", "top3_share") ~ "minutes load", TRUE ~ dial))
area_cols <- c(Offense = wiz_blue, Defense = "#7A6F8C", Rotation = gold, Decisions = wiz_red)

# --- team dials from the team tables (full seasons and the early-season cuts share one schema) ---------
rd_tbl <- function(prefix, t) { f <- file.path(DATA_DIR, paste0(prefix, "_", t, ".csv")); if (file.exists(f)) rd(paste0(prefix, "_", t)) else NULL }
clock_shares <- function(d, early_col, late_col) d |> group_by(team_id, season) |>
  summarise("{early_col}" := sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , "{late_col}" := sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]), .groups = "drop")
# player minutes from the five-man lineup table: each lineup's minutes credited to its five players,
# so a traded player's minutes stay with the team he earned them on
lineup_rotation <- function(lu) {
  units <- lu |> group_by(team_id, season) |> mutate(share = min / sum(min)) |>
    summarise(top_lineup_share = max(share), lineups = n(), .groups = "drop")
  players <- lu |> mutate(pid = str_extract_all(group_id, "\\d+")) |> select(team_id, season, pid, min) |> unnest(pid) |>
    group_by(team_id, season, pid) |> summarise(min = sum(min), .groups = "drop") |>
    group_by(team_id, season) |> arrange(desc(min), .by_group = TRUE) |>
    summarise(top3_share = sum(head(min, 3)) / sum(min), player_min = list(min), .groups = "drop")
  left_join(units, players, by = c("team_id", "season"))
}
team_dials <- function(prefix) {
  adv <- rd_tbl(prefix, "advanced"); pas <- rd_tbl(prefix, "passing")
  zon <- rd_tbl(prefix, "shotzones"); oloc <- rd_tbl(prefix, "opploc"); ff <- rd_tbl(prefix, "fourfactors")
  zone <- zon |> transmute(team_id, season, tot = rowSums(across(starts_with("fga_") & !matches("backcourt|^fga_corner_3$")))
                           , rim_share = fga_restricted_area / tot
                           , three_share = (fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3) / tot)
  ozone <- oloc |> transmute(team_id, season, tot = rowSums(across(starts_with("opp_fga_") & !matches("backcourt|^opp_fga_corner_3$")))
                             , opp_rim_share = opp_fga_restricted_area / tot
                             , opp_three_share = (opp_fga_left_corner_3 + opp_fga_right_corner_3 + opp_fga_above_the_break_3) / tot)
  adv |> select(team_id, season, gp, pace, poss, team_min = min, off_rating, def_rating, net_rating) |>
    left_join(pas |> select(team_id, season, team = team_abbreviation, passes_made), by = c("team_id", "season")) |>
    left_join(clock_shares(rd_tbl(prefix, "shotclock"), "early_share", "late_share"), by = c("team_id", "season")) |>
    left_join(clock_shares(rd_tbl(prefix, "oppclock"), "opp_early_share", "opp_late_share"), by = c("team_id", "season")) |>
    left_join(zone |> select(-tot), by = c("team_id", "season")) |> left_join(ozone |> select(-tot), by = c("team_id", "season")) |>
    left_join(ff |> select(team_id, season, opp_fta_rate, opp_tov_pct, opp_oreb_pct), by = c("team_id", "season")) |>
    left_join(lineup_rotation(rd_tbl(prefix, "lineups")), by = c("team_id", "season")) |>
    mutate(passes_per_poss = passes_made / (poss / gp)               # POSS is a season total even with PerMode=PerGame
           , rotation_size = map2_dbl(player_min, gp, \(m, g) sum(m / g >= 10))
           , lineups_pg = lineups / gp) |>
    select(-player_min)
}
dial_full  <- team_dials("panel")
dial_early <- team_dials("early")
N_EARLY <- rd("early_advanced")$n_games[1]
cat("\n== Early-season cut: first", N_EARLY, "games; cut dates ==\n")
pt(rd("early_advanced") |> distinct(season, cut_date))

# lineup table completeness: lineup minutes should add up to five times the team's minutes
lu_check <- rd("panel_lineups") |> group_by(team_id, season) |> summarise(lineup_min = sum(min), .groups = "drop") |>
  left_join(rd("panel_advanced") |> transmute(team_id, season, team_min = min), by = c("team_id", "season")) |>
  mutate(coverage = lineup_min / team_min)
cat("\n== five-man lineup minutes / team minutes (1 = every minute is in the lineup table) ==\n")
pt(lu_check |> group_by(season) |> summarise(min_cov = min(coverage), median_cov = median(coverage), max_cov = max(coverage)))

# --- decision dials from play-by-play, by games played -------------------------
# game order per team, by date (game ids are not in date order)
game_idx <- ev |> distinct(season, game_id, home_id, away_id) |>
  pivot_longer(c(home_id, away_id), values_to = "team_id") |> filter(!is.na(team_id)) |> select(season, game_id, team_id) |>
  left_join(game_dates, by = c("season", "game_id", "team_id")) |>
  group_by(season, team_id) |> arrange(game_date, game_id, .by_group = TRUE) |> mutate(g = row_number()) |> ungroup()
decision_dials <- function(max_g = Inf, k = 60) {
  keep <- \(d) d |> inner_join(game_idx |> filter(g <= max_g) |> select(season, game_id, team_id), by = c("season", "game_id", "team_id"))
  gp <- game_idx |> filter(g <= max_g) |> count(season, team_id, name = "games")
  shrink <- \(d, nm) keep(d) |> group_by(season) |> mutate(lg = mean(pts)) |> group_by(season, team_id) |>
    summarise("{nm}" := (sum(pts) + k * first(lg)) / (n() + k), "{nm}_n" := n(), .groups = "drop")
  gp |>
    left_join(keep(run_resp) |> group_by(season, team_id) |> summarise(runs_faced = n(), to_per_run = mean(called), .groups = "drop"), by = c("season", "team_id")) |>
    # timeouts in hand late: share of close games entering the final 2:00 with fewer than 2 (more than 2 buys nothing:
    # only 2 may be used after the 3:00 mark of the fourth). Needs 8+ close games before it means anything.
    left_join(keep(to_left_game) |> group_by(season, team_id) |> summarise(close_games = n(), short_on_timeouts = mean(left < 2), .groups = "drop") |>
                mutate(short_on_timeouts = if_else(close_games >= 8, short_on_timeouts, NA_real_)), by = c("season", "team_id")) |>
    left_join(keep(chal) |> group_by(season, team_id) |> summarise(challenges = n(), wins = sum(won), .groups = "drop"), by = c("season", "team_id")) |>
    left_join(shrink(ato, "ato_ppp"), by = c("season", "team_id")) |>
    left_join(shrink(q1, "q1_ppp"), by = c("season", "team_id")) |>
    mutate(challenges = replace_na(challenges, 0), wins = replace_na(wins, 0), challenges_pg = challenges / games
           , win_rate = if_else(challenges > 0, wins / challenges, NA_real_))
}
# rotation dials that need box scores (section 5's starting fives and minutes-response fit), by games played
box_dials <- function(max_g = Inf) {
  st <- starts |> filter(g <= max_g) |> group_by(season, team_id) |>
    summarise(changes_per_10 = 10 * sum(changed, na.rm = TRUE) / pmax(n() - 1, 1), .groups = "drop")
  rp <- pm |> filter(next_g <= max_g) |> group_by(season, team_id) |> group_modify(~ fit_team(.x)) |> ungroup() |>
    transmute(season, team_id, per10 = slope * 10)
  # starters' share of player-minutes (the team-table bench split's MIN is not player-minutes, so use the box scores)
  ss <- box_league |> filter(g <= max_g) |> group_by(season, team_id) |> summarise(starter_share = sum(mins[starter]) / sum(mins), .groups = "drop")
  st |> full_join(rp, by = c("season", "team_id")) |> full_join(ss, by = c("season", "team_id"))
}
dial_full  <- dial_full |> left_join(box_dials(), by = c("season", "team_id"))
box_early  <- box_dials(N_EARLY)

dec_full  <- decision_dials()
dec_early <- decision_dials(N_EARLY)
cat("\n== decision dials: team-seasons by season ==\n")
pt(dec_full |> count(season))

# --- league ranks (1 = most of the thing) ---------------------------------------
to_ranks <- function(d) d |> select(season, team_id, any_of(dials$dial)) |>
  pivot_longer(-c(season, team_id), names_to = "dial", values_to = "value") |>
  group_by(season, dial) |> mutate(rank = rank(-value, na.last = "keep", ties.method = "average")) |> ungroup()
rk_full  <- bind_rows(to_ranks(dial_full), to_ranks(dec_full))
rk_early <- bind_rows(to_ranks(dial_early), to_ranks(box_early |> filter(season != CUR)), to_ranks(dec_early |> filter(season != CUR)))

# --- 1. How much does an early rank say about the final one? ---------------------
calib <- rk_early |> filter(season != CUR) |> rename(early = rank, early_value = value) |>
  inner_join(rk_full |> select(season, team_id, dial, final = rank), by = c("season", "team_id", "dial")) |>
  filter(!is.na(early), !is.na(final)) |> left_join(dials, by = "dial")
trust <- calib |> group_by(dial, area, long) |>
  summarise(r = cor(early, final, method = "spearman"), mad_rank = median(abs(final - early)), seasons = n_distinct(season), pairs = n(), .groups = "drop")
cat("\n== Spearman r between a team's rank after", N_EARLY, "games and its final rank ==\n")
pt(trust |> arrange(area, desc(r)), digits = 2)
common_dials <- trust$dial[trust$seasons == max(trust$seasons)] |> intersect(dials$dial[dials$counted])   # judged in every baseline season

# likely final rank for an early rank e0: the q[1]-q[2] percentiles of where teams that sat within w places of e0
# at the same point ended up (window 3, or 5 when only two seasons of play-by-play / box scores exist; w_add widens it)
predict_final <- function(d, e0, w_add = 0, q = c(0.1, 0.9)) {
  w <- (if (n_distinct(d$season) <= 2) 5 else 3) + w_add
  near <- d |> filter(abs(early - e0) <= w)
  if (nrow(near) < 5) return(tibble(final_lo = NA_real_, final_mid = NA_real_, final_hi = NA_real_, n_near = nrow(near)))
  tibble(final_lo = unname(quantile(near$final, q[1])), final_mid = median(near$final), final_hi = unname(quantile(near$final, q[2])), n_near = nrow(near))
}
# how often does that range actually hold? judge each season's teams against the other seasons only
coverage <- calib |> filter(dial %in% dials$dial[dials$counted]) |> group_by(season, dial) |>
  group_modify(\(d, k) bind_cols(d |> select(final), map_dfr(d$early, \(e0) predict_final(calib |> filter(dial == k$dial, season != k$season), e0)))) |>
  ungroup() |> filter(!is.na(final_lo)) |> summarise(coverage = mean(final >= final_lo & final <= final_hi)) |> pull(coverage)
cat(sprintf("\n== the nominal 10th-90th percentile range holds for %.0f%% of teams, judged out of season ==\n", 100 * coverage))

# --- 2. How much does a rank move in an ordinary offseason? ------------------------
coach_of <- coach_tbl() |> select(team_id, season, coach = coach_name)
churn_moves <- rk_full |> filter(season != CUR, team_id != WIZ_ID) |>
  left_join(coach_of, by = c("team_id", "season")) |>
  left_join(dial_full |> select(team_id, season, gp), by = c("team_id", "season")) |>
  filter(is.na(gp) | gp >= 60) |>
  mutate(season_start = as.integer(substr(season, 1, 4))) |>
  group_by(team_id, dial) |> arrange(season_start, .by_group = TRUE) |>
  mutate(d_rank = rank - lag(rank), same = coach == lag(coach) & season_start - lag(season_start) == 1) |> ungroup() |>
  filter(same, !is.na(d_rank))
churn_at <- \(p) churn_moves |> group_by(dial) |> summarise(churn = unname(quantile(abs(d_rank), p)), churn_pairs = n(), .groups = "drop")
churn <- churn_at(0.8) |> rename(churn80 = churn)
cat("\n== 80th percentile of |rank change| between consecutive seasons under the same coach ==\n")
pt(churn |> left_join(dials |> select(dial, area), by = "dial") |> arrange(area, churn80))

# --- 3. The verdicts: the Wizards, and every other team, under one procedure ---------------
# flag = last season's rank is outside this season's likely final range AND the likely move is bigger than
# what churn_p of same-coach teams move in an offseason. Computed for the Wizards now, and replayed on every team
# from 2021-22 on (previous full season vs first N_EARLY games), each judged with calibration from other seasons.
prev_season <- \(s) sprintf("%d-%02d", as.integer(substr(s, 1, 4)) - 1, as.integer(substr(s, 1, 4)) %% 100)
cur_k <- str_replace(CUR, "-", "_"); prev_k <- str_replace(PREV, "-", "_"); first_k <- str_replace(KEEFE[1], "-", "_")
hist_base <- rk_early |> filter(season != CUR, !is.na(rank), dial %in% dials$dial[dials$counted]) |> select(season, team_id, dial, early = rank) |>
  mutate(prev_s = prev_season(season)) |>
  inner_join(rk_full |> select(prev_s = season, team_id, dial, prev = rank), by = c("prev_s", "team_id", "dial")) |>
  filter(!is.na(prev)) |>
  left_join(coach_of |> rename(coach_now = coach), by = c("team_id", "season")) |>
  left_join(coach_of |> rename(prev_s = season, coach_prev = coach), by = c("team_id", "prev_s")) |>
  filter(!is.na(coach_now), !is.na(coach_prev)) |>
  mutate(kind = if_else(coach_now == coach_prev, "Kept coach", "New coach"))
run_spec <- function(w_add = 0, q = c(0.1, 0.9), churn_p = 0.8) {
  ch <- churn_at(churn_p)
  w <- rk_full |> filter(team_id == WIZ_ID, season %in% KEEFE) |> select(season, dial, value, rank) |>
    pivot_wider(names_from = season, values_from = c(value, rank)) |> rename_with(~ str_replace(.x, "-", "_")) |>
    mutate(pred = map2(dial, .data[[paste0("rank_", cur_k)]], \(dl, e0) if (is.na(e0)) predict_final(calib[0, ], 0) else predict_final(calib |> filter(dial == dl), e0, w_add, q))) |>
    unnest(pred) |> left_join(ch, by = "dial") |> left_join(dials, by = "dial") |>
    mutate(prev = .data[[paste0("rank_", prev_k)]], first = .data[[paste0("rank_", first_k)]], now = .data[[paste0("rank_", cur_k)]]
           , flag = (prev < final_lo | prev > final_hi) & abs(final_mid - prev) > churn)
  h <- hist_base |> group_by(season, dial) |>
    group_modify(\(d, k) bind_cols(d, map_dfr(d$early, \(e0) predict_final(calib |> filter(dial == k$dial, season != k$season), e0, w_add, q)))) |>
    ungroup() |> left_join(ch, by = "dial") |>
    mutate(flag = (prev < final_lo | prev > final_hi) & abs(final_mid - prev) > churn) |> filter(!is.na(flag))
  judged <- w |> filter(counted, !is.na(flag))
  base <- h |> filter(team_id != WIZ_ID) |> group_by(kind, season, team_id) |> summarise(dials = n(), flagged = sum(flag), share = flagged / dials, .groups = "drop")
  list(wiz = w, hist = h, base = base, flags = sum(judged$flag), dials = nrow(judged)
       , pctile = mean(base$share[base$kind == "Kept coach"] >= sum(judged$flag) / nrow(judged)))
}
spec <- run_spec()
wiz <- spec$wiz |>
  mutate(now_verdict = case_when(is.na(now) | is.na(flag) ~ "No data yet"
                                 , !counted ~ "Not counted (noisy)"
                                 , flag ~ "Moved more than normal"
                                 , TRUE ~ "No clear change")
         , toward = case_when(!coalesce(flag, FALSE) ~ "", (final_mid < prev) == (dir == "more") ~ "toward the label", TRUE ~ "away from the label"))
# outward ranks: 1 = the petal's edge, whatever the dial's direction
flip <- \(rk, dir) if_else(dir == "less", N_TEAMS + 1 - rk, rk)
wiz <- wiz |> mutate(across(c(first, prev, now, final_mid), ~ flip(.x, dir), .names = "out_{.col}")
                     , out_lo = flip(if_else(dir == "less", final_hi, final_lo), dir)
                     , out_hi = flip(if_else(dir == "less", final_lo, final_hi), dir)) |>
  rename(churn80 = churn) |> left_join(trust |> select(dial, r), by = "dial")
cat("\n== Keefe vs Keefe: outward ranks (1 = the petal's edge) and verdicts ==\n")
pt(wiz |> select(area, long, out_first, out_prev, out_now, out_lo, out_hi, churn80, r, now_verdict) |> arrange(area), digits = 2)

baseline <- spec$base
keefe_flags <- spec$flags; keefe_dials <- spec$dials; keefe_share <- keefe_flags / keefe_dials
flagged_dials <- wiz |> filter(now_verdict == "Moved more than normal")
keefe_distinct <- n_distinct(flagged_dials$cluster)                 # overlapping dials (pace / late clock, starters / top three) count once
cat("\n== Share of dials flagged, by whether the team kept its coach ==\n")
pt(baseline |> group_by(kind, season) |> summarise(teams = n(), dials = first(dials), mean_share = mean(share), .groups = "drop"), digits = 2)
pt(baseline |> group_by(kind) |> summarise(team_seasons = n(), mean_share = mean(share), p90 = quantile(share, 0.9)), digits = 2)
cat(sprintf("Wizards %s: %d of %d counted dials (%.0f%%), %d distinct changes; same-coach team-seasons at or above: %.1f%%; new-coach team-seasons at or above: %.1f%%\n"
            , CUR, keefe_flags, keefe_dials, 100 * keefe_share, keefe_distinct, 100 * spec$pctile
            , 100 * mean(baseline$share[baseline$kind == "New coach"] >= keefe_share)))

# the instrument's own blind spot: does it tell a new coach from the same coach at all?
kind_gap <- baseline |> group_by(kind) |> summarise(mean_share = mean(share), .groups = "drop") |> deframe()

# only the dials judged in every baseline season (the play-by-play and box-score dials exist only from 2024-25)
common_w <- wiz |> filter(dial %in% common_dials, now_verdict %in% c("Moved more than normal", "No clear change"))
common_base <- spec$hist |> filter(team_id != WIZ_ID, kind == "Kept coach", dial %in% common_dials) |> group_by(season, team_id) |> summarise(share = mean(flag), .groups = "drop")
common <- list(flags = sum(common_w$flag), dials = nrow(common_w), base_mean = mean(common_base$share), pctile = mean(common_base$share >= mean(common_w$flag)))
cat(sprintf("Common dials only: %d of %d; same-coach mean %.0f%%; at or above: %.1f%%\n", common$flags, common$dials, 100 * common$base_mean, 100 * common$pctile))

# regression to the mean: an extreme rank last season flags more easily, and the 17-65 Wizards were extreme on
# many dials. Model the flag rate on how far last season's rank sat from the middle, by area, in the same-coach
# baseline, and compare the Wizards' count with what that model expects from their own starting points.
same_hist <- spec$hist |> filter(team_id != WIZ_ID, kind == "Kept coach") |> left_join(dials |> select(dial, area), by = "dial") |> mutate(ext = abs(prev - 15.5))
ext_fit <- glm(flag ~ ext * area, family = binomial, data = same_hist)
keefe_expected <- sum(predict(ext_fit, newdata = wiz |> filter(now_verdict %in% c("Moved more than normal", "No clear change")) |> mutate(ext = abs(prev - 15.5)), type = "response"))
o_minus_e <- same_hist |> mutate(p = predict(ext_fit, type = "response")) |> group_by(season, team_id) |> summarise(ome = sum(flag) - sum(p), .groups = "drop")
keefe_ome_pctile <- mean(o_minus_e$ome >= keefe_flags - keefe_expected)
cat(sprintf("Expected flags given last season's extremes: %.1f (observed %d); same-coach team-seasons with at least as large an excess: %.1f%%\n"
            , keefe_expected, keefe_flags, 100 * keefe_ome_pctile))

# the same verdicts on the Wizards one year earlier, 2025-26's first games against 2024-25: a placebo for "something is different now"
placebo <- spec$hist |> filter(team_id == WIZ_ID, season == PREV) |> summarise(flags = sum(flag), dials = n())
cat(sprintf("Placebo, Wizards %s first %d games vs %s: %d of %d dials\n", PREV, N_EARLY, KEEFE[1], placebo$flags, placebo$dials))

# how much does the answer depend on the choices above? window, range and churn yardstick, varied
sens <- tribble(~choice, ~w_add, ~q_lo, ~q_hi, ~churn_p,
                "As published", 0, 0.10, 0.90, 0.80,
                "Narrower window", -1, 0.10, 0.90, 0.80,
                "Wider window", 2, 0.10, 0.90, 0.80,
                "Wider range (5th-95th)", 0, 0.05, 0.95, 0.80,
                "Narrower range (25th-75th)", 0, 0.25, 0.75, 0.80,
                "Looser yardstick (70th pct)", 0, 0.10, 0.90, 0.70,
                "Stricter yardstick (90th pct)", 0, 0.10, 0.90, 0.90) |>
  mutate(res = pmap(list(w_add, q_lo, q_hi, churn_p), \(wa, lo, hi, cp) if (wa == 0 && lo == 0.1 && hi == 0.9 && cp == 0.8) .env$spec else run_spec(wa, c(lo, hi), cp))
         , flags = map_dbl(res, \(r) r$flags), dials = map_dbl(res, \(r) r$dials)
         , same_mean = map_dbl(res, \(r) mean(r$base$share[r$base$kind == "Kept coach"])), pctile_above = map_dbl(res, "pctile")) |>
  select(-res)
cat("\n== Sensitivity of the Wizards' count to the procedure's choices ==\n")
pt(sens, digits = 2)

# --- 4. Roster and schedule: what changed around the coach --------------------------------
# minutes from five-man lineups (each lineup's minutes credited to its five players, so traded players stay put)
pmin_by <- rd("panel_lineups") |> filter(season %in% c(PREV, CUR)) |> mutate(pid = str_extract_all(group_id, "\\d+")) |>
  select(team_id, season, group_id, pid, min) |> unnest(pid)
player_min <- pmin_by |> group_by(team_id, season, pid) |> summarise(min = sum(min), .groups = "drop")
prev_min <- player_min |> filter(season == PREV) |> select(team_id, pid, prev_min = min)
continuity <- player_min |> filter(season == CUR) |> left_join(prev_min, by = c("team_id", "pid")) |>
  group_by(team_id) |> summarise(cont_any = sum(min[!is.na(prev_min)]) / sum(min), cont_500 = sum(min[coalesce(prev_min, 0) >= 500]) / sum(min), .groups = "drop") |>
  mutate(rank_any = rank(cont_any), rank_500 = rank(cont_500))     # 1 = least continuity
newcomer_lineups <- pmin_by |> filter(season == CUR, team_id == WIZ_ID) |> left_join(prev_min, by = c("team_id", "pid")) |>
  group_by(group_id) |> summarise(min = first(min), has_new = any(coalesce(prev_min, 0) < 500), .groups = "drop") |>
  summarise(share_with_newcomer = sum(min[has_new]) / sum(min)) |> pull()
wiz_names <- box_league |> filter(season == CUR, team_id == WIZ_ID) |> distinct(pid = as.character(person_id), name)
wiz_top <- player_min |> filter(season == CUR, team_id == WIZ_ID) |> mutate(share = 5 * min / sum(min)) |>
  left_join(prev_min, by = c("team_id", "pid")) |> left_join(wiz_names, by = "pid") |> arrange(desc(min)) |> head(8) |>
  transmute(name, share_of_minutes = share / 5 * 5, prev_wiz_min = round(coalesce(prev_min, 0)))
cat("\n== Roster continuity, ", CUR, ": share of minutes to players who played for the team in ", PREV, " (rank 1 = least) ==\n", sep = "")
pt(continuity |> filter(team_id == WIZ_ID), digits = 2)
cat("Wizards lineup minutes including a player with <500 Wizards minutes last season:", round(newcomer_lineups, 3), "\n")
pt(wiz_top, digits = 2)
# schedule: the opponents' net rating (full season for past years, to date for this one) over the first N games
nets <- rd("panel_advanced") |> select(season, team_id, net_rating) |> left_join(rd("panel_passing") |> select(season, team_id, abbr = team_abbreviation), by = c("season", "team_id"))
sched <- map_dfr(KEEFE, \(s) rd(paste0("wizards_gamelog_", s)) |> filter(!is.na(wl), wl != "") |>
                   mutate(date = as.Date(game_date, format = "%b %d, %Y"), abbr = word(matchup, -1)) |> arrange(date) |> head(n_games) |>
                   left_join(nets |> filter(season == s) |> select(abbr, net_rating), by = "abbr") |> summarise(season = s, opp_net = mean(net_rating)))
cat("\n== Opponents' net rating over the Wizards' first", n_games, "games ==\n")
pt(sched, digits = 2)

# --- 5. Is it the roster? What the new players' own habits predict -------------------------
# Hold every player's habits fixed at the earlier season (wherever he played it) and change only who plays how
# many minutes. The change in that roster-implied profile is pure roster composition: no coaching in it at all.
# Validated on every team across two offseasons before it is asked about the Wizards.
# A player's habit pools his last two seasons where both were pulled (minutes-weighted), so a short, injury-hit season
# (Trae Young's 384 minutes in 2025-26) doesn't stand in for his level. Touch habits stay single-season (per-game table).
pool_two <- function(stem, r, cols) {
  rs <- c(r, prev_season(r)); rs <- rs[file.exists(file.path(DATA_DIR, paste0(stem, "_", rs, ".csv")))]
  map_dfr(rs, \(x) rd(paste0(stem, "_", x))) |> group_by(player_id) |> summarise(across(all_of(cols), \(v) sum(v, na.rm = TRUE)), .groups = "drop")
}
habits <- function(r) {
  zcols <- c("fga_restricted_area", "fga_in_the_paint_non_ra", "fga_mid_range", "fga_left_corner_3", "fga_right_corner_3", "fga_above_the_break_3")
  b <- pool_two("player_base", r, c("min", "fga", "pf", "blk", "stl")); z <- pool_two("player_zones", r, zcols); ps <- rd(paste0("player_poss_", r))
  b |> transmute(person_id = player_id, min, fga_pm = fga / min, pf_pm = pf / min, blk_pm = blk / min, stl_pm = stl / min) |>
    left_join(z |> transmute(person_id = player_id, tot = fga_restricted_area + fga_in_the_paint_non_ra + fga_mid_range + fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3
                             , rim = fga_restricted_area / tot, three = (fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3) / tot) |> select(-tot), by = "person_id") |>
    left_join(ps |> transmute(person_id = player_id, touch_pm = touches / min, sec_touch = avg_sec_per_touch), by = "person_id") |>
    filter(min >= 100)                                     # habits from fewer than 100 minutes are noise
}
implied_profile <- function(m, r) {
  h <- habits(r)
  lg <- h |> summarise(across(c(fga_pm, pf_pm, blk_pm, stl_pm, rim, three, touch_pm, sec_touch), ~ weighted.mean(.x, min, na.rm = TRUE)))
  box_league |> filter(season == m, mins > 0) |> group_by(team_id, person_id) |> summarise(mins = sum(mins), .groups = "drop") |>
    left_join(h |> select(-min), by = "person_id") |>
    mutate(imputed = is.na(fga_pm), across(c(fga_pm, pf_pm, blk_pm, stl_pm, rim, three, touch_pm, sec_touch), ~ coalesce(.x, lg[[cur_column()]]))) |>   # rookies etc. get the league habit
    group_by(team_id) |>
    summarise(imp_rim = weighted.mean(rim, mins * fga_pm), imp_three = weighted.mean(three, mins * fga_pm)
              , imp_fouls = sum(mins * pf_pm) / sum(mins), imp_blocks = sum(mins * blk_pm) / sum(mins), imp_steals = sum(mins * stl_pm) / sum(mins)
              , imp_hold = weighted.mean(sec_touch, mins * touch_pm), imputed_share = sum(mins[imputed]) / sum(mins), .groups = "drop") |>
    mutate(season = m)
}
roster_pairs <- tribble(~measure, ~implied, ~actual, ~label,
  "rim", "imp_rim", "rim_share", "Rim share of shots",
  "three", "imp_three", "three_share", "3PA share of shots",
  "fouls", "imp_fouls", "opp_fta_rate", "Opponent free-throw rate (fouling)",
  "rim_def", "imp_blocks", "opp_rim_share", "Opponent rim share allowed",
  "steals", "imp_steals", "opp_tov_pct", "Opponent turnover rate (forcing turnovers)",
  "pace", "imp_hold", "pace", "Pace",
  "late", "imp_hold", "late_share", "Late-clock share")
transitions <- tibble(r = KEEFE[1:2], m_prev = KEEFE[1:2], m_now = KEEFE[2:3])
roster_change <- pmap_dfr(transitions, \(r, m_prev, m_now) {
  ip <- implied_profile(m_prev, r); inow <- implied_profile(m_now, r)
  act <- dial_full |> filter(season %in% c(m_prev, m_now)) |> select(season, team_id, any_of(roster_pairs$actual))
  map_dfr(seq_len(nrow(roster_pairs)), \(i) {
    rp <- roster_pairs[i, ]
    tibble(team_id = inow$team_id, d_implied = inow[[rp$implied]] - ip[[rp$implied]][match(inow$team_id, ip$team_id)]
           , imputed_share = inow$imputed_share) |>
      left_join(act |> filter(season == m_now) |> select(team_id, now = all_of(rp$actual)), by = "team_id") |>
      left_join(act |> filter(season == m_prev) |> select(team_id, before = all_of(rp$actual)), by = "team_id") |>
      mutate(d_actual = now - before, measure = rp$measure, label = rp$label, transition = paste(m_prev, "to", m_now))
  })
}) |>
  group_by(measure, transition) |> mutate(z_implied = (d_implied - mean(d_implied, na.rm = TRUE)) / sd(d_implied, na.rm = TRUE)
                                          , z_actual = (d_actual - mean(d_actual, na.rm = TRUE)) / sd(d_actual, na.rm = TRUE)) |> ungroup()
# how well the roster predicts every other team's change, and what it predicts for the Wizards now
roster_fit <- roster_change |> group_by(measure, label) |> group_modify(\(d, k) {
  others <- d |> filter(team_id != WIZ_ID)
  fit <- lm(z_actual ~ z_implied + transition, data = others)
  wz <- d |> filter(team_id == WIZ_ID, transition == paste(PREV, "to", CUR))
  pr <- predict(fit, newdata = wz, interval = "prediction", level = 0.8)
  tibble(r_prev = with(others |> filter(transition == paste(KEEFE[1], "to", PREV)), cor(z_implied, z_actual, use = "complete.obs"))
         , r_now = with(others |> filter(transition == paste(PREV, "to", CUR)), cor(z_implied, z_actual, use = "complete.obs"))
         , wiz_implied = wz$z_implied, wiz_actual = wz$z_actual, wiz_pred = pr[1, "fit"], pred_lo = pr[1, "lwr"], pred_hi = pr[1, "upr"]
         , wiz_imputed = wz$imputed_share)
}) |> ungroup() |>
  mutate(inside = wiz_actual >= pred_lo & wiz_actual <= pred_hi)
# how far off its own line each team sits (residual from the fit on all teams, per measure), to see whether the Wizards are unusual
roster_resid <- roster_change |> group_by(measure) |> group_modify(\(d, k) d |> mutate(resid = residuals(lm(z_actual ~ z_implied + transition, data = d, na.action = na.exclude)))) |> ungroup() |>
  group_by(measure) |> mutate(rank_most_below = rank(resid)) |> ungroup()
wiz_resid <- roster_resid |> filter(team_id == WIZ_ID, transition == paste(PREV, "to", CUR)) |> select(measure, resid, rank_most_below) |>
  mutate(team_seasons = sum(roster_resid$measure == "fouls" & !is.na(roster_resid$resid)))
cat("\n== Wizards' distance from the roster line (rank 1 = furthest below, out of all team-offseasons) ==\n")
pt(wiz_resid, digits = 2)
cat("\n== Roster-implied change vs actual change (z within season; r across the other 29 teams) and the Wizards ==\n")
pt(roster_fit, digits = 2)

# the Wizards in raw units (percentage points of shots, fouls per 36), and who drives each prediction. A player's
# contribution to the roster-implied change is his share of the team's weight this season minus last season, times
# how far his habit sits from last season's roster-implied average (so a newcomer who shoots fewer threes than the
# old roster pushes the prediction down); the contributions add up to the roster-implied change exactly.
wiz_raw <- roster_change |> filter(team_id == WIZ_ID, transition == paste(PREV, "to", CUR)) |> select(measure, label, d_implied, d_actual, before, now)
h_prev <- habits(PREV)
lg_h <- h_prev |> summarise(across(c(fga_pm, pf_pm, rim, three), ~ weighted.mean(.x, min, na.rm = TRUE)))
wiz_min <- box_league |> filter(team_id == WIZ_ID, season %in% c(PREV, CUR), mins > 0) |> group_by(season, person_id, name) |> summarise(mins = sum(mins), .groups = "drop")
contrib_base <- wiz_min |> group_by(person_id) |> mutate(name = last(name)) |> ungroup() |>
  pivot_wider(id_cols = c(person_id, name), names_from = season, values_from = mins, values_fill = 0) |>
  rename(min_prev = all_of(PREV), min_now = all_of(CUR)) |>
  left_join(h_prev |> select(person_id, fga_pm, pf_pm, rim, three), by = "person_id") |>
  mutate(rookie_or_new = is.na(fga_pm), across(c(fga_pm, pf_pm, rim, three), ~ coalesce(.x, lg_h[[cur_column()]])))
roster_contrib <- bind_rows(
  contrib_base |> mutate(w_prev = min_prev * fga_pm / sum(min_prev * fga_pm), w_now = min_now * fga_pm / sum(min_now * fga_pm)
                         , measure = "three", habit = three, contribution = (w_now - w_prev) * (three - sum(w_prev * three))),
  contrib_base |> mutate(w_prev = min_prev * fga_pm / sum(min_prev * fga_pm), w_now = min_now * fga_pm / sum(min_now * fga_pm)
                         , measure = "rim", habit = rim, contribution = (w_now - w_prev) * (rim - sum(w_prev * rim))),
  contrib_base |> mutate(w_prev = min_prev / sum(min_prev), w_now = min_now / sum(min_now)
                         , measure = "fouls", habit = 36 * pf_pm, contribution = 36 * (w_now - w_prev) * (pf_pm - sum(w_prev * pf_pm)))) |>
  select(measure, name, min_prev, min_now, habit, rookie_or_new, w_prev, w_now, contribution)
cat("\n== Who drives the Wizards' roster-implied change (largest contributions either way) ==\n")
pt(roster_contrib |> group_by(measure) |> slice_max(abs(contribution), n = 6) |> ungroup(), digits = 3)
# the habit gap behind the three-point prediction: newcomers vs the players whose minutes they took
habit_gap <- contrib_base |> mutate(group = case_when(min_prev < 500 & min_now > 0 ~ "Arrived (under 500 Wizards minutes last season)"
                                                    , min_now == 0 ~ "Gone (no minutes this season)", TRUE ~ "Stayed")) |>
  group_by(group) |> summarise(players = n(), minutes_now = sum(min_now), minutes_prev = sum(min_prev)
                               , three = weighted.mean(three, (min_now + min_prev) * fga_pm), rim = weighted.mean(rim, (min_now + min_prev) * fga_pm)
                               , fouls36 = 36 * weighted.mean(pf_pm, min_now + min_prev), .groups = "drop")
pt(habit_gap, digits = 3)

p_roster <- roster_change |> filter(measure %in% c("three", "fouls", "rim")) |>
  left_join(roster_fit |> select(measure, r_prev, r_now), by = "measure") |>
  mutate(short = c(three = "Three-point share", fouls = "Fouling (opponent FT rate)", rim = "Rim share")[measure]
         , facet = paste0(short, "   r = ", sprintf("%.2f", r_prev), ", ", sprintf("%.2f", r_now))
         , facet = factor(facet, unique(facet[order(match(measure, c("three", "fouls", "rim")))]))
         , is_wiz = team_id == WIZ_ID & transition == paste(PREV, "to", CUR)) |>
  ggplot(aes(z_implied, z_actual)) +
  geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
  geom_smooth(data = \(d) d |> filter(team_id != WIZ_ID), method = "lm", formula = y ~ x, colour = "grey40", fill = "grey85", linewidth = 0.6) +
  geom_point(data = \(d) d |> filter(!is_wiz), colour = "grey65", size = 2.2, alpha = 0.8) +
  geom_point(data = \(d) d |> filter(is_wiz), colour = wiz_blue, size = 5.5) +
  geom_text(data = \(d) d |> filter(is_wiz), label = "Wizards", colour = wiz_blue, family = font, size = 4.8, hjust = -0.2, vjust = -0.5) +
  facet_wrap(~ facet, scales = "free", ncol = 1) +
  labs(title = "What the new roster predicts"
       , subtitle = paste0("Each dot is one team's change over an offseason. Across: what\nits new roster predicted. Up: what happened. Near the line =\nthe roster explains it. r = fit for other teams, last offseason, this one.")
       , x = "Change predicted by the roster (SD)", y = "Actual change (SD)", caption = caption_text) +
  theme_wp(17) + theme(strip.text = element_text(face = "bold", size = rel(1), hjust = 0), panel.spacing.y = unit(1.2, "lines"))
save_plot("roster_implied_change.png", p_roster, width = 7.5, height = 15)

# --- 6. Fouling against the schedule -------------------------------------------------------
# opponents' own free-throw rate (to date) over the Wizards' first N games, against what the Wizards allowed them
ff_all <- rd("panel_fourfactors") |> select(season, team_id, fta_rate, opp_fta_rate) |> left_join(rd("panel_passing") |> select(season, team_id, abbr = team_abbreviation), by = c("season", "team_id"))
foul_sched <- map_dfr(KEEFE, \(s) {
  gl <- rd(paste0("wizards_gamelog_", s)) |> filter(!is.na(wl), wl != "") |> mutate(date = as.Date(game_date, format = "%b %d, %Y"), abbr = word(matchup, -1)) |> arrange(date) |> head(n_games)
  f <- ff_all |> filter(season == s)
  tibble(season = s, opp_usual = mean(f$fta_rate[match(gl$abbr, f$abbr)]), league = mean(f$fta_rate), wiz_allowed = f$opp_fta_rate[f$team_id == WIZ_ID])
}) |> mutate(below_usual = wiz_allowed - opp_usual)
cat("\n== Opponents' usual free-throw rate vs what the Wizards allowed (first", n_games, "games; full season for past years' opponents) ==\n")
pt(foul_sched, digits = 3)

# --- 7. The decisions that are only his, three seasons ------------------------------------
own <- bind_rows(
  dec_full |> transmute(season, team_id, value = to_per_run, what = "Timeouts per opponent run faced"),
  dec_full |> transmute(season, team_id, value = win_rate, what = "Coach's challenges won"),
  dial_full |> filter(season %in% KEEFE) |> transmute(season, team_id, value = per10, what = "Minutes next game per +10 plus-minus")) |>
  filter(!is.na(value)) |> group_by(what, season) |> mutate(rank_most = rank(-value)) |> ungroup()
own_wiz <- own |> filter(team_id == WIZ_ID)
cat("\n== Keefe's own decisions, three seasons (rank 1 = most) ==\n")
pt(own_wiz |> select(what, season, value, rank_most), digits = 3)
# every team as a dot (beeswarm), the league's middle half and 80% as ggdist bands behind them, Keefe as the big labelled dot
own_meta <- tribble(~what, ~panel, ~left, ~right, ~fmt,
  "Coach's challenges won", "Challenges won (%)", "Loses more", "Wins more", "pct",
  "Timeouts per opponent run faced", "Timeouts called during opponent runs (% of runs)", "Lets runs play out", "Stops the game more", "pct",
  "Minutes next game per +10 plus-minus", "Minutes next game after a +10 plus-minus night", "Cuts minutes after good nights", "Adds minutes after good nights", "num")
own_plot <- own |> left_join(own_meta, by = "what") |>
  mutate(panel = factor(panel, own_meta$panel), season_lab = factor(if_else(season == CUR, paste0(season, "\n(", n_games, " games)"), season), rev(c(KEEFE[1:2], paste0(CUR, "\n(", n_games, " games)")))),
         is_wiz = team_id == WIZ_ID, value = if_else(fmt == "pct", 100 * value, value))
own_lab <- own_plot |> filter(is_wiz) |> mutate(lab = paste0(ordinal(round(rank_most)), if_else(fmt == "pct", paste0(" (", round(value), "%)"), paste0(" (", sprintf("%+.1f", value), ")"))))
own_ends <- own_plot |> group_by(panel, left, right) |> summarise(lo = min(value), hi = max(value), .groups = "drop")
p_own <- ggplot(own_plot, aes(value, season_lab)) +
  ggdist::stat_interval(data = \(d) d |> filter(!is_wiz), .width = c(0.5, 0.8), linewidth = 9, alpha = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = c(cream, "#DCC7BF")) +
  ggbeeswarm::geom_quasirandom(data = \(d) d |> filter(!is_wiz), orientation = "y", width = 0.28, size = 1.9, colour = "grey45", alpha = 0.85) +
  geom_point(data = \(d) d |> filter(is_wiz), size = 5.2, colour = wiz_blue) +
  geom_text(data = own_lab, aes(label = lab), nudge_y = 0.36, family = font, size = 3.6, colour = wiz_blue, fontface = "bold") +
  geom_text(data = own_ends, aes(x = lo, y = 0.35, label = paste0("← ", left)), hjust = 0, size = 3.1, family = font, colour = "grey40", inherit.aes = FALSE) +
  geom_text(data = own_ends, aes(x = hi, y = 0.35, label = paste0(right, " →")), hjust = 1, size = 3.1, family = font, colour = "grey40", inherit.aes = FALSE) +
  facet_wrap(~ panel, ncol = 1, scales = "free_x") +
  scale_y_discrete(expand = expansion(add = c(0.75, 0.55))) +
  scale_x_continuous(labels = \(x) x, expand = expansion(mult = 0.04)) +
  labs(title = "His in-game calls: about where they were"
       , subtitle = paste0("Every NBA team is a grey dot, each season; the shaded band holds the<br>middle half (darker) and 80% of the league. <span style='color:", wiz_blue, "'>**Keefe**</span> is the<br>large navy dot, with his league rank (1st = most).")
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp(14) + theme(plot.subtitle = ggtext::element_markdown(lineheight = 1.2), strip.text = element_text(face = "bold", hjust = 0, size = rel(1.05))
                       , panel.grid.major.y = element_blank(), panel.spacing.y = unit(1.6, "lines"), axis.text.y = element_text(colour = "grey25"))
save_plot("keefe_own_decisions.png", p_own, width = 7.5, height = 13)

# How much stronger is the roster? Hollinger's Game Score per 36 minutes (a box-score production index that,
# unlike plus-minus, does not inherit the player's team result) from each player's previous season, wherever
# he played (500+ minutes; anyone else, rookies included, gets the league value for 100-499-minute players, which
# undersells a top pick), weighted by his minutes on this team this season.
roster_strength <- map_dfr(KEEFE, \(m) {
  r <- prev_season(m); f <- file.path(DATA_DIR, paste0("player_base_", r, ".csv"))
  if (!file.exists(f)) return(NULL)
  b <- pool_two("player_base", r, c("min", "pts", "fgm", "fga", "fta", "ftm", "oreb", "dreb", "stl", "ast", "blk", "pf", "tov")) |>   # last two seasons pooled
    mutate(gmsc = pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) + 0.7 * oreb + 0.3 * dreb + stl + 0.7 * ast + 0.7 * blk - 0.4 * pf - tov)
  fill <- b |> filter(min < 500, min >= 100) |> summarise(v = 36 * sum(gmsc) / sum(min)) |> pull(v)
  box_league |> filter(season == m, mins > 0) |> group_by(team_id, person_id) |> summarise(mins = sum(mins), .groups = "drop") |>
    left_join(b |> filter(min >= 500) |> transmute(person_id = player_id, gs36 = 36 * gmsc / min), by = "person_id") |>
    mutate(gs36 = coalesce(gs36, fill)) |>
    group_by(team_id) |> summarise(strength = weighted.mean(gs36, mins), .groups = "drop") |>
    mutate(season = m, rank_best = rank(-strength))
})
cat("\n== Roster strength (minutes-weighted prior-season Game Score per 36), Wizards; rank 1 = strongest ==\n")
pt(roster_strength |> filter(team_id == WIZ_ID), digits = 2)

# the 2025-26 tank, in one number: how many different players the team used
players_used <- box_league |> filter(mins > 0) |> group_by(season, team_id) |> summarise(players = n_distinct(person_id), games = n_distinct(game_id), .groups = "drop") |>
  group_by(season) |> mutate(rank_most = rank(-players)) |> ungroup()
cat("\n== Players used, Wizards (rank 1 = most) ==\n")
pt(players_used |> filter(team_id == WIZ_ID))

# --- 7b. The rotation is his: which parts of it changed? -----------------------------------
# A starting-lineup change is "forced" when one of the previous game's starters didn't play the next game, and
# "chosen" when all five played and the starting five changed anyway. And how hard does he lean on his best available
# players: the top five by minutes per game played (20+ games; 4+ this season), whatever games they missed.
st5 <- box_league |> filter(starter) |> group_by(season, team_id, g) |> summarise(five = list(sort(person_id)), .groups = "drop")
played_ids <- box_league |> filter(mins > 0) |> group_by(season, team_id, g) |> summarise(ids = list(person_id), .groups = "drop")
lineup_changes <- st5 |> group_by(season, team_id) |> arrange(g, .by_group = TRUE) |> mutate(prev_five = lag(five)) |> ungroup() |>
  filter(!map_lgl(prev_five, is.null)) |> left_join(played_ids, by = c("season", "team_id", "g")) |>
  mutate(changed = map2_lgl(five, prev_five, \(a, b) !setequal(a, b)), all_available = map2_lgl(prev_five, ids, \(p, i) all(p %in% i))
         , kind = case_when(!changed ~ "same", all_available ~ "chosen", TRUE ~ "forced")) |>
  group_by(season, team_id) |> summarise(chosen_per_10 = 10 * mean(kind == "chosen"), forced_per_10 = 10 * mean(kind == "forced"), .groups = "drop") |>
  group_by(season) |> mutate(rank_chosen = rank(-chosen_per_10), rank_forced = rank(-forced_per_10), lg_chosen = mean(chosen_per_10)) |> ungroup()
top_load <- box_league |> filter(mins > 0) |> group_by(season, team_id, person_id) |> summarise(gp = n(), mpg = mean(mins), .groups = "drop") |>
  filter(gp >= if_else(season == CUR, 4, 20)) |> group_by(season, team_id) |> slice_max(mpg, n = 5, with_ties = FALSE) |>
  summarise(top5_mpg = mean(mpg), .groups = "drop") |> group_by(season) |> mutate(rank_top5 = rank(-top5_mpg), lg_top5 = mean(top5_mpg)) |> ungroup()
rotation_choice <- lineup_changes |> filter(team_id == WIZ_ID) |> left_join(top_load |> filter(team_id == WIZ_ID), by = c("season", "team_id"))
cat("\n== The rotation: chosen vs forced starting-lineup changes per 10 games, and top-five minutes when available (rank 1 = most) ==\n")
pt(rotation_choice, digits = 3)

# --- 8. Fouling without the intentional fouls ------------------------------------------
# A team that trails late gets fouled on purpose; a 17-65 team trails late far more than a 4-6 one. Recompute the
# free-throw rate the defense allows with the last two minutes of the fourth quarter and overtime removed.
opp_ft <- ft_split |> inner_join(ft_split |> select(season, game_id, defender = team_id), by = c("season", "game_id"), relationship = "many-to-many") |>
  filter(defender != team_id) |>
  inner_join(game_idx |> filter(g <= n_games) |> select(season, game_id, defender = team_id), by = c("season", "game_id", "defender")) |>
  group_by(season, defender) |> summarise(ft_rate_all = sum(fta) / sum(fga), ft_rate_no_late = sum(fta_early) / sum(fga_early), .groups = "drop") |>
  group_by(season) |> mutate(rank_fewest_all = rank(ft_rate_all), rank_fewest_no_late = rank(ft_rate_no_late), league_no_late = mean(ft_rate_no_late)) |> ungroup()
cat("\n== Free-throw rate allowed over the first", n_games, "games, with and without the last 2 minutes of Q4/OT (rank 1 = fewest) ==\n")
pt(opp_ft |> filter(defender == WIZ_ID), digits = 3)

# --- 9. How Keefe is using the new players: minutes when available, and who closes -------
wiz_avail <- box_league |> filter(season == CUR, team_id == WIZ_ID) |> group_by(person_id, name) |>
  summarise(played = sum(mins > 0), team_games = n_distinct(game_id), mpg = sum(mins) / pmax(sum(mins > 0), 1), .groups = "drop")
closing <- stints |> mutate(s = pmax(start, 2580), e = pmin(end, 2880)) |> filter(e > s) |>       # the last five minutes of regulation
  group_by(person_id) |> summarise(close_min = sum(e - s) / 60, .groups = "drop")
closers <- wiz_avail |> left_join(closing, by = "person_id") |>
  mutate(close_min = coalesce(close_min, 0), close_pg = close_min / pmax(played, 1)) |>
  filter(played >= 3, mpg >= 12) |> arrange(desc(mpg)) |> select(name, played, team_games, mpg, close_pg)
cat("\n== Wizards", CUR, ": minutes per game played, and minutes per game played in the last 5:00 of regulation ==\n")
pt(closers, digits = 2)

grp_base <- baseline |> mutate(kind = factor(kind, c("Kept coach", "New coach")))
p_base <- ggplot(grp_base, aes(share)) +
  geom_histogram(binwidth = 1 / 21, boundary = 0, fill = "grey75", colour = "white") +
  geom_vline(xintercept = keefe_share, colour = wiz_blue, linewidth = 1.2) +
  geom_vline(data = grp_base |> group_by(kind) |> summarise(m = mean(share)), aes(xintercept = m), colour = "grey35", linetype = "dashed") +
  geom_text(data = tibble(kind = factor("Kept coach", levels(grp_base$kind))), aes(x = keefe_share, y = Inf), vjust = 1.5, hjust = -0.04, family = font, colour = wiz_blue, size = 4.6
            , label = sprintf("Wizards: %d of %d", keefe_flags, keefe_dials)) +
  facet_wrap(~ kind, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = percent, limits = c(0, 0.6), oob = scales::squish) +
  labs(title = "Big change, but not a coach's kind"
       , subtitle = paste0("Share of measures that moved more than a normal offseason,\nevery team, ", min(baseline$season), " to ", max(baseline$season), ". Dashed line = average.\n"
                           , "New coaches move teams no more than old ones (", percent(kind_gap[["New coach"]], 1), " vs ", percent(kind_gap[["Kept coach"]], 1), ").")
       , x = "Share of measures flagged", y = "Team-seasons", caption = caption_text) +
  theme_wp(17) + theme(strip.text = element_text(face = "bold", hjust = 0, size = rel(1.05)))
save_plot("change_baseline.png", p_base, width = 7.5, height = 8)

# --- Viz 1: the fans, four areas x three seasons ---------------------------------------
fan_df <- wiz |> select(dial, area, petal, dir, out_first, out_prev, out_now, out_lo, out_hi) |>
  pivot_longer(c(out_first, out_prev, out_now), names_to = "which", values_to = "out") |>
  mutate(season = c(out_first = KEEFE[1], out_prev = PREV, out_now = CUR)[which], r = N_TEAMS + 1 - out)
season_lab <- function(s) {
  rec <- dial_full |> filter(team_id == WIZ_ID, season == s)
  w <- rd("panel_advanced") |> filter(team_id == WIZ_ID, season == s)
  paste0(s, "\n", w$w, "-", w$l, if (s == CUR) paste0(", ", w$gp, " games") else "")
}
season_labs <- set_names(map_chr(KEEFE, season_lab), KEEFE)
fan_area <- function(a, show_title = TRUE) {
  d <- fan_df |> filter(area == a) |> mutate(petal = factor(petal, dials$petal[dials$area == a]), season = factor(season_labs[season], season_labs))
  # every petal carries its name and rank; the fan is turned 30 degrees so no petal points straight left or right,
  # which keeps names from running into the neighbouring fan at phone width
  ggplot(d, aes(petal, r)) +
    geom_col(width = 1, colour = "white", fill = area_cols[[a]], alpha = 0.85, linewidth = 0.4, na.rm = TRUE) +
    geom_hline(yintercept = 15.5, colour = "grey45", linewidth = 0.35, linetype = "dotted") +
    geom_hline(yintercept = 30.5, colour = "grey85", linewidth = 0.3) +
    geom_text(aes(y = 38.5, label = paste0(petal, "\n", if_else(is.na(out), "–", ordinal(round(out))))), size = 3.5, colour = "grey15", family = font, lineheight = 0.85) +
    coord_radial(expand = FALSE, inner.radius = 0.04, clip = "off", start = -pi / 6) +
    scale_y_continuous(limits = c(0, 40), breaks = NULL) +
    facet_wrap(~ season, nrow = 1) +
    labs(title = a, x = NULL, y = NULL) +
    theme_wp(15) +
    theme(panel.grid = element_blank(), axis.text.x = element_blank()
          , plot.title = element_text(colour = area_cols[[a]], size = rel(1.35), margin = margin(b = 2))
          , strip.text = element_text(face = "bold", lineheight = 0.95, size = rel(0.95), margin = margin(b = 26))
          , panel.spacing.x = unit(3.2, "lines"), plot.margin = margin(4, 26, 22, 26))
}
p_fans <- wrap_plots(map(names(area_cols), fan_area), ncol = 1) +
  plot_annotation(title = "The Wizards under Keefe"
                  , subtitle = "Each petal is a league rank. A petal that fills the circle is 1st in\nthe NBA at what its name says; one that barely shows is 30th."
                  , caption = caption_text, theme = theme_wp(15))
save_plot("keefe_evolution_fans.png", p_fans, width = 9, height = 19)

# --- Viz 1b: the same three seasons as NYT-style hairpins -----------------------------------
# each measure: a line from its 2024-25 league rank to 2025-26 (top), a U-turn, and an arrow to 2026-27 (bottom).
# x is the outward rank (1st = the end the petal's name points to) on a reversed axis, so 1st is on the right
make_hairpins <- function(sel = NULL, title = "How Keefe's Wizards have shifted", file = "keefe_hairpins.png", width = 9, height = 16.5, base = 13, label_full = FALSE, key_bottom = 1.01, key_top = 1.075, strip_names = NULL, off = 0.17, r = 0.5, strip_size = 1.1, extra = NULL) {
hp <- bind_rows(wiz |> select(dial, area, petal, long, out_first, out_prev, out_now), extra) |> filter(!is.na(out_first), !is.na(out_prev), !is.na(out_now), is.null(sel) | dial %in% sel) |>
  mutate(label = if (label_full) long else str_replace_all(petal, "\n", " "),
         area = if (is.null(strip_names)) factor(area, names(area_cols)) else { al <- paste0(area, ": ", strip_names[dial]); factor(al, unique(al[order(match(as.character(area), names(area_cols)))])) }) |>
  group_by(area) |> arrange(out_now, .by_group = TRUE) |> mutate(row = -row_number()) |> ungroup() |>
  mutate(loop = sign(out_prev - out_first) == -sign(out_now - out_prev) & abs(out_prev - out_first) >= 2 & abs(out_now - out_prev) >= 2,
         y_start = if_else(loop, row + off, row), y_end = if_else(loop, row - off, row))
interp <- \(x0, x1, y, n = 60) tibble(x = seq(x0, x1, length.out = n), y = y)
hp_path <- hp |> rowwise() |> reframe({
  dirn <- if (out_prev != out_first) sign(out_prev - out_first) else if (out_now != out_prev) -sign(out_now - out_prev) else 1
  th <- seq(0, pi, length.out = 40)
  (if (!loop) interp(out_first, out_now, row) |> mutate(part = 1) else
  bind_rows(interp(out_first, out_prev, row + off) |> mutate(part = 1),
            tibble(x = out_prev + dirn * r * sin(th), y = row + off * cos(th), part = 2),
            interp(out_prev, out_now, row - off) |> mutate(part = 3))) |>
    mutate(dial = dial, area = area, ord = row_number())
}) |> ungroup() |>
  group_by(dial) |> mutate(xend = lead(x), yend = lead(y), side = (x + coalesce(xend, x)) / 2 < 15.5) |> ungroup() |> filter(!is.na(xend))
# label beside the U-turn (outside it), and the 2026-27 rank just past the arrowhead
hp <- hp |> mutate(dirn = case_when(out_prev != out_first ~ sign(out_prev - out_first), out_now != out_prev ~ -sign(out_now - out_prev), TRUE ~ 1),
                   lab_x = out_prev + dirn * (r + 0.5), lab_h = if_else(dirn > 0, 1, 0),
                   end_dir = case_when(!loop & out_now != out_first ~ sign(out_now - out_first), out_now == out_prev ~ -dirn, TRUE ~ sign(out_now - out_prev)),
                   end_x = out_now + end_dir * 0.45, end_h = if_else(end_dir > 0, 1, 0))
side_cols <- c(`TRUE` = teal, `FALSE` = wiz_red)
p_hp <- ggplot() +
  geom_vline(xintercept = 15.5, colour = "grey15", linewidth = 0.4) +
  geom_segment(data = hp_path, aes(x, y, xend = xend, yend = yend, colour = side), linewidth = 1.25, lineend = "round") +
  geom_point(data = hp, aes(out_first, y_start, colour = out_first < 15.5), size = 1.8) +
  # on a straight arrow, a small tick marks where the middle season sat
  geom_point(data = hp |> filter(!loop, out_prev > pmin(out_first, out_now), out_prev < pmax(out_first, out_now)), aes(out_prev, row), shape = 124, size = 3.2, colour = "grey20") +
  geom_segment(data = hp, aes(x = out_now - end_dir * 0.5, xend = out_now, y = y_end, yend = y_end, colour = out_now < 15.5),
               linewidth = 1.25, arrow = arrow(length = unit(0.09, "inches"), type = "closed")) +
  geom_text(data = hp |> filter(is.null(strip_names)), aes(x = pmin(pmax((out_first + out_prev) / 2, 3), 28), y = y_start + 0.27, label = label), size = 3.3 * base / 13, family = font, colour = "grey15") +
  geom_text(data = hp, aes(x = end_x, y = y_end, label = ordinal(round(out_now)), hjust = end_h), size = 3.2, family = font, colour = "grey35", fontface = "bold") +
  scale_colour_manual(values = side_cols, guide = "none") +
  scale_x_reverse(limits = c(36, -3), breaks = c(30, 25, 20, 15, 10, 5, 1), labels = ordinal, position = "bottom", expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = 0.5)) +
  facet_wrap(~ area, ncol = 1, scales = "free_y", strip.position = "top") +
  labs(title = title,
       subtitle = paste0("League rank on each measure in each of his three seasons. Each line\nstarts at ", KEEFE[1], " (dot), runs to ", PREV, ", turns, and the arrow ends at\n", CUR, " (", n_games, " games). Where the team kept moving the same way,\nit's a straight arrow (a tick marks ", PREV, "). Right of the black line (navy)\nis the top half of the league at what the measure's name says.\n\n"),
       x = NULL, y = NULL, caption = caption_text) +
  theme_wp(base) + theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
                       panel.grid.major.x = element_line(colour = "grey92"), strip.text = element_text(face = "bold", hjust = 0.5, size = rel(strip_size)),
                       panel.spacing.y = unit(0.9, "lines"))
key_df <- tibble(x = c(22, 12), y = 0)
th <- seq(0, pi, length.out = 40)
p_key <- ggplot() +
  geom_path(data = tibble(x = c(24, 12), y = 0.3), aes(x, y), colour = teal, linewidth = 1.1) +
  geom_path(data = tibble(x = 12 - 1.6 * sin(th), y = 0.3 * cos(th)), aes(x, y), colour = teal, linewidth = 1.1) +
  geom_segment(aes(x = 12, xend = 20, y = -0.3, yend = -0.3), colour = teal, linewidth = 1.1, arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  geom_point(aes(24, 0.3), colour = teal, size = 1.8) +
  annotate("text", x = 24.8, y = 0.3, label = KEEFE[1], hjust = 1, size = 3, family = font, colour = "grey30") +
  annotate("text", x = 9.6, y = 0, label = PREV, hjust = 0, size = 3, family = font, colour = "grey30") +
  annotate("text", x = 20.8, y = -0.3, label = CUR, hjust = 1, size = 3, family = font, colour = "grey30") +
  scale_x_reverse(limits = c(30, 4)) + scale_y_continuous(limits = c(-0.6, 0.6)) + theme_void()
p_hp <- p_hp + inset_element(p_key, left = 0.7, bottom = key_bottom, right = 1.0, top = key_top, align_to = "panel", clip = FALSE)
save_plot(file, p_hp, width = width, height = height)
invisible(p_hp)
}
make_hairpins()
# the short version: one measure per area, the ones the post's argument rests on
# offensive box-outs per game (hustle tracking; rank 1 = most), a measure outside the petals with a clean three-season loop
boxouts_rk <- rd("panel_hustle") |> filter(season %in% KEEFE) |> group_by(season) |> mutate(rk = rank(-off_boxouts)) |> ungroup() |> filter(team_id == WIZ_ID) |>
  select(season, rk) |> deframe()
extra_hp <- tibble(dial = "off_boxouts", area = "Offense", petal = "Offensive box-outs", long = "Offensive box-outs per game",
                   out_first = boxouts_rk[[KEEFE[1]]], out_prev = boxouts_rk[[PREV]], out_now = boxouts_rk[[CUR]])
make_hairpins(sel = c("off_boxouts", "opp_fta_rate", "starter_share", "win_rate"), extra = extra_hp, title = "Where Keefe's Wizards have moved", file = "keefe_hairpins_key.png", width = 9, height = 8, base = 15, key_bottom = 1.16, key_top = 1.42, off = 0.26, r = 1.1, strip_size = 0.85,
              strip_names = c(off_boxouts = "offensive box-outs", three_share = "share of shots from three", opp_fta_rate = "fewest free throws allowed", starter_share = "starters' share of minutes", win_rate = "challenges won"))

# --- Viz 2: how much does a nine-game rank tell you? ------------------------------------
p_trust <- trust |> left_join(dials |> select(dial, petal), by = "dial") |>
  mutate(long = fct_reorder(long, r), area = factor(area, names(area_cols))) |>
  ggplot(aes(r, long, colour = area)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_segment(aes(x = 0, xend = r, yend = long), linewidth = 0.8, alpha = 0.5) +
  geom_point(size = 3.6) +
  geom_text(aes(label = paste0("±", round(mad_rank), " places")), hjust = -0.25, size = 3.1, colour = "grey30", family = font) +
  scale_colour_manual(values = area_cols, name = NULL) +
  scale_x_continuous(limits = c(min(0, min(trust$r)), 1.12), breaks = seq(0, 1, 0.25)) +
  labs(title = paste0("How much does a ", english_number(N_EARLY), "-game rank tell you?")
       , subtitle = paste0("Rank correlation between where each team stood after ", N_EARLY, " games and where it finished, ", min(calib$season[calib$area != "Decisions"]), " to ", max(calib$season)
                           , " (in-game decisions: ", paste(sort(unique(calib$season[calib$area == "Decisions"])), collapse = " and "), ").\n"
                           , "1 = the November order holds all season; 0 = it tells you nothing. Labels: the typical distance between November rank and final rank.")
       , x = "Rank correlation, early vs final", y = NULL, caption = caption_text) +
  theme_wp(12) + theme(legend.position = "top", axis.text.y = element_text(colour = "black"))
save_plot("early_rank_trust.png", p_trust, width = 12, height = 9.5)

# --- Viz 3: the change ledger --------------------------------------------------------
verdict_cols <- c(`Moved more than normal` = wiz_red, `No clear change` = "grey45", `Not counted (noisy)` = "grey70", `No data yet` = "grey80")
led <- wiz |> mutate(area = factor(area, names(area_cols)), long = factor(long, rev(dials$long)), now_verdict = factor(now_verdict, names(verdict_cols)))
p_led <- ggplot(led, aes(y = long)) +
  geom_segment(aes(x = pmax(out_prev - churn80, 0.5), xend = pmin(out_prev + churn80, 30.5), yend = long), colour = "grey91", linewidth = 7, na.rm = TRUE) +
  geom_segment(aes(x = out_lo, xend = out_hi, yend = long, colour = now_verdict), linewidth = 2.2, alpha = 0.45, na.rm = TRUE) +
  geom_point(aes(x = out_first), shape = 21, fill = "white", colour = "grey55", size = 2.6, na.rm = TRUE) +
  geom_point(aes(x = out_prev), shape = 21, fill = "grey45", colour = "grey45", size = 2.8, na.rm = TRUE) +
  geom_point(aes(x = out_now, colour = now_verdict), size = 3.8, na.rm = TRUE) +
  geom_text(aes(x = -0.5, label = now_verdict, colour = now_verdict), hjust = 0, size = 3.1, family = font, show.legend = FALSE) +
  scale_colour_manual(values = verdict_cols, guide = "none") +
  scale_x_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30), labels = ordinal, limits = c(30.8, -11), expand = c(0, 0)) +
  facet_grid(area ~ ., scales = "free_y", space = "free_y") +
  labs(title = "What has actually changed since last season"
       , subtitle = paste0("League rank on each dial, with 1st on the right at the end the label names. Open circle = ", KEEFE[1], "; grey dot = ", PREV, "; coloured dot = ", CUR, " so far,\n"
                           , "with its likely final range (from recent seasons at the same point; it held for ", percent(coverage, 1), " of teams checked out of season). Grey band = how far 80% of\n"
                           , "teams move in a normal offseason under the same coach. 'Moved more than normal' = last season's rank is outside this season's likely range,\n"
                           , "and the likely move is outside the grey band.")
       , x = "League rank", y = NULL, caption = caption_text) +
  theme_wp(12) + theme(strip.text.y = element_text(angle = 0, face = "bold", hjust = 0), axis.text.y = element_text(colour = "black", size = rel(0.85))
                       , panel.grid.major.x = element_line(colour = "grey94"))
save_plot("keefe_change_ledger.png", p_led, width = 13, height = 12)

# --- 10. Hold the players fixed: are the same players doing different things here? -------------
# The roster test above changes who plays and holds habits fixed. This is the mirror image: it holds the players
# fixed and asks whether each one's habits changed. For every player, his rate over his team's first N games is
# compared with his own previous full season, wherever he played it (200+ minutes then, 40+ now); the team's
# minutes-weighted average of those changes, centred on the league's, is a team effect with the roster taken out.
# Late-game fouls (last 2:00 of Q4/OT) are dropped, as above. Placebo: the same test at the N-game mark of 2025-26.
sp_pbp <- map_dfr(KEEFE, \(s) rd_games("pbp", s) |> mutate(season = s) |>
  select(season, game_id, period, clock, team_id, person_id, action_type, sub_type, is_field_goal, shot_value))
sp_clk <- \(x) as.numeric(str_match(x, "PT(\\d+)M")[, 2]) * 60 + as.numeric(str_match(x, "M([\\d.]+)S")[, 2])
def_foul_types <- c("Shooting", "Personal", "Loose Ball", "Personal Take", "Transition Take", "Away From Play", "Clear Path", "Flagrant Type 1", "Flagrant Type 2")
sp_ev <- sp_pbp |> filter(!is.na(person_id), person_id > 0) |> mutate(late = period >= 4 & sp_clk(clock) <= 120) |>
  group_by(season, game_id, team_id, person_id) |>
  summarise(fouls = sum(action_type == "Foul" & sub_type %in% def_foul_types & !late)
            , fga = sum(is_field_goal == 1, na.rm = TRUE), fg3a = sum(is_field_goal == 1 & shot_value == 3, na.rm = TRUE), .groups = "drop")
rm(sp_pbp)
sp_pg <- box_league |> filter(mins > 0) |> select(season, game_id, team_id, person_id, g, mins) |>
  left_join(sp_ev, by = c("season", "game_id", "team_id", "person_id")) |> mutate(across(c(fouls, fga, fg3a), ~ replace_na(.x, 0)))
sp_agg <- \(d) d |> summarise(mins = sum(mins), fouls = sum(fouls), fga = sum(fga), fg3a = sum(fg3a), .groups = "drop")
sp_prev <- sp_pg |> group_by(season, person_id) |> sp_agg()
# baseline: the player's previous two seasons pooled where both exist (one for the placebo season), 1000+ minutes,
# so a short injury season isn't treated as a player's level
sp_base <- \(s) { b <- c(prev_season(s), prev_season(prev_season(s))) |> intersect(KEEFE)
  sp_prev |> filter(season %in% b) |> group_by(person_id) |> summarise(p_mins = sum(mins), p_fouls = sum(fouls), p_fga = sum(fga), p_fg3a = sum(fg3a), .groups = "drop") |> filter(p_mins >= 1000) }
sp_within <- function(s, min_g = 1, max_g = n_games) {
  sp_pg |> filter(season == s, g >= min_g, g <= max_g) |> group_by(season, team_id, person_id) |> sp_agg() |> filter(mins >= 40) |>
    inner_join(sp_base(s), by = "person_id") |>
    mutate(d_foul36 = 36 * (fouls / mins - p_fouls / p_mins), d_three = fg3a / pmax(fga, 1) - p_fg3a / p_fga)
}
sp_team <- \(w) w |> group_by(season, team_id) |>
  summarise(players = n(), d_foul36 = weighted.mean(d_foul36, mins), d_three = weighted.mean(d_three, fga), .groups = "drop") |>
  mutate(d_foul36 = d_foul36 - mean(d_foul36), d_three = d_three - mean(d_three)
         , rank_foul_drop = rank(d_foul36), rank_three_rise = rank(-d_three), z_foul = d_foul36 / sd(d_foul36), z_three = d_three / sd(d_three))
sp_now <- sp_within(CUR); sp_plac <- sp_within(PREV)
same_player <- bind_rows(sp_team(sp_now), sp_team(sp_plac))
# is there any team signal at all? shuffle players across teams (no team effect) and compare the spread of team averages
sp_null <- function(w, col, wt, reps = 2000) {
  obs <- sd(sp_team(w)[[col]])
  sims <- replicate(reps, { w2 <- w |> mutate(team_id = sample(team_id)); sd(sp_team(w2)[[col]]) })
  tibble(measure = col, obs_sd = obs, null_sd = median(sims), p_spread = mean(sims >= obs)
         , true_sd = sqrt(max(obs^2 - median(sims)^2, 0)), shrink = max(obs^2 - median(sims)^2, 0) / obs^2)
}
sp_noise <- bind_rows(sp_null(sp_now, "d_foul36"), sp_null(sp_now, "d_three"))
same_player <- same_player |> mutate(foul_shrunk = d_foul36 * sp_noise$shrink[1], three_shrunk = d_three * sp_noise$shrink[2])
# stability on non-overlapping windows: games 1-N against games N+1 on (2025-26)
sp_full <- sp_team(sp_within(PREV, n_games + 1, Inf)) |> select(team_id, full_foul = d_foul36, full_three = d_three)
sp_rel <- sp_team(sp_plac) |> inner_join(sp_full, by = "team_id") |> summarise(r_foul = cor(d_foul36, full_foul), r_three = cor(d_three, full_three))
sp_players <- sp_now |> filter(team_id == WIZ_ID) |> left_join(box_league |> filter(season == CUR) |> distinct(person_id, name), by = "person_id") |>
  transmute(name, mins, foul36_prev = 36 * p_fouls / p_mins, foul36_now = 36 * fouls / mins, three_prev = p_fg3a / p_fga, three_now = fg3a / pmax(fga, 1), fga) |> arrange(desc(mins))
cat("\n== Same players, new habits? Within-player change vs own previous season, team average centred on the league ==\n")
pt(same_player |> filter(team_id == WIZ_ID), digits = 3); pt(sp_noise, digits = 3); pt(sp_rel, digits = 2); pt(sp_players, digits = 3)

# figure: every team's same-player change, this season and the placebo season, Wizards highlighted
sp_fig <- same_player |> select(season, team_id, `Fouls per 36 minutes` = d_foul36, `Share of shots from three` = d_three) |>
  pivot_longer(-c(season, team_id), names_to = "what", values_to = "v") |>
  mutate(v = if_else(what == "Share of shots from three", 100 * v, v), is_wiz = team_id == WIZ_ID
         , season = factor(if_else(season == CUR, paste0(CUR, "\n(first ", n_games, " games)"), paste0(PREV, "\n(first ", n_games, " games)"))
                           , c(paste0(PREV, "\n(first ", n_games, " games)"), paste0(CUR, "\n(first ", n_games, " games)"))))
p_sp <- ggplot(sp_fig, aes(v, season)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_jitter(data = \(d) d |> filter(!is_wiz), height = 0.12, width = 0, colour = "grey65", size = 2.4, alpha = 0.8) +
  geom_point(data = \(d) d |> filter(is_wiz), colour = wiz_blue, size = 5.5) +
  geom_text(data = \(d) d |> filter(is_wiz), label = "Wizards", colour = wiz_blue, family = font, size = 4.6, vjust = -1.3) +
  facet_wrap(~ what, ncol = 1, scales = "free_x") +
  labs(title = "Same players, different habits? Not yet"
       , subtitle = "Each dot is a team: how much its players changed from their own\nprevious two seasons (wherever they played them), relative to the\nleague. Fouls per 36 minutes, late-game fouls removed; threes in\npoints of shot share. At this sample the spread is no wider than\nshuffling players between teams at random produces."
       , x = "Change vs the same players last season\n(relative to the league)", y = NULL, caption = caption_text) +
  scale_x_continuous(expand = expansion(mult = 0.14)) +
  theme_wp(17) + theme(strip.text = element_text(face = "bold", hjust = 0, size = rel(1)), panel.spacing.y = unit(1.4, "lines"))
save_plot("same_player_habits.png", p_sp, width = 7.5, height = 10)

# roles behind the three-point shift: who the returning players share the floor with (five-man lineup minutes)
wiz_lu <- rd("panel_lineups") |> filter(season == CUR, team_id == WIZ_ID) |> mutate(pid = str_extract_all(group_id, "\\d+"))
pid_of <- \(nm) as.character(box_league$person_id[box_league$season == CUR & box_league$name == nm][1])
with_share <- \(who, mates) { d <- wiz_lu |> filter(map_lgl(pid, \(p) pid_of(who) %in% p)); sum(d$min[map_lgl(d$pid, \(p) any(map_chr(mates, pid_of) %in% p))]) / sum(d$min) }
conc15 <- rd("panel_lineups") |> filter(season == CUR) |> group_by(team_id) |> arrange(desc(min), .by_group = TRUE) |>
  summarise(top15 = sum(head(min, 15)) / sum(min), .groups = "drop") |> mutate(rank_conc = rank(-top15))
roles <- list(sarr_with_big = with_share("Alex Sarr", c("Anthony Davis", "Deandre Ayton")), bub_with_trae = with_share("Bub Carrington", "Trae Young")
              , top15_share = conc15$top15[conc15$team_id == WIZ_ID], top15_rank = conc15$rank_conc[conc15$team_id == WIZ_ID])
cat("\n== Roles: Sarr's minutes next to Davis or Ayton, Carrington's next to Young; top-15 lineup share and rank (1 = most concentrated) ==\n"); str(roles)

# --- 11. Who gets the minutes: the youngest rotation in the league? ----------------------------------
# share of player-minutes (five-man lineup table) to players 23 or younger, ages from the previous season's player
# table plus one; players missing from it (rookies) count as young
youth <- map_dfr(c(PREV, CUR), \(s) {
  ages <- rd(paste0("player_base_", prev_season(s))) |> transmute(pid = as.character(player_id), age = age + 1)
  rd("panel_lineups") |> filter(season == s) |> mutate(pid = str_extract_all(group_id, "\\d+")) |> select(team_id, pid, min) |> unnest(pid) |>
    left_join(ages, by = "pid") |> group_by(team_id) |>
    summarise(young_share = sum(min[coalesce(age, 21) <= 23]) / sum(min), unknown_share = sum(min[is.na(age)]) / sum(min), .groups = "drop") |>
    mutate(season = s, rank_youngest = rank(-young_share))
})
cat("\n== Share of minutes to players 23 and under (rank 1 = most) ==\n")
pt(youth |> filter(team_id == WIZ_ID), digits = 3)

# --- 12. The defense: results and what it gives up ------------------------------------------------
# a defense can stop fouling by gambling less, which shows up as fewer forced turnovers; across all full team-seasons
# the two go together, so check whether the Wizards paid that price
def_prof <- rd("panel_fourfactors") |> select(season, team_id, opp_fta_rate, opp_tov_pct, opp_efg_pct, opp_oreb_pct) |>
  left_join(rd("panel_advanced") |> select(season, team_id, def_rating), by = c("season", "team_id")) |>
  group_by(season) |> mutate(rk_fta = rank(opp_fta_rate), rk_tov = rank(-opp_tov_pct), rk_efg = rank(opp_efg_pct), rk_def = rank(def_rating)) |> ungroup()
foul_tov_r <- def_prof |> filter(season != CUR) |> group_by(season) |> mutate(across(c(opp_fta_rate, opp_tov_pct), ~ (.x - mean(.x)) / sd(.x))) |> ungroup() |>
  summarise(r = cor(opp_fta_rate, opp_tov_pct)) |> pull(r)
cat("\n== Wizards defense by season (rank 1 = best / fewest fouls / most turnovers forced); league r(fouling, forcing TOs) =", round(foul_tov_r, 2), "==\n")
pt(def_prof |> filter(team_id == WIZ_ID, season %in% KEEFE), digits = 3)

# --- 13. What the eleven games can and can't show ---------------------------------------------
# (a) Three-point luck. Opponents' three-point percentage is mostly outside a defense's control, and a team's own is
# noisy over eleven games; re-score each team's ratings as if both sides had shot the league's percentage on their threes.
three_tot <- \(d, pre) d |> transmute(season, team_id, a = .data[[paste0(pre, "fga_left_corner_3")]] + .data[[paste0(pre, "fga_right_corner_3")]] + .data[[paste0(pre, "fga_above_the_break_3")]]
                                      , m = .data[[paste0(pre, "fgm_left_corner_3")]] + .data[[paste0(pre, "fgm_right_corner_3")]] + .data[[paste0(pre, "fgm_above_the_break_3")]])
neutral <- rd("panel_advanced") |> filter(season %in% c(PREV, CUR)) |> select(season, team_id, off_rating, def_rating, net_rating, poss) |>
  left_join(three_tot(rd("panel_shotzones"), "") |> rename(own_a = a, own_m = m), by = c("season", "team_id")) |>
  left_join(three_tot(rd("panel_opploc"), "opp_") |> rename(opp_a = a, opp_m = m), by = c("season", "team_id")) |>
  group_by(season) |> mutate(lg3 = sum(own_m) / sum(own_a)) |> ungroup() |>
  mutate(own3 = own_m / own_a, opp3 = opp_m / opp_a
         , off_n = off_rating + 100 * 3 * (lg3 * own_a - own_m) / poss, def_n = def_rating + 100 * 3 * (lg3 * opp_a - opp_m) / poss, net_n = off_n - def_n) |>
  group_by(season) |> mutate(rk_off = rank(-off_rating), rk_off_n = rank(-off_n), rk_def = rank(def_rating), rk_def_n = rank(def_n)
                            , rk_net = rank(-net_rating), rk_net_n = rank(-net_n), rk_opp3 = rank(opp3), rk_own3 = rank(-own3)) |> ungroup()
cat("\n== Ratings with both teams' three-point shooting set to the league rate (rank 1 = best) ==\n")
pt(neutral |> filter(team_id == WIZ_ID) |> select(season, own3, opp3, lg3, off_rating, off_n, rk_off, rk_off_n, def_rating, def_n, rk_def, rk_def_n, net_n, rk_net, rk_net_n, rk_opp3), digits = 3)

# (b) Starting-lineup changes, three kinds: forced (a previous starter didn't play), a return (everyone played, but a
# new starter had missed the previous game), and chosen (everyone available both games, and the five changed anyway)
lc3 <- st5 |> group_by(season, team_id) |> arrange(g, .by_group = TRUE) |> mutate(prev_five = lag(five)) |> ungroup() |>
  filter(!map_lgl(prev_five, is.null)) |> left_join(played_ids, by = c("season", "team_id", "g")) |>
  left_join(played_ids |> mutate(g = g + 1) |> rename(prev_ids = ids), by = c("season", "team_id", "g")) |>
  mutate(changed = map2_lgl(five, prev_five, \(a, b) !setequal(a, b)), all_available = map2_lgl(prev_five, ids, \(p, i) all(p %in% i))
         , incoming_played = pmap_lgl(list(five, prev_five, prev_ids), \(f, p, pi) is.null(pi) || all(setdiff(f, p) %in% pi))
         , kind = case_when(!changed ~ "same", !all_available ~ "forced", !incoming_played ~ "return", TRUE ~ "chosen"))
lineup_kinds <- lc3 |> group_by(season, team_id) |> summarise(chosen = sum(kind == "chosen"), returns = sum(kind == "return"), forced = sum(kind == "forced")
                                                          , chosen_per_10 = 10 * mean(kind == "chosen"), forced_per_10 = 10 * mean(kind == "forced"), .groups = "drop") |>
  group_by(season) |> mutate(lg_chosen = mean(chosen_per_10), rank_chosen = rank(-chosen_per_10), rank_forced = rank(-forced_per_10)) |> ungroup()
cat("\n== Starting-lineup changes by kind (rank 1 = most) ==\n"); pt(lineup_kinds |> filter(team_id == WIZ_ID), digits = 3)
chosen_games <- lc3 |> filter(season == CUR, team_id == WIZ_ID, kind == "chosen") |> left_join(st5 |> filter(season == CUR, team_id == WIZ_ID) |> select(g), by = "g") |>
  left_join(box_league |> filter(season == CUR, team_id == WIZ_ID) |> distinct(g, game_id), by = "g") |> select(g, game_id)

# (c) Top-five minutes over the same first N games of each season (full tank seasons include late-season shutdowns)
top5_early <- box_league |> filter(mins > 0, g <= n_games) |> group_by(season, team_id, person_id) |> summarise(gp = n(), mpg = mean(mins), .groups = "drop") |>
  filter(gp >= 4) |> group_by(season, team_id) |> slice_max(mpg, n = 5, with_ties = FALSE) |> summarise(top5_mpg = mean(mpg), .groups = "drop") |>
  group_by(season) |> mutate(rank_top5 = rank(-top5_mpg)) |> ungroup()
cat("\n== Top-five minutes per game played, first", n_games, "games of each season (rank 1 = most) ==\n"); pt(top5_early |> filter(team_id == WIZ_ID), digits = 3)

# (d) Davis on and off: five-man lineup minutes and possession-weighted ratings, and who carries the minutes without him
pid <- \(nm) as.character(box_league$person_id[box_league$season == CUR & box_league$name == nm][1])
ad <- pid("Anthony Davis"); ty <- pid("Trae Young"); ay <- pid("Deandre Ayton"); sarr <- pid("Alex Sarr")
wl <- rd("panel_lineups") |> filter(season == CUR, team_id == WIZ_ID) |> mutate(p = str_extract_all(group_id, "\\d+")
      , has_ad = map_lgl(p, \(x) ad %in% x), has_ty = map_lgl(p, \(x) ty %in% x), has_ay = map_lgl(p, \(x) ay %in% x), has_sarr = map_lgl(p, \(x) sarr %in% x))
onoff <- \(d, lab) d |> summarise(group = lab, net = sum((off_rating - def_rating) * poss) / sum(poss), min = sum(min), poss = sum(poss))
davis_split <- bind_rows(onoff(wl |> filter(has_ad), "Davis on"), onoff(wl |> filter(!has_ad), "Davis off"),
                         onoff(wl |> filter(has_ty, !has_ad), "Young on, Davis off"), onoff(wl |> filter(has_ad, !has_ty), "Davis on, Young off"),
                         onoff(wl |> filter(has_ay, !has_ad), "Ayton without Davis"), onoff(wl |> filter(!has_ad, !has_ay), "Neither Davis nor Ayton"))
cat("\n== Davis on and off (five-man lineup minutes; net rating per 100 possessions) ==\n"); pt(davis_split, digits = 3)

# (e) play-by-play splits that need who was on the floor (Wizards stints from section 7)
p13 <- rd_games("pbp", CUR) |> filter(game_id %in% unique(stints$game_id)) |>
  mutate(clock_sec = as.numeric(str_match(clock, "PT(\\d+)M")[, 2]) * 60 + as.numeric(str_match(clock, "M([\\d.]+)S")[, 2])
         , elapsed = if_else(period <= 4, (period - 1) * 720, 2880 + (period - 5) * 300) + if_else(period <= 4, 720, 300) - clock_sec
         , late = period >= 4 & clock_sec <= 120)
on_at <- function(ev, who) { st <- stints |> filter(person_id == as.numeric(who)) |> select(game_id, start, end)
  ev |> mutate(.row = row_number()) |> left_join(st, by = "game_id", relationship = "many-to-many") |>
    group_by(.row) |> summarise(on = any(elapsed > start & elapsed <= end, na.rm = TRUE), .groups = "drop") |> arrange(.row) |> pull(on) }
opp_ev <- p13 |> filter(team_id != WIZ_ID, !late, (is_field_goal == 1) | action_type == "Free Throw")
opp_ev$ad_on <- on_at(opp_ev, ad)
davis_ft <- opp_ev |> group_by(ad_on) |> summarise(fga = sum(is_field_goal == 1, na.rm = TRUE), fta = sum(action_type == "Free Throw"), ft_rate = fta / fga)
cat("\n== Opponents' free-throw rate with Davis on / off (late-game removed) ==\n"); pt(davis_ft, digits = 3)
sarr_sh <- p13 |> filter(person_id == as.numeric(sarr), is_field_goal == 1)
sarr_sh$big <- on_at(sarr_sh, ad) | on_at(sarr_sh, ay)
bub <- pid("Bub Carrington"); bub_sh <- p13 |> filter(person_id == as.numeric(bub), is_field_goal == 1); bub_sh$ty_on <- on_at(bub_sh, ty)
shot_split <- bind_rows(sarr_sh |> group_by(with = if_else(big, "Sarr with Davis or Ayton", "Sarr as the only big")) |> summarise(fga = n(), three = mean(shot_value == 3)),
                        bub_sh |> group_by(with = if_else(ty_on, "Carrington with Young", "Carrington without Young")) |> summarise(fga = n(), three = mean(shot_value == 3)))
cat("\n== Three-point share of shots by who else is on the floor ==\n"); pt(shot_split, digits = 3)
# close finishes: games within 8 points with 5:00 left in the fourth; who played those last five minutes
home_of <- box_league |> filter(season == CUR, team_id == WIZ_ID) |> distinct(game_id, home)
margin5 <- p13 |> filter(elapsed <= 2580, !is.na(score_home), score_home != "") |> group_by(game_id) |> slice_max(elapsed, n = 1, with_ties = FALSE) |> ungroup() |>
  left_join(home_of, by = "game_id") |> mutate(margin = if_else(home, 1, -1) * (as.numeric(score_home) - as.numeric(score_away)))
close_ids <- margin5$game_id[abs(margin5$margin) <= 8]
close_finish <- stints |> filter(game_id %in% close_ids) |> mutate(s = pmax(start, 2580), e = pmin(end, 2880)) |> filter(e > s) |>
  group_by(person_id) |> summarise(close_min = sum(e - s) / 60, games = n_distinct(game_id), .groups = "drop") |>
  left_join(box_league |> filter(season == CUR) |> distinct(person_id, name), by = "person_id") |> arrange(desc(close_min))
cat("\n== Close finishes (within 8 with 5:00 left):", length(close_ids), "games; minutes in those last five minutes ==\n"); pt(close_finish |> head(8), digits = 3)

# (e2) the minutes without Davis, second by second: split his bench stints in games he played from the games he missed,
# and score each kind of non-Davis lineup by point margin per 48 minutes (play-by-play scores, Wizards' side)
ppl <- c(ad = ad, ay = ay, sarr = sarr, ty = ty)
grid <- p13 |> group_by(game_id) |> summarise(end = max(elapsed), .groups = "drop") |>
  mutate(t = map(end, \(e) seq_len(ceiling(e)))) |> select(game_id, t) |> unnest(t)
for (k in names(ppl)) {
  st <- stints |> filter(person_id == as.numeric(ppl[[k]])) |> select(game_id, start, end)
  on <- grid |> inner_join(st, by = "game_id", relationship = "many-to-many") |> filter(t > start, t <= end) |> distinct(game_id, t) |> mutate(on = TRUE)
  grid <- grid |> left_join(on, by = c("game_id", "t")) |> mutate(on = coalesce(on, FALSE)) |> rename(!!k := on)
}
ad_games <- stints |> filter(person_id == as.numeric(ad)) |> distinct(game_id) |> pull()
pts <- p13 |> filter(!is.na(score_home), score_home != "") |> left_join(home_of, by = "game_id") |>
  mutate(wiz = as.numeric(if_else(home, score_home, score_away)), opp = as.numeric(if_else(home, score_away, score_home))) |>
  group_by(game_id) |> arrange(elapsed, action_number, .by_group = TRUE) |>
  mutate(d = (wiz - lag(wiz, default = 0)) - (opp - lag(opp, default = 0)), t = pmax(ceiling(elapsed), 1)) |> ungroup() |>
  group_by(game_id, t) |> summarise(d = sum(d), .groups = "drop")
g2 <- grid |> left_join(pts, by = c("game_id", "t")) |> mutate(d = coalesce(d, 0), ad_played = game_id %in% ad_games)
per48 <- \(x, lab) x |> summarise(group = lab, min = n() / 60, margin48 = 48 * sum(d) / (n() / 60))
davis_off <- bind_rows(per48(g2 |> filter(ad), "Davis on"), per48(g2 |> filter(!ad, ad_played), "Davis on the bench, games he played"),
                       per48(g2 |> filter(!ad, !ad_played), "Games Davis missed"),
                       per48(g2 |> filter(!ad, ad_played, ty), "Young on, Davis on the bench"),
                       per48(g2 |> filter(!ad, ad_played, ay), "Ayton the only center (Davis on the bench)"),
                       per48(g2 |> filter(!ad, !ay, sarr), "Sarr the only big"))
cat("\n== Minutes without Davis, by kind (point margin per 48 minutes, from play-by-play) ==\n"); pt(davis_off, digits = 3)

# (f) pressure and the glass: deflections and opponents' offensive rebounds
hustle_rk <- rd("panel_hustle") |> filter(season %in% KEEFE) |> select(season, team_id, deflections) |> group_by(season) |> mutate(rk_defl = rank(-deflections)) |> ungroup() |>
  left_join(rd("panel_fourfactors") |> select(season, team_id, opp_oreb_pct) |> group_by(season) |> mutate(rk_oreb = rank(opp_oreb_pct)) |> ungroup(), by = c("season", "team_id")) |>
  filter(team_id == WIZ_ID)
cat("\n== Deflections per game and opponents' offensive rebound rate (rank 1 = most deflections / fewest offensive boards) ==\n"); pt(hustle_rk, digits = 3)

# (g) does an eleven-game net rating say whether a roster is over- or under-performing? fit net rating on roster strength
rs_net <- roster_strength |> left_join(rd("panel_advanced") |> select(season, team_id, net_rating), by = c("season", "team_id"))
rs_fit_now <- lm(net_rating ~ strength, data = rs_net |> filter(season == CUR))
rs_r <- rs_net |> group_by(season) |> summarise(r = cor(strength, net_rating))
wz <- rs_net |> filter(season == CUR, team_id == WIZ_ID)
rs_pi <- predict(rs_fit_now, newdata = wz, interval = "prediction", level = 0.8)
others <- rs_net |> filter(season == CUR, team_id != WIZ_ID) |> pull(net_rating)
rank_at <- \(v) 1 + sum(others > v)
roster_check <- tibble(strength_rank = wz$rank_best, pred_net = rs_pi[1, "fit"], lo = rs_pi[1, "lwr"], hi = rs_pi[1, "upr"], actual = wz$net_rating
                       , pred_rank = rank_at(rs_pi[1, "fit"]), rank_lo = rank_at(rs_pi[1, "upr"]), rank_hi = rank_at(rs_pi[1, "lwr"]), actual_rank = rank_at(wz$net_rating)
                       , r_now = rs_r$r[rs_r$season == CUR], r_prev = rs_r$r[rs_r$season == PREV])
cat("\n== Roster strength vs net rating: Wizards' predicted net (80% interval) and ranks ==\n"); pt(roster_check, digits = 3)

# --- 14. The fan complaints, tested: games they should have won, embarrassing losses, questionable lineups -------
# (a) expected wins: each game's win probability from both teams' net rating to date and home court, every team
gm <- box_league |> filter(season == CUR) |> group_by(game_id, team_id, team_tricode, home) |> summarise(pts = sum(points, na.rm = TRUE), .groups = "drop") |>
  group_by(game_id) |> mutate(opp_pts = sum(pts) - pts, opp_id = rev(team_id), opp = rev(team_tricode)) |> ungroup() |> mutate(margin = pts - opp_pts)
net_now <- rd("panel_advanced") |> filter(season == CUR) |> select(team_id, net_rating)
home_edge <- gm |> filter(home) |> summarise(h = mean(margin)) |> pull(h); sd_margin <- sd(gm$margin)
gm <- gm |> left_join(net_now, by = "team_id") |> left_join(net_now |> rename(opp_id = team_id, opp_net = net_rating), by = "opp_id") |>
  mutate(exp_margin = net_rating - opp_net + if_else(home, 1, -1) * home_edge, p_win = pnorm(exp_margin / sd_margin), won = margin > 0)
exp_wins <- gm |> group_by(team_id, team_tricode) |> summarise(gp = n(), w = sum(won), ew = sum(p_win), .groups = "drop") |>
  mutate(diff = w - ew, rank_below = rank(diff))
# (b) blown leads and blowouts, from play-by-play scores
sc <- rd_games("pbp", CUR) |> filter(!is.na(score_home), score_home != "") |> group_by(game_id) |>
  summarise(max_home = max(as.numeric(score_home) - as.numeric(score_away)), max_away = max(as.numeric(score_away) - as.numeric(score_home)), .groups = "drop")
gm <- gm |> left_join(sc, by = "game_id") |> mutate(max_lead = if_else(home, max_home, max_away))
ugly <- gm |> group_by(team_id, team_tricode) |> summarise(blown10 = sum(!won & max_lead >= 10), lost_by20 = sum(margin <= -20), worst = min(margin), .groups = "drop") |>
  mutate(rank_blown = rank(-blown10, ties.method = "min"), rank_blowouts = rank(-lost_by20, ties.method = "min"), lg_blown = mean(blown10), lg_lost20 = mean(lost_by20))
wiz_games <- gm |> filter(team_id == WIZ_ID) |> left_join(game_dates |> filter(season == CUR) |> select(game_id, team_id, game_date), by = c("game_id", "team_id")) |> arrange(game_date) |> mutate(g = row_number()) |>
  transmute(g, game_id, game_date, opp, home, pts, opp_pts, margin, won, opp_net, exp_margin, p_win, max_lead, upset_loss = !won & p_win > 0.5, upset_win = won & p_win < 0.5)
cat("\n== Fan complaint 1: games they should have won. Wizards game by game, with pre-game win probability ==\n"); pt(wiz_games, digits = 2)
cat("Expected wins", round(exp_wins$ew[exp_wins$team_id == WIZ_ID], 2), "actual", exp_wins$w[exp_wins$team_id == WIZ_ID], "rank below expectation (1 = most)", exp_wins$rank_below[exp_wins$team_id == WIZ_ID], "\n")
cat("\n== Fan complaint 2: embarrassing losses (blown 10+ leads, losses by 20+), Wizards and league ==\n"); pt(ugly |> filter(team_id == WIZ_ID), digits = 2)
# (c) questionable lineups: every Wizards five-man unit and pair with 10+ minutes, and where its net rating sits
lu_w <- rd("panel_lineups") |> filter(season == CUR, team_id == WIZ_ID) |> mutate(pid = str_extract_all(group_id, "\\d+"))
nm_tbl <- box_league |> filter(season == CUR, team_id == WIZ_ID) |> distinct(pid = as.character(person_id), name) |> mutate(last = word(name, -1))
lab <- \(p) paste(nm_tbl$last[match(p, nm_tbl$pid)], collapse = " / ")
fives <- lu_w |> mutate(unit = map_chr(pid, lab), has_ad = map_lgl(pid, \(x) ad %in% x), has_ay = map_lgl(pid, \(x) ay %in% x), has_ty = map_lgl(pid, \(x) ty %in% x)) |>
  select(unit, gp, min, poss, off_rating, def_rating, net_rating, has_ad, has_ay, has_ty) |> arrange(desc(min))
pairs <- rd(paste0("wizards_pairs_", CUR)) |> mutate(pid = str_extract_all(group_id, "\\d+"), pair = map_chr(pid, lab)) |> select(pair, gp, min, net_rating) |> arrange(desc(min))
cat("\n== Fan complaint 3: the lineups. Five-man units with 10+ minutes ==\n"); pt(fives |> filter(min >= 10), digits = 3)
cat("\n== Pairs with 100+ minutes ==\n"); pt(pairs |> filter(min >= 100), digits = 3)
young_ayton <- pairs |> filter(pair %in% c("Young / Ayton", "Ayton / Young"))
# minutes Young has played with Ayton but not Davis, from the second-by-second grid
ya <- per48(g2 |> filter(ty, ay, !ad, ad_played), "Young and Ayton, Davis on the bench")
ya_all <- per48(g2 |> filter(ty, ay, !ad), "Young and Ayton without Davis, all games")
# the record split by whether Davis played: where the season's point margin actually comes from
davis_games <- wiz_games |> mutate(davis = game_id %in% ad_games)
davis_record <- davis_games |> group_by(davis) |> summarise(games = n(), w = sum(won), l = sum(!won), per_game = mean(margin), exp_margin = sum(exp_margin), margin = sum(margin), ew = sum(p_win), .groups = "drop")
# inside the games he played: margin by who was on the floor
margin_parts <- g2 |> filter(ad_played) |> mutate(part = case_when(ad & ty ~ "Davis and Young", ad & !ty ~ "Davis without Young", !ad & ty & ay ~ "Young with Ayton, no Davis", !ad & ty ~ "Young without Davis or Ayton", TRUE ~ "Neither Davis nor Young")) |>
  group_by(part) |> summarise(min = n() / 60, margin = sum(d), per48 = 48 * sum(d) / (n() / 60), .groups = "drop")
# against teams better and worse than the Wizards (net rating to date), with the chance of the 0-for record by luck alone
wiz_net_now <- net_now$net_rating[net_now$team_id == WIZ_ID]
sched_split <- wiz_games |> mutate(better = opp_net > wiz_net_now) |> group_by(better) |>
  summarise(games = n(), w = sum(won), l = sum(!won), ew = sum(p_win), margin = sum(margin), exp_margin = sum(exp_margin)
            , p_this_few = { pr <- c(1, rep(0, n())); for (pp in p_win) pr <- c(pr * (1 - pp), 0) + c(0, pr * pp); sum(pr[seq_len(sum(won) + 1)]) }
            , opps = paste(unique(opp), collapse = ", "), .groups = "drop")
# league rank of each opponent, for the prose
opp_ranks <- net_now |> mutate(rank_net = rank(-net_rating)) |> left_join(gm |> distinct(team_id, team_tricode), by = "team_id")
# the games Davis missed: who started at center, who played, against what expectation
missed_detail <- davis_games |> filter(!davis) |> select(game_id, game_date, opp, home, margin, exp_margin, pts) |>
  left_join(box_league |> filter(season == CUR, team_id == WIZ_ID, name == "Deandre Ayton") |> transmute(game_id, ayton_started = starter, ayton_min = mins), by = "game_id")
missed_starters <- box_league |> filter(season == CUR, team_id == WIZ_ID, starter, game_id %in% missed_detail$game_id) |> group_by(game_id) |> summarise(starters = paste(word(name, -1), collapse = ", "), .groups = "drop")
missed_detail <- missed_detail |> left_join(missed_starters, by = "game_id") |> mutate(vs_expected = margin - exp_margin)
# noise on pair net ratings: per 100 possessions, with possessions ~ minutes x 100 / 48 and about 1.05 points^2 per possession per side
pairs <- pairs |> mutate(poss = min * 100 / 48, se100 = 100 * sqrt(2 * 1.05 / poss))
cat("\n== Against better and worse teams ==\n"); pt(sched_split, digits = 3); pt(missed_detail, digits = 3)
cat("\n== Record and margin by whether Davis played ==\n"); pt(davis_record, digits = 3); pt(margin_parts, digits = 3); pt(bind_rows(ya, ya_all), digits = 3)
cat("\n== Young with Ayton and without Davis ==\n"); pt(ya, digits = 3)
fan_tests <- list(games = wiz_games, exp_wins = exp_wins |> filter(team_id == WIZ_ID), ugly = ugly |> filter(team_id == WIZ_ID), lg_blown = ugly$lg_blown[1], lg_lost20 = ugly$lg_lost20[1]
                  , fives = fives, pairs = pairs, young_ayton = young_ayton, ya48 = ya, ya48_all = ya_all, home_edge = home_edge, davis_record = davis_record, margin_parts = margin_parts, sched_split = sched_split, missed_detail = missed_detail, opp_ranks = opp_ranks)

# figure: each game as an arrow from its expected margin to its actual margin, games ordered from the weakest opponent to
# the strongest, split where opponents become better than the Wizards
wiz_net_fig <- net_now$net_rating[net_now$team_id == WIZ_ID]
gsw <- wiz_games |> mutate(lab = paste0(if_else(home, "vs ", "@ "), opp, "\n", format(game_date, "%b %e")), lab = gsub("  ", " ", lab)
                           , better_opp = opp_net > wiz_net_fig, beat = margin > exp_margin) |>
  arrange(game_date) |> mutate(x = row_number())
grp_lab <- gsw |> group_by(better_opp) |> summarise(x = mean(x), w = sum(won), l = sum(!won), m = sum(margin), em = sum(exp_margin), .groups = "drop") |>
  mutate(txt = paste0(if_else(better_opp, "Opponents ranked above the Wizards", "Opponents ranked below the Wizards"), "\n", w, "-", l,
                      ", point margin ", sprintf("%+d", as.integer(round(m))), " (expected ", sprintf("%+d", as.integer(round(em))), ")"))
ytop <- max(c(gsw$margin, gsw$exp_margin)) + 6
p_games <- ggplot(gsw, aes(x = x)) +
  geom_rect(data = \(d) d |> filter(better_opp), aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf), fill = cream, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.5) +
  annotate("text", x = nrow(gsw) + 0.55, y = 1.5, label = "Win", hjust = 1, vjust = 0, size = 3.4, family = font, colour = "grey40") +
  annotate("text", x = nrow(gsw) + 0.55, y = -1.5, label = "Loss", hjust = 1, vjust = 1, size = 3.4, family = font, colour = "grey40") +
  geom_segment(data = \(d) d |> filter(abs(margin - exp_margin) >= 3), aes(xend = x, y = exp_margin, yend = margin, colour = beat), linewidth = 1.3, arrow = arrow(length = unit(0.12, "inches"), type = "closed")) +
  geom_point(data = \(d) d |> filter(abs(margin - exp_margin) < 3), aes(y = margin, colour = beat), size = 2.6) +
  geom_point(aes(y = exp_margin), shape = 21, fill = "white", colour = "grey20", size = 3.6, stroke = 1) +
  geom_text(aes(x = x + 0.17, y = margin, label = sprintf("%+d", as.integer(margin)), colour = beat), hjust = 0, size = 3.4, family = font, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = teal, `FALSE` = wiz_red), guide = "none") +
  scale_x_continuous(breaks = gsw$x, labels = gsw$lab, expand = expansion(add = c(0.4, 0.6))) +
  scale_y_continuous(limits = c(min(gsw$margin) - 8, ytop + 6), breaks = seq(-40, 30, 10)) +
  labs(title = "Games they should have won?", subtitle = paste0("Every game in order. The open circle is the margin the matchup predicted (both teams' net rating\nto date, plus home court); the arrow runs to the real margin, navy when the Wizards did better than predicted\nand red when they did worse. Above the zero line is a win. Shaded games are against teams ranked above\nthe Wizards. "
                                                         , paste0(c("Against teams ranked below: ", "Against teams ranked above: "), grp_lab$w[order(grp_lab$better_opp)], "-", grp_lab$l[order(grp_lab$better_opp)], ", point margin ", sprintf("%+d", as.integer(round(grp_lab$m[order(grp_lab$better_opp)]))), " (expected ", sprintf("%+d", as.integer(round(grp_lab$em[order(grp_lab$better_opp)]))), ")", collapse = ".\n"), ".")
       , x = NULL, y = "Point margin", caption = caption_text) +
  theme_wp(13) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = rel(0.85), lineheight = 0.9))
save_plot("games_should_have_won.png", p_games, width = 11, height = 7)

# figure: who plays with whom. Minutes each pair of the nine most-used players has shared, from the two-man lineup table.
# Usage is Keefe's decision and isn't noisy; results for pairs at this sample are, so they are left off the chart.
top9 <- box_league |> filter(season == CUR, team_id == WIZ_ID) |> group_by(person_id, name) |> summarise(min = sum(mins), .groups = "drop") |>
  slice_max(min, n = 9) |> mutate(last = word(name, -1), pid = as.character(person_id))
pair_min <- rd(paste0("wizards_pairs_", CUR)) |> mutate(pid = str_extract_all(group_id, "\\d+")) |> select(pid, min) |>
  mutate(a = map_chr(pid, 1), b = map_chr(pid, 2)) |> filter(a %in% top9$pid, b %in% top9$pid)
ord <- top9$last[order(-top9$min)]
pm_full <- pair_min |> mutate(a = top9$last[match(a, top9$pid)], b = top9$last[match(b, top9$pid)]
                             , ra = match(a, ord), rb = match(b, ord), x = if_else(ra < rb, a, b), y = if_else(ra < rb, b, a)) |>
  transmute(a = factor(x, ord), b = factor(y, rev(ord)), min)
stopifnot(nrow(pm_full) == nrow(pair_min), !any(is.na(pm_full$a)), !any(is.na(pm_full$b)))
team_min <- sum(rd("panel_lineups") |> filter(season == CUR, team_id == WIZ_ID) |> pull(min))
p_pairs <- ggplot(pm_full, aes(a, b, fill = min)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = round(min), colour = min > 160), size = 3.6, family = font) +
  scale_fill_gradient(low = cream, high = wiz_blue, name = "Minutes\ntogether") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(title = "Who plays with whom", subtitle = paste0("Minutes each pair of the Wizards' nine most-used players has spent on the floor together, all ", n_games, " games\n(", round(team_min), " team minutes). Players run from most minutes to fewest. These are Keefe's choices; how each pair has\nscored over so few minutes is mostly noise, so the results are left off."), x = NULL, y = NULL, caption = caption_text) +
  theme_wp(13) + theme(panel.grid = element_blank(), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), legend.position = "right")
save_plot("pairs_net.png", p_pairs, width = 11, height = 8)

# figure: the five-man lineups Keefe actually uses. Ten most-used units by minutes, and how much of the team's time
# the top ten cover compared with every other team (usage only; five-man results over so few minutes are noise)
lu_all <- rd("panel_lineups") |> filter(season == CUR)
top10_share <- lu_all |> group_by(team_id) |> arrange(desc(min), .by_group = TRUE) |>
  summarise(lineups = n(), top1 = first(min) / sum(min), top10 = sum(head(min, 10)) / sum(min), .groups = "drop") |>
  mutate(rank_top10 = rank(-top10), rank_lineups = rank(-lineups))
five <- lu_all |> filter(team_id == WIZ_ID) |> arrange(desc(min)) |> head(10) |>
  mutate(pid = str_extract_all(group_id, "\\d+"), names = map_chr(pid, \(x) paste(sort(nm_tbl$last[match(x, nm_tbl$pid)]), collapse = ", ")),
         has_ay = map_lgl(pid, \(x) ay %in% x), has_ad = map_lgl(pid, \(x) ad %in% x), share = min / sum(lu_all$min[lu_all$team_id == WIZ_ID]),
         kind = case_when(has_ad & !has_ay ~ "Davis at center", has_ay & !has_ad ~ "Ayton at center", has_ad & has_ay ~ "Davis and Ayton", TRUE ~ "Sarr or no center"),
         names = fct_reorder(names, min))
wz10 <- top10_share |> filter(team_id == WIZ_ID)
kind_cols <- c(`Davis at center` = wiz_blue, `Ayton at center` = wiz_red, `Davis and Ayton` = "grey60", `Sarr or no center` = gold)
p_five <- ggplot(five, aes(min, names, fill = kind)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(min), " min, ", gp, if_else(gp == 1, " game", " games"))), hjust = -0.08, size = 3.3, family = font, colour = "grey25") +
  scale_fill_manual(values = kind_cols, name = NULL, breaks = names(kind_cols)) +
  scale_x_continuous(limits = c(0, max(five$min) * 1.35), expand = c(0, 0)) +
  labs(title = "The five-man lineups Keefe uses", subtitle = paste0("The Wizards' ten most-used lineups, ", n_games, " games. Together they cover ", percent(wz10$top10, 1), " of the team's minutes, ", ordinal(round(wz10$rank_top10)), " of 30;\n",
                                                                   "the Wizards have used ", wz10$lineups, " different lineups (", ordinal(round(wz10$rank_lineups)), "-most). Colour shows who plays center.\nResults are left off: at a few dozen minutes each, a lineup's scoring margin is mostly noise.")
       , x = "Minutes together", y = NULL, caption = caption_text) +
  theme_wp(13) + theme(legend.position = "top", panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "black"))
save_plot("five_man_lineups.png", p_five, width = 11, height = 7)

# lineup types: pool lineups by who plays center and by how many of the two stars are on the floor, so each type has hundreds
# of possessions; score each as played and with both sides' threes reset to the league rate (the same adjustment as the team table)
lu_adv <- rd("panel_lineups") |> filter(season == CUR, team_id == WIZ_ID) |> select(group_id, min, poss, off_rating, def_rating)
lu_b <- rd(paste0("wizards_lineups_base_", CUR)) |> select(group_id, fg3a, fg3m)
lu_o <- rd(paste0("wizards_lineups_opponent_", CUR)) |> select(group_id, opp_fg3a, opp_fg3m)
lg3_now <- neutral$lg3[neutral$season == CUR][1]
lu_t <- lu_adv |> left_join(lu_b, by = "group_id") |> left_join(lu_o, by = "group_id") |>
  mutate(pid = str_extract_all(group_id, "\\d+"), has_ad = map_lgl(pid, \(x) ad %in% x), has_ay = map_lgl(pid, \(x) ay %in% x),
         has_sarr = map_lgl(pid, \(x) sarr %in% x), has_ty = map_lgl(pid, \(x) ty %in% x),
         center = case_when(has_ad & has_ay ~ "Davis and Ayton together", has_ad ~ "Davis at center", has_ay ~ "Ayton at center", has_sarr ~ "Sarr as the only big", TRUE ~ "No true center"),
         stars = case_when(has_ad & has_ty ~ "Davis and Young", has_ad ~ "Davis, no Young", has_ty ~ "Young, no Davis", TRUE ~ "Neither"),
         pts = (off_rating - def_rating) * poss / 100, luck = 3 * ((fg3m - lg3_now * fg3a) - (opp_fg3m - lg3_now * opp_fg3a)))
stopifnot(!any(is.na(lu_t$fg3a)), !any(is.na(lu_t$opp_fg3a)))
type_sum <- \(d, grp, lab) d |> group_by(type = .data[[grp]]) |>
  summarise(lineups = n(), min = sum(min), poss = sum(poss), net = 100 * sum(pts) / sum(poss), net_n = 100 * (sum(pts) - sum(luck)) / sum(poss),
            opp3 = sum(opp_fg3m) / sum(opp_fg3a), own3 = sum(fg3m) / sum(fg3a), .groups = "drop") |>
  mutate(grouping = lab, se = 100 * sqrt(2 * 1.05 / poss), share = min / sum(min))
lineup_types <- bind_rows(type_sum(lu_t, "center", "Who plays center"), type_sum(lu_t, "stars", "Stars on the floor"))
cat("\n== Lineup types: pooled net rating per 100, as played and three-point-neutral, with noise SE ==\n"); pt(lineup_types, digits = 3)
p_types <- lineup_types |> filter(min >= 30) |>
  mutate(type_lab = paste0(type, "\n", round(min), " min"), type_lab = fct_reorder(type_lab, net), grouping = factor(grouping, c("Stars on the floor", "Who plays center"))) |>
  ggplot(aes(y = type_lab)) +
  # where the true figure could plausibly be: 40 equally likely values from the noise around each type's net rating (quantile dots)
  ggdist::stat_dots(aes(xdist = distributional::dist_normal(net, se), fill = after_stat(x < 0), colour = after_stat(x < 0)), quantiles = 40,
                    side = "both", layout = "swarm", overflow = "compress", alpha = 0.28, stroke = 0) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
  geom_point(aes(x = net), colour = "white", size = 7) +
  geom_point(aes(x = net), colour = "grey10", size = 5.4) +
  geom_text(aes(x = 36, label = sprintf("%+.1f", net), colour = net < 0), hjust = 1, size = 4, family = font, fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = wiz_red, `FALSE` = teal), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = wiz_red, `FALSE` = teal), guide = "none") +
  facet_wrap(~ grouping, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(-40, 30, 10)) + coord_cartesian(xlim = c(-42, 36), clip = "off") +
  labs(title = "Which kinds of lineups are working?", subtitle = paste0("Every Wizards lineup pooled by type, all ", n_games, " games (including the two Davis missed). Black dot: net rating per\n100 possessions as played. The 40 small dots show where the true figure could plausibly be, given how few possessions\neach type has played; each is equally likely. Red falls below zero, navy above. Nearly all one colour means a real gap."),
       x = "Net rating per 100 possessions", y = NULL, caption = caption_text) +
  theme_wp(13) + theme(strip.text = element_text(face = "bold", hjust = 0, size = rel(1.05)), panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "black", lineheight = 0.95))
save_plot("lineup_types.png", p_types, width = 11, height = 8)
cat("\n== Five-man lineups: Wizards top ten and league comparison ==\n"); pt(five |> select(names, gp, min, share, kind), digits = 3); pt(wz10, digits = 3)

# --- 15. Player development: do Keefe's players beat their own expectations? -------------------------------------
# After Faberman (2025, UNC Sports Analytics Intelligence Lab), who ranks coaches by whether their players beat an
# age-adjusted expectation of their own prior-season production. Here: Game Score per 36 minutes this season, against
# what a league-wide fit on the player's prior production (his previous two seasons pooled where both exist) and his age
# predicts. The minutes-weighted average residual of a team's players is its coach's development score. Rookies have no
# prior season and are left out, so Dybantsa does not count here. Players need 500+ prior minutes.
gs36 <- \(d) d |> mutate(gmsc = pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) + 0.7 * oreb + 0.3 * dreb + stl + 0.7 * ast + 0.7 * blk - 0.4 * pf - tov, gs36 = 36 * gmsc / min)
pb <- \(stem) rd(stem) |> select(player_id, player_name, team_id, age, min, pts, fgm, fga, fta, ftm, oreb, dreb, stl, ast, blk, pf, tov)
pool_prior <- \(stems) map_dfr(stems, pb) |> group_by(player_id) |> summarise(across(c(min, pts, fgm, fga, fta, ftm, oreb, dreb, stl, ast, blk, pf, tov), sum), .groups = "drop") |> gs36() |> filter(min >= 500) |> select(player_id, p_min = min, p_gs36 = gs36)
dev_score <- function(now, prior, min_now, label) {
  d <- now |> gs36() |> filter(min >= min_now) |> inner_join(prior, by = "player_id") |> mutate(age = pmin(pmax(age, 19), 36))
  fit <- lm(gs36 ~ p_gs36 + splines::ns(age, 3), data = d)
  d <- d |> mutate(expected = fitted(fit), over = gs36 - expected)
  team <- d |> group_by(team_id) |> summarise(players = n(), dev = weighted.mean(over, min), .groups = "drop") |> mutate(rank_dev = rank(-dev), label = label, r_fit = cor(d$gs36, d$expected))
  list(players = d, team = team)
}
dev_prev <- dev_score(pb(paste0("player_base_", PREV)), pool_prior(paste0("player_base_", KEEFE[1])), 500, paste0(PREV, " (full season)"))
dev_now  <- dev_score(pb(paste0("player_base_", CUR)), pool_prior(paste0("player_base_", KEEFE[1:2])), 150, paste0(CUR, " (first ", n_games, " games)"))
dev_plac <- dev_score(pb(paste0("player_base_early_", PREV)), pool_prior(paste0("player_base_", KEEFE[1])), 150, paste0(PREV, " (first ", n_games, " games)"))
# games 12 on of 2025-26 = full season minus the early cut, for a non-overlapping reliability check
rest_prev <- pb(paste0("player_base_", PREV)) |> inner_join(pb(paste0("player_base_early_", PREV)) |> select(player_id, team_id, min, pts, fgm, fga, fta, ftm, oreb, dreb, stl, ast, blk, pf, tov) |> rename_with(~ paste0("e_", .x), -c(player_id, team_id)), by = c("player_id", "team_id")) |>
  mutate(across(c(min, pts, fgm, fga, fta, ftm, oreb, dreb, stl, ast, blk, pf, tov), ~ .x - get(paste0("e_", cur_column())))) |> select(-starts_with("e_"))
dev_rest <- dev_score(rest_prev, pool_prior(paste0("player_base_", KEEFE[1])), 350, paste0(PREV, " (games ", n_games + 1, " on)"))
dev_rel <- dev_plac$team |> inner_join(dev_rest$team |> select(team_id, dev_rest = dev), by = "team_id") |> summarise(r = cor(dev, dev_rest)) |> pull(r)
# shuffle null for the eleven-game score: reassign players to teams at random
dev_null <- { d <- dev_now$players; obs <- sd(dev_now$team$dev)
  sims <- replicate(2000, { d2 <- d |> mutate(team_id = sample(team_id)); sd(d2 |> group_by(team_id) |> summarise(v = weighted.mean(over, min)) |> pull(v)) })
  tibble(obs_sd = obs, null_sd = median(sims), p_spread = mean(sims >= obs), shrink = max(obs^2 - median(sims)^2, 0) / obs^2) }
development <- bind_rows(dev_prev$team, dev_plac$team, dev_now$team)
dev_wiz_players <- dev_now$players |> filter(team_id == WIZ_ID) |> transmute(name = player_name, age, min, p_gs36, gs36, expected, over) |> arrange(desc(min))
cat("\n== Player development: team average of players' Game Score/36 over an age-adjusted expectation (rank 1 = best) ==\n")
pt(development |> filter(team_id == WIZ_ID), digits = 3); pt(dev_null, digits = 3); cat("reliability of the", n_games, "-game score vs the rest of", PREV, ": r =", round(dev_rel, 2), "\n"); pt(dev_wiz_players, digits = 2)
p_dev <- development |> mutate(is_wiz = team_id == WIZ_ID, label = factor(label, unique(label))) |>
  ggplot(aes(dev, label)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  ggdist::stat_interval(data = \(d) d |> filter(!is_wiz), .width = c(0.5, 0.8), linewidth = 9, alpha = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = c(cream, "#DCC7BF")) +
  ggbeeswarm::geom_quasirandom(data = \(d) d |> filter(!is_wiz), orientation = "y", width = 0.25, colour = "grey45", size = 2.2, alpha = 0.85) +
  geom_point(data = \(d) d |> filter(is_wiz), colour = wiz_blue, size = 5.5) +
  geom_label(data = \(d) d |> filter(is_wiz), aes(label = paste0("Wizards, ", ordinal(round(rank_dev)))), colour = wiz_blue, family = font, size = 4.2, vjust = -0.7, fontface = "bold", fill = alpha("white", 0.85), label.size = 0, label.padding = unit(0.15, "lines")) +
  labs(title = "Do his players beat expectations?", subtitle = "Each dot is a team: how far its players' production (Game\nScore per 36 minutes) sits above or below what their own\nprevious seasons and ages predict, averaged by minutes.\nRookies are not included."
       , x = "Game Score per 36 above expectation (team average)", y = NULL, caption = caption_text) +
  scale_x_continuous(expand = expansion(mult = 0.15)) + theme_wp(15)
save_plot("player_development.png", p_dev, width = 7.5, height = 7)

# --- numbers for the write-up ----------------------------------------------------------
# coaching_post.qmd reads this file for every Keefe-vs-Keefe number, so the prose cannot drift from the data
evo <- list(
  through_day = through_day, n_games = n_games, n_early = N_EARLY, keefe = KEEFE
  , early_seasons = sort(unique(calib$season[calib$area != "Decisions"]))
  , wiz = wiz |> select(dial, area, long, petal, dir, value_first = paste0("value_", first_k), value_prev = paste0("value_", prev_k), value_now = paste0("value_", cur_k)
                        , out_first, out_prev, out_now, out_lo, out_hi, out_mid = out_final_mid, churn80, r, now_verdict, toward, counted, cluster)
  , trust = trust, churn = churn, coverage = coverage
  , baseline = baseline, kind_gap = kind_gap, keefe_flags = keefe_flags, keefe_dials = keefe_dials, keefe_distinct = keefe_distinct
  , keefe_pctile = spec$pctile, keefe_pctile_new = mean(baseline$share[baseline$kind == "New coach"] >= keefe_share)
  , common = common, keefe_expected = keefe_expected, keefe_ome_pctile = keefe_ome_pctile, placebo = placebo, sens = sens
  , continuity = continuity, newcomer_lineups = newcomer_lineups, wiz_top = wiz_top, sched = sched
  , opp_ft = opp_ft |> filter(defender == WIZ_ID), closers = closers, rotation_choice = rotation_choice
  , roster_fit = roster_fit, wiz_resid = wiz_resid, wiz_raw = wiz_raw, roster_contrib = roster_contrib, habit_gap = habit_gap, foul_sched = foul_sched, roster_strength = roster_strength |> filter(team_id == WIZ_ID), own_wiz = own_wiz, players_used = players_used |> filter(team_id == WIZ_ID)
  , coach_sens = summary_tbl |> select(metric, label, group, ratio, lo, hi, r_travel)
  , roles = roles, fan_tests = fan_tests, lineup_types = lineup_types, five = five |> select(names, gp, min, share, kind, has_ad, has_ay), top10_share = wz10, top10_lg = top10_share, development = development |> filter(team_id == WIZ_ID), dev_null = dev_null, dev_rel = dev_rel, dev_wiz_players = dev_wiz_players, neutral = neutral |> filter(team_id == WIZ_ID), lineup_kinds = lineup_kinds |> filter(team_id == WIZ_ID), chosen_games = chosen_games, lineup_kinds_lg = lineup_kinds |> group_by(season) |> summarise(lg_chosen = first(lg_chosen))
  , top5_early = top5_early |> filter(team_id == WIZ_ID), davis_split = davis_split, davis_off = davis_off, davis_ft = davis_ft, shot_split = shot_split
  , close_finish = close_finish, n_close = length(close_ids), hustle = hustle_rk, roster_check = roster_check
  , appx_b = appx_b, start_sum = start_sum |> filter(team_id == WIZ_ID) |> select(season, games, distinct_fives, changes)
  , same_player = same_player |> filter(team_id == WIZ_ID), same_player_all = same_player, same_player_rel = sp_rel, same_player_wiz = sp_players, same_player_noise = sp_noise
  , youth = youth |> filter(team_id == WIZ_ID), def_prof = def_prof |> filter(team_id == WIZ_ID, season %in% KEEFE), foul_tov_r = foul_tov_r
  , records = rd("panel_advanced") |> filter(team_id == WIZ_ID, season %in% KEEFE) |> select(season, gp, w, l)
  , rating_ranks = rd("panel_advanced") |> group_by(season) |>
      mutate(off_rk = rank(-off_rating), def_rk = rank(def_rating), net_rk = rank(-net_rating)) |>   # 1 = best
      ungroup() |> filter(team_id == WIZ_ID) |> select(season, off_rating, off_rk, def_rating, def_rk, net_rating, net_rk)
)
saveRDS(evo, file.path(DATA_DIR, "keefe_evolution.rds"))
cat("\nKeefe-vs-Keefe figures written; numbers saved to", file.path(DATA_DIR, "keefe_evolution.rds"), "\n")

cat("\nAll done. Figures are in possession_figs/.\n")
