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
# Nothing in the data folder is meant to be committed. Figures go to
# ./possession_figs.
#
# Sections: 0 data  1 possession length & shot timing  2 shot quality vs
# shot making  3 the coaching-change panel (offense)  4 defense & rotation
# 5 does he move minutes on last night's box score  6 timeouts, after-timeout
# plays, challenges  7 data art (tapestry, timeout clock, constellation)
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

pal <- c(red = "#D9535B", teal = "#3A9AB2", teal_dark = "#2A7F96", navy = "#1B2A6B", grey = "#4D4D4D", gold = "#E1AF00", ink = "#0B1230")
wiz_blue <- pal[["navy"]]; wiz_red <- pal[["red"]]; teal <- pal[["teal"]]; teal_dark <- pal[["teal_dark"]]
gold <- pal[["gold"]]; ink <- pal[["ink"]]; grey_text <- pal[["grey"]]
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
  ggplot2::ggsave(file.path("possession_figs", name), p, width = width, height = height, dpi = 300, device = ragg::agg_png, bg = bg)
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


def pt_stats(season, measure):
    return fetch("leaguedashptstats", dict(
        College="", Conference="", Country="", DateFrom="", DateTo="", Division="", DraftPick="", DraftYear="", GameScope="",
        Height="", LastNGames=0, LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PerMode="PerGame",
        PlayerExperience="", PlayerOrTeam="Team", PlayerPosition="", PtMeasureType=measure, Season=season, SeasonSegment="",
        SeasonType=SEASON_TYPE, StarterBench="", TeamID=0, VsConference="", VsDivision="", Weight=""))


def clock_shots(season, endpoint):
    frames = []
    for rng in CLOCK_RANGES:
        d = fetch(endpoint, dict(
            CloseDefDistRange="", DateFrom="", DateTo="", DribbleRange="", GameSegment="", GeneralRange="Overall", LastNGames=0,
            LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N", PerMode="PerGame",
            Period=0, PlusMinus="N", Rank="N", Season=season, SeasonSegment="", SeasonType=SEASON_TYPE, ShotClockRange=rng,
            ShotDistRange="", TeamID=0, TouchTimeRange="", VsConference="", VsDivision=""))
        d["SHOT_CLOCK_RANGE"] = rng
        frames.append(d)
    return pd.concat(frames, ignore_index=True)


def shot_locations(season, measure="Base"):
    data = get_json("leaguedashteamshotlocations", dict(
        Conference="", DateFrom="", DateTo="", DistanceRange="By Zone", Division="", GameScope="", GameSegment="", LastNGames=0,
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


def hustle(season):
    return fetch("leaguehustlestatsteam", dict(
        College="", Conference="", Country="", DateFrom="", DateTo="", Division="", DraftPick="", DraftYear="", GameScope="",
        Height="", LastNGames=0, LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0, PaceAdjust="N",
        PerMode="PerGame", PlayerExperience="", PlayerPosition="", Season=season, SeasonSegment="", SeasonType=SEASON_TYPE,
        TeamID=0, VsConference="", VsDivision="", Weight=""))


def lineups(season, team_id, n=5):
    return fetch("leaguedashlineups", dict(
        Conference="", DateFrom="", DateTo="", Division="", GameSegment="", GroupQuantity=n, LastNGames=0, LeagueID="00",
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

    # 4. play-by-play and box scores, appended game by game (always resumes; picks up new games)
    plan = {KEEFE_SEASONS[0]: "wizards", PREV: "all", CUR: "all"}
    for season, scope in plan.items():
        if season == CUR or not have(f"pbp_{season}.csv"):
            append_by_game(f"pbp_{season}.csv", season, scope, pbp_game)
    plan = {KEEFE_SEASONS[0]: "wizards", PREV: "wizards", CUR: "all"}
    for season, scope in plan.items():
        if season == CUR or not have(f"boxscores_{season}.csv"):
            append_by_game(f"boxscores_{season}.csv", season, scope, box_game)
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
needed <- c("panel_advanced.csv", "panel_lineups.csv", "panel_coaches.csv", sprintf("league_shots_%s.csv", CUR), sprintf("pbp_%s.csv", CUR), sprintf("boxscores_%s.csv", CUR))
if (REFRESH || !all(file.exists(file.path(DATA_DIR, needed)))) run_fetch() else message("Using cached data in ", DATA_DIR, " (set REFRESH=1 to pull the latest games).")

# readers ------------------------------------------------------------------
rd <- function(stem) read_csv(file.path(DATA_DIR, paste0(stem, ".csv")), show_col_types = FALSE, guess_max = 5000) |> clean_names()
rd_season <- function(table, season) rd(paste0("panel_", table)) |> filter(season == !!season)
rd_games <- function(stem, season) read_csv(file.path(DATA_DIR, sprintf("%s_%s.csv", stem, season)), show_col_types = FALSE
                                            , col_types = cols(gameId = col_character(), .default = col_guess()), guess_max = 5000) |> clean_names()
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
p_cch <- rd("panel_coaches") |>
  group_by(team_id, season) |> slice(1) |> ungroup()   # one head coach of record per team-season

cat("zone columns:", paste(names(p_zon)[str_starts(names(p_zon), "fga_")], collapse = ", "), "\n")

zone_fga <- p_zon |>
  transmute(team_id, season
            , fga_total = rowSums(across(starts_with("fga_") & !matches("backcourt")))
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
dims <- c(rk_pace = "Pace", rk_early_share = "Early", rk_late_share = "No late", rk_passes_per_poss = "Passes"
          , rk_rim_share = "Rim", rk_three_share = "Threes")
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
  geom_text(aes(label = scales::ordinal(rank)), nudge_y = 3.2, size = 3.2, colour = "grey20") +
  coord_radial(expand = FALSE, inner.radius = 0.05) +
  scale_y_continuous(limits = c(0, 34), breaks = NULL) +
  facet_wrap(~ lab) +
  labs(title = "Brian Keefe's three offenses"
       , subtitle = "Same petals as the league chart, as league ranks out of 30 (red ring = 15th). 'No late' is ranked so that 1st = fewest late-clock shots."
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp(13) + theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"), panel.spacing.x = unit(3, "lines"))
save_plot("keefe_fingerprints.png", p_keefe, width = 12, height = 5.5)

cat("\n== Keefe league ranks by season, 1 = most (late-clock not flipped here) ==\n")
pt(panel_z |> filter(team_id == WIZ_ID, season_start >= 2024) |>
     select(season, rk_pace, rk_early_share, rk_late_share, rk_passes_per_poss, rk_rim_share, rk_three_share, rk_mid_share, rk_efg_pct, rk_off_rating))

cat("\nPart B figures written.\n")


# ===========================================================================
# 4. Defense and the rotation: the same three tests, plus Keefe's defensive dials
# ===========================================================================
set.seed(202)
wiz_blue <- "#172869FF"; wiz_red <- "#D9565CFF"; teal <- "#3B9AB2"; gold <- "#E1AF00"

# ---------------------------------------------------------------------------
# Build the team-season panel
# ---------------------------------------------------------------------------
adv  <- rd("panel_advanced");   pas <- rd("panel_passing");  clk <- rd("panel_shotclock"); zon <- rd("panel_shotzones")
cch  <- rd("panel_coaches") |> group_by(team_id, season) |> slice(1) |> ungroup()
dfn  <- rd("panel_defense");   ff  <- rd("panel_fourfactors")
oclk <- rd("panel_oppclock");  oloc <- rd("panel_opploc");   lu  <- rd("panel_lineups")
hus  <- if (file.exists(file.path(DATA_DIR, "panel_hustle.csv"))) rd("panel_hustle") else NULL

off_clock <- clk |> group_by(team_id, season) |>
  summarise(early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]), .groups = "drop")
def_clock <- oclk |> group_by(team_id, season) |>
  summarise(opp_early_share = sum(fga_frequency[shot_clock_range %in% c("24-22", "22-18 Very Early")])
            , opp_late_share = sum(fga_frequency[shot_clock_range %in% c("7-4 Late", "4-0 Very Late")]), .groups = "drop")
off_zone <- zon |> transmute(team_id, season, tot = rowSums(across(starts_with("fga_") & !matches("backcourt|corner_3$")))
                             , rim_share = fga_restricted_area / tot, mid_share = fga_mid_range / tot
                             , three_share = (fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3) / tot) |> select(-tot)
def_zone <- oloc |> transmute(team_id, season, tot = rowSums(across(starts_with("opp_fga_") & !matches("backcourt|corner_3$")))
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
grp_cols <- c(`Offense: process` = wiz_blue, `Defense: process` = teal, Rotation = gold, Outcome = wiz_red)
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
ddims <- c(rk_opp_three_share = "Fewer 3s\nallowed", rk_opp_rim_share = "Fewer rim\nshots", rk_opp_late_share = "Force late\nclock"
           , rk_opp_fta_rate = "Fewer\nfouls", rk_opp_tov_pct = "Force\nturnovers", rk_opp_oreb_pct = "Clean\nglass")
dfp <- panel_z |> filter(team_id == WIZ_ID, season_start >= 2024) |>
  select(season, w, l, def_rating, all_of(names(ddims))) |>
  # flip the ones where "more" is bad so every petal's rank 1 = the scheme-friendly end
  mutate(across(c(rk_opp_three_share, rk_opp_rim_share, rk_opp_fta_rate, rk_opp_oreb_pct), ~ 31 - .x)) |>
  pivot_longer(all_of(names(ddims)), names_to = "dim", values_to = "rank") |>
  mutate(dim = factor(ddims[dim], levels = ddims), r = 31 - rank, lab = paste0(season, "  (", w, "-", l, ", DRtg ", round(def_rating, 1), ")"))
p_dfp <- ggplot(dfp, aes(dim, r)) +
  geom_col(width = 1, colour = "white", fill = teal, alpha = 0.9) +
  geom_hline(yintercept = 15.5, colour = wiz_red, linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(label = ordinal(rank)), nudge_y = 3.5, size = 3.2, colour = "grey20", family = font) +
  coord_radial(expand = FALSE, inner.radius = 0.05) + scale_y_continuous(limits = c(0, 34), breaks = NULL) +
  facet_wrap(~ lab) +
  labs(title = "Brian Keefe's three defenses", subtitle = "Six things a defensive scheme decides, as league ranks out of 30 (red ring = 15th). Every petal points outward when the defense is doing\nthe thing coaches ask for: a petal at the rim is 1st in the NBA at it, one that barely shows is 30th."
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wp(13) + theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"), panel.spacing.x = unit(3, "lines"), axis.text.x = element_text(size = 9, lineheight = 0.8))
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
       , subtitle = "Share of all minutes taken by each team's most-used 5-man lineups, 2026-27. Each grey line is a team; blue is the Wizards.\nA steep line means the coach leans on a few lineups; a flat one means he is still searching.", x = "Lineups, ranked by minutes", y = "Share of all minutes", caption = caption_text) +
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
coaches <- rd("panel_coaches") |> group_by(team_id, season) |> slice(1) |> ungroup() |>
  select(team_id, season, coach = coach_name)

# game order and result per team
games <- box |> group_by(season, game_id, team_id, team_tricode, home) |> summarise(pts = sum(points, na.rm = TRUE), .groups = "drop") |>
  group_by(season, game_id) |> mutate(opp_pts = sum(pts) - pts, won = pts > opp_pts) |> ungroup() |>
  group_by(season, team_id) |> arrange(game_id, .by_group = TRUE) |> mutate(g = row_number()) |> ungroup()
box <- box |> left_join(games |> select(season, game_id, team_id, g, won), by = c("season", "game_id", "team_id"))

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
  scale_fill_gradient(low = "grey95", high = wiz_blue, name = "Minutes", limits = c(0, 42)) +
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

# every scoring event, with points for each side, to compute runs around a timestamp
scores <- pbp |> filter(action_type %in% c("Made Shot", "Free Throw"), !is.na(team_id), team_id > 0) |>
  group_by(season, game_id) |> arrange(action_number, .by_group = TRUE) |>
  mutate(pts = (score_home + score_away) - lag(score_home + score_away, default = 0)) |> ungroup() |>
  filter(pts > 0) |> select(season, game_id, elapsed, team_id, pts)

# --- Timeouts -----------------------------------------------------------------
to <- pbp |> filter(action_type == "Timeout", sub_type == "Regular", !is.na(team_id), team_id > 0) |>
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

# --- team / coach level, 2025-26 and 2026-27 ---------------------------------------
coaches <- rd("panel_coaches") |> group_by(team_id, season) |> slice(1) |> ungroup()
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
  mutate(used_before_2min = replace_na(used_before_2min, 0), left = pmax(7 - used_before_2min, 0)) |>
  group_by(season, team_id) |> summarise(close_games = n(), mean_to_left_2min = mean(left), .groups = "drop")
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
# only some get a "Coach Challenge" timeout row. The replay row carries no team, so attribute the challenge to
# the team penalised on the call being reviewed: the preceding foul / turnover / violation's team, or the
# challenge-timeout row's team when one exists within the previous few actions.
replay_rows <- pbp |> filter(action_type == "Instant Replay", str_starts(sub_type, "Coach Challenge")) |>
  select(season, game_id, action_number, period, clock_sec, elapsed, ruling = sub_type, score_home, score_away, home_id, away_id)
attribute_team <- function(key, a) {
  g <- ev_split[[key]]; idx <- match(a, g$action_number); if (is.na(idx)) return(NA_real_)
  lo <- max(1, idx - 8)
  for (j in seq(idx - 1, lo)) {
    if (j < 1) break
    if (g$action_type[j] == "Timeout" && identical(g$sub_type[j], "Coach Challenge") && g$team_id[j] > 0) return(g$team_id[j])
  }
  for (j in seq(idx - 1, lo)) {
    if (j < 1) break
    if (g$action_type[j] %in% c("Foul", "Turnover", "Violation") && g$team_id[j] > 0) return(g$team_id[j])
  }
  NA_real_
}
chal <- replay_rows |> mutate(key = paste(season, game_id), team_id = map2_dbl(key, action_number, attribute_team)) |>
  filter(!is.na(team_id)) |>
  mutate(margin = if_else(team_id == home_id, score_home - score_away, score_away - score_home)
         , won = str_detect(ruling, regex("overturn", ignore_case = TRUE))
         , late_close = period >= 4 & clock_sec <= 300 & abs(margin) <= 8) |>
  left_join(to |> distinct(team_id, team_tricode), by = "team_id")
cat("\n== challenges found and attributed:", nrow(chal), "of", nrow(replay_rows), "replay rows ==\n")
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
  scale_fill_gradient(low = "grey95", high = "grey35", name = "League timeouts\n(2025-26 + 2026-27)") +
  geom_vline(xintercept = c(12, 24, 36), colour = "white", linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = wiz_red, linewidth = 0.4, linetype = "dashed") +
  geom_point(data = to_plot |> filter(who == "Wizards", season == CUR), aes(game_min, margin, colour = stop_the_bleeding), size = 3.2, alpha = 0.9) +
  scale_colour_manual(values = c(`TRUE` = wiz_red, `FALSE` = wiz_blue), labels = c(`TRUE` = "on a 6-0+ opponent run", `FALSE` = "other"), name = "Keefe's 2026-27 timeouts") +
  scale_x_continuous(breaks = seq(0, 48, 6), limits = c(0, 48)) +
  coord_cartesian(ylim = c(-30, 30)) +
  labs(title = "When coaches call timeout, and when Keefe does"
       , subtitle = "Game minute (x) and score margin for the calling team (y) at every regular timeout, regulation only.\nThe dark blocks near 6:00 of each quarter are the mandatory TV timeouts; the block at the end is the two-minute endgame.\nBlue and red dots: the Wizards' own timeouts this season."
       , x = "Minute of game", y = "Margin for the team calling timeout", caption = caption_text) +
  theme_wp() + theme(legend.position = "right")
save_plot("timeout_map.png", p_map, width = 13, height = 7.5)

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
player_pal <- c("#172869", "#D9565C", "#3B9AB2", "#E1AF00", "#78B7C5", "#F21A00", "#8C6D31", "#7570B3", "#66A61E", "#E7298A"
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


cat("\nAll done. Figures are in possession_figs/.\n")
