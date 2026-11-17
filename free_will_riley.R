###########################################################
# Free Will Riley
# Who Will Riley is, who he resembles, and the case for playing him more.
#
# One script: pulls the data (embedded Python fetcher), fits the Stan models, writes every figure to
# figs/ and every number the write-up quotes to riley_results.rds. free_will_riley.qmd only reads that file.
#
# Run from this folder:   Rscript free_will_riley.R          (REFRESH=1 re-pulls the current season)
# Then:                   quarto render free_will_riley.qmd
#
# R 4.5; tidyverse, janitor, scales, patchwork, ggrepel, ggtext, ragg, cmdstanr, posterior.
###########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(scales)
  library(patchwork)
  library(ggrepel)
  library(ggtext)
  library(cmdstanr)
  library(posterior)
})
set.seed(202)
options(dplyr.summarise.inform = FALSE)

# ===========================================================================
# House style (same as wizards_rankings.R and keefe_deepdive.R)
# ===========================================================================
font <- "Gill Sans"
if (requireNamespace("systemfonts", quietly = TRUE) && !font %in% systemfonts::system_fonts()$family) font <- "sans"
pal <- c(red = "#D9535B", teal = "#3A9AB2", teal_dark = "#2A7F96", navy = "#1B2A6B", grey = "#4D4D4D", light = "#BFBFBF", gold = "#E3A33B")
caption_text <- "Data: nba.com/stats\nwizardspoints.substack.com"
WIZ_ID <- 1610612764; RILEY_ID <- 1642860
ord_word <- function(n) c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth", "13th", "14th", "15th")[n]
CUR <- "2026-27"; PREV <- "2025-26"
GOODWIN_ID <- 1630692; GOODWIN_SEASON <- "2022-23"  # the hustle comparison: Jordan Goodwin's one full season as a Wizard
STAB_SEASON <- "2024-25"  # paired with PREV to check that hustle rates repeat from one season to the next

theme_wiz <- function(base_size = 18) {
  theme_minimal(base_size = base_size, base_family = font) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.6), colour = "black", margin = margin(b = 10))
      , plot.subtitle = element_textbox_simple(colour = pal[["grey"]], size = rel(1.05), lineheight = 1.1, margin = margin(b = 18))
      , plot.caption = element_text(hjust = 1, colour = "black", size = rel(0.75), lineheight = 1.1, margin = margin(t = 14))
      , plot.title.position = "plot", plot.caption.position = "plot"
      , panel.grid = element_blank()
      , axis.text = element_text(colour = pal[["grey"]])
      , axis.title.x = element_text(size = rel(1.0), margin = margin(t = 8))
      , axis.title.y = element_text(size = rel(1.0), margin = margin(r = 8))
      , legend.text = element_text(colour = pal[["grey"]])
      , strip.text = element_text(face = "bold", colour = "black", hjust = 0)
      , plot.margin = margin(18, 22, 12, 18)
      , plot.background = element_rect(fill = "white", colour = NA))
}
dir.create("figs", showWarnings = FALSE)
save_plot <- function(name, p, width = 11, height = 8) {
  ggsave(file.path("figs", name), p, width = width, height = height, dpi = 300, device = ragg::agg_png, bg = "white")
  invisible(p)
}
pt <- function(x, digits = 3) print(as.data.frame(x), digits = digits, row.names = FALSE)
pct <- function(x, acc = 1) scales::percent(x, accuracy = acc)
season_str <- function(y) sprintf("%d-%02d", y, (y + 1) %% 100)
season_year <- function(s) as.integer(substr(s, 1, 4))

# ===========================================================================
# 0. Data: the embedded fetcher, a one-time virtualenv, and a cache folder
# ===========================================================================
# stats.nba.com refuses R's TLS fingerprint, so the pull runs in Python with curl_cffi (as in keefe_deepdive.R).
DATA_DIR <- "data"
REFRESH <- nzchar(Sys.getenv("REFRESH"))
dir.create(DATA_DIR, showWarnings = FALSE)

nba_fetch_py <- r"---(
#!/usr/bin/env python3
"""
fetch_riley.py  (embedded in free_will_riley.R; also runnable on its own)

Pulls what the Will Riley post needs from stats.nba.com into OUT_DIR. Prints counts, never data.
Files that already exist are skipped; --refresh-current re-pulls the current season's files.

    python fetch_riley.py <out_dir> [--refresh-current] [--season 2026-27]

League-wide, every season from 1996-97: player Base and Advanced totals, bio stats (height, weight, age, draft).
Will Riley's two seasons: every player's game log, his shot chart, Synergy play types (offense), tracking
(drives, catch-and-shoot, pull-ups), hustle, the Wizards' on/off table, Wizards box scores (DNP comments) and Wizards play-by-play (cdn.nba.com).
"""
import os, sys, time
import pandas as pd
from curl_cffi import requests as cf_requests

OUT_DIR = sys.argv[1] if len(sys.argv) > 1 else "data"
REFRESH_CURRENT = "--refresh-current" in sys.argv
CUR = sys.argv[sys.argv.index("--season") + 1] if "--season" in sys.argv else "2026-27"
WIZ, RILEY = 1610612764, 1642860
ST = "Regular Season"
SLEEP = 1.0
BASE = "https://stats.nba.com/stats"
season_str = lambda y: f"{y}-{str(y + 1)[-2:]}"
HIST = [season_str(y) for y in range(1996, int(CUR[:4]) + 1)]
RILEY_SEASONS = [season_str(y) for y in range(int(CUR[:4]) - 1, int(CUR[:4]) + 1)]
HUSTLE_EXTRA = ["2022-23", "2024-25"]  # Goodwin's Wizards season; the season before Riley's rookie year, for year-to-year stability
PLAY_TYPES = ["Isolation", "Transition", "PRBallHandler", "PRRollman", "Postup", "Spotup", "Handoff", "Cut", "OffScreen", "OffRebound", "Misc"]

HEADERS = {
    "Connection": "keep-alive", "Accept": "application/json, text/plain, */*", "x-nba-stats-token": "true",
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36",
    "x-nba-stats-origin": "stats", "Sec-Fetch-Site": "same-origin", "Sec-Fetch-Mode": "cors",
    "Referer": "https://www.nba.com/", "Accept-Encoding": "gzip, deflate, br", "Accept-Language": "en-US,en;q=0.9",
}
session = cf_requests.Session(impersonate="chrome120")


def path(name):
    return os.path.join(OUT_DIR, name)


def get_json(endpoint, params, tries=4):
    for attempt in range(tries):
        try:
            r = session.get(f"{BASE}/{endpoint}", params=params, headers=HEADERS, timeout=60)
            r.raise_for_status()
            time.sleep(SLEEP)
            return r.json()
        except Exception as e:  # noqa
            print(f"  ! {endpoint} attempt {attempt + 1}: {type(e).__name__}", file=sys.stderr, flush=True)
            time.sleep(5 * (attempt + 1))
    raise RuntimeError(f"gave up on {endpoint}")


def fetch(endpoint, params, rs_name=None, rs_index=0):
    data = get_json(endpoint, params)
    rs = data.get("resultSets") or [data.get("resultSet")]
    if isinstance(rs, dict):
        rs = [rs]
    sel = next(r for r in rs if r["name"] == rs_name) if rs_name else rs[rs_index]
    return pd.DataFrame(sel["rowSet"], columns=sel["headers"])


def save(df, name):
    df.to_csv(path(name), index=False)
    print(f"  wrote {name}: {len(df)} rows", flush=True)


def need(name, season=None):
    return not os.path.exists(path(name)) or (REFRESH_CURRENT and season == CUR)


DASH = dict(College="", Conference="", Country="", DateFrom="", DateTo="", Division="", DraftPick="", DraftYear="", GameScope="",
            GameSegment="", Height="", LastNGames=0, LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0,
            PaceAdjust="N", PerMode="Totals", Period=0, PlayerExperience="", PlayerPosition="", PlusMinus="N", Rank="N",
            SeasonSegment="", SeasonType=ST, ShotClockRange="", StarterBench="", TeamID=0, TwoWay=0, VsConference="", VsDivision="", Weight="")


def player_dash(season, measure):
    return fetch("leaguedashplayerstats", dict(DASH, MeasureType=measure, Season=season))


def team_dash(season):
    p = {k: v for k, v in DASH.items() if k not in ("College", "Country", "DraftPick", "DraftYear", "Height", "Weight")}
    return fetch("leaguedashteamstats", dict(p, MeasureType="Advanced", Season=season))


def bio(season):
    p = {k: v for k, v in DASH.items() if k not in ("PaceAdjust", "PlusMinus", "Rank", "TwoWay")}
    return fetch("leaguedashplayerbiostats", dict(p, Season=season))


def pt(season, measure):
    p = {k: v for k, v in DASH.items() if k not in ("PaceAdjust", "PlusMinus", "Rank", "TwoWay", "GameSegment", "Period", "ShotClockRange")}
    return fetch("leaguedashptstats", dict(p, PerMode="Totals", PlayerOrTeam="Player", PtMeasureType=measure, Season=season))


def hustle(season):
    p = {k: v for k, v in DASH.items() if k not in ("PlusMinus", "Rank", "TwoWay", "GameSegment", "Period", "ShotClockRange")}
    return fetch("leaguehustlestatsplayer", dict(p, Season=season))


def synergy(season):
    frames = []
    for pt_ in PLAY_TYPES:
        d = fetch("synergyplaytypes", dict(LeagueID="00", PerMode="Totals", PlayType=pt_, PlayerOrTeam="P", SeasonType=ST,
                                           SeasonYear=season, TypeGrouping="offensive"))
        d["PLAY_TYPE"] = pt_
        frames.append(d)
    return pd.concat(frames, ignore_index=True)


def shots(season, player_id):
    p = dict(AheadBehind="", ClutchTime="", ContextFilter="", ContextMeasure="FGA", DateFrom="", DateTo="", EndPeriod=10, EndRange=28800,
             GameID="", GameSegment="", LastNGames=0, LeagueID="00", Location="", Month=0, OpponentTeamID=0, Outcome="", PORound=0,
             Period=0, PlayerID=player_id, PlayerPosition="", PointDiff="", Position="", RangeType=0, RookieYear="", Season=season,
             SeasonSegment="", SeasonType=ST, StartPeriod=1, StartRange=0, TeamID=0, VsConference="", VsDivision="")
    data = get_json("shotchartdetail", p)
    out = {}
    for r in data["resultSets"]:
        out[r["name"]] = pd.DataFrame(r["rowSet"], columns=r["headers"])
    return out["Shot_Chart_Detail"], out["LeagueAverages"]


def gamelog(season):
    return fetch("leaguegamelog", dict(Counter=100000, DateFrom="", DateTo="", Direction="ASC", LeagueID="00", PlayerOrTeam="P",
                                       Season=season, SeasonType=ST, Sorter="DATE"))


def onoff(season):
    p = dict(DateFrom="", DateTo="", GameSegment="", LastNGames=0, LeagueID="00", Location="", MeasureType="Advanced", Month=0,
             OpponentTeamID=0, Outcome="", PaceAdjust="N", PerMode="Totals", Period=0, PlusMinus="N", Rank="N", Season=season,
             SeasonSegment="", SeasonType=ST, TeamID=WIZ, VsConference="", VsDivision="")
    data = get_json("teamplayeronoffdetails", p)
    frames = []
    for r in data["resultSets"]:
        if r["name"] in ("PlayersOnCourtTeamPlayerOnOffDetails", "PlayersOffCourtTeamPlayerOnOffDetails"):
            d = pd.DataFrame(r["rowSet"], columns=r["headers"])
            d["COURT"] = "on" if "OnCourt" in r["name"] else "off"
            frames.append(d)
    return pd.concat(frames, ignore_index=True)


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


def wizard_boxes(season):
    name = f"boxscores_{season}.csv"
    gl = fetch("leaguegamelog", dict(Counter=5000, DateFrom="", DateTo="", Direction="ASC", LeagueID="00", PlayerOrTeam="T",
                                     Season=season, SeasonType=ST, Sorter="DATE"))
    gids = sorted(gl.loc[gl["TEAM_ID"] == WIZ, "GAME_ID"].astype(str).str.zfill(10).unique().tolist())
    p = path(name)
    done = set(pd.read_csv(p, usecols=["gameId"], dtype=str)["gameId"].unique()) if os.path.exists(p) else set()
    todo = [g for g in gids if g not in done]
    print(f"  {name}: {len(todo)} games to fetch", flush=True)
    for gid in todo:
        df = box_game(gid)
        df.to_csv(p, mode="a", header=not os.path.exists(p), index=False)


CDN_PBP = "https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_{}.json"
PBP_COLS = ["gameId", "orderNumber", "period", "clock", "teamId", "personId", "actionType", "subType", "scoreHome", "scoreAway",
            "descriptor", "foulDrawnPersonId", "jumpBallWonPersonId", "jumpBallLostPersonId"]


def cdn_pbp(gid, tries=4):
    """Every action in one game from cdn.nba.com's liveData feed: substitutions carry the player and in/out,
    and every row carries the score. (stats.nba.com's gamerotation feed has stints that end before they start.)"""
    for attempt in range(tries):
        try:
            r = session.get(CDN_PBP.format(gid), headers={"Referer": "https://www.nba.com/", "Origin": "https://www.nba.com"}, timeout=45)
            r.raise_for_status()
            time.sleep(0.3)
            acts = pd.DataFrame(r.json()["game"]["actions"])
            acts["gameId"] = gid
            for c in PBP_COLS:
                if c not in acts.columns:
                    acts[c] = None
            return acts[PBP_COLS]
        except Exception as e:  # noqa
            print(f"  ! cdn pbp {gid} attempt {attempt + 1}: {type(e).__name__}", file=sys.stderr, flush=True)
            time.sleep(4 * (attempt + 1))
    raise RuntimeError(f"gave up on cdn pbp {gid}")


def wizard_pbp(season):
    gl = fetch("leaguegamelog", dict(Counter=5000, DateFrom="", DateTo="", Direction="ASC", LeagueID="00", PlayerOrTeam="T",
                                     Season=season, SeasonType=ST, Sorter="DATE"))
    gids = sorted(gl.loc[gl["TEAM_ID"] == WIZ, "GAME_ID"].astype(str).str.zfill(10).unique().tolist())
    p = path(f"pbp_{season}.csv")
    # files from before the loose-ball and jump-ball columns were kept are pulled again
    if os.path.exists(p) and "descriptor" not in pd.read_csv(p, nrows=0).columns:
        os.remove(p)
    done = set(pd.read_csv(p, usecols=["gameId"], dtype=str)["gameId"].unique()) if os.path.exists(p) else set()
    todo = [g for g in gids if g not in done]
    print(f"  pbp_{season}.csv: {len(todo)} games to fetch", flush=True)
    for gid in todo:
        df = cdn_pbp(gid)
        df.to_csv(p, mode="a", header=not os.path.exists(p), index=False)


def main():
    os.makedirs(os.path.join(OUT_DIR, "hist"), exist_ok=True)
    if need("player_index.csv", CUR):
        save(fetch("playerindex", dict(LeagueID="00", Season=CUR, Historical=1, Active="", AllStar="", College="", Country="",
                                       DraftPick="", DraftRound="", DraftYear="", Height="", Position="", TeamID=0, Weight="")),
             "player_index.csv")
    for s in HIST:
        for stem, f in (("base", lambda s: player_dash(s, "Base")), ("adv", lambda s: player_dash(s, "Advanced")), ("bio", bio), ("team", team_dash)):
            name = f"hist/{stem}_{s}.csv"
            if need(name, s):
                save(f(s), name)
    for s in RILEY_SEASONS:
        if need(f"gamelog_{s}.csv", s):
            save(gamelog(s), f"gamelog_{s}.csv")
        if need(f"shots_riley_{s}.csv", s):
            sh, lg = shots(s, RILEY)
            save(sh, f"shots_riley_{s}.csv"); save(lg, f"shots_league_avg_{s}.csv")
        if need(f"synergy_{s}.csv", s):
            save(synergy(s), f"synergy_{s}.csv")
        for m in ("Drives", "CatchShoot", "PullUpShot"):
            if need(f"track_{m}_{s}.csv", s):
                save(pt(s, m), f"track_{m}_{s}.csv")
        if need(f"hustle_{s}.csv", s):
            save(hustle(s), f"hustle_{s}.csv")
        if need(f"onoff_{s}.csv", s):
            save(onoff(s), f"onoff_{s}.csv")
        wizard_boxes(s)
        wizard_pbp(s)
    # hustle tracking for the comparison the eye test suggests: Jordan Goodwin's one full season as a Wizard
    for s in HUSTLE_EXTRA:
        if need(f"hustle_{s}.csv", s):
            save(hustle(s), f"hustle_{s}.csv")
    print("done", flush=True)


if __name__ == "__main__":
    main()
)---"

find_python <- function() {
  for (venv in c(".venv-nba", "../.venv-nba")) {
    venv_py <- file.path(venv, "bin", "python")
    if (file.exists(venv_py) && system2(venv_py, c("-c", shQuote("import curl_cffi, pandas")), stdout = FALSE, stderr = FALSE) == 0) return(venv_py)
  }
  for (p in c("python3.12", "python3.13", "python3.11", "python3")) {
    exe <- Sys.which(p); if (!nzchar(exe)) next
    if (system2(exe, c("-c", shQuote("import curl_cffi, pandas")), stdout = FALSE, stderr = FALSE) == 0) return(exe)
  }
  base <- Sys.which("python3"); if (!nzchar(base)) stop("No python3 found; install Python 3.11+ and re-run.")
  message("Setting up ./.venv-nba with curl_cffi + pandas (one time) ...")
  venv_py <- file.path(".venv-nba", "bin", "python")
  system2(base, c("-m", "venv", ".venv-nba")); system2(venv_py, c("-m", "pip", "install", "-q", "curl_cffi", "pandas"))
  if (system2(venv_py, c("-c", shQuote("import curl_cffi, pandas")), stdout = FALSE, stderr = FALSE) != 0) stop("virtualenv setup failed")
  venv_py
}
run_fetch <- function() {
  f <- tempfile("fetch_riley_", fileext = ".py"); writeLines(nba_fetch_py, f)
  message("Pulling data from stats.nba.com (first run takes ~25 minutes; later runs only add what is missing) ...")
  if (system2(find_python(), c(f, DATA_DIR, "--season", CUR, if (REFRESH) "--refresh-current"), stdout = "", stderr = "") != 0) stop("the data pull failed; see messages above")
}
needed <- c("player_index.csv", sprintf("hist/%s_%s.csv", c("base", "adv", "bio", "team"), CUR), sprintf("gamelog_%s.csv", c(PREV, CUR))
            , sprintf("pbp_%s.csv", c(PREV, CUR)), sprintf("boxscores_%s.csv", c(PREV, CUR)), sprintf("onoff_%s.csv", CUR), sprintf("synergy_%s.csv", PREV)
            , sprintf("hustle_%s.csv", c(PREV, CUR, GOODWIN_SEASON, STAB_SEASON)))
# the play-by-play must carry the loose-ball-foul and jump-ball columns (older caches do not)
pbp_cols_ok <- all(map_lgl(c(PREV, CUR), \(s) { f <- file.path(DATA_DIR, sprintf("pbp_%s.csv", s)); file.exists(f) && "descriptor" %in% names(read_csv(f, n_max = 0, show_col_types = FALSE)) }))
if (REFRESH || !pbp_cols_ok || !all(file.exists(file.path(DATA_DIR, needed)))) run_fetch() else message("Using cached data in ", DATA_DIR, " (REFRESH=1 pulls the latest games).")

# ===========================================================================
# Stan models (written to stan/ at run time, so this script is the only source file)
# ===========================================================================
dir.create("stan", showWarnings = FALSE)
minutes_stan <- r"---(
// Second-season minutes per game for young rotation rookies, from what their first season showed.
// Student-t likelihood: a few players' second seasons are wrecked by injury or trade, and those should not drag
// the typical player's prediction. Predictors are standardized before they reach Stan.
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
  int<lower=1> M;          // players to predict (Riley)
  matrix[M, K] Xp;
  int<lower=0, upper=1> prior_only;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
  real<lower=1> nu;
}
model {
  alpha ~ normal(20, 8);
  beta ~ normal(0, 4);
  sigma ~ normal(0, 8);
  nu ~ gamma(2, 0.1);
  if (!prior_only) y ~ student_t(nu, alpha + X * beta, sigma);
}
generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;
  vector[M] mu_p = alpha + Xp * beta;
  vector[M] y_p;
  for (n in 1:N) {
    y_rep[n] = student_t_rng(nu, alpha + X[n] * beta, sigma);
    log_lik[n] = student_t_lpdf(y[n] | nu, alpha + X[n] * beta, sigma);
  }
  for (m in 1:M) y_p[m] = student_t_rng(nu, mu_p[m], sigma);
}
)---"

shoot_stan <- r"---(
// Three latent shooting talents per young player, on the logit scale relative to the league that season:
//   ft  = free-throw talent (rookie season)
//   t1  = three-point talent as a rookie
//   tf  = three-point talent over career years 2-4
// They share a multivariate normal (non-centered, LKJ on the correlation), and each is seen only through a
// binomial count. A player with no attempts in a slot contributes nothing for it, so the model fills
// that slot from the correlation with the slots it does see.
data {
  int<lower=1> N;
  array[N] int<lower=0> ftm;  array[N] int<lower=0> fta;
  array[N] int<lower=0> m1;   array[N] int<lower=0> a1;
  array[N] int<lower=0> mf;   array[N] int<lower=0> af;
  vector[N] off_ft; vector[N] off1; vector[N] offf;   // logit of league rate in the matching seasons
  int<lower=0, upper=1> prior_only;
}
parameters {
  vector[3] mu;
  vector<lower=0>[3] tau;
  cholesky_factor_corr[3] L;
  matrix[3, N] z;
}
transformed parameters {
  matrix[N, 3] theta = rep_matrix(mu', N) + (diag_pre_multiply(tau, L) * z)';
}
model {
  mu ~ normal(0, 0.5);
  tau ~ normal(0, 0.5);
  L ~ lkj_corr_cholesky(2);
  to_vector(z) ~ std_normal();
  if (!prior_only) {
    ftm ~ binomial_logit(fta, off_ft + theta[, 1]);
    m1 ~ binomial_logit(a1, off1 + theta[, 2]);
    mf ~ binomial_logit(af, offf + theta[, 3]);
  }
}
generated quantities {
  matrix[3, 3] Omega = multiply_lower_tri_self_transpose(L);
  array[N] int mf_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    mf_rep[n] = binomial_rng(af[n], inv_logit(offf[n] + theta[n, 3]));
    log_lik[n] = binomial_logit_lpmf(mf[n] | af[n], offf[n] + theta[n, 3]);
  }
}
)---"

# Diagnostics gate on the parameters whose values the write-up uses. Fails loudly: no cache is written for a bad fit.
check_fit <- function(fit, pars, label, extra = character()) {
  s <- fit$summary(variables = c(pars, extra), "rhat", "ess_bulk", "ess_tail", "mean", "sd", mcse = "mcse_mean")
  sd_ <- fit$sampler_diagnostics(format = "df")
  ebfmi <- sd_ |> group_by(.chain) |> summarise(e = sum(diff(energy__)^2) / sum((energy__ - mean(energy__))^2)) |> pull(e)
  out <- list(label = label, n_par = nrow(s), max_rhat = max(s$rhat, na.rm = TRUE), min_ess_bulk = min(s$ess_bulk, na.rm = TRUE)
              , min_ess_tail = min(s$ess_tail, na.rm = TRUE), max_mcse_sd = max(s$mcse / s$sd, na.rm = TRUE)
              , divergent = sum(sd_$divergent__), max_treedepth_hits = sum(sd_$treedepth__ >= 10), min_ebfmi = min(ebfmi), summary = s)
  gated <- s |> filter(variable %in% fit$summary(variables = pars, "rhat")$variable)
  bad <- c(if (max(gated$rhat, na.rm = TRUE) >= 1.01) "R-hat", if (min(gated$ess_bulk, na.rm = TRUE) < 400) "bulk ESS"
           , if (min(gated$ess_tail, na.rm = TRUE) < 400) "tail ESS", if (out$divergent > 0) "divergences", if (out$min_ebfmi < 0.3) "E-BFMI")
  message(sprintf("[%s] max R-hat %.3f, min bulk/tail ESS %.0f/%.0f, divergences %d, min E-BFMI %.2f", label, out$max_rhat, out$min_ess_bulk, out$min_ess_tail, out$divergent, out$min_ebfmi))
  if (length(bad)) stop(sprintf("[%s] failed the diagnostics gate: %s", label, paste(bad, collapse = ", ")))
  out
}

# ===========================================================================
# 1. Load: every player-season since 1996-97, and Riley's two seasons in detail
# ===========================================================================
rd <- function(f, ...) read_csv(file.path(DATA_DIR, f), show_col_types = FALSE, guess_max = 10000, ...) |> clean_names()

hist_seasons <- season_str(1996:season_year(CUR))
load_hist <- function(s) {
  b <- rd(sprintf("hist/base_%s.csv", s)) |> select(player_id, player_name, team_id, team_abbreviation, age, gp, min, fgm, fga, fg3m, fg3a, ftm, fta, oreb, dreb, reb, ast, tov, stl, blk, pf, pts, plus_minus)
  a <- rd(sprintf("hist/adv_%s.csv", s)) |> select(player_id, off_rating, def_rating, net_rating, ast_pct, oreb_pct, dreb_pct, reb_pct, tm_tov_pct, ts_pct, usg_pct, pace, pie, poss)
  bi <- rd(sprintf("hist/bio_%s.csv", s)) |> select(player_id, height_in = player_height_inches, weight = player_weight, draft_year, draft_round, draft_number) |>
    mutate(across(-player_id, as.character))
  b |> left_join(a, by = "player_id") |> left_join(bi, by = "player_id") |> mutate(season = s)
}
ps <- map_dfr(hist_seasons, load_hist) |>
  mutate(height_in = suppressWarnings(as.numeric(height_in)), weight = suppressWarnings(as.numeric(weight))
         , draft_number = suppressWarnings(as.integer(draft_number)), sy = season_year(season))

pidx <- rd("player_index.csv") |>
  transmute(player_id = person_id, name = paste(player_first_name, player_last_name), position, from_year = as.integer(from_year)
            , idx_height = height, idx_weight = as.numeric(weight), idx_draft = suppressWarnings(as.integer(draft_number)))

# career year from the player index's first season, not from the first row inside this data window
ps <- ps |> left_join(pidx |> select(player_id, from_year, position), by = "player_id") |>
  mutate(career_yr = sy - from_year + 1)

# league context each season (minutes-weighted), so rates from 1997 and 2026 can sit on one scale
lg <- ps |> group_by(season, sy) |>
  summarise(lg_ts = sum(pts) / (2 * (sum(fga) + 0.44 * sum(fta))), lg_3p = sum(fg3m) / sum(fg3a), lg_ft = sum(ftm) / sum(fta)
            , lg_3par = sum(fg3a) / sum(fga), lg_ftr = sum(fta) / sum(fga), lg_mpg_top = NA_real_, .groups = "drop")
ps <- ps |> left_join(lg, by = c("season", "sy")) |>
  mutate(mpg = min / gp, ts = pts / (2 * (fga + 0.44 * fta)), ts_rel = ts - lg_ts
         , three_rate = if_else(fga > 0, fg3a / fga, NA_real_), ft_rate = if_else(fga > 0, fta / fga, NA_real_)
         , per100 = 100 / pmax(poss, 1), stl100 = stl * per100, blk100 = blk * per100, pts100 = pts * per100, ast100 = ast * per100)
stopifnot(!anyDuplicated(ps[c("player_id", "season")]))

riley <- ps |> filter(player_id == RILEY_ID)
stopifnot(nrow(riley) == 2, all(riley$career_yr == 1:2))

# ===========================================================================
# 2. Who he is: the 2025-26 rookie season against every wing and forward
# ===========================================================================
# Peers: everyone listed 6'6" to 6'10" who played 800+ minutes in 2025-26, leaving out players the index lists as
# centers (C, C-F, F-C), so the group is wings and forwards.
trk <- function(m, s = PREV) rd(sprintf("track_%s_%s.csv", m, s)) |> select(-any_of(c("player_name", "team_id", "team_abbreviation", "gp", "w", "l", "min")))
# a traded player has one Synergy row per team: pool them, then recompute his shares and points per play
syn_raw <- rd(sprintf("synergy_%s.csv", PREV))
syn <- syn_raw |> group_by(player_id, player_name, play_type) |>
  summarise(percentile = weighted.mean(percentile, poss), poss = sum(poss), pts = sum(pts), .groups = "drop") |>
  group_by(player_id) |> mutate(poss_pct = poss / sum(poss), ppp = pts / poss) |> ungroup()
syn_wide <- syn |> select(player_id, play_type, poss_pct, ppp, poss) |>
  pivot_wider(names_from = play_type, values_from = c(poss_pct, ppp, poss), values_fill = 0)
hus <- rd(sprintf("hustle_%s.csv", PREV)) |> select(player_id, deflections, contested_shots)
peers <- ps |> filter(season == PREV, min >= 800, between(height_in, 78, 82), !str_detect(replace_na(position, ""), "C")) |>
  left_join(trk("Drives"), by = "player_id") |> left_join(trk("CatchShoot"), by = "player_id") |>
  left_join(syn_wide, by = "player_id") |> left_join(hus, by = "player_id") |>
  mutate(drives36 = drives / min * 36, defl36 = deflections / min * 36, prbh = poss_pct_PRBallHandler
         , cut_ppp = if_else(poss_Cut >= 20, ppp_Cut, NA_real_), cs3 = if_else(catch_shoot_fg3a >= 100, catch_shoot_fg3_pct, NA_real_))
stopifnot(RILEY_ID %in% peers$player_id)
profile_rows <- tribble(
  ~var, ~label, ~sub, ~fmt,
  "drives36", "Drives", "per 36 minutes", "%.1f",
  "prbh", "Pick-and-roll ball handler", "share of his plays", "pct",
  "ft_rate", "Free throws per shot", "free throws per shot taken", "%.2f",
  "cut_ppp", "Points per cut", "players with 20+ cuts", "%.2f",
  "usg_pct", "Plays he finished", "share of team plays on the floor", "pct",
  "ts", "True shooting", "scoring per shot, counting 3s and free throws", "pct",
  "cs3", "Catch-and-shoot threes", "100+ attempts", "pct",
  "ast_pct", "Assists", "share of teammate baskets he set up", "pct",
  "tm_tov_pct", "Turnovers", "per 100 of his plays", "%.1f",
  "stl100", "Steals", "per 100 possessions", "%.1f",
  "blk100", "Blocks", "per 100 possessions", "%.1f",
  "defl36", "Deflections", "per 36 minutes", "%.1f",
  "dreb_pct", "Defensive rebounds", "share of misses he grabbed", "pct",
  "weight", "Weight", "listed pounds", "%.0f",
  "age", "Age", "nba.com listed", "%.0f")
prof_long <- map_dfr(seq_len(nrow(profile_rows)), \(i) {
  col <- profile_rows$var[i]
  tibble(player_id = peers$player_id, player_name = peers$player_name, value = peers[[col]]) |> filter(!is.na(value)) |> bind_cols(profile_rows[i, ])
}) |> group_by(var) |>
  mutate(n = n(), rank_most = rank(-value, ties.method = "min"), pos = (rank(value, ties.method = "average") - 1) / (n - 1)) |> ungroup()
prof_riley <- prof_long |> filter(player_id == RILEY_ID) |>
  mutate(value_txt = map2_chr(fmt, value, \(f, v) if (f == "pct") percent(v, 0.1) else sprintf(f, v)))
# rank from each end, counted directly so ties stay honest
prof_riley$rank_least <- map_int(seq_len(nrow(prof_riley)), \(i) {
  d <- prof_long |> filter(var == prof_riley$var[i]); as.integer(sum(d$value < prof_riley$value[i]) + 1) })
prof_riley$n_tied <- map_int(seq_len(nrow(prof_riley)), \(i) sum(prof_long$var == prof_riley$var[i] & prof_long$value == prof_riley$value[i]))
prof_riley <- prof_riley |> mutate(rank_txt = case_when(
  rank_most == 1 ~ paste0("Highest of ", n), rank_least == 1 & n_tied > 1 ~ paste0("Tied lowest of ", n), rank_least == 1 ~ paste0("Lowest of ", n)
  , rank_most <= n / 3 ~ paste0(scales::ordinal(rank_most), "-highest of ", n)
  , rank_least <= n / 3 ~ paste0(scales::ordinal(rank_least), "-lowest of ", n)
  , TRUE ~ paste0("Middle: ", scales::ordinal(rank_most), " of ", n)))
pt(prof_riley |> select(label, value_txt, rank_txt))

row_levels <- rev(profile_rows$label)
p_profile <- prof_long |> mutate(label = factor(label, row_levels)) |>
  ggplot(aes(pos, label)) +
  geom_hline(aes(yintercept = label), colour = "grey88", linewidth = 0.4) +
  geom_point(data = \(d) filter(d, player_id != RILEY_ID), colour = pal[["teal"]], alpha = 0.45, size = 2.6
             , position = position_jitter(height = 0.12, width = 0, seed = 202)) +
  geom_point(data = prof_riley |> mutate(label = factor(label, row_levels)), colour = pal[["red"]], size = 6) +
  geom_text(data = prof_riley |> mutate(label = factor(label, row_levels)), aes(label = value_txt), colour = pal[["red"]]
            , family = font, size = 4.6, nudge_y = 0.38, fontface = "bold") +
  geom_text(data = prof_riley |> mutate(label = factor(label, row_levels)), aes(x = 1.04, label = rank_txt), hjust = 0
            , family = font, size = 4.4, colour = pal[["navy"]], fontface = "bold") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Lowest", "Highest"), expand = expansion(mult = c(0.02, 0.02))) +
  coord_cartesian(xlim = c(0, 1), clip = "off") +
  labs(title = "A 6'9\" ball handler with a guard's frame"
       , subtitle = sprintf("Riley's rookie year next to all %d wings and forwards who played 800+ minutes (6'6\" to 6'10\", not centers). Each dot is one player. <span style='color:%s'>**Riley is in red**</span>.", n_distinct(peers$player_id), pal[["red"]])
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wiz(16) +
  theme(axis.text.y = element_text(colour = "black", size = rel(1.0)), plot.margin = margin(18, 175, 12, 18))
save_plot("riley_profile.png", p_profile, width = 12, height = 12)

# --- how he gets his offense: Synergy play types --------------------------------------------------------
pt_names <- c(PRBallHandler = "Pick-and-roll ball handler", Spotup = "Spot-up", Transition = "Transition", Isolation = "Isolation"
              , Handoff = "Handoffs", Cut = "Cuts", OffScreen = "Off screens", OffRebound = "Putbacks", PRRollman = "Roll man", Postup = "Post-ups", Misc = "Other")
riley_pt <- syn |> filter(player_id == RILEY_ID) |>
  transmute(play_type, label = pt_names[play_type], poss, poss_pct, ppp, pctile = percentile) |> arrange(desc(poss))
lg_ppp <- syn |> group_by(play_type) |> summarise(lg_ppp = sum(pts) / sum(poss))
riley_pt <- riley_pt |> left_join(lg_ppp, by = "play_type") |> filter(play_type != "Misc")
pt(riley_pt)
p_playtypes <- riley_pt |> slice_head(n = 8) |> mutate(label = fct_reorder(label, poss)) |>
  ggplot(aes(y = label)) +
  geom_col(aes(x = ppp), fill = pal[["navy"]], width = 0.62) +
  geom_errorbar(aes(xmin = lg_ppp, xmax = lg_ppp), colour = pal[["red"]], linewidth = 1.3, width = 0.8) +
  geom_text(aes(x = 0.03, label = sprintf("%d plays · %s of his offense", poss, percent(poss_pct, 1))), hjust = 0, colour = "white", family = font, size = 4.3) +
  geom_text(aes(x = pmax(ppp, lg_ppp) + 0.03, label = sprintf("%.2f · %s", ppp, scales::ordinal(round(100 * pctile)))), hjust = 0, family = font, size = 4.6, colour = pal[["grey"]]) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 1.5, 0.5), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "A capable pick-and-roll handler and an elite cutter"
       , subtitle = sprintf("How Riley scored as a rookie, by type of play, most-used first. Bars show points per play. The <span style='color:%s'>**red tick**</span> is the league average. The rank on the right: 90th means he beat 90 of every 100 players.", pal[["red"]])
       , x = "Points per play", y = NULL, caption = caption_text) +
  theme_wiz(16) + theme(axis.text.y = element_text(colour = "black"))
save_plot("riley_playtypes.png", p_playtypes, width = 12, height = 8)

# --- where he shoots from: zones against the league ------------------------------------------------------
shots <- rd(sprintf("shots_riley_%s.csv", PREV))
lg_zone <- rd(sprintf("shots_league_avg_%s.csv", PREV)) |> select(shot_zone_basic, shot_zone_area, lg_fga = fga, lg_fg = fg_pct) |>
  group_by(shot_zone_basic, shot_zone_area) |> summarise(lg_fg = weighted.mean(lg_fg, lg_fga), lg_fga = sum(lg_fga), .groups = "drop")
zones <- shots |> filter(shot_zone_basic != "Backcourt") |> group_by(shot_zone_basic, shot_zone_area) |>
  summarise(fga = n(), fgm = sum(shot_made_flag), x = mean(loc_x), y = mean(loc_y), pts_per = sum(shot_made_flag * if_else(str_detect(shot_type, "3PT"), 3, 2)) / n(), .groups = "drop") |>
  left_join(lg_zone, by = c("shot_zone_basic", "shot_zone_area")) |> mutate(fg = fgm / fga, diff = fg - lg_fg, share = fga / sum(fga))
diet <- shots |> mutate(zone = case_when(shot_zone_basic == "Restricted Area" ~ "Rim", str_detect(shot_type, "3PT") ~ "Three", TRUE ~ "Middle")) |>
  count(zone) |> mutate(share = n / sum(n))
# league shot diet for the same zones, from the league-average grid
lg_diet <- lg_zone |> mutate(zone = case_when(shot_zone_basic == "Restricted Area" ~ "Rim", str_detect(shot_zone_basic, "3") ~ "Three", shot_zone_basic == "Backcourt" ~ NA_character_, TRUE ~ "Middle")) |>
  filter(!is.na(zone)) |> group_by(zone) |> summarise(n = sum(lg_fga)) |> mutate(lg_share = n / sum(n)) |> select(-n)
diet <- diet |> left_join(lg_diet, by = "zone")
pt(zones |> select(shot_zone_basic, shot_zone_area, fga, fg, lg_fg, diff)); pt(diet)

court_lines <- function() {
  arc <- function(r, a0, a1, cx = 0, cy = 0) { a <- seq(a0, a1, length.out = 120); tibble(x = cx + r * cos(a), y = cy + r * sin(a)) }
  three_a <- acos(220 / 237.5)
  list(
    annotate("path", x = c(-250, 250, 250, -250, -250), y = c(-47.5, -47.5, 422.5, 422.5, -47.5), colour = "grey70"),
    annotate("path", x = c(-80, -80, 80, 80), y = c(-47.5, 142.5, 142.5, -47.5), colour = "grey70"),
    annotate("path", x = arc(60, 0, pi, 0, 142.5)$x, y = arc(60, 0, pi, 0, 142.5)$y, colour = "grey70"),
    annotate("path", x = arc(40, 0, pi)$x, y = arc(40, 0, pi)$y, colour = "grey70"),
    annotate("path", x = arc(7.5, 0, 2 * pi)$x, y = arc(7.5, 0, 2 * pi)$y, colour = "grey55"),
    annotate("segment", x = -30, xend = 30, y = -7.5, yend = -7.5, colour = "grey55"),
    annotate("path", x = c(-220, -220, arc(237.5, pi - three_a, three_a)$x, 220, 220), y = c(-47.5, 92.5, arc(237.5, pi - three_a, three_a)$y, 92.5, -47.5), colour = "grey70"))
}
lim <- max(abs(zones$diff[zones$fga >= 15]))
p_shots <- ggplot() + court_lines() +
  geom_point(data = shots |> filter(shot_zone_basic != "Backcourt"), aes(loc_x, loc_y), colour = "grey80", size = 0.9, alpha = 0.7) +
  geom_point(data = zones |> filter(fga >= 10), aes(x, y, size = fga, fill = diff), shape = 21, colour = "white", stroke = 1.2) +
  geom_text(data = zones |> filter(fga >= 10), aes(x, y, label = sprintf("%d%%", round(100 * fg))), family = font, size = 3.9, colour = "white", fontface = "bold") +
  scale_fill_gradient2(low = pal[["red"]], mid = "grey65", high = pal[["navy"]], midpoint = 0, limits = c(-lim, lim), labels = \(x) sprintf("%+.0f", 100 * x), name = "Made shots vs\nleague (points)") +
  scale_size(range = c(11, 27), guide = "none") +
  coord_fixed(xlim = c(-250, 250), ylim = c(-50, 300)) +
  labs(title = "Finishes at the rim; the jumper isn't there yet"
       , subtitle = sprintf("Every shot from Riley's rookie year (grey), grouped by spot on the floor. Bigger circles mean more shots (10+ only). The number is how often he made it. Blue means better than the league from that spot; red means worse. %s of his shots came at the rim (league: %s). %s were threes (league: %s).",
                            percent(diet$share[diet$zone == "Rim"], 1), percent(diet$lg_share[diet$zone == "Rim"], 1), percent(diet$share[diet$zone == "Three"], 1), percent(diet$lg_share[diet$zone == "Three"], 1))
       , x = NULL, y = NULL, caption = caption_text) +
  theme_wiz(16) + theme(axis.text = element_blank(), legend.position = "right")
save_plot("riley_shots.png", p_shots, width = 11, height = 9.5)

# ===========================================================================
# 3. Comps: the rookie seasons that looked most like Riley's, and what came next
# ===========================================================================
# Pool: every first season since 1996-97 by a player nba.com lists as 20 or younger, with at least 800 minutes.
# Rates are measured against that season's league, so a 1999 rookie and a 2025 rookie sit on one scale.
comp_feats <- c(age = "Age", height_in = "Height", weight = "Weight", mpg = "Minutes per game", usg_pct = "Usage"
                , ts_rel = "True shooting vs league", three_rel = "Threes per shot vs league", ftr_rel = "Free throws per shot vs league"
                , ft_pct = "Free-throw %", ast_pct = "Assist %", oreb_pct = "Off. rebound %", dreb_pct = "Def. rebound %"
                , tm_tov_pct = "Turnover %", stl100 = "Steals per 100", blk100 = "Blocks per 100")
ps <- ps |> mutate(three_rel = three_rate - lg_3par, ftr_rel = ft_rate - lg_ftr, ft_pct = if_else(fta > 0, ftm / fta, NA_real_))
pool <- ps |> filter(career_yr == 1, age <= 20, min >= 800, season != CUR) |>
  filter(if_all(all_of(names(comp_feats)), ~ !is.na(.x)))
stopifnot(RILEY_ID %in% pool$player_id)
Z <- scale(as.matrix(pool[names(comp_feats)]))
r_i <- which(pool$player_id == RILEY_ID)
pool$dist <- sqrt(rowSums(sweep(Z, 2, Z[r_i, ])^2))
comps <- pool |> filter(player_id != RILEY_ID) |> arrange(dist) |> slice_head(n = 10)
pt(comps |> select(player_name, season, age, mpg, usg_pct, ts_rel, three_rel, ftr_rel, ft_pct, dist))

# which features make Riley distinctive inside the pool (his z-scores)
riley_z <- tibble(feat = names(comp_feats), label = comp_feats, z = Z[r_i, ]) |> arrange(desc(abs(z)))

# 2-D map of the pool for the constellation chart
pc <- prcomp(Z)
# principal components have no built-in sign: point the first one toward size (guards left, bigs right) and the second
# toward a bigger role (more of the team's plays, more minutes) so the plain-language axis names stay true on every run
pc_sign <- c(sign(cor(pc$x[, 1], pool$height_in)), sign(cor(pc$x[, 2], pool$usg_pct)))
pool <- pool |> mutate(pc1 = pc_sign[1] * pc$x[, 1], pc2 = pc_sign[2] * pc$x[, 2], is_comp = player_id %in% comps$player_id, is_riley = player_id == RILEY_ID)
# what the two directions mean: the measures that load most on each, and the players at each end
pc_load <- tibble(feat = names(comp_feats), label = comp_feats, pc1 = pc_sign[1] * pc$rotation[, 1], pc2 = pc_sign[2] * pc$rotation[, 2])
pt(pc_load |> arrange(desc(abs(pc1)))); pt(pc_load |> arrange(desc(abs(pc2))))
pc_ends <- list(left = pool |> arrange(pc1) |> slice_head(n = 5) |> pull(player_name), right = pool |> arrange(desc(pc1)) |> slice_head(n = 5) |> pull(player_name)
                , bottom = pool |> arrange(pc2) |> slice_head(n = 5) |> pull(player_name), top = pool |> arrange(desc(pc2)) |> slice_head(n = 5) |> pull(player_name)
                , riley_pc = c(pool$pc1[pool$is_riley], pool$pc2[pool$is_riley]), pc1_range = range(pool$pc1))
print(pc_ends)
# guard the names: right = taller, heavier, more rebounds and blocks; up = more of the team's plays, more minutes, more assists
stopifnot(all(pc_load$pc1[pc_load$feat %in% c("height_in", "weight", "oreb_pct", "blk100")] > 0.25), all(pc_load$pc2[pc_load$feat %in% c("usg_pct", "mpg", "ast_pct")] > 0.3)
          , cor(pool$pc1, pool$height_in) > 0.5, cor(pool$pc2, pool$usg_pct) > 0.5)
pc_var <- summary(pc)$importance[2, 1:2]

# what the comps did next: career years 1-6
futures <- ps |> filter(player_id %in% c(comps$player_id, RILEY_ID), career_yr <= 6) |>
  select(player_id, player_name, season, career_yr, age, gp, min, mpg, ts_rel, usg_pct, pie)
fut_summary <- futures |> filter(player_id != RILEY_ID) |> group_by(player_id, player_name) |>
  summarise(y1_mpg = mpg[career_yr == 1], y2_mpg = if (any(career_yr == 2)) mpg[career_yr == 2] else NA_real_
            , best_mpg_y2_5 = suppressWarnings(max(mpg[career_yr %in% 2:5 & gp >= 20])), y4_mpg = if (any(career_yr == 4)) mpg[career_yr == 4] else NA_real_
            , seasons_y2_5 = sum(career_yr %in% 2:5), .groups = "drop") |>
  mutate(best_mpg_y2_5 = if_else(is.finite(best_mpg_y2_5), best_mpg_y2_5, NA_real_))
pt(fut_summary)

# comps with at least one later season on record: rookie seasons through 2024-25 (Riley's own class is still in year two)
comps_f <- pool |> filter(player_id != RILEY_ID, sy <= season_year(PREV) - 1) |> arrange(dist) |> slice_head(n = 10)
pool_y2 <- pool |> filter(sy <= season_year(PREV) - 1) |> left_join(ps |> filter(career_yr == 2) |> select(player_id, mpg2 = mpg), by = "player_id")
pool_base <- list(n = sum(!is.na(pool_y2$mpg2)), share_10 = mean(pool_y2$mpg2 >= 10, na.rm = TRUE), lottery = mean(pool$draft_number <= 14, na.rm = TRUE)
                  , med_pick = median(pool$draft_number, na.rm = TRUE), riley_dist_rank = mean(pool$dist[pool$player_id != RILEY_ID] < max(comps_f$dist)))
classmates_comp <- pool |> filter(player_id != RILEY_ID, season == PREV) |> arrange(dist) |> slice_head(n = 3)
pool <- pool |> mutate(is_comp = player_id %in% comps_f$player_id)
p_constellation <- ggplot(pool, aes(pc1, pc2)) +
  geom_point(colour = "grey80", size = 2) +
  geom_segment(data = pool |> filter(is_comp) |> mutate(x0 = pool$pc1[pool$is_riley], y0 = pool$pc2[pool$is_riley]),
               aes(x = x0, y = y0, xend = pc1, yend = pc2), colour = pal[["teal"]], alpha = 0.5) +
  geom_point(data = \(d) filter(d, is_comp), colour = pal[["teal_dark"]], size = 3.6) +
  geom_text_repel(data = \(d) filter(d, is_comp), aes(label = paste0(player_name, " '", substr(season, 3, 4))), family = font, size = 4.2, colour = pal[["navy"]], seed = 202, box.padding = 0.5, min.segment.length = 0) +
  geom_point(data = \(d) filter(d, is_riley), colour = pal[["red"]], size = 6) +
  geom_text(data = \(d) filter(d, is_riley), aes(label = "Will Riley '25"), family = font, fontface = "bold", colour = pal[["red"]], size = 5, hjust = 1, nudge_x = -0.25) +
  labs(title = "Who he looked like as a rookie"
       , subtitle = sprintf("Each dot is a rookie year since 1996-97 by a player 20 or younger who played 800+ minutes (%d in all). Players who were alike sit close together. <span style='color:%s'>**Lines**</span> link Riley to the ten players most like him.", nrow(pool), pal[["teal_dark"]])
       , x = "Plays like a guard  \u2190     \u2192  Plays like a big man", y = "Smaller role  \u2190     \u2192  Ball in his hands more", caption = caption_text) +
  theme_wiz(16) + theme(axis.text = element_blank(), axis.title = element_text(colour = pal[["grey"]]))
save_plot("riley_constellation.png", p_constellation, width = 11, height = 9)

fut <- ps |> filter(player_id %in% c(comps_f$player_id, RILEY_ID), career_yr <= 6) |> select(player_id, player_name, career_yr, season, mpg, gp)
fut_end <- fut |> filter(player_id != RILEY_ID) |> group_by(player_id) |> slice_max(career_yr, n = 1) |> ungroup()
p_futures <- ggplot(fut |> filter(player_id != RILEY_ID), aes(career_yr, mpg, group = player_id)) +
  geom_line(colour = pal[["teal"]], alpha = 0.7, linewidth = 1) + geom_point(colour = pal[["teal"]], size = 2.2) +
  geom_text_repel(data = fut_end, aes(label = player_name), family = font, size = 4, colour = pal[["navy"]], hjust = 0, nudge_x = 0.25, direction = "y", seed = 202, segment.colour = "grey80") +
  geom_line(data = fut |> filter(player_id == RILEY_ID), colour = pal[["red"]], linewidth = 2) +
  geom_point(data = fut |> filter(player_id == RILEY_ID), colour = pal[["red"]], size = 5) +
  annotate("text", x = 2.12, y = fut$mpg[fut$player_id == RILEY_ID & fut$career_yr == 2], label = "Riley, this season", hjust = 0, family = font, fontface = "bold", colour = pal[["red"]], size = 5) +
  scale_x_continuous(breaks = 1:6, labels = c("Rookie", paste("Year", 2:6)), limits = c(1, 7.2)) + scale_y_continuous(limits = c(0, 38)) +
  labs(title = "Where his comps went"
       , subtitle = sprintf("Minutes per game, year by year, for the ten players whose rookie years looked most like Riley's. In year two, the fewest any of them played was %.1f minutes a game. Riley is at %.1f so far this season.", min(fut$mpg[fut$career_yr == 2 & fut$player_id != RILEY_ID]), fut$mpg[fut$player_id == RILEY_ID & fut$career_yr == 2])
       , x = NULL, y = "Minutes per game", caption = caption_text) +
  theme_wiz(16)
save_plot("riley_futures.png", p_futures, width = 11, height = 8.5)
comp_table <- comps_f |> left_join(fut |> filter(career_yr == 2) |> select(player_id, y2_mpg = mpg), by = "player_id") |>
  left_join(fut |> group_by(player_id) |> summarise(peak_mpg = max(mpg[gp >= 20 & career_yr %in% 2:6], -Inf), seasons = n()), by = "player_id") |>
  mutate(peak_mpg = if_else(is.finite(peak_mpg), peak_mpg, NA_real_)) |>
  select(player_name, season, draft_number, mpg, usg_pct, ts_rel, ft_pct, y2_mpg, peak_mpg, dist)

# ===========================================================================
# 4. The cut: what teams usually do in year two with a rookie like this
# ===========================================================================
team <- map_dfr(hist_seasons, \(s) rd(sprintf("hist/team_%s.csv", s)) |> transmute(team_id, season = s, team_net = net_rating, team_gp = gp, team_w = w))
# a rookie's teams in years one and two are the teams nba.com lists him on at season's end
y1 <- ps |> filter(career_yr == 1, age <= 21, min >= 800) |>
  transmute(player_id, player_name, season1 = season, sy1 = sy, age1 = age, mpg1 = mpg, pie1 = pie, ts_rel1 = ts_rel, usg1 = usg_pct
            , draft = if_else(is.na(draft_number), 61L, draft_number), team1 = team_id)
y2 <- ps |> filter(career_yr == 2) |> transmute(player_id, season2 = season, mpg2 = mpg, gp2 = gp, team2 = team_id)
yy <- y1 |> inner_join(y2, by = "player_id") |>
  left_join(team |> select(team1 = team_id, season1 = season, net1 = team_net), by = c("team1", "season1")) |>
  left_join(team |> select(team2 = team_id, season2 = season, net2 = team_net), by = c("team2", "season2")) |>
  mutate(log_draft = log(draft), d_net = net2 - net1)
# fit on completed second seasons with 10+ games; the 2025-26 rookies (Riley's class) are still in year two
fit_set <- yy |> filter(season2 != CUR, gp2 >= 10) |> filter(if_all(c(mpg1, pie1, ts_rel1, usg1, age1, log_draft, net1, d_net), ~ !is.na(.x)))
class25 <- yy |> filter(season1 == PREV)
riley_yy <- class25 |> filter(player_id == RILEY_ID)
stopifnot(nrow(riley_yy) == 1)
xvars <- c(mpg1 = "Rookie minutes per game", pie1 = "Rookie PIE", ts_rel1 = "Rookie true shooting vs league", usg1 = "Rookie usage"
           , age1 = "Age as a rookie", log_draft = "Draft slot (log)", net1 = "Team net rating, year one", d_net = "Team net rating change")
ctr <- colMeans(fit_set[names(xvars)]); scl <- apply(fit_set[names(xvars)], 2, sd)
std <- function(d) sweep(sweep(as.matrix(d[names(xvars)]), 2, ctr), 2, scl, "/")
X <- std(fit_set); Xp <- std(class25)
stopifnot(kappa(X, exact = TRUE) < 30)

# prior predictive: minutes per game the priors alone would generate for an average rookie
pp_min <- local({ n <- 4000; a <- rnorm(n, 20, 8); s <- abs(rnorm(n, 0, 8)); nu <- rgamma(n, 2, 0.1)
  b <- matrix(rnorm(n * ncol(X), 0, 4), n); i <- sample(nrow(X), n, TRUE); mu <- a + rowSums(b * X[i, ]); mu + s * rt(n, nu) })
prior_check_min <- quantile(pp_min, c(0.005, 0.5, 0.995)); obs_range_min <- range(fit_set$mpg2)

mod_min <- cmdstan_model(write_stan_file(minutes_stan, dir = "stan", basename = "riley_minutes"))
fit_min <- mod_min$sample(data = list(N = nrow(X), K = ncol(X), X = X, y = fit_set$mpg2, M = nrow(Xp), Xp = Xp, prior_only = 0)
                          , seed = 202, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1500, adapt_delta = 0.99, refresh = 0, show_messages = FALSE)
diag_min <- check_fit(fit_min, c("alpha", "beta", "sigma", "nu", "mu_p", "y_p"), "minutes")
dr_min <- fit_min$draws(format = "draws_matrix")
beta_min <- tibble(var = names(xvars), label = xvars, draws = map(seq_along(xvars), \(k) as.numeric(dr_min[, sprintf("beta[%d]", k)]))) |>
  mutate(mean = map_dbl(draws, mean), lo = map_dbl(draws, quantile, 0.05), hi = map_dbl(draws, quantile, 0.95))
pt(beta_min |> select(label, mean, lo, hi))
r_k <- which(class25$player_id == RILEY_ID)
riley_pred_mu <- as.numeric(dr_min[, sprintf("mu_p[%d]", r_k)])
riley_pred_y <- as.numeric(dr_min[, sprintf("y_p[%d]", r_k)])
riley_now_mpg <- riley_yy$mpg2
min_pred <- list(mu_med = median(riley_pred_mu), mu_lo = quantile(riley_pred_mu, 0.05), mu_hi = quantile(riley_pred_mu, 0.95)
                 , y_lo = quantile(riley_pred_y, 0.05), y_hi = quantile(riley_pred_y, 0.95), p_below = mean(riley_pred_y <= riley_now_mpg)
                 , now = riley_now_mpg, now_gp = riley_yy$gp2)
# how often a rookie like this lost this much: share of the fit set whose year-two minutes fell below Riley's current mark
hist_cut <- list(n = nrow(fit_set), n_18 = sum(fit_set$mpg1 >= 18), below_now = sum(fit_set$mpg2 <= riley_now_mpg)
                 , below_now_18 = sum(fit_set$mpg1 >= 18 & fit_set$mpg2 <= riley_now_mpg), min_ratio = min(fit_set$mpg2 / fit_set$mpg1)
                 , riley_ratio = riley_now_mpg / riley_yy$mpg1, lowest = fit_set |> arrange(mpg2 / mpg1) |> slice_head(n = 3) |> select(player_name, season1, mpg1, mpg2))
# posterior predictive check: the spread and the low tail of year-two minutes
yrep_min <- dr_min[, grep("^y_rep", colnames(dr_min))]
ppc_min <- tibble(stat = c("Median", "10th percentile", "Share under 10 mpg"),
                  obs = c(median(fit_set$mpg2), quantile(fit_set$mpg2, 0.1), mean(fit_set$mpg2 < 10)),
                  rep_lo = c(quantile(apply(yrep_min, 1, median), 0.05), quantile(apply(yrep_min, 1, quantile, 0.1), 0.05), quantile(rowMeans(yrep_min < 10), 0.05)),
                  rep_hi = c(quantile(apply(yrep_min, 1, median), 0.95), quantile(apply(yrep_min, 1, quantile, 0.1), 0.95), quantile(rowMeans(yrep_min < 10), 0.95)))
pt(ppc_min)
# lower-tail calibration in sample: how many players fall below the model's 1st / 5th / 10th percentile
pit <- map_dbl(seq_len(nrow(fit_set)), \(i) mean(yrep_min[, i] <= fit_set$mpg2[i]))
pit_tail <- tibble(cut = c(0.01, 0.05, 0.10), observed = map_int(cut, \(q) sum(pit < q)), expected = cut * nrow(fit_set))

# Riley's mark is only as many games old as the Wizards have played; the model predicts full seasons. How far do
# second-year players' first N team games (N = Wizards games so far) sit from their full season? Measured on last season's second-year players from the same cohort, then added to
# the model's prediction for Riley.
gl_prev <- rd(sprintf("gamelog_%s.csv", PREV))
n_early <- rd(sprintf("gamelog_%s.csv", CUR)) |> filter(team_id == WIZ_ID) |> distinct(game_id) |> nrow()
g14 <- gl_prev |> distinct(team_id, game_id, game_date) |> arrange(team_id, game_date) |> group_by(team_id) |>
  summarise(date14 = game_date[min(n_early, n())])
early <- gl_prev |> left_join(g14, by = "team_id") |> filter(game_date <= date14) |>
  group_by(player_id) |> summarise(gp14 = n(), mpg14 = mean(min), .groups = "drop") |>
  inner_join(yy |> filter(season2 == PREV) |> select(player_id, player_name, mpg1, mpg2), by = "player_id") |>
  filter(gp14 >= 5) |> mutate(dev = mpg14 - mpg2)
set.seed(202)
riley_pred_early <- riley_pred_y + sample(early$dev, length(riley_pred_y), replace = TRUE)
min_pred$p_below_early <- mean(riley_pred_early <= riley_now_mpg)
min_pred$early_lo <- quantile(riley_pred_early, 0.05); min_pred$early_hi <- quantile(riley_pred_early, 0.95)
early_sum <- list(n = nrow(early), mean = mean(early$dev), sd = sd(early$dev), biggest = early |> arrange(dev) |> slice_head(n = 3) |> select(player_name, mpg14, mpg2))
# classmates already at or below Riley's mark (the 2025 class is in year two now, so it is not in the fit set)
class_low <- class25 |> filter(player_id != RILEY_ID, gp2 >= 5, mpg2 <= riley_now_mpg + 0.5) |> select(player_name, mpg1, mpg2, gp2)
pt(early |> arrange(dev)); str(min_pred); pt(class_low); pt(pit_tail)
# the model's expectation for every 2025 classmate, to check whether the squeeze is team-wide (Tre Johnson is on the same team)
class_pred <- class25 |> mutate(pred = map_dbl(seq_len(nrow(class25)), \(k) median(dr_min[, sprintf("mu_p[%d]", k)]))) |> select(player_name, mpg1, mpg2, gp2, pred)
# sensitivity of "none finished this low" to the age cutoff: full second seasons below Riley's mark at 22 or younger
age22_low <- ps |> filter(career_yr == 1, age <= 22, min >= 800) |> select(player_id, player_name, age, mpg1 = mpg, season) |>
  inner_join(ps |> filter(career_yr == 2, season != CUR, gp >= 10) |> select(player_id, mpg2 = mpg), by = "player_id") |> filter(mpg2 <= riley_now_mpg)
pt(class_pred); pt(age22_low)

classmates <- class25 |> filter(player_id != RILEY_ID, gp2 >= 5)
p_cliff <- ggplot(fit_set, aes(mpg1, mpg2)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey75", linetype = "dashed") +
  annotate("text", x = 34.5, y = 35.8, label = "Same minutes as a rookie", family = font, colour = "grey55", size = 4.2, hjust = 1) +
  geom_point(colour = pal[["teal"]], alpha = 0.45, size = 2.4) +
  geom_point(data = classmates, shape = 21, colour = pal[["navy"]], fill = "white", size = 3, stroke = 1) +
  geom_text_repel(data = classmates |> filter(player_name %in% c("Drake Powell", "Tre Johnson")), aes(label = player_name), family = font, size = 4, colour = pal[["navy"]], seed = 202, min.segment.length = 0, nudge_y = -3.5, nudge_x = -1.5) +
  geom_text_repel(data = fit_set |> arrange(mpg2 / mpg1) |> slice_head(n = 2), aes(label = paste(player_name, substr(season2, 1, 4))), family = font, size = 3.8, colour = pal[["grey"]], seed = 202, nudge_y = -3, nudge_x = -2.5, min.segment.length = 0) +
  annotate("segment", x = riley_yy$mpg1, xend = riley_yy$mpg1, y = min_pred$y_lo, yend = min_pred$y_hi, colour = pal[["red"]], linewidth = 5, alpha = 0.25) +
  annotate("segment", x = riley_yy$mpg1, xend = riley_yy$mpg1, y = min_pred$mu_lo, yend = min_pred$mu_hi, colour = pal[["red"]], linewidth = 5, alpha = 0.6) +
  geom_point(data = riley_yy, colour = pal[["red"]], size = 6) +
  annotate("text", x = riley_yy$mpg1 + 0.8, y = riley_yy$mpg2, label = sprintf("Will Riley: %.1f mpg so far", riley_yy$mpg2), hjust = 0, family = font, fontface = "bold", colour = pal[["red"]], size = 5.2) +
  annotate("text", x = riley_yy$mpg1 + 0.8, y = min_pred$mu_hi + 1.5, label = sprintf("What the model expects\nfor a rookie like him: %.0f mpg", min_pred$mu_med), hjust = 0, family = font, colour = pal[["red"]], size = 4.4, lineheight = 0.95) +
  scale_x_continuous(limits = c(10, 36)) + scale_y_continuous(limits = c(0, 38)) +
  labs(title = "No full second season has gone this low"
       , subtitle = sprintf("Minutes per game as a rookie (across) and the next year (up), for all %d players since 1996-97 who were 21 or younger as rookies and played 800+ minutes (teal). Open circles: Riley's 2025 draft classmates so far this season. <span style='color:%s'>**Red bar**</span>: what our model expects for a full season from a player like Riley. Dark is the most likely range; light covers 9 in 10 players.", nrow(fit_set), pal[["red"]])
       , x = "Minutes per game as a rookie", y = "Minutes per game in year two", caption = caption_text) +
  theme_wiz(16)
save_plot("riley_cliff.png", p_cliff, width = 11, height = 9.5)

# ===========================================================================
# 5. The jumper: what an 80% free-throw shooter's 31.6% from three usually becomes
# ===========================================================================
# Young players: first season at 21 or younger with 500+ minutes and 20+ free throws. Their rookie free throws and
# threes, and their threes in career years 2-4 (whatever of those have happened). League offsets are attempt-weighted.
lgx <- lg |> select(season, lg_3p, lg_ft)
sh_rook <- ps |> filter(career_yr == 1, age <= 21, min >= 500, fta >= 20) |>
  transmute(player_id, player_name, season, ftm, fta, m1 = fg3m, a1 = fg3a, off_ft = qlogis(lg_ft), off1 = qlogis(lg_3p))
sh_fut <- ps |> filter(career_yr %in% 2:4, player_id %in% sh_rook$player_id) |>
  group_by(player_id) |> summarise(mf = sum(fg3m), af = sum(fg3a), offf = if (sum(fg3a) > 0) qlogis(weighted.mean(lg_3p, fg3a)) else qlogis(mean(lg_3p)), n_fut = n(), .groups = "drop")
sh <- sh_rook |> left_join(sh_fut, by = "player_id") |>
  mutate(mf = replace_na(mf, 0L), af = replace_na(af, 0L), offf = coalesce(offf, qlogis(lg$lg_3p[lg$season == CUR])), n_fut = replace_na(n_fut, 0L)) |>
  mutate(across(c(ftm, fta, m1, a1, mf, af), as.integer))
stopifnot(RILEY_ID %in% sh$player_id)
r_s <- which(sh$player_id == RILEY_ID)
sh_data <- list(N = nrow(sh), ftm = sh$ftm, fta = sh$fta, m1 = sh$m1, a1 = sh$a1, mf = sh$mf, af = sh$af
                , off_ft = sh$off_ft, off1 = sh$off1, offf = sh$offf, prior_only = 0)

# prior predictive: implied three-point percentages for a player at a league-average offset
pp_3 <- local({ n <- 4000; mu <- rnorm(n, 0, 0.5); tau <- abs(rnorm(n, 0, 0.5)); plogis(qlogis(0.355) + mu + tau * rnorm(n)) })
prior_check_3 <- quantile(pp_3, c(0.005, 0.5, 0.995))

mod_sh <- cmdstan_model(write_stan_file(shoot_stan, dir = "stan", basename = "riley_shooting"))
fit_sh <- mod_sh$sample(data = sh_data, seed = 202, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 2500
                        , adapt_delta = 0.99, refresh = 0, show_messages = FALSE)
sh_gate <- c("mu", "tau", sprintf("theta[%d,%d]", r_s, 1:3))
diag_sh <- check_fit(fit_sh, sh_gate, "shooting", extra = c("Omega[2,1]", "Omega[3,1]", "Omega[3,2]"))
dr_sh <- fit_sh$draws(format = "draws_matrix")
lg3_cur <- lg$lg_3p[lg$season == CUR]; lg3_prev <- lg$lg_3p[lg$season == PREV]
r_t1 <- as.numeric(dr_sh[, sprintf("theta[%d,2]", r_s)]); r_tf <- as.numeric(dr_sh[, sprintf("theta[%d,3]", r_s)]); r_ft <- as.numeric(dr_sh[, sprintf("theta[%d,1]", r_s)])
shoot_out <- list(
  rookie_obs = sh$m1[r_s] / sh$a1[r_s], rookie_att = sh$a1[r_s], ft_obs = sh$ftm[r_s] / sh$fta[r_s], fta = sh$fta[r_s]
  , fut_att = sh$af[r_s], fut_makes = sh$mf[r_s], lg3_prev = lg3_prev, lg3_cur = lg3_cur
  , rookie_true = quantile(plogis(qlogis(lg3_prev) + r_t1), c(0.05, 0.5, 0.95))
  , future_true = quantile(plogis(qlogis(lg3_cur) + r_tf), c(0.05, 0.25, 0.5, 0.75, 0.95))
  , p_future_above_lg = mean(r_tf > 0), p_future_above_rookie_obs = mean(plogis(qlogis(lg3_cur) + r_tf) > sh$m1[r_s] / sh$a1[r_s])
  , rho_ft_future = quantile(as.numeric(dr_sh[, "Omega[3,1]"]), c(0.05, 0.5, 0.95))
  , rho_rookie_future = quantile(as.numeric(dr_sh[, "Omega[3,2]"]), c(0.05, 0.5, 0.95))
  , rho_ft_rookie = quantile(as.numeric(dr_sh[, "Omega[2,1]"]), c(0.05, 0.5, 0.95))
  , n_players = nrow(sh), n_with_future = sum(sh$af >= 50))
str(shoot_out)

# the forecast for a player with Riley's rookie threes (78 of 247) and 180 free throws at each free-throw %: condition the
# fitted population on those observed rates, with their binomial measurement error, and draw the years 2-4 talent
lgft_prev <- lg$lg_ft[lg$season == PREV]
cond_draws <- function(ft) {
  y <- c(qlogis(ft) - qlogis(lgft_prev), qlogis(shoot_out$rookie_obs) - qlogis(lg3_prev))
  R <- diag(c(1 / (sh$fta[r_s] * ft * (1 - ft)), 1 / (sh$a1[r_s] * shoot_out$rookie_obs * (1 - shoot_out$rookie_obs))))
  mu <- unclass(dr_sh[, c("mu[1]", "mu[2]", "mu[3]")]); tau <- unclass(dr_sh[, c("tau[1]", "tau[2]", "tau[3]")])
  map_dbl(seq_len(nrow(dr_sh)), \(d) {
    O <- matrix(c(1, dr_sh[d, "Omega[2,1]"], dr_sh[d, "Omega[3,1]"], dr_sh[d, "Omega[2,1]"], 1, dr_sh[d, "Omega[3,2]"], dr_sh[d, "Omega[3,1]"], dr_sh[d, "Omega[3,2]"], 1), 3)
    S <- diag(as.numeric(tau[d, ])) %*% O %*% diag(as.numeric(tau[d, ]))
    K <- S[3, 1:2] %*% solve(S[1:2, 1:2] + R)
    m <- as.numeric(mu[d, 3] + K %*% (y - as.numeric(mu[d, 1:2]))); v <- as.numeric(S[3, 3] - K %*% S[1:2, 3])
    plogis(qlogis(lg3_cur) + rnorm(1, m, sqrt(v)))
  })
}
set.seed(202)
ft_grid <- tibble(ft = seq(0.6, 0.9, length.out = 16)) |>
  mutate(draws = map(ft, cond_draws), med = map_dbl(draws, median), lo = map_dbl(draws, quantile, 0.05), hi = map_dbl(draws, quantile, 0.95)) |> select(-draws)
# check: at his own free-throw %, the conditional forecast must match the fitted posterior (it ignores only his 12 year-two threes)
stopifnot(abs(approx(ft_grid$ft, ft_grid$med, shoot_out$ft_obs)$y - shoot_out$future_true[3]) < 0.006)
shoot_out$at_65 <- approx(ft_grid$ft, ft_grid$med, 0.65)$y; shoot_out$at_90 <- approx(ft_grid$ft, ft_grid$med, 0.90)$y

# posterior predictive check: replicated vs observed future three-point % by free-throw band (players with 100+ future attempts)
mf_rep <- dr_sh[, grep("^mf_rep", colnames(dr_sh))]
ppc_sh <- sh |> mutate(i = row_number(), ft_band = cut(ftm / fta, c(0, 0.65, 0.72, 0.78, 0.84, 1), labels = c("<65%", "65-72%", "72-78%", "78-84%", "84%+"))) |>
  filter(af >= 100) |> group_by(ft_band) |>
  summarise(n = n(), obs = sum(mf) / sum(af), rep = list(rowSums(mf_rep[, i, drop = FALSE]) / sum(af)), .groups = "drop") |>
  mutate(rep_lo = map_dbl(rep, quantile, 0.05), rep_hi = map_dbl(rep, quantile, 0.95)) |> select(-rep)
pt(ppc_sh)

band_obs <- sh |> filter(af >= 100) |> mutate(ft = ftm / fta, fut3 = mf / af)
p_shoot <- ggplot() +
  geom_point(data = band_obs, aes(ft, fut3), colour = pal[["teal"]], alpha = 0.35, size = 2) +
  geom_ribbon(data = ft_grid, aes(ft, ymin = lo, ymax = hi), fill = pal[["navy"]], alpha = 0.15) +
  geom_line(data = ft_grid, aes(ft, med), colour = pal[["navy"]], linewidth = 1.2) +
  geom_hline(yintercept = lg3_cur, linetype = "dashed", colour = "grey55") +
  annotate("text", x = 0.6, y = lg3_cur + 0.006, label = sprintf("League average this season, %s", percent(lg3_cur, 0.1)), hjust = 0, family = font, colour = "grey45", size = 4.2) +
  annotate("point", x = shoot_out$ft_obs, y = shoot_out$rookie_obs, shape = 4, size = 5, stroke = 1.6, colour = pal[["red"]]) +
  annotate("text", x = shoot_out$ft_obs + 0.006, y = shoot_out$rookie_obs - 0.006, label = sprintf("His rookie threes: %s", percent(shoot_out$rookie_obs, 0.1)), hjust = 0, vjust = 1, family = font, colour = pal[["red"]], size = 4.4) +
  annotate("errorbar", x = shoot_out$ft_obs, ymin = shoot_out$future_true[1], ymax = shoot_out$future_true[5], colour = pal[["red"]], width = 0.008, linewidth = 1.2) +
  annotate("point", x = shoot_out$ft_obs, y = shoot_out$future_true[3], colour = pal[["red"]], size = 5.5) +
  annotate("text", x = shoot_out$ft_obs + 0.008, y = shoot_out$future_true[3] + 0.004, label = sprintf("Riley's forecast: %s", percent(shoot_out$future_true[3], 0.1)), hjust = 0, vjust = 0, family = font, fontface = "bold", colour = pal[["red"]], size = 4.8) +
  scale_x_continuous(labels = percent_format(1), limits = c(0.55, 0.95)) + scale_y_continuous(labels = percent_format(1)) +
  coord_cartesian(ylim = c(0.2, 0.45)) +
  labs(title = "The jumper is the open question"
       , subtitle = sprintf("Each dot is a player who was 21 or younger as a rookie: his free-throw %% as a rookie (across) and his three-point %% in years 2-4 (up). The <span style='color:%s'>**navy band**</span> is our forecast for a player who shot threes like Riley did as a rookie (31.6%% on 247 tries), for each free-throw %%. Making free throws helps. But even a 90%% free-throw shooter stays below the league average.", pal[["navy"]])
       , x = "Free-throw % as a rookie", y = "Three-point % in career years 2-4", caption = caption_text) +
  theme_wiz(16)
save_plot("riley_shooting.png", p_shoot, width = 11, height = 9)

# ===========================================================================
# 6. This season: who plays, and when Riley's minutes come
# ===========================================================================
box <- function(s) read_csv(file.path(DATA_DIR, sprintf("boxscores_%s.csv", s)), col_types = cols(gameId = col_character(), minutes = col_character(), .default = col_guess())) |>
  clean_names() |> filter(team_id == WIZ_ID) |>
  mutate(min = suppressWarnings(as.numeric(str_extract(minutes, "^\\d+")) + as.numeric(str_extract(minutes, "(?<=:)\\d+")) / 60) |> replace_na(0)
         , comment = replace_na(comment, ""), season = s)
bx <- bind_rows(box(PREV), box(CUR))
n_games_cur <- n_distinct(bx$game_id[bx$season == CUR])
rot_now <- bx |> filter(season == CUR) |> group_by(person_id, name) |>
  summarise(gp = sum(min > 0), min = sum(min), dnp_cd = sum(str_detect(comment, "Coach")), out = sum(str_detect(comment, "Inactive|Injury|Not With")), starts = sum(starter), .groups = "drop") |>
  mutate(mpg = if_else(gp > 0, min / gp, 0)) |> arrange(desc(min))   # minutes per game played; games he sat do not count
pt(rot_now)

# stints and the score, rebuilt from the cdn.nba.com play-by-play: a player is on the floor at the start of a period if his
# first action in it is anything but a substitution in; substitutions open and close stints; every action carries the score
pbp <- bind_rows(map(c(PREV, CUR), \(s) read_csv(file.path(DATA_DIR, sprintf("pbp_%s.csv", s)), col_types = cols(gameId = col_character(), descriptor = col_character(), foulDrawnPersonId = col_double()
                                                                                                   , jumpBallWonPersonId = col_double(), jumpBallLostPersonId = col_double(), .default = col_guess()), show_col_types = FALSE) |>
  clean_names() |> mutate(season = s))) |>
  mutate(rem = as.numeric(str_extract(clock, "(?<=PT)\\d+")) * 60 + as.numeric(str_extract(clock, "(?<=M)[\\d.]+"))
         , p_start = if_else(period <= 4, (period - 1) * 720, 2880 + (period - 5) * 300), p_len = if_else(period <= 4, 720, 300)
         , t = p_start + p_len - rem) |> arrange(season, game_id, order_number)
wiz_home <- bx |> distinct(game_id, home)
build_stints <- function(g) {
  d <- pbp |> filter(game_id == g)
  home <- wiz_home$home[wiz_home$game_id == g][1]
  # technical fouls and ejections can be charged to players on the bench (or coaches), so they say nothing about who is on
  w <- d |> filter(team_id == WIZ_ID, !is.na(person_id), person_id > 0, !(action_type == "foul" & sub_type == "technical"), action_type != "ejection")
  out <- list()
  for (per in sort(unique(d$period))) {
    pd <- w |> filter(period == per); p0 <- d$p_start[d$period == per][1]; p1 <- p0 + d$p_len[d$period == per][1]
    first <- pd |> group_by(person_id) |> slice_head(n = 1) |> ungroup()
    on <- set_names(rep(p0, sum(!(first$action_type == "substitution" & first$sub_type == "in"))), first$person_id[!(first$action_type == "substitution" & first$sub_type == "in")])
    # a player can go a whole (short) period without a recorded action: if fewer than five are on, carry over players who
    # finished the previous period on the floor and have no action at all in this one
    if (length(on) < 5 && exists("last_on", inherits = FALSE)) {
      carry <- setdiff(last_on, as.character(first$person_id))[seq_len(min(5 - length(on), length(setdiff(last_on, as.character(first$person_id)))))]
      for (pid in carry) on[[pid]] <- p0
    }
    subs <- pd |> filter(action_type == "substitution")
    for (i in seq_len(nrow(subs))) {
      pid <- as.character(subs$person_id[i])
      if (subs$sub_type[i] == "out" && pid %in% names(on)) { out[[length(out) + 1]] <- tibble(person_id = as.numeric(pid), t_in = on[[pid]], t_out = subs$t[i]); on <- on[names(on) != pid] }
      if (subs$sub_type[i] == "in" && !pid %in% names(on)) on[[pid]] <- subs$t[i]
    }
    for (pid in names(on)) out[[length(out) + 1]] <- tibble(person_id = as.numeric(pid), t_in = on[[pid]], t_out = p1)
    stopifnot(length(on) == 5)
    last_on <- names(on)
  }
  st <- bind_rows(out) |> filter(t_out > t_in) |> mutate(game_id = g, season = d$season[1])
  # the score at every second: Wizards minus opponent
  sc <- d |> filter(!is.na(score_home)) |> select(t, score_home, score_away)
  secs <- seq(0.5, max(d$t), by = 1)
  idx <- findInterval(secs, sc$t)
  marg <- if_else(idx == 0, 0, (sc$score_home[pmax(idx, 1)] - sc$score_away[pmax(idx, 1)]) * if_else(home, 1, -1))
  # garbage time, after Cleaning the Glass: in the fourth quarter, a lead of 25+ with 12-9 minutes left, 20+ with 9-6, 10+ under 6
  left <- 2880 - secs
  garbage <- secs >= 2160 & secs < 2880 & ((left > 540 & abs(marg) >= 25) | (left > 360 & left <= 540 & abs(marg) >= 20) | (left <= 360 & abs(marg) >= 10))
  in_st <- function(a, b, cond) sum(cond[secs >= a & secs < b])
  st |> mutate(sec = t_out - t_in
               , sec_close10 = map2_dbl(t_in, t_out, \(a, b) in_st(a, b, abs(marg) <= 10))
               , sec_close15 = map2_dbl(t_in, t_out, \(a, b) in_st(a, b, abs(marg) <= 15))
               , sec_garbage = map2_dbl(t_in, t_out, \(a, b) in_st(a, b, garbage))
               , sec_q1 = map2_dbl(t_in, t_out, \(a, b) in_st(a, b, secs < 720))
               , sec_down10 = map2_dbl(t_in, t_out, \(a, b) in_st(a, b, marg < -10)))
}
stint_state <- map_dfr(unique(pbp$game_id), build_stints) |> left_join(bx |> arrange(desc(season)) |> distinct(person_id, .keep_all = TRUE) |> select(person_id, name), by = "person_id")
state_by_player <- stint_state |> group_by(season, person_id, name) |>
  summarise(min = sum(sec) / 60, close10 = sum(sec_close10) / 60, close15 = sum(sec_close15) / 60, garbage = sum(sec_garbage) / 60
            , q1 = sum(sec_q1) / 60, down10 = sum(sec_down10) / 60, .groups = "drop") |>
  mutate(share_close10 = close10 / min, share_close15 = close15 / min, share_garbage = garbage / min, share_q1 = q1 / min, share_down10 = down10 / min)
# the stints file and the box score must agree on minutes, or the game-state split is built on the wrong clock
chk_game <- stint_state |> group_by(season, game_id, person_id) |> summarise(pbp_min = sum(sec) / 60, .groups = "drop") |>
  full_join(bx |> filter(min > 0) |> select(season, game_id, person_id, box_min = min), by = c("season", "game_id", "person_id")) |>
  mutate(across(c(pbp_min, box_min), ~ replace_na(.x, 0)), d = pbp_min - box_min)
pt(chk_game |> arrange(desc(abs(d))) |> head(5))
stopifnot(max(abs(chk_game$d)) < 1)
riley_state <- state_by_player |> filter(person_id == RILEY_ID)
# the rest of this season's rotation (100+ minutes), for comparison
rot_garbage <- state_by_player |> filter(season == CUR, min >= 100, person_id != RILEY_ID) |> summarise(max_garbage = max(share_garbage), max_name = name[which.max(share_garbage)])
# the two games AJ Dybantsa missed: who started, and what Riley got
dyb_out <- bx |> filter(season == CUR, person_id == 1643407, str_detect(comment, "Inactive|Injury")) |> pull(game_id)
wiz_order <- rd(sprintf("gamelog_%s.csv", CUR)) |> filter(team_id == WIZ_ID) |> distinct(game_id = as.character(game_id), game_date, matchup) |> arrange(game_date) |> mutate(game_no = row_number())
dyb_games <- bx |> filter(season == CUR, game_id %in% dyb_out, person_id %in% c(RILEY_ID, 1641731)) |> select(game_id, name, starter, min, comment) |>
  left_join(wiz_order, by = "game_id")
pt(dyb_games)
team_close <- stint_state |> group_by(season) |> summarise(share_close10 = sum(sec_close10) / sum(sec))
pt(riley_state); pt(team_close)

now_plot <- rot_now |> filter(min > 0) |> left_join(state_by_player |> filter(season == CUR) |> select(person_id, garbage), by = "person_id") |>
  mutate(garbage = replace_na(garbage, 0), garb_pg = garbage / gp, real_pg = mpg - garb_pg, name = fct_reorder(name, mpg), is_r = person_id == RILEY_ID) |>
  filter(mpg >= 1)
p_now <- now_plot |> pivot_longer(c(real_pg, garb_pg), names_to = "state", values_to = "m") |>
  mutate(state = factor(state, c("garb_pg", "real_pg"), c("Garbage time", "Competitive"))) |>
  ggplot(aes(m, name, fill = interaction(state, is_r))) +
  geom_col(width = 0.7) +
  geom_text(data = now_plot, aes(x = mpg + 0.5, y = name, label = sprintf("%.1f in %d %s  (%s garbage time)", mpg, gp, if_else(gp == 1, "game", "games"), percent(garbage / min, 1))), inherit.aes = FALSE, hjust = 0, family = font, size = 4.2, colour = pal[["grey"]]) +
  scale_fill_manual(values = c("Garbage time.FALSE" = "#C9D9E8", "Competitive.FALSE" = pal[["navy"]], "Garbage time.TRUE" = "#F2B8BB", "Competitive.TRUE" = pal[["red"]]), guide = "none") +
  scale_x_continuous(limits = c(0, 56), breaks = seq(0, 40, 10), expand = expansion(mult = c(0, 0))) +
  labs(title = sprintf("The %s man, stuck in garbage time", ord_word(sum(rot_now$mpg > rot_now$mpg[rot_now$person_id == RILEY_ID]) + 1))
       , subtitle = sprintf("Minutes per game for each Wizard this season, counting only the games he played (the team has played %d). Light: garbage time, late in games that are already decided. Dark: everything else.", n_games_cur)
       , x = "Minutes per game played", y = NULL, caption = caption_text) +
  theme_wiz(16) + theme(axis.text.y = element_text(colour = "black"))
save_plot("riley_rotation_now.png", p_now, width = 11, height = 8.5)

# ===========================================================================
# 7. Riley against the bench players ahead of him, on the same team
# ===========================================================================
bench_rivals <- c("Will Riley", "Tre Johnson", "Bub Carrington", "Bilal Coulibaly")
rival_ids <- ps |> filter(season == CUR, player_name %in% bench_rivals, team_id == WIZ_ID) |> distinct(player_id, player_name)
stopifnot(nrow(rival_ids) == length(bench_rivals))
rival_box <- ps |> filter(player_id %in% rival_ids$player_id, season %in% c(PREV, CUR)) |>
  select(player_name, season, gp, min, mpg, usg_pct, ts, ts_rel, ast_pct, tm_tov_pct, dreb_pct, stl100, blk100, pie, ft_rate, three_rate)
# on/off per 48 minutes from box scores, bootstrapped over games so the noise is visible
team_margin <- bx |> group_by(season, game_id) |> summarise(margin = sum(plus_minus) / 5, game_min = sum(min) / 5, .groups = "drop")
onoff_boot <- function(pid, s, B = 4000) {
  d <- bx |> filter(person_id == pid, season == s) |> select(game_id, on_min = min, on_pm = plus_minus) |>
    inner_join(team_margin |> filter(season == s), by = "game_id") |>
    mutate(off_min = game_min - on_min, off_pm = margin - on_pm)
  est <- function(x) sum(x$on_pm) / sum(x$on_min) * 48 - sum(x$off_pm) / sum(x$off_min) * 48
  on_est <- function(x) sum(x$on_pm) / sum(x$on_min) * 48
  bs <- replicate(B, { x <- d[sample(nrow(d), replace = TRUE), ]; c(est(x), on_est(x)) })
  tibble(person_id = pid, season = s, on_min = sum(d$on_min), on48 = on_est(d), onoff = est(d)
         , lo = quantile(bs[1, ], 0.05), hi = quantile(bs[1, ], 0.95), se = sd(bs[1, ]), on_lo = quantile(bs[2, ], 0.05), on_hi = quantile(bs[2, ], 0.95))
}
set.seed(202)
rival_onoff <- expand_grid(person_id = rival_ids$player_id, season = c(PREV, CUR)) |> pmap_dfr(\(person_id, season) onoff_boot(person_id, season)) |>
  left_join(rival_ids, by = c("person_id" = "player_id"))
pt(rival_onoff)
# how each of the four got his offense last season (Synergy share of plays)
rival_pt <- syn |> filter(player_id %in% rival_ids$player_id, play_type %in% c("PRBallHandler", "Spotup", "Isolation", "Transition")) |>
  select(player_name, play_type, poss, poss_pct, ppp) |> arrange(play_type, desc(poss_pct))
pt(rival_pt)
p_rivals <- rival_onoff |> mutate(player_name = factor(player_name, rev(bench_rivals)), season = factor(season, c(PREV, CUR))) |>
  ggplot(aes(onoff, player_name, colour = player_name == "Will Riley")) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 2.2, alpha = 0.35, orientation = "y") +
  geom_point(size = 5) +
  geom_text(aes(label = sprintf("%+.1f  ·  %s min", onoff, comma(round(on_min)))), nudge_y = 0.32, family = font, size = 4.2) +
  facet_wrap(~season, ncol = 2) +
  scale_colour_manual(values = c(`TRUE` = pal[["red"]], `FALSE` = pal[["navy"]]), guide = "none") +
  labs(title = "On/off can't tell these four apart"
       , subtitle = "How much better (right) or worse (left) the Wizards did with each player on the floor than with him off it, in points per 48 minutes. The bars show how much this can swing by luck. It doesn't account for teammates or opponents."
       , x = "Team points per 48 minutes, on the floor minus off", y = NULL, caption = caption_text) +
  theme_wiz(16) + theme(axis.text.y = element_text(colour = "black"), panel.spacing = unit(2, "lines"))
save_plot("riley_rivals.png", p_rivals, width = 12, height = 7.5)


# ===========================================================================
# 7b. Box-score impact (SPM): is Riley worse than the players ahead of him?
# ===========================================================================
# A transparent statistical plus-minus, fit on every season since 1996-97, run inside local() so its tables
# don't collide with the rest of the script. Two stages:
#   1. SHAPE. Each player-season's on/off net rating (on-court net minus the team's net with him off) is
#      ridge-regressed on his per-100 box rates relative to that season's league (points over a league-TS
#      baseline, shot volume, 3PA, AST, TOV, OREB, DREB, STL, BLK, PF). Player-level on/off sees redistribution
#      (defensive rebounds a teammate would have grabbed do not move on/off); a pure team-level fit put ~1 point
#      on every defensive rebound and failed the leaderboard test (SGA 19th in 2024-25).
#   2. SCALE. Team net rating is regressed (no intercept) on the team's possession-weighted sum of stage-1
#      values, so five on-court SPMs add up to a predicted team net rating. League average is 0.
#   Traded players are left out of both fitting stages. Uncertainty: both stages refit on a bootstrap over
#   team-seasons, each draw paired with a simulation of luck in the player's season totals. The intervals do
#   not cover what box scores miss (defense beyond steals/blocks/rebounds, screening, off-ball value).
spm <- local({
HIST    <- file.path(DATA_DIR, "hist")
seasons <- sprintf("%d-%02d", 1996:2026, (1997:2027) %% 100)
FIT_SEASONS <- seasons[seasons <= "2025-26"]
HOLDOUT     <- c("2023-24", "2024-25", "2025-26")
WIZ <- c(`Will Riley` = 1642860, `Tre Johnson` = 1642848
         , `Bub Carrington` = 1642267, `Bilal Coulibaly` = 1641731)
FEATS  <- c("pts_add", "tsa", "fg3a", "ast", "tov", "oreb", "dreb", "stl", "blk", "pf")
N_BOOT <- 1000
rd <- \(x, k = 1) round(x, k)

# ---- 1. Player-season and team-season tables -------------------------------
read_hist <- \(kind, s) read_csv(file.path(HIST, paste0(kind, "_", s, ".csv")), show_col_types = FALSE)
raw <- map_dfr(seasons, \(s) {
  read_hist("base", s) |>
    left_join(read_hist("adv", s) |> select(PLAYER_ID, POSS, ON_NET = NET_RATING), by = "PLAYER_ID") |>
    transmute(player_id = PLAYER_ID, player = PLAYER_NAME, team_id = TEAM_ID
              , team_count = TEAM_COUNT, age = AGE, season = s, min = MIN, poss = POSS
              , on_net = ON_NET, fga = FGA, fg3a = FG3A, fg3m = FG3M, fgm = FGM
              , fta = FTA, ftm = FTM, ast = AST, tov = TOV, oreb = OREB, dreb = DREB
              , stl = STL, blk = BLK, pf = PF, pts = PTS)
}) |> filter(poss > 0, min > 0)

team <- map_dfr(seasons, \(s) read_hist("team", s) |>
                  transmute(team_id = TEAM_ID, season = s, team_min = MIN
                            , net = NET_RATING, team_poss = POSS)) |>
  group_by(season) |> mutate(y = net - mean(net)) |> ungroup()

lg <- raw |>
  group_by(season) |>
  summarise(lg_ts = sum(pts) / (2 * sum(fga + 0.44 * fta))
            , across(c(fga, fta, fg3a, ast, tov, oreb, dreb, stl, blk, pf)
                     , ~ sum(.x) / sum(poss) * 100, .names = "lg_{.col}")
            , .groups = "drop") |>
  mutate(lg_tsa = lg_fga + 0.44 * lg_fta)

# relative per-100 features from (possibly simulated) counts
make_feats <- function(d) {
  d |>
    mutate(tsa_n = fga + 0.44 * fta
           , pts_add = (pts - 2 * lg_ts * tsa_n) / poss * 100
           , tsa  = tsa_n / poss * 100 - lg_tsa
           , across(c(fg3a, ast, tov, oreb, dreb, stl, blk, pf)
                    , ~ .x / poss * 100 - get(paste0("lg_", cur_column()))))
}
counts <- raw |> left_join(lg, by = "season")   # raw counts, kept for the bootstrap
ps <- make_feats(counts)

# stage-1 rows: full-season-with-one-team players, on/off from team totals
pl <- ps |>
  filter(team_count == 1, season %in% FIT_SEASONS) |>
  inner_join(team, by = c("team_id", "season")) |>
  mutate(off_poss = team_poss - poss
         , off_net = (net * team_poss - on_net * poss) / off_poss
         , onoff = on_net - off_net
         , w = 1 / (1 / poss + 1 / off_poss)) |>
  filter(poss >= 300, off_poss >= 500) |>
  mutate(w = w / mean(w), key = paste(team_id, season))

# stage-2 rows: team box line = 5 x possession-weighted mean of player rates
tx <- ps |>
  filter(team_count == 1, season %in% FIT_SEASONS) |>
  group_by(team_id, season) |>
  summarise(across(all_of(FEATS), ~ 5 * weighted.mean(.x, poss)), cov_min = sum(min)
            , .groups = "drop") |>
  inner_join(team, by = c("team_id", "season")) |>
  mutate(coverage = pmin(cov_min / (5 * team_min), 1), key = paste(team_id, season)) |>
  filter(coverage >= 0.6)

# ---- 2. Two-stage fit -------------------------------------------------------
Xm <- \(d) as.matrix(d[, FEATS])
ridge <- \(X, y, w, lambda) drop(solve(crossprod(X * w, X) + diag(lambda, ncol(X)), crossprod(X * w, y)))
fit_spm <- function(pl_d, tx_d, lambda) {
  shape <- ridge(Xm(pl_d), pl_d$onoff, pl_d$w, lambda)
  agg <- drop(Xm(tx_d) %*% shape)
  scale <- sum(tx_d$coverage * agg * tx_d$y) / sum(tx_d$coverage * agg^2)
  list(b = shape * scale, shape = shape, scale = scale)
}
pick_lambda <- function(d, grid = c(1, 10, 100, 1000, 10000)) {
  cv <- map_dbl(grid, \(l) sqrt(sum(map_dbl(unique(d$season), \(s) {
    tr <- d[d$season != s, ]; te <- d[d$season == s, ]
    sum(te$w * (te$onoff - Xm(te) %*% ridge(Xm(tr), tr$onoff, tr$w, l))^2)
  })) / sum(d$w)))
  list(lambda = grid[which.min(cv)], cv = tibble(lambda = grid, loso_wrmse_onoff = cv))
}
lam <- pick_lambda(pl)
fit <- fit_spm(pl, tx, lam$lambda)
r2_team <- with(tx, 1 - sum(coverage * (y - Xm(tx) %*% fit$b)^2) / sum(coverage * y^2))

# cluster bootstrap over team-seasons (both stages refit)
keys <- unique(tx$key)
pl_idx <- split(seq_len(nrow(pl)), pl$key); tx_idx <- split(seq_len(nrow(tx)), tx$key)
draws <- t(replicate(N_BOOT, {
  k <- sample(keys, replace = TRUE)
  fit_spm(pl[unlist(pl_idx[k], use.names = FALSE), ], tx[unlist(tx_idx[k], use.names = FALSE), ]
          , lam$lambda)$b
}))
colnames(draws) <- FEATS
coef_tab <- tibble(term = FEATS, shape_onoff = fit$shape, est = fit$b, sd = apply(draws, 2, sd)
                   , lo = apply(draws, 2, quantile, 0.05), hi = apply(draws, 2, quantile, 0.95)
                   , player_sd = map_dbl(FEATS, \(f) sd(ps[[f]][ps$min >= 1000]))) |>
  mutate(across(where(is.double), \(x) round(x, 3)))

# ---- 3. Player SPM + leaderboards ------------------------------------------
score <- function(d, b) {
  d |>
    mutate(raw_spm = drop(Xm(d) %*% b)) |>
    group_by(season) |>
    mutate(spm = raw_spm - weighted.mean(raw_spm, min)) |>
    ungroup()
}
ps <- score(ps, fit$b)
centre <- ps |> group_by(season) |> summarise(across(all_of(FEATS), ~ weighted.mean(.x, min)))

board <- function(s) {
  d <- ps |> filter(season == s, min >= 1500) |> arrange(desc(spm)) |>
    transmute(player, min = round(min), spm = rd(spm))
  list(top = head(d, 20), bottom = tail(d, 10))
}
leaderboards <- list(`2024-25` = board("2024-25"), `2025-26` = board("2025-26"))

# ---- 4. Out-of-sample checks (fit on 1996-97..2022-23) ---------------------
pre <- \(d) filter(d, !season %in% HOLDOUT)
fit_oos <- fit_spm(pre(pl), pre(tx), pick_lambda(pre(pl))$lambda)
# (a) same-season: team box line -> team net rating
te <- filter(tx, season %in% HOLDOUT) |> mutate(pred = drop(Xm(pick(everything())) %*% fit_oos$b))
same <- c(r = cor(te$pred, te$y), rmse = sqrt(mean((te$pred - te$y)^2)), rmse_null = sqrt(mean(te$y^2)))
# (b) player on/off in held-out seasons (weighted correlation)
plt <- filter(pl, season %in% HOLDOUT) |> mutate(pred = drop(Xm(pick(everything())) %*% fit_oos$shape))
onoff_r <- cov.wt(cbind(plt$pred, plt$onoff), wt = plt$w, cor = TRUE)$cor[1, 2]
# (c) roster test: last season's SPM (pre-holdout coefficients) x this season's minutes
ps_oos <- score(ps |> select(-raw_spm, -spm), fit_oos$b) |> mutate(sy = as.integer(substr(season, 1, 4)))
prev <- ps_oos |>
  group_by(player_id, sy) |> slice_max(min, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(player_id, sy = sy + 1, prev_spm = spm, prev_min = min)
newcomer <- ps_oos |> filter(!season %in% HOLDOUT) |> anti_join(prev, by = c("player_id", "sy")) |>
  summarise(v = weighted.mean(spm, min)) |> pull(v)
roster <- ps_oos |>
  filter(season %in% HOLDOUT) |>
  left_join(prev, by = c("player_id", "sy")) |>
  mutate(prev_spm = if_else(is.na(prev_spm) | prev_min < 250, newcomer, prev_spm)) |>
  group_by(team_id, season) |>
  summarise(pred = 5 * weighted.mean(prev_spm, min), .groups = "drop") |>
  inner_join(team, by = c("team_id", "season")) |>
  left_join(team |> transmute(team_id, season = seasons[match(season, seasons) + 1], last_net = y)
            , by = c("team_id", "season"))
m_roster <- lm(y ~ pred, roster)
roster_stats <- c(r_spm = cor(roster$pred, roster$y), r_last_net = cor(roster$last_net, roster$y)
                  , slope = unname(coef(m_roster)[2]), rmse_recal = sigma(m_roster)
                  , rmse_null = sd(roster$y))
# (d) year-to-year stability, 1000+ minutes both seasons (full-data coefficients)
yy <- ps |> mutate(sy = as.integer(substr(season, 1, 4))) |>
  group_by(player_id, sy) |> slice_max(min, n = 1, with_ties = FALSE) |> ungroup() |>
  select(player_id, sy, spm, min)
yy <- inner_join(yy, mutate(yy, sy = sy - 1), by = c("player_id", "sy"), suffix = c("", "_n")) |>
  filter(min >= 1000, min_n >= 1000)
yy_band <- filter(yy, between(min, 1200, 2400), between(min_n, 1200, 2400))
oos <- list(lambda = lam$lambda, lambda_cv = lam$cv, scale = fit$scale, team_r2_in = r2_team
            , same_season_team = same, heldout_player_onoff_r = onoff_r, roster = roster_stats
            , yoy_r = cor(yy$spm, yy$spm_n), yoy_n = nrow(yy)
            , yoy_change_sd_1200_2400 = sd(yy_band$spm_n - yy_band$spm) / sqrt(2))

# ---- 5. Uncertainty: bootstrap coefficients x parametric season noise -------
sim_counts <- function(r, n) {
  pct <- \(m, a) ifelse(a > 0, m / a, 0)
  fg2a <- rpois(n, r$fga - r$fg3a); fg3a <- rpois(n, r$fg3a); fta <- rpois(n, r$fta)
  fg2m <- rbinom(n, fg2a, pct(r$fgm - r$fg3m, r$fga - r$fg3a))
  fg3m <- rbinom(n, fg3a, pct(r$fg3m, r$fg3a)); ftm <- rbinom(n, fta, pct(r$ftm, r$fta))
  tibble(fga = fg2a + fg3a, fg3a = fg3a, fta = fta, pts = 2 * fg2m + 3 * fg3m + ftm
         , ast = rpois(n, r$ast), tov = rpois(n, r$tov), oreb = rpois(n, r$oreb)
         , dreb = rpois(n, r$dreb), stl = rpois(n, r$stl), blk = rpois(n, r$blk)
         , pf = rpois(n, r$pf), poss = r$poss) |>
    bind_cols(select(r, starts_with("lg_")))
}
# draw-level SPM for one player-season: bootstrap coefficient draw i paired with season sim i
spm_draws <- function(pid, s, b = draws) {
  r <- filter(counts, player_id == pid, season == s)
  X <- Xm(make_feats(sim_counts(r, nrow(b))))
  ctr <- unlist(filter(centre, season == s)[, FEATS])
  rowSums(X * b) - drop(b %*% ctr)
}
mins <- counts |> filter(player_id %in% WIZ, season %in% c("2025-26", "2026-27")) |>
  select(player_id, season, min, poss)
wiz_draws <- map(WIZ, \(pid) {
  l <- map(c(`2025-26` = "2025-26", `2026-27` = "2026-27"), \(s) spm_draws(pid, s))
  p <- filter(mins, player_id == pid) |> arrange(season)   # pooled: possession-weighted
  c(l, list(pooled = (p$poss[1] * l[[1]] + p$poss[2] * l[[2]]) / sum(p$poss)))
})
wiz <- imap_dfr(wiz_draws, \(l, nm) imap_dfr(l, \(x, s) {
  m <- filter(mins, player_id == WIZ[[nm]])
  tibble(player = nm, season = s
         , minutes = round(if (s == "pooled") sum(m$min) else m$min[m$season == s])
         , spm = rd(mean(x)), lo = rd(quantile(x, 0.05)), hi = rd(quantile(x, 0.95)))
}))
# width with season noise switched off (coefficient uncertainty only)
wiz$coef_only_sd <- map2_dbl(wiz$player, wiz$season, \(nm, s) {
  if (s == "pooled") return(NA_real_)
  r <- filter(ps, player_id == WIZ[[nm]], season == s)
  rd(sd(drop(draws %*% (unlist(r[, FEATS]) - unlist(filter(centre, season == s)[, FEATS])))), 2)
})
diffs <- map_dfr(c("2025-26", "pooled"), \(s) map_dfr(names(WIZ)[-1], \(nm) {
  d <- wiz_draws[["Will Riley"]][[s]] - wiz_draws[[nm]][[s]]
  tibble(comparison = paste("Riley -", nm), basis = s, est = rd(mean(d))
         , lo = rd(quantile(d, 0.05)), hi = rd(quantile(d, 0.95)), p_riley_better = rd(mean(d > 0), 2))
}))
# is the parametric season noise realistic? compare to year-to-year change at similar minutes
oos$param_noise_sd_1200_2400 <- ps |> filter(season == "2025-26", between(min, 1200, 2400)) |>
  slice_sample(n = 40) |>
  mutate(s = map_dbl(player_id, \(pid) sd(Xm(make_feats(sim_counts(
    filter(counts, player_id == pid, season == "2025-26"), 1000))) %*% fit$b))) |>
  pull(s) |> median()

# ---- 6. Scale: young players ------------------------------------------------
young <- ps |> filter(season == "2025-26", min >= 800, age <= 22)
young_dist <- tibble(n = nrow(young), median = rd(median(young$spm))
                     , p10 = rd(quantile(young$spm, 0.1)), p90 = rd(quantile(young$spm, 0.9)))
wiz_rank_young <- young |> filter(player_id %in% WIZ) |>
  transmute(player, spm = rd(spm), pct_rank = rd(map_dbl(spm, \(v) mean(young$spm <= v)), 2))

method_note <- paste(
  "SPM is a box-score estimate of a player's impact per 100 possessions, built in two stages on",
  "1996-97 to 2025-26 nba.com totals. First, each player-season's on/off net rating was ridge-regressed",
  "on his per-100 box rates relative to the league that season (points above a league-average-TS",
  "baseline, shot volume, 3PA, AST, TOV, OREB, DREB, STL, BLK, PF), which sets the relative weights.",
  sprintf("Second, those weights were rescaled (x%.2f) so that a team's minutes-weighted SPMs best", fit$scale),
  "reproduce its actual net rating; five on-court SPMs sum to a predicted team net rating, and the",
  "minutes-weighted league average is 0. Traded players were left out of the fitting rows.",
  "Intervals combine refits on a bootstrap over team-seasons with a simulation of the luck in a player's",
  "season totals. They do not cover what box scores miss (defence beyond steals, blocks and rebounds,",
  "screening, off-ball value), nor how a role or teammates shape a player's box line.")

res <- list(method_note = method_note, leaderboards = leaderboards, oos = oos, wiz = wiz
            , diffs = diffs, young_dist = young_dist, wiz_rank_young = wiz_rank_young
            , coefficients = coef_tab)
  list(method_note = method_note, leaderboards = leaderboards, oos = oos, wiz = wiz
       , diffs = diffs, young_dist = young_dist, wiz_rank_young = wiz_rank_young, coefficients = coef_tab)
})
# leaderboard test, as a gate: the league's best players must be at the top
stopifnot(all(c("Nikola Jokić", "Shai Gilgeous-Alexander", "Victor Wembanyama") %in% head(spm$leaderboards$`2025-26`$top$player, 5)))
print(spm$wiz, n = Inf); print(spm$diffs)

spm_plot <- spm$wiz |> filter(season == "pooled") |> mutate(player = factor(player, rev(bench_rivals)))
p_spm <- ggplot(spm_plot, aes(spm, player, colour = player == "Will Riley")) +
  geom_vline(xintercept = spm$young_dist$median, colour = "grey60", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 2.4, alpha = 0.35, orientation = "y") +
  geom_point(size = 5) +
  geom_text(aes(label = sprintf("%+.1f", spm)), nudge_y = 0.3, family = font, size = 4.4) +
  scale_colour_manual(values = c(`TRUE` = pal[["red"]], `FALSE` = pal[["navy"]]), guide = "none") +
  labs(title = "Box score: Riley ahead of Johnson, even with Carrington"
       , subtitle = sprintf("A rating built from box-score stats, last season and this one together. It estimates how many points a player adds per 100 possessions, compared to an average player. Zero is average. Bars show the likely range. Dashed line: the typical young player (22 or younger, 800+ minutes last season, %d in all).", spm$young_dist$n)
       , x = "Points per 100 possessions vs an average player (0 = league average)", y = NULL, caption = caption_text) +
  theme_wiz(16) + theme(axis.text.y = element_text(colour = "black"), panel.spacing = unit(2, "lines"))
save_plot("riley_spm.png", p_spm, width = 11, height = 7)

# ===========================================================================
# 8. The rookie arc: what happened when the minutes came
# ===========================================================================
glp <- rd(sprintf("gamelog_%s.csv", PREV)) |> filter(player_id == RILEY_ID) |> arrange(game_date) |>
  mutate(month = floor_date(game_date, "month"))
lg_ts_prev <- lg$lg_ts[lg$season == PREV]
arc <- glp |> group_by(month) |>
  summarise(gp = n(), mpg = mean(min), pts36 = sum(pts) / sum(min) * 36, tsa = sum(fga) + 0.44 * sum(fta), ts = sum(pts) / (2 * tsa)
            , fg3m = sum(fg3m), fg3a = sum(fg3a), ftr = sum(fta) / sum(fga), ast36 = sum(ast) / sum(min) * 36, tov36 = sum(tov) / sum(min) * 36, .groups = "drop") |>
  mutate(lab = format(month, "%b"))
# halves split at the game where his minutes jumped for good, chosen as the season midpoint in games, not by eye
glp <- glp |> mutate(n = row_number(), half = if_else(n <= ceiling(n() / 2), "First 37 games", "Last 37 games"))
halves <- glp |> group_by(half) |>
  summarise(gp = n(), mpg = mean(min), pts36 = sum(pts) / sum(min) * 36, ts = sum(pts) / (2 * (sum(fga) + 0.44 * sum(fta))), p3 = sum(fg3m) / sum(fg3a), fg3a = sum(fg3a)
            , ftr = sum(fta) / sum(fga), ast36 = sum(ast) / sum(min) * 36, tov36 = sum(tov) / sum(min) * 36, first = min(game_date), last = max(game_date), .groups = "drop")
set.seed(202)
ts_boot <- replicate(4000, { g <- glp[sample(nrow(glp), replace = TRUE), ]
  a <- g[g$half == "Last 37 games", ]; b <- g[g$half == "First 37 games", ]
  sum(a$pts) / (2 * (sum(a$fga) + 0.44 * sum(a$fta))) - sum(b$pts) / (2 * (sum(b$fga) + 0.44 * sum(b$fta))) })
halves_ts_gap <- c(est = halves$ts[2] - halves$ts[1], lo = quantile(ts_boot, 0.05), hi = quantile(ts_boot, 0.95), p_pos = mean(ts_boot > 0))
pt(halves)
# what else changed between the halves: the rest of the roster, three-point luck, and on/off within each half
split_date <- max(glp$game_date[glp$half == "First 37 games"])
gl_wiz <- gl_prev |> filter(team_id == WIZ_ID) |> mutate(half = if_else(game_date <= split_date, "first", "second"))
n_team_g <- gl_wiz |> distinct(game_id, half) |> count(half)
roster_shift <- gl_wiz |> group_by(player_name, half) |> summarise(min = sum(min), .groups = "drop") |>
  left_join(n_team_g, by = "half") |> mutate(mpg_team = min / n) |> select(player_name, half, mpg_team) |>
  pivot_wider(names_from = half, values_from = mpg_team, values_fill = 0) |> mutate(change = second - first) |> arrange(change)
team_halves <- gl_wiz |> distinct(game_id, half, wl) |> count(half, wl) |> pivot_wider(names_from = wl, values_from = n)
ts_at_first3 <- glp |> filter(half == "Last 37 games") |> summarise(pts_adj = sum(pts) - 3 * sum(fg3m) + 3 * sum(fg3a) * halves$p3[1], tsa = sum(fga) + 0.44 * sum(fta)) |>
  mutate(ts = pts_adj / (2 * tsa)) |> pull(ts)
mates_ts <- gl_wiz |> filter(player_id != RILEY_ID) |> group_by(half) |> summarise(ts = sum(pts) / (2 * (sum(fga) + 0.44 * sum(fta))))
half_onoff <- map_dfr(c("first", "second"), \(h) {
  gids <- unique(as.character(gl_wiz$game_id[gl_wiz$half == h]))
  d <- bx |> filter(season == PREV, person_id == RILEY_ID, game_id %in% gids) |> select(game_id, on_min = min, on_pm = plus_minus) |>
    inner_join(team_margin |> filter(season == PREV), by = "game_id") |> mutate(off_min = game_min - on_min, off_pm = margin - on_pm)
  tibble(half = h, on48 = sum(d$on_pm) / sum(d$on_min) * 48, onoff = sum(d$on_pm) / sum(d$on_min) * 48 - sum(d$off_pm) / sum(d$off_min) * 48)
})
pt(roster_shift |> filter(abs(change) > 8)); pt(team_halves); print(ts_at_first3); pt(mates_ts); pt(half_onoff)
arc_top <- arc |> ggplot(aes(month, ts)) +
  geom_hline(yintercept = lg_ts_prev, linetype = "dashed", colour = "grey55") +
  annotate("text", x = max(arc$month) - 25, y = lg_ts_prev + 0.015, label = "League average", hjust = 0, family = font, colour = "grey45", size = 4) +
  geom_line(colour = pal[["red"]], linewidth = 1.4) + geom_point(aes(size = tsa), colour = pal[["red"]]) +
  geom_text(aes(label = percent(ts, 0.1)), nudge_y = 0.035, family = font, size = 4.4, colour = pal[["red"]], fontface = "bold") +
  scale_y_continuous(labels = percent_format(1), limits = c(0.3, 0.68)) + scale_size_area(max_size = 7, guide = "none") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = NULL, y = "True shooting") + theme_wiz(16) + theme(axis.text.x = element_blank(), plot.margin = margin(0, 22, 0, 18))
arc_bot <- arc |> ggplot(aes(month, mpg)) +
  geom_col(fill = pal[["navy"]], width = 20) +
  geom_text(aes(label = sprintf("%.0f", mpg)), vjust = -0.4, family = font, size = 4.4, colour = pal[["navy"]]) +
  scale_y_continuous(limits = c(0, 40), breaks = NULL) + scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = NULL, y = "Minutes\nper game") + theme_wiz(16) + theme(plot.margin = margin(0, 22, 0, 18))
p_arc <- (arc_top / arc_bot) + plot_layout(heights = c(2.2, 1)) +
  plot_annotation(title = "A better second half, on a different team"
                  , subtitle = sprintf("Riley's rookie year by month: <span style='color:%s'>**true shooting**</span> (scoring per shot; bigger dots mean more shots) and minutes per game. Over his last 37 games he played %.0f minutes a night with %s true shooting. Over his first 37, it was %.0f minutes and %s.", pal[["red"]], halves$mpg[2], percent(halves$ts[2], 0.1), halves$mpg[1], percent(halves$ts[1], 0.1))
                  , caption = caption_text, theme = theme_wiz(16))
save_plot("riley_arc.png", p_arc, width = 11, height = 8)

# ===========================================================================
# 8b. Hustle: what the tracking counts, and what it can't see
# ===========================================================================
# nba.com's hustle tracking. Its glossary (nba.com/stats/help/glossary): a loose ball is "recovered" only when a player "gains sole
# possession of a live ball that is not in the control of either team"; a deflection is any time a defender "gets their hand on the
# ball on a non-shot attempt", possession or not. A dive that doesn't come up with the ball counts for nothing.
hustle_stats <- c(loose = "Loose balls recovered", defl = "Deflections", stl = "Steals", oreb = "Offensive rebounds", cont = "Contested shots", box = "Box-outs")
hus_season <- function(s) {
  rd(sprintf("hustle_%s.csv", s)) |>
    select(player_id, player_name, team_abbreviation, min, loose = loose_balls_recovered, defl = deflections, cont = contested_shots, box = box_outs, charges = charges_drawn) |>
    left_join(rd(sprintf("hist/base_%s.csv", s)) |> select(player_id, stl, oreb), by = "player_id") |>
    left_join(pidx |> select(player_id, position), by = "player_id") |>
    mutate(season = s)
}
# every non-center with 500+ minutes that season, rates per 36 minutes and their percentile in the group
hus_peers <- function(s) {
  hus_season(s) |> filter(min >= 500, !str_detect(replace_na(position, "C"), "C")) |>
    mutate(across(all_of(names(hustle_stats)), \(x) 36 * x / min, .names = "{.col}36")) |>
    mutate(across(all_of(paste0(names(hustle_stats), "36")), percent_rank, .names = "p_{.col}"), n = n())
}
hp_prev <- hus_peers(PREV); hp_good <- hus_peers(GOODWIN_SEASON); hp_stab <- hus_peers(STAB_SEASON)
hustle_cmp <- bind_rows(hp_prev |> filter(player_id == RILEY_ID) |> mutate(who = "Will Riley")
                        , hp_good |> filter(player_id == GOODWIN_ID) |> mutate(who = "Jordan Goodwin"))
stopifnot(nrow(hustle_cmp) == 2)
hustle_long <- map_dfr(names(hustle_stats), \(k) tibble(who = hustle_cmp$who, season = hustle_cmp$season, key = k, stat = hustle_stats[[k]]
                                                        , count = hustle_cmp[[k]], per36 = hustle_cmp[[paste0(k, "36")]], pctile = hustle_cmp[[paste0("p_", k, "36")]]))
pt(hustle_long)
# leaderboard test: the top of loose balls plus charges should be the players known for it
hustle_leaders <- hp_prev |> mutate(scrap36 = 36 * (loose + charges) / min) |> arrange(desc(scrap36)) |> slice_head(n = 8) |>
  select(player_name, team_abbreviation, min, loose, charges, scrap36)
pt(hustle_leaders)
# is Riley's low count just bad luck on small numbers? the top of a 90% Poisson interval on his rate, against the group
r_h <- hp_prev |> filter(player_id == RILEY_ID)
hustle_noise <- map_dfr(c("loose", "defl"), \(k) {
  hi <- 36 * qgamma(0.95, r_h[[k]] + 0.5) / r_h$min
  tibble(key = k, count = r_h[[k]], per36 = r_h[[paste0(k, "36")]], hi90 = hi, peer_median = median(hp_prev[[paste0(k, "36")]])
         , pctile_at_hi = mean(hp_prev[[paste0(k, "36")]] < hi))
})
pt(hustle_noise)
# do these rates repeat? the same players, the season before and Riley's rookie season
hustle_stab <- map_dfr(names(hustle_stats), \(k) {
  j <- inner_join(hp_stab |> select(player_id, a = all_of(paste0(k, "36"))), hp_prev |> select(player_id, b = all_of(paste0(k, "36"))), by = "player_id")
  tibble(key = k, stat = hustle_stats[[k]], n = nrow(j), r = cor(j$a, j$b))
})
pt(hustle_stab)
# Goodwin since he left: is it a trait, or one season?
goodwin_since <- bind_rows(hp_stab, hp_prev) |> filter(player_id == GOODWIN_ID) |> select(season, team_abbreviation, min, loose36, p_loose36, defl36, p_defl36)
pt(goodwin_since)

# the near misses, from the Wizards' play-by-play: loose-ball fouls (either side) and held balls (tie-ups) are fights for a
# 50-50 ball that the hustle tracking only credits if someone comes up with it. Rates use Wizards minutes from the box scores.
lb_fouls <- pbp |> filter(action_type == "foul", descriptor == "loose ball")
held <- pbp |> filter(action_type == "jumpball", descriptor == "heldball") |> distinct(season, game_id, order_number, .keep_all = TRUE)
near_miss <- bind_rows(lb_fouls |> transmute(season, person_id, kind = "lb_committed"), lb_fouls |> transmute(season, person_id = foul_drawn_person_id, kind = "lb_drawn")
                       , held |> transmute(season, person_id = jump_ball_won_person_id, kind = "tieup"), held |> transmute(season, person_id = jump_ball_lost_person_id, kind = "tieup")) |>
  filter(!is.na(person_id)) |> count(season, person_id, kind) |> pivot_wider(names_from = kind, values_from = n, values_fill = 0)
wiz_near <- bx |> filter(season == PREV) |> group_by(person_id) |> summarise(name = last(name), min = sum(min), .groups = "drop") |> filter(min >= 300) |>
  left_join(near_miss |> filter(season == PREV) |> select(-season), by = "person_id") |>
  left_join(pidx |> select(person_id = player_id, position, idx_height), by = "person_id") |>
  mutate(across(c(lb_committed, lb_drawn, tieup), \(x) replace_na(x, 0)), near = lb_committed + lb_drawn + tieup, near36 = 36 * near / min
         , height_in = as.numeric(str_extract(idx_height, "^\\d+")) * 12 + as.numeric(str_extract(idx_height, "(?<=-)\\d+"))) |>
  arrange(desc(near36)) |> mutate(rank_near = row_number(), is_big = height_in >= 82)
pt(wiz_near)
stopifnot(sum(near_miss$lb_committed) == nrow(lb_fouls), sum(near_miss$tieup) == 2 * nrow(held) - sum(is.na(c(held$jump_ball_won_person_id, held$jump_ball_lost_person_id))))

# the chart: each stat's percentile for the two players, one row per stat
hl <- hustle_long |> mutate(stat = factor(stat, rev(hustle_stats)), who = factor(who, c("Jordan Goodwin", "Will Riley")))
hl_w <- hl |> select(stat, who, pctile) |> pivot_wider(names_from = who, values_from = pctile)
p_hustle <- ggplot(hl, aes(pctile, stat)) +
  geom_vline(xintercept = 0.5, colour = pal[["light"]], linetype = "dashed") +
  annotate("text", x = 0.5, y = 6.45, label = "Median player", family = font, size = 4, colour = pal[["grey"]], vjust = 0) +
  geom_segment(data = hl_w, aes(x = `Will Riley`, xend = `Jordan Goodwin`, y = stat, yend = stat), colour = "grey85", linewidth = 2.2, inherit.aes = FALSE) +
  geom_point(aes(colour = who), size = 6) +
  geom_text(aes(label = sprintf("%.1f", per36), colour = who), family = font, size = 4.1, vjust = -1.3, show.legend = FALSE) +
  scale_colour_manual(values = c("Jordan Goodwin" = pal[["navy"]], "Will Riley" = pal[["red"]]), guide = "none") +
  scale_x_continuous(labels = \(x) scales::ordinal(round(100 * x)), limits = c(0, 1.02), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1))) +
  labs(title = "Goodwin's hustle showed up in the numbers. Riley's hasn't."
       , subtitle = sprintf("How each ranked among the %d or so non-centers who played 500+ minutes that season: 99th means he beat 99 of every 100 players. Numbers are per 36 minutes. <span style='color:%s'>**Jordan Goodwin, %s Wizards**</span> and <span style='color:%s'>**Will Riley, %s rookie season**</span>.", round(mean(hustle_cmp$n), -1), pal[["navy"]], GOODWIN_SEASON, pal[["red"]], PREV)
       , x = "Rank among players (right is more)", y = NULL, caption = caption_text) +
  theme_wiz(16) + theme(axis.text.y = element_text(colour = "black"))
save_plot("riley_hustle.png", p_hustle, width = 11, height = 7.5)

# ===========================================================================
# 9. What it would cost to find out
# ===========================================================================
# wins per point of net rating, from every full season in the data (one regression line, slope reported with its interval)
team_full <- team |> filter(season != CUR) |> mutate(wpct = team_w / team_gp)
wfit <- lm(wpct ~ team_net, data = team_full)
w_slope <- coef(wfit)[["team_net"]]; w_slope_ci <- confint(wfit, "team_net", level = 0.9)
games_left <- 82 - n_games_cur
cost <- expand_grid(gap = seq(-5, 3, by = 0.25), extra = c(10, 15, 20)) |>
  mutate(wins = w_slope * gap * extra / 48 * games_left)
cost_at <- function(gap, extra = 15) w_slope * gap * extra / 48 * games_left
# what the box-score model says the gap is, for the two players whose minutes the post proposes to move
gap_band <- spm$diffs |> filter(basis == "pooled", comparison == "Riley - Tre Johnson")
gap_lo <- min(gap_band$lo); gap_hi <- max(gap_band$hi)
p_cost <- ggplot(cost |> filter(extra == 15), aes(gap, wins)) +
  annotate("rect", xmin = gap_lo, xmax = gap_hi, ymin = -Inf, ymax = Inf, fill = "grey92") +
  annotate("text", x = (gap_lo + gap_hi) / 2, y = max(cost$wins[cost$extra == 15]) * 0.95, label = "Where the box score puts\nRiley against Tre Johnson\n(90% range)", family = font, size = 4.2, colour = pal[["grey"]], lineheight = 0.95, vjust = 1) +
  geom_hline(yintercept = 0, colour = "grey60") + geom_vline(xintercept = 0, colour = "grey80") +
  geom_line(linewidth = 1.6, colour = pal[["red"]]) +
  annotate("point", x = gap_band$est, y = cost_at(gap_band$est), size = 5, colour = pal[["red"]]) +
  annotate("text", x = gap_band$est, y = cost_at(gap_band$est), label = sprintf("Best guess: %+.1f wins", cost_at(gap_band$est)), hjust = -0.15, vjust = 1.4, family = font, fontface = "bold", colour = pal[["red"]], size = 4.6) +
  scale_x_continuous(breaks = seq(-5, 3, 1), labels = \(x) sprintf("%+g", x)) +
  labs(title = "Riley for Johnson: probably a small gain"
       , subtitle = sprintf("Wins the Wizards would gain (above zero) or lose (below) over their last %d games if Riley took 15 of Tre Johnson's minutes a night. Farther right means Riley is the better player. To turn points into wins, we used every NBA team since 1996-97.", games_left)
       , x = "How much better Riley is than Johnson (points per 100 possessions)", y = "Wins gained, rest of season", caption = caption_text) +
  theme_wiz(16)
save_plot("riley_cost.png", p_cost, width = 11, height = 8)

# ===========================================================================
# 10. Everything the write-up quotes
# ===========================================================================
res <- list(
  hustle = list(long = hustle_long, leaders = hustle_leaders, noise = hustle_noise, stab = hustle_stab, goodwin_since = goodwin_since, wiz_near = wiz_near, n = hustle_cmp |> select(who, season, n, min, charges))
  , through = max(rd(sprintf("gamelog_%s.csv", CUR))$game_date), n_games_cur = n_games_cur, games_left = games_left
  , riley = riley, lg = lg, wiz_record = bx |> filter(season == CUR) |> distinct(game_id) |> nrow()
  , peers_n = n_distinct(peers$player_id), prof_riley = prof_riley, riley_pt = riley_pt, diet = diet, zones = zones
  , pool_n = nrow(pool), pool_base = pool_base, comps = comps_f, comp_table = comp_table, classmates_comp = classmates_comp, riley_z = riley_z, pc_var = pc_var, pc_ends = pc_ends, pc_load = pc_load
  , fit_n = nrow(fit_set), min_pred = min_pred, hist_cut = hist_cut, beta_min = beta_min |> select(-draws), riley_yy = riley_yy, class25 = class25
  , ppc_min = ppc_min, pit_tail = pit_tail, class_pred = class_pred, age22_low = age22_low, early_sum = early_sum, class_low = class_low, early = early, prior_check_min = prior_check_min, obs_range_min = obs_range_min, diag_min = diag_min[names(diag_min) != "summary"], diag_min_tab = diag_min$summary
  , shoot = shoot_out, ppc_sh = ppc_sh, prior_check_3 = prior_check_3, diag_sh = diag_sh[names(diag_sh) != "summary"], diag_sh_tab = diag_sh$summary, ft_grid = ft_grid
  , rot_now = rot_now, riley_state = riley_state, rot_garbage = rot_garbage, dyb_games = dyb_games, team_close = team_close, state_by_player = state_by_player
  , rival_box = rival_box, rival_onoff = rival_onoff, rival_pt = rival_pt
  , arc = arc, halves = halves, halves_ts_gap = halves_ts_gap, roster_shift = roster_shift, team_halves = team_halves, ts_at_first3 = ts_at_first3
  , mates_ts = mates_ts, half_onoff = half_onoff, team_now = team |> filter(season == CUR) |> mutate(rk_net = rank(-team_net)) |> filter(team_id == WIZ_ID)
  , w_slope = w_slope, w_slope_ci = w_slope_ci, cost_15_3 = cost_at(-3), cost_15_1 = cost_at(-1), cost_20_3 = cost_at(-3, 20), cost = cost, spm = spm, gap_lo = gap_lo, gap_hi = gap_hi, cost_gap_lo = cost_at(gap_lo), cost_gap_hi = cost_at(gap_hi), cost_gap_mid = cost_at(gap_band$est)
)
# who arrived: Wizards minutes last season vs this season for the players added to the rotation
res$arrivals <- bx |> filter(person_id %in% c(1629027, 203076, 1643407)) |> group_by(person_id, season) |> summarise(min = sum(min), .groups = "drop") |>
  complete(person_id, season = c(PREV, CUR), fill = list(min = 0)) |>
  left_join(rot_now |> select(person_id, name, mpg), by = "person_id") |> left_join(pidx |> select(person_id = player_id, draft = idx_draft), by = "person_id")
# young pick-and-roll handlers among the profile peers
res$young_prbh <- peers |> filter(age <= 20, prbh >= 0.2) |> select(player_name, age, prbh) |> arrange(desc(prbh))
res$d_net_sd <- sd(fit_set$d_net)
res$draft <- pidx |> filter(player_id %in% rival_ids$player_id) |> select(player_id, name, position, idx_draft)
saveRDS(res, "riley_results.rds")
message("Wrote riley_results.rds and ", length(list.files("figs")), " figures to figs/")
