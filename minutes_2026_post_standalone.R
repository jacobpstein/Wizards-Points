###########################################################
# The other 210 minutes -- single-file reproduction of the Substack post
#
# Fully self-contained: no _targets.R, no tar_read(), no other .R file. This
# script fits the ratings model, the development curve, and the rookie
# reference class from the raw data itself, builds the 2026-27 projection and
# the two rotations, scores every lineup, runs the two backtests, and then
# pulls the exact numbers and figures the post uses -- in that order, top to
# bottom, one file.
#
# What "self-contained" does and doesn't mean here: every R function this
# needs is defined below (copied verbatim from tank_functions.R, which is
# this repo's normal home for them -- the project's own targets pipeline in
# _targets.R is what stays in sync with that file day to day; this script is
# a point-in-time flattening of it for the post). It still reads the raw
# data CSVs already fetched into this folder (fetch_tank_data.py) and the
# two .stan model specifications (tank_development.stan, tank_rookie.stan)
# that already live here -- those are inputs, the same way the CSVs are, not
# application code, so they aren't inlined as string literals.
#
# Seeding: _targets.R sets a fresh, independent random seed before each
# stage (derived from that stage's name plus the pipeline's global seed,
# 202), not one seed reused continuously top to bottom. A plain sequential
# set.seed(202) at the top of a flat script drifts from that after the first
# stage that consumes randomness, and two Stan fits deep in the pipeline
# (the rookie model, refit inside each backtest on a smaller training
# window) are numerically sensitive enough that a different draw can fail to
# converge outright. stage_seed() below reproduces _targets.R's exact
# per-stage seed (same hash, same global seed), called at each stage
# boundary below, so this script draws the same random numbers the tracked
# pipeline does and reproduces its results, not just its method.
#
# Runtime: this refits everything from scratch -- the RAPM ratings model
# over ~470k stints, two Stan fits (development curve, rookie model), and
# two more full refits for the backtests. Expect on the order of 30-60+
# minutes depending on the machine, most of it in fit_rapm()'s
# marginal-likelihood optimization and the four cmdstanr fits. Needs
# cmdstanr set up and pointed at a working CmdStan install.
#
# Session info
# R version 4.5.3 (2026-03-11) -- "Reassured Reassurer"
# Platform: aarch64-apple-darwin20
###########################################################

set.seed(202)

library(tidyverse)
library(janitor)
library(Matrix)
library(cmdstanr)
library(posterior)
library(secretbase)
library(ggtext)
library(ggrepel)
library(ggbeeswarm)
library(usaidplot)

# Reproduces _targets.R's tar_option_set(seed = 202) + the per-target seed
# targets derives from it (targets:::tar_seed_create): shake256 hash of the
# stage's name and the global seed, truncated to a 32-bit int. Verified
# against tar_seed_create() directly -- identical output, no targets
# dependency needed to compute it.
stage_seed <- function(name, global_seed = 202L) {
  secretbase::shake256(x = list(as.character(name), as.integer(global_seed)), bits = 32L, convert = NA)
}
set_stage <- function(name) set.seed(stage_seed(name))

wiz_blue     <- "#172869FF"
wiz_red      <- "#D9565CFF"
teal         <- "#3B9AB2"
caption_text <- "Data: nba.com/stats\nwizardspoints.substack.com"

save_plot <- function(name, p, height = NULL, width = NULL) {
  ggsave(name, p, width = width %||% 11, height = height %||% 7, dpi = 300, device = ragg::agg_png)
}

titles_wrap <- theme(
  plot.title = ggtext::element_textbox_simple(size = rel(1.15), face = "bold", lineheight = 1.15
    , margin = margin(b = 26), width = unit(1, "npc"))
  , plot.subtitle = ggtext::element_textbox_simple(size = rel(0.92), colour = "grey30", lineheight = 1.25
    , margin = margin(b = 12), width = unit(1, "npc"))
  , plot.title.position = "plot"
  , plot.caption.position = "plot"
  , plot.margin = margin(t = 12, r = 18, b = 10, l = 12)
)

s1  <- function(x) sprintf("%+.1f", x)
f1  <- function(x) sprintf("%.1f", x)
pct <- function(x) sprintf("%.0f%%", 100 * x)
comma <- function(x) trimws(format(round(x), big.mark = ","))
and_list <- function(x) if (length(x) < 2) x else paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))


# =============================================================================
# Functions -- copied verbatim from tank_functions.R (this repo's normal home
# for them, kept in sync by _targets.R day to day).
# =============================================================================

# =============================================================================
# Tank or talent? — functions for the targets pipeline in _targets.R
#
# Every stage is a function of its inputs; targets decides what to rerun when
# code, data or a .stan file changes. Nothing here reads or writes a cache.
# =============================================================================

WIZARDS_ID <- 1610612764
# Ratings, development and the rookie reference class use every season the
# liveData feed has (it begins in 2019-20). Anything priced from absences —
# the decomposition, the fingerprint, projection availability — starts in
# 2021-22: the bubble season and the 72-game season that followed are full of
# health-and-safety absences and neutral or empty arenas that would read as
# deployment choices.
SEASONS <- c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
DECOMP_SEASONS <- c("2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
# standings only: the season before the first rated one, for last-season exposure
SEASONS_ALL <- c("2018-19", SEASONS)
BUBBLE <- as.Date(c("2020-07-30", "2020-08-31"))

# CmdStan ships with the conda install here and is not on the path in a
# non-interactive Rscript session
if (is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL))) {
  cmdstanr::set_cmdstan_path("/opt/anaconda3/bin/cmdstan")
}

# ---- parsing helpers ---------------------------------------------------------

parse_min <- function(x) {
  m <- as.numeric(str_extract(x, "(?<=PT)\\d+(?=M)"))
  s <- as.numeric(str_extract(x, "(?<=M)[\\d.]+(?=S)"))
  coalesce(m, 0) + coalesce(s, 0) / 60
}

parse_clock <- function(x) {
  m <- as.numeric(str_extract(x, "(?<=PT)\\d+(?=M)"))
  s <- as.numeric(str_extract(x, "(?<=M)[\\d.]+(?=S)"))
  m * 60 + s
}

# last observation carried forward, starting from zero (score before the tip)
carry_forward <- function(x) {
  x[1] <- coalesce(x[1], 0L)
  x[cummax(ifelse(is.na(x), 0L, seq_along(x)))]
}

height_inches <- function(h) {
  map_dbl(str_split(h, "-"), \(p) if (length(p) == 2) as.numeric(p[1]) * 12 + as.numeric(p[2]) else NA_real_)
}

position_group <- function(pos) {
  case_when(
    pos %in% c("G", "G-F") ~ "Guard"
    , pos %in% c("F-G", "F") ~ "Forward"
    , pos %in% c("F-C", "C-F", "C") ~ "Big"
    , TRUE ~ NA_character_
  )
}

# ---- Stage 1: inputs ---------------------------------------------------------

read_player_index <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    transmute(person_id, name = paste(player_first_name, player_last_name), team_id, position
      , pos_group = position_group(position), height_idx = height_inches(height)
      # a handful of players carry draft number 0; they are undrafted, and a 0
      # would otherwise pass a first-round filter and take log(0) in the prior
      , draft_year, draft_number = if_else(draft_number > 0, draft_number, NA_real_), from_year)
}

# One row per player per team-game, including everyone listed who did not
# play, with the league's own reason code.
read_player_games <- function(box_paths, games_path) {
  # dates from the official game list: the feed's UTC tip-off stamp crosses
  # midnight for late games
  game_dates <- read_csv(games_path, show_col_types = FALSE, col_types = cols(GAME_ID = "c", .default = col_guess())) |>
    distinct(game_id = str_pad(GAME_ID, 10, pad = "0"), game_date = as.Date(GAME_DATE))
  pg <- map_dfr(box_paths, \(p) {
    read_csv(p, show_col_types = FALSE
      , col_types = cols(game_id = "c", minutes = "c", name = "c", position = "c", team_tricode = "c"
          , status = "c", not_playing_reason = "c", not_playing_description = "c"
          , game_time_utc = "c", .default = col_double())) |>
      mutate(season = str_extract(p, "\\d{4}-\\d{2}"))
  }) |>
    mutate(game_id = str_pad(game_id, 10, pad = "0"), min = parse_min(minutes)
      , season_start = as.integer(str_sub(season, 1, 4))) |>
    left_join(game_dates, by = "game_id") |>
    select(-minutes, -game_time_utc)
  stopifnot(!anyNA(pg$game_date))
  pg
}

# ---- Stage 2: lineup stints ----------------------------------------------------
# The liveData feed logs every substitution with a player ID (including those
# made between periods, stamped at 12:00) and the box score marks the starting
# five, so lineups are tracked exactly rather than inferred.
#
# pbpstats was checked as an alternative on 30 games from 2025-26: identical
# points, possessions within ±5 per team-game, but player seconds matched the
# box score less closely (mean error 0.72s vs 0.16s here, worst 35s vs 17s),
# one game failed on an overtime lineup, and its stats layer raised on the
# current feed. It would also need the raw JSON for every game, which this
# pipeline does not keep. The checks in validate_stints() are what matter,
# whichever tool builds the stints.
#
# Possessions are counted from events, not the feed's possession marker, which
# flickers on fouls and jump balls. A team's possession ends on a made field
# goal, a made final free throw of a trip (2 of 2, 3 of 3), a turnover, or a
# live-ball defensive rebound by the opponent.

build_game_stints <- function(a, bx) {
  a <- a[order(a$orderNumber), ]
  home_id <- bx$team_id[bx$home == 1][1]
  away_id <- bx$team_id[bx$home == 0][1]
  st_h <- bx$person_id[bx$home == 1 & bx$starter == 1]
  st_a <- bx$person_id[bx$home == 0 & bx$starter == 1]
  if (length(st_h) != 5 || length(st_a) != 5) return(NULL)

  n <- nrow(a)
  per <- a$period
  plen <- ifelse(per <= 4, 720, 300)
  pstart <- ifelse(per <= 4, (per - 1) * 720, 2880 + (per - 5) * 300)
  el <- pstart + plen - parse_clock(a$clock)

  is_sub <- a$actionType == "substitution"
  ids <- unique(c(bx$person_id[bx$played == 1], a$personId[is_sub]))
  ids <- ids[!is.na(ids) & ids > 0]
  team_of <- coalesce(bx$team_id[match(ids, bx$person_id)], a$teamId[is_sub][match(ids, a$personId[is_sub])])

  delta <- matrix(0L, length(ids), n)
  si <- which(is_sub)
  delta[cbind(match(a$personId[si], ids), si)] <- ifelse(a$subType[si] == "in", 1L, -1L)
  state <- as.integer(ids %in% c(st_h, st_a)) + t(apply(delta, 1, cumsum))
  if (length(ids) == 1) state <- matrix(state, nrow = 1)

  # segments break at the start of each period and after each run of subs
  brk <- (a$actionType == "period" & a$subType == "start") | (!is_sub & c(FALSE, is_sub[-n]))
  brk[1] <- TRUE
  seg <- cumsum(brk)

  sh <- carry_forward(suppressWarnings(as.integer(a$scoreHome)))
  sa <- carry_forward(suppressWarnings(as.integer(a$scoreAway)))
  dh <- pmax(diff(c(0L, sh)), 0L)
  da <- pmax(diff(c(0L, sa)), 0L)

  at <- a$actionType; stp <- a$subType; q <- coalesce(a$qualifiers, ""); tm <- a$teamId
  made <- coalesce(a$shotResult, "") == "Made"
  end_team <- rep(NA_real_, n)
  fg_made <- at %in% c("2pt", "3pt") & made
  end_team[fg_made] <- tm[fg_made]
  # 2019-20 writes these without spaces ("2of2")
  ft_final <- at == "freethrow" & made & str_remove_all(coalesce(stp, ""), " ") %in% c("2of2", "3of3")
  end_team[ft_final] <- tm[ft_final]
  tov <- at == "turnover"
  end_team[tov] <- tm[tov]
  dreb <- at == "rebound" & stp == "defensive" & !str_detect(q, "deadball")
  end_team[dreb] <- ifelse(tm[dreb] == home_id, away_id, home_id)

  seg_first <- match(unique(seg), seg)
  nseg <- length(seg_first)
  # a stint begins when its substitutions happened, not at the next logged
  # event, which can be many seconds later
  boundary <- ifelse(seg_first > 1 & is_sub[pmax(seg_first - 1, 1)], seg_first - 1, seg_first)
  seg_start <- el[boundary]
  seg_per <- per[seg_first]
  seg_end <- c(seg_start[-1], NA)
  last_in_per <- c(seg_per[-1] != seg_per[-nseg], TRUE)
  seg_end[last_in_per] <- (pstart + plen)[seg_first][last_in_per]

  sum_by <- function(x) as.numeric(tapply(x, seg, sum))
  lh <- matrix(NA_real_, nseg, 5); la <- matrix(NA_real_, nseg, 5)
  ok <- logical(nseg)
  on_h <- ids[team_of == home_id]; on_a <- ids[team_of == away_id]
  for (j in seq_len(nseg)) {
    col <- state[, seg_first[j]]
    h <- sort(ids[col == 1 & ids %in% on_h]); aw <- sort(ids[col == 1 & ids %in% on_a])
    if (length(h) == 5 && length(aw) == 5) {
      lh[j, ] <- h; la[j, ] <- aw; ok[j] <- TRUE
    }
  }

  tibble(
    game_id = a$game_id[1], seg = seq_len(nseg), period = seg_per, t_start = seg_start, t_end = seg_end
    , home_id = home_id, away_id = away_id
    , h1 = lh[, 1], h2 = lh[, 2], h3 = lh[, 3], h4 = lh[, 4], h5 = lh[, 5]
    , a1 = la[, 1], a2 = la[, 2], a3 = la[, 3], a4 = la[, 4], a5 = la[, 5]
    , pts_h = sum_by(dh), pts_a = sum_by(da)
    , poss_h = sum_by(!is.na(end_team) & end_team == home_id)
    , poss_a = sum_by(!is.na(end_team) & end_team == away_id)
    , margin_start = (sh - dh)[seg_first] - (sa - da)[seg_first]
    , lineup_ok = ok
  )
}

build_stints <- function(pbp_paths, pg) {
  map_dfr(pbp_paths, \(p) {
    s <- str_extract(p, "\\d{4}-\\d{2}")
    pbp <- read_csv(p, show_col_types = FALSE
      , col_types = cols(game_id = "c", clock = "c", actionType = "c", subType = "c", descriptor = "c"
          , qualifiers = "c", shotResult = "c", scoreHome = "c", scoreAway = "c", description = "c"
          , teamTricode = "c", periodType = "c", timeActual = "c", area = "c", .default = col_double())) |>
      mutate(game_id = str_pad(game_id, 10, pad = "0"))
    bx <- split(pg |> filter(season == s), pg$game_id[pg$season == s])
    pbp_split <- split(pbp, pbp$game_id)
    map_dfr(names(pbp_split), \(gid) build_game_stints(pbp_split[[gid]], bx[[gid]])) |>
      mutate(season = s)
  })
}

# Stints are validated game by game: every stint's lineup resolves to five,
# stint points add up to the final score, and every player's rebuilt minutes
# are within a minute of the box score. Games that fail are dropped from the
# model. Part of 2019-20 uses an older logging format (a substitution is a
# single "out" event naming the incoming player only in text, and lineup
# changes between periods are not logged), and those games fail here rather
# than being rebuilt by name matching. The hard gate is on the seasons the
# decomposition uses, where almost nothing should be dropped.
validate_stints <- function(stints, pg) {
  finals <- pg |> distinct(game_id, team_id, team_score)
  pts <- bind_rows(
      stints |> group_by(game_id, team_id = home_id) |> summarise(pts = sum(pts_h), .groups = "drop")
      , stints |> group_by(game_id, team_id = away_id) |> summarise(pts = sum(pts_a), .groups = "drop")
    ) |>
    inner_join(finals, by = c("game_id", "team_id")) |>
    group_by(game_id) |>
    summarise(points_match = all(pts == team_score), .groups = "drop")
  secs <- stints |>
    filter(lineup_ok) |>
    mutate(dur = (t_end - t_start) / 60) |>
    select(game_id, dur, h1:a5) |>
    pivot_longer(h1:a5, values_to = "person_id") |>
    group_by(game_id, person_id) |>
    summarise(min_stint = sum(dur), .groups = "drop") |>
    right_join(pg |> filter(played == 1) |> select(game_id, person_id, min), by = c("game_id", "person_id")) |>
    mutate(err = coalesce(min_stint, 0) - min)
  games <- stints |>
    group_by(season, game_id) |>
    summarise(lineup_fail = mean(!lineup_ok), .groups = "drop") |>
    left_join(pts, by = "game_id") |>
    left_join(secs |> group_by(game_id) |> summarise(max_err_min = max(abs(err)), .groups = "drop"), by = "game_id") |>
    mutate(keep = lineup_fail == 0 & coalesce(points_match, FALSE) & coalesce(max_err_min, Inf) <= 1)
  kept <- secs |> filter(game_id %in% games$game_id[games$keep])
  by_season <- games |>
    group_by(season) |>
    summarise(games_in_feed = n(), games_kept = sum(keep), share_dropped = mean(!keep), .groups = "drop")
  v <- list(
    keep = games$game_id[games$keep], by_season = by_season
    , n_stints = sum(stints$game_id %in% games$game_id[games$keep]), n_games = sum(games$keep)
    , minutes_mean_abs_err_sec = 60 * mean(abs(kept$err))
    , minutes_max_abs_err_sec = 60 * max(abs(kept$err))
    , err_quantiles_sec = 60 * quantile(abs(kept$err), c(0.5, 0.99, 0.999))
  )
  if (any(by_season$share_dropped[by_season$season %in% DECOMP_SEASONS] > 0.005)) {
    print(by_season)
    stop("stint validation dropped more than 0.5% of games in a decomposition season")
  }
  v
}

garbage_rule <- function(period, t_start, margin, starters_h, starters_a) {
  rem <- 2880 - t_start
  m <- abs(margin)
  period == 4 & starters_h <= 2 & starters_a <= 2 & (
    (rem > 540 & m >= 25) | (rem > 360 & rem <= 540 & m >= 20) | (rem <= 360 & m >= 10)
  )
}

# One row per team per stint, from that team's offensive point of view. Used
# both for the possession check and as the model's observations.
stint_team_rows <- function(stints, pg) {
  starter_keys <- pg |> filter(starter == 1) |> transmute(k = paste(game_id, person_id)) |> pull(k)
  # the 2020 restart was played at one site with no fans: no home team
  neutral <- pg |> distinct(game_id, game_date) |> filter(between(game_date, BUBBLE[1], BUBBLE[2])) |> pull(game_id)
  n_starters <- function(gid, cols) Reduce(`+`, map(cols, \(cl) as.integer(paste(gid, cl) %in% starter_keys)))
  st <- stints |>
    filter(lineup_ok, t_end > t_start | poss_h + poss_a > 0) |>
    mutate(
      stint_id = row_number()
      , garbage = garbage_rule(period, t_start, margin_start
          , n_starters(game_id, list(h1, h2, h3, h4, h5)), n_starters(game_id, list(a1, a2, a3, a4, a5)))
    )
  side <- function(off, def, pts, poss, home_off, o, d) {
    st |>
      transmute(stint_id, season, game_id, garbage, home_off = home_off
        , off_team = .data[[off]], def_team = .data[[def]], pts = .data[[pts]], poss = .data[[poss]]
        , o1 = .data[[o[1]]], o2 = .data[[o[2]]], o3 = .data[[o[3]]], o4 = .data[[o[4]]], o5 = .data[[o[5]]]
        , d1 = .data[[d[1]]], d2 = .data[[d[2]]], d3 = .data[[d[3]]], d4 = .data[[d[4]]], d5 = .data[[d[5]]])
  }
  bind_rows(
    side("home_id", "away_id", "pts_h", "poss_h", 0.5, paste0("h", 1:5), paste0("a", 1:5))
    , side("away_id", "home_id", "pts_a", "poss_a", -0.5, paste0("a", 1:5), paste0("h", 1:5))
  ) |>
    mutate(home_off = if_else(game_id %in% neutral, 0, home_off))
}

# Event-counted possessions run a near-constant 1.3% below nba.com's published
# estimate (itself a formula), with almost no spread across teams. Ratings in
# the write-up are rescaled onto the official scale; the model uses the counts.
#
# The feed does not have every game (about a quarter of 2019-20 is absent,
# mostly February 2020), so possessions are compared per game played in the
# data, and the net-rating comparison uses only team-seasons with at least
# 95% of their games present.
check_possessions <- function(team_rows, team_adv_path, pg, games_path) {
  official <- read_csv(team_adv_path, show_col_types = FALSE) |>
    clean_names() |>
    select(season, team_id, gp_official = gp, poss_official = poss, net_rating)
  covered <- team_rows |> distinct(season, team_id = off_team, game_id) |> count(season, team_id, name = "gp_data")
  scheduled <- read_csv(games_path, show_col_types = FALSE, col_types = cols(GAME_ID = "c", .default = col_guess())) |>
    distinct(season = SEASON, game_id = GAME_ID) |>
    count(season, name = "games_scheduled")
  mine <- team_rows |>
    group_by(season, team_id = off_team) |>
    summarise(pts = sum(pts), poss = sum(poss), .groups = "drop") |>
    left_join(team_rows |> group_by(season, team_id = def_team) |>
        summarise(opp_pts = sum(pts), opp_poss = sum(poss), .groups = "drop"), by = c("season", "team_id")) |>
    inner_join(official, by = c("season", "team_id")) |>
    left_join(covered, by = c("season", "team_id")) |>
    mutate(coverage = gp_data / gp_official
      , ratio = (poss / gp_data) / (poss_official / gp_official)
      , net = 100 * pts / poss - 100 * opp_pts / opp_poss)
  scale <- mine |> group_by(season) |> summarise(poss_ratio = mean(ratio), ratio_sd = sd(ratio), coverage = mean(coverage))
  complete <- mine |> filter(coverage >= 0.95)
  stopifnot(nrow(mine) == 30 * n_distinct(team_rows$season)
    , all(abs(scale$poss_ratio - 1) < 0.03), all(scale$ratio_sd < 0.02))
  list(scale = scale, teams = mine, coverage = team_rows |> distinct(season, game_id) |> count(season, name = "games_in_model") |>
      left_join(scheduled, by = "season")
    , net_cor = cor(complete$net, complete$net_rating), net_mae = mean(abs(complete$net - complete$net_rating)))
}


# Lottery-race label: bottom K by win% entering the game, at least min_gp
# games played. Standings come from each team's running record through the
# previous game date, so a game's own result never enters its label.
#
# Two more team-level quantities ride along:
#   late          the team has played min_gp games. The label can only switch
#                 on after that, so without a matching "late in the season"
#                 control the lottery coefficient partly measures early vs.
#                 late season (a first version's "defensive slippage" was
#                 exactly that).
#   exposure_prev the share of the team's games in the lottery race LAST
#                 season: an ex-ante measure of a tanking organisation that
#                 nothing in the current season can move. Pass the season
#                 before the first rated season so every rated season has one.
build_lottery <- function(games_path, seasons, K = 6, min_gp = 10) {
  games <- read_csv(games_path, show_col_types = FALSE, col_types = cols(GAME_ID = "c", .default = col_guess())) |>
    filter(SEASON %in% seasons) |>
    transmute(season = SEASON, team_id = TEAM_ID, game_id = str_pad(GAME_ID, 10, pad = "0")
      , game_date = as.Date(GAME_DATE), win = WL == "W") |>
    group_by(season) |>
    complete(team_id, game_date) |>
    group_by(season, team_id) |>
    arrange(game_date, .by_group = TRUE) |>
    mutate(gp_before = lag(cumsum(!is.na(win)), default = 0L), w_before = lag(cumsum(coalesce(win, FALSE)), default = 0L)) |>
    group_by(season, game_date) |>
    mutate(bottom_rank = rank(if_else(gp_before > 0, w_before / gp_before, 0.5), ties.method = "min")) |>
    ungroup() |>
    filter(!is.na(game_id)) |>
    mutate(lottery = as.integer(gp_before >= min_gp & bottom_rank <= K), late = as.integer(gp_before >= min_gp))
  exposure <- games |>
    group_by(season, team_id) |>
    summarise(exposure = mean(lottery), .groups = "drop") |>
    mutate(next_season = SEASONS_ALL[match(season, SEASONS_ALL) + 1])
  games |>
    left_join(exposure |> select(season = next_season, team_id, exposure_prev = exposure), by = c("season", "team_id")) |>
    select(season, game_id, team_id, game_date, gp_before, bottom_rank, lottery, late, exposure_prev)
}

label_rows <- function(team_rows, lottery) {
  lot <- lottery |> select(game_id, team_id, lottery, late, exposure_prev)
  out <- team_rows |>
    left_join(lot |> rename(off_team = team_id, L_off = lottery, late_off = late, prev_off = exposure_prev), by = c("game_id", "off_team")) |>
    left_join(lot |> rename(def_team = team_id, L_def = lottery, late_def = late, prev_def = exposure_prev), by = c("game_id", "def_team"))
  stopifnot(!anyNA(out$L_off), !anyNA(out$L_def), !anyNA(out$prev_off), !anyNA(out$prev_def))
  out
}

# ---- Stage 3: adjusted plus-minus with team-season context ---------------------
# Each stint gives two observations, one per offense. For offense team t
# against defense team u in season s,
#
#   100 * pts / poss = mu_s + h_s * home
#                    + sum over the five offensive players of O[p,s]
#                    - sum over the five defensive players of D[q,s]
#                    + cO[t,s] - cD[u,s]
#                    + bO * L[t,g] - bD * L[u,g]          lottery-race stretch
#                    + lO * late[t,g] - lD * late[u,g]    past game 10
#                    + eO * prev[t,s] - eD * prev[u,s]    last season's exposure
#                    + noise with variance sigma^2 / poss
#
# O[p,s] = box-score prior mean (z'gamma) + a[p] (career) + e[p,s] (season).
# Be clear about what that makes the ratings: the box-score prior carries
# most of each rating, and the lineup data mainly set the prior's weights and
# adjust players with enough minutes. The share is reported, not assumed.
#
# Given the variance components the posterior is exactly multivariate normal.
# The components are set by maximising the marginal likelihood.

# Box-score prior features, one row per player-season. Rates are shrunk toward
# the league rate with 300 pseudo-minutes. Minutes per game is deliberately
# not a feature: it is the deployment decision being separated from talent.
# Fouls drawn, paint points and 3P% were dropped after a first fit: the first
# two correlate above 0.9 with free-throw attempts and points and let the prior
# put a +2 defensive weight on drawing fouls; 3P% on these samples is noise.
AGE_FEATURES <- c("age", "age2", "log_pick", "experience")

build_box_prior <- function(pg, bio_path, player_index) {
  bio <- read_csv(bio_path, show_col_types = FALSE) |>
    clean_names() |>
    group_by(person_id = player_id, season) |>
    summarise(age = first(age), height_in = first(player_height_inches), .groups = "drop")
  rate_cols <- c("points", "fieldGoalsAttempted", "threePointersAttempted", "freeThrowsAttempted", "assists"
    , "turnovers", "reboundsOffensive", "reboundsDefensive", "steals", "blocks", "foulsPersonal")
  box_ps <- pg |>
    filter(played == 1, min > 0) |>
    group_by(person_id, season, season_start) |>
    summarise(name = last(name), min = sum(min), gp = n(), threePointersMade = sum(threePointersMade, na.rm = TRUE)
      , across(all_of(rate_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  lg <- box_ps |> group_by(season) |> summarise(across(all_of(rate_cols), \(x) sum(x) / sum(min)), .groups = "drop")
  m0 <- 300
  box_ps <- box_ps |>
    left_join(lg |> rename_with(\(x) paste0("lg_", x), -season), by = "season") |>
    mutate(
      across(all_of(rate_cols), \(x) 36 * (x + m0 * get(paste0("lg_", cur_column()))) / (min + m0), .names = "r_{.col}")
      , lg_ts = lg_points / (2 * (lg_fieldGoalsAttempted + 0.44 * lg_freeThrowsAttempted))
      , ts_shr = (points + 2 * 150 * lg_ts) / (2 * (fieldGoalsAttempted + 0.44 * freeThrowsAttempted + 150))
    ) |>
    left_join(bio, by = c("person_id", "season")) |>
    left_join(player_index |> select(person_id, height_idx, draft_number, from_year), by = "person_id") |>
    mutate(height = coalesce(height_in, height_idx), log_pick = log(pmin(coalesce(draft_number, 70), 70))
      , experience = coalesce(pmax(season_start - from_year, 0), 0)) |>
    group_by(season) |>
    mutate(age = coalesce(age, mean(age, na.rm = TRUE)), height = coalesce(height, mean(height, na.rm = TRUE))) |>
    ungroup()

  feat_cols <- c(paste0("r_", rate_cols), "ts_shr", "height", "age", "log_pick", "experience")
  Zraw <- as.matrix(box_ps[feat_cols])
  wmin <- box_ps$min / sum(box_ps$min)
  z_mean <- colSums(Zraw * wmin)
  z_sd <- sqrt(colSums(sweep(Zraw, 2, z_mean)^2 * wmin))
  Z <- sweep(sweep(Zraw, 2, z_mean), 2, z_sd, "/")
  Z <- cbind(Z, age2 = Z[, "age"]^2 - mean(Z[, "age"]^2))
  cz <- cor(Z)
  list(box_ps = box_ps, Z = Z, z_mean = z_mean, z_sd = z_sd
    , feature_cor_max = max(abs(cz[upper.tri(cz)])), condition = kappa(Z, exact = TRUE))
}

# Roles from what players do, not nba.com's listed position (which calls a
# 7-footer who plays center a forward and a 6'6" wing a guard). Two stages,
# each season:
#   size    height plus half each of rebounding and shot-blocking; the top 20%
#           of minutes by size are bigs (one on the floor)
#   handle  assists minus twice height, among everyone else; the top half of
#           their minutes are guards and the rest wings (two and two)
# A first version used one score (height and rebounding and blocks, minus
# assists) and cut it at the 40th and 80th percentiles; assists outweighed
# size, so Nikola Jokic, Nikola Vucevic and Domantas Sabonis came out as guards
# or wings. The two-stage rule classes 95% of listed centers' minutes as bigs.
# Players with no box-score line fall back to their listed position.
weighted_quantile <- function(x, w, probs) {
  o <- order(x); cw <- cumsum(w[o]) / sum(w)
  map_dbl(probs, \(p) x[o][which(cw >= p)[1]])
}

listed_role <- function(pos) {
  case_when(pos %in% c("G") ~ "Guard", pos %in% c("G-F", "F-G", "F") ~ "Wing"
    , pos %in% c("F-C", "C-F", "C") ~ "Big", TRUE ~ "Wing")
}

assign_roles <- function(box, player_index) {
  bp <- box$box_ps
  zw <- function(x, w) (x - weighted.mean(x, w)) / sqrt(sum(w * (x - weighted.mean(x, w))^2) / sum(w))
  bp |>
    group_by(season) |>
    mutate(
      size = zw(height, min) + 0.5 * zw(r_reboundsOffensive + r_reboundsDefensive, min) + 0.5 * zw(r_blocks, min)
      , is_big = size > weighted_quantile(size, min, 0.8)
      , handle = zw(r_assists, min) - 2 * zw(height, min)
    ) |>
    group_by(season, is_big) |>
    mutate(guard_cut = weighted_quantile(handle, min, 0.5)) |>
    ungroup() |>
    mutate(role = case_when(is_big ~ "Big", handle >= guard_cut ~ "Guard", TRUE ~ "Wing")) |>
    select(person_id, season, name, role, size, handle, height, r_assists, r_blocks, r_threePointersAttempted
      , threePointersMade, threePointersAttempted, min)
}

role_of <- function(person_ids, season_, roles, player_index) {
  r <- roles |> filter(season == season_) |> select(person_id, role)
  tibble(person_id = person_ids) |>
    left_join(r, by = "person_id") |>
    left_join(player_index |> select(person_id, position), by = "person_id") |>
    mutate(role = coalesce(role, listed_role(position))) |>
    pull(role)
}

rapm_system <- function(dir_rows, box, seasons, drop_garbage = TRUE) {
  box_ps <- box$box_ps |> filter(season %in% seasons)
  Z <- box$Z[box$box_ps$season %in% seasons, , drop = FALSE]
  K <- ncol(Z)
  fit_rows <- dir_rows |> filter(season %in% seasons, poss > 0, !(drop_garbage & garbage))
  stopifnot(length(seasons) > 1)   # the career term needs more than one season

  ps_tab <- box_ps |> transmute(ps = row_number(), person_id, season, name, min)
  player_tab <- ps_tab |> distinct(person_id) |> mutate(pl = row_number())
  ps_tab <- ps_tab |> left_join(player_tab, by = "person_id")
  ts_tab <- fit_rows |> distinct(season, team_id = off_team) |> arrange(season, team_id) |> mutate(ts = row_number())

  ps_key <- paste(ps_tab$person_id, ps_tab$season)
  O_ps <- sapply(paste0("o", 1:5), \(cl) match(paste(fit_rows[[cl]], fit_rows$season), ps_key))
  D_ps <- sapply(paste0("d", 1:5), \(cl) match(paste(fit_rows[[cl]], fit_rows$season), ps_key))
  if (anyNA(O_ps) || anyNA(D_ps)) stop("stint player missing from box-score player-seasons")
  O_pl <- matrix(ps_tab$pl[O_ps], ncol = 5)
  D_pl <- matrix(ps_tab$pl[D_ps], ncol = 5)
  n <- nrow(fit_rows); PS <- nrow(ps_tab); PL <- nrow(player_tab); TS <- nrow(ts_tab); S <- length(seasons)

  blocks <- list(mu = S, home = S, lot_O = 1, lot_D = 1, late_O = 1, late_D = 1, env_O = 1, env_D = 1
    , gam_O = K, gam_D = K, a_O = PL, e_O = PS, a_D = PL, e_D = PS, c_O = TS, c_D = TS)
  off <- setNames(cumsum(c(0, unlist(blocks)))[seq_along(blocks)], names(blocks))
  p_total <- sum(unlist(blocks))

  ri <- seq_len(n)
  si <- match(fit_rows$season, seasons)
  ts_off <- match(paste(fit_rows$season, fit_rows$off_team), paste(ts_tab$season, ts_tab$team_id))
  ts_def <- match(paste(fit_rows$season, fit_rows$def_team), paste(ts_tab$season, ts_tab$team_id))
  Zsum_O <- Z[O_ps[, 1], ] + Z[O_ps[, 2], ] + Z[O_ps[, 3], ] + Z[O_ps[, 4], ] + Z[O_ps[, 5], ]
  Zsum_D <- Z[D_ps[, 1], ] + Z[D_ps[, 2], ] + Z[D_ps[, 3], ] + Z[D_ps[, 4], ] + Z[D_ps[, 5], ]
  trip <- list(
    list(i = ri, j = off["mu"] + si, x = 1)
    , list(i = ri, j = off["home"] + si, x = fit_rows$home_off)
    , list(i = ri, j = off["lot_O"] + 1, x = fit_rows$L_off)
    , list(i = ri, j = off["lot_D"] + 1, x = -fit_rows$L_def)
    , list(i = ri, j = off["late_O"] + 1, x = fit_rows$late_off)
    , list(i = ri, j = off["late_D"] + 1, x = -fit_rows$late_def)
    , list(i = ri, j = off["env_O"] + 1, x = fit_rows$prev_off)
    , list(i = ri, j = off["env_D"] + 1, x = -fit_rows$prev_def)
    , list(i = rep(ri, K), j = off["gam_O"] + rep(seq_len(K), each = n), x = as.vector(Zsum_O))
    , list(i = rep(ri, K), j = off["gam_D"] + rep(seq_len(K), each = n), x = -as.vector(Zsum_D))
    , list(i = rep(ri, 5), j = off["a_O"] + as.vector(O_pl), x = 1)
    , list(i = rep(ri, 5), j = off["e_O"] + as.vector(O_ps), x = 1)
    , list(i = rep(ri, 5), j = off["a_D"] + as.vector(D_pl), x = -1)
    , list(i = rep(ri, 5), j = off["e_D"] + as.vector(D_ps), x = -1)
    , list(i = ri, j = off["c_O"] + ts_off, x = 1)
    , list(i = ri, j = off["c_D"] + ts_def, x = -1)
  ) |>
    map(\(t) list(i = t$i, j = rep_len(t$j, length(t$i)), x = rep_len(t$x, length(t$i))))
  X <- sparseMatrix(i = unlist(map(trip, "i")), j = unlist(map(trip, "j")), x = unlist(map(trip, "x"))
    , dims = c(n, p_total))
  w <- fit_rows$poss
  y_raw <- 100 * fit_rows$pts / fit_rows$poss
  XtW <- t(X * w)
  sys <- list(
    X = X, XtW = XtW, A = forceSymmetric(XtW %*% X), w = w, y_raw = y_raw, n = n
    , sum_log_w = sum(log(w)), blocks = blocks, off = off, p_total = p_total
    , ps_tab = ps_tab, ts_tab = ts_tab, box_ps = box_ps, Z = Z, seasons = seasons
    , fit_rows = fit_rows |> select(season, game_id, off_team, def_team, poss, L_off, L_def, prev_off, prev_def)
  )
  set_response(sys, y_raw)
}

# the response enters only through b0 and yWy, so a planted change to y needs
# no new factorisation
set_response <- function(sys, y_raw) {
  sys$y_center <- sum(sys$w * y_raw) / sum(sys$w)
  y <- y_raw - sys$y_center
  sys$b0 <- as.vector(sys$XtW %*% y)
  sys$yWy <- sum(sys$w * y^2)
  sys
}

prior_precision <- function(sys, tau) {
  d <- numeric(sys$p_total)
  set <- function(blk, v) d[sys$off[blk] + seq_len(sys$blocks[[blk]])] <<- v
  for (b in c("mu", "home", "lot_O", "lot_D", "late_O", "late_D", "env_O", "env_D")) set(b, 1 / 5^2)
  set("gam_O", 1 / 2^2); set("gam_D", 1 / 2^2)
  set("a_O", 1 / tau[["aO"]]^2); set("e_O", 1 / tau[["eO"]]^2)
  set("a_D", 1 / tau[["aD"]]^2); set("e_D", 1 / tau[["eD"]]^2)
  set("c_O", 1 / tau[["cO"]]^2); set("c_D", 1 / tau[["cD"]]^2)
  d
}

TAU_NAMES <- c("aO", "eO", "aD", "eD", "cO", "cD")

rapm_factor <- function(sys, sigma, tau, ch = NULL) {
  P <- sys$A / sigma^2 + Diagonal(sys$p_total, prior_precision(sys, tau))
  if (is.null(ch)) Cholesky(P, LDL = FALSE, perm = TRUE, super = TRUE) else update(ch, P)
}

neg_log_ml <- function(sys, ch, sigma, tau) {
  s2 <- sigma^2
  lam <- prior_precision(sys, tau)
  b <- sys$b0 / s2
  quad <- sum(b * as.vector(solve(ch, b, system = "A")))
  logdetP <- as.numeric(determinant(ch, sqrt = FALSE)$modulus)
  0.5 * (sys$n * log(s2) - sys$sum_log_w + sys$yWy / s2 - quad + logdetP - sum(log(lam)))
}

# Variance components. A first version ran L-BFGS-B on all seven components at
# once and stopped short: sigma is pinned about a million times more tightly
# than the prior SDs, so a shared finite-difference step is useless for one or
# the other. Here sigma (1-D Brent) and the six prior SDs (Nelder-Mead on the
# log scale) are optimised in alternation until neither moves, and the result
# is checked with central-difference gradients. A component that runs to the
# lower bound is recorded as collapsed — the data say that term is not needed
# — rather than treated as an error.
optimize_hyper <- function(sys, init = NULL, lower = log(1e-3), upper = log(20), max_passes = 4, maxit = c(500, 200)) {
  tau <- if (is.null(init)) c(aO = 0.5, eO = 0.3, aD = 1, eD = 0.3, cO = 0.5, cD = 0.5) else init[TAU_NAMES]
  sigma <- if (is.null(init)) sqrt(sys$yWy / sys$n) else init[["sigma"]]
  ch <- rapm_factor(sys, sigma, tau)
  evals <- 0
  f <- function(ls, lt) {
    lt <- pmin(pmax(lt, lower), upper)
    tt <- setNames(exp(lt), TAU_NAMES)
    ch <<- rapm_factor(sys, exp(ls), tt, ch)
    evals <<- evals + 1
    neg_log_ml(sys, ch, exp(ls), tt)
  }
  lt <- log(tau); ls <- log(sigma)
  value <- f(ls, lt)
  trace <- tibble()
  for (pass in seq_len(max_passes)) {
    old <- value
    ls <- optimize(\(x) f(x, lt), interval = ls + c(-0.1, 0.1), tol = 1e-6)$minimum
    nm <- optim(lt, \(p) f(ls, p), method = "Nelder-Mead", control = list(maxit = if (pass == 1) maxit[1] else maxit[2], reltol = 1e-10))
    lt <- pmin(pmax(nm$par, lower), upper)
    value <- f(ls, lt)
    trace <- bind_rows(trace, tibble(pass = pass, value = value, evals = evals, sigma = exp(ls), !!!setNames(as.list(exp(lt)), TAU_NAMES)))
    if (abs(old - value) < 1e-3) break
  }
  # central-difference gradient on the log scale; collapsed components are
  # at the bound, where the gradient is one-sided by construction
  h <- 0.05
  collapsed <- lt <= lower + 1e-6
  grad <- map_dbl(seq_along(lt), \(k) {
    if (collapsed[k]) return(NA_real_)
    e <- replace(numeric(length(lt)), k, h)
    (f(ls, lt + e) - f(ls, lt - e)) / (2 * h)
  })
  grad_sigma <- (f(ls + 1e-4, lt) - f(ls - 1e-4, lt)) / 2e-4
  f(ls, lt)
  gate <- all(abs(grad[!collapsed]) < 0.5)
  if (!gate) stop("variance components not at an optimum: gradients ", paste(round(grad, 3), collapse = ", "))
  list(hyper = c(sigma = exp(ls), setNames(exp(lt), TAU_NAMES)), value = value, evals = evals, trace = trace
    , gradient = setNames(grad, TAU_NAMES), gradient_sigma = grad_sigma, collapsed = TAU_NAMES[collapsed])
}

rapm_posterior <- function(sys, hyper, n_draws = 1000, ch = NULL) {
  ch <- rapm_factor(sys, hyper[["sigma"]], hyper[TAU_NAMES], ch)
  s2 <- hyper[["sigma"]]^2
  post_mean <- as.vector(solve(ch, sys$b0 / s2, system = "A"))
  zmat <- matrix(rnorm(sys$p_total * n_draws), sys$p_total, n_draws)
  theta <- post_mean + as.matrix(solve(ch, solve(ch, zmat, system = "Lt"), system = "Pt"))
  blk <- function(name, m = theta) m[sys$off[name] + seq_len(sys$blocks[[name]]), , drop = FALSE]
  bmean <- function(name) post_mean[sys$off[name] + seq_len(sys$blocks[[name]])]
  Z <- sys$Z; ps_tab <- sys$ps_tab
  O_draws <- Z %*% blk("gam_O") + blk("a_O")[ps_tab$pl, ] + blk("e_O")
  D_draws <- Z %*% blk("gam_D") + blk("a_D")[ps_tab$pl, ] + blk("e_D")
  age_cols <- colnames(Z) %in% AGE_FEATURES
  gO <- bmean("gam_O"); gD <- bmean("gam_D")
  std_resid <- (sys$y_raw - sys$y_center - as.vector(sys$X %*% post_mean)) * sqrt(sys$w) / hyper[["sigma"]]
  list(
    hyper = hyper, seasons = sys$seasons, feat_cols = colnames(Z), post_mean = post_mean
    , ps_tab = ps_tab |> mutate(
        O_mean = rowMeans(O_draws), O_sd = apply(O_draws, 1, sd), D_mean = rowMeans(D_draws), D_sd = apply(D_draws, 1, sd)
        # the part of each rating the box-score prior supplies, and the part
        # that comes from age, experience and draft slot alone
        , O_box = as.vector(Z %*% gO), D_box = as.vector(Z %*% gD)
        , O_age = as.vector(Z[, age_cols] %*% gO[age_cols]), D_age = as.vector(Z[, age_cols] %*% gD[age_cols]))
    , ts_tab = sys$ts_tab |> mutate(cO_mean = rowMeans(blk("c_O")), cO_sd = apply(blk("c_O"), 1, sd)
        , cD_mean = rowMeans(blk("c_D")), cD_sd = apply(blk("c_D"), 1, sd))
    , O_draws = O_draws, D_draws = D_draws, cO_draws = blk("c_O"), cD_draws = blk("c_D")
    , lot_draws = rbind(O = blk("lot_O")[1, ], D = blk("lot_D")[1, ])
    , late_draws = rbind(O = blk("late_O")[1, ], D = blk("late_D")[1, ])
    , env_draws = rbind(O = blk("env_O")[1, ], D = blk("env_D")[1, ])
    , gam = list(O = blk("gam_O"), D = blk("gam_D")), home = blk("home") |> `rownames<-`(sys$seasons)
    , box_ps = sys$box_ps
    , resid = list(sample = sample(std_resid, 20000), sd = sd(std_resid), n = sys$n
        , kurtosis = mean(std_resid^4) / mean(std_resid^2)^2
        , quantiles = quantile(std_resid, c(0.001, 0.01, 0.05, 0.5, 0.95, 0.99, 0.999)))
    , prior_sd = list(context = hyper[c("cO", "cD")])
  )
}

fit_rapm <- function(dir_rows, box, seasons, hyper = NULL, init = NULL, drop_garbage = TRUE, n_draws = 1000, ...) {
  sys <- rapm_system(dir_rows, box, seasons, drop_garbage)
  opt <- NULL
  if (is.null(hyper)) {
    opt <- optimize_hyper(sys, init = init, ...)
    hyper <- opt$hyper
  }
  out <- rapm_posterior(sys, hyper, n_draws)
  out$opt <- opt
  out
}

# The parts of a fit the write-up reads, without the draw matrices
rapm_report <- function(rapm) {
  big <- rapm$ps_tab |> filter(min >= 500)
  rapm[c("hyper", "seasons", "feat_cols", "ps_tab", "ts_tab", "lot_draws", "late_draws", "env_draws", "home", "resid", "opt")] |>
    c(list(
      gam = map(rapm$gam, \(g) tibble(feature = rapm$feat_cols, mean = rowMeans(g), sd = apply(g, 1, sd)))
      , box_share = c(O = cor(big$O_box, big$O_mean)^2, D = cor(big$D_box, big$D_mean)^2)
      , context_shrink = c(O = median(rapm$ts_tab$cO_sd) / rapm$hyper[["cO"]], D = median(rapm$ts_tab$cD_sd) / rapm$hyper[["cD"]])
    ))
}

# ---- Stage 3b: what the model can and cannot see --------------------------------
# Planted effects. The variance components and the factorisation stay fixed and
# only the response changes, so each plant costs one solve. Each plant is a
# known shift in points per 100 added to the stint outcomes; the question is
# where the model puts it.
calibrate_rapm <- function(dir_rows, box, seasons, hyper, pg) {
  sys <- rapm_system(dir_rows, box, seasons)
  ch <- rapm_factor(sys, hyper[["sigma"]], hyper[TAU_NAMES])
  s2 <- hyper[["sigma"]]^2
  solve_mean <- function(y_raw) {
    s <- set_response(sys, y_raw)
    as.vector(solve(ch, s$b0 / s2, system = "A"))
  }
  base <- solve_mean(sys$y_raw)
  fr <- sys$fit_rows
  was_seasons <- intersect(DECOMP_SEASONS, seasons)
  coef <- function(m, name) m[sys$off[name] + 1]
  # Washington's minutes-weighted team talent (sum of five) per season
  was_w <- pg |>
    filter(team_id == WIZARDS_ID, season %in% was_seasons, min > 0) |>
    group_by(season, person_id) |>
    summarise(m = sum(min), .groups = "drop") |>
    group_by(season) |>
    mutate(wt = 5 * m / sum(m)) |>
    ungroup() |>
    left_join(sys$ps_tab |> select(person_id, season, ps, pl), by = c("person_id", "season")) |>
    filter(!is.na(ps))
  # each Washington quantity is averaged over the seasons a plant touches, so a
  # one-season plant is not diluted across five
  talent_of <- function(m, ss) {
    blkm <- function(name) m[sys$off[name] + seq_len(sys$blocks[[name]])]
    O <- as.vector(sys$Z %*% blkm("gam_O")) + blkm("a_O")[sys$ps_tab$pl] + blkm("e_O")
    D <- as.vector(sys$Z %*% blkm("gam_D")) + blkm("a_D")[sys$ps_tab$pl] + blkm("e_D")
    was_w |> filter(season %in% ss) |> mutate(v = wt * (O[ps] + D[ps])) |>
      group_by(season) |> summarise(t = sum(v), .groups = "drop") |> pull(t) |> mean()
  }
  context_of <- function(m, ss) {
    k <- sys$ts_tab |> filter(team_id == WIZARDS_ID, season %in% ss) |> pull(ts)
    mean(m[sys$off["c_O"] + k] + m[sys$off["c_D"] + k])
  }
  # Washington's exposure to the two league-wide terms, possession-weighted
  exposure_of <- function(col, ss) {
    rr <- fr$off_team == WIZARDS_ID & fr$season %in% ss
    weighted.mean(fr[[col]][rr], fr$poss[rr])
  }
  plant <- function(label, scope, delta, planted, ss = was_seasons) {
    m <- solve_mean(sys$y_raw + delta)
    d_env <- (coef(m, "env_O") - coef(base, "env_O")) + (coef(m, "env_D") - coef(base, "env_D"))
    d_lot <- (coef(m, "lot_O") - coef(base, "lot_O")) + (coef(m, "lot_D") - coef(base, "lot_D"))
    tibble(
      plant = label, scope = scope, planted_net = planted
      , recovered_env = d_env, recovered_lottery = d_lot
      , was_context = context_of(m, ss) - context_of(base, ss)
      , was_talent = talent_of(m, ss) - talent_of(base, ss)
      , was_league_terms = d_env * exposure_of("prev_off", ss) + d_lot * exposure_of("L_off", ss)
      , was_prev_exposure = exposure_of("prev_off", ss), was_lottery_share = exposure_of("L_off", ss)
    )
  }
  is_was_off <- fr$off_team == WIZARDS_ID & fr$season %in% was_seasons
  is_was_def <- fr$def_team == WIZARDS_ID & fr$season %in% was_seasons
  is_was_off26 <- fr$off_team == WIZARDS_ID & fr$season == "2025-26"
  is_was_def26 <- fr$def_team == WIZARDS_ID & fr$season == "2025-26"
  bind_rows(
    # -1 scored and +1 allowed per 100 for each full season of last-year exposure
    plant("Teams that tanked last season play 2 points worse", "league", -fr$prev_off + fr$prev_def, -2)
    , plant("Lottery-race stretches play 2 points worse", "league", -fr$L_off + fr$L_def, -2)
    , plant("Washington only, every season since 2021-22, 2 points worse", "was", -is_was_off + is_was_def, -2)
    , plant("Washington only, 2025-26 only, 2 points worse", "was26", -is_was_off26 + is_was_def26, -2, ss = "2025-26")
  ) |>
    mutate(
      # for the Washington plants, what the model never attributes to anything
      residual = if_else(scope == "league", NA_real_, planted_net - (was_context + was_talent + was_league_terms))
    )
}

# ---- Stage 4: the deployment decomposition ------------------------------------
# For every team-season, net rating is split into additive pieces, each linear
# in the posterior draws so its uncertainty comes along:
#
#   talent        minutes-weighted O + D of the roster, if a typical
#                 non-lottery coach had split the minutes and every listed
#                 player had been available
#   availability  what absences cost, whatever the league labelled them
#   allocation    how minutes were split among the players who dressed,
#                 against a typical non-lottery coach with the same players
#   lottery race  the league-wide shift during bottom-six stretches
#   tank env.     the league-wide shift for teams that tanked last season
#   context       this team-season's own context effect
#   opponents     schedule: the talent, context and tank status of opponents
#   other         home/road balance, garbage time, season timing, and what
#                 the model misses
#
# Every piece is a deviation from the league average of that piece, so they
# sum to the team's actual net rating.
#
# The typical coach. Each night's eligible dressed players are grouped by role
# (guard / wing / big, from what they do rather than nba.com's listing),
# ranked within role by rating, and given the average share of that role's
# minutes that rank gets in non-lottery games. The minutes each role gets are
# the league's typical mix for non-lottery games, not what the team actually
# played: a first version held each team's actual positional minutes fixed,
# which cannot put a missing center back on the floor (Washington's listed
# "bigs" played 7% of its minutes in 2025-26 against a league 19%, so an
# available Anthony Davis could only have taken Alex Sarr's minutes). If no
# eligible player fills a role that night, its minutes go to the other roles.
# Players with fewer than `min_elig` minutes that season keep their actual
# minutes: a player the coach barely used has a rating shrunk toward his box
# score, and moving minutes to him on that estimate would flatter the
# counterfactual.

# Absences the league labels as team choices on their face
discretionary_absence <- function(reason, desc) {
  r <- coalesce(reason, ""); d <- str_to_lower(coalesce(desc, ""))
  str_detect(r, "REST|COACH|GLEAGUE_ON_ASSIGNMENT|RECONDITIONING") |
    r %in% c("INACTIVE_NOT_WITH_TEAM", "NWT_NOT_WITH_TEAM") |
    (str_detect(r, "INJURY") & str_detect(d, "management|\\brest\\b|recondition|load"))
}

player_game_deployment <- function(pg, rapm, lottery, player_index, roles, min_elig) {
  season_min <- pg |> group_by(person_id, season) |> summarise(season_min = sum(min), .groups = "drop")
  order_games <- pg |>
    distinct(season, team_id, game_id, game_date) |>
    group_by(season, team_id) |>
    arrange(game_date, .by_group = TRUE) |>
    mutate(game_no = row_number(), n_games = n()) |>
    ungroup()
  pg |>
    left_join(rapm$ps_tab |> transmute(person_id, season, ps, v = O_mean + D_mean, v_noage = v - O_age - D_age)
      , by = c("person_id", "season")) |>
    left_join(roles |> select(person_id, season, role), by = c("person_id", "season")) |>
    select(-any_of("position")) |>   # the box score's starting slot, not a position
    left_join(player_index |> select(person_id, position, pos_group), by = "person_id") |>
    left_join(lottery |> select(game_id, team_id, lottery), by = c("game_id", "team_id")) |>
    left_join(season_min, by = c("person_id", "season")) |>
    left_join(order_games, by = c("season", "team_id", "game_id", "game_date")) |>
    mutate(
      dressed = status == "ACTIVE" & !str_detect(coalesce(not_playing_reason, ""), "^(DND|NWT)")
      , structural = str_detect(coalesce(not_playing_reason, ""), "GLEAGUE_TWOWAY|TRADE|SUSPENSION")
      , discretionary = !dressed & discretionary_absence(not_playing_reason, not_playing_description)
      , absent_any = !dressed & !is.na(not_playing_reason) & !structural
      , role = coalesce(role, listed_role(position))
      , pos_group = coalesce(pos_group, "Forward")
      , eligible = coalesce(season_min, 0) >= min_elig
    ) |>
    filter(!is.na(ps))
}

# Absences flagged as tank-driven by behaviour rather than label. The league's
# codes cannot tell a real injury from a convenient one (Anthony Davis's 31
# games are all "Left Finger; Sprain"), and lottery-race teams label a smaller
# share of absences as rest than other teams do. Three tests, each independent
# of the label, with non-lottery teams as the baseline so ordinary injuries
# are not counted:
#   acquisition  joined a lottery-race team mid-season and dressed for under a
#                quarter of its remaining games
#   shutdown     one of the team's five best players (by rating, listed 20+
#                games), not dressed in the final 25 games of a season the team
#                entered in the lottery race
#   long run     for minor injury labels only (sprains, strains, soreness,
#                contusions and the like — never fractures, surgeries, tears
#                or clots), the part of an absence that runs past the 90th
#                percentile length of absences with the same kind of label on
#                non-lottery teams, while the team was in the lottery race
# Together with the label-based absences, these make the upper end of the
# "tank share of availability" range; the label-based absences alone make the
# lower end. The upper end deliberately leans generous — some shutdowns are
# real injuries — which is why it is reported as a range.
tank_absence_flags <- function(pgd) {
  cand <- pgd |> filter(!dressed, !structural)
  # acquisition
  stints_team <- pgd |>
    group_by(season, person_id, team_id) |>
    summarise(first_game = min(game_no), first_date = min(game_date), dressed_share = mean(dressed)
      , lottery_at_join = lottery[which.min(game_no)], .groups = "drop") |>
    group_by(season, person_id) |>
    mutate(joined_mid = first_game > 20 | first_date > min(first_date)) |>
    ungroup()
  acq <- cand |>
    semi_join(stints_team |> filter(joined_mid, lottery_at_join == 1, dressed_share < 0.25), by = c("season", "person_id", "team_id")) |>
    transmute(game_id, team_id, person_id, flag = "acquisition")
  # shutdown
  late_teams <- pgd |>
    filter(game_no == n_games - 24) |>
    distinct(season, team_id, late_lottery = lottery)
  top5 <- pgd |>
    group_by(season, team_id, person_id) |>
    summarise(listed = n(), v = first(v), .groups = "drop") |>
    filter(listed >= 20) |>
    group_by(season, team_id) |>
    slice_max(v, n = 5, with_ties = FALSE) |>
    ungroup()
  shut <- cand |>
    semi_join(top5, by = c("season", "team_id", "person_id")) |>
    inner_join(late_teams |> filter(late_lottery == 1), by = c("season", "team_id")) |>
    filter(game_no > n_games - 25) |>
    transmute(game_id, team_id, person_id, flag = "shutdown")
  # long injury runs
  runs <- pgd |>
    arrange(season, team_id, person_id, game_no) |>
    group_by(season, team_id, person_id) |>
    mutate(
      inj = !dressed & str_detect(coalesce(not_playing_reason, ""), "INJURY")
      , run_id = cumsum(inj & !lag(inj, default = FALSE))
    ) |>
    ungroup() |>
    filter(inj) |>
    group_by(season, team_id, person_id, run_id) |>
    mutate(
      run_pos = row_number(), run_len = n()
      , label = str_to_lower(paste(unique(coalesce(not_playing_description, "")), collapse = " "))
      , serious = str_detect(label, "fractur|surger|tear|torn|ruptur|thrombo|dvt|acl|achilles|dislocat|concussion|reconstruct|repair|stress reaction|clot|embol|cardiac|heart")
      , part = case_when(
          str_detect(label, "sprain") ~ "sprain", str_detect(label, "strain") ~ "strain"
          , str_detect(label, "contusion|bruise") ~ "contusion", str_detect(label, "sore|tight|stiff|spasm|pain") ~ "soreness"
          , str_detect(label, "tendin|bursitis|inflamm") ~ "tendon or bursa", str_detect(label, "illness") ~ "illness"
          , TRUE ~ "other")
    ) |>
    ungroup() |>
    filter(!serious)
  team_lot <- pgd |> distinct(season, team_id, game_id, lottery) |> group_by(season, team_id) |> summarise(ts_lot = mean(lottery), .groups = "drop")
  base <- runs |>
    distinct(season, team_id, person_id, run_id, part, run_len) |>
    left_join(team_lot, by = c("season", "team_id")) |>
    filter(ts_lot < 0.25)
  overall_p90 <- quantile(base$run_len, 0.9)
  p90 <- base |> group_by(part) |> summarise(n = n(), p90 = if (n() >= 15) quantile(run_len, 0.9) else overall_p90, .groups = "drop")
  long <- runs |>
    left_join(p90 |> select(part, p90), by = "part") |>
    mutate(p90 = coalesce(p90, overall_p90)) |>
    filter(run_pos > p90, lottery == 1) |>
    transmute(game_id, team_id, person_id, flag = "long run")
  bind_rows(acq, shut, long) |>
    distinct(game_id, team_id, person_id, .keep_all = TRUE)
}

decompose <- function(rapm, pg, dir_rows, lottery, player_index, roles, poss_scale, min_elig = 250
    , seasons = DECOMP_SEASONS, groups = c("role", "listed", "none"), keep_detail = TRUE) {
  groups <- match.arg(groups)
  ps_tab <- rapm$ps_tab
  n_draws <- ncol(rapm$O_draws)
  pg <- pg |> filter(season %in% seasons)
  dir_rows <- dir_rows |> filter(season %in% seasons)
  pgd <- player_game_deployment(pg, rapm, lottery, player_index, roles, min_elig) |>
    mutate(grp = switch(groups, role = role, listed = pos_group, none = "All"))
  flags <- tank_absence_flags(pgd)
  pgd <- pgd |>
    left_join(flags |> rename(tank_flag = flag), by = c("game_id", "team_id", "person_id")) |>
    mutate(tank_high = discretionary | (!dressed & !is.na(tank_flag)))

  rank_within <- function(d) {
    d |>
      group_by(game_id, team_id, grp) |>
      mutate(k = rank(-v, ties.method = "first"), n_in_group = n()) |>
      ungroup()
  }
  normal <- pgd |> filter(dressed, lottery == 0)
  typical_share <- normal |>
    filter(eligible) |>
    rank_within() |>
    group_by(game_id, team_id, grp) |>
    mutate(grp_min = sum(min)) |>
    ungroup() |>
    filter(grp_min > 0) |>
    mutate(n_cap = pmin(n_in_group, 7), share = min / grp_min) |>
    group_by(grp, n_cap, k) |>
    summarise(share = mean(share), n_obs = n(), .groups = "drop") |>
    group_by(grp, n_cap) |>
    mutate(share = share / sum(share)) |>
    ungroup()
  # minutes each group gets: the league's typical mix (role, none) or the
  # team's own positional minutes (listed, the original rule, kept for comparison)
  game_minutes <- pgd |> group_by(season, game_id, team_id) |> summarise(game_min = sum(min), .groups = "drop")
  league_mix <- normal |>
    group_by(game_id, team_id, grp) |>
    summarise(m = sum(min), .groups = "drop") |>
    group_by(game_id, team_id) |>
    mutate(s = m / sum(m)) |>
    ungroup() |>
    complete(nesting(game_id, team_id), grp, fill = list(s = 0)) |>
    group_by(grp) |>
    summarise(mix = mean(s), .groups = "drop")
  actual_grp <- pgd |> group_by(season, game_id, team_id, grp) |> summarise(grp_actual = sum(min), .groups = "drop")
  inelig_grp <- pgd |> filter(!eligible, dressed) |> group_by(game_id, team_id, grp) |> summarise(inelig = sum(min), .groups = "drop")

  counterfactual <- function(candidates) {
    elig <- candidates |> filter(eligible)
    present <- elig |> distinct(game_id, team_id, grp)
    budget <- if (groups == "listed") {
      actual_grp |> semi_join(present, by = c("game_id", "team_id", "grp")) |> transmute(game_id, team_id, grp, budget = grp_actual)
    } else {
      present |>
        left_join(league_mix, by = "grp") |>
        group_by(game_id, team_id) |>
        mutate(mix = mix / sum(mix)) |>
        ungroup() |>
        left_join(game_minutes, by = c("game_id", "team_id")) |>
        transmute(game_id, team_id, grp, budget = mix * game_min)
    }
    budget <- budget |>
      left_join(inelig_grp, by = c("game_id", "team_id", "grp")) |>
      mutate(budget = pmax(budget - coalesce(inelig, 0), 0))
    elig |>
      rank_within() |>
      mutate(n_cap = pmin(n_in_group, 7)) |>
      left_join(typical_share |> select(grp, n_cap, k, share), by = c("grp", "n_cap", "k")) |>
      mutate(share = coalesce(share, 0)) |>
      group_by(game_id, team_id, grp) |>
      mutate(share = if (sum(share) > 0) share / sum(share) else rep(1 / n(), n())) |>
      ungroup() |>
      left_join(budget, by = c("game_id", "team_id", "grp")) |>
      mutate(min_cf = share * coalesce(budget, 0)) |>
      bind_rows(candidates |> filter(!eligible, dressed) |> mutate(min_cf = min))
  }
  cf <- list(
    alloc = counterfactual(pgd |> filter(dressed))
    , disc = counterfactual(pgd |> filter(dressed | discretionary))
    , tank_high = counterfactual(pgd |> filter(dressed | tank_high))
    , health = counterfactual(pgd |> filter(dressed | absent_any))
  )

  ts_keys <- rapm$ts_tab |> filter(season %in% seasons) |> select(season, team_id, ts) |> mutate(k = row_number())
  weight_matrix <- function(d, min_col) {
    agg <- d |>
      group_by(season, team_id, ps) |>
      summarise(m = sum(.data[[min_col]]), .groups = "drop") |>
      group_by(season, team_id) |>
      mutate(wt = 5 * m / sum(m)) |>
      ungroup() |>
      inner_join(ts_keys, by = c("season", "team_id"))
    sparseMatrix(i = agg$k, j = agg$ps, x = agg$wt, dims = c(nrow(ts_keys), nrow(ps_tab)))
  }
  W <- list(actual = weight_matrix(pgd, "min"), alloc = weight_matrix(cf$alloc, "min_cf")
    , disc = weight_matrix(cf$disc, "min_cf"), tank_high = weight_matrix(cf$tank_high, "min_cf")
    , health = weight_matrix(cf$health, "min_cf"))
  Tt <- map(W, \(w) list(O = as.matrix(w %*% rapm$O_draws), D = as.matrix(w %*% rapm$D_draws)))

  # allocation by player, posterior means, with and without the part of each
  # rating that comes from age, experience and draft slot
  dW <- summary(W$actual - W$alloc)
  alloc_by_player <- tibble(k = dW$i, ps = dW$j, dw = dW$x) |>
    left_join(ts_keys, by = "k") |>
    left_join(ps_tab |> select(ps, person_id, name, O_mean, D_mean, O_age, D_age), by = "ps") |>
    mutate(contrib = dw * (O_mean + D_mean), contrib_noage = dw * (O_mean + D_mean - O_age - D_age)) |>
    select(season, team_id, person_id, name, dw, contrib, contrib_noage)

  rows <- dir_rows |>
    left_join(ts_keys |> select(season, off_team = team_id, ts_off = ts), by = c("season", "off_team")) |>
    left_join(ts_keys |> select(season, def_team = team_id, ts_def = ts), by = c("season", "def_team"))
  ps_key <- paste(ps_tab$person_id, ps_tab$season)
  row_sum_draws <- function(r, cols, draws) {
    idx <- sapply(cols, \(cl) match(paste(r[[cl]], r$season), ps_key))
    idx[is.na(idx)] <- 0
    out <- matrix(0, nrow(r), ncol(draws))
    for (j in seq_len(ncol(idx))) {
      ok <- idx[, j] > 0
      out[ok, ] <- out[ok, ] + draws[idx[ok, j], , drop = FALSE]
    }
    out
  }
  rows_by_off <- split(rows, paste(rows$season, rows$off_team))
  rows_by_def <- split(rows, paste(rows$season, rows$def_team))
  lotO <- rapm$lot_draws["O", ]; lotD <- rapm$lot_draws["D", ]
  envO <- rapm$env_draws["O", ]; envD <- rapm$env_draws["D", ]

  comp <- map_dfr(seq_len(nrow(ts_keys)), \(i) {
    key <- ts_keys[i, ]
    j <- key$ts
    ro <- rows_by_off[[paste(key$season, key$team_id)]]
    rd <- rows_by_def[[paste(key$season, key$team_id)]]
    wo <- ro$poss / sum(ro$poss); wd <- rd$poss / sum(rd$poss)
    opp_D <- as.vector(crossprod(wo, row_sum_draws(ro, paste0("d", 1:5), rapm$D_draws)))
    opp_O <- as.vector(crossprod(wd, row_sum_draws(rd, paste0("o", 1:5), rapm$O_draws)))
    opp_cD <- as.vector(crossprod(wo, rapm$cD_draws[ro$ts_def, , drop = FALSE]))
    opp_cO <- as.vector(crossprod(wd, rapm$cO_draws[rd$ts_off, , drop = FALSE]))
    tibble(
      ts = j, season = key$season, team_id = key$team_id, draw = seq_len(n_draws)
      , actual_O = 100 * sum(ro$pts) / sum(ro$poss), actual_D = -100 * sum(rd$pts) / sum(rd$poss)
      , talent_O = Tt$health$O[i, ], talent_D = Tt$health$D[i, ]
      , avail_O = Tt$alloc$O[i, ] - Tt$health$O[i, ], avail_D = Tt$alloc$D[i, ] - Tt$health$D[i, ]
      , tanklow_O = Tt$alloc$O[i, ] - Tt$disc$O[i, ], tanklow_D = Tt$alloc$D[i, ] - Tt$disc$D[i, ]
      , tankhigh_O = Tt$alloc$O[i, ] - Tt$tank_high$O[i, ], tankhigh_D = Tt$alloc$D[i, ] - Tt$tank_high$D[i, ]
      , alloc_O = Tt$actual$O[i, ] - Tt$alloc$O[i, ], alloc_D = Tt$actual$D[i, ] - Tt$alloc$D[i, ]
      , lottery_O = lotO * sum(wo * ro$L_off), lottery_D = lotD * sum(wd * rd$L_def)
      , env_O = envO * sum(wo * ro$prev_off), env_D = envD * sum(wd * rd$prev_def)
      , context_O = rapm$cO_draws[j, ], context_D = rapm$cD_draws[j, ]
      # this offense scored against opponents' defense (talent, context,
      # lottery status, last-season tanking), and vice versa
      , opponents_O = -(opp_D + opp_cD + lotD * sum(wo * ro$L_def) + envD * sum(wo * ro$prev_def))
      , opponents_D = -(opp_O + opp_cO + lotO * sum(wd * rd$L_off) + envO * sum(wd * rd$prev_off))
      , garbage_share = sum(c(ro$poss[ro$garbage], rd$poss[rd$garbage])) / sum(c(ro$poss, rd$poss))
    )
  })

  talent_center <- comp |> group_by(season) |> summarise(O = mean(talent_O), D = mean(talent_D), .groups = "drop")
  poss_ratio <- setNames(poss_scale$poss_ratio, poss_scale$season)
  # availability and its tank-flagged part before centring, so "what absences
  # cost" can be read directly and compared with the league's typical cost
  availability_raw <- comp |>
    group_by(season, team_id) |>
    summarise(avail = mean(avail_O + avail_D), tank_low = mean(tanklow_O + tanklow_D), tank_high = mean(tankhigh_O + tankhigh_D)
      , avail_q10 = quantile(avail_O + avail_D, 0.1), avail_q90 = quantile(avail_O + avail_D, 0.9), .groups = "drop") |>
    mutate(across(c(avail, tank_low, tank_high, avail_q10, avail_q90), \(x) x * poss_ratio[season]))
  pieces <- c("actual", "talent", "avail", "tanklow", "tankhigh", "alloc", "lottery", "env", "context", "opponents")
  comp <- comp |>
    group_by(season, draw) |>
    mutate(across(matches(paste0("^(", paste(pieces, collapse = "|"), ")_[OD]$")), \(x) x - mean(x))) |>
    ungroup() |>
    mutate(
      other_O = actual_O - (talent_O + avail_O + alloc_O + lottery_O + env_O + context_O + opponents_O)
      , other_D = actual_D - (talent_D + avail_D + alloc_D + lottery_D + env_D + context_D + opponents_D)
    ) |>
    mutate(across(matches("_(O|D)$"), \(x) x * poss_ratio[season]))

  out <- list(comp = comp, availability_raw = availability_raw, typical_share = typical_share, league_mix = league_mix, min_elig = min_elig, groups = groups
    , talent_center = talent_center, alloc_by_player = alloc_by_player |> mutate(across(c(contrib, contrib_noage), \(x) x * poss_ratio[season])))
  if (keep_detail) {
    out$pgd <- pgd |> select(season, game_id, game_date, game_no, n_games, team_id, team_tricode, person_id, name, ps, v, role, pos_group
      , status, not_playing_reason, not_playing_description, dressed, structural, discretionary, absent_any, tank_flag, tank_high
      , eligible, starter, played, min, lottery)
    out$cf <- map(cf, \(d) d |> select(season, game_id, team_id, person_id, ps, min, min_cf))
    out$flags <- flags
  }
  out
}

# net-rating summary of one team-season's decomposition, per piece
summarise_decomp <- function(comp, team = WIZARDS_ID, season_ = "2025-26") {
  comp |>
    filter(team_id == team, season == season_) |>
    transmute(
      draw
      , actual = actual_O + actual_D, talent = talent_O + talent_D, avail = avail_O + avail_D
      , tank_low = tanklow_O + tanklow_D, tank_high = tankhigh_O + tankhigh_D
      , alloc = alloc_O + alloc_D, lottery = lottery_O + lottery_D, env = env_O + env_D
      , context = context_O + context_D, opponents = opponents_O + opponents_D, other = other_O + other_D
    ) |>
    pivot_longer(-draw, names_to = "piece") |>
    group_by(piece) |>
    summarise(mean = mean(value), q10 = quantile(value, 0.1), q90 = quantile(value, 0.9), .groups = "drop")
}

sensitivity_summary <- function(dc, label) {
  summarise_decomp(dc$comp) |>
    mutate(spec = label) |>
    bind_rows(
      dc$alloc_by_player |>
        filter(team_id == WIZARDS_ID, season == "2025-26") |>
        summarise(mean = sum(contrib_noage)) |>
        mutate(piece = "alloc_noage_raw", spec = label, q10 = NA, q90 = NA)
    )
}

# Is the allocation bill a tank fingerprint, or what any team this young pays?
# Across team-seasons, regress the allocation piece on the share of minutes
# that went to players 22 and under and on lottery-race exposure; the residual
# is the part the youth and tanking of the roster do not explain.
benchmark_allocation <- function(decomp, rapm) {
  ages <- rapm$box_ps |> select(person_id, season, age)
  youth <- decomp$pgd |>
    left_join(ages, by = c("person_id", "season")) |>
    group_by(season, team_id, team_tricode) |>
    summarise(youth_share = sum(min[coalesce(age, 30) <= 22]) / sum(min), lottery_share = mean(lottery), .groups = "drop")
  tm <- decomp$comp |>
    group_by(season, team_id) |>
    summarise(alloc = mean(alloc_O + alloc_D), .groups = "drop") |>
    left_join(youth |> group_by(season, team_id) |> slice(1) |> ungroup(), by = c("season", "team_id"))
  fit <- lm(alloc ~ youth_share + lottery_share, data = tm)
  tm <- tm |> mutate(expected = fitted(fit), residual = resid(fit))
  young_vet <- decomp$alloc_by_player |>
    left_join(ages, by = c("person_id", "season")) |>
    mutate(group = if_else(coalesce(age, 30) <= 23, "23 and under", "24 and over")) |>
    group_by(season, team_id, group) |>
    summarise(contrib = sum(contrib), contrib_noage = sum(contrib_noage), .groups = "drop")
  list(teams = tm, coef = coef(summary(fit)), r2 = summary(fit)$r.squared, resid_sd = sigma(fit), young_vet = young_vet)
}
# ---- Stan helpers --------------------------------------------------------------

# CmdStan warmup can propose values that trip a check (an infinite scale, a
# degenerate correlation) and are rejected; that is harmless during warmup and
# a problem if it continues while sampling. Progress lines are printed every
# `refresh` iterations, so rejections after the first "(Sampling)" line are
# counted separately and gated.
sample_stan <- function(model, data, chains = 4, warmup = 1000, iter = 1000, adapt_delta = 0.99) {
  model$sample(data = data, chains = chains, parallel_chains = chains, iter_warmup = warmup
    , iter_sampling = iter, adapt_delta = adapt_delta, seed = 202, refresh = 250, init = 0.5
    , show_messages = FALSE, show_exceptions = FALSE)
}

rejections_by_phase <- function(fit) {
  map_dfr(seq_len(fit$num_chains()), \(ch) {
    # $output() prints the console text and returns NULL, so read the lines
    # from the process record instead
    out <- fit$runset$procs$proc_output(ch)
    if (length(out) == 0) stop("no console output captured for chain ", ch)
    first_sampling <- which(str_detect(out, "\\(Sampling\\)"))[1]
    rej <- which(str_detect(out, "Metropolis proposal is about to be rejected"))
    tibble(chain = ch, warmup = sum(rej < first_sampling), sampling = sum(rej > first_sampling)
      , messages = paste(unique(str_trim(str_remove(out[rej + 1], "^Chain \\d+ "))), collapse = " | "))
  })
}

fit_diagnostics <- function(fit, vars, label) {
  s <- fit$summary(variables = vars, "mean", "sd", ~quantile(.x, c(0.05, 0.95)), "rhat", "ess_bulk", "ess_tail", "mcse_mean")
  sd_ <- fit$sampler_diagnostics(format = "df")
  ebfmi <- sd_ |>
    group_by(.chain) |>
    summarise(ebfmi = sum(diff(energy__)^2) / sum((energy__ - mean(energy__))^2))
  rej <- rejections_by_phase(fit)
  diag <- tibble(
    model = label
    , max_rhat = max(s$rhat, na.rm = TRUE)
    , min_ess_bulk = min(s$ess_bulk, na.rm = TRUE)
    , min_ess_tail = min(s$ess_tail, na.rm = TRUE)
    , max_mcse_over_sd = max(s$mcse_mean / s$sd, na.rm = TRUE)
    , divergences = sum(sd_$divergent__)
    , min_ebfmi = min(ebfmi$ebfmi)
    , rejections_warmup = sum(rej$warmup)
    , rejections_sampling = sum(rej$sampling)
    , rejection_messages = paste(unique(rej$messages[rej$messages != ""]), collapse = " | ")
  )
  # gate on the parameters whose values are consumed downstream
  if (diag$max_rhat > 1.01 || diag$min_ess_bulk < 400 || diag$min_ess_tail < 400 ||
      diag$divergences > 0 || diag$min_ebfmi < 0.3 || diag$rejections_sampling > 0) {
    print(s |> arrange(desc(rhat)) |> head(10))
    stop(label, " failed diagnostics: ", paste(names(diag), unlist(diag), collapse = "; "))
  }
  list(summary = s, diag = diag)
}

# posterior's draws_matrix keeps its class (and its matrix shape) when subset,
# which silently turns a column into a 1-column matrix downstream
plain <- function(x) matrix(as.vector(x), nrow(x), ncol(x), dimnames = list(NULL, colnames(x)))

prior_check <- function(prior_draws, observed, label) {
  pr <- quantile(prior_draws, c(0.005, 0.995), na.rm = TRUE)
  ob <- quantile(observed, c(0.005, 0.995), na.rm = TRUE)
  ratio <- unname(diff(pr) / diff(ob))
  tibble(model = label, prior_lo = pr[[1]], prior_hi = pr[[2]], data_lo = ob[[1]], data_hi = ob[[2]]
    , width_ratio = ratio, verdict = case_when(ratio < 1 ~ "too narrow", ratio > 5 ~ "too wide", TRUE ~ "ok"))
}

age_bins <- function(age) pmin(pmax(floor(age), 20), 35) - 19   # 1 = age <= 20 ... 16 = 35+

# ---- Stage 5a: development curve ------------------------------------------------
# Pairs of consecutive seasons for the same player, 200+ minutes in each.
#
# The de-tank check needs care. The obvious version — how much a player's
# lottery exposure fell from one season to the next — is confounded: when a
# young core improves, the team wins more and leaves the lottery race, so
# improvement causes the "de-tank". The check used is ex ante: only players who
# changed teams, scored by the lottery exposure of the team they were joining,
# measured in the season BEFORE they joined it.
build_dev_pairs <- function(rapm, pgd) {
  primary <- pgd |>
    filter(min > 0) |>
    group_by(person_id, season, team_id) |>
    summarise(m = sum(min), lot = sum(min * lottery) / sum(min), .groups = "drop") |>
    group_by(person_id, season) |>
    mutate(lot_player = sum(m * lot) / sum(m)) |>
    slice_max(m, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(person_id, season, primary_team = team_id, lot_player)
  team_lot <- pgd |> distinct(season, team_id, game_id, lottery) |>
    group_by(season, team_id) |> summarise(team_lot = mean(lottery), .groups = "drop")
  rapm$ps_tab |>
    left_join(rapm$box_ps |> select(person_id, season, age), by = c("person_id", "season")) |>
    mutate(season_start = as.integer(str_sub(season, 1, 4))) |>
    select(person_id, name, season, season_start, ps, min, age) |>
    inner_join(rapm$ps_tab |> transmute(person_id, season_next = season, ps_next = ps, min_next = min
        , season_start = as.integer(str_sub(season, 1, 4)) - 1L), by = c("person_id", "season_start")) |>
    filter(min >= 200, min_next >= 200) |>
    left_join(primary, by = c("person_id", "season")) |>
    left_join(primary |> rename(season_next = season, next_team = primary_team, lot_player_next = lot_player)
      , by = c("person_id", "season_next")) |>
    left_join(team_lot |> rename(next_team = team_id, next_team_lot_before = team_lot), by = c("season", "next_team")) |>
    mutate(
      age_bin = age_bins(age)
      , mover = coalesce(next_team != primary_team, FALSE)
      , detank_naive = coalesce(lot_player, 0) - coalesce(lot_player_next, 0)
      , detank_exante = if_else(mover, coalesce(lot_player, 0) - coalesce(next_team_lot_before, 0), 0)
    )
}

fit_development <- function(rapm, dev_pairs, stan_file, run_checks = TRUE) {
  mod <- cmdstan_model(stan_file)
  map(c(O = "O", D = "D"), \(side) {
    dr <- if (side == "O") rapm$O_draws else rapm$D_draws
    delta <- dr[dev_pairs$ps_next, ] - dr[dev_pairs$ps, ]
    sdat <- list(N = nrow(dev_pairs), A = 16L, age = dev_pairs$age_bin
      , d_mean = rowMeans(delta), d_sd = apply(delta, 1, sd)
      , detank = dev_pairs$detank_exante, use_detank = 0L, prior_only = 0L)
    fit0 <- sample_stan(mod, sdat)
    dg <- fit_diagnostics(fit0, c("f", "sigma_dev", "nu"), paste("development", side))
    res <- list(f = plain(fit0$draws("f", format = "matrix"))
      , sigma_dev = as.vector(fit0$draws("sigma_dev", format = "matrix")), summary = dg$summary, diag = dg$diag)
    if (!run_checks) return(res)
    prior_fit <- sample_stan(mod, modifyList(sdat, list(prior_only = 1L)), chains = 2, warmup = 500)
    fit_ex <- sample_stan(mod, modifyList(sdat, list(use_detank = 1L)))
    fit_nv <- sample_stan(mod, modifyList(sdat, list(use_detank = 1L, detank = dev_pairs$detank_naive)))
    loo0 <- fit0$loo()
    c(res, list(
      prior_check = prior_check(as.vector(prior_fit$draws("d_rep", format = "matrix")), sdat$d_mean, paste("development", side))
      , prior_rejections = rejections_by_phase(prior_fit)
      , beta_exante = as.vector(fit_ex$draws("beta_detank", format = "matrix"))
      , beta_naive = as.vector(fit_nv$draws("beta_detank", format = "matrix"))
      , check_diag = bind_rows(
          fit_diagnostics(fit_ex, c("f", "sigma_dev", "nu", "beta_detank"), paste("development", side, "ex-ante check"))$diag
          , fit_diagnostics(fit_nv, c("f", "sigma_dev", "nu", "beta_detank"), paste("development", side, "naive check"))$diag)
      , ppc = list(obs = sdat$d_mean, rep = plain(fit0$draws("d_rep", format = "matrix"))[sample(4000, 100), ])
      , loo_compare = loo::loo_compare(list(no_detank = loo0, exante = fit_ex$loo()))
      , pareto_k_bad = sum(loo0$diagnostics$pareto_k > 0.7)
    ))
  })
}

# ---- Stage 5b: rookie reference class ---------------------------------------------
# First-round picks' first season on record, from the first draft inside the
# window onward (requiring the debut to be the draft year dropped players who
# sat a season, like Chet Holmgren). Predictors: log draft slot and whether the
# rookie played as a wing, from what he did rather than his listing.

fit_rookie <- function(rapm, pg, player_index, roles, stan_file) {
  mod <- cmdstan_model(stan_file)
  rk_mpg <- pg |> filter(played == 1) |> group_by(person_id, season) |> summarise(mpg = mean(min), gp = n(), .groups = "drop")
  rookies <- rapm$ps_tab |>
    mutate(season_start = as.integer(str_sub(season, 1, 4))) |>
    inner_join(player_index |> select(person_id, draft_year, draft_number), by = "person_id") |>
    filter(draft_year >= min(season_start), draft_number <= 30) |>
    group_by(person_id) |>
    slice_min(season_start, n = 1, with_ties = FALSE) |>
    ungroup() |>
    filter(min >= 250) |>
    left_join(roles |> select(person_id, season, role), by = c("person_id", "season")) |>
    left_join(rk_mpg, by = c("person_id", "season"))
  lp <- log(rookies$draft_number)
  lp_mean <- mean(lp); lp_sd <- sd(lp)
  rk_dat <- list(N = nrow(rookies), log_pick_z = (lp - lp_mean) / lp_sd
    , forward = as.numeric(rookies$role == "Wing")
    , y = map2(rookies$O_mean, rookies$D_mean, c), y_sd = map2(rookies$O_sd, rookies$D_sd, c)
    , pick1_z = (log(1) - lp_mean) / lp_sd, prior_only = 0L)
  prior_fit <- sample_stan(mod, modifyList(rk_dat, list(prior_only = 1L)), chains = 2, warmup = 500)
  fit <- sample_stan(mod, rk_dat)
  dg <- fit_diagnostics(fit, c("alpha", "beta_pick", "beta_fwd", "tau", "rho"), "rookie")
  prior_theta <- plain(prior_fit$draws("theta", format = "matrix"))
  list(
    n = nrow(rookies), lp_mean = lp_mean, lp_sd = lp_sd
    , pars = fit$draws(c("alpha", "beta_pick", "beta_fwd", "tau", "rho"), format = "df") |> as_tibble()
    , table = rookies |> select(person_id, name, season, draft_number, role, min, mpg, O_mean, O_sd, D_mean, D_sd)
    , pick1 = fit$draws(c("pick1_fwd", "pick1_net"), format = "df") |> as_tibble()
    , summary = dg$summary, diag = dg$diag, prior_rejections = rejections_by_phase(prior_fit)
    , prior_check = bind_rows(
        prior_check(prior_theta[, str_detect(colnames(prior_theta), ",1\\]$")], rookies$O_mean, "rookie O")
        , prior_check(prior_theta[, str_detect(colnames(prior_theta), ",2\\]$")], rookies$D_mean, "rookie D"))
  )
}

# a draw of (O, D) for a rookie at any slot, wing or not, from the posterior
rookie_draws <- function(rk, pick, wing, n) {
  pars <- rk$pars[sample(nrow(rk$pars), n, replace = nrow(rk$pars) < n), ]
  z <- (log(pick) - rk$lp_mean) / rk$lp_sd
  e1 <- rnorm(n); e2 <- rnorm(n)
  list(
    O = pars$`alpha[1]` + pars$`beta_pick[1]` * z + pars$`beta_fwd[1]` * wing + pars$`tau[1]` * e1
    , D = pars$`alpha[2]` + pars$`beta_pick[2]` * z + pars$`beta_fwd[2]` * wing +
        pars$`tau[2]` * (pars$rho * e1 + sqrt(1 - pars$rho^2) * e2)
  )
}

mean_growth <- function(development, age, years = 1) {
  fO <- colMeans(development$O$f); fD <- colMeans(development$D$f)
  gO <- Reduce(`+`, map(seq_len(years) - 1, \(k) fO[age_bins(age + k)]))
  gD <- Reduce(`+`, map(seq_len(years) - 1, \(k) fD[age_bins(age + k)]))
  list(O = gO, D = gD)
}

# A player's recent seasons before the target, Marcel-style: up to his last
# three, weighted 5/4/3 times minutes played, each aged to the target season.
# A first version projected from the latest season alone, which let one short
# season carry the projection (Trae Young's 384 minutes in 2025-26 rated +1.9;
# his three seasons before that averaged about +0.2).
history_rows <- function(rapm, person_ids, target_season_start, n = 3, season_weights = c(5, 4, 3)) {
  rapm$ps_tab |>
    filter(person_id %in% person_ids) |>
    left_join(rapm$box_ps |> select(person_id, season, age), by = c("person_id", "season")) |>
    mutate(season_start = as.integer(str_sub(season, 1, 4)), age = coalesce(age, 27)) |>
    filter(season_start < target_season_start) |>
    group_by(person_id) |>
    arrange(desc(season_start), .by_group = TRUE) |>
    slice_head(n = n) |>
    mutate(k = row_number(), w = season_weights[k] * min, w = w / sum(w), years = target_season_start - season_start) |>
    ungroup()
}

# posterior-mean version, for rosters projected without draws
history_means <- function(rapm, development, person_ids, target_season_start) {
  fO <- colMeans(development$O$f); fD <- colMeans(development$D$f)
  grow <- function(f, age, years) map2_dbl(age, years, \(a, y) sum(f[age_bins(a + seq_len(y) - 1)]))
  history_rows(rapm, person_ids, target_season_start) |>
    mutate(gO = grow(fO, age, years), gD = grow(fD, age, years)) |>
    group_by(person_id) |>
    summarise(O = sum(w * (O_mean + gO)), D = sum(w * (D_mean + gD)), growth = sum(w * (gO + gD))
      , age = first(age) + first(years), seasons = n(), .groups = "drop")
}

# talent draws for a list of players in the target season
project_players <- function(person_ids, target_season_start, rapm, development, rookie, player_index, n_draws) {
  hist <- history_rows(rapm, person_ids, target_season_start)
  f_rows <- sample(nrow(development$O$f), n_draws, replace = TRUE)
  map(person_ids, \(pid) {
    lt <- hist |> filter(person_id == pid)
    info <- player_index |> filter(person_id == pid)
    if (nrow(lt) >= 1) {
      out <- map(c(O = "O", D = "D"), \(side) {
        dr <- if (side == "O") rapm$O_draws else rapm$D_draws
        aged <- map(seq_len(nrow(lt)), \(j) {
          grow <- Reduce(`+`, map(seq_len(lt$years[j]) - 1, \(k) development[[side]]$f[f_rows, age_bins(lt$age[j] + k)]))
          lt$w[j] * (dr[lt$ps[j], ] + grow)
        })
        Reduce(`+`, aged) + rnorm(n_draws, 0, development[[side]]$sigma_dev[f_rows]) * sqrt(min(lt$years))
      })
      out$source <- paste0("weighted ", paste(rev(lt$season), collapse = ", "), " ratings + development")
    } else if (nrow(info) == 1 && coalesce(info$draft_year == target_season_start, FALSE) && !is.na(info$draft_number)) {
      out <- rookie_draws(rookie, info$draft_number, as.numeric(listed_role(info$position) == "Wing"), n_draws)
      out$source <- if (info$draft_number > 30) "rookie model (extrapolated past round one)" else "rookie model"
    } else {
      out <- NULL
    }
    out
  }) |>
    setNames(person_ids)
}

# ---- Stage 5c: the 2026-27 Wizards --------------------------------------------------
# Talent only, on a league-average schedule, outside the lottery race.
#
# Three kinds of minutes:
#   rule          a depth chart sorted purely by projected rating (two guard,
#                 two wing, one big starter slots). It is a reference point,
#                 not a rotation anyone would run: it starts three bigs when
#                 bigs rate highest and buries the No. 1 pick.
#   rotations     the two rotations, built by rule in project_2026 (win-focused: sorted by rating; development-focused: the same with veterans sitting in no-chance games), as minutes
#                 per game when a player is available.
#   sweep         the win-focused rotation with Dybantsa's minutes varied and
#                 the other wings' planned minutes adjusted to match.
# In every simulated game, available players get their planned minutes and the
# minutes of anyone who is out are filled the way a coach would: first by
# available players at the same position who are in the plan, up to 4 minutes
# over plan and never past 36; then by available players at that position who
# aren't in the plan, best-rated first, up to 24 each; then by the same two
# steps at other positions. A first version scaled every available player up
# in proportion instead, which with typical absences put the regulars about a
# quarter over plan (Dybantsa at 37 a night on a 30-minute plan, above any
# recent No. 1 pick) and never used the end of the bench. The same simulated
# availability is used for every scenario, so differences between scenarios
# are not availability noise.
#
# Availability comes from games a player's team was NOT in the lottery race,
# shrunk toward the league rate; known 2026-27 absences in
# tank_roster_notes_2026.csv override it.
#
# The league it is ranked against: every 2025-26 roster, aged a year on the
# same development curve and available at the same non-lottery rates. Each team
# is scored twice: under the minutes its own coach actually gave out (O_act,
# D_act, net_act), and under the rating-sorted rule (O, D, net). Zero is the
# average team under its real minutes, and the two rotations are ranked against
# that column: ranking them against teams sorted purely by rating scores every
# rotation about 4 points low by construction. Only the
# rating-sorted row is ranked against the rating-sorted column.
#
# Who starts is not decided here. ESPN's projected depth chart is a hand-kept
# input (tank_espn_depth_2026.csv, read_depth_chart) and best_lineups() takes
# its starting five as an argument.

role_slots <- function(role) {
  switch(role, Guard = c("G", "W"), Wing = c("W", "G"), Big = c("B", "W"), c("W", "G"))
}

typical_rank_shares <- function(pgd) {
  pgd |>
    filter(lottery == 0, dressed, min > 0) |>
    group_by(game_id, team_id) |>
    arrange(desc(min), .by_group = TRUE) |>
    mutate(r = row_number(), share = min / sum(min)) |>
    ungroup() |>
    group_by(r) |>
    summarise(share = mean(share), .groups = "drop") |>
    filter(r <= 13) |>
    mutate(share = share / sum(share))
}

depth_minutes <- function(avail, v, slots, shares) {
  mins <- numeric(length(v))
  pool <- which(avail)
  if (length(pool) == 0) return(mins)
  ord <- pool[order(-v[pool])]
  open <- c(G = 2, W = 2, B = 1)
  starters <- integer(0); bench <- integer(0)
  for (i in ord) {
    fit <- slots[[i]][open[slots[[i]]] > 0]
    if (length(fit) > 0) {
      open[fit[1]] <- open[fit[1]] - 1
      starters <- c(starters, i)
    } else {
      bench <- c(bench, i)
    }
  }
  depth <- c(starters, bench)
  sh <- shares$share[match(seq_along(depth), shares$r)]
  sh[is.na(sh)] <- 0
  if (sum(sh) > 0) mins[depth] <- 240 * sh / sum(sh)
  mins
}

rule_minutes <- function(A, v, slots, shares) {
  P <- dim(A)[1]; G <- dim(A)[2]; S <- dim(A)[3]
  out <- matrix(0, P, S)
  for (s in seq_len(S)) for (g in seq_len(G)) out[, s] <- out[, s] + depth_minutes(A[, g, s], v, slots, shares)
  out
}

# one game: planned minutes for whoever is available, and the absent players'
# minutes filled position by position (see the header above)
fill_rotation <- function(avail, planned, role, v, extra = 4, cap = 36, bench_cap = 24) {
  m <- planned * avail
  in_plan <- avail & planned > 0
  room <- ifelse(in_plan, pmax(pmin(planned + extra, cap) - planned, 0), ifelse(avail, bench_cap, 0))
  # proportional to room among `idx`, never past each player's room
  take_prop <- function(need, idx) {
    idx <- idx[room[idx] > 0]
    if (need <= 0 || !length(idx)) return(need)
    give <- pmin(room[idx], need * room[idx] / sum(room[idx]))
    m[idx] <<- m[idx] + give; room[idx] <<- room[idx] - give
    need - sum(give)
  }
  # best-rated first, each up to his room
  take_seq <- function(need, idx) {
    for (i in idx[order(-v[idx])]) {
      if (need <= 0) break
      give <- min(room[i], need)
      m[i] <<- m[i] + give; room[i] <<- room[i] - give
      need <- need - give
    }
    need
  }
  missing <- tapply(planned * !avail, factor(role, levels = unique(role)), sum)
  left <- 0
  for (r in names(missing)[missing > 0]) {
    need <- missing[[r]]
    need <- take_prop(need, which(role == r & in_plan))
    need <- take_seq(need, which(role == r & avail & !in_plan))
    left <- left + need
  }
  left <- take_prop(left, which(in_plan))
  left <- take_seq(left, which(avail & !in_plan))
  # a night with too few players left to fill 240 inside the caps: spread the
  # rest over everyone available, never past 48
  while (240 - sum(m) > 1e-6) {
    open <- which(avail & m < 48)
    if (!length(open)) break
    add <- pmin(48 - m[open], (240 - sum(m)) / length(open))
    m[open] <- m[open] + add
  }
  m
}

rotation_minutes <- function(A, planned, role, v, extra = 4) {
  P <- dim(A)[1]; G <- dim(A)[2]; S <- dim(A)[3]
  memo <- new.env(hash = TRUE)
  out <- matrix(0, P, S)
  for (s in seq_len(S)) for (g in seq_len(G)) {
    a <- A[, g, s]
    key <- paste(as.integer(a), collapse = "")
    m <- memo[[key]]
    if (is.null(m)) {
      m <- fill_rotation(a, planned, role, v, extra = extra)
      memo[[key]] <- m
    }
    out[, s] <- out[, s] + m
  }
  out
}

team_talent <- function(PO, PD, mins, center) {
  w <- sweep(mins, 2, colSums(mins), "/") * 5
  tibble(O = colSums(w * PO) - center$O, D = colSums(w * PD) - center$D) |> mutate(net = O + D)
}

non_lottery_availability <- function(pgd) {
  obs <- pgd |>
    filter(lottery == 0, !structural) |>
    group_by(person_id) |>
    summarise(listed = n(), dressed_n = sum(dressed), .groups = "drop")
  lg <- sum(obs$dressed_n) / sum(obs$listed)
  list(table = obs |> transmute(person_id, avail = (dressed_n + 40 * lg) / (listed + 40)), lg = lg)
}

reference_league <- function(rapm, pgd, development, player_index, roles, shares, n_sims = 100) {
  last <- max(rapm$seasons)
  av <- non_lottery_availability(pgd)
  team_games <- pgd |> filter(season == last) |> distinct(team_id, game_id) |> count(team_id, name = "team_games")
  tp <- pgd |>
    filter(season == last) |>
    group_by(team_id, team_tricode, person_id, ps) |>
    summarise(listed = n(), m = sum(min), .groups = "drop") |>
    filter(m >= 100) |>
    left_join(team_games, by = "team_id") |>
    left_join(av$table, by = "person_id") |>
    mutate(
      avail = coalesce(avail, av$lg) * listed / team_games
      , role = role_of(person_id, last, roles, player_index)
    )
  # the same multi-season, aged projection Washington's players get
  hm <- history_means(rapm, development, unique(tp$person_id), as.integer(str_sub(last, 1, 4)) + 1)
  tp <- tp |> left_join(hm |> select(person_id, O, D, growth), by = "person_id") |> mutate(v = O + D)
  map_dfr(split(tp, tp$team_id), \(d) {
    A <- array(runif(nrow(d) * 82 * n_sims) < d$avail, dim = c(nrow(d), 82, n_sims))
    mins <- rowMeans(rule_minutes(A, d$v, map(d$role, role_slots), shares))
    w <- 5 * mins / sum(mins)
    # the same roster under the minutes its coach actually gave out last season.
    # Hand-written rotation plans belong against this, not against a depth chart
    # sorted by rating, which no coach runs and which scores about 4 points higher.
    w_act <- 5 * d$m / sum(d$m)
    tibble(team_id = d$team_id[1], team_tricode = d$team_tricode[1], O = sum(w * d$O), D = sum(w * d$D)
      , growth = sum(w * d$growth), O_act = sum(w_act * d$O), D_act = sum(w_act * d$D))
  })
}

# A typical rotation backup at each position: 1,000+ minute players last season
# who were not their team's top two at the position (top one at big) by minutes,
# projected the same way as everyone else.
backup_benchmarks <- function(rapm, pgd, development, roles, last) {
  tm <- pgd |>
    filter(season == last, min > 0) |>
    group_by(person_id, team_id) |>
    summarise(m = sum(min), .groups = "drop") |>
    group_by(person_id) |>
    mutate(total = sum(m)) |>
    slice_max(m, n = 1, with_ties = FALSE) |>
    ungroup() |>
    left_join(roles |> filter(season == last) |> select(person_id, role), by = "person_id") |>
    filter(!is.na(role), total >= 1000) |>
    group_by(team_id, role) |>
    mutate(rk = rank(-m, ties.method = "first")) |>
    ungroup() |>
    filter(rk > if_else(role == "Big", 1, 2))
  hm <- history_means(rapm, development, unique(tm$person_id), as.integer(str_sub(last, 1, 4)) + 1)
  tm |>
    left_join(hm |> select(person_id, O, D), by = "person_id") |>
    group_by(role) |>
    summarise(O = weighted.mean(O, total), D = weighted.mean(D, total), n = n(), .groups = "drop")
}

# The benchmark a 2026-27 projection should be measured against: the league's
# typical rotation player at each role, carried forward one year the same way the
# roster is. Comparing aged projections with an un-aged benchmark flattered
# Washington by about a third of a point at every position.
role_benchmarks <- function(rapm, roles, box, development, min_min = 1000, aged = TRUE) {
  last <- max(rapm$seasons)
  ages <- box$box_ps |> filter(season == last) |> transmute(person_id, age = coalesce(age, 27))
  d <- rapm$ps_tab |>
    filter(season == last, min >= min_min) |>
    left_join(roles |> filter(season == last) |> select(person_id, role), by = "person_id") |>
    left_join(ages, by = "person_id")
  g <- mean_growth(development, d$age)
  d |>
    mutate(O = O_mean + aged * g$O, D = D_mean + aged * g$D) |>
    group_by(role) |>
    summarise(O = weighted.mean(O, min), D = weighted.mean(D, min), v = O + D, n = n(), .groups = "drop")
}

project_2026 <- function(rapm, decomp, development, rookie, player_index, roles, box, roster_path, notes_path, margin_sd, schedule_path
    , official, n_sims = 400, aj_minutes = 30, vet_age = 28, no_chance_p = 0.25) {
  n_draws <- ncol(rapm$O_draws)
  pgd <- decomp$pgd
  last <- max(rapm$seasons)
  shares <- typical_rank_shares(pgd)
  reference <- reference_league(rapm, pgd, development, player_index, roles, shares)
  # zero = the average team under its own real allocation. Centering on the
  # rating-sorted rule instead put every level about 4.4 points too low and made
  # the two rotations look worse than the league by construction.
  center <- reference |> summarise(O = mean(O_act), D = mean(D_act))
  reference <- reference |>
    mutate(across(c(O, O_act), \(x) x - center$O), across(c(D, D_act), \(x) x - center$D)
      , net = O + D, net_act = O_act + D_act)

  roster <- read_player_index(roster_path) |> filter(team_id == WIZARDS_ID) |> select(-team_id)
  # the player index can lag the team: keep only the official roster, and stop if
  # the index is missing anyone on it
  stopifnot(all(official$name %in% roster$name))
  roster_dropped <- setdiff(roster$name, official$name)
  roster <- roster |> filter(name %in% official$name)
  proj <- project_players(roster$person_id, 2026, rapm, development, rookie, roster, n_draws) |> compact()
  roster <- roster |> filter(person_id %in% as.numeric(names(proj)))
  proj <- proj[as.character(roster$person_id)]
  PO <- do.call(rbind, map(proj, "O")); PD <- do.call(rbind, map(proj, "D"))
  latest_role <- roles |> group_by(person_id) |> slice_max(season, n = 1, with_ties = FALSE) |> ungroup() |> select(person_id, role)
  av <- non_lottery_availability(pgd)
  notes <- read_csv(notes_path, show_col_types = FALSE, col_types = cols(person_id = "d", availability = "d", .default = "c"))
  roster <- roster |>
    mutate(source = map_chr(proj, "source"), O_mean = rowMeans(PO), D_mean = rowMeans(PD), v = O_mean + D_mean
      , O_sd = apply(PO, 1, sd), D_sd = apply(PD, 1, sd)) |>
    left_join(latest_role, by = "person_id") |>
    mutate(role = coalesce(role, listed_role(position))) |>
    left_join(av$table, by = "person_id") |>
    mutate(avail = coalesce(avail, av$lg)) |>
    left_join(notes |> select(person_id, availability, note), by = "person_id") |>
    mutate(avail = coalesce(availability, avail)) |>
    select(-availability)
  aj <- which(roster$name == "AJ Dybantsa")
  stopifnot(length(aj) == 1)
  P <- nrow(roster)

  planned_of <- function(sc) {
    r <- rotations |> filter(scenario == sc)
    x <- setNames(numeric(P), roster$name)
    x[r$name] <- r$mpg
    x
  }

  set.seed(202)
  A_usual <- array(runif(P * 82 * n_sims) < roster$avail, dim = c(P, 82, n_sims))
  A_healthy <- array(roster$avail > 0, dim = c(P, 82, n_sims))
  # a two-way player can be active for at most 50 games
  two_way <- roster$name %in% official$name[official$two_way == 1]
  A_usual[two_way, 51:82, ] <- FALSE
  A_healthy[two_way, 51:82, ] <- FALSE
  A_dy <- A_usual; A_dy[roster$name %in% c("Anthony Davis", "Trae Young"), , ] <- TRUE
  # without Dybantsa he is off the roster, not an unplanned wing the fill rule
  # would hand bench minutes to (a first version left him playing about 12 a night)
  A_noaj <- A_usual; A_noaj[aj, , ] <- FALSE
  pair <- sample(n_sims, n_draws, replace = TRUE)
  talent <- function(mins) team_talent(PO, PD, mins[, pair, drop = FALSE], center) |> mutate(draw = row_number())
  rot <- function(A, pl) rotation_minutes(A, pl, roster$role, roster$v)

  # ---- the two rotations, built by rule (no hand-written minutes) ---------------------
  # Both give Dybantsa aj_minutes a night. WIN-FOCUSED sorts everyone else by
  # projected rating: the typical share of minutes for the best player at each
  # slot, the next-best behind him, and so on (depth_minutes). DEVELOPMENT-FOCUSED
  # is the same rotation except in games the Wizards have almost no chance of
  # winning, where the veterans sit and their minutes go down the same sort to the
  # younger players. A game counts as no-chance when the model gives Washington
  # under no_chance_p to win it: the gap in projected net rating to the opponent,
  # over the spread of game margins around that gap in past games (margin_sd).
  slots_all <- map(roster$role, role_slots)
  age_last <- box$box_ps |> filter(season == last) |> select(person_id, age)
  roster$age_next <- age_last$age[match(roster$person_id, age_last$person_id)] + 1
  sorted_plan <- function(sit) {
    a <- roster$avail > 0; a[sit] <- FALSE; a[aj] <- FALSE
    m <- depth_minutes(a, roster$v, slots_all, shares) * (240 - aj_minutes) / 240
    m[aj] <- aj_minutes
    m
  }
  # The same plan with the league's positional balance, as a check on the sorted one: the minutes each role gets are the
  # league's (the share of the 240 that makes two guards, two wings and a big on the floor), and inside a role the best-rated
  # player gets the share the league's top player at that role gets, the next-best the second's, and so on.
  role_min <- pgd |> filter(lottery == 0, dressed, min > 0, !is.na(role)) |> group_by(role) |> summarise(m = sum(min), .groups = "drop") |> mutate(share = m / sum(m))
  rr_shares <- pgd |> filter(lottery == 0, dressed, min > 0, !is.na(role)) |>
    group_by(game_id, team_id, role) |> arrange(desc(min), .by_group = TRUE) |>
    mutate(r = row_number(), sh = min / sum(min)) |> ungroup() |>
    group_by(role, r) |> summarise(share = mean(sh), n = n(), .groups = "drop") |> filter(n >= 200)
  balanced_plan <- function() {
    m <- numeric(P)
    for (r in role_min$role) {
      pool <- 240 * role_min$share[role_min$role == r]
      if (roster$role[aj] == r) pool <- pool - aj_minutes
      idx <- which(roster$avail > 0 & roster$role == r & seq_len(P) != aj); idx <- idx[order(-roster$v[idx])]
      if (!length(idx)) next
      sh <- rr_shares$share[rr_shares$role == r][seq_along(idx)]; sh[is.na(sh)] <- 0
      m[idx] <- pool * sh / sum(sh)
    }
    m[aj] <- aj_minutes
    m * 240 / sum(m)
  }
  is_vet <- function(age) which(!is.na(roster$age_next) & roster$age_next >= age)
  pl_win <- sorted_plan(integer(0))
  win_net <- mean(talent(rot(A_usual, pl_win))$net)
  opp <- reference |> filter(team_id != WIZARDS_ID)
  # The real released schedule (tank_schedule_2026.csv), one row per game, joined to each opponent's projected net
  # rating and given a home-court adjustment (the fitted home-minus-road term, most recent season). Two games are not
  # yet scheduled (Cup knockout-round slots); the rule below is computed on the 80 that are and scaled to 82.
  home_adj <- unname(tail(rowMeans(rapm$home), 1))
  sched_raw <- read_csv(schedule_path, show_col_types = FALSE)
  sched <- sched_raw |> left_join(opp |> select(team_tricode, net_act), by = c("opponent" = "team_tricode")) |>
    mutate(adj_gap = net_act - win_net - if_else(site == "home", home_adj, -home_adj))
  stopifnot(nrow(sched_raw) <= 82, !anyNA(sched$net_act), n_distinct(sched$opponent) == 29)
  g_opp <- sched$adj_gap                                # how much better each scheduled opponent is than the win-focused rotation, with home court
  sched_scale <- 82 / nrow(sched)
  no_chance_share <- function(p) mean(pnorm(-g_opp / margin_sd) < p)
  # expected wins given up when the win-focused rotation's talent drops by `drop` points in the games against the
  # opponents with a win chance under p, computed on the real schedule (with home court) and scaled to 82 games
  wins_lost <- function(p, total_gap) {
    f <- no_chance_share(p)
    if (f == 0) return(0)
    drop <- total_gap / f
    sel <- pnorm(-g_opp / margin_sd) < p
    sched_scale * sum(sel * (pnorm(-g_opp / margin_sd) - pnorm(-(g_opp + drop) / margin_sd)))
  }
  # a rotation is a list of (plan, share of games) parts; the season's games are split in that order
  dev_parts <- function(p, age) {
    f <- no_chance_share(p)
    list(list(pl = pl_win, share = 1 - f), list(pl = sorted_plan(is_vet(age)), share = f))
  }
  parts_of <- list(`Win-focused rotation` = list(list(pl = pl_win, share = 1)), `Development-focused rotation` = dev_parts(no_chance_p, vet_age))
  rot_parts <- function(A, parts, aj_to = NULL, within_role = TRUE, extra = 4) {
    G <- dim(A)[2]; ends <- round(cumsum(map_dbl(parts, "share")) * G); starts <- c(0, head(ends, -1)) + 1
    Reduce(`+`, map(seq_along(parts), \(i) {
      if (ends[i] < starts[i]) return(0)
      pl <- parts[[i]]$pl
      if (!is.null(aj_to)) pl <- move_aj(pl, aj_to, within_role = within_role)
      rotation_minutes(A[, starts[i]:ends[i], , drop = FALSE], pl, roster$role, roster$v, extra = extra)
    }))
  }
  rot_sc <- function(A, sc, ...) rot_parts(A, parts_of[[sc]], ...)
  pl_dev <- Reduce(`+`, map(parts_of[["Development-focused rotation"]], \(x) x$pl * x$share))
  rotations <- bind_rows(
    tibble(scenario = "Win-focused rotation", name = roster$name, mpg = pl_win)
    , tibble(scenario = "Development-focused rotation", name = roster$name, mpg = pl_dev)) |> filter(mpg > 1e-9)
  totals <- rotations |> group_by(scenario) |> summarise(total = sum(mpg))
  stopifnot(all(abs(totals$total - 240) < 1e-6))
  sens <- crossing(p = c(0.2, 0.25, 0.3, 0.4, 1), age = c(28, 30)) |>
    mutate(share = map_dbl(p, no_chance_share)
      , net = map2_dbl(p, age, \(pp, aa) mean(talent(rot_parts(A_usual, dev_parts(pp, aa)))$net))
      , wins_lost = pmap_dbl(list(p, win_net - net), wins_lost))
  # the count of opponents under the bar depends on the cover rule, which moves the win-focused rotation's level
  by_cover <- map_dfr(c(2, 4, 8), \(ex) {
    wn <- mean(talent(rotation_minutes(A_usual, pl_win, roster$role, roster$v, extra = ex))$net)
    adj <- sched$net_act - wn - if_else(sched$site == "home", home_adj, -home_adj)
    tibble(extra = ex, win_net = wn, games = sum(pnorm(-adj / margin_sd) < no_chance_p))
  })
  pl_bal <- balanced_plan()
  bal_net <- talent(rot_parts(A_usual, list(list(pl = pl_bal, share = 1))))$net
  # the on-floor mix of each rotation as simulated, against the league's
  role_floor <- function(mins) tibble(role = roster$role, m = rowMeans(mins) / 82) |> group_by(role) |> summarise(on_floor = sum(m) / 48, .groups = "drop")
  balanced <- list(net = mean(bal_net), q10 = unname(quantile(bal_net, 0.1)), q90 = unname(quantile(bal_net, 0.9)), plan = tibble(name = roster$name, mpg = pl_bal)
    , floor_win = role_floor(rot_parts(A_usual, parts_of[["Win-focused rotation"]])), floor_bal = role_floor(rot_parts(A_usual, list(list(pl = pl_bal, share = 1))))
    , floor_league = role_min |> transmute(role, on_floor = 5 * share))
  no_chance <- list(share = no_chance_share(no_chance_p), p = no_chance_p, vet_age = vet_age, margin_sd = margin_sd
    , vets = roster$name[is_vet(vet_age)], home_adj = home_adj, n_sched = nrow(sched), n_tbd = 82 - nrow(sched_raw)
      , win_prob = sched |> transmute(date = date, team = opponent, site, p_win = pnorm(-adj_gap / margin_sd), net = net_act) |> arrange(p_win)
    , sensitivity = sens, by_cover = by_cover, balanced = balanced)

  # Dybantsa's planned minutes set to m, with the difference taken from or given
  # to the other players in his role who are in the plan — a coach replaces a
  # wing with wings. The whole-rotation version is kept for comparison; it
  # hands wing minutes to bigs, who rate highest, and so flatters "without him".
  move_aj <- function(pl, m, within_role = TRUE, cap = 36) {
    delta <- pl[aj] - m
    pl[aj] <- m
    pool <- setdiff(which(pl > 0), aj)
    if (within_role) {
      same <- pool[roster$role[pool] == roster$role[aj]]
      if (length(same)) pool <- same
    }
    if (delta <= 0) {                      # he takes minutes from them, in proportion
      pl[pool] <- pmax(pl[pool] + delta * pl[pool] / sum(pl[pool]), 0)
      return(pl)
    }
    # he gives minutes back, in proportion but never past the cap; whatever the
    # group can't hold goes to the rest of the plan
    for (grp in list(pool, setdiff(which(pl > 0), c(aj, pool)))) {
      repeat {
        open <- grp[pl[grp] < cap & pl[grp] > 0]
        if (delta <= 1e-9 || !length(open) || sum(pl[open]) == 0) break
        give <- pmin(cap - pl[open], delta * pl[open] / sum(pl[open]))
        pl[open] <- pl[open] + give
        delta <- delta - sum(give)
      }
    }
    pl
  }

  mins_rule <- rule_minutes(A_usual, roster$v, map(roster$role, role_slots), shares)
  scen <- list(`Rule-based depth chart (sorted by rating)` = mins_rule)
  scen_A <- list(`Rule-based depth chart (sorted by rating)` = A_usual)
  for (sc in unique(rotations$scenario)) {
    scen[[sc]] <- rot_sc(A_usual, sc)
    scen[[paste0(sc, ", without Dybantsa")]] <- rot_sc(A_noaj, sc, aj_to = 0)
    scen[[paste0(sc, ", without Dybantsa, whole rotation absorbs")]] <- rot_sc(A_noaj, sc, aj_to = 0, within_role = FALSE)
    scen_A[[sc]] <- A_usual
    scen_A[c(paste0(sc, ", without Dybantsa"), paste0(sc, ", without Dybantsa, whole rotation absorbs"))] <- list(A_noaj)
  }
  win <- unique(rotations$scenario)[1]
  scen[[paste0(win, ", Davis and Young always available")]] <- rot(A_dy, planned_of(win))
  scen[[paste0(win, ", everyone available who is not ruled out")]] <- rot(A_healthy, planned_of(win))
  scen_A[[paste0(win, ", Davis and Young always available")]] <- A_dy
  scen_A[[paste0(win, ", everyone available who is not ruled out")]] <- A_healthy
  scenarios <- imap_dfr(scen, \(m, nm) talent(m) |> mutate(scenario = nm))
  # minutes per team game (games missed count as zero) and per game available
  minutes <- imap_dfr(scen, \(m, nm) {
    games_avail <- rowMeans(apply(scen_A[[nm]], c(1, 3), sum))
    tibble(scenario = nm, name = roster$name, mpg = rowMeans(m) / 82
      , mpg_available = if_else(games_avail > 0, rowMeans(m) / games_avail, 0))
  })

  # Dybantsa's effect inside each rotation, draw by draw
  aj_effect <- map_dfr(unique(rotations$scenario), \(sc) {
    with_aj <- scenarios |> filter(scenario == sc)
    map_dfr(c(`other wings` = ", without Dybantsa", `whole rotation` = ", without Dybantsa, whole rotation absorbs"), \(suffix) {
      without <- scenarios |> filter(scenario == paste0(sc, suffix))
      d <- with_aj$net - without$net
      tibble(rotation = sc, mean = mean(d), q10 = quantile(d, 0.1), q90 = quantile(d, 0.9), p_positive = mean(d > 0)
        , O = mean(with_aj$O - without$O), D = mean(with_aj$D - without$D))
    }, .id = "replacement")
  })

  # sweep on the win-leaning rotation, other wings absorbing the difference
  base_pl <- planned_of(win)
  sweep <- map_dfr(c(0, 12, 18, 24, 30, 36), \(m) {
    talent(rot(if (m == 0) A_noaj else A_usual, move_aj(base_pl, m))) |> mutate(aj_mpg = m)
  })
  sweep_prob <- sweep |>
    left_join(sweep |> filter(aj_mpg == 0) |> select(draw, base = net), by = "draw") |>
    group_by(aj_mpg) |>
    summarise(mean = mean(net - base), q10 = quantile(net - base, 0.1), q90 = quantile(net - base, 0.9)
      , p_positive = mean(net - base > 0), O = mean(O), D = mean(D), net = mean(net), .groups = "drop")

  # the fill rule's headroom is an assumption, so the headline differences are
  # recomputed with a tighter and a looser one
  dev_rot <- setdiff(unique(rotations$scenario), win)[1]
  fill_sensitivity <- map_dfr(c(2, 4, 8), \(ex) {
    w <- talent(rot_sc(A_usual, win, extra = ex))$net
    d <- talent(rot_sc(A_usual, dev_rot, extra = ex))$net
    wo <- talent(rot_sc(A_noaj, win, aj_to = 0, extra = ex))$net
    h <- talent(rot_sc(A_dy, win, extra = ex))$net
    tibble(extra = ex, win = mean(w), dev = mean(d), gap = mean(w - d), aj_cost = mean(wo - w), health = mean(h - w))
  })

  # adding a typical rotation backup at each position, simulated the same way:
  # he takes `add_mpg` planned minutes from that position's lowest-rated players
  backups <- backup_benchmarks(rapm, pgd, development, roles, last)
  add_mpg <- 20
  whatif_add <- map_dfr(seq_len(nrow(backups)), \(i) {
    r <- backups$role[i]
    pl <- planned_of(win)
    cand <- setdiff(which(roster$role == r & pl > 0), aj)
    cand <- cand[order(roster$v[cand])]
    need <- add_mpg
    taken <- character(0)
    for (j in cand) {
      if (need <= 1e-9) break
      t <- min(pl[j], need); pl[j] <- pl[j] - t; need <- need - t; taken <- c(taken, roster$name[j])
    }
    pl <- c(pl, add_mpg - need)
    A2 <- array(FALSE, dim = c(P + 1, 82, n_sims))
    A2[seq_len(P), , ] <- A_usual
    A2[P + 1, , ] <- runif(82 * n_sims) < av$lg
    mins2 <- rotation_minutes(A2, pl, c(roster$role, r), c(roster$v, backups$O[i] + backups$D[i]))
    t2 <- team_talent(rbind(PO, backups$O[i]), rbind(PD, backups$D[i]), mins2[, pair, drop = FALSE], center)
    base <- scenarios |> filter(scenario == win)
    d <- t2$net - base$net
    tibble(role = r, typical = backups$O[i] + backups$D[i], n_players = backups$n[i], mpg = add_mpg - need
      , from = paste(taken, collapse = ", "), mean = mean(d), q10 = quantile(d, 0.1), q90 = quantile(d, 0.9)
      , p_positive = mean(d > 0), net = mean(t2$net), draws = list(d))
  })

  bench <- role_benchmarks(rapm, roles, box, development)
  depth_by_role <- roster |>
    filter(avail > 0) |>
    group_by(role) |>
    arrange(desc(v), .by_group = TRUE) |>
    summarise(top2 = paste(head(name, 2), collapse = " and "), top2_v = mean(head(v, 2))
      , top2_O = mean(head(O_mean, 2)), top2_D = mean(head(D_mean, 2)), n = n(), .groups = "drop") |>
    left_join(bench |> select(role, league_O = O, league_D = D), by = "role") |>
    mutate(gap = top2_v - (league_O + league_D))

  list(
    roster = roster |> select(person_id, name, position, role, source, avail, note, O_mean, O_sd, D_mean, D_sd, v)
    , PO = PO, PD = PD, aj = tibble(O = PO[aj, ], D = PD[aj, ])
    , scenarios = scenarios, minutes = minutes, aj_effect = aj_effect, sweep = sweep, sweep_prob = sweep_prob
    , rotations = rotations, reference = reference, center = center, shares = shares, lg_avail = av$lg
    , depth_by_role = depth_by_role, aj_typical_mpg = rookie$table |> filter(draft_number <= 3) |> summarise(m = mean(mpg)) |> pull(m)
    , win_rotation = win, no_chance = no_chance, fill_sensitivity = fill_sensitivity, whatif_add = whatif_add, backups = backups
    , roster_dropped = roster_dropped
  )
}

# ---- Stage 5d: lineups worth seeing ----------------------------------------------
# Every five-man group from next season's available roster, scored on the
# projected ratings (additive: the model has no terms for fit, and the skill
# checks below do not supply it: tested with check_additivity(), none of them
# predicts efficiency beyond the five ratings, so they are an eligibility filter)
# and kept only if it passes basic basketball:
# one or two bigs, at least one player who creates (5+ assists per 36), at
# least two who space the floor (5+ threes per 36, and a three-season 3P%,
# shrunk toward the league, within two points of league average), and at
# least one rim protector (1.4+ blocks
# per 36). Dybantsa gets no credit for shooting or creating: the model has no
# NBA evidence on either. Last season's on-court numbers for pairs inside each
# lineup who actually played together are attached as evidence, with sample
# sizes, not as part of the score.
#
# Bench units don't face league-average fives: when a team has its reserves
# in, so, mostly, does the other team. From the last two seasons' stints
# outside garbage time, this measures how the opposing five rates (sum of
# posterior-mean O + D) against the average five, by how many of a team's
# starters are on the floor. A lineup's expected margin against the units it
# would actually see is its net against an average five minus that offset.
lineup_opponents <- function(dir_rows, pg, rapm, box, development, n_seasons = 2) {
  seasons <- tail(sort(unique(dir_rows$season)), n_seasons)
  starter_keys <- pg |> filter(starter == 1, season %in% seasons) |> transmute(k = paste(game_id, person_id)) |> pull(k)
  v <- rapm$ps_tab |> transmute(k = paste(person_id, season), v = O_mean + D_mean)
  vv <- setNames(v$v, v$k)
  rows <- dir_rows |> filter(season %in% seasons, poss > 0, !garbage)
  n_starters <- function(cols) Reduce(`+`, map(cols, \(cl) as.integer(paste(rows$game_id, rows[[cl]]) %in% starter_keys)))
  five_v <- function(cols) Reduce(`+`, map(cols, \(cl) coalesce(vv[paste(rows[[cl]], rows$season)], 0)))
  # what an average five on the floor rates, by end, in the latest season, plus
  # a year of typical development for the players in it: the baseline a
  # projected 2026-27 lineup should be measured against
  vO <- setNames(rapm$ps_tab$O_mean, paste(rapm$ps_tab$person_id, rapm$ps_tab$season))
  vD <- setNames(rapm$ps_tab$D_mean, paste(rapm$ps_tab$person_id, rapm$ps_tab$season))
  ages <- box$box_ps |> transmute(k = paste(person_id, season), age = coalesce(age, 27))
  g <- mean_growth(development, ages$age)
  gO <- setNames(g$O, ages$k); gD <- setNames(g$D, ages$k)
  five <- function(tab, cols) Reduce(`+`, map(cols, \(cl) coalesce(tab[paste(rows[[cl]], rows$season)], 0)))
  rows <- rows |> mutate(own_starters = n_starters(paste0("o", 1:5)), opp_v = five_v(paste0("d", 1:5)), own_v = five_v(paste0("o", 1:5))
    , own_O = five(vO, paste0("o", 1:5)), own_D = five(vD, paste0("o", 1:5))
    , own_gO = five(gO, paste0("o", 1:5)), own_gD = five(gD, paste0("o", 1:5)))
  # Every rating is measured against the average five of its own season, so an
  # offset does not pick up the drift in the rating scale between seasons
  # (about a point from 2024-25 to 2025-26; it moved the
  # offsets by under 0.1). A first version centered on
  # the two-season pool while the lineup scores center on the latest season.
  rows <- rows |> group_by(season) |> mutate(avg_s = weighted.mean(own_v, poss)) |> ungroup()
  last_rows <- rows |> filter(season == max(seasons))
  base <- with(last_rows, c(avg_O = weighted.mean(own_O, poss), avg_D = weighted.mean(own_D, poss)
    , grow_O = weighted.mean(own_gO, poss), grow_D = weighted.mean(own_gD, poss)))
  rows |>
    group_by(own_starters) |>
    summarise(opp_offset = weighted.mean(opp_v - avg_s, poss), own_offset = weighted.mean(own_v - avg_s, poss)
      # how much the fives actually faced vary around that typical one: a group's
      # chance against the typical opponent is not its chance against any one
      , opp_sd = sqrt(weighted.mean((opp_v - avg_s - weighted.mean(opp_v - avg_s, poss))^2, poss))
      , total_poss = sum(poss), .groups = "drop") |>
    mutate(base_season = max(seasons), share = total_poss / sum(total_poss)
      , seasons = paste(range(seasons), collapse = " to ")
      , avg_O = base[["avg_O"]], avg_D = base[["avg_D"]], grow_O = base[["grow_O"]], grow_D = base[["grow_D"]])
}

# A lineup's rating is five ratings added up, and the ratings pay bigs more
# (defensive rebounding is the heaviest term). So a five with two bigs scores
# higher than one with one big before any basketball happens. The question is
# whether that premium survives contact: if adding across roles over-credited
# big lineups, they would underperform their own sum on the floor. This
# regresses observed offensive efficiency on the two rating sums plus the number
# of bigs on each side, over the last two seasons of non-garbage stints. A
# positive coefficient on defensive bigs means big-heavy defenses allow more
# than their ratings predict.
# Points per 100 are the unit everything here is measured in, and nobody thinks
# in them. This fits the exchange rate from the same team-seasons the rest of
# the analysis uses rather than borrowing a constant: wins in 82 games against
# net rating, one line per team-season.
fit_wins_per_point <- function(team_adv_file) {
  d <- read_csv(team_adv_file, show_col_types = FALSE) |>
    janitor::clean_names() |>
    filter(gp > 0) |>
    mutate(w82 = 82 * w / gp)
  fit <- lm(w82 ~ net_rating, data = d)
  co <- summary(fit)$coefficients
  list(per_point = co["net_rating", 1], intercept = co["(Intercept)", 1], se = co["net_rating", 2]
    , r2 = summary(fit)$r.squared, n = nrow(d)
    , seasons = paste(range(d$season), collapse = " to "))
}

check_additivity <- function(dir_rows, rapm, roles, n_seasons = 2) {
  seasons <- tail(sort(unique(dir_rows$season)), n_seasons)
  key <- function(ids, ss) paste(ids, ss)
  O <- setNames(rapm$ps_tab$O_mean, key(rapm$ps_tab$person_id, rapm$ps_tab$season))
  D <- setNames(rapm$ps_tab$D_mean, key(rapm$ps_tab$person_id, rapm$ps_tab$season))
  B <- setNames(roles$role == "Big", key(roles$person_id, roles$season))
  # The same three flags the basketball checks use, defined the same way:
  # a creator (5+ assists per 36 in the season), a shooter (5+ three-point
  # attempts per 36 in the season, and a three-point percentage pooled over the
  # three seasons ending then, shrunk with 100 attempts, within two points of
  # the league's over the same window) and a rim protector (1.4+ blocks per 36).
  all_seasons <- sort(unique(roles$season))
  window <- function(s) tail(all_seasons[all_seasons <= s], 3)
  fg3 <- map_dfr(seasons, \(sn) {
    w <- roles |> filter(season %in% window(sn))
    lg <- sum(w$threePointersMade) / sum(w$threePointersAttempted)
    w |> group_by(person_id) |>
      summarise(fg3 = (sum(threePointersMade) + 100 * lg) / (sum(threePointersAttempted) + 100), .groups = "drop") |>
      mutate(season = sn, lg3 = lg)
  })
  fl <- roles |> filter(season %in% seasons) |>
    left_join(fg3, by = c("person_id", "season")) |>
    mutate(creator = coalesce(r_assists >= 5, FALSE), rim = coalesce(r_blocks >= 1.4, FALSE)
      , spacer = coalesce(r_threePointersAttempted >= 5 & fg3 >= lg3 - 0.02, FALSE))
  F <- map(c(creator = "creator", spacer = "spacer", rim = "rim"), \(v) setNames(fl[[v]], key(fl$person_id, fl$season)))
  rows <- dir_rows |> filter(season %in% seasons, poss >= 2, !garbage)
  sum_of <- function(tab, cols) Reduce(`+`, map(cols, \(c) coalesce(tab[key(rows[[c]], rows$season)], 0)))
  cnt <- function(tab, cols) Reduce(`+`, map(cols, \(c) as.integer(coalesce(tab[key(rows[[c]], rows$season)], FALSE))))
  o <- paste0("o", 1:5); d <- paste0("d", 1:5)
  x <- tibble(
    poss = rows$poss, ortg = 100 * rows$pts / rows$poss, home = rows$home_off
    , sO = sum_of(O, o), sD = sum_of(D, d)
    , bigO = cnt(B, o), bigD = cnt(B, d)
    , creatorO = cnt(F$creator, o), creatorD = cnt(F$creator, d)
    , spacerO = cnt(F$spacer, o), spacerD = cnt(F$spacer, d)
    , rimO = cnt(F$rim, o), rimD = cnt(F$rim, d)
  ) |> filter(is.finite(ortg))
  # the effect of one more unit on the five's own net rating beyond its ratings:
  # an offense-side term counts as it stands, a defense-side term (points
  # allowed) with its sign flipped
  effects <- function(fit, units) {
    co <- summary(fit)$coefficients; V <- vcov(fit)
    map_dfr(units, \(u) { a <- paste0(u, "O"); b <- paste0(u, "D")
      tibble(unit = u, effect = co[a, 1] - co[b, 1], se = sqrt(V[a, a] + V[b, b] - 2 * V[a, b])) })
  }
  fit <- lm(ortg ~ sO + sD + home + bigO + bigD + creatorO + creatorD + spacerO + spacerD + rimO + rimD, data = x, weights = poss)
  co <- summary(fit)$coefficients
  # two other specifications: bigs alone, and the lineup scorer's own 1:1 sum
  # as an offset (it adds offense and defense at equal weight)
  fit_big <- lm(ortg ~ sO + sD + home + bigO + bigD, data = x, weights = poss)
  fit_11 <- lm(I(ortg - sO + sD) ~ home + bigO + bigD + creatorO + creatorD + spacerO + spacerD + rimO + rimD, data = x, weights = poss)
  eff_main <- effects(fit, c("big", "creator", "spacer", "rim"))
  spec <- bind_rows(
    effects(fit, "big") |> mutate(spec = "ratings free, all four role counts")
    , effects(fit_big, "big") |> mutate(spec = "ratings free, bigs only")
    , effects(fit_11, "big") |> mutate(spec = "ratings added 1:1, all four role counts")
  ) |> select(spec, effect, se)
  # a point of defensive rating against a point of offensive rating: the fitted
  # weight, and what the model's own shrinkage would produce with no basketball
  # in it (a slope on a shrunken estimate is 1 / shrinkage, and defense is
  # shrunk harder)
  ps <- rapm$ps_tab |> filter(season %in% seasons)
  shr <- function(m, sd, w) { vm <- sum(w * (m - weighted.mean(m, w))^2) / sum(w); vm / (vm + weighted.mean(sd^2, w)) }
  shrink_O <- shr(ps$O_mean, ps$O_sd, ps$min); shrink_D <- shr(ps$D_mean, ps$D_sd, ps$min)
  list(
    table = tibble(term = rownames(co), estimate = co[, 1], se = co[, 2])
    , effects = eff_main, spec = spec
    , poss = sum(x$poss), stints = nrow(x), seasons = paste(range(seasons), collapse = " to ")
    , net_per_big = eff_main$effect[eff_main$unit == "big"], net_se = eff_main$se[eff_main$unit == "big"]
    , d_over_o = -co["sD", 1] / co["sO", 1]
    , shrink_O = shrink_O, shrink_D = shrink_D, d_over_o_from_shrinkage = shrink_O / shrink_D
  )
}

# The official roster: Basketball-Reference's list for the upcoming season, kept
# by hand in tank_roster_bbref_2026.csv (with the date it was read). The player
# index the pipeline fetches can lag the team: it still listed a player who has
# left, so the projection keeps only the players named here. (TW) marks the
# two-way contracts in the source.
read_bbref_roster <- function(path) {
  d <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c", two_way = "i"))
  stopifnot(nrow(d) == 17, n_distinct(d$name) == nrow(d), !anyNA(d$name), all(d$two_way %in% 0:1), n_distinct(d$retrieved) == 1)
  d |> mutate(retrieved = as.Date(retrieved))
}

# A depth chart for the 2026-27 Wizards, kept by hand as it was read (ESPN's from
# https://www.espn.com/nba/team/depth/_/name/wsh, RealGM's from a screenshot,
# since RealGM blocks automated access), with the date. rank is the row on the
# page, and rank 1 is the starter. Deferring to an outside chart for who starts
# replaces an earlier version that called the five biggest minute loads in the
# win-focused rotation the starters, which made "who starts" one of this
# analysis's own choices. A chart may list players who are not on the
# roster, and omit players who are, so only the starters are required to be.
read_depth_chart <- function(path, roster_names) {
  d <- read_csv(path, show_col_types = FALSE) |> mutate(retrieved = as.Date(retrieved))
  starters <- d |> filter(rank == 1) |> pull(name)
  stopifnot(
    length(starters) == 5, !anyNA(starters), n_distinct(starters) == 5
    , all(starters %in% roster_names), n_distinct(d$retrieved) == 1
    , !anyDuplicated(d[c("position", "rank")])
  )
  list(depth = d, starters = starters, retrieved = unique(d$retrieved), source = unique(d$source))
}

# Which five is the best on each of the projection's draws? A group's talent
# score is a mean over draws, and "the best group" is an argmax over thousands of
# groups, so the chance that a particular five is the best, and the chance that
# the best five has a given property, are different from a comparison between
# two groups chosen in advance. This takes the argmax draw by draw, over the same
# eligible groups, and counts how many groups clear each bar on each draw.
argmax_draws <- function(projection, lineups) {
  ros <- projection$roster
  a <- lineups$all |> filter(checks)
  groups <- map(a$players, \(x) str_split_1(x, ", "))
  base <- sum(lineups$base)
  M <- sapply(groups, \(g) { i <- match(g, ros$name); colSums(projection$PO[i, , drop = FALSE]) + colSums(projection$PD[i, , drop = FALSE]) - base })
  win <- max.col(M, ties.method = "first")
  core <- c("AJ Dybantsa", "Alex Sarr", "Bilal Coulibaly")
  has <- function(nm) map_lgl(groups[win], \(g) nm %in% g)
  players <- unique(unlist(groups))
  best_pl <- a$players[which.max(a$net)]
  espn_i <- which(a$players == a$players[map_lgl(groups, \(g) setequal(g, lineups$starters))])
  off <- setNames(lineups$opponents$opp_offset, lineups$opponents$own_starters)
  k0 <- which(a$n_starters == 0)
  aj_i <- which(map_lgl(groups, \(g) "AJ Dybantsa" %in% g))
  up <- rowSums(M > 0); up_aj <- rowSums(M[, aj_i, drop = FALSE] > 0)
  rk <- function(i) 1 + rowSums(M > M[, i])
  list(
    n_draws = nrow(M), n_groups = ncol(M)
    , p_excl_aj = mean(!has("AJ Dybantsa")), p_excl_core = mean(!map_lgl(groups[win], \(g) any(core %in% g)))
    , top = tibble(players = a$players[win]) |> count(players, sort = TRUE) |> mutate(share = n / nrow(M))
    , per_player = tibble(name = players, share = map_dbl(players, \(nm) mean(has(nm)))) |> arrange(desc(share))
    , best_share = mean(a$players[win] == best_pl), best_rank = quantile(rk(which(a$players == best_pl)), c(0.5, 0.1, 0.9))
    , espn_rank = quantile(rk(espn_i), c(0.5, 0.1, 0.9))
    , n_up = c(mean = mean(up), quantile(up, c(0.1, 0.9))), n_up_aj = c(mean = mean(up_aj), quantile(up_aj, c(0.1, 0.9)))
    , clear0_any = mean(rowSums(M[, k0, drop = FALSE] > off[["0"]]) > 0), clear0_mean = mean(rowSums(M[, k0, drop = FALSE] > off[["0"]]))
  )
}

# How well do the ratings separate a team's leading player at a role from his
# backups? Ratings built mostly from box scores can order guards and wings by
# how they play and still fail to order bigs, where rebounds and blocks follow
# position more than quality. For each role: the mean rating of a team's leader
# (most minutes), of the rest, and the correlation between minutes and rating.
role_resolution <- function(rapm, roles, pgd, season, min_min = 1000) {
  tm <- pgd |>
    filter(season == !!season, min > 0) |>
    group_by(person_id, team_id) |> summarise(m = sum(min), .groups = "drop") |>
    group_by(person_id) |> mutate(total = sum(m)) |> slice_max(m, n = 1, with_ties = FALSE) |> ungroup() |>
    filter(total >= min_min)
  tm |>
    left_join(rapm$ps_tab |> filter(season == !!season) |> transmute(person_id, v = O_mean + D_mean), by = "person_id") |>
    left_join(roles |> filter(season == !!season) |> select(person_id, role), by = "person_id") |>
    filter(!is.na(v), !is.na(role)) |>
    group_by(team_id, role) |> mutate(rk = rank(-total, ties.method = "first")) |> ungroup() |>
    group_by(role) |>
    summarise(leader = mean(v[rk == 1]), backup = mean(v[rk > 1]), r = cor(total, v)
      , n_leaders = sum(rk == 1), n_backups = sum(rk > 1), .groups = "drop")
}

best_lineups <- function(projection, roles, dir_rows, opponents, depth) {
  ros <- projection$roster |> mutate(idx = row_number()) |> filter(avail > 0)
  # 3P% is the noisiest of these checks (one season put Trae Young a hair under
  # the line), so it pools each player's last three seasons; volume, passing
  # and shot-blocking come from his latest season
  recent3 <- tail(sort(unique(roles$season)), 3)
  lg3 <- roles |> filter(season %in% recent3) |> summarise(p = sum(threePointersMade) / sum(threePointersAttempted)) |> pull(p)
  fg3 <- roles |>
    filter(season %in% recent3) |>
    group_by(person_id) |>
    summarise(fg3 = (sum(threePointersMade) + 100 * lg3) / (sum(threePointersAttempted) + 100), .groups = "drop")
  feats <- roles |>
    group_by(person_id) |>
    slice_max(season, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(person_id, ast36 = r_assists, blk36 = r_blocks, tpa36 = r_threePointersAttempted) |>
    left_join(fg3, by = "person_id")
  # a spacer is a shooter a defense has to stay attached to: real volume, and
  # accuracy within two points of league average (a fixed 35% line sat
  # exactly on top of two of Washington's rotation players)
  ros <- ros |>
    left_join(feats, by = "person_id") |>
    mutate(
      creator = coalesce(ast36 >= 5, FALSE), spacer = coalesce(tpa36 >= 5 & fg3 >= lg3 - 0.02, FALSE)
      , rim = coalesce(blk36 >= 1.4, FALSE), big = role == "Big"
    )
  # every group is scored; the basketball checks are a flag, so the effect of
  # imposing them can be shown. Starters are ESPN's projected starting five
  # (read_depth_chart), so a group's starter count, and with it the opponent
  # adjustment below, doesn't rest on this analysis's own rotation plans.
  starters <- depth$starters
  opp_offset <- setNames(opponents$opp_offset, opponents$own_starters)
  # a league-average five as teams actually play them, aged a year. A first
  # version subtracted the average of rating-sorted depth charts (about 4.4
  # points higher), which made every lineup look worse than it is.
  base_O <- opponents$avg_O[1] + opponents$grow_O[1]
  base_D <- opponents$avg_D[1] + opponents$grow_D[1]
  combos <- combn(nrow(ros), 5)
  PO <- projection$PO; PD <- projection$PD
  all_scores <- map_dfr(seq_len(ncol(combos)), \(j) {
    r <- ros[combos[, j], ]
    o_draws <- colSums(PO[r$idx, , drop = FALSE]) - base_O
    d_draws <- colSums(PD[r$idx, , drop = FALSE]) - base_D
    n_draws <- o_draws + d_draws
    k <- sum(r$name %in% starters)
    tibble(lineup = j, players = paste(r$name, collapse = ", ")
      , checks = sum(r$big) %in% 1:2 && sum(r$creator) >= 1 && sum(r$spacer) >= 2 && sum(r$rim) >= 1
      , has_aj = "AJ Dybantsa" %in% r$name, has_trae = "Trae Young" %in% r$name
      , young = sum(r$name %in% c("Alex Sarr", "Bilal Coulibaly", "Kyshawn George", "Tre Johnson", "Bub Carrington", "Will Riley"))
      , n_starters = k
      , O = mean(o_draws), D = mean(d_draws), net = mean(n_draws), q10 = quantile(n_draws, 0.1), q90 = quantile(n_draws, 0.9)
      , p_positive = mean(n_draws > 0)
      # against the units it would typically face, given how many starters it has
      , vs_opp = mean(n_draws) - opp_offset[[as.character(k)]], p_beats_opp = mean(n_draws > opp_offset[[as.character(k)]]))
  })
  score <- all_scores |> filter(checks)
  # each lineup must differ from every one already chosen by at least two
  # players, so the three are three ideas rather than one lineup with a sub
  pick <- function(d, label, chosen) {
    if (nrow(chosen)) {
      prev <- str_split(chosen$players, ", ")
      d <- d |> filter(map_lgl(players, \(p) all(map_int(prev, \(q) length(intersect(str_split_1(p, ", "), q))) <= 3)))
    }
    d |> slice_max(net, n = 1, with_ties = FALSE) |> mutate(label = label)
  }
  l1 <- pick(score |> filter(has_aj, has_trae), "Young and Dybantsa together", tibble())
  l2 <- pick(score |> filter(has_aj, !has_trae), "Dybantsa's unit, Young resting", l1)
  l3 <- pick(score |> filter(has_aj, young >= 3), "The next core", bind_rows(l1, l2))
  best_without_aj <- score |> filter(!has_aj) |> slice_max(net, n = 1, with_ties = FALSE)
  chosen <- bind_rows(l1, l2, l3)

  # on-court evidence for pairs inside each chosen lineup, last two seasons
  recent <- dir_rows |> filter(season %in% tail(sort(unique(dir_rows$season)), 2))
  team_season_net <- recent |>
    group_by(season, team = off_team) |> summarise(pf = sum(pts), po = sum(poss), .groups = "drop") |>
    left_join(recent |> group_by(season, team = def_team) |> summarise(pa = sum(pts), pd = sum(poss), .groups = "drop"), by = c("season", "team")) |>
    transmute(season, team, net = 100 * (pf / po - pa / pd))
  on_pairs <- function(rows, cols, team_col) {
    rows |>
      mutate(row = row_number()) |>
      select(row, season, team = all_of(team_col), pts, poss, all_of(cols)) |>
      pivot_longer(all_of(cols), values_to = "person_id") |>
      select(-name)
  }
  pair_evidence <- map_dfr(seq_len(nrow(chosen)), \(i) {
    ids <- ros$person_id[match(str_split_1(chosen$players[i], ", "), ros$name)]
    ids <- ids[!is.na(ids)]
    pairs <- combn(ids, 2)
    offl <- on_pairs(recent |> filter(if_any(o1:o5, \(x) x %in% ids)), paste0("o", 1:5), "off_team") |> filter(person_id %in% ids)
    defl <- on_pairs(recent |> filter(if_any(d1:d5, \(x) x %in% ids)), paste0("d", 1:5), "def_team") |> filter(person_id %in% ids)
    map_dfr(seq_len(ncol(pairs)), \(k) {
      a <- pairs[1, k]; b <- pairs[2, k]
      o <- offl |> filter(person_id %in% c(a, b)) |> group_by(row, season, team, pts, poss) |> filter(n() == 2) |> ungroup() |> distinct(row, season, team, pts, poss)
      d <- defl |> filter(person_id %in% c(a, b)) |> group_by(row, season, team, pts, poss) |> filter(n() == 2) |> ungroup() |> distinct(row, season, team, pts, poss)
      # below about 250 possessions a side, a pair's net rating is mostly noise
      if (sum(o$poss) < 250 || sum(d$poss) < 250) return(NULL)
      # the same team-seasons' overall net, weighted like the pair's minutes, so
      # the comparison is "better or worse than the team they played for"
      shared <- bind_rows(o, d) |> count(season, team, wt = poss, name = "w")
      team_net <- shared |> left_join(team_season_net, by = c("season", "team")) |> summarise(x = weighted.mean(net, w)) |> pull(x)
      pair_net <- 100 * (sum(o$pts) / sum(o$poss) - sum(d$pts) / sum(d$poss))
      tibble(label = chosen$label[i], pair = paste(ros$name[match(c(a, b), ros$person_id)], collapse = " + ")
        , poss = sum(o$poss) + sum(d$poss), net = pair_net, team_net = team_net, relative = pair_net - team_net)
    })
  })
  # how much the answers move with the thresholds and the opponent offset
  flags <- function(tpa, tol, rim) list(
    creator = coalesce(ros$ast36 >= 5, FALSE)
    , spacer = coalesce(ros$tpa36 >= tpa & ros$fg3 >= lg3 - tol, FALSE)
    , rim = coalesce(ros$blk36 >= rim, FALSE), big = ros$role == "Big")
  # (the cap on bigs is the one rule that decides the top of the list, so it is
  # varied here too: with the other three checks dropped the best group is the same)
  grid <- expand_grid(tpa = c(4.5, 5, 5.5), tol = c(0.01, 0.02, 0.03), rim = c(1.3, 1.4, 1.5), maxbig = 1:3, shift = c(-1, 0, 1)) |>
    mutate(res = pmap(list(tpa, tol, rim, maxbig, shift), \(tpa, tol, rim, maxbig, shift) {
      f <- flags(tpa, tol, rim)
      cnt <- function(x) colSums(matrix(x[combos], nrow = 5))
      ok <- cnt(f$big) %in% 1:maxbig & cnt(f$creator) >= 1 & cnt(f$spacer) >= 2 & cnt(f$rim) >= 1
      w <- ok & all_scores$vs_opp > shift
      # the same question on talent alone, which does not depend on who starts
      up <- ok & all_scores$net > shift
      top <- all_scores$players[ok][which.max(all_scores$net[ok])]
      tibble(n_pass = sum(ok), n_win = sum(w), n_win_aj = sum(w & all_scores$has_aj)
        , win_has_young = if (sum(w) == 0) NA else mean(map_lgl(all_scores$players[w], \(p) "Trae Young" %in% str_split_1(p, ", ")))
        , n_up = sum(up), n_up_aj = sum(up & all_scores$has_aj)
        , up_has_young = if (sum(up) == 0) NA else mean(map_lgl(all_scores$players[up], \(p) "Trae Young" %in% str_split_1(p, ", ")))
        , top_players = top, top_net = max(all_scores$net[ok])
        , top_without_core = !any(c("AJ Dybantsa", "Alex Sarr", "Bilal Coulibaly") %in% str_split_1(top, ", "))
        , top_without_aj = !("AJ Dybantsa" %in% str_split_1(top, ", ")))
    })) |>
    unnest(res)
  # the best group under the cap on bigs alone, with no other check
  nb <- colSums(matrix(flags(5, 0.02, 1.4)$big[combos], nrow = 5))
  bigcap_only <- all_scores[nb %in% 1:2, ] |> slice_max(net, n = 1, with_ties = FALSE)
  list(chosen = chosen, best_without_aj = best_without_aj, candidates = score |> arrange(desc(net))
    , bigcap_only = bigcap_only
    , thresholds = grid, base = c(O = base_O, D = base_D)
    , n_candidates = nrow(score), features = ros |> select(name, role, creator, spacer, rim, big, ast36, tpa36, fg3, blk36, O_mean, D_mean)
    , pair_evidence = pair_evidence
    , all = all_scores |> arrange(desc(net)), starters = starters, opponents = opponents)
}

context_persistence <- function(rapm) {
  ts_tab <- rapm$ts_tab |> mutate(season_start = as.integer(str_sub(season, 1, 4)))
  pairs <- ts_tab |>
    inner_join(ts_tab |> transmute(team_id, ts_next = ts, season_start = season_start - 1L), by = c("team_id", "season_start"))
  map_dbl(seq_len(ncol(rapm$cO_draws)), \(d) cor(
    rapm$cO_draws[pairs$ts, d] + rapm$cD_draws[pairs$ts, d]
    , rapm$cO_draws[pairs$ts_next, d] + rapm$cD_draws[pairs$ts_next, d]))
}

# ---- Stage 6: sensitivity ----------------------------------------------------------
# Each alternative changes one choice and reports Washington's 2025-26
# decomposition. Refits reuse the main variance components.

sensitivity_refit <- function(label, team_rows, games_path, box, hyper, pg, player_index, roles, poss_scale
    , K = 6, drop_garbage = TRUE) {
  lot <- build_lottery(games_path, SEASONS_ALL, K = K)
  rows <- label_rows(team_rows, lot)
  fit <- fit_rapm(rows, box, SEASONS, hyper = hyper, drop_garbage = drop_garbage)
  dc <- decompose(fit, pg, rows, lot, player_index, roles, poss_scale, keep_detail = FALSE)
  sensitivity_summary(dc, label) |>
    mutate(lot_O = mean(fit$lot_draws["O", ]), lot_D = mean(fit$lot_draws["D", ]))
}

# ---- Stage 7: out-of-sample checks -----------------------------------------------------
# Refit on the seasons before the target season — variance components included,
# warm-started from the main fit but optimised on training data only — project
# each player's talent for the target season the same way the 2026-27
# projection does, weight by the minutes teams actually played, and compare
# with what happened. Two fairer baselines than last season's team net rating
# get the same actual minutes: each player's previous-season on-court net
# (shrunk toward zero with 4,000 possessions) and each player's previous team's
# net rating. Actual minutes carry in-season information (trades, injuries,
# the tank itself), which every predictor here shares.

# actual team ratings from the stints, centred per season and put on nba.com's
# possession scale (the same numbers the decomposition's "actual" row holds)
actual_ratings <- function(team_rows, poss_scale) {
  ratio <- setNames(poss_scale$poss_ratio, poss_scale$season)
  team_rows |>
    group_by(season, team_id = off_team) |> summarise(pf = sum(pts), po = sum(poss), .groups = "drop") |>
    left_join(team_rows |> group_by(season, team_id = def_team) |> summarise(pa = sum(pts), pd = sum(poss), .groups = "drop"), by = c("season", "team_id")) |>
    group_by(season) |>
    mutate(actual_O = (100 * pf / po - mean(100 * pf / po)) * ratio[season], actual_D = -(100 * pa / pd - mean(100 * pa / pd)) * ratio[season]) |>
    ungroup() |>
    select(season, team_id, actual_O, actual_D)
}

run_backtest <- function(target, team_rows, pg, games_path, bio_path, player_index, hyper, dev_stan, rookie_stan, poss_scale
    , n_draws = 1000, optimize = TRUE) {
  bt_seasons <- SEASONS[SEASONS < target]
  target_start <- as.integer(str_sub(target, 1, 4))
  prev <- SEASONS[match(target, SEASONS) - 1]
  pg_bt <- pg |> filter(season %in% bt_seasons)
  lot_bt <- build_lottery(games_path, SEASONS_ALL[SEASONS_ALL < target])
  rows_bt <- label_rows(team_rows |> filter(season %in% bt_seasons), lot_bt)
  box_bt <- build_box_prior(pg_bt, bio_path, player_index)
  rapm_bt <- if (optimize) fit_rapm(rows_bt, box_bt, bt_seasons, init = hyper, n_draws = n_draws) else fit_rapm(rows_bt, box_bt, bt_seasons, hyper = hyper, n_draws = n_draws)
  roles_bt <- assign_roles(box_bt, player_index)
  pgd_bt <- player_game_deployment(pg_bt, rapm_bt, lot_bt, player_index, roles_bt, 250)
  dev_bt <- fit_development(rapm_bt, build_dev_pairs(rapm_bt, pgd_bt), dev_stan, run_checks = FALSE)
  rk_bt <- fit_rookie(rapm_bt, pg_bt, player_index, roles_bt, rookie_stan)

  actuals <- actual_ratings(team_rows, poss_scale)
  mins_t <- pg |> filter(season == target, min > 0) |> group_by(team_id, team_tricode, person_id) |>
    summarise(m = sum(min), .groups = "drop")
  pids <- unique(mins_t$person_id)
  proj <- project_players(pids, target_start, rapm_bt, dev_bt, rk_bt, player_index, n_draws)
  late <- rapm_bt$ps_tab |>
    mutate(season_start = as.integer(str_sub(season, 1, 4))) |>
    left_join(player_index |> select(person_id, draft_number, from_year), by = "person_id") |>
    filter(coalesce(from_year, season_start) == season_start, coalesce(draft_number, 99) > 30) |>
    summarise(O = weighted.mean(O_mean, min), D = weighted.mean(D_mean, min))
  n_late <- sum(map_lgl(proj, is.null))
  nohist_ids <- as.numeric(names(proj))[map_lgl(proj, is.null)]
  proj <- map(proj, \(p) if (is.null(p)) list(O = rep(late$O, n_draws), D = rep(late$D, n_draws)) else p)
  PO <- do.call(rbind, map(proj, "O")); PD <- do.call(rbind, map(proj, "D"))
  row_of <- match(mins_t$person_id, as.numeric(names(proj)))

  # baselines from the previous season's stints
  prev_rows <- team_rows |> filter(season == prev)
  on_court <- full_join(
      prev_rows |> select(pts, poss, o1:o5) |> pivot_longer(o1:o5, values_to = "person_id") |>
        group_by(person_id) |> summarise(pf = sum(pts), po = sum(poss), .groups = "drop")
      , prev_rows |> select(pts, poss, d1:d5) |> pivot_longer(d1:d5, values_to = "person_id") |>
        group_by(person_id) |> summarise(pa = sum(pts), pd = sum(poss), .groups = "drop")
      , by = "person_id") |>
    mutate(net = 100 * (pf / po - pa / pd), shrunk = net * (po + pd) / (po + pd + 4000))
  prev_team_net <- prev_rows |>
    group_by(team_id = off_team) |> summarise(pf = sum(pts), po = sum(poss), .groups = "drop") |>
    left_join(prev_rows |> group_by(team_id = def_team) |> summarise(pa = sum(pts), pd = sum(poss), .groups = "drop"), by = "team_id") |>
    mutate(team_net = 100 * (pf / po - pa / pd))
  prev_primary <- pg |> filter(season == prev, min > 0) |> group_by(person_id, team_id) |> summarise(m = sum(min), .groups = "drop") |>
    group_by(person_id) |> slice_max(m, n = 1, with_ties = FALSE) |> ungroup() |>
    left_join(prev_team_net |> select(team_id, team_net), by = "team_id") |> select(person_id, prev_team_net = team_net)

  teams <- mins_t |>
    mutate(row = row_of) |>
    left_join(on_court |> select(person_id, shrunk), by = "person_id") |>
    left_join(prev_primary, by = "person_id") |>
    group_by(team_id, team_tricode) |>
    summarise(
      pred_O = mean(colSums((5 * m / sum(m)) * PO[row, , drop = FALSE]))
      , pred_D = mean(colSums((5 * m / sum(m)) * PD[row, , drop = FALSE]))
      , base_oncourt = sum(m * coalesce(shrunk, 0)) / sum(m)
      , base_prevteam = sum(m * coalesce(prev_team_net, 0)) / sum(m)
      , .groups = "drop") |>
    mutate(across(c(pred_O, pred_D, base_oncourt, base_prevteam), \(x) x - mean(x)), pred_net = pred_O + pred_D) |>
    left_join(actuals |> filter(season == target) |> select(team_id, actual_O, actual_D), by = "team_id") |>
    left_join(actuals |> filter(season == prev) |> transmute(team_id, prev_net = actual_O + actual_D), by = "team_id") |>
    mutate(actual_net = actual_O + actual_D)
  # the spread of each team's projection over the draws (ratings only: the minutes are the ones actually played)
  TD <- t(sapply(teams$team_id, \(tid) {
    i <- which(mins_t$team_id == tid); w <- 5 * mins_t$m[i] / sum(mins_t$m[i])
    colSums(w * PO[row_of[i], , drop = FALSE]) + colSums(w * PD[row_of[i], , drop = FALSE])
  }))
  TD <- sweep(TD, 2, colMeans(TD))
  teams <- teams |> mutate(pred_q10 = apply(TD, 1, quantile, 0.1), pred_q90 = apply(TD, 1, quantile, 0.9), pred_sd = apply(TD, 1, sd))
  r2 <- function(x) cor(x, teams$actual_net)^2
  rmse <- function(x) sqrt(mean((teams$actual_net - x)^2))
  list(
    ids = as.numeric(names(proj)), PO = PO, PD = PD, no_history_ids = nohist_ids
    , target = target, train_seasons = bt_seasons, hyper = rapm_bt$hyper, teams = teams
    , n_players = length(pids), n_no_history = n_late
    , r2 = c(model = r2(teams$pred_net), oncourt = r2(teams$base_oncourt), prev_team = r2(teams$base_prevteam), prev_net = r2(teams$prev_net))
    , r2_O = cor(teams$pred_O, teams$actual_O)^2, r2_D = cor(teams$pred_D, teams$actual_D)^2
    , rmse = c(model = rmse(teams$pred_net), prev_net = rmse(teams$prev_net))
    , slope = unname(coef(lm(actual_net ~ pred_net, data = teams))[2])
  )
}


# Out-of-sample check on five-man groups. The projections in `bt` were made from
# seasons before the target season only. Each five that actually played in the
# target season (at least min_poss possessions on both offense and defense) is
# scored the way the report scores a group: its five players' projected offense
# and defense added up, against the possession-weighted average five. The outcome
# is the five's realized net rating in the stints (garbage time excluded), which
# does not come from the rating model.
# sigma is the residual SD per possession, x100: the method-of-moments estimate from the 2025-26 non-garbage stints (points minus
# possessions times the lineup's mean, summed within stint) is 117.70, the same as the rating model's own estimate.
backtest_lineups <- function(bt, team_rows, min_poss = 50, sigma = 117.7) {
  key <- function(m) apply(m, 1, \(x) paste(sort(x), collapse = "-"))
  rows <- team_rows |> filter(season == bt$target, !garbage, poss > 0)
  O <- as.matrix(rows[paste0("o", 1:5)]); D <- as.matrix(rows[paste0("d", 1:5)])
  off <- tibble(team = rows$off_team, k = key(O), pts = rows$pts, poss = rows$poss) |>
    group_by(team, k) |> summarise(pf = sum(pts), po = sum(poss), .groups = "drop")
  def <- tibble(team = rows$def_team, k = key(D), pts = rows$pts, poss = rows$poss) |>
    group_by(team, k) |> summarise(pa = sum(pts), pd = sum(poss), .groups = "drop")
  lu <- inner_join(off, def, by = c("team", "k")) |>
    mutate(net = 100 * (pf / po - pa / pd), n = 2 / (1 / po + 1 / pd), sd_noise = sigma * sqrt(1 / po + 1 / pd))
  ids <- lapply(strsplit(lu$k, "-"), as.numeric)
  keep <- map_lgl(ids, \(x) all(x %in% bt$ids))
  lu <- lu[keep, ]; ids <- ids[keep]
  draws <- t(sapply(ids, \(x) { i <- match(x, bt$ids); colSums(bt$PO[i, , drop = FALSE]) + colSums(bt$PD[i, , drop = FALSE]) }))
  w_all <- lu$po + lu$pd
  # center the projected score on the possession-weighted average five, draw by draw
  draws <- sweep(draws, 2, colSums(draws * w_all) / sum(w_all))
  lu <- lu |> mutate(proj = rowMeans(draws), proj_sd = apply(draws, 1, sd)) |> mutate(row = row_number())
  use <- which(lu$po >= min_poss & lu$pd >= min_poss)
  d <- lu[use, ]
  fit <- lm(net ~ proj, data = d, weights = n)
  # within-team: demean projected and realized by team (possession weighted)
  wm <- function(x, w) sum(x * w) / sum(w)
  d <- d |> group_by(team) |> mutate(proj_w = proj - wm(proj, n), net_w = net - wm(net, n)) |> ungroup()
  fit_w <- lm(net_w ~ proj_w, data = d, weights = n)
  wr2 <- function(x, y, w) { mx <- wm(x, w); my <- wm(y, w); sum(w * (x - mx) * (y - my))^2 / (sum(w * (x - mx)^2) * sum(w * (y - my)^2)) }
  # what R-squared a perfectly calibrated projection would get, given only the sampling noise in each five's realized net
  set.seed(202)
  # (the truth is taken as the projection rescaled by the fitted slope, since the projection's spread is too wide by that much)
  truth <- coef(fit)[[1]] + coef(fit)[[2]] * d$proj
  sim_r2 <- mean(replicate(200, { y <- truth + rnorm(nrow(d), 0, d$sd_noise); wr2(d$proj, y, d$n) }))
  # standard errors that let the fives of one team share what they share: a bootstrap over teams
  boot <- replicate(500, { tm <- sample(unique(d$team), replace = TRUE); dd <- bind_rows(lapply(seq_along(tm), \(i) d |> filter(team == tm[i]) |> mutate(team = i)))
    c(coef(lm(net ~ proj, data = dd, weights = n))[[2]], coef(lm(net_w ~ proj_w, data = dd, weights = n))[[2]]) })
  z <- (d$net - d$proj) / sqrt(d$proj_sd^2 + d$sd_noise^2)
  z_slope <- (d$net - coef(fit)[[1]] - coef(fit)[[2]] * d$proj) / sqrt((coef(fit)[[2]] * d$proj_sd)^2 + d$sd_noise^2)
  d <- d |> mutate(bin = cut(proj, breaks = wtd_quantile(proj, n, seq(0, 1, 0.2)), include.lowest = TRUE, labels = paste0("Q", 1:5)))
  bins <- d |> group_by(bin) |> summarise(fives = n(), poss = sum(n), proj = wm(proj, n), realized = wm(net, n)
    , se = sqrt(sum(n * (net - wm(net, n))^2) / sum(n) / fives), .groups = "drop")
  list(target = bt$target, n_fives = nrow(d), poss = sum(d$n), min_poss = min_poss
    , slope = unname(coef(fit)[2]), slope_se = sd(boot[1, ]), intercept = unname(coef(fit)[1])
    , r2 = wr2(d$proj, d$net, d$n), r2_ceiling = sim_r2
    , within_slope = unname(coef(fit_w)[2]), within_slope_se = sd(boot[2, ]), within_r2 = wr2(d$proj_w, d$net_w, d$n), within_n_teams = n_distinct(d$team)
    , z_sd = sd(z), z_cover80 = mean(abs(z) < qnorm(0.9)), z_bias = mean(z)
    , bins = bins, lineups = d |> select(team, k, n, po, pd, net, proj, proj_sd, sd_noise))
}

wtd_quantile <- function(x, w, probs) {
  o <- order(x); cw <- cumsum(w[o]) / sum(w)
  q <- sapply(probs, \(p) x[o][which(cw >= p)[1]])
  q[1] <- min(x); q[length(q)] <- max(x); unique(q)
}


# The spread of a game's final margin around what the two teams' season margins
# predict, from the same stints the ratings use (every game since the first
# season, garbage time included). It turns a gap in net rating into a win chance.
game_margin_sd <- function(team_rows) {
  g <- team_rows |> group_by(game_id, season) |>
    summarise(home = first(off_team), away = first(def_team)
      , h_pts = sum(pts[off_team == first(off_team)]), a_pts = sum(pts[off_team != first(off_team)]), .groups = "drop") |>
    mutate(margin = h_pts - a_pts)
  long <- bind_rows(g |> transmute(season, team = home, m = margin), g |> transmute(season, team = away, m = -margin))
  tm <- long |> group_by(season, team) |> summarise(mm = mean(m), .groups = "drop")
  g <- g |> left_join(tm |> rename(home = team, mh = mm), by = c("season", "home")) |>
    left_join(tm |> rename(away = team, ma = mm), by = c("season", "away"))
  sd(g$margin - (g$mh - g$ma))
}

# =============================================================================
# Raw data -- fetch if missing
#
# Everything below this point is a local computation on files already on
# disk. Getting those files onto disk in the first place is not: it's
# thousands of individual requests to cdn.nba.com for per-game box scores and
# play-by-play, one game at a time, with deliberate rate-limiting and 403
# backoff, using a spoofed Chrome TLS fingerprint (curl_cffi) because plain
# header spoofing isn't enough to get past stats.nba.com's bot detection. On
# a clean run that can take hours, it depends on stats.nba.com/cdn.nba.com
# staying reachable and not changing what defeats their bot detection, and
# there's no equivalent of the TLS-fingerprint trick in base R -- so rather
# than re-implement that scrape in R (unverified, and liable to just get
# blocked), this step shells out to the project's own fetch script, which is
# tested and already works. It only runs if a required file is actually
# missing.
# =============================================================================

required_files <- c(
  "tank_games.csv", "tank_player_index.csv", "tank_player_index_2026.csv"
  , "tank_bio.csv", "tank_team_advanced.csv"
  , sprintf("tank_pbp_%s.csv.gz", SEASONS)
  , sprintf("tank_box_%s.csv.gz", SEASONS)
)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files)) {
  cat("\n>>> Missing", length(missing_files), "raw data file(s):\n")
  cat(paste0("    ", missing_files, collapse = "\n"), "\n")
  cat(">>> Running fetch_tank_data.py to pull them from stats.nba.com / cdn.nba.com.\n")
  cat(">>> This is a live scrape of thousands of games and can take hours; it resumes\n")
  cat(">>> from whatever's already cached in raw/, so a second run only fetches what a\n")
  cat(">>> first run didn't finish.\n\n")
  py <- Sys.which("python3")
  if (py == "") stop("python3 not found on PATH -- install it (with pandas and curl_cffi) or fetch the raw data manually with fetch_tank_data.py first.")
  status <- system2(py, "fetch_tank_data.py")
  if (status != 0) stop("fetch_tank_data.py exited with status ", status, " -- see its output above. Common causes: cdn.nba.com throttling (rerun -- it resumes) or stats.nba.com changing its bot detection (the script may need updating).")
  still_missing <- required_files[!file.exists(required_files)]
  if (length(still_missing)) stop("fetch_tank_data.py finished but these are still missing: ", paste(still_missing, collapse = ", "))
  cat(">>> Fetch complete.\n")
} else {
  cat("\n>>> Raw data already present -- skipping the fetch.\n")
}


# =============================================================================
# Driver -- the same sequence _targets.R runs, flattened into plain
# assignments in dependency order (targets topologically sorts the DAG
# below; this is that sort, done by hand). set_stage("<name>") before each
# stage matches that stage's exact target name in _targets.R, so its random
# draws -- and only its random draws -- match what the tracked pipeline used.
# =============================================================================

cat("\n>>> Reading raw inputs...\n")

games_file       <- "tank_games.csv"
box_files        <- sprintf("tank_box_%s.csv.gz", SEASONS)
pbp_files        <- sprintf("tank_pbp_%s.csv.gz", SEASONS)
bio_file         <- "tank_bio.csv"
player_index_file <- "tank_player_index.csv"
roster_file      <- "tank_player_index_2026.csv"
team_adv_file    <- "tank_team_advanced.csv"
roster_notes_file <- "tank_roster_notes_2026.csv"
schedule_file    <- "tank_schedule_2026.csv"
bbref_file       <- "tank_roster_bbref_2026.csv"
espn_depth_file  <- "tank_espn_depth_2026.csv"
realgm_depth_file <- "tank_realgm_depth_2026.csv"
dev_stan_file    <- "tank_development.stan"
rookie_stan_file <- "tank_rookie.stan"

set_stage("official_roster"); official_roster <- read_bbref_roster(bbref_file)
set_stage("player_index");    player_index    <- read_player_index(player_index_file)
set_stage("pg");              pg              <- read_player_games(box_files, games_file)

cat(">>> Building and validating stints...\n")
set_stage("stints");       stints       <- build_stints(pbp_files, pg)
set_stage("stint_checks"); stint_checks <- validate_stints(stints, pg)
set_stage("team_rows");    team_rows    <- stint_team_rows(stints |> filter(game_id %in% stint_checks$keep), pg)
set_stage("poss");         poss         <- check_possessions(team_rows, team_adv_file, pg, games_file)
set_stage("lottery");      lottery      <- build_lottery(games_file, SEASONS_ALL)
set_stage("dir_rows");     dir_rows     <- label_rows(team_rows, lottery)
set_stage("margin_sd");    margin_sd    <- game_margin_sd(team_rows)

cat(">>> Box-score priors and roles...\n")
set_stage("box_prior"); box_prior <- build_box_prior(pg, bio_file, player_index)
set_stage("roles");     roles     <- assign_roles(box_prior, player_index)

cat(">>> Fitting the ratings model (RAPM, empirical-Bayes hyperparameters)...\n")
set_stage("rapm");         rapm         <- fit_rapm(dir_rows, box_prior, SEASONS)
set_stage("rapm_summary"); rapm_summary <- rapm_report(rapm)

cat(">>> Decomposing 2025-26 deployment (needed for the 2026-27 projection's zero point)...\n")
set_stage("decomp"); decomp <- decompose(rapm, pg, dir_rows, lottery, player_index, roles, poss$scale)

cat(">>> Fitting the development curve (Stan)...\n")
set_stage("pgd_all");   pgd_all   <- player_game_deployment(pg, rapm, lottery, player_index, roles, 250)
set_stage("dev_pairs"); dev_pairs <- build_dev_pairs(rapm, pgd_all)
set_stage("development"); development <- fit_development(rapm, dev_pairs, dev_stan_file)

cat(">>> Fitting the rookie reference class (Stan)...\n")
set_stage("rookie"); rookie <- fit_rookie(rapm, pg, player_index, roles, rookie_stan_file)

cat(">>> Building the 2026-27 projection and the two rotations...\n")
set_stage("projection")
projection <- project_2026(rapm, decomp, development, rookie, player_index, roles, box_prior
  , roster_file, roster_notes_file, margin_sd, schedule_file, official_roster)

cat(">>> Scoring every five-man lineup...\n")
set_stage("lineup_opp");    lineup_opp   <- lineup_opponents(dir_rows, pg, rapm_summary, box_prior, development)
set_stage("espn_depth");    espn_depth   <- read_depth_chart(espn_depth_file, official_roster$name)
set_stage("realgm_depth");  realgm_depth <- read_depth_chart(realgm_depth_file, official_roster$name)
set_stage("lineups"); lineups <- best_lineups(projection, roles, dir_rows, lineup_opp, espn_depth)
set_stage("argmax");  argmax  <- argmax_draws(projection, lineups)

set_stage("wins_per_point"); wins_per_point <- fit_wins_per_point(team_adv_file)

cat(">>> Backtesting on 2024-25 and 2025-26 (two more full refits)...\n")
set_stage("backtest_2025")
backtest_2025 <- run_backtest("2025-26", team_rows, pg, games_file, bio_file, player_index, rapm$hyper
  , dev_stan_file, rookie_stan_file, poss$scale)
set_stage("backtest_2024")
backtest_2024 <- run_backtest("2024-25", team_rows, pg, games_file, bio_file, player_index, rapm$hyper
  , dev_stan_file, rookie_stan_file, poss$scale)

cat(">>> Pipeline reproduced. Pulling the post's material...\n")

ros    <- projection$roster
win_rot <- projection$win_rotation
aj_i   <- match("AJ Dybantsa", ros$name)
n_draw <- ncol(projection$PO)

cat("\n================ 1. NO. 1 PICKS' ROOKIE MINUTES ================\n")

# rookie minutes chart data: every first-round pick, 2019-2025 drafts, 10+ games played
slot_group <- function(n) case_when(n == 1 ~ "No. 1", n <= 3 ~ "2-3", n <= 10 ~ "4-10", n <= 20 ~ "11-20", TRUE ~ "21-30")
rk_all <- pg |>
  inner_join(player_index |> select(person_id, draft_year, draft_number), by = "person_id") |>
  filter(!is.na(draft_number), draft_number <= 30, draft_year >= 2019, season_start == draft_year) |>
  group_by(person_id, draft_year, draft_number) |>
  summarise(name = last(name), gp = sum(played == 1), mpg = mean(min[played == 1]), .groups = "drop")
rk_min <- rk_all |> filter(gp >= 10) |> mutate(group = factor(slot_group(draft_number), levels = c("No. 1", "2-3", "4-10", "11-20", "21-30")))
grp_med <- rk_min |> group_by(group) |> summarise(med = median(mpg), n = n(), .groups = "drop")
med_of <- function(g) grp_med$med[grp_med$group == g]
no1 <- rk_min |> filter(draft_number == 1) |> arrange(draft_year)

cat("No. 1 picks, median rookie mpg:", f1(med_of("No. 1")), "(n =", nrow(no1), ")\n")
print(no1 |> select(name, draft_year, mpg))
cat("Picks 2-3 median:", f1(med_of("2-3")), " | Picks 4-10:", f1(med_of("4-10")), "\n")

row_levels <- c("No. 1 picks", "Picks 2-3", "Picks 4-10", "Picks 11-20", "Picks 21-30")
rk_rows <- rk_min |> mutate(row = factor(if_else(group == "No. 1", "No. 1 picks", paste("Picks", group)), levels = row_levels))
med_rows <- grp_med |> mutate(row = factor(if_else(group == "No. 1", "No. 1 picks", paste("Picks", group)), levels = row_levels)
  , ypos = match(as.character(row), rev(row_levels)))
no1_rows <- rk_rows |> filter(draft_number == 1)

p_rk <- ggplot(mapping = aes(x = mpg, y = row)) +
  geom_vline(xintercept = med_of("No. 1"), linetype = "dashed", color = wiz_red, alpha = 0.6) +
  geom_point(data = rk_rows |> filter(draft_number != 1), color = "grey55", size = 2.2, alpha = 0.6
    , position = position_jitter(height = 0.18, width = 0, seed = 202)) +
  geom_segment(data = med_rows, aes(x = med, xend = med, y = ypos - 0.32, yend = ypos + 0.32), color = "grey15", linewidth = 1.3) +
  geom_text(data = med_rows, aes(x = med, y = ypos - 0.42, label = paste0("median ", f1(med))), size = 3.2, color = "grey15", vjust = 1) +
  geom_point(data = no1_rows, color = wiz_red, size = 3.2) +
  geom_text_repel(data = no1_rows, aes(label = paste(word(name, -1), f1(mpg))), size = 3.2, color = wiz_red, seed = 202
    , nudge_y = 0.38, direction = "x", min.segment.length = 0, segment.color = "grey70") +
  usaid_plot() +
  scale_y_discrete(limits = rev(row_levels)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  labs(
    title = paste0("**<span style='color:#D9565CFF;'>No. 1 picks</span> have played a median ", f1(med_of("No. 1")), " minutes a game as rookies**")
    , subtitle = paste0("Minutes per game played in each first-round pick's rookie season, drafts ", min(rk_all$draft_year), " to ", max(rk_all$draft_year), ", for the ", nrow(rk_min), " who played 10+ games in the data (grey; No. 1 picks in red). Black ticks: median for each draft range.")
    , x = "Rookie minutes per game played", y = NULL, caption = caption_text
  ) +
  titles_wrap
save_plot("post_rookie_minutes.png", p_rk, height = 7)

cat("\n================ 2. THE WIN-FOCUSED ROTATION'S PLANNED MINUTES ================\n")
plan_tab <- projection$rotations |> filter(scenario == win_rot) |> arrange(desc(mpg))
print(plan_tab |> select(name, mpg))
cat("Only Davis plans at or above Dybantsa's 30: ", plan_tab$mpg[plan_tab$name == "Anthony Davis"] >= 30, "\n")

cat("\n================ 3. THE BEST FIVE-MAN GROUP ================\n")
best_t <- lineups$all |> filter(checks) |> slice_max(net, n = 1)
cat("Best five on average:", best_t$players, "\n")
cat("  net:", f1(best_t$net), " | chance above average:", pct(best_t$p_positive), "\n")

cat("\nWho's in the best five, across all 1,000 simulated draws:\n")
print(argmax$per_player, n = 20)
cat("Best five leaves out Dybantsa on", pct(argmax$p_excl_aj), "of draws.\n")
cat("Best five is the single most common winner on only", pct(argmax$best_share), "of draws.\n")

cat("\nDybantsa's own projection:\n")
cat("  rating:", f1(ros$v[aj_i]), " | draw-to-draw spread (SD):", f1(sd(projection$PO[aj_i, ] + projection$PD[aj_i, ])), "\n")

cat("\n================ 4. THE BEST FIVE WITH DYBANTSA IN IT ================\n")
aj_t <- lineups$candidates |> filter(has_aj) |> slice_max(net, n = 1)
cat("Best group with Dybantsa:", aj_t$players, "\n")
cat("  net:", f1(aj_t$net), " | chance above average:", pct(aj_t$p_positive), "\n")

cat("\n================ 5. ANTHONY DAVIS: RATING, AVAILABILITY, HEALTH ================\n")
cat("Davis rating:", f1(ros$v[ros$name == "Anthony Davis"]), "(best on the roster:", ros$v[ros$name == "Anthony Davis"] == max(ros$v), ")\n")
cat("Davis modeled availability:", pct(ros$avail[ros$name == "Anthony Davis"]), "\n")
scen <- projection$scenarios |> group_by(scenario) |> summarise(net_mean = mean(net), .groups = "drop")
sv <- function(lab) scen$net_mean[scen$scenario == lab]
healthy <- paste0(win_rot, ", Davis and Young always available")
cat("Full health for Davis and Young is worth:", f1(sv(healthy) - sv(win_rot)), "points\n")

cat("\n================ 6. DYBANTSA'S MINUTES: WHOSE THEY COME FROM ================\n")
aj_eff <- projection$aj_effect
aj_of <- function(rot, col, repl) aj_eff[[col]][aj_eff$rotation == rot & aj_eff$replacement == repl]
cat("If Washington's other wings absorb his 30 minutes:\n")
cat("  cost:", f1(-aj_of(win_rot, "mean", "other wings")), " | chance it still helps:", pct(aj_of(win_rot, "p_positive", "other wings")), "\n")
cat("If the whole rotation absorbs them (what the benchmark effectively does):\n")
cat("  cost:", f1(-aj_of(win_rot, "mean", "whole rotation")), "\n")
win_planned <- projection$rotations |> filter(scenario == win_rot)
wing_pool <- ros$name[ros$role == "Wing" & ros$name != "AJ Dybantsa" & ros$name %in% win_planned$name[win_planned$mpg > 0]]
cat("The 'other wings' whose minutes actually move (planned > 0 in the win-focused rotation): ", paste(wing_pool, collapse = ", "), "\n")

# the beeswarm chart: every draw's cost under each counterfactual
aj_cost_d <- projection$scenarios |> filter(scenario %in% c(win_rot, paste0(win_rot, ", without Dybantsa"), paste0(win_rot, ", without Dybantsa, whole rotation absorbs"))) |>
  select(draw, scenario, net) |>
  pivot_wider(names_from = scenario, values_from = net) |>
  transmute(draw
    , `Other wings absorb it` = .data[[win_rot]] - .data[[paste0(win_rot, ", without Dybantsa")]]
    , `The whole rotation absorbs it` = .data[[win_rot]] - .data[[paste0(win_rot, ", without Dybantsa, whole rotation absorbs")]]) |>
  pivot_longer(-draw, names_to = "counterfactual", values_to = "cost") |>
  mutate(counterfactual = fct_relevel(counterfactual, "The whole rotation absorbs it", "Other wings absorb it"), helps = cost > 0)
aj_cost_s <- aj_cost_d |> group_by(counterfactual) |>
  summarise(mean = mean(cost), q10 = quantile(cost, 0.1), q90 = quantile(cost, 0.9), p_positive = mean(cost > 0), .groups = "drop")

p_ajcost <- ggplot(aj_cost_d, aes(x = counterfactual, y = cost)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_quasirandom(aes(color = helps), width = 0.32, size = 1.1, alpha = 0.5) +
  geom_crossbar(data = aj_cost_s, aes(x = counterfactual, y = mean, ymin = mean, ymax = mean), width = 0.5, color = "black", linewidth = 0.9, inherit.aes = FALSE) +
  geom_text(data = aj_cost_s, aes(x = counterfactual, y = 5.2, label = paste0(s1(mean), "  (", pct(p_positive), " chance it helps)"))
    , inherit.aes = FALSE, size = 4.4, fontface = "bold", hjust = 0) +
  usaid_plot() +
  scale_color_manual(values = c(`TRUE` = wiz_blue, `FALSE` = wiz_red), guide = "none") +
  scale_y_continuous(limits = c(NA, 12.5)) +
  coord_flip() +
  labs(
    title = "**Dybantsa's minutes cost more the more of the roster gives them up**"
    , subtitle = paste0("Change in the win-focused rotation's projected talent from playing him ", projection$rotations$mpg[projection$rotations$scenario == win_rot & projection$rotations$name == "AJ Dybantsa"], " minutes a night, points per 100. Each dot is one of ", comma(n_draw), " simulated draws: <span style='color:", wiz_blue, ";'>blue</span> draws favor playing him, <span style='color:", wiz_red, ";'>red</span> don't. Black bar: the mean.")
    , x = NULL, y = "Change in roster talent, points per 100", caption = caption_text
  ) +
  titles_wrap
save_plot("post_aj_cost.png", p_ajcost, height = 5.5)

cat("\n================ 7. CAN THE MODEL RECOVER A SEASON ALREADY PLAYED? ================\n")
cat("2024-25 -- projected:", f1(mean(backtest_2024$teams$pred_net)), "| actual (WAS):"
  , f1(backtest_2024$teams$actual_net[backtest_2024$teams$team_tricode == "WAS"])
  , "| projected (WAS):", f1(backtest_2024$teams$pred_net[backtest_2024$teams$team_tricode == "WAS"]), "\n")
cat("2025-26 -- projected (WAS):", f1(backtest_2025$teams$pred_net[backtest_2025$teams$team_tricode == "WAS"])
  , "| actual (WAS):", f1(backtest_2025$teams$actual_net[backtest_2025$teams$team_tricode == "WAS"]), "\n")
cat("(2025-26 is 'last season' as of this post -- projected -11.8 vs actual -11.5.)\n")

cat("\n================ 8. DEPTH CHART CHECK ================\n")
espn_extra <- setdiff(espn_depth$depth$name, official_roster$name)
realgm_extra <- setdiff(realgm_depth$depth$name, official_roster$name)
espn_missing <- setdiff(official_roster$name[official_roster$two_way == 1], espn_depth$depth$name)
cat("On ESPN's chart but not on the official roster:", paste(espn_extra, collapse = ", "), "-- left the team.\n")
cat("On RealGM's chart but not on the official roster:", paste(realgm_extra, collapse = ", "), "-- on an Exhibit 10 deal, may not make the roster.\n")
cat("Two-way players ESPN's chart leaves out entirely:", paste(espn_missing, collapse = ", "), "\n")
cat("Felix Okpara draft slot:", player_index$draft_number[player_index$name == "Felix Okpara"][1], "(second round)\n")

cat("\nDone. Figures saved: post_rookie_minutes.png, post_aj_cost.png\n")
