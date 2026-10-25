###############################################################################
# Where do the Wizards rank? 2026-27 team metrics + an alley-oop defense model
# Session info:
#R version 4.5.3 (2026-03-11) -- "Reassured Reassurer"
# Copyright (C) 2026 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20

###############################################################################

# ---- 0. Setup ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(hoopR)      # ESPN data loaders
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(cmdstanr) 
  library(posterior)
})

focus   <- "WSH"
current <- hoopR::most_recent_nba_season()   # 2027 = the 2026-27 season
seasons <- (current - 2):current             # two prior seasons inform the model
out_dir <- "wizards_rankings_output"
dir.create(out_dir, showWarnings = FALSE)

# House style
font <- "Gill Sans"
if (requireNamespace("systemfonts", quietly = TRUE) &&
    !font %in% systemfonts::system_fonts()$family) {
  message("Gill Sans not found; falling back to 'sans'.")
  font <- "sans"
}
pal <- c(red = "#D9535B", teal = "#3A9AB2", teal_dark = "#2A7F96",
         navy = "#1B2A6B", grey = "#4D4D4D")
house_caption <- "Data: ESPN\nwizardspoints.substack.com"

theme_wiz <- function(base_size = 18) {
  theme_minimal(base_size = base_size, base_family = font) +
    theme(
      plot.title          = element_text(face = "bold", size = rel(1.6), colour = "black",
                                         margin = margin(b = 10)),
      plot.subtitle       = element_text(colour = pal[["grey"]], size = rel(1.05),
                                         lineheight = 1.1, margin = margin(b = 18)),
      plot.caption        = element_text(hjust = 1, colour = "black", size = rel(0.9),
                                         lineheight = 1.1, margin = margin(t = 14)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid          = element_blank(),
      axis.text           = element_text(colour = pal[["grey"]]),
      axis.title.x        = element_text(size = rel(1.1), margin = margin(t = 8)),
      plot.background     = element_rect(fill = "white", colour = NA)
    )
}

ordinal <- function(n) {
  suf <- ifelse(n %% 100 %in% 11:13, "th",
                c("th", "st", "nd", "rd", rep("th", 6))[(n %% 10) + 1])
  paste0(n, suf)
}

# Rates print as .461; everything else to one decimal
fmt_val <- function(x, metric) {
  ifelse(str_detect(metric, "rate|%|share") & abs(x) < 1,
         sub("^0", "", sprintf("%.3f", x)),
         sprintf("%.1f", x))
}

# ---- 1. Load ESPN data -----------------------------------------------------
pbp_raw  <- load_nba_pbp(seasons = seasons)
tbox     <- load_nba_team_box(seasons = current)  |> filter(season_type == 2)
pbox     <- load_nba_player_box(seasons = current) |> filter(season_type == 2)

tbox <- tbox |> mutate(across(c(game_id, team_id, opponent_team_id), as.character))
pbox <- pbox |> mutate(across(c(game_id, team_id, athlete_id), as.character))
teams30 <- sort(unique(tbox$team_abbreviation))   # drops All-Star/exhibition teams
stopifnot(length(teams30) == 30)

# ---- 2. Flag play-by-play events -------------------------------------------
flag_events <- function(pbp) {
  pbp |>
    filter(season_type == 2) |>
    mutate(across(c(game_id, team_id, home_team_id, away_team_id, athlete_id_1),
                  as.character)) |>
    arrange(game_id, game_play_number) |>
    mutate(
      type_text = coalesce(type_text, ""),
      text      = coalesce(text, ""),
      made      = coalesce(as.logical(scoring_play), FALSE),
      pts       = if_else(made, as.numeric(score_value), 0),
      is_ft     = str_starts(type_text, "Free Throw"),
      # Heaves are in the play-by-play but not in box-score FGA
      is_fga    = coalesce(as.logical(shooting_play), FALSE) & !is_ft &
                  !str_detect(type_text, "Heave"),
      # Use the structured shot value, not the text: many threes read "jump shot"
      is_three  = is_fga & as.numeric(points_attempted) == 3,
      dist      = as.numeric(str_match(text, "(\\d+)-foot")[, 2]),
      is_rim    = is_fga & !is_three &
                  (str_detect(type_text, "Dunk|Layup|Tip|Putback|Finger Roll") |
                     coalesce(dist <= 4, FALSE)),
      is_mid    = is_fga & !is_three & !is_rim & coalesce(dist >= 10, FALSE),
      is_pull   = is_fga & str_detect(type_text, "Pullup|Step Back"),
      is_oop    = str_detect(type_text, "Alley Oop"),
      is_dunk   = str_detect(type_text, "Dunk"),
      # Team rebounds (no player attached) are logged as offensive rebounds after
      # missed first free throws etc. Only player rebounds match the box score.
      oreb      = type_text == "Offensive Rebound" & !is.na(athlete_id_1),
      dreb      = type_text == "Defensive Rebound",
      tov       = str_detect(type_text, regex("Turnover|Traveling", ignore_case = TRUE)),
      tech      = str_detect(type_text, "Technical") & !is_ft,
      def_team_id = if_else(team_id == home_team_id, away_team_id, home_team_id)
    )
}

pbp_all <- flag_events(pbp_raw)
pbp_cur <- pbp_all |> filter(season == current)

# ---- 3. Audit: play-by-play vs box score -----------------------------------
audit <- pbp_cur |>
  filter(!is.na(team_id)) |>
  group_by(game_id, team_id) |>
  summarise(pts  = sum(pts), fga = sum(is_fga), fta = sum(is_ft),
            fg3a = sum(is_three), oreb = sum(oreb),
            ast  = sum(is_fga & made & str_detect(text, "assists")),
            .groups = "drop") |>
  inner_join(tbox |> transmute(game_id, team_id,
                               b_pts  = as.numeric(team_score),
                               b_fga  = as.numeric(field_goals_attempted),
                               b_fta  = as.numeric(free_throws_attempted),
                               b_fg3a = as.numeric(three_point_field_goals_attempted),
                               b_oreb = as.numeric(offensive_rebounds),
                               b_ast  = as.numeric(assists)),
             by = c("game_id", "team_id"))

audit_cols <- c("pts", "fga", "fta", "fg3a", "oreb", "ast")
cat("Team-games audited:", nrow(audit), "\n")
print(tibble(stat = audit_cols,
             share_exact = map_dbl(audit_cols, \(s) mean(audit[[s]] == audit[[paste0("b_", s)]]))))
# Expected (through Oct 24): 72 team-games; pts, fta, oreb, ast all 1.000;
# fga ~0.98 and fg3a ~0.96 (one-off differences on a couple of shots).

# ---- 4. Team metrics from box scores ---------------------------------------
stat_cols <- c("team_score", "assists", "blocks", "defensive_rebounds",
               "offensive_rebounds", "total_rebounds", "fast_break_points",
               "field_goals_made", "field_goals_attempted", "free_throws_made",
               "free_throws_attempted", "fouls", "points_in_paint", "steals",
               "three_point_field_goals_made", "three_point_field_goals_attempted",
               "total_turnovers", "turnover_points", "largest_lead", "lead_percentage")

tbox <- tbox |>
  mutate(across(all_of(stat_cols), as.numeric), team_winner = as.logical(team_winner))

opp <- tbox |>
  select(game_id, opponent_team_id = team_id, all_of(stat_cols)) |>
  rename_with(~ paste0("o_", .x), all_of(stat_cols))

tot <- tbox |>
  left_join(opp, by = c("game_id", "opponent_team_id")) |>
  group_by(team = team_abbreviation) |>
  summarise(gp = n(), wins = sum(team_winner),
            across(c(all_of(stat_cols), starts_with("o_")), sum), .groups = "drop") |>
  mutate(
    poss   = field_goals_attempted - offensive_rebounds + total_turnovers +
             0.44 * free_throws_attempted,
    o_poss = o_field_goals_attempted - o_offensive_rebounds + o_total_turnovers +
             0.44 * o_free_throws_attempted,
    P      = (poss + o_poss) / 2
  )

box_metrics <- tot |>
  transmute(
    team,
    `Win %`                          = wins / gp,
    `Points per game`                = team_score / gp,
    `Opp points per game`            = o_team_score / gp,
    `Point differential per game`    = (team_score - o_team_score) / gp,
    `Offensive rating`               = 100 * team_score / P,
    `Defensive rating`               = 100 * o_team_score / P,
    `Net rating`                     = 100 * (team_score - o_team_score) / P,
    `Pace`                           = P / gp,
    `FG%`                            = field_goals_made / field_goals_attempted,
    `Opp FG%`                        = o_field_goals_made / o_field_goals_attempted,
    `3P%`                            = three_point_field_goals_made / three_point_field_goals_attempted,
    `Opp 3P%`                        = o_three_point_field_goals_made / o_three_point_field_goals_attempted,
    `FT%`                            = free_throws_made / free_throws_attempted,
    `eFG%`                           = (field_goals_made + 0.5 * three_point_field_goals_made) / field_goals_attempted,
    `Opp eFG%`                       = (o_field_goals_made + 0.5 * o_three_point_field_goals_made) / o_field_goals_attempted,
    `TS%`                            = team_score / (2 * (field_goals_attempted + 0.44 * free_throws_attempted)),
    `3PM per game`                   = three_point_field_goals_made / gp,
    `3PA per game`                   = three_point_field_goals_attempted / gp,
    `3PA share`                      = three_point_field_goals_attempted / field_goals_attempted,
    `Free-throw rate`                = free_throws_attempted / field_goals_attempted,
    `Free-throw attempts per game`   = free_throws_attempted / gp,
    `Free throws made per game`      = free_throws_made / gp,
    `FGM per game`                   = field_goals_made / gp,
    `FGA per game`                   = field_goals_attempted / gp,
    `Opp FGA per game`               = o_field_goals_attempted / gp,
    `Assists per game`               = assists / gp,
    `Assist share of FGM`            = assists / field_goals_made,
    `Rebounds per game`              = total_rebounds / gp,
    `Offensive rebounds per game`    = offensive_rebounds / gp,
    `Opp offensive rebounds per game`= o_offensive_rebounds / gp,
    `Defensive rebounds per game`    = defensive_rebounds / gp,
    `OREB%`                          = offensive_rebounds / (offensive_rebounds + o_defensive_rebounds),
    `DREB%`                          = defensive_rebounds / (defensive_rebounds + o_offensive_rebounds),
    `Steals per game`                = steals / gp,
    `Blocks per game`                = blocks / gp,
    `Turnovers per game`             = total_turnovers / gp,
    `TOV%`                           = total_turnovers / poss,
    `Opp turnovers per game`         = o_total_turnovers / gp,
    `Opp TOV%`                       = o_total_turnovers / o_poss,
    `Points off turnovers per game`  = turnover_points / gp,
    `Opp points off turnovers per game` = o_turnover_points / gp,
    `Fast-break points per game`     = fast_break_points / gp,
    `Opp fast-break points per game` = o_fast_break_points / gp,
    `Paint points per game`          = points_in_paint / gp,
    `Opp paint points per game`      = o_points_in_paint / gp,
    `Fouls per game`                 = fouls / gp,
    `Fouls drawn per game`           = o_fouls / gp,
    `Average largest lead`           = largest_lead / gp,
    `Average share of game leading`  = lead_percentage / gp
  )

# ---- 5. Team metrics from play-by-play -------------------------------------
abbr <- tbox |> distinct(team_id, team = team_abbreviation)
gp   <- tot |> select(team, gp)

off_ct <- pbp_cur |>
  filter(!is.na(team_id)) |>
  group_by(team_id) |>
  summarise(oop = sum(is_oop & made), dunk = sum(is_dunk & made),
            fga = sum(is_fga), fgm = sum(is_fga & made),
            rim_a = sum(is_rim), rim_m = sum(is_rim & made),
            mid_a = sum(is_mid), mid_m = sum(is_mid & made),
            pull_a = sum(is_pull), pull_m = sum(is_pull & made),
            astd = sum(is_fga & made & str_detect(text, "assists")),
            tech = sum(tech), .groups = "drop")

# Defensive side: credit each event to the team NOT on the event
def_ct <- pbp_cur |>
  filter(!is.na(def_team_id)) |>
  group_by(team_id = def_team_id) |>
  summarise(o_oop = sum(is_oop & made), o_dunk = sum(is_dunk & made),
            o_fga = sum(is_fga), o_rim_a = sum(is_rim), o_rim_m = sum(is_rim & made),
            # A foul event carries the fouling team, so its opponent drew it
            shfoul_drawn = sum(type_text == "Shooting Foul"), .groups = "drop")

# Second-chance points: points a team scores after its own offensive rebound,
# until the opponent rebounds, the team turns it over, or a field goal is made.
second_chance <- function(g) {
  flag <- NA_character_
  tm <- character(); pt <- numeric()
  for (i in seq_len(nrow(g))) {
    t <- g$team_id[i]
    if (g$oreb[i]) flag <- t else if (g$dreb[i] || g$tov[i]) flag <- NA_character_
    if (g$made[i] && !is.na(flag) && !is.na(t)) {
      if (flag == t) { tm <- c(tm, t); pt <- c(pt, g$pts[i]) } else flag <- NA_character_
    }
    if (g$made[i] && g$is_fga[i] && !is.na(t) && identical(flag, t)) flag <- NA_character_
  }
  tibble(team_id = tm, pts = pt)
}
sc <- pbp_cur |>
  group_split(game_id) |>
  map(\(g) second_chance(g)) |>
  list_rbind() |>
  group_by(team_id) |>
  summarise(sc_pts = sum(pts), .groups = "drop")

# And-ones: a made field goal followed within five events by a 1-of-1 free
# throw from the same player
and1 <- pbp_cur |>
  group_by(game_id) |>
  mutate(and1 = is_fga & made &
           reduce(1:5, \(acc, k) acc | coalesce(lead(type_text, k) == "Free Throw - 1 of 1" &
                                                  lead(athlete_id_1, k) == athlete_id_1, FALSE),
                  .init = FALSE)) |>
  ungroup() |>
  filter(and1) |>
  count(team_id, name = "and1")

# Quarter point differentials
q_for <- pbp_cur |> filter(made, period_number <= 4) |>
  count(team_id, period_number, wt = pts, name = "pf")
q_agst <- pbp_cur |> filter(made, period_number <= 4, !is.na(def_team_id)) |>
  count(team_id = def_team_id, period_number, wt = pts, name = "pa")
q_diff <- full_join(q_for, q_agst, by = c("team_id", "period_number")) |>
  mutate(across(c(pf, pa), ~ coalesce(.x, 0)), diff = pf - pa) |>
  select(team_id, period_number, diff) |>
  pivot_wider(names_from = period_number, values_from = diff, names_prefix = "q")

# Clutch: last five minutes of the 4th/OT with the margin within five
pbp_cur <- pbp_cur |>
  group_by(game_id) |>
  mutate(pre_margin = lag(as.numeric(home_score), default = 0) -
                      lag(as.numeric(away_score), default = 0)) |>
  ungroup()
clutch_ev <- pbp_cur |>
  filter(period_number >= 4, end_game_seconds_remaining <= 300, abs(pre_margin) <= 5, made)
clutch <- full_join(clutch_ev |> count(team_id, wt = pts, name = "cf"),
                    clutch_ev |> count(team_id = def_team_id, wt = pts, name = "ca"),
                    by = "team_id") |>
  mutate(across(c(cf, ca), ~ coalesce(.x, 0)), clutch = cf - ca) |>
  select(team_id, clutch)

# Bench scoring
bench <- pbox |>
  mutate(points = as.numeric(points), starter = as.logical(starter)) |>
  group_by(team_id) |>
  summarise(bench_pts = sum(points[!starter], na.rm = TRUE),
            all_pts   = sum(points, na.rm = TRUE),
            games_20  = sum(points >= 20, na.rm = TRUE), .groups = "drop")

pbp_metrics <- abbr |>
  left_join(gp, by = "team") |>
  left_join(off_ct, by = "team_id") |> left_join(def_ct, by = "team_id") |>
  left_join(sc, by = "team_id") |> left_join(and1, by = "team_id") |>
  left_join(q_diff, by = "team_id") |> left_join(clutch, by = "team_id") |>
  left_join(bench, by = "team_id") |>
  mutate(across(where(is.numeric), ~ coalesce(.x, 0))) |>
  transmute(
    team,
    `Alley-oops made per game`        = oop / gp,
    `Alley-oops allowed per game`     = o_oop / gp,
    `Dunks per game`                  = dunk / gp,
    `Dunks allowed per game`          = o_dunk / gp,
    `Rim attempt share`               = rim_a / fga,
    `Rim FG%`                         = rim_m / rim_a,
    `Opp rim attempt share`           = o_rim_a / o_fga,
    `Opp rim FG%`                     = o_rim_m / o_rim_a,
    `Midrange attempt share`          = mid_a / fga,
    `Midrange FG%`                    = mid_m / mid_a,
    `Pull-up/step-back share`         = pull_a / fga,
    `Pull-up/step-back FG%`           = pull_m / pull_a,
    `Assisted share of FGM`           = astd / fgm,
    `Second-chance points per game`   = sc_pts / gp,
    `And-ones per game`               = and1 / gp,
    `Shooting fouls drawn per game`   = shfoul_drawn / gp,
    `Technical fouls per game`        = tech / gp,
    `Q1 point differential per game`  = q1 / gp,
    `Q2 point differential per game`  = q2 / gp,
    `Q3 point differential per game`  = q3 / gp,
    `Q4 point differential per game`  = q4 / gp,
    `Clutch point differential`       = clutch,
    `Bench points per game`           = bench_pts / gp,
    `Bench share of points`           = bench_pts / all_pts,
    `20-point games by players`       = games_20
  )

# ---- 6. Rank every metric --------------------------------------------------
lower_better <- c(
  "Opp points per game", "Defensive rating", "Opp FG%", "Opp 3P%", "Opp eFG%",
  "Opp FGA per game", "Opp offensive rebounds per game", "Turnovers per game", "TOV%",
  "Opp points off turnovers per game", "Opp fast-break points per game",
  "Opp paint points per game", "Fouls per game", "Alley-oops allowed per game",
  "Dunks allowed per game", "Opp rim attempt share", "Opp rim FG%",
  "Midrange attempt share", "Technical fouls per game"
)

all_metrics <- bind_rows(
  box_metrics |> pivot_longer(-team, names_to = "metric"),
  pbp_metrics |> pivot_longer(-team, names_to = "metric")
) |>
  mutate(higher_better = !metric %in% lower_better) |>
  group_by(metric) |>
  mutate(rank = rank(if_else(higher_better, -value, value),
                     ties.method = "min", na.last = "keep")) |>
  add_count(metric, rank, name = "n_tied") |>
  ungroup()

leaders <- all_metrics |>
  arrange(metric, rank) |>
  group_by(metric) |>
  summarise(top4 = paste(head(team, 4), fmt_val(head(value, 4), metric[1]), collapse = ", "),
            .groups = "drop")

focus_top <- all_metrics |>
  filter(team == focus, rank <= 3) |>
  left_join(leaders, by = "metric") |>
  arrange(rank, metric)

cat("\nMetrics ranked:", n_distinct(all_metrics$metric), "\n")
print(focus_top |> select(metric, value, rank, n_tied, top4), n = Inf, width = Inf)
# Expected (through Oct 24, 74 metrics):
#   Alley-oops allowed         0.0   T-1st with CLE, LAL, POR
#   Dunks per game             8.3   2nd (LAL 9.0)
#   Fouls drawn per game       28.3  2nd (TOR 30.0)
#   Free throws made per game  28.7  2nd (CHI 30.0)
#   Free-throw attempts        34.7  2nd (CHI 35.7)
#   Free-throw rate            .402  2nd (UTAH .415)
#   Alley-oops made            4.0   3rd (SA 5.0, LAL 4.5)
#   Rim FG%                    .720  3rd (LAL .797, LAC .724)
#   20-point games by players  6     T-3rd with 4 others (dropped from charts)

write.csv(all_metrics, file.path(out_dir, "all_team_metrics.csv"), row.names = FALSE)

# ---- 7. Simple null check: zero alley-oops allowed --------------------------
# Treats every defense as identical and shuffles game counts across team-games.
# Useful only early in the season; Section 8 is the real model.
allowed <- tbox |>
  select(game_id, team_id) |>
  left_join(pbp_cur |> filter(is_oop, made) |> count(game_id, team_id = def_team_id, name = "n"),
            by = c("game_id", "team_id")) |>
  mutate(n = coalesce(n, 0L))

all_zero_teams <- function(n, team) sum(tapply(n, team, \(x) all(x == 0)))
observed_all_zero <- all_zero_teams(allowed$n, allowed$team_id)
set.seed(2027)
perm <- replicate(20000, all_zero_teams(sample(allowed$n), allowed$team_id))
cat(sprintf("\nShare of team-games with zero allowed: %.3f\n", mean(allowed$n == 0)))
cat(sprintf("Teams with zero in every game: observed %d, shuffled mean %.2f, P(>= observed) = %.2f\n",
            observed_all_zero, mean(perm), mean(perm >= observed_all_zero)))
# Expected: 0.333; observed 4 (CLE, LAL, POR, WSH), shuffled mean ~2.4,
# P(>= observed) ~0.16

# ---- 8. Hierarchical negative binomial model --------------------------------
# y = made alley-oops by the offense in one team-game.
# log mu = season intercept + home + offense[team, season] + defense[team, season]
#          + arena (home scorekeepers' tagging tendency, constant across seasons)
# Team effects follow a stationary AR(1) across seasons, so last season's
# defense is this season's prior, with a year-to-year correlation rho estimated
# from the 2024-25 -> 2025-26 transition.
stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> S;
  array[N] int<lower=0> y;
  array[N] int<lower=1, upper=T> off;
  array[N] int<lower=1, upper=T> def;
  array[N] int<lower=1, upper=T> arena;
  array[N] int<lower=1, upper=S> season;
  vector[N] home;
  int<lower=1, upper=T> focus;
  int<lower=0> N_focus;
  array[N_focus] int<lower=1, upper=N> idx_focus;
}
parameters {
  vector[S] alpha;
  real beta_home;
  matrix[T, S] z_off;
  matrix[T, S] z_def;
  vector[T] z_arena;
  real<lower=0> sigma_off;
  real<lower=0> sigma_def;
  real<lower=0> sigma_arena;
  real<lower=0, upper=1> rho_off;
  real<lower=0, upper=1> rho_def;
  real<lower=0> inv_sqrt_phi;
}
transformed parameters {
  real phi = inv_square(inv_sqrt_phi);
  matrix[T, S] o;
  matrix[T, S] d;
  vector[T] a = sigma_arena * z_arena;
  o[, 1] = sigma_off * z_off[, 1];
  d[, 1] = sigma_def * z_def[, 1];
  for (s in 2:S) {
    o[, s] = rho_off * o[, s - 1] + sigma_off * sqrt(1 - square(rho_off)) * z_off[, s];
    d[, s] = rho_def * d[, s - 1] + sigma_def * sqrt(1 - square(rho_def)) * z_def[, s];
  }
}
model {
  vector[N] eta;
  for (n in 1:N)
    eta[n] = alpha[season[n]] + beta_home * home[n] + o[off[n], season[n]]
             + d[def[n], season[n]] + a[arena[n]];
  alpha ~ normal(0, 1);
  beta_home ~ normal(0, 0.5);
  to_vector(z_off) ~ std_normal();
  to_vector(z_def) ~ std_normal();
  z_arena ~ std_normal();
  sigma_off ~ normal(0, 0.5);
  sigma_def ~ normal(0, 0.5);
  sigma_arena ~ normal(0, 0.5);
  rho_off ~ beta(2, 2);
  rho_def ~ beta(2, 2);
  inv_sqrt_phi ~ normal(0, 1);
  y ~ neg_binomial_2_log(eta, phi);
}
generated quantities {
  vector[S] zero_share_rep = rep_vector(0, S);
  {
    vector[S] n_s = rep_vector(0, S);
    for (n in 1:N) {
      real eta_n = alpha[season[n]] + beta_home * home[n] + o[off[n], season[n]]
                   + d[def[n], season[n]] + a[arena[n]];
      zero_share_rep[season[n]] += neg_binomial_2_log_rng(eta_n, phi) == 0;
      n_s[season[n]] += 1;
    }
    zero_share_rep = zero_share_rep ./ n_s;
  }
  vector[T] rr_def = exp(d[, S]);
  vector[T] rr_off = exp(o[, S]);
  array[T] int rank_def;
  array[T] int rank_off;
  for (t in 1:T) {
    rank_def[t] = 1;
    rank_off[t] = 1;
    for (u in 1:T) {
      rank_def[t] += d[u, S] < d[t, S];
      rank_off[t] += o[u, S] > o[t, S];
    }
  }
  real p_zero_fitted = 1;
  real p_zero_average = 1;
  for (k in 1:N_focus) {
    int n = idx_focus[k];
    real base = alpha[S] + beta_home * home[n] + o[off[n], S] + a[arena[n]];
    p_zero_fitted *= exp(neg_binomial_2_log_lpmf(0 | base + d[focus, S], phi));
    p_zero_average *= exp(neg_binomial_2_log_lpmf(0 | base, phi));
  }
}
"

games_all <- pbp_all |>
  distinct(season, game_id, home_team_id, away_team_id, home_team_abbrev, away_team_abbrev)
oop_ct <- pbp_all |> filter(is_oop, made) |> count(game_id, team_id, name = "y")

model_df <- bind_rows(
  games_all |> transmute(season, game_id, off_id = home_team_id, off = home_team_abbrev,
                         def = away_team_abbrev, arena = home_team_abbrev, home = 1),
  games_all |> transmute(season, game_id, off_id = away_team_id, off = away_team_abbrev,
                         def = home_team_abbrev, arena = home_team_abbrev, home = 0)
) |>
  left_join(oop_ct, by = c("game_id", "off_id" = "team_id")) |>
  mutate(y = coalesce(y, 0L)) |>
  filter(off %in% teams30, def %in% teams30) |>
  arrange(season, game_id, desc(home))

model_df |> group_by(season) |>
  summarise(team_games = n(), mean_y = mean(y), var_y = var(y), zero_share = mean(y == 0)) |>
  print()
# Expected: 2025: 2462 tg, mean 1.15, zero share .377; 2026: 2460, 1.14, .377;
#           2027: 72, 1.65, .333

tix <- setNames(seq_along(teams30), teams30)
focus_rows <- which(model_df$season == current & model_df$def == focus)
stan_data <- list(
  N = nrow(model_df), T = 30, S = length(seasons),
  y = model_df$y,
  off = unname(tix[model_df$off]), def = unname(tix[model_df$def]),
  arena = unname(tix[model_df$arena]),
  season = match(model_df$season, seasons),
  home = model_df$home,
  focus = unname(tix[focus]), N_focus = length(focus_rows), idx_focus = focus_rows
)

mod <- cmdstan_model(write_stan_file(stan_code))
fit <- mod$sample(data = stan_data, chains = 4, parallel_chains = 4,
                  iter_warmup = 1000, iter_sampling = 1000, seed = 2027, refresh = 0)
# A few "precision parameter is 0/inf" messages in early warmup are harmless
# rejections; check the diagnostics below instead.
fit$diagnostic_summary()
fit$summary(c("alpha", "beta_home", "sigma_off", "sigma_def", "sigma_arena",
              "rho_off", "rho_def", "phi", "zero_share_rep",
              "p_zero_fitted", "p_zero_average")) |> print(n = Inf)
# Expected: rho_def ~0.74 (defenses persist), sigma_arena ~0.04 (arena tagging
# differences are small), alpha[3] ~0.34 (league rate up ~40% so far this season),
# zero_share_rep ~ .37/.38/.27 vs observed .377/.377/.321.

f <- tix[[focus]]
d_draws  <- as_draws_matrix(fit$draws("d"))
rk_draws <- as_draws_matrix(fit$draws("rank_def"))
q90 <- \(x) sprintf("%.2f [%.2f, %.2f]", mean(x), quantile(x, 0.05), quantile(x, 0.95))

wsh_def <- tibble(
  season = seasons,
  rate_ratio = map_chr(seq_along(seasons),
                       \(s) q90(exp(d_draws[, sprintf("d[%d,%d]", f, s)])))
)
print(wsh_def)
wsh_rank <- rk_draws[, sprintf("rank_def[%d]", f)]
model_summary <- list(
  rank_median   = median(wsh_rank),
  rank_90       = quantile(wsh_rank, c(0.05, 0.95)),
  p_top3        = mean(wsh_rank <= 3),
  p_better_avg  = mean(d_draws[, sprintf("d[%d,%d]", f, length(seasons))] < 0),
  p_zero_fitted = mean(as_draws_matrix(fit$draws("p_zero_fitted")))
)
str(model_summary)
# Expected for WSH: rate ratio 1.23 (2024-25), 1.22 (2025-26), 1.13 [0.91, 1.37]
# now; median rank 24 of 30 (90%: 9 to 30); P(top 3) ~1%; P(better than
# average) ~16%; P(three zero games) ~0.06 at its fitted level, ~0.07 if average.

league_def <- tibble(
  team    = teams30,
  rr_mean = colMeans(exp(d_draws[, sprintf("d[%d,%d]", seq_along(teams30), length(seasons))])),
  rank_median = apply(rk_draws, 2, median),
  p_top3  = colMeans(rk_draws <= 3)
) |> arrange(rr_mean)
print(league_def, n = Inf)
write.csv(league_def, file.path(out_dir, "alley_oop_defense_model.csv"), row.names = FALSE)

# ---- 9. House-style charts --------------------------------------------------
n_games <- max(tot$gp[tot$team == focus])
n_word  <- function(n) if (n <= 10) c("one", "two", "three", "four", "five", "six",
                                      "seven", "eight", "nine", "ten")[n] else as.character(n)
dev_png <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

subs <- c(
  "Free-throw rate"             = "FTA per FGA",
  "Fouls drawn per game"        = "Opponent fouls",
  "Alley-oops allowed per game" = "Fewer is better",
  "Rim FG%"                     = "Shots within 4 feet, dunks, layups"
)

# Bottom-line metrics: top-3 finishes (excluding near-meaningless mass ties),
# plus metrics we always want on the strip chart even if they fall out of the
# top three. The podium below only ever shows true top-three finishes.
always_show <- c("Alley-oops allowed per game", "Alley-oops made per game")
plot_tbl <- all_metrics |>
  filter(team == focus, (rank <= 3 & n_tied < 5) | metric %in% always_show) |>
  arrange(rank, metric)
plot_metrics <- plot_tbl$metric

note_for <- function(m, r, n_tied) {
  x <- all_metrics |> filter(metric == m) |> arrange(rank)
  if (r == 1 && n_tied == 1) {
    nxt <- x |> filter(team != focus) |> slice(1)
    paste0("Next: ", nxt$team, " ", fmt_val(nxt$value, m))
  } else if (n_tied > 1) {
    others <- x |> filter(rank == r, team != focus) |> pull(team)
    paste0("With ", paste(head(others, 3), collapse = ", "),
           if (length(others) > 3) paste0(" +", length(others) - 3) else "")
  } else {
    ahead <- x |> filter(rank < r)
    if (n_distinct(ahead$value) == 1) {
      paste0(paste(ahead$team, collapse = ", "), " at ", fmt_val(ahead$value[1], m))
    } else {
      paste(ahead$team, fmt_val(ahead$value, m), collapse = ", ")
    }
  }
}

lab <- plot_tbl |>
  mutate(
    row      = match(metric, rev(plot_metrics)),
    rank_lab = paste0(if_else(n_tied > 1, "T-", ""), ordinal(rank)),
    note     = pmap_chr(list(metric, rank, n_tied), note_for),
    sub      = unname(coalesce(subs[metric], "")),
    val_lab  = fmt_val(value, metric)
  )

strip <- all_metrics |>
  filter(metric %in% plot_metrics) |>
  group_by(metric) |>
  mutate(pos = if_else(higher_better,
                       (value - min(value)) / (max(value) - min(value)),
                       (max(value) - value) / (max(value) - min(value)))) |>
  arrange(metric, value, desc(team == focus), team) |>
  group_by(metric, value) |>
  mutate(k = row_number() - 1,
         off = if_else(k == 0, 0, if_else(k %% 2 == 1, -1, 1) * ceiling(k / 2))) |>
  ungroup() |>
  mutate(row = match(metric, rev(plot_metrics)), y = row + off * 0.07,
         is_focus = team == focus)

lab <- lab |>
  left_join(strip |> filter(is_focus) |> select(metric, pos), by = "metric") |>
  mutate(lab_x = if_else(pos > 0.8, pos - 0.025, pos + 0.025),
         lab_h = if_else(pos > 0.8, 1, 0),
         lab_y = if_else(nchar(sub) > 0, row + 0.12, row))

model_line <- sprintf(
  "A model using the past two seasons still rates Washington's alley-oop defense about %s.",
  ordinal(round(model_summary$rank_median)))

# Labels live inside the panel (x < 0 and x > 1) so the title, subtitle and
# caption align with the plot edges, as in the house style.
rows <- seq_along(plot_metrics)
p_strip <- ggplot(strip) +
  annotate("segment", x = 0, xend = 1, y = rows, yend = rows, colour = "grey80", linewidth = 0.4) +
  annotate("segment", x = c(0, 1), xend = c(0, 1), y = 0.5, yend = max(rows) + 0.5,
           colour = "grey90", linewidth = 0.4) +
  geom_point(data = filter(strip, !is_focus), aes(pos, y),
             colour = pal[["teal"]], alpha = 0.55, size = 2.8) +
  geom_point(data = filter(strip, is_focus), aes(pos, y), colour = "white", size = 7) +
  geom_point(data = filter(strip, is_focus), aes(pos, y), colour = pal[["red"]], size = 5.5) +
  geom_text(data = lab, aes(lab_x, row + 0.3, label = val_lab, hjust = lab_h),
            colour = pal[["red"]], family = font, size = 6) +
  geom_text(data = lab, aes(-0.03, lab_y, label = metric), hjust = 1, family = font, size = 5.4) +
  geom_text(data = lab, aes(-0.03, row - 0.2, label = sub),
            hjust = 1, family = font, size = 4.2, colour = pal[["grey"]]) +
  geom_text(data = lab, aes(1.04, row + 0.1, label = rank_lab),
            hjust = 0, family = font, fontface = "bold", size = 8.5, colour = pal[["navy"]]) +
  geom_text(data = lab, aes(1.04, row - 0.22, label = note),
            hjust = 0, family = font, size = 4.2, colour = pal[["grey"]]) +
  annotate("text", x = c(0, 1), y = 0.15, label = c("League worst", "League best"),
           family = font, size = 5.2, colour = pal[["grey"]]) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL, limits = c(0, max(rows) + 0.6), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-0.6, 1.38), clip = "off") +
  labs(
    title    = "The Wizards are number one (for now)",
    subtitle = str_wrap(paste0(
      "Every 2026-27 team after ", n_word(n_games), " games, from league worst to league best; ",
      "Washington in red. Washington ranks in the top three in ", sum(plot_tbl$rank <= 3),
      " categories. ", model_line), 85),
    x = NULL, y = NULL, caption = house_caption
  ) +
  theme_wiz()

ggsave(file.path(out_dir, "wizards_strip.png"), p_strip,
       width = 12, height = 3.6 + 1.05 * length(rows), dpi = 200, bg = "white", device = dev_png)

# Podium: 2nd | 1st | 3rd. Items stack upward from each block; each item's
# height depends on how many lines its wrapped label needs.
pod <- tibble(place = c(2, 1, 3), x = 1:3, h = c(0.6, 0.85, 0.4),
              fill = c(pal[["navy"]], pal[["red"]], pal[["teal"]]),
              val_col = c(pal[["navy"]], pal[["red"]], pal[["teal_dark"]]))

pod_items <- lab |>
  filter(rank <= 3) |>
  mutate(place = rank, label = str_wrap(metric, 20),
         nl = str_count(label, "\n") + 1,
         item_h = 0.07 + 0.06 * nl + 0.02 + 0.16 + 0.06) |>
  arrange(place, desc(match(metric, plot_metrics))) |>      # bottom item first
  group_by(place) |>
  mutate(y0 = lag(cumsum(item_h), default = 0)) |>
  ungroup() |>
  left_join(pod, by = "place") |>
  mutate(y0 = h + 0.05 + y0,
         y_label = y0 + 0.07,
         y_value = y0 + 0.07 + 0.06 * nl + 0.02)

p_pod <- ggplot() +
  geom_rect(data = pod, aes(xmin = x - 0.47, xmax = x + 0.47, ymin = 0, ymax = h, fill = fill)) +
  geom_text(data = pod, aes(x, h - 0.03, label = place, size = 12 + 6 * (place == 1)),
            colour = "white", family = font, fontface = "bold", vjust = 1) +
  geom_text(data = pod_items, aes(x - 0.44, y_value, label = val_lab, colour = val_col),
            hjust = 0, vjust = 0, family = font, fontface = "bold", size = 12) +
  geom_text(data = pod_items, aes(x - 0.44, y_label, label = label),
            hjust = 0, vjust = 0, family = font, size = 5.6, lineheight = 0.95) +
  geom_text(data = pod_items, aes(x - 0.44, y0, label = note),
            hjust = 0, vjust = 0, family = font, size = 4.6, colour = pal[["grey"]]) +
  geom_hline(yintercept = 0, colour = pal[["grey"]], linewidth = 0.8) +
  scale_fill_identity() + scale_colour_identity() + scale_size_identity() +
  scale_x_continuous(limits = c(0.5, 3.5), breaks = NULL) +
  scale_y_continuous(limits = c(0, max(pod_items$y_value) + 0.22), breaks = NULL) +
  labs(
    title    = "Where Washington Ranks in the Top Three",
    subtitle = str_wrap(paste0(
      "Out of ", n_distinct(all_metrics$metric), " team metrics after ", n_word(n_games),
      " games of 2026-27. ", model_line), 70),
    x = NULL, y = NULL, caption = house_caption
  ) +
  theme_wiz() +
  theme(plot.margin = margin(24, 24, 18, 24))

ggsave(file.path(out_dir, "wizards_podium.png"), p_pod,
       width = 10.8, height = 13.5, dpi = 200, bg = "white", device = dev_png)

# ---- 10. Lineups: rebuild who was on the floor ------------------------------
# ESPN play-by-play has substitutions (athlete_id_1 enters, athlete_id_2 leaves)
# but no list of who starts each period. Q1 starters come from the box score.
# For later periods, a player is on the floor at the start if his first event
# that period is being subbed OUT or any non-substitution play; gaps are filled
# from whoever ended the previous period and wasn't subbed IN first.
id_chr <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_character_, format(x, scientific = FALSE, trim = TRUE))
}

roster <- pbox |>
  filter(team_abbreviation == focus) |>
  mutate(athlete_id = id_chr(athlete_id), minutes = as.numeric(minutes),
         plus_minus = as.numeric(plus_minus), starter = as.logical(starter))
pname <- roster |> distinct(athlete_id, athlete_short_name) |> tibble::deframe()
pfull <- roster |> distinct(athlete_id, athlete_display_name) |> tibble::deframe()
lu_games <- unique(roster$game_id)

track_game <- function(g) {
  G <- pbp_cur |>
    filter(game_id == g) |>
    arrange(game_play_number) |>
    mutate(a1 = id_chr(athlete_id_1), a2 = id_chr(athlete_id_2), a3 = id_chr(athlete_id_3),
           clk = as.numeric(clock_minutes) * 60 + as.numeric(clock_seconds),
           t = if_else(period_number <= 4,
                       (period_number - 1) * 720 + 720 - clk,
                       2880 + (period_number - 5) * 300 + 300 - clk))
  tid <- if (G$home_team_abbrev[1] == focus) G$home_team_id[1] else G$away_team_id[1]
  R <- roster$athlete_id[roster$game_id == g]
  on <- roster$athlete_id[roster$game_id == g & roster$starter]
  is_sub <- G$type_text == "Substitution" & G$team_id %in% tid
  lineup <- rep(NA_character_, nrow(G))
  prev_end <- on
  for (per in sort(unique(G$period_number))) {
    idx <- which(G$period_number == per)
    if (per > 1) {
      first <- character()
      for (i in idx) {
        if (is_sub[i]) {
          if (!is.na(G$a2[i]) && !G$a2[i] %in% names(first)) first[G$a2[i]] <- "on"
          if (!is.na(G$a1[i]) && !G$a1[i] %in% names(first)) first[G$a1[i]] <- "off"
        } else {
          for (a in c(G$a1[i], G$a2[i], G$a3[i]))
            if (!is.na(a) && a %in% R && !a %in% names(first)) first[a] <- "on"
        }
      }
      on <- names(first)[first == "on"]
      for (a in prev_end) {
        if (length(on) >= 5) break
        if (!identical(unname(first[a]), "off")) on <- union(on, a)
      }
      if (length(on) != 5) stop("Could not resolve lineup: game ", g, " period ", per)
    }
    for (i in idx) {
      if (is_sub[i]) on <- union(setdiff(on, G$a2[i]), G$a1[i])
      else lineup[i] <- paste(sort(on), collapse = "|")
    }
    prev_end <- on
  }
  G |> mutate(lineup = lineup, tid = tid) |> filter(!is.na(lineup))
}

E <- map(lu_games, track_game) |>
  list_rbind() |>
  arrange(game_id, t) |>
  group_by(game_id) |>
  mutate(dt = pmax(lead(t, default = last(t)) - t, 0)) |>
  # Free throws belong to the five on the floor when the foul happened
  group_by(game_id, t) |>
  mutate(lu_pts = if_else(is_ft, first(lineup), lineup)) |>
  ungroup() |>
  mutate(us = coalesce(team_id == tid, FALSE),
         them = !is.na(team_id) & coalesce(team_id != tid, FALSE),
         poss_c = is_fga + 0.44 * is_ft - oreb + tov)

L <- full_join(
  E |> group_by(lineup) |> summarise(sec = sum(dt), .groups = "drop"),
  E |> group_by(lineup = lu_pts) |>
    summarise(pf = sum(pts[us]), pa = sum(pts[them]),
              pu = sum(poss_c[us]), pt = sum(poss_c[them]), .groups = "drop"),
  by = "lineup") |>
  mutate(across(c(sec, pf, pa, pu, pt), ~ coalesce(.x, 0)),
         min = sec / 60, pm = pf - pa, poss = (pu + pt) / 2,
         net = if_else(poss > 0, 100 * pm / poss, NA_real_),
         players = str_split(lineup, fixed("|")),
         names = map_chr(players, \(p) paste(sort(pname[p]), collapse = " \u00b7 ")))

# Audit: minutes and plus-minus by player must match the box score
player_check <- roster |>
  group_by(athlete_id, name = athlete_short_name) |>
  summarise(min_box = sum(minutes, na.rm = TRUE), pm_box = sum(plus_minus, na.rm = TRUE),
            .groups = "drop") |>
  mutate(on_mask = map(athlete_id, \(a) map_lgl(L$players, \(p) a %in% p)),
         min_pbp = map_dbl(on_mask, \(m) sum(L$min[m])),
         pm_pbp  = map_dbl(on_mask, \(m) sum(L$pm[m]))) |>
  select(-on_mask)
print(player_check, n = Inf)
cat("Plus-minus mismatches:", sum(player_check$pm_pbp != player_check$pm_box), "\n")
# Expected: minutes within 1 of the box (box rounds) and 0 plus-minus mismatches

# Shrink each lineup's net rating toward the team average.
# Noise: ~155 points per 100 for a single pair of possessions, so SE = 155/sqrt(poss).
# Prior: true lineup net ratings vary around the team average with SD tau = 8.
team_net <- 100 * sum(L$pm) / sum(L$poss)
shrink_net <- function(net, poss, mu = team_net, tau = 8, sd1 = 155) {
  se <- sd1 / sqrt(poss); w <- tau^2 / (tau^2 + se^2)
  tibble(post = mu + w * (net - mu), post_sd = sqrt(1 / (1 / tau^2 + 1 / se^2)), weight = w)
}
L <- bind_cols(L, shrink_net(L$net, L$poss))

L |> filter(min >= 3) |> arrange(desc(post)) |>
  select(names, min, poss, pf, pa, net, post, post_sd, weight) |>
  print(n = Inf, width = Inf)
# Expected: 53 lineups, 144 minutes. Starting five 37.2 min, 88-95, net -7.8,
# shrunk ~0.0 (weight 0.19). Every shrunk estimate sits within ~4 of team net.

# On/off by player
on_off <- roster |>
  distinct(athlete_id, name = athlete_short_name) |>
  mutate(on = map(athlete_id, \(a) map_lgl(L$players, \(p) a %in% p)),
         min_on  = map_dbl(on, \(m) sum(L$min[m])),
         net_on  = map_dbl(on, \(m) 100 * sum(L$pm[m]) / sum(L$poss[m])),
         net_off = map_dbl(on, \(m) 100 * sum(L$pm[!m]) / sum(L$poss[!m])),
         on_off  = net_on - net_off) |>
  select(-on) |>
  filter(min_on >= 10) |>                 # drop garbage-time cameos
  arrange(desc(on_off))
print(on_off)
# Expected: Champagnie +41.3 (on +24.0, off -17.3) ... Coulibaly -24.6

# Lineup types: the most-used starting five, units with the spotlight player
# (best on/off among regulars), and everything else
starting_five <- roster |> filter(starter) |>
  group_by(game_id) |> summarise(l = paste(sort(athlete_id), collapse = "|")) |>
  count(l, sort = TRUE) |> slice(1) |> pull(l)
spotlight_id <- on_off |> filter(min_on >= 20) |> slice(1) |> pull(athlete_id)
spotlight <- pfull[[spotlight_id]]
lu_type <- function(l) {
  has_spot <- map_lgl(str_split(l, fixed("|")), \(p) spotlight_id %in% p)
  case_when(l == starting_five ~ "Starting five",
            has_spot ~ paste("Lineups with", spotlight),
            TRUE ~ "All other lineups")
}
type_tbl <- L |> mutate(type = lu_type(lineup)) |>
  group_by(type) |>
  summarise(min = sum(min), pf = sum(pf), pa = sum(pa), poss = sum(poss), .groups = "drop") |>
  mutate(net = 100 * (pf - pa) / poss)
print(type_tbl)
# Expected: Champagnie units 69.6 min, 183-147, +24.0; starting five 37.2, 88-95,
# -7.8; others 37.2, 83-106, -27.6

write.csv(L |> select(-players), file.path(out_dir, "wizards_lineups.csv"), row.names = FALSE)

# ---- 11. Lineup charts --------------------------------------------------------
type_cols <- setNames(c(pal[["navy"]], pal[["teal"]], "#C9CED6"),
                      c("Starting five", paste("Lineups with", spotlight),
                        "All other lineups"))

# Stints: consecutive events with the same five
stints <- E |>
  group_by(game_id) |>
  mutate(sid = cumsum(lineup != lag(lineup, default = ""))) |>
  group_by(game_id, sid, lineup) |>
  summarise(s = min(t), e = max(t + dt),
            pf = sum(pts[us]), pa = sum(pts[them]), .groups = "drop") |>
  filter(e > s) |>
  mutate(type = lu_type(lineup))

top10 <- player_check |> arrange(desc(min_box)) |> slice_head(n = 10)
row_of <- setNames(10:1, top10$athlete_id)            # most minutes at the top

segs <- stints |>
  mutate(pid = str_split(lineup, fixed("|"))) |>
  unnest(pid) |>
  filter(pid %in% top10$athlete_id) |>
  mutate(row = row_of[pid])

game_lab <- tbox |>
  filter(team_abbreviation == focus) |>
  transmute(game_id,
            label = sprintf("%s vs %s \u00b7 %s %d\u2013%d", format(as.Date(game_date), "%b %d"),
                            opponent_team_display_name,
                            if_else(team_winner, "W", "L"), team_score, opponent_team_score))

base <- 12.6; k <- 0.12                                # margin strip sits above the rows
margin <- pbp_cur |>
  filter(game_id %in% lu_games) |>
  arrange(game_id, game_play_number) |>
  mutate(clk = as.numeric(clock_minutes) * 60 + as.numeric(clock_seconds),
         t = if_else(period_number <= 4, (period_number - 1) * 720 + 720 - clk,
                     2880 + (period_number - 5) * 300 + 300 - clk),
         m = (as.numeric(home_score) - as.numeric(away_score)) *
             if_else(home_team_abbrev == focus, 1, -1)) |>
  group_by(game_id) |>
  mutate(t_end = lead(t, default = max(t))) |>
  ungroup() |>
  filter(t_end > t, m != 0)

best_stint <- stints |>
  mutate(pm = pf - pa) |>
  group_by(game_id) |> slice_max(pm, n = 1, with_ties = FALSE) |> ungroup() |>
  filter(pm >= 8) |>   # Expected: Oct 21 16-5 in 4:08; Oct 24 20-6 in 5:10 (starting five)
  mutate(run_label = sprintf("%d\u2013%d in %d:%02d \u00b7 %s", as.integer(pf), as.integer(pa),
                             as.integer((e - s) %/% 60), as.integer(round((e - s) %% 60)),
                         map_chr(str_split(lineup, fixed("|")),
                                 \(p) paste(str_remove(pname[p], "^.*\\. "), collapse = ", "))))

facet_lab <- function(df) df |> left_join(game_lab, by = "game_id")

p_bar <- ggplot() +
  geom_vline(xintercept = c(12, 24, 36), colour = "grey88") +
  geom_rect(data = facet_lab(margin),
            aes(xmin = t / 60, xmax = t_end / 60, ymin = base, ymax = base + m * k,
                fill = if_else(m > 0, "lead", "trail")), alpha = 0.4) +
  geom_hline(yintercept = base, colour = "grey60", linewidth = 0.3) +
  geom_rect(data = facet_lab(segs),
            aes(xmin = s / 60, xmax = e / 60, ymin = row - 0.36, ymax = row + 0.36, fill = type)) +
  geom_segment(data = facet_lab(best_stint),
               aes(x = s / 60, xend = e / 60, y = base + 2.45, yend = base + 2.45),
               colour = pal[["teal_dark"]], linewidth = 1) +
  geom_text(data = facet_lab(best_stint),
            aes(x = s / 60 - 0.3, y = base + 2.45, label = run_label),
            hjust = 1, family = font, fontface = "bold", size = 4.3, colour = pal[["teal_dark"]]) +
  scale_fill_manual(values = c(type_cols, lead = pal[["navy"]], trail = pal[["red"]]),
                    breaks = names(type_cols), name = NULL) +
  scale_x_continuous(breaks = c(6, 18, 30, 42), labels = paste0("Q", 1:4),
                     limits = c(0, 48), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(1:10, base), labels = c(rev(top10$name), "Score margin"),
                     expand = expansion(add = 0.5)) +
  facet_wrap(~ label, ncol = 1) +
  labs(
    title    = paste(spotlight, "Is the Common Thread"),
    subtitle = str_wrap(sprintf(
      "Who was on the floor for every second of Washington's first %s games. Lineups with %s are %+.0f per 100 possessions; the starting five is %+.0f.",
      n_word(length(lu_games)), spotlight,
      type_tbl$net[str_detect(type_tbl$type, "^Lineups with")],
      type_tbl$net[type_tbl$type == "Starting five"]), 95),
    x = NULL, y = NULL, caption = house_caption
  ) +
  theme_wiz() +
  theme(legend.position = "top", legend.justification = "left",
        strip.text = element_text(face = "bold", hjust = 0, size = rel(1.05)),
        axis.text.y = element_text(colour = "black"))

ggsave(file.path(out_dir, "wizards_rotation_barcode.png"), p_bar,
       width = 14.4, height = 3 + 4.2 * length(lu_games), dpi = 200, bg = "white",
       device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")

# On/off + "why no best five yet": one figure, two panels (needs patchwork)
panel_theme <- theme(plot.title = element_text(face = "bold", size = rel(1.15), colour = "black"),
                     axis.text.y = element_text(colour = "black"),
                     panel.grid.major.x = element_line(colour = "grey90"))

p_onoff <- on_off |>
  mutate(name = factor(name, levels = rev(name)),
         col = if_else(on_off >= 0, pal[["teal"]], pal[["red"]]),
         lab_col = if_else(on_off >= 0, pal[["teal_dark"]], pal[["red"]])) |>
  ggplot(aes(on_off, name)) +
  geom_col(aes(fill = col), width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_text(aes(label = sprintf("%+.1f", on_off), colour = lab_col,
                hjust = if_else(on_off >= 0, -0.15, 1.15)), family = font, size = 5.5) +
  scale_fill_identity() + scale_colour_identity() +
  scale_x_continuous(limits = c(-1, 1) * (max(abs(on_off$on_off)) + 16)) +
  labs(title = "On minus off, per 100 possessions", x = NULL, y = NULL) +
  theme_wiz() + panel_theme

# Five most-used lineups: raw net (open dot) vs shrunk estimate (filled dot)
last_names <- \(ids) paste(str_remove(pname[ids], "^.*\\. "), collapse = " \u00b7 ")
shrink_tbl <- L |>
  slice_max(poss, n = 5, with_ties = FALSE) |>
  mutate(label = if_else(lineup == starting_five, "Starting five", map_chr(players, last_names)),
         label = sprintf("%s\n%.0f possessions", label, poss),
         label = factor(label, levels = rev(label)),
         raw_c = pmax(pmin(net, 40), -40))

p_shrink <- ggplot(shrink_tbl, aes(y = label)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_segment(aes(x = raw_c, xend = post, yend = label), colour = "#9AA3AE", linewidth = 0.9) +
  geom_point(aes(x = raw_c), shape = 21, fill = "white", colour = pal[["red"]], size = 4.5, stroke = 1.3) +
  geom_point(aes(x = post), colour = pal[["navy"]], size = 4.5) +
  scale_x_continuous(limits = c(-40, 40), breaks = c(-40, 0, 40), labels = c("\u221240", "0", "+40")) +
  labs(title = 'Why no "best five" yet',
       subtitle = str_wrap(sprintf(
         "Raw net rating (open dot) pulled toward the team average by sample size (filled dot). No unit has played %d possessions.",
         ceiling(max(L$poss) / 10) * 10), 55),
       x = NULL, y = NULL) +
  theme_wiz() + panel_theme +
  theme(plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 10)),
        axis.text.y = element_text(colour = "black", size = rel(0.8), lineheight = 1))

if (requireNamespace("patchwork", quietly = TRUE)) {
  p_detail <- patchwork::wrap_plots(p_onoff, p_shrink, widths = c(1, 1.1)) +
    patchwork::plot_annotation(
      title    = "Players Tell Us More Than Lineups, For Now",
      subtitle = sprintf("On-off splits across Washington's first %s games, and why no five-man unit has earned the title of best lineup yet.",
                         n_word(length(lu_games))) |> str_wrap(110),
      caption  = paste0("Lineups rebuilt from substitutions; player minutes and plus-minus match the box score.\n",
                        "Shrinkage assumes true lineup net ratings vary with SD 8 per 100.\n\n", house_caption),
      theme    = theme_wiz())
  ggsave(file.path(out_dir, "wizards_on_off_shrinkage.png"), p_detail,
         width = 14.4, height = 9.2, dpi = 200, bg = "white",
         device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")
} else {
  message("Install 'patchwork' to combine the on/off and shrinkage panels; saving them separately.")
  for (nm in c("on_off", "shrinkage")) {
    p <- if (nm == "on_off") p_onoff else p_shrink
    ggsave(file.path(out_dir, paste0("wizards_", nm, ".png")), p + labs(caption = house_caption),
           width = 7.2, height = 9.2, dpi = 200, bg = "white",
           device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")
  }
}

# Net rating by lineup type, as its own figure
type_tbl <- type_tbl |>
  mutate(detail_lab = sprintf("%.1f minutes \u00b7 %d\u2013%d", min, as.integer(pf), as.integer(pa)))
p_types <- type_tbl |>
  mutate(type = factor(type, levels = type[order(net)]),
         lab  = sprintf("%+.1f", net)) |>
  ggplot(aes(net, type)) +
  geom_col(aes(fill = type), width = 0.6) +
  geom_vline(xintercept = 0, colour = pal[["grey"]], linewidth = 0.8) +
  geom_text(aes(label = lab, hjust = if_else(net >= 0, -0.2, 1.2)),
            family = font, fontface = "bold", size = 10) +
  scale_fill_manual(values = c(type_cols[1:2], "All other lineups" = "#9AA3AE"), guide = "none") +
  scale_y_discrete(labels = \(x) paste0(x, "\n", type_tbl$detail_lab[match(x, type_tbl$type)])) +
  scale_x_continuous(limits = c(-1, 1) * (max(abs(type_tbl$net)) + 14)) +
  labs(title = "Net Rating by Lineup Type",
       subtitle = str_wrap(sprintf(
         "Points per 100 possessions in Washington's first %s games, split by who was on the floor.",
         n_word(length(lu_games))), 70),
       x = "Net rating, points per 100", y = NULL, caption = house_caption) +
  theme_wiz() +
  theme(axis.text.y = element_text(colour = "black", size = rel(1.1), lineheight = 1.1),
        panel.grid.major.x = element_line(colour = "grey90"))

ggsave(file.path(out_dir, "wizards_lineup_types.png"), p_types,
       width = 10.8, height = 10.8, dpi = 200, bg = "white",
       device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")

message("Saved charts and tables to ", normalizePath(out_dir))
