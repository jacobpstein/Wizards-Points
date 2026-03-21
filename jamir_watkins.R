###############################################################################
# Jamir Watkins Deep Dive
# Making the case: quality defender, effective in the paint, elevates teammates
# R version 4.5.2 (2025-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sequoia
###############################################################################

# Setup ----

library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(bayesplot)
library(loo)
library(usaidplot)
library(ggridges)
library(ggtext)
library(janitor)
library(ggrepel)
library(slider)

set.seed(202)

extrafont::loadfonts(quiet = TRUE)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 3)

wizards_id <- 1610612764
season_str <- "2025-26"

# Jamir's person_id (looked up via nba_playerindex previously)
jamir_id <- 1642364
jamir_name <- "Jamir Watkins"


# Pull in data from python file ----
# I was having trouble using hoopR so you need to run the python script to get the data
# this is a bummer

game_dates <- read_csv("jamir_game_dates.csv", show_col_types = FALSE) |>
  clean_names() |>
  dplyr::select(any_of(c("game_id", "game_date", "matchup"))) |>
  distinct() |>
  mutate(game_id = str_pad(as.character(game_id), 10, pad = "0"))

trad_box <- read_csv(
  "jamir_boxscore_traditional.csv"
  , col_types = cols(minutes = col_character(), .default = col_guess())
  , show_col_types = FALSE
) |>
  clean_names() |>
  mutate(game_id = str_pad(as.character(game_id), 10, pad = "0"))

adv_box <- read_csv(
  "jamir_boxscore_advanced.csv"
  , col_types = cols(minutes = col_character(), .default = col_guess())
  , show_col_types = FALSE
) |>
  clean_names() |>
  mutate(game_id = str_pad(as.character(game_id), 10, pad = "0"))

league_adv <- read_csv("jamir_league_advanced.csv", show_col_types = FALSE) |>
  clean_names()

# on_off derived from 5-man lineup data via Python (teamplayeronoffdetails endpoint is unavailable)
on_off <- read_csv("jamir_on_off.csv", show_col_types = FALSE) |>
  clean_names() |>
  rename(status = status)

shot_chart_jamir <- read_csv("jamir_shot_chart.csv", show_col_types = FALSE) |>
  clean_names()

shot_chart_team <- read_csv("jamir_shot_chart_team.csv", show_col_types = FALSE) |>
  clean_names()

lineup_data <- read_csv("jamir_lineups.csv", show_col_types = FALSE) |>
  clean_names()

league_trad <- read_csv("jamir_league_traditional.csv", show_col_types = FALSE) |>
  clean_names()

hustle_stats <- read_csv("jamir_hustle_stats.csv", show_col_types = FALSE) |>
  clean_names()

# Normalize games-played column name (hustle endpoint uses "g" not "gp")
if ("g" %in% names(hustle_stats) && !"gp" %in% names(hustle_stats)) {
  hustle_stats <- hustle_stats |> rename(gp = g)
}


# Defensive Analysis ----

# Clean traditional box scores
trad_clean <- trad_box |>
  mutate(
    player_name = paste(first_name, family_name)
    , min_numeric = as.numeric(ms(minutes)) / 60
    , min_numeric = ifelse(is.na(min_numeric), 0, min_numeric)
    , across(
      any_of(c("points", "rebounds_total", "rebounds_offensive"
               , "rebounds_defensive", "assists", "steals", "blocks"
               , "field_goals_attempted", "field_goals_made"))
      , as.numeric
    )
  )

# Clean advanced box scores (game-level defensive rating)
adv_clean <- adv_box |>
  mutate(
    player_name = paste(first_name, family_name)
    , min_numeric = as.numeric(ms(minutes)) / 60
    , min_numeric = ifelse(is.na(min_numeric), 0, min_numeric)
    , across(
      any_of(c("offensive_rating", "defensive_rating", "net_rating"
               , "usage_percentage"))
      , as.numeric
    )
  )

# game_dates was cached from the data pull phase (nba_leaguegamefinder)

# Game-level defensive data for Jamir
jamir_games <- adv_clean |>
  filter(person_id == jamir_id & min_numeric > 0) |>
  left_join(game_dates, by = "game_id") |>
  arrange(game_date) |>
  mutate(game_number = row_number())

# Rolling 5-game defensive rating
jamir_def_rolling <- jamir_games |>
  mutate(
    def_rating_roll = slide_dbl(
      defensive_rating, mean, .before = 4, .complete = FALSE, na.rm = TRUE
    )
  )

# On/off defensive differential
on_off_summary <- on_off |>
  mutate(
    across(any_of(c("net_rating", "def_rating", "off_rating")), as.numeric)
  ) |>
  dplyr::select(vs_player_name, status, net_rating, def_rating, off_rating) |>
  pivot_wider(
    names_from = status
    , values_from = c(net_rating, def_rating, off_rating)
    , names_sep = "_"
  ) |>
  mutate(
    net_diff = net_rating_ON - net_rating_OFF
    , def_diff = def_rating_ON - def_rating_OFF
    , off_diff = off_rating_ON - off_rating_OFF
  )

jamir_on_off <- on_off_summary |>
  filter(str_detect(vs_player_name, "Watkins"))

# Season-level defensive rating from league dashboard
wizards_adv <- league_adv |>
  filter(team_id == wizards_id) |>
  mutate(across(any_of(c("def_rating", "off_rating", "net_rating", "min", "gp")),
                as.numeric))

jamir_season_def <- wizards_adv |>
  filter(str_detect(player_name, "Watkins"))

# Per-game averages
jamir_trad_summary <- trad_clean |>
  filter(person_id == jamir_id & min_numeric > 0) |>
  summarise(
    games = n()
    , mpg = mean(min_numeric)
    , spg = mean(steals)
    , bpg = mean(blocks)
    , ppg = mean(points)
    , rpg = mean(rebounds_total)
  )


# Paint Performance ----

# Jamir shot zones
jamir_zones <- shot_chart_jamir |>
  mutate(shot_made_flag = as.numeric(shot_made_flag)) |>
  group_by(shot_zone_basic) |>
  summarise(
    fga = n()
    , fgm = sum(shot_made_flag, na.rm = TRUE)
    , fg_pct = fgm / fga
    , .groups = "drop"
  ) |>
  mutate(source = "Jamir Watkins")

# Team shot zones
team_zones <- shot_chart_team |>
  mutate(shot_made_flag = as.numeric(shot_made_flag)) |>
  group_by(shot_zone_basic) |>
  summarise(
    fga = n()
    , fgm = sum(shot_made_flag, na.rm = TRUE)
    , fg_pct = fgm / fga
    , .groups = "drop"
  ) |>
  mutate(source = "Wizards Team")

# Combine for paint zones
paint_comparison <- bind_rows(jamir_zones, team_zones) |>
  filter(shot_zone_basic %in% c("Restricted Area", "In The Paint (Non-RA)"))

paint_comparison |>
  arrange(shot_zone_basic, source) |>
  mutate(fg_pct_label = scales::percent(fg_pct, accuracy = 0.1)) 

# Points in the paint per game for Jamir
n_games_jamir <- jamir_trad_summary$games

jamir_paint_totals <- shot_chart_jamir |>
  mutate(shot_made_flag = as.numeric(shot_made_flag)) |>
  filter(shot_zone_basic %in% c("Restricted Area", "In The Paint (Non-RA)")) |>
  summarise(
    paint_fgm = sum(shot_made_flag, na.rm = TRUE)
    , paint_fga = n()
  ) |>
  mutate(
    paint_pts = paint_fgm * 2
    , paint_ppg = paint_pts / n_games_jamir
    , paint_fga_pg = paint_fga / n_games_jamir
  )

# teammate Lift ----

# Rank all Wizards by on/off net rating differential
on_off_rankings <- on_off_summary |>
  arrange(desc(net_diff))

on_off_rankings |>
  dplyr::select(vs_player_name, net_diff, def_diff) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  print(n = 20)

# Best lineups containing Jamir
if (nrow(lineup_data) > 0 && "group_name" %in% names(lineup_data)) {
  jamir_lineups <- lineup_data |>
    filter(str_detect(group_name, "Watkins")) |>
    mutate(across(any_of(c("net_rating", "def_rating", "off_rating", "min")),
                  as.numeric)) |>
    arrange(desc(net_rating))
} else {
  jamir_lineups <- tibble()
}

if (nrow(jamir_lineups) > 0) {
  jamir_lineups |>
    dplyr::select(group_name, min, net_rating, def_rating, off_rating) |>
    mutate(across(where(is.numeric), ~ round(.x, 1))) |>
    head(5) 

  # Identify best-fit teammate from lineup combos
  jamir_lineup_players <- jamir_lineups |>
    mutate(
      teammates = str_remove(group_name, "Watkins")
      , teammates = str_remove_all(teammates, "^ - | - $")
    ) |>
    separate_rows(teammates, sep = " - ") |>
    filter(teammates != "" & !str_detect(teammates, "Watkins")) |>
    group_by(teammates) |>
    summarise(
      n_lineups = n()
      , avg_net_rating = weighted.mean(net_rating, w = pmax(min, 1), na.rm = TRUE)
      , total_min = sum(min, na.rm = TRUE)
      , .groups = "drop"
    ) |>
    filter(total_min > 10) |>
    arrange(desc(avg_net_rating))

  best_fit_teammate <- jamir_lineup_players$teammates[1]
  message("\nBest-fit lineup partner: ", best_fit_teammate)
} else {
  message("No lineup data available")
  jamir_lineup_players <- tibble(
    teammates = character(), avg_net_rating = numeric(), total_min = numeric()
  )
  best_fit_teammate <- NA
}


# Modelsssss ----

# Prep game-level defensive rating for all Wizards players with meaningful minutes
model_df <- adv_clean |>
  filter(min_numeric >= 5) |>
  left_join(game_dates, by = "game_id") |>
  arrange(person_id, game_date) |>
  group_by(person_id) |>
  mutate(game_num_raw = row_number()) |>
  ungroup() |>
  filter(!is.na(defensive_rating))

# Create player index mapping
player_lookup <- model_df |>
  distinct(person_id, player_name) |>
  mutate(player_index = row_number())

model_df <- model_df |>
  left_join(player_lookup, by = c("person_id", "player_name")) |>
  mutate(
    game_num_z = as.numeric(scale(game_num_raw))
  )

# Stan data
# minutes_raw passed as raw values (not z-scored) so the precision weighting
# sigma_y / sqrt(minutes_raw / mean_minutes) is on an interpretable scale.
# mean_minutes is the reference level at which sigma_y is defined.
mean_minutes <- mean(model_df$min_numeric)

stan_data <- list(
  N = nrow(model_df)
  , N_players = nrow(player_lookup)
  , player = model_df$player_index
  , game_num = model_df$game_num_z
  , minutes_raw = model_df$min_numeric
  , mean_minutes = mean_minutes
  , def_rating = model_df$defensive_rating
)

message("\nStan data: ", stan_data$N, " obs, ", stan_data$N_players, " players")

# Compile and fit (with caching)
def_mod <- cmdstan_model("jamir_watkins_defense.stan")

fit_cache_dir <- "jamir_defense_fit_cache"
fit_cache_csvs <- if (dir.exists(fit_cache_dir)) {
  list.files(fit_cache_dir, pattern = "\\.csv$", full.names = TRUE)
} else {
  character(0)
}

if (length(fit_cache_csvs) == 4) {
  message("Loading cached Stan fit from ", fit_cache_dir)
  fit <- as_cmdstan_fit(fit_cache_csvs)
} else {
  dir.create(fit_cache_dir, showWarnings = FALSE)
  fit <- def_mod$sample(
    data = stan_data
    , seed = 202
    , chains = 4
    , parallel_chains = 4
    , iter_warmup = 2000
    , iter_sampling = 2000
    , adapt_delta = 0.95
    , output_dir = fit_cache_dir
  )
}

# Convergence checks
fit$diagnostic_summary()

convergence_issues <- fit$summary() |>
  filter(rhat > 1.01 | ess_bulk < 400)

if (nrow(convergence_issues) > 0) {
  message("\nWARNING: Some parameters have convergence issues:")
  print(convergence_issues)
} else {
  message("\nAll parameters converged: rhat < 1.01, ess_bulk > 400")
}

# LOO-CV
loo_result <- fit$loo()

# Extract key posteriors
theta_draws <- fit$draws("theta", format = "draws_df") |>
  pivot_longer(
    cols = starts_with("theta")
    , names_to = "variable"
    , values_to = "theta"
  ) |>
  mutate(player_index = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(player_lookup, by = "player_index")

trend_draws <- fit$draws("beta_trend", format = "draws_df") |>
  pivot_longer(
    cols = starts_with("beta_trend")
    , names_to = "variable"
    , values_to = "beta_trend"
  ) |>
  mutate(player_index = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(player_lookup, by = "player_index")

# Jamir's player index
jamir_idx <- player_lookup |>
  filter(person_id == jamir_id) |>
  pull(player_index)

# Theta summary (defensive ability — lower = better)
theta_summary <- fit$summary("theta") |>
  mutate(player_index = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(player_lookup, by = "player_index") |>
  arrange(mean)

theta_summary |>
  dplyr::select(player_name, mean, median, q5, q95) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  print(n = 20)

# Observed vs. model rank comparison (active roster)
active_roster_cmp <- c("Carrington","Riley","Champagnie","Tre Johnson","Coulibaly",
  "Sarr","Vukcevic","Gill","Watkins","Cooper","Hardy","Young","Reese")

rank_comparison <- theta_summary |>
  dplyr::select(player_name, model_theta = mean) |>
  left_join(
    wizards_adv |> mutate(def_rating = as.numeric(def_rating), min = as.numeric(min)) |>
      dplyr::select(player_name, def_rating, min)
    , by = "player_name"
  ) |>
  filter(str_detect(player_name, paste(active_roster_cmp, collapse = "|"))) |>
  mutate(
    obs_rank   = rank(def_rating,   ties.method = "first")
    , model_rank = rank(model_theta, ties.method = "first")
    , rank_shift = obs_rank - model_rank
  ) |>
  arrange(obs_rank) |>
  transmute(
    Player = player_name, MPG = round(min, 1)
    , `Obs. Def Rtg` = round(def_rating, 1), `Obs. Rank` = obs_rank
    , `Model θ` = round(model_theta, 1), `Model Rank` = model_rank
    , `Rank Shift` = rank_shift
  )

# Trend summary
trend_summary <- fit$summary("beta_trend") |>
  mutate(player_index = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(player_lookup, by = "player_index") |>
  arrange(mean)

jamir_trend <- trend_summary |> filter(player_index == jamir_idx)


# Viz time! ----

# League average defensive rating for reference line
league_avg_def <- league_adv |>
  mutate(def_rating = as.numeric(def_rating)) |>
  summarise(avg = mean(def_rating, na.rm = TRUE)) |>
  pull(avg)

# Plot 1: Defensive ability posteriors ----
# Add a flag to highlight Jamir
theta_draws <- theta_draws |>
  mutate(is_jamir = player_name == "Jamir Watkins")

# Compute median theta per player for fill color
theta_medians <- theta_draws |>
  group_by(player_name) |>
  summarise(med_theta = median(theta), .groups = "drop") |>
  mutate(is_jamir = player_name == "Jamir Watkins")

p1 <- ggplot(
  theta_draws
  , aes(
    x = theta
    , y = reorder(player_name, theta)
  )
) +
  stat_halfeye(
    aes(fill = reorder(player_name, theta) == "Jamir Watkins")
    , .width = c(0.5, 0.8, 0.95)
    , show.legend = FALSE
  ) +
  scale_fill_manual(values = c("TRUE" = "maroon", "FALSE" = "#3B9AB2")) +
  geom_vline(
    xintercept = league_avg_def
    , linetype = "dashed"
    , color = "grey40"
  ) +
  annotate(
    "text"
    , x = league_avg_def + 0.3
    , y = 1
    , label = "League Avg"
    , hjust = 0
    , size = 3
    , color = "grey40"
  ) +
  labs(
    title = "Estimated defensive ability among Wizards players"
    , subtitle = "Posterior distributions of baseline defensive rating (lower = better)\nEstimates share information hierarchically and account for minutes played"
    , x = "Defensive Rating (lower = better)"
    , y = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaidplot::usaid_plot() +
  theme(
    axis.text.y = element_text(
      color = ifelse(
        levels(reorder(theta_medians$player_name, theta_medians$med_theta)) == "Jamir Watkins"
        , "maroon"
        , "grey30"
      )
      , face = ifelse(
        levels(reorder(theta_medians$player_name, theta_medians$med_theta)) == "Jamir Watkins"
        , "bold"
        , "plain"
      )
    )
  )

ggsave("jamir_defense_posterior.png", p1, width = 16, height = 10, dpi = 600, device = ragg::agg_png)


# Plot 2: Paint scoring comparison ----
paint_plot_df <- paint_comparison |>
  mutate(
    zone_label = case_when(
      shot_zone_basic == "Restricted Area" ~ "Restricted\nArea"
      , TRUE ~ "Paint\n(Non-RA)"
    )
  )

# Force factor ordering so Jamir comes first (gets the first color)
paint_plot_df <- paint_plot_df |>
  mutate(source = factor(source, levels = c("Jamir Watkins", "Wizards Team")))

p2 <- ggplot(
  paint_plot_df
  , aes(x = zone_label, y = fg_pct, fill = source)
) +
  geom_col(
    position = position_dodge(width = 0.7)
    , width = 0.6
  ) +
  geom_text(
    aes(label = scales::percent(fg_pct, accuracy = 0.1))
    , position = position_dodge(width = 0.7)
    , vjust = -0.5
    , size = 4
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
    , expand = expansion(mult = c(0, 0.15))
  ) +
  usaidplot::usaid_plot() +
  scale_fill_manual(
    values = c("Jamir Watkins" = "maroon", "Wizards Team" = "#3B9AB2")
  ) +
  labs(
    title = "<span style='color:maroon;'>Jamir Watkins</span> vs <span style='color:#3B9AB2;'>Wizards Team</span> Shooting in the Paint"
    , subtitle = "Comparing field goal percentage by paint zone"
    , x = NULL
    , y = "Field Goal %"
    , fill = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(
    plot.title = element_markdown()
    , legend.position = "none"
  )

ggsave("jamir_paint_scoring.png", p2, width = 14, height = 10, dpi = 600, device = ragg::agg_png)


# Plot 3: On/off impact dumbbell chart ----
# Clean up names: "Last, First" -> "First Last" and filter NAs
on_off_plot <- on_off_summary |>
  filter(!is.na(net_rating_ON) & !is.na(net_rating_OFF)) |>
  mutate(
    player_display = str_replace(vs_player_name, "^(.+),\\s*(.+)$", "\\2 \\1")
    , is_jamir = str_detect(vs_player_name, "Watkins")
  )

p3 <- ggplot(on_off_plot, aes(y = reorder(player_display, net_diff))) +
  geom_segment(
    aes(
      x = net_rating_OFF
      , xend = net_rating_ON
      , yend = reorder(player_display, net_diff)
    )
    , color = "grey70"
    , linewidth = 1
  ) +
  geom_point(
    aes(x = net_rating_OFF)
    , color = "maroon"
    , size = 4
  ) +
  geom_point(
    aes(x = net_rating_ON)
    , color = "#3B9AB2"
    , size = 4
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Team net rating with each player <span style='color:#3B9AB2;'>ON</span> vs <span style='color:maroon;'>OFF</span> the court"
    , subtitle = "Players ordered by the size of their on/off differential"
    , x = "Team Net Rating"
    , y = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaidplot::usaid_plot() +
  theme(
    plot.title = element_markdown()
    , axis.text.y = element_text(
      color = ifelse(on_off_plot$is_jamir[order(on_off_plot$net_diff)], "maroon", "grey30")
      , face = ifelse(on_off_plot$is_jamir[order(on_off_plot$net_diff)], "bold", "plain")
    )
  )

ggsave("jamir_on_off_impact.png", p3, width = 14, height = 10, dpi = 600, device = ragg::agg_png)


# Plot 4: Defensive trend over the season ----
# Use Jamir's actual game-level data with posterior predictive intervals
# Filter to games with >= 5 min (matching model) to avoid extreme def ratings

jamir_model_games <- model_df |>
  filter(person_id == jamir_id) |>
  arrange(game_num_raw)

# Extract Jamir's posterior draws
jamir_theta_draws <- fit$draws(paste0("theta[", jamir_idx, "]"), format = "matrix")
jamir_trend_draws <- fit$draws(paste0("beta_trend[", jamir_idx, "]"), format = "matrix")
sigma_y_draws <- fit$draws("sigma_y", format = "matrix")

# Compute posterior predictive for each of Jamir's actual games.
# Noise is precision-weighted: sigma_y / sqrt(minutes / mean_minutes).
# Short-minute games have wider predictive intervals.
pred_matrix <- matrix(NA, nrow = nrow(jamir_theta_draws), ncol = nrow(jamir_model_games))
for (g in seq_len(nrow(jamir_model_games))) {
  mu_g    <- jamir_theta_draws[, 1] +
    jamir_trend_draws[, 1] * jamir_model_games$game_num_z[g]
  sigma_g <- sigma_y_draws[, 1] / sqrt(jamir_model_games$min_numeric[g] / mean_minutes)
  pred_matrix[, g] <- rnorm(nrow(jamir_theta_draws), mu_g, sigma_g)
}

pred_summary <- tibble(
  game_number = seq_len(nrow(jamir_model_games))
  , median = apply(pred_matrix, 2, median)
  , lower = apply(pred_matrix, 2, quantile, 0.1)
  , upper = apply(pred_matrix, 2, quantile, 0.9)
)

# Also compute the mean trend line (without sigma_y) for the dashed line
trend_matrix <- matrix(NA, nrow = nrow(jamir_theta_draws), ncol = nrow(jamir_model_games))
for (g in seq_len(nrow(jamir_model_games))) {
  trend_matrix[, g] <- jamir_theta_draws[, 1] +
    jamir_trend_draws[, 1] * jamir_model_games$game_num_z[g]
}

trend_summary <- tibble(
  game_number = seq_len(nrow(jamir_model_games))
  , median = apply(trend_matrix, 2, median)
)

# Rolling average on the filtered data (>= 5 min games only)
jamir_filtered_rolling <- jamir_model_games |>
  mutate(
    game_number = row_number()
    , def_rating_roll = slide_dbl(
      defensive_rating, mean, .before = 4, .complete = FALSE, na.rm = TRUE
    )
  )

p4 <- ggplot() +
  geom_ribbon(
    data = pred_summary
    , aes(x = game_number, ymin = lower, ymax = upper)
    , fill = "#3B9AB2"
    , alpha = 0.2
  ) +
  geom_line(
    data = trend_summary
    , aes(x = game_number, y = median)
    , color = "#3B9AB2"
    , linewidth = 1
    , linetype = "dashed"
  ) +
  geom_point(
    data = jamir_filtered_rolling
    , aes(x = game_number, y = defensive_rating)
    , color = "grey60"
    , size = 1.5
    , alpha = 0.5
  ) +
  geom_line(
    data = jamir_filtered_rolling
    , aes(x = game_number, y = def_rating_roll)
    , color = "maroon"
    , linewidth = 1.2
  ) +
  labs(
    title = "Jamir Watkins' <span style='color:maroon;'>rolling defensive rating</span> with <span style='color:#3B9AB2;'>model-estimated trend</span>"
    , subtitle = "5-game rolling average (solid maroon) with individual games (grey dots)\nBlue ribbon shows 80% posterior predictive interval; dashed line is the model's expected value"
    , x = "Game Number (games with 5+ minutes)"
    , y = "Defensive Rating (lower = better)"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaidplot::usaid_plot() +
  theme(
    plot.title = element_markdown()
  )

ggsave("jamir_defense_trend.png", p4, width = 14, height = 10, dpi = 600, device = ragg::agg_png)


# Plot 5: Shot chart on court ----
# Draw a half court and plot Jamir's shots colored by make/miss

# Half court coordinates (simplified)
court_lines <- tibble(
  # outer boundary
  element = "boundary"
) |>
  bind_rows()

# Court drawing helper
draw_court <- function() {
  # All coordinates in feet (NBA court: 50 x 94, half court = 50 x 47)
  # Shot chart coords from NBA: loc_x in [-250, 250] (tenths of feet from center)
  # loc_y in [-50, 900+] (tenths of feet from baseline)

  circle_pts <- function(center_x, center_y, radius, n = 100) {
    angles <- seq(0, 2 * pi, length.out = n)
    tibble(x = center_x + radius * cos(angles), y = center_y + radius * sin(angles))
  }

  # Three point arc
  three_arc <- circle_pts(0, 0, 237.5, 200) |> filter(y >= 0)
  # Corner threes connect at y=0
  three_left <- tibble(x = c(-220, -220), y = c(-47.5, 0))
  three_right <- tibble(x = c(220, 220), y = c(-47.5, 0))

  # Paint
  paint <- tibble(
    x = c(-80, -80, 80, 80)
    , y = c(-47.5, 190 - 47.5, 190 - 47.5, -47.5)
  )

  # Free throw circle
  ft_circle <- circle_pts(0, 190 - 47.5, 60, 100)

  # Restricted area
  ra <- circle_pts(0, 0, 40, 100) |> filter(y >= 0)

  # Hoop

  hoop <- circle_pts(0, 0, 7.5, 50)

  # Backboard
  bb <- tibble(x = c(-30, 30), y = c(-7.5, -7.5))

  # Outer box
  outer <- tibble(
    x = c(-250, -250, 250, 250, -250)
    , y = c(-47.5, 422.5, 422.5, -47.5, -47.5)
  )

  list(
    geom_path(data = outer, aes(x = x, y = y), color = "black", linewidth = 0.3)
    , geom_path(data = three_arc, aes(x = x, y = y), color = "black", linewidth = 0.3)
    , geom_path(data = three_left, aes(x = x, y = y), color = "black", linewidth = 0.3)
    , geom_path(data = three_right, aes(x = x, y = y), color = "black", linewidth = 0.3)
    , geom_path(data = paint, aes(x = x, y = y), color = "black", linewidth = 0.3)
    , geom_path(data = ft_circle |> filter(y >= 190 - 47.5), aes(x = x, y = y)
                , color = "black", linewidth = 0.3)
    , geom_path(data = ft_circle |> filter(y < 190 - 47.5), aes(x = x, y = y)
                , color = "black", linewidth = 0.3, linetype = "dashed")
    , geom_path(data = ra, aes(x = x, y = y), color = "black", linewidth = 0.3)
    , geom_path(data = hoop, aes(x = x, y = y), color = "black", linewidth = 0.3)
    , geom_segment(data = bb, aes(x = x[1], xend = x[2], y = y[1], yend = y[2])
                   , color = "black", linewidth = 0.5)
  )
}

# Prep shot chart data
shot_plot_df <- shot_chart_jamir |>
  mutate(
    loc_x = as.numeric(loc_x)
    , loc_y = as.numeric(loc_y)
    , made = as.numeric(shot_made_flag) == 1
    , zone = shot_zone_basic
  ) |>
  filter(!is.na(loc_x) & !is.na(loc_y))

p5 <- ggplot(shot_plot_df, aes(x = loc_x, y = loc_y)) +
  stat_density_2d(
    aes(fill = after_stat(density))
    , geom = "raster"
    , contour = FALSE
    , n = 200
  ) +
  draw_court() +
  geom_point(
    aes(color = made)
    , size = 2
    , alpha = 0.6
    , shape = 16
  ) +
  coord_fixed(
    xlim = c(-250, 250)
    , ylim = c(-50, 300)
    , clip = "off"
  ) +
  labs(
    title = "Jamir Watkins Shooting Heat Map"
    , subtitle = "<span style='color:maroon;'>Hot zones</span> reveal where Jamir does his damage — paint-first with a developing mid-range game"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaidplot::usaid_plot(data_type = "continuous") +
  scale_fill_gradientn(
    colours = c("transparent", "#3B9AB2", "#F2E205", "maroon")
    , values = scales::rescale(c(0, 0.0001, 0.0005, 0.002))
    , guide = "none"
  ) +
  scale_color_manual(
    values = c("TRUE" = "white", "FALSE" = "grey30")
    , labels = c("TRUE" = "Made", "FALSE" = "Missed")
    , name = NULL
  ) +
  theme(
    plot.subtitle = element_markdown()
    , axis.text = element_blank()
    , axis.title = element_blank()
    , axis.ticks = element_blank()
    , panel.grid = element_blank()
    , legend.position = "none"
    , plot.background = element_rect(fill = "white", color = NA)
    , panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("jamir_shot_chart.png", p5, width = 12, height = 10, dpi = 600, device = ragg::agg_png)


# Plot 6: League peer comparison (STL/36 vs BLK/36 scatter) ----
# Traditional stats have STL, BLK, PLAYER_POSITION; join DEF_RATING from advanced

league_peers <- league_trad |>
  mutate(
    across(any_of(c("min", "gp", "stl", "blk")), as.numeric)
    , player_id = as.numeric(player_id)
  ) |>
  left_join(
    league_adv |>
      mutate(player_id = as.numeric(player_id), def_rating = as.numeric(def_rating)) |>
      dplyr::select(player_id, def_rating)
    , by = "player_id"
  ) |>
  filter(
    !is.na(def_rating)
    , !is.na(stl)
    , gp >= 15
    , min >= 12
  ) |>
  mutate(
    stl_per36 = stl / min * 36
    , blk_per36 = blk / min * 36
    , is_jamir  = player_id == jamir_id
  )

# Percentile summary (for blog narrative)
jamir_peer_row <- league_peers |> filter(is_jamir) |> slice_head(n = 1)

if (nrow(jamir_peer_row) > 0) {
  stl_pct <- mean(league_peers$stl_per36 <= jamir_peer_row$stl_per36, na.rm = TRUE) * 100
  blk_pct <- mean(league_peers$blk_per36 <= jamir_peer_row$blk_per36, na.rm = TRUE) * 100
  def_pct <- mean(league_peers$def_rating >= jamir_peer_row$def_rating, na.rm = TRUE) * 100

  message("\n--- Jamir Watkins Peer Percentiles (", nrow(league_peers), " qualifying NBA players) ---")
  message("  STL/36: ", round(jamir_peer_row$stl_per36, 2),
          " (", round(stl_pct, 1), "th percentile)")
  message("  BLK/36: ", round(jamir_peer_row$blk_per36, 2),
          " (", round(blk_pct, 1), "th percentile)")
  message("  DEF_RATING: ", round(jamir_peer_row$def_rating, 1),
          " (", round(def_pct, 1), "th percentile, lower = better)")
} else {
  message("WARNING: Jamir not found in league peer filter — check position / minutes filter")
}

avg_stl_per36 <- mean(league_peers$stl_per36, na.rm = TRUE)
avg_blk_per36 <- mean(league_peers$blk_per36, na.rm = TRUE)

highlight_names <- c(
  "Victor Wembanyama", "Alex Caruso", "Ausar Thompson", "Jalen Suggs", "Scottie Barnes"
  , "Alex Sarr", "Bilal Coulibaly"
)

league_peers <- league_peers |>
  mutate(
    dot_type = case_when(
      is_jamir                          ~ "Watkins"
      , player_name %in% highlight_names ~ "Named"
      , TRUE                             ~ "Other"
    )
  )

# Helper to build the peer scatter — reused for both versions
make_peer_plot <- function(df, subtitle_text) {
  avg_stl <- mean(df$stl_per36, na.rm = TRUE)
  avg_blk <- mean(df$blk_per36, na.rm = TRUE)
  ggplot(df, aes(x = stl_per36, y = blk_per36)) +
    geom_vline(xintercept = avg_stl, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = avg_blk, linetype = "dashed", color = "grey50") +
    geom_point(
      aes(size = min, color = dot_type, alpha = dot_type)
      , show.legend = FALSE
    ) +
    usaid_plot() +
    scale_color_manual(values = c("Other" = "grey70", "Named" = "#468499", "Watkins" = "#990000")) +
    scale_alpha_manual(values = c("Other" = 0.5, "Named" = 1, "Watkins" = 1)) +
    scale_size_continuous(range = c(1, 10)) +
    geom_text_repel(
      data = filter(df, dot_type != "Other")
      , aes(label = player_name, color = dot_type)
      , fontface = "bold", size = 4, show.legend = FALSE
    ) +
    labs(
      title    = "Watkins among NBA players: steals & blocks per 36 minutes"
      , subtitle = subtitle_text
      , x = "Steals per 36 min"
      , y = "Blocks per 36 min"
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    )
}

p6 <- make_peer_plot(
  league_peers
  , "2025-26 regular season · players with 15+ GP and 12+ MPG · dot size = minutes/game"
)
ggsave("jamir_league_context.png", p6, width = 12, height = 9, dpi = 600, device = ragg::agg_png)

# Unrestricted version — all players with any minutes
league_peers_all <- league_trad |>
  mutate(across(any_of(c("min", "gp", "stl", "blk")), as.numeric), player_id = as.numeric(player_id)) |>
  left_join(
    league_adv |>
      mutate(player_id = as.numeric(player_id), def_rating = as.numeric(def_rating)) |>
      dplyr::select(player_id, def_rating)
    , by = "player_id"
  ) |>
  filter(!is.na(stl), !is.na(min), min > 0) |>
  mutate(
    stl_per36 = stl / min * 36
    , blk_per36 = blk / min * 36
    , is_jamir = player_id == jamir_id
    , dot_type = case_when(
        is_jamir ~ "Watkins"
        , player_name %in% highlight_names ~ "Named"
        , TRUE ~ "Other"
      )
  )

p6_all <- make_peer_plot(
  league_peers_all
  , "2025-26 regular season · all players · dot size = minutes/game"
)
ggsave("jamir_league_context_all.png", p6_all, width = 12, height = 9, dpi = 600, device = ragg::agg_png)


# Plot 7: Hustle stats context ----

hustle_peers <- hustle_stats |>
  filter(min >= 12) |>
  {function(df) if ("gp" %in% names(df)) filter(df, gp >= 15) else df}()

jamir_hustle <- hustle_peers |>
  filter(as.numeric(player_id) == jamir_id) |>
  slice_head(n = 1)

if (nrow(jamir_hustle) > 0) {
  hustle_metrics <- intersect(
    c("deflections", "charges_drawn", "contested_shots_2pt", "contested_shots_3pt")
    , names(hustle_peers)
  )

  if (length(hustle_metrics) > 0) {
    hustle_pcts <- hustle_peers |>
      summarise(across(
        all_of(hustle_metrics)
        , ~ mean(as.numeric(.x) <= as.numeric(jamir_hustle[[cur_column()]]), na.rm = TRUE) * 100
        , .names = "{.col}_pct"
      )) |>
      pivot_longer(everything(), names_to = "metric", values_to = "percentile") |>
      mutate(
        metric = str_remove(metric, "_pct$") |>
          str_replace_all("_", " ") |>
          str_to_title()
      )

    hustle_pcts |>
      mutate(percentile = round(percentile, 1)) 

    p7 <- ggplot(hustle_pcts, aes(x = percentile, y = reorder(metric, percentile))) +
      geom_col(fill = "#172869FF", width = 0.6) +
      geom_vline(xintercept = 50, linetype = "dashed", color = "grey50") +
      scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "th")) +
      labs(
        title   = "Watkins' hustle metrics rank among NBA players with 12+ MPG"
        , subtitle = "2025-26 regular season percentile among qualifying players"
        , x = "Percentile rank"
        , y = NULL
        , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
      ) +
      usaid_plot()

    ggsave("jamir_hustle_stats.png", p7, width = 10, height = 7, dpi = 600, device = ragg::agg_png)
  } else {
    message("WARNING: No hustle metrics found in hustle_stats — skipping Plot 7")
  }
} else {
  message("WARNING: Jamir not found in hustle peers — skipping Plot 7")
}


#  Wraping up---

jamir_theta_summary <- theta_summary |> filter(player_index == jamir_idx)
jamir_def_rank <- which(theta_summary$player_index == jamir_idx)
n_players_model <- nrow(theta_summary)
prob_below_avg <- mean(theta_draws$theta[theta_draws$player_index == jamir_idx] < league_avg_def)
jamir_on_off_rank <- which(on_off_rankings$vs_player_name == jamir_on_off$vs_player_name)

jamir_ra <- jamir_zones |> filter(shot_zone_basic == "Restricted Area")
team_ra <- team_zones |> filter(shot_zone_basic == "Restricted Area")
jamir_paint_nra <- jamir_zones |> filter(shot_zone_basic == "In The Paint (Non-RA)")
team_paint_nra <- team_zones |> filter(shot_zone_basic == "In The Paint (Non-RA)")

jamir_trend_prob_improving <- mean(
  trend_draws$beta_trend[trend_draws$player_index == jamir_idx] < 0
)

mpg_lookup_rec1 <- wizards_adv |>
  mutate(
    min_num = as.numeric(min)
    , last_nm = str_extract(player_name, "\\S+$") |> str_trim()
  ) |>
  group_by(last_nm) |>
  slice_head(n = 1) |>
  ungroup() |>
  dplyr::select(last_nm, min_num)

on_off_rec1 <- on_off_summary |>
  filter(!is.na(net_rating_ON), !is.na(net_rating_OFF)) |>
  mutate(
    player_display = str_replace(vs_player_name, "^(.+),\\s*(.+)$", "\\2 \\1") |> str_trim()
    , last_nm       = str_extract(vs_player_name, "^[^,]+") |> str_trim()
    , is_jamir      = str_detect(vs_player_name, "Watkins")
  ) |>
  left_join(mpg_lookup_rec1, by = "last_nm") |>
  filter(!is.na(min_num), min_num > 0)

p_rec1 <- ggplot(on_off_rec1, aes(x = min_num, y = def_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(
    aes(color = is_jamir, size = abs(def_diff))
    , alpha = 0.8
    , show.legend = FALSE
  ) +
  geom_text_repel(
    data = filter(on_off_rec1, !is_jamir)
    , aes(label = player_display)
    , size = 3.5, color = "grey45"
    , show.legend = FALSE
  ) +
  geom_text_repel(
    data = filter(on_off_rec1, is_jamir)
    , aes(label = player_display)
    , size = 4.5, color = "#172869FF", fontface = "bold"
    , show.legend = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "#172869FF", "FALSE" = "grey55")) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title   = "Watkins delivers elite defensive impact relative to his minutes"
    , subtitle = "Wizards 2025-26 · negative = team defends better when player is on court"
    , x = "Minutes per game"
    , y = "Defensive rating: ON minus OFF (negative = better)"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaid_plot()

ggsave("jamir_rec1_minutes.png", p_rec1, width = 12, height = 9, dpi = 600, device = ragg::agg_png)
message("Saved jamir_rec1_minutes.png")


# --hustle percentile chart, 3pt contest bar highlighted ----

if (exists("hustle_pcts") && nrow(hustle_pcts) > 0) {
  hustle_rec3 <- hustle_pcts |>
    mutate(
      is_gap    = str_detect(str_to_lower(metric), "3pt|3 pt|three")
      , bar_color = if_else(is_gap, "#D9565CFF", "#172869FF")
    )

  p_rec3 <- ggplot(hustle_rec3, aes(x = percentile, y = reorder(metric, percentile))) +
    geom_col(aes(fill = bar_color), width = 0.6, show.legend = FALSE) +
    geom_text(
      aes(label = paste0(round(percentile, 0), "th"))
      , hjust = -0.1, size = 3.5
    ) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "grey50") +
    scale_fill_identity() +
    scale_x_continuous(limits = c(0, 110), labels = function(x) paste0(x, "th")) +
    labs(
      title   = "Strong hustle across the board — <span style='color:#D9565CFF;'>3-point contesting is the gap</span>"
      , subtitle = "2025-26 · percentile rank among NBA players with 12+ MPG"
      , x = "Percentile rank"
      , y = NULL
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    ) +
    usaid_plot() +
    theme(plot.title = element_markdown())

  ggsave("jamir_rec3_hustle.png", p_rec3, width = 10, height = 7, dpi = 600, device = ragg::agg_png)
  message("Saved jamir_rec3_hustle.png")
} else {
  message("Skipping jamir_rec3_hustle.png — hustle_pcts not available")
}


# -- watkins vs Coulibaly head-to-head percentile comparison ----

coulibaly_pat <- "Coulibaly"

comp_peers <- league_peers |>
  filter(is_jamir | str_detect(player_name, coulibaly_pat)) |>
  dplyr::select(player_name, stl_per36, blk_per36)

if (nrow(comp_peers) >= 2) {
  comp_pcts <- comp_peers |>
    mutate(
      stl_pct = map_dbl(stl_per36, ~ mean(league_peers$stl_per36 <= .x, na.rm = TRUE) * 100)
      , blk_pct = map_dbl(blk_per36, ~ mean(league_peers$blk_per36 <= .x, na.rm = TRUE) * 100)
    )

  if (exists("hustle_peers") && nrow(hustle_peers) > 0 && "deflections" %in% names(hustle_peers)) {
    defl_pool <- as.numeric(hustle_peers$deflections)
    hustle_comp4 <- hustle_peers |>
      filter(str_detect(player_name, "Watkins") | str_detect(player_name, coulibaly_pat)) |>
      mutate(defl_pct = map_dbl(
        as.numeric(deflections)
        , ~ mean(defl_pool <= .x, na.rm = TRUE) * 100
      )) |>
      dplyr::select(player_name, defl_pct)
    comp_pcts <- comp_pcts |> left_join(hustle_comp4, by = "player_name")
  } else {
    comp_pcts <- comp_pcts |> mutate(defl_pct = NA_real_)
  }

  rec4_long <- comp_pcts |>
    dplyr::select(player_name, stl_pct, blk_pct, defl_pct) |>
    pivot_longer(-player_name, names_to = "metric", values_to = "percentile") |>
    filter(!is.na(percentile)) |>
    mutate(
      metric_label = case_when(
        metric == "stl_pct"  ~ "Steals / 36 min"
        , metric == "blk_pct"  ~ "Blocks / 36 min"
        , metric == "defl_pct" ~ "Deflections / game"
        , TRUE ~ metric
      )
      , is_jamir = str_detect(player_name, "Watkins")
    )

  p_rec4 <- ggplot(rec4_long, aes(x = percentile, y = metric_label, fill = is_jamir)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.55, show.legend = FALSE) +
    geom_text(
      aes(label = paste0(round(percentile, 0), "th"))
      , position = position_dodge(width = 0.65)
      , hjust = -0.1, size = 3.5
    ) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = c("TRUE" = "#172869FF", "FALSE" = "#D9565CFF")) +
    scale_x_continuous(limits = c(0, 115), labels = function(x) paste0(x, "th")) +
    labs(
      title   = "<span style='color:#172869FF;'>Watkins</span> and <span style='color:#D9565CFF;'>Coulibaly</span> are nearly identical defenders"
      , subtitle = "2025-26 · percentile rank among NBA players with 15+ GP and 12+ MPG"
      , x = "Percentile rank"
      , y = NULL
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    ) +
    usaid_plot() +
    theme(plot.title = element_markdown())

  ggsave("jamir_rec4_vs_coulibaly.png", p_rec4, width = 11, height = 7, dpi = 600, device = ragg::agg_png)
  message("Saved jamir_rec4_vs_coulibaly.png")
} else {
  message("Skipping jamir_rec4_vs_coulibaly.png — Coulibaly not found in peer data")
}


# --Sarr / Coulibaly / Watkins defensive core grouped bar ----

sarr_pat <- "Sarr"

core_players <- league_peers |>
  filter(
    is_jamir
    | str_detect(player_name, coulibaly_pat)
    | str_detect(player_name, sarr_pat)
  ) |>
  dplyr::select(player_name, stl_per36, blk_per36)

if (nrow(core_players) >= 2) {
  if (exists("hustle_peers") && nrow(hustle_peers) > 0 && "deflections" %in% names(hustle_peers)) {
    hustle_core <- hustle_peers |>
      filter(
        str_detect(player_name, "Watkins")
        | str_detect(player_name, coulibaly_pat)
        | str_detect(player_name, sarr_pat)
      ) |>
      dplyr::select(player_name, deflections) |>
      mutate(deflections = as.numeric(deflections))
    core_players <- core_players |> left_join(hustle_core, by = "player_name")
  } else {
    core_players <- core_players |> mutate(deflections = NA_real_)
  }

  core_long <- core_players |>
    pivot_longer(-player_name, names_to = "metric", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(
      metric_label = case_when(
        metric == "stl_per36"   ~ "Steals / 36 min"
        , metric == "blk_per36"   ~ "Blocks / 36 min"
        , metric == "deflections" ~ "Deflections / game"
      )
      , last_name = str_extract(player_name, "\\S+$")
    )

  p_rec5 <- ggplot(core_long, aes(x = metric_label, y = value, fill = last_name)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.65) +
    geom_text(
      aes(label = round(value, 2))
      , position = position_dodge(width = 0.72)
      , vjust = -0.4, size = 3.5
    ) +
    scale_fill_manual(
      values = c("Sarr" = "#3B9AB2", "Coulibaly" = "#D9565CFF", "Watkins" = "#172869FF")
      , name = NULL
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title   = "Washington's emerging defensive core: three complementary profiles"
      , subtitle = "2025-26 · Sarr deters at the rim, Coulibaly & Watkins generate plays on the perimeter"
      , x = NULL
      , y = "Per-game or per-36-minute rate"
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    ) +
    usaid_plot()

  ggsave("jamir_rec5_defensive_core.png", p_rec5, width = 12, height = 8, dpi = 600, device = ragg::agg_png)
  message("Saved jamir_rec5_defensive_core.png")
} else {
  message("Skipping jamir_rec5_defensive_core.png — fewer than 2 core players found in peer data")
}


#  sandbox on posteriors ----

# Shared prep ----

# Player order by median theta (best defenders first / left)
theta_order_vec <- theta_summary |> arrange(mean) |> pull(player_name)

# Active roster filter for swarm plot
active_roster <- c(
  "Carrington", "Riley", "Champagnie", "Tre Johnson", "Coulibaly"
  , "Sarr", "Vukcevic", "Gill", "Watkins", "Cooper", "Hardy", "Young", "Reese"
)

# Active roster order: keep theta_order_vec ranking but only active players
active_theta_order <- theta_order_vec[
  str_detect(theta_order_vec, paste(active_roster, collapse = "|"))
]

# Thinned posterior draws (150 per player keeps the plots fast)
set.seed(202)
theta_viz <- theta_draws |>
  filter(str_detect(player_name, paste(active_roster, collapse = "|"))) |>
  mutate(player_name = factor(player_name, levels = active_theta_order)) |>
  filter(!is.na(player_name)) |>
  group_by(player_name) |>
  slice_sample(n = 150) |>
  ungroup() |>
  mutate(
    is_jamir  = player_name == "Jamir Watkins"
    , dot_type = case_when(
        is_jamir               ~ "Watkins"
        , theta < league_avg_def ~ "Better"
        , TRUE                   ~ "Worse"
      )
  )

# Observed game data (for Plot B)
game_viz <- model_df |>
  left_join(player_lookup, by = c("person_id", "player_name")) |>
  mutate(
    player_name = factor(player_name, levels = theta_order_vec)
    , is_jamir  = person_id == jamir_id
  ) |>
  filter(!is.na(player_name))

theta_interval_df <- theta_summary |>
  mutate(
    player_name = factor(player_name, levels = theta_order_vec)
    , is_jamir  = player_name == "Jamir Watkins"
  )

# Player-level beta_trend summary (trend_summary was overwritten in Plot 4)
player_trend_summary <- fit$summary("beta_trend") |>
  mutate(player_index = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(player_lookup, by = "player_index")


p_swarm <- ggplot(theta_viz, aes(x = theta, y = player_name, color = dot_type)) +
    geom_vline(xintercept = league_avg_def, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    annotate("text", x = league_avg_def + 0.15, y = 0.6,
             label = "League avg", hjust = 0, size = 3, color = "grey40") +
    geom_quasirandom(
      aes(alpha = dot_type)
      , size = 0.65
      , orientation = "y"
      , show.legend = FALSE
    ) +
    scale_color_manual(values = c("Watkins" = "#172869FF", "Better" = "#3B9AB2", "Worse" = "grey65")) +
    scale_alpha_manual(values = c("Watkins" = 0.85, "Better" = 0.4, "Worse" = 0.2)) +
    scale_y_discrete() +
    labs(
      title    = "Posterior defensive rating estimates — all Wizards players"
      , subtitle = "Each dot = one posterior draw (150 shown per player)\n<span style='color:#172869FF;'>Watkins</span> · <span style='color:#3B9AB2;'>better than league avg</span> · <span style='color:grey60;'>worse than league avg</span>"
      , x      = "Estimated defensive rating (lower = better)"
      , y      = NULL
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    ) +
    usaid_plot() +
    theme(plot.subtitle = element_markdown())

  ggsave("jamir_posterior_swarm.png", p_swarm, width = 14, height = 10, dpi = 600, device = ragg::agg_png)


p_noise_signal <- ggplot() +
  geom_vline(xintercept = league_avg_def, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_jitter(
    data = game_viz
    , aes(x = defensive_rating, y = player_name)
    , color = "grey72", size = 0.9, alpha = 0.45, height = 0.3
    , show.legend = FALSE
  ) +
  geom_pointrange(
    data = theta_interval_df
    , aes(x = mean, xmin = q5, xmax = q95, y = player_name, color = is_jamir)
    , linewidth = 1.1, fatten = 2.5
    , show.legend = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "#172869FF", "FALSE" = "#D9565CFF")) +
  scale_y_discrete() +
  labs(
    title    = "The noise vs. the signal"
    , subtitle = "<span style='color:grey60;'>Each grey dot is one game</span> · <span style='color:#D9565CFF;'>Red: model estimate (90% interval)</span> · <span style='color:#172869FF;'>Watkins in blue</span>"
    , x      = "Defensive rating (lower = better)"
    , y      = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaid_plot() +
  theme(plot.subtitle = element_markdown())

ggsave("jamir_data_vs_model.png", p_noise_signal, width = 14, height = 10, dpi = 600, device = ragg::agg_png)

two_d_df <- theta_summary |>
  dplyr::select(player_name, player_index, theta_mean = mean, theta_q5 = q5, theta_q95 = q95) |>
  left_join(
    player_trend_summary |>
      dplyr::select(player_index, trend_mean = mean, trend_q5 = q5, trend_q95 = q95)
    , by = "player_index"
  ) |>
  left_join(
    wizards_adv |>
      mutate(min_num = as.numeric(min)) |>
      dplyr::select(player_name, min_num)
    , by = "player_name"
  ) |>
  mutate(
    is_jamir   = player_name == "Jamir Watkins"
    , label    = if_else(
        is_jamir
        | theta_mean == min(theta_mean, na.rm = TRUE)
        | abs(trend_mean) > quantile(abs(trend_mean), 0.75, na.rm = TRUE)
        , player_name, NA_character_
      )
  )

team_theta_mid <- mean(two_d_df$theta_mean, na.rm = TRUE)

p_2d <- ggplot(two_d_df, aes(x = theta_mean, y = trend_mean)) +
  annotate("rect"
           , xmin = -Inf, xmax = team_theta_mid, ymin = -Inf, ymax = 0
           , fill = "#3B9AB2", alpha = 0.05) +
  annotate("text"
           , x = min(two_d_df$theta_mean, na.rm = TRUE) + 0.05, y = -0.05
           , label = "Better & improving", hjust = 0, vjust = 1
           , size = 3, color = "#3B9AB2", fontface = "italic") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = team_theta_mid, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = trend_q5, ymax = trend_q95, color = is_jamir)
    , width = 0.04, alpha = 0.2, show.legend = FALSE
  ) +
  geom_errorbarh(
    aes(xmin = theta_q5, xmax = theta_q95, color = is_jamir)
    , height = 0.04, alpha = 0.2, show.legend = FALSE
  ) +
  geom_point(
    aes(color = is_jamir, size = coalesce(min_num, 10))
    , alpha = 0.85, show.legend = FALSE
  ) +
  geom_text_repel(
    aes(label = label, color = is_jamir)
    , size = 3.5, na.rm = TRUE, show.legend = FALSE, seed = 202
  ) +
  scale_color_manual(values = c("TRUE" = "#172869FF", "FALSE" = "grey50")) +
  scale_size_continuous(range = c(2, 6)) +
  scale_y_continuous() +
  labs(
    title    = "Where each Wizard stands: defensive level vs. trajectory"
    , subtitle = "x = estimated defensive rating (lower = better) · y = seasonal trend (negative = improving over season)\ndashed lines at team averages · dot size = minutes/game · error bars = 90% credible intervals"
    , x      = "Estimated defensive rating (lower = better)"
    , y      = "Defensive trend (negative = improving)"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaid_plot()

ggsave("jamir_2d_defense.png", p_2d, width = 14, height = 10, dpi = 600, device = ragg::agg_png)


theta_halfeye <- theta_draws |>
  filter(str_detect(player_name, paste(active_roster, collapse = "|"))) |>
  mutate(
    player_name = factor(player_name, levels = active_theta_order)
    , is_jamir  = player_name == "Jamir Watkins"
  ) |>
  filter(!is.na(player_name))

p_halfeye <- ggplot(theta_halfeye, aes(x = theta, y = player_name)) +
  stat_halfeye(
    aes(fill = is_jamir)
    , .width = c(0.5, 0.8, 0.95)
    , show.legend = FALSE
  ) +
  geom_vline(xintercept = league_avg_def, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  annotate(
    "text", x = league_avg_def + 0.1, y = 0.6
    , label = "League avg", hjust = 0, size = 3, color = "grey40"
  ) +
  usaid_plot() +
  scale_fill_manual(values = c("TRUE" = "#468499", "FALSE" = "#c6e2ff")) +
  scale_y_discrete() +
  labs(
    title    = "Wizards model adjusted defensive rating"
    , subtitle = "Even after accounting for the game number and weighting by minutes, Jamir Watkins comes out on top"
    , x      = "Estimated defensive rating (lower = better)"
    , y      = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("jamir_posterior_halfeye.png", p_halfeye, width = 14, height = 10, dpi = 600, device = ragg::agg_png)
