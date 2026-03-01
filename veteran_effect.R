###############################################################################
# Veteran Effect on Young Player Development
# Does veteran presence on NBA rosters accelerate young player improvement?#
# R version 4.4.2 (2024-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sequoia
###############################################################################

# Set up, the usual----

library(hoopR)
library(tidyverse)
library(cmdstanr)
library(loo)
library(tidybayes)
library(usaidplot)
library(ggridges)
library(ggtext)
library(janitor)
library(bayesplot)
library(ggrepel)

set.seed(202)

# do this to avoid issues
extrafont::loadfonts(quiet = TRUE)

# expand buffer for hoopR downloads
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 3)

# Wizards team ID
wizards_id <- "1610612764"

# seasons to pull
seasons <- 2010:2025

# check for cached data first
# setting this up to avoid having to run a bunch of times because it's a major pain
if (file.exists("veteran_effect_data.rds")) {

  cached <- readRDS("veteran_effect_data.rds")
  adv_all <- cached$adv_all
  base_all <- cached$base_all
  player_info <- cached$player_info

} else {


  # Advanced stats (net rating, off/def rating, etc.)
  adv_list <- list()
  for (i in seq_along(seasons)) {
    season_str <- paste0(seasons[i], "-", substr(seasons[i] + 1, 3, 4))
    message("Pulling advanced stats for ", season_str)

    result <- tryCatch({
      stats <- nba_leaguedashplayerstats(
        season = season_str
        , season_types = "Regular Season"
        , per_mode = "PerGame"
        , measure_type = "Advanced"
      )
      stats_data <- stats$LeagueDashPlayerStats
      stats_data$season_start <- seasons[i]
      stats_data$season_str <- season_str
      stats_data
    }, error = function(e) {
      message("  Skipping ", season_str, ": ", e$message)
      NULL
    })

    if (!is.null(result)) adv_list[[length(adv_list) + 1]] <- result
    Sys.sleep(3)
  }

  adv_all <- bind_rows(adv_list) |> clean_names()

  # Base/Totals stats (minutes totals for vet exposure)
  base_list <- list()
  for (i in seq_along(seasons)) {
    season_str <- paste0(seasons[i], "-", substr(seasons[i] + 1, 3, 4))
    message("Pulling base totals for ", season_str)

    result <- tryCatch({
      stats <- nba_leaguedashplayerstats(
        season = season_str
        , season_types = "Regular Season"
        , per_mode = "Totals"
        , measure_type = "Base"
      )
      stats_data <- stats$LeagueDashPlayerStats
      stats_data$season_start <- seasons[i]
      stats_data$season_str <- season_str
      stats_data
    }, error = function(e) {
      message("  Skipping ", season_str, ": ", e$message)
      NULL
    })

    if (!is.null(result)) base_list[[length(base_list) + 1]] <- result
    Sys.sleep(3)
  }

  base_all <- bind_rows(base_list) |> clean_names()

  # Player index for draft info and career start
  player_info <- nba_playerindex(historical = 1)$PlayerIndex |>
    clean_names() |>
    mutate(
      from_year = as.numeric(from_year)
      , draft_number = as.numeric(draft_number)
      , draft_round = as.numeric(draft_round)
      , draft_round_char = case_when(
        draft_round == 1 ~ "First Round"
        , draft_round == 2 ~ "Second Round"
        , TRUE ~ "Undrafted"
      )
    )

  # cache everything
  saveRDS(
    list(
      adv_all = adv_all
      , base_all = base_all
      , player_info = player_info
    )
    , "veteran_effect_data.rds"
  )

  message("Data cached to veteran_effect_data.rds")
}

# Pull game logs for accurate per-team minutes attribution
# nba_leaguedashplayerstats aggregates traded players under their final team,
# so we use game logs to correctly split minutes by team 
# this ensures we get guys like CJ on the Wizards becuase the API
# will bucket him to the Hawks otherwise
if (file.exists("veteran_effect_gamelogs.rds")) {

  message("Loading cached game logs...")
  gamelog_all <- readRDS("veteran_effect_gamelogs.rds")

} else {

  message("Pulling game logs from NBA API (for per-team minute attribution)...")
  gamelog_list <- list()
  for (i in seq_along(seasons)) {
    season_str <- paste0(seasons[i], "-", substr(seasons[i] + 1, 3, 4))
    message("Pulling game logs for ", season_str)

    result <- tryCatch({
      gl <- nba_leaguegamelog(
        season = season_str
        , season_type = "Regular Season"
        , player_or_team = "P"
      )
      gl_data <- gl$LeagueGameLog
      gl_data$season_start <- seasons[i]
      gl_data
    }, error = function(e) {
      message("  Skipping ", season_str, ": ", e$message)
      NULL
    })

    if (!is.null(result)) gamelog_list[[length(gamelog_list) + 1]] <- result
    Sys.sleep(3)
  }

  gamelog_all <- bind_rows(gamelog_list) |> clean_names()
  saveRDS(gamelog_all, "veteran_effect_gamelogs.rds")
  message("Game logs cached to veteran_effect_gamelogs.rds")
}


# clean up ----

# clean up advanced stats
adv_df <- adv_all |>
  mutate(
    player_id = as.character(player_id)
    , across(
      any_of(c("min", "net_rating", "off_rating", "def_rating"
               , "ts_pct", "efg_pct", "pace", "gp", "w", "l"))
      , as.numeric
    )
  )

# clean up base totals (used for panel join; veteran exposure now uses game logs)
base_df <- base_all |>
  mutate(
    player_id = as.character(player_id)
    , min = as.numeric(min)
    , gp = as.numeric(gp)
  ) |>
  select(player_id, team_id, team_abbreviation, season_start
         , total_min = min, gp_total = gp)

# merge draft info for career season calculation
player_draft <- player_info |>
  select(person_id, from_year, draft_round, draft_number
         , draft_round_char, position) |>
  rename(player_id = person_id, rookie_year = from_year)

# build the full player-season panel
panel <- adv_df |>
  left_join(player_draft, by = "player_id") |>
  left_join(
    base_df |> select(player_id, team_id, season_start, total_min)
    , by = c("player_id", "team_id", "season_start")
  )

# compute career season per player
# use rookie_year from player index for accurate career season
panel <- panel |>
  arrange(player_id, season_start) |>
  mutate(
    career_season = ifelse(
      !is.na(rookie_year)
      , season_start - rookie_year + 1
      , NA_real_
    )
  ) |>
  # for players missing rookie_year, fall back to row_number within data
  group_by(player_id) |>
  mutate(
    career_season = ifelse(is.na(career_season), row_number(), career_season)
    , first_season = min(season_start)
  ) |>
  ungroup()

# tag player types
panel <- panel |>
  mutate(
    player_type = case_when(
      career_season <= 4 ~ "young"
      , career_season >= 8 ~ "veteran"
      , TRUE ~ "mid_career"
    )
  )

# compute team-season veteran exposure FROM GAME LOGS
# game logs correctly attribute minutes to the team each player was on per game,
# which handles mid-season trades (e.g., CJ McCollum WAS→ATL gets minutes
# counted for both teams proportionally)

gamelog_df <- gamelog_all |>
  mutate(
    player_id = as.character(player_id)
    , team_id = as.character(team_id)
    , min_played = suppressWarnings(as.numeric(min))
  ) |>
  filter(!is.na(min_played) & min_played > 0) |>
  group_by(player_id, team_id, season_start) |>
  summarise(
    total_min = sum(min_played, na.rm = TRUE)
    , .groups = "drop"
  )

# join with career info to tag player types
gamelog_df <- gamelog_df |>
  left_join(
    player_draft |> select(player_id, rookie_year)
    , by = "player_id"
  ) |>
  mutate(
    career_season = ifelse(
      !is.na(rookie_year)
      , season_start - rookie_year + 1
      , NA_real_
    )
    , player_type = case_when(
      career_season <= 4 ~ "young"
      , career_season >= 8 ~ "veteran"
      , !is.na(career_season) ~ "mid_career"
      , TRUE ~ NA_character_
    )
  )

# total minutes by team-season
team_minutes <- gamelog_df |>
  group_by(team_id, season_start) |>
  summarise(
    team_total_min = sum(total_min, na.rm = TRUE)
    , .groups = "drop"
  )

# veteran minutes by team-season
vet_minutes <- gamelog_df |>
  filter(player_type == "veteran") |>
  group_by(team_id, season_start) |>
  summarise(
    vet_total_min = sum(total_min, na.rm = TRUE)
    , .groups = "drop"
  )

# weighted average teammate experience by team-season 
team_experience <- gamelog_df |>
  filter(!is.na(career_season)) |>
  group_by(team_id, season_start) |>
  summarise(
    weighted_avg_exp = weighted.mean(career_season, w = total_min, na.rm = TRUE)
    , .groups = "drop"
  )

team_vet_exposure <- team_minutes |>
  left_join(vet_minutes, by = c("team_id", "season_start")) |>
  left_join(team_experience, by = c("team_id", "season_start")) |>
  mutate(
    vet_total_min = replace_na(vet_total_min, 0)
    , vet_min_share = vet_total_min / team_total_min
  )

# average vet minute share
mean(team_vet_exposure$vet_min_share, na.rm=T)

team_vet_exposure |> arrange(vet_min_share)

# team win% from game logs (deduplicate to team-game level)
team_win_pct <- gamelog_all |>
  mutate(team_id = as.character(team_id)) |>
  distinct(team_id, season_start, game_id, .keep_all = TRUE) |>
  group_by(team_id, season_start) |>
  summarise(
    team_win_pct = mean(wl == "W", na.rm = TRUE)
    , .groups = "drop"
  )

team_vet_exposure <- team_vet_exposure |>
  left_join(team_win_pct, by = c("team_id", "season_start"))

# Lagged win% (previous season) for causal identification
# controls for team quality confound without blocking the mediator path
# (this year's vets can't cause last year's wins)
team_win_pct_lag <- team_win_pct |>
  mutate(season_start = season_start + 1) |>
  rename(team_win_pct_lag = team_win_pct)

team_vet_exposure <- team_vet_exposure |>
  left_join(team_win_pct_lag, by = c("team_id", "season_start"))

# Veteran quality decomposition: split vet minutes by good/bad net rating
# if mentorship matters, BOTH good and bad vets should help
# if it's just good teammates, only good-vet minutes should matter
player_net_rating <- adv_df |>
  select(player_id, season_start, net_rating) |>
  filter(!is.na(net_rating))

vet_quality_df <- gamelog_df |>
  filter(player_type == "veteran") |>
  left_join(player_net_rating, by = c("player_id", "season_start")) |>
  filter(!is.na(net_rating))

good_vet_minutes <- vet_quality_df |>
  filter(net_rating >= 0) |>
  group_by(team_id, season_start) |>
  summarise(good_vet_min = sum(total_min, na.rm = TRUE), .groups = "drop")

bad_vet_minutes <- vet_quality_df |>
  filter(net_rating < 0) |>
  group_by(team_id, season_start) |>
  summarise(bad_vet_min = sum(total_min, na.rm = TRUE), .groups = "drop")

team_vet_exposure <- team_vet_exposure |>
  left_join(good_vet_minutes, by = c("team_id", "season_start")) |>
  left_join(bad_vet_minutes, by = c("team_id", "season_start")) |>
  mutate(
    good_vet_min = replace_na(good_vet_min, 0)
    , bad_vet_min = replace_na(bad_vet_min, 0)
    , good_vet_min_share = good_vet_min / team_total_min
    , bad_vet_min_share = bad_vet_min / team_total_min
  )

# build young player panel with year-over-year deltas
young_panel <- panel |>
  filter(player_type == "young" & !is.na(net_rating)) |>
  arrange(player_id, season_start) |>
  group_by(player_id) |>
  mutate(
    prev_net_rating = lag(net_rating)
    , prev_ts_pct = lag(ts_pct)
    , delta_net_rating = net_rating - prev_net_rating
    , delta_ts_pct = ts_pct - prev_ts_pct
  ) |>
  ungroup() |>
  filter(!is.na(delta_net_rating)) |>
  left_join(team_vet_exposure, by = c("team_id", "season_start")) |>
  filter(!is.na(vet_min_share))

# add team abbreviation labels
team_labels <- panel |>
  distinct(team_id, team_abbreviation) |>
  group_by(team_id) |>
  slice_tail(n = 1) |>
  ungroup()

young_panel <- young_panel |>
  left_join(
    team_labels |> select(team_id, team_abbrev = team_abbreviation)
    , by = "team_id"
  )

# create Wizards flag
young_panel <- young_panel |>
  mutate(
    is_wiz = team_id == wizards_id
    , wiz_label = ifelse(is_wiz, "Wizards", "Other Teams")
  )

# create vet exposure terciles
young_panel <- young_panel |>
  mutate(
    vet_tercile = ntile(vet_min_share, 3)
    , vet_tercile_label = case_when(
      vet_tercile == 1 ~ "Low Veteran Exposure"
      , vet_tercile == 2 ~ "Medium Veteran Exposure"
      , vet_tercile == 3 ~ "High Veteran Exposure"
    )
    , vet_tercile_label = factor(
      vet_tercile_label
      , levels = c("Low Veteran Exposure"
                   , "Medium Veteran Exposure"
                   , "High Veteran Exposure")
    )
  )


# Let's take a look

# Veteran exposure trend over time
# scatter of all teams, Wizards highlighted, league avg line

p1 <- team_vet_exposure |>
  left_join(team_labels, by = "team_id") |>
  mutate(is_wiz = team_id == wizards_id) |>
  ggplot(aes(x = season_start, y = vet_min_share)) +
  geom_point(
    aes(fill = is_wiz, size = is_wiz)
    , shape = 21, color = "white", alpha = 0.6
  ) +
  stat_summary(
    geom = "line", fun = "mean"
    , color = "#D9565CFF", linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  usaid_plot() +
  scale_fill_manual(
    values = c("TRUE" = "#172869FF", "FALSE" = "#CCCCCC")
    , guide = "none"
  ) +
  scale_size_manual(values = c("TRUE" = 5, "FALSE" = 2.5), guide = "none") +
  labs(
    x = ""
    , y = "Share of Team Minutes Played by Veterans (8+ seasons)"
    , title = "Veteran Minute Share Across the NBA Over Time"
    , subtitle = "<span style='color:#172869FF;'>**Wizards**</span> highlighted against all other teams, with the <span style='color:#D9565CFF;'>**league average**</span>"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(plot.subtitle = element_markdown())

ggsave("vet_exposure_trend.png", p1, height = 10, width = 16, dpi = 300)


# delta_net_rating by vet exposure tercile

p2 <- young_panel |>
  filter(abs(delta_net_rating) < 30) |>
  ggplot(aes(x = delta_net_rating, y = vet_tercile_label
             , fill = vet_tercile_label)) +
  geom_density_ridges(
    alpha = 0.7, scale = 1.2
    , quantile_lines = TRUE, quantiles = 2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(
    values = c("#D9565CFF", "#3B9AB2", "#172869FF")
    , guide = "none"
  ) +
  usaid_plot() +
  labs(
    x = "Year-over-Year Change in Net Rating"
    , y = ""
    , title = "Young Player Improvement by Veteran Exposure Level"
    , subtitle = "Distribution of year-over-year net rating changes for players in seasons 1-4\nVertical dashed line at zero; solid lines show medians"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )

#vet exposure vs improvement, faceted by career season

p3 <- young_panel |>
  filter(abs(delta_net_rating) < 30) |>
  mutate(career_label = paste0("Season ", career_season)) |>
  ggplot(aes(x = vet_min_share, y = delta_net_rating)) +
  geom_point(
    aes(fill = wiz_label), shape = 21, color = "white"
    , alpha = 0.4, size = 2.5
  ) +
  geom_smooth(method = "lm", color = "#D9565CFF", se = TRUE, alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_x_continuous(labels = scales::percent_format()) +
  facet_wrap(~career_label) +
  usaid_plot() +
  scale_fill_manual(values = c("#CCCCCC", "#172869FF")) +
  labs(
    x = "Veteran Minute Share"
    , y = "Year-over-Year Change in Net Rating"
    , title = "Veteran Exposure and Young Player Improvement by Career Season"
    , subtitle = "<span style='color:#172869FF;'>**Wizards**</span> young players highlighted"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(plot.subtitle = element_markdown())

ggsave("vet_scatter_faceted.png", p3, height = 10, width = 16, dpi = 300)


# Wizards specific timeline 

wiz_timeline <- team_vet_exposure |>
  filter(team_id == wizards_id)

league_avg <- team_vet_exposure |>
  # filter(team_total_min > 18000) |>
  group_by(season_start) |>
  summarise(avg_vet_share = mean(vet_min_share, na.rm = TRUE), .groups = "drop")

p4 <- wiz_timeline |>
  ggplot(aes(x = season_start, y = vet_min_share)) +
  # era shading
  annotate(
    "rect", xmin = 2014, xmax = 2020, ymin = -Inf, ymax = Inf
    , fill = "#172869FF", alpha = 0.08
  ) +
  annotate(
    "rect", xmin = 2020, xmax = 2024, ymin = -Inf, ymax = Inf
    , fill = "#D9565CFF", alpha = 0.08
  ) +
  annotate(
    "rect", xmin = 2024, xmax = 2025, ymin = -Inf, ymax = Inf
    , fill = "#3B9AB2", alpha = 0.08
  ) +
  geom_line(color = "#172869FF", linewidth = 1.2) +
  geom_point(color = "#172869FF", size = 4) +
  geom_line(
    data = league_avg
    , aes(x = season_start, y = avg_vet_share)
    , color = "#D9565CFF", linewidth = 1, linetype = "dashed"
    , inherit.aes = FALSE
  ) +
  geom_text(
    data = league_avg |> filter(season_start == max(season_start))
    , aes(x = season_start-2, y = avg_vet_share+.01, label = "League avg")
    , color = "#D9565CFF", vjust = -0.8, hjust = 0, fontface = "bold", size = 3.5
    , inherit.aes = FALSE
  ) +
  # era labels
  annotate(
    "text", x = 2016.5, y = .62
    , label = "Wall/Beal Prime", fontface = "bold"
    , color = "#172869FF", size = 6
  ) +
  annotate(
    "text", x = 2021.5, y = .6
    , label = "Treadmill\nof\nMediocrity", fontface = "bold"
    , color = "#D9565CFF", size = 6
  ) +
  annotate(
    "text", x = 2024.4, y = .62
    , label = "Rebuild", fontface = "bold"
    , color = "#3B9AB2", size = 6
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
  usaid_plot() +
  labs(
    x = ""
    , y = "Veteran Minute Share"
    , title = "Wizards Veteran Minute Share Over Time"
    , subtitle = "The rebuild era features the lowest veteran presence in franchise recent history"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("wiz_vet_timeline.png", p4, height = 10, width = 16, dpi = 300)


# prior year vs current year net rating

wiz_young <- young_panel |>
  filter(is_wiz) |>
  mutate(player_label = paste0(player_name, " (Yr ", career_season, ")"))

p5 <- wiz_young |> filter(first_season > 2023) |> 
  ggplot(aes(y = reorder(player_label, delta_net_rating))) +
  geom_segment(
    aes(x = prev_net_rating, xend = net_rating
        , yend = reorder(player_label, delta_net_rating))
    , color = "grey60", linewidth = 0.8
  ) +
  geom_point(aes(x = prev_net_rating), color = "#D9565CFF", size = 4) +
  geom_point(aes(x = net_rating), color = "#172869FF", size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  usaid_plot() +
  labs(
    x = "Net Rating"
    , y = ""
    , title = "<span style='color:#D9565CFF;'>Prior Year</span> vs. <span style='color:#172869FF;'>Current Year</span> Net Rating for Wizards Young Players"
    , subtitle = "Young players (seasons 1-4) who played consecutive seasons with the Wizards"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(plot.title = element_markdown())



# Model timeeeee ----

# prep model data
# Student-t likelihood handles heavy tails and outliers via the estimated
# degrees-of-freedom parameter (nu), so no need to filter on gp or delta range
model_df <- young_panel |>
  filter(
    !is.na(delta_net_rating)
    & !is.na(vet_min_share)
    & !is.na(prev_net_rating)
    & !is.na(draft_round_char)
    & !is.na(min)
    & !is.na(gp)
    & !is.na(position)
    & !is.na(team_id)
    & !is.na(season_start)
    & !is.na(team_win_pct)
  ) |>
  mutate(
    career_season = as.numeric(career_season)
    , position = case_when(position %in% c("C", "C-F", "F-C") ~ "Center"
                                      , position %in% c("F", "F-G", "G-F") ~ "Forward"
                                      , position %in% c("G") ~ "Guard"
    )
  )

nrow(model_df) 
n_distinct(model_df$player_id) # 1146 players in our data

# compile the Stan model
model <- cmdstan_model("veteran_effect.stan")

# create integer index lookups for random effects
team_lookup <- model_df |>
  distinct(team_id) |>
  arrange(team_id) |>
  mutate(team_idx = row_number())

season_lookup <- model_df |>
  distinct(season_start) |>
  arrange(season_start) |>
  mutate(season_idx = row_number())

position_lookup <- model_df |>
  distinct(position) |>
  arrange(position) |>
  mutate(position_idx = row_number())

player_lookup <- model_df |>
  distinct(player_id) |>
  arrange(player_id) |>
  mutate(player_idx = row_number())

model_df <- model_df |>
  left_join(team_lookup, by = "team_id") |>
  left_join(season_lookup, by = "season_start") |>
  left_join(position_lookup, by = "position") |>
  left_join(player_lookup, by = "player_id")

# standardize continuous predictors so Normal(0, 2.5) priors are appropriate
# without this, beta_gp * median(gp) ~ 2.5 * 50 = 125, which generates
# absurd prior predictive ranges
pred_means <- c(
  vet_min_share = mean(model_df$vet_min_share)
  , prev_net_rating = mean(model_df$prev_net_rating)
  , career_season = mean(model_df$career_season)
  , min = mean(model_df$min)
  , gp = mean(model_df$gp)
  , weighted_avg_exp = mean(model_df$weighted_avg_exp, na.rm = TRUE)
  , team_win_pct = mean(model_df$team_win_pct)
  , team_win_pct_lag = mean(model_df$team_win_pct_lag, na.rm = TRUE)
  , good_vet_min_share = mean(model_df$good_vet_min_share, na.rm = TRUE)
  , bad_vet_min_share = mean(model_df$bad_vet_min_share, na.rm = TRUE)
)

pred_sds <- c(
  vet_min_share = sd(model_df$vet_min_share)
  , prev_net_rating = sd(model_df$prev_net_rating)
  , career_season = sd(model_df$career_season)
  , min = sd(model_df$min)
  , gp = sd(model_df$gp)
  , weighted_avg_exp = sd(model_df$weighted_avg_exp, na.rm = TRUE)
  , team_win_pct = sd(model_df$team_win_pct)
  , team_win_pct_lag = sd(model_df$team_win_pct_lag, na.rm = TRUE)
  , good_vet_min_share = sd(model_df$good_vet_min_share, na.rm = TRUE)
  , bad_vet_min_share = sd(model_df$bad_vet_min_share, na.rm = TRUE)
)

model_df <- model_df |>
  mutate(
    vet_min_share = (vet_min_share - pred_means["vet_min_share"]) / pred_sds["vet_min_share"]
    , prev_net_rating = (prev_net_rating - pred_means["prev_net_rating"]) / pred_sds["prev_net_rating"]
    , career_season = (career_season - pred_means["career_season"]) / pred_sds["career_season"]
    , min = (min - pred_means["min"]) / pred_sds["min"]
    , gp = (gp - pred_means["gp"]) / pred_sds["gp"]
    , weighted_avg_exp = (weighted_avg_exp - pred_means["weighted_avg_exp"]) / pred_sds["weighted_avg_exp"]
    , team_win_pct = (team_win_pct - pred_means["team_win_pct"]) / pred_sds["team_win_pct"]
    , team_win_pct_lag = (team_win_pct_lag - pred_means["team_win_pct_lag"]) / pred_sds["team_win_pct_lag"]
    , good_vet_min_share = (good_vet_min_share - pred_means["good_vet_min_share"]) / pred_sds["good_vet_min_share"]
    , bad_vet_min_share = (bad_vet_min_share - pred_means["bad_vet_min_share"]) / pred_sds["bad_vet_min_share"]
  )


# Empirical priors (computed on standardized predictors)
prior_intercept_mean <- mean(model_df$delta_net_rating, na.rm = TRUE)
prior_intercept_sd_raw <- sd(model_df$delta_net_rating, na.rm = TRUE)



# OLS for veteran effect (on standardized scale)
ols_vet <- lm(delta_net_rating ~ vet_min_share, data = model_df)
prior_vet_mean <- as.numeric(coef(ols_vet)["vet_min_share"])
prior_vet_sd_raw <- summary(ols_vet)$coefficients["vet_min_share", "Std. Error"]

# regression to mean
ols_prev <- lm(delta_net_rating ~ prev_net_rating, data = model_df)
prior_prev_mean <- as.numeric(coef(ols_prev)["prev_net_rating"])
prior_prev_sd_raw <- summary(ols_prev)$coefficients["prev_net_rating", "Std. Error"]

# career season effect
ols_career <- lm(delta_net_rating ~ career_season, data = model_df)
prior_career_mean <- as.numeric(coef(ols_career)["career_season"])
prior_career_sd_raw <- summary(ols_career)$coefficients["career_season", "Std. Error"]

# helper to build Stan data with varying prior/likelihood settings
make_stan_data <- function(
    df
    , exposure_col = "vet_min_share"
    , win_pct_col = "team_win_pct"
    , prior_scale = 2
    , nu_alpha = 2, nu_beta = 0.1
    , use_student_t = 1L
    , use_team_re = 1L
    , use_win_pct = 1L
) {
  list(
    N = nrow(df)
    , N_teams = max(df$team_idx)
    , N_seasons = max(df$season_idx)
    , N_positions = max(df$position_idx)
    , N_players = max(df$player_idx)
    , team_id = df$team_idx
    , season_id = df$season_idx
    , position_id = df$position_idx
    , player_id = df$player_idx
    , delta_net_rating = df$delta_net_rating
    , vet_min_share = df[[exposure_col]]
    , prev_net_rating = df$prev_net_rating
    , career_season = df$career_season
    , minutes = df$min
    , games_played = df$gp
    , first_round = as.integer(df$draft_round_char == "First Round")
    , second_round = as.integer(df$draft_round_char == "Second Round")
    , team_win_pct = df[[win_pct_col]]
    , prior_intercept_mean = prior_intercept_mean
    , prior_intercept_sd = prior_intercept_sd_raw * prior_scale
    , prior_vet_mean = prior_vet_mean
    , prior_vet_sd = prior_vet_sd_raw * prior_scale
    , prior_prev_mean = prior_prev_mean
    , prior_prev_sd = prior_prev_sd_raw * prior_scale
    , prior_career_mean = prior_career_mean
    , prior_career_sd = prior_career_sd_raw * prior_scale
    , nu_prior_alpha = nu_alpha
    , nu_prior_beta = nu_beta
    , use_student_t = as.integer(use_student_t)
    , use_team_re = as.integer(use_team_re)
    , use_win_pct = as.integer(use_win_pct)
  )
}

# fit main model: Student-t, 2x empirical priors, gamma(2, 0.1) on nu
stan_data <- make_stan_data(model_df)

fit <- model$sample(
  data = stan_data
  , chains = 4
  , parallel_chains = 4
  , iter_warmup = 2000
  , iter_sampling = 2000
  , seed = 202
  , adapt_delta = 0.99
)

# MCMC diagnostics ----
(fit$summary(
  variables = c("alpha", "beta_vet", "beta_prev", "beta_career"
                , "beta_min", "beta_gp", "beta_first", "beta_second"
                , "beta_win_pct", "sigma_y", "nu", "sigma_team", "sigma_season"
                , "sigma_position", "sigma_player")
))

fit$diagnostic_summary()

param_summary <- fit$summary()
max_rhat <- max(param_summary$rhat, na.rm = TRUE)
min_ess <- min(param_summary$ess_bulk, na.rm = TRUE)


if (max_rhat > 1.01) warning("Some Rhat > 1.01 — check convergence!")
if (min_ess < 400) warning("Some ESS < 400 — consider more iterations!")

# trace plots for key parameters
p_trace <- mcmc_trace(
  fit$draws()
  , pars = c("beta_vet", "beta_prev", "beta_career", "sigma_y", "nu")
) +
  labs(title = "Trace Plots for Key Parameters")

# posterior predictive checks
y <- model_df$delta_net_rating
yrep <- fit$draws("delta_rep", format = "matrix")

p_ppc <- ppc_dens_overlay(y, yrep[1:100, ]) +
  labs(title = "Posterior Predictive Check: delta_net_rating")

p_ppc_stat <- ppc_stat(y, yrep, stat = "mean", bins = 100) +
  labs(title = "PPC: Distribution of Predicted Median vs. Observed Median")

# residual diagnostics (using posterior means of mu)
draws_df <- fit$draws(format = "df")

alpha_hat <- mean(draws_df$alpha)
bv_hat <- mean(draws_df$beta_vet)
bp_hat <- mean(draws_df$beta_prev)
bc_hat <- mean(draws_df$beta_career)
bm_hat <- mean(draws_df$beta_min)
bg_hat <- mean(draws_df$beta_gp)
bf_hat <- mean(draws_df$beta_first)
bs_hat <- mean(draws_df$beta_second)
bwp_hat <- mean(draws_df$beta_win_pct)

re_team_hat <- as.numeric(
  colMeans(fit$draws("re_team", format = "matrix"))
)
re_season_hat <- as.numeric(
  colMeans(fit$draws("re_season", format = "matrix"))
)
re_position_hat <- as.numeric(
  colMeans(fit$draws("re_position", format = "matrix"))
)
re_player_hat <- as.numeric(
  colMeans(fit$draws("re_player", format = "matrix"))
)

predicted <- alpha_hat +
  bv_hat * model_df$vet_min_share +
  bp_hat * model_df$prev_net_rating +
  bc_hat * model_df$career_season +
  bm_hat * model_df$min +
  bg_hat * model_df$gp +
  bf_hat * as.integer(model_df$draft_round_char == "First Round") +
  bs_hat * as.integer(model_df$draft_round_char == "Second Round") +
  bwp_hat * model_df$team_win_pct +
  re_team_hat[model_df$team_idx] +
  re_season_hat[model_df$season_idx] +
  re_position_hat[model_df$position_idx] +
  re_player_hat[model_df$player_idx]

residual <- y - predicted

resid_z <- (residual - mean(residual)) / sd(residual)

# nu (degrees of freedom) diagnostic
nu_draws <- draws_df$nu

# R-squared (point estimate from posterior mean predictions)
var_fitted <- var(predicted)
var_resid <- var(residual)
r2_point <- var_fitted / (var_fitted + var_resid)

# extract veteran effect posterior
vet_posterior <- tibble(beta_vet = draws_df$beta_vet)
prob_positive <- mean(vet_posterior$beta_vet > 0)


# Viz time

#  Posterior of veteran effect

p6 <- vet_posterior |>
  ggplot(aes(x = beta_vet)) +
  stat_halfeye(
    aes(fill = after_stat(x))
    , .width = c(0.5, 0.8, 0.95)
    , point_interval = median_qi
    , slab_alpha = 0.8
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  usaid_plot(data_type = "continuous") +
  scale_fill_gradient2(
    low = "#D9565CFF", mid = "#3B9AB2", high = "#172869FF"
    , midpoint = 0, guide = "none"
  ) +
  labs(
    x = "Effect of 1 SD Increase in Veteran Minute Share on Net Rating Improvement"
    , y = ""
    , title = "Posterior Distribution of the Veteran Effect"
    , subtitle = paste0(
      "P(veteran exposure helps) = ", round(prob_positive * 100, 1), "%"
      , " | 1 SD ≈ ", round(pred_sds["vet_min_share"] * 100, 0)
      , " percentage points of vet minutes"
    )
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(axis.text.y = element_blank())

ggsave("vet_posterior.png", p6, height = 10, width = 16, dpi = 300)


# Team random effects caterpillar plot

re_team_draws_tbl <- fit$draws("re_team", format = "matrix") |>
  as_tibble() |>
  pivot_longer(everything(), names_to = "param", values_to = "value") |>
  mutate(team_idx = as.integer(str_extract(param, "\\d+"))) |>
  group_by(team_idx) |>
  summarise(
    b = median(value)
    , .lower = quantile(value, 0.05)
    , .upper = quantile(value, 0.95)
  ) |>
  left_join(team_lookup, by = "team_idx") |>
  left_join(team_labels, by = "team_id") |>
  mutate(
    is_wiz = team_id == wizards_id
    , label = ifelse(is.na(team_abbreviation), team_id, team_abbreviation)
  )

p7 <- re_team_draws_tbl |>
  ggplot(aes(x = b, y = reorder(label, b))) +
  geom_pointrange(
    aes(xmin = .lower, xmax = .upper, color = is_wiz)
    , size = 0.8, linewidth = 0.8
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(
    values = c("TRUE" = "#172869FF", "FALSE" = "#999999")
    , guide = "none"
  ) +
  usaid_plot() +
  labs(
    x = "Team Random Effect (Intercept)"
    , y = ""
    , title = "Team Effects on Young Player Development"
    , subtitle = "<span style='color:#172869FF;'>**Wizards**</span> highlighted | 90% credible intervals shown"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(plot.subtitle = element_markdown())

ggsave("vet_team_effects.png", p7, height = 12, width = 14, dpi = 300)



# Let's look at the wiz ----

# Era comparison
wiz_eras <- young_panel |>
  filter(is_wiz) |>
  mutate(
    era = case_when(
      season_start >= 2014 & season_start <= 2019 ~ "Wall/Beal Prime\n(2014-2019)"
      , season_start >= 2020 & season_start <= 2023 ~ "Teardown\n(2020-2023)"
      , season_start >= 2024 ~ "Rebuild\n(2024+)"
      , TRUE ~ "Early\n(2010-2013)"
    )
    , era = factor(era, levels = c("Early\n(2010-2013)"
                                   , "Wall/Beal Prime\n(2014-2019)"
                                   , "Teardown\n(2020-2023)"
                                   , "Rebuild\n(2024+)"))
  )

era_summary <- wiz_eras |>
  group_by(era) |>
  summarise(
    n_player_seasons = n()
    , mean_vet_share = mean(vet_min_share, na.rm = TRUE)
    , mean_delta_nr = mean(delta_net_rating, na.rm = TRUE)
    , sd_delta_nr = sd(delta_net_rating, na.rm = TRUE)
    , .groups = "drop"
  )

era_summary

# Case studies — specific Wizards young players
wiz_case_studies <- c("John Wall", "Bradley Beal", "Otto Porter Jr."
                      , "Rui Hachimura", "Deni Avdija"
                      , "Bilal Coulibaly", "Bub Carrington", "Alex Sarr")

wiz_cases <- young_panel |>
  filter(is_wiz & player_name %in% wiz_case_studies) |>
  select(player_name, season_start, career_season, net_rating
         , prev_net_rating, delta_net_rating, vet_min_share) |>
  arrange(player_name, season_start)

wiz_cases


# Counterfactual for current rebuild players
current_rebuild <- young_panel |>
  filter(is_wiz & season_start >= 2024) |>
  select(player_id, player_name, season_start, career_season
         , net_rating, prev_net_rating, delta_net_rating
         , vet_min_share, good_vet_min_share, bad_vet_min_share
         , min, gp, draft_round_char, position
         , team_win_pct)

if (nrow(current_rebuild) > 0) {

  # standardize current_rebuild predictors using same scaling as model_df
  current_rebuild_z <- current_rebuild |>
    mutate(
      vet_min_share_z = (vet_min_share - pred_means["vet_min_share"]) / pred_sds["vet_min_share"]
      , good_vet_min_share_z = (good_vet_min_share - pred_means["good_vet_min_share"]) / pred_sds["good_vet_min_share"]
      , prev_net_rating_z = (prev_net_rating - pred_means["prev_net_rating"]) / pred_sds["prev_net_rating"]
      , career_season_z = (career_season - pred_means["career_season"]) / pred_sds["career_season"]
      , min_z = (min - pred_means["min"]) / pred_sds["min"]
      , gp_z = (gp - pred_means["gp"]) / pred_sds["gp"]
      , team_win_pct_z = (team_win_pct - pred_means["team_win_pct"]) / pred_sds["team_win_pct"]
      , team_win_pct_lag_z = (team_win_pct - pred_means["team_win_pct_lag"]) / pred_sds["team_win_pct_lag"]
    ) |>
    left_join(player_lookup, by = "player_id")

  # (p9 counterfactual is built after causal models are fitted — see below)
}

# Extract Wizards team random effect
wiz_re <- re_team_draws_tbl |> filter(team_id == wizards_id)


# Sensitivity Analysis ----

# 8a: Prior predictive checks ----
# sample from the priors in R to see what delta_net_rating values they imply


n_prior_sims <- 4000

# sample fixed effects from their priors (at 2x scale)
alpha_prior <- rnorm(n_prior_sims, prior_intercept_mean
                     , prior_intercept_sd_raw * 2)
beta_vet_prior <- rnorm(n_prior_sims, prior_vet_mean, prior_vet_sd_raw * 2)
beta_prev_prior <- rnorm(n_prior_sims, prior_prev_mean, prior_prev_sd_raw * 2)
beta_career_prior <- rnorm(n_prior_sims, prior_career_mean
                           , prior_career_sd_raw * 2)
beta_min_prior <- rnorm(n_prior_sims, 0, 2.5)
beta_gp_prior <- rnorm(n_prior_sims, 0, 2.5)
beta_win_pct_prior <- rnorm(n_prior_sims, 0, 2.5)

sigma_prior <- abs(rnorm(n_prior_sims, 0, 5))
nu_prior <- rgamma(n_prior_sims, 2, 0.1)

# compute prior predictive for a "typical" young player (standardized scale)
# medians of standardized predictors are around 0
x_vet <- median(model_df$vet_min_share)
x_prev <- median(model_df$prev_net_rating)
x_career <- median(model_df$career_season)
x_min <- median(model_df$min)
x_gp <- median(model_df$gp)
x_win_pct <- median(model_df$team_win_pct)

prior_pred_mu <- alpha_prior +
  beta_vet_prior * x_vet +
  beta_prev_prior * x_prev +
  beta_career_prior * x_career +
  beta_min_prior * x_min +
  beta_gp_prior * x_gp +
  beta_win_pct_prior * x_win_pct

# sample from student-t noise (clamped nu >= 2 to match Stan constraint)
nu_clamped <- pmax(nu_prior, 2.01)
prior_pred_y <- prior_pred_mu + rt(n_prior_sims, df = nu_clamped) * sigma_prior

prior_pred_df <- tibble(
  source = c(
    rep("Prior Predictive", n_prior_sims)
    , rep("Observed Data", length(y))
  )
  , value = c(prior_pred_y, y)
)

p_prior_pred <- prior_pred_df |>
  filter(abs(value) < 100) |>
  ggplot(aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  usaid_plot() +
  scale_fill_manual(values = c("#3B9AB2", "#D9565CFF")) +
  labs(
    x = "delta_net_rating"
    , y = "Density"
    , fill = ""
    , title = "Prior Predictive Check"
    , subtitle = "Do the priors generate plausible outcome values before seeing the data?"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )



#LOO cross-calidation (Student-t vs Gaussian) ----


# LOO for main Student-t model
log_lik_t <- fit$draws("log_lik", format = "matrix")
loo_t <- loo(log_lik_t, cores = 4)

(loo_t)

# fit Gaussian model for comparison
stan_data_gauss <- make_stan_data(model_df, use_student_t = 0L)

loo_gauss <- NULL
loo_comp <- NULL

tryCatch({
  fit_gauss <- model$sample(
    data = stan_data_gauss
    , chains = 4
    , parallel_chains = 4
    , iter_warmup = 2000
    , iter_sampling = 2000
    , seed = 202
    , adapt_delta = 0.99
  )

  log_lik_gauss <- fit_gauss$draws("log_lik", format = "matrix")
  loo_gauss <- loo(log_lik_gauss, cores = 4)

  print(loo_gauss)

  # compare
  loo_comp <- loo_compare(loo_t, loo_gauss)
  print(loo_comp)
}, error = function(e) {
  message("Gaussian model failed: ", e$message)
  message("This is expected — nu ≈ 2 confirms data is incompatible with Gaussian likelihood.")
})


# Specification Sensitivity: Team RE vs Win% ----
# Check whether beta_vet is driven by the inclusion of team_win_pct
# (which could be a mediator on the causal path vet_min → team_wins → development)
# or team random effects, or both.
#
# Spec A: Team RE only (no win%)          — original-style model
# Spec B: Win% only (no team RE)          — fixed team quality control
# Spec C: Both (current main model)       — already fitted above

spec_configs <- list(
  list(use_team_re = 1L, use_win_pct = 0L, label = "Team RE only (no win%)")
  , list(use_team_re = 0L, use_win_pct = 1L, label = "Win% only (no team RE)")
)

spec_results <- tibble(
  spec = "Both (main model)"
  , mean_vet = mean(draws_df$beta_vet)
  , lower_95 = quantile(draws_df$beta_vet, 0.025)
  , upper_95 = quantile(draws_df$beta_vet, 0.975)
  , prob_pos = prob_positive
  , median_nu = median(draws_df$nu)
)

spec_draws_all <- tibble(
  spec = "Both (main model)", beta_vet = draws_df$beta_vet
)

for (cfg in spec_configs) {

  message("Fitting specification: ", cfg$label)

  tryCatch({
    stan_data_spec <- make_stan_data(
      model_df
      , use_team_re = cfg$use_team_re
      , use_win_pct = cfg$use_win_pct
    )

    fit_spec <- model$sample(
      data = stan_data_spec
      , chains = 4
      , parallel_chains = 4
      , iter_warmup = 2000
      , iter_sampling = 2000
      , seed = 202
      , adapt_delta = 0.99
    )

    draws_spec <- fit_spec$draws(format = "df")

    spec_results <- bind_rows(spec_results, tibble(
      spec = cfg$label
      , mean_vet = mean(draws_spec$beta_vet)
      , lower_95 = quantile(draws_spec$beta_vet, 0.025)
      , upper_95 = quantile(draws_spec$beta_vet, 0.975)
      , prob_pos = mean(draws_spec$beta_vet > 0)
      , median_nu = median(draws_spec$nu)
    ))

    spec_draws_all <- bind_rows(spec_draws_all, tibble(
      spec = cfg$label, beta_vet = draws_spec$beta_vet
    ))
  }, error = function(e) {
    message("Specification '", cfg$label, "' failed: ", e$message)
  })
}

spec_results

p_spec <- spec_draws_all |>
  ggplot(aes(x = beta_vet, fill = spec)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  usaid_plot() +
  scale_fill_manual(values = c("#172869FF", "#D9565CFF", "#3B9AB2")) +
  labs(
    x = "Veteran Effect (beta_vet)"
    , y = "Density"
    , fill = "Specification"
    , title = "Specification Sensitivity: Team RE vs. Win% as Team Quality Control"
    , subtitle = "Does the veteran effect depend on how we control for team quality?"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("vet_spec_sensitivity.png", p_spec, height = 10, width = 16, dpi = 300)


# Descriptive Analysis: Good vs Bad Veterans ----
# How many veterans are "good" (net rating >= 0) vs "bad" (net rating < 0)?
# How many of each type have played for the Wizards?

# get player names from adv_df (which has player_name from the API)
vet_names <- adv_df |>
  select(player_id, player_name) |>
  distinct(player_id, .keep_all = TRUE)

vet_descriptive <- vet_quality_df |>
  left_join(vet_names, by = "player_id") |>
  mutate(
    vet_type = ifelse(net_rating >= 0, "Good (NRtg >= 0)", "Bad (NRtg < 0)")
    , is_wiz = team_id == wizards_id
  )

# league-wide summary: unique player-seasons by type
vet_type_summary <- vet_descriptive |>
  group_by(vet_type) |>
  summarise(
    n_player_seasons = n()
    , n_unique_players = n_distinct(player_id)
    , avg_net_rating = round(mean(net_rating, na.rm = TRUE), 1)
    , median_net_rating = round(median(net_rating, na.rm = TRUE), 1)
    , avg_min = round(mean(total_min, na.rm = TRUE), 0)
    , .groups = "drop"
  )


# share of vet player-seasons that are "good"
pct_good <- vet_type_summary |>
  mutate(pct = n_player_seasons / sum(n_player_seasons) * 100)

# Wizards-specific
wiz_vet_descriptive <- vet_descriptive |>
  filter(is_wiz)

wiz_vet_type_summary <- wiz_vet_descriptive |>
  group_by(vet_type) |>
  summarise(
    n_player_seasons = n()
    , n_unique_players = n_distinct(player_id)
    , avg_net_rating = round(mean(net_rating, na.rm = TRUE), 1)
    , median_net_rating = round(median(net_rating, na.rm = TRUE), 1)
    , avg_min = round(mean(total_min, na.rm = TRUE), 0)
    , .groups = "drop"
  )


wiz_pct_good <- wiz_vet_type_summary |>
  mutate(pct = n_player_seasons / sum(n_player_seasons) * 100)

# list Wizards veterans by type (most recent season first)
wiz_good_vets <- wiz_vet_descriptive |>
  filter(vet_type == "Good (NRtg >= 0)") |>
  arrange(desc(season_start), desc(net_rating)) |>
  select(player_name, season_start, net_rating, total_min)

wiz_bad_vets <- wiz_vet_descriptive |>
  filter(vet_type == "Bad (NRtg < 0)") |>
  arrange(desc(season_start), net_rating) |>
  select(player_name, season_start, net_rating, total_min)


# trend over time: what share of Wizards vet minutes were "good" vs "bad"?
wiz_vet_trend <- vet_descriptive |>
  filter(is_wiz) |>
  group_by(season_start, vet_type) |>
  summarise(total_min = sum(total_min, na.rm = TRUE), .groups = "drop") |>
  group_by(season_start) |>
  mutate(pct_of_vet_min = total_min / sum(total_min) * 100) |>
  ungroup()

p_vet_quality <- wiz_vet_trend |>
  ggplot(aes(x = season_start, y = pct_of_vet_min, fill = vet_type)) +
  geom_col(position = "stack") +
  usaid_plot() +
  scale_fill_manual(values = c("Bad (NRtg < 0)" = "#D9565CFF"
                                , "Good (NRtg >= 0)" = "#3B9AB2")) +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  labs(
    x = "Season"
    , y = "% of Veteran Minutes"
    , fill = ""
    , title = "Share of Wizards Veteran Minutes by Quality"
    , subtitle = "Good = net rating >= 0 | Bad = net rating < 0"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("wiz_vet_quality_trend.png", p_vet_quality, height = 10, width = 16
       , dpi = 300)


# Causal Identification: Lagged Win% + Vet Quality Decomposition ----
# 1. Lagged win%: blocks the confound (team quality → vets) without blocking
#    the mediator (vets → current wins → development environment)
# 2. Vet quality decomposition: if mentorship matters, both good AND bad vets
#    should help. If it's just "good teammates," only good-vet minutes should matter.

# helper to rebuild contiguous Stan indices for a subset of model_df
rebuild_stan_indices <- function(df) {
  team_lk <- df |> distinct(team_id) |> arrange(team_id) |>
    mutate(team_idx = row_number())
  season_lk <- df |> distinct(season_start) |> arrange(season_start) |>
    mutate(season_idx = row_number())
  position_lk <- df |> distinct(position) |> arrange(position) |>
    mutate(position_idx = row_number())
  player_lk <- df |> distinct(player_id) |> arrange(player_id) |>
    mutate(player_idx = row_number())

  df |>
    select(-team_idx, -season_idx, -position_idx, -player_idx) |>
    left_join(team_lk, by = "team_id") |>
    left_join(season_lk, by = "season_start") |>
    left_join(position_lk, by = "position") |>
    left_join(player_lk, by = "player_id")
}

# filter to observations with lagged win% and rebuild indices
model_df_lag <- model_df |>
  filter(!is.na(team_win_pct_lag)) |>
  rebuild_stan_indices()

causal_results <- tibble()
causal_draws_all <- tibble()

# Lagged win% + team RE (the key causal test)
tryCatch({
  stan_data_lag <- make_stan_data(
    model_df_lag
    , win_pct_col = "team_win_pct_lag"
    , use_team_re = 1L
    , use_win_pct = 1L
  )

  fit_lag <- model$sample(
    data = stan_data_lag
    , chains = 4
    , parallel_chains = 4
    , iter_warmup = 2000
    , iter_sampling = 2000
    , seed = 202
    , adapt_delta = 0.99
  )

  draws_lag <- fit_lag$draws(format = "df")

  causal_results <- bind_rows(causal_results, tibble(
    spec = "Lagged win% + team RE"
    , mean_vet = mean(draws_lag$beta_vet)
    , lower_95 = quantile(draws_lag$beta_vet, 0.025)
    , upper_95 = quantile(draws_lag$beta_vet, 0.975)
    , prob_pos = mean(draws_lag$beta_vet > 0)
    , beta_win_pct = mean(draws_lag$beta_win_pct)
  ))

  causal_draws_all <- bind_rows(causal_draws_all, tibble(
    spec = "Lagged win% + team RE", beta_vet = draws_lag$beta_vet
  ))
}, error = function(e) {
  message("Lagged win% model failed: ", e$message)
})

# Good veteran exposure (with lagged win%)
tryCatch({
  model_df_good <- model_df_lag |>
    filter(!is.na(good_vet_min_share))

  stan_data_good <- make_stan_data(
    model_df_good
    , exposure_col = "good_vet_min_share"
    , win_pct_col = "team_win_pct_lag"
    , use_team_re = 1L
    , use_win_pct = 1L
  )

  fit_good <- model$sample(
    data = stan_data_good
    , chains = 4
    , parallel_chains = 4
    , iter_warmup = 2000
    , iter_sampling = 2000
    , seed = 202
    , adapt_delta = 0.99
  )

  draws_good <- fit_good$draws(format = "df")

  causal_results <- bind_rows(causal_results, tibble(
    spec = "Good vets (net rtg >= 0)"
    , mean_vet = mean(draws_good$beta_vet)
    , lower_95 = quantile(draws_good$beta_vet, 0.025)
    , upper_95 = quantile(draws_good$beta_vet, 0.975)
    , prob_pos = mean(draws_good$beta_vet > 0)
    , beta_win_pct = mean(draws_good$beta_win_pct)
  ))

  causal_draws_all <- bind_rows(causal_draws_all, tibble(
    spec = "Good vets (net rtg >= 0)", beta_vet = draws_good$beta_vet
  ))
}, error = function(e) {
  message("Good vet model failed: ", e$message)
})

# Bad veteran exposure (with lagged win%)
tryCatch({
  model_df_bad <- model_df_lag |>
    filter(!is.na(bad_vet_min_share))

  stan_data_bad <- make_stan_data(
    model_df_bad
    , exposure_col = "bad_vet_min_share"
    , win_pct_col = "team_win_pct_lag"
    , use_team_re = 1L
    , use_win_pct = 1L
  )

  fit_bad <- model$sample(
    data = stan_data_bad
    , chains = 4
    , parallel_chains = 4
    , iter_warmup = 2000
    , iter_sampling = 2000
    , seed = 202
    , adapt_delta = 0.99
  )

  draws_bad <- fit_bad$draws(format = "df")

  causal_results <- bind_rows(causal_results, tibble(
    spec = "Bad vets (net rtg < 0)"
    , mean_vet = mean(draws_bad$beta_vet)
    , lower_95 = quantile(draws_bad$beta_vet, 0.025)
    , upper_95 = quantile(draws_bad$beta_vet, 0.975)
    , prob_pos = mean(draws_bad$beta_vet > 0)
    , beta_win_pct = mean(draws_bad$beta_win_pct)
  ))

  causal_draws_all <- bind_rows(causal_draws_all, tibble(
    spec = "Bad vets (net rtg < 0)", beta_vet = draws_bad$beta_vet
  ))
}, error = function(e) {
  message("Bad vet model failed: ", e$message)
})

causal_results

if (nrow(causal_draws_all) > 0) {
  p_causal <- causal_draws_all |> filter(spec!= "Lagged win% + team RE" ) |> 
    ggplot(aes(x = beta_vet, fill = spec)) +
    geom_density(alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    usaid_plot() +
    scale_fill_manual(values = c("#172869FF", "#3B9AB2", "#D9565CFF")) +
    labs(
      x = "Effect on Young Player Net Rating Improvement"
      , y = ""
      , fill = ""
      , title = "Distribution of effects between good and bad vets"
      , subtitle = "If mentorship matters, both good and bad veteran exposure should help development"
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    )

  ggsave("vet_causal_identification.png", p_causal, height = 10, width = 16
         , dpi = 300)
}


# Quality counterfactual — good vs bad veteran exposure ----

if (exists("draws_good") && exists("draws_bad")) {

  # Reference player: median 2nd-year first-round pick (no REs)
  ref_prev_z_q   <- (median(model_df$prev_net_rating) - pred_means["prev_net_rating"]) / pred_sds["prev_net_rating"]
  ref_career_z_q <- (2 - pred_means["career_season"]) / pred_sds["career_season"]
  ref_min_z_q    <- (median(model_df$min) - pred_means["min"]) / pred_sds["min"]
  ref_gp_z_q     <- (median(model_df$gp) - pred_means["gp"]) / pred_sds["gp"]
  ref_lag_z_q    <- (median(model_df_good$team_win_pct_lag, na.rm = TRUE) - pred_means["team_win_pct_lag"]) / pred_sds["team_win_pct_lag"]

  # Quantiles of good/bad vet share (already z-scored in model_df_good/bad)
  good_quantiles <- quantile(model_df_good$good_vet_min_share, probs = c(0.10, 0.50, 0.90))
  bad_quantiles  <- quantile(model_df_bad$bad_vet_min_share,  probs = c(0.10, 0.50, 0.90))

  # Back-transform to original scale for axis labels
  good_quantiles_orig <- good_quantiles * pred_sds["good_vet_min_share"] + pred_means["good_vet_min_share"]
  bad_quantiles_orig  <- bad_quantiles  * pred_sds["bad_vet_min_share"]  + pred_means["bad_vet_min_share"]

  # Predictions for good vet exposure
  pred_draws_good_cf <- sapply(good_quantiles, function(vq) {
    draws_good$alpha +
      draws_good$beta_vet    * vq +
      draws_good$beta_prev   * ref_prev_z_q +
      draws_good$beta_career * ref_career_z_q +
      draws_good$beta_min    * ref_min_z_q +
      draws_good$beta_gp     * ref_gp_z_q +
      draws_good$beta_first  * 1 +
      draws_good$beta_second * 0 +
      draws_good$beta_win_pct * ref_lag_z_q
  })

  # Predictions for bad vet exposure
  pred_draws_bad_cf <- sapply(bad_quantiles, function(vq) {
    draws_bad$alpha +
      draws_bad$beta_vet    * vq +
      draws_bad$beta_prev   * ref_prev_z_q +
      draws_bad$beta_career * ref_career_z_q +
      draws_bad$beta_min    * ref_min_z_q +
      draws_bad$beta_gp     * ref_gp_z_q +
      draws_bad$beta_first  * 1 +
      draws_bad$beta_second * 0 +
      draws_bad$beta_win_pct * ref_lag_z_q
  })

  # Center at the draw level (subtract 50th pct column) so uncertainty reflects
  # variance of beta_vet * (vq - vq_median) rather than collapsing correlated quantiles
  pred_draws_good_cf <- pred_draws_good_cf - pred_draws_good_cf[, 2]
  pred_draws_bad_cf  <- pred_draws_bad_cf  - pred_draws_bad_cf[, 2]

  pred_summary_good_cf <- tibble(
    vet_label = paste0(c("10th", "50th", "90th"), " percentile")
    , vet_share = good_quantiles
    , mean      = colMeans(pred_draws_good_cf)
    , lower     = apply(pred_draws_good_cf, 2, quantile, 0.05)
    , upper     = apply(pred_draws_good_cf, 2, quantile, 0.95)
    , lower_50  = apply(pred_draws_good_cf, 2, quantile, 0.25)
    , upper_50  = apply(pred_draws_good_cf, 2, quantile, 0.75)
    , vet_type  = "Good Veterans (NRtg \u2265 0)"
  )

  pred_summary_bad_cf <- tibble(
    vet_label = paste0(c("10th", "50th", "90th"), " percentile")
    , vet_share = bad_quantiles
    , mean      = colMeans(pred_draws_bad_cf)
    , lower     = apply(pred_draws_bad_cf, 2, quantile, 0.05)
    , upper     = apply(pred_draws_bad_cf, 2, quantile, 0.95)
    , lower_50  = apply(pred_draws_bad_cf, 2, quantile, 0.25)
    , upper_50  = apply(pred_draws_bad_cf, 2, quantile, 0.75)
    , vet_type  = "Bad Veterans (NRtg < 0)"
  )

  pred_summary_quality <- bind_rows(pred_summary_good_cf, pred_summary_bad_cf) |>
    mutate(vet_type = factor(vet_type, levels = c("Good Veterans (NRtg \u2265 0)", "Bad Veterans (NRtg < 0)")))

  p8 <- pred_summary_quality |>
    ggplot(aes(x = reorder(vet_label, vet_share), y = mean, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper)
      , color = "#172869FF", width = 0.12, linewidth = 0.8
    ) +
    # geom_errorbar(
      # aes(ymin = lower_50, ymax = upper_50)
      # , color = "#172869FF", width = 0.12, linewidth = 2
    # ) +
    geom_line(color = "#172869FF", linewidth = 1.2) +
    geom_point(color = "#172869FF", size = 3) +
    facet_wrap(~vet_type, scales = "free_x") +
    usaid_plot() +
    labs(
      x = "Veteran Exposure Level"
      , y = "Net Rating Change vs. Median Veteran Exposure"
      , title = "Veteran Quality, Not Quantity, Drives Young Player Development"
      , subtitle = "Predicted change relative to median exposure for a 2nd-year first-round pick"
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    )

  ggsave("vet_counterfactual.png", p8, height = 10, width = 16, dpi = 300)


  # Wizards quality counterfactual ----

  if (nrow(current_rebuild) > 0) {

    # Get Wizards team RE from fit_good (uses model_df_good's re-indexed team_idx)
    wiz_team_idx_good <- model_df_good |>
      filter(team_id == wizards_id) |>
      pull(team_idx) |>
      unique() |>
      head(1)

    re_wiz_draws_good <- if (length(wiz_team_idx_good) > 0 && !is.na(wiz_team_idx_good)) {
      fit_good$draws(
        paste0("re_team[", wiz_team_idx_good, "]")
        , format = "matrix"
      )[, 1]
    } else {
      rep(0, nrow(draws_good))
    }

    # Scenario reference values for good_vet_min_share (z-scored)
    league_avg_good_z     <- median(model_df_good$good_vet_min_share)
    top_quartile_good_z   <- quantile(model_df_good$good_vet_min_share, 0.75)

    # Back-transform for labels
    league_avg_good_orig   <- league_avg_good_z   * pred_sds["good_vet_min_share"] + pred_means["good_vet_min_share"]
    top_quartile_good_orig <- top_quartile_good_z * pred_sds["good_vet_min_share"] + pred_means["good_vet_min_share"]

    wiz_target_players <- c(
      "Tre Johnson", "Will Riley", "Alex Sarr"
      , "Kyshawn George", "Bub Carrington", "Tristan Vukcevic", "Bilal Coulibaly"
    )

    scenario_results_good <- current_rebuild_z |>
      filter(player_name %in% wiz_target_players) |>
      group_by(player_name) |>
      slice_max(career_season, n = 1) |>
      ungroup() |>
      crossing(scenario = c("Actual", "League Average Quality", "Top Quartile Quality")) |>
      mutate(
        good_vet_pred_z = case_when(
          scenario == "Actual"                ~ good_vet_min_share_z
          , scenario == "League Average Quality" ~ league_avg_good_z
          , scenario == "Top Quartile Quality"   ~ top_quartile_good_z
        )
      )

    counterfactual_summary_good <- purrr::map_dfr(
      seq_len(nrow(scenario_results_good)), function(i) {
        row <- scenario_results_good[i, ]
        # player REs set to 0 to avoid index mismatch between fit and fit_good
        mu_draws <- draws_good$alpha +
          draws_good$beta_vet    * row$good_vet_pred_z +
          draws_good$beta_prev   * row$prev_net_rating_z +
          draws_good$beta_career * row$career_season_z +
          draws_good$beta_min    * row$min_z +
          draws_good$beta_gp     * row$gp_z +
          draws_good$beta_first  * as.integer(row$draft_round_char == "First Round") +
          draws_good$beta_second * as.integer(row$draft_round_char == "Second Round") +
          draws_good$beta_win_pct * row$team_win_pct_lag_z +
          re_wiz_draws_good

        tibble(
          player_name  = row$player_name
          , scenario   = row$scenario
          , pred_mean  = mean(mu_draws)
          , pred_lower = quantile(mu_draws, 0.1)
          , pred_upper = quantile(mu_draws, 0.9)
          , actual_delta = row$delta_net_rating
        )
      }
    )

    message("\nQuality Counterfactual Predictions for Current Wizards Young Players:")
    print(counterfactual_summary_good)

    counterfactual_summary_good <- counterfactual_summary_good |>
      mutate(
        scenario = factor(
          scenario
          , levels = c("Actual", "League Average Quality", "Top Quartile Quality")
          # , labels = c(
            # paste0("Actual (", round(mean(current_rebuild$good_vet_min_share, na.rm = TRUE) * 100, 0), "%)")
            # , paste0("League Median (", round(league_avg_good_orig * 100, 0), "%)")
            # , paste0("Top Quartile (", round(top_quartile_good_orig * 100, 0), "%)")
          )
        # )
      ) |>
      left_join(
        current_rebuild |>
          group_by(player_name) |>
          slice_max(career_season, n = 1) |>
          ungroup() |>
          distinct(player_name, career_season) |>
          rename(career_yr = career_season)
        , by = "player_name"
      ) |>
      mutate(
        player_label = paste0(player_name, " (Yr ", career_yr, ")")
      )

    p9 <- counterfactual_summary_good |>
      ggplot(aes(
        x = pred_mean
        , y = reorder(player_name, pred_mean)
        , color = scenario
      )) +
      geom_pointrange(
        aes(xmin = pred_lower, xmax = pred_upper)
        , size = 0.8, linewidth = 0.8
        , position = position_dodge(width = 0.6)
      ) +
      # geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
      usaid_plot() +
      scale_color_manual(values = c("#D9565CFF", "#3B9AB2", "#172869FF")) +
      labs(
        x = "Predicted Year-over-Year Net Rating Change"
        , y = ""
        , color = "Good Veteran Share"
        , title = "What If the Wizards Had Better Veteran Quality?"
        , subtitle = "Predicted improvement under different good-veteran minute share scenarios | 80% CI"
        , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
      ) +
      theme(legend.position = "top")

    ggsave("wiz_player_counterfactual.png", p9, height = 10, width = 16, dpi = 300)

    ggsave("wiz_vet_gap.png", p9, height = 10, width = 16, dpi = 300)
  }
}


# Prior Scale Sensitivity (1x, 2x, 5x) ----


scale_results <- tibble(
  scale = "2x (main)"
  , mean_vet = mean(draws_df$beta_vet)
  , lower_95 = quantile(draws_df$beta_vet, 0.025)
  , upper_95 = quantile(draws_df$beta_vet, 0.975)
  , prob_pos = prob_positive
  , median_nu = median(draws_df$nu)
)

# collect draws for overlay plot
scale_draws <- tibble(
  scale = "2x (main)", beta_vet = draws_df$beta_vet
)

for (s in c(1, 5)) {


  stan_data_s <- make_stan_data(model_df, prior_scale = s)

  fit_s <- model$sample(
    data = stan_data_s
    , chains = 4
    , parallel_chains = 4
    , iter_warmup = 2000
    , iter_sampling = 2000
    , seed = 202
    , adapt_delta = 0.99
  )

  draws_s <- fit_s$draws(format = "df")

  scale_results <- bind_rows(scale_results, tibble(
    scale = paste0(s, "x")
    , mean_vet = mean(draws_s$beta_vet)
    , lower_95 = quantile(draws_s$beta_vet, 0.025)
    , upper_95 = quantile(draws_s$beta_vet, 0.975)
    , prob_pos = mean(draws_s$beta_vet > 0)
    , median_nu = median(draws_s$nu)
  ))

  scale_draws <- bind_rows(scale_draws, tibble(
    scale = paste0(s, "x"), beta_vet = draws_s$beta_vet
  ))
}

scale_results

p_scale <- scale_draws |>
  ggplot(aes(x = beta_vet, fill = scale)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  usaid_plot() +
  scale_fill_manual(values = c("#D9565CFF", "#3B9AB2", "#172869FF")) +
  labs(
    x = "Veteran Effect (beta_vet)"
    , y = "Density"
    , fill = "Prior Scale"
    , title = "Prior Scale Sensitivity: How Much Do Priors Matter?"
    , subtitle = "1x = tight (OLS SE), 2x = main model, 5x = diffuse"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("vet_prior_scale_sensitivity.png", p_scale, height = 10, width = 14
       , dpi = 300)


#  Nu Prior Sensitivity ----


nu_configs <- list(
  list(alpha = 2, beta = 0.1, label = "gamma(2, 0.1) — main")
  , list(alpha = 2, beta = 0.5, label = "gamma(2, 0.5) — heavier tails")
  , list(alpha = 10, beta = 1, label = "gamma(10, 1) — lighter tails")
)

nu_results <- tibble()
nu_draws_all <- tibble()

for (cfg in nu_configs) {


  if (cfg$alpha == 2 && cfg$beta == 0.1) {
    # reuse main model fit
    d <- draws_df
  } else {
    stan_data_nu <- make_stan_data(
      model_df
      , nu_alpha = cfg$alpha, nu_beta = cfg$beta
    )

    fit_nu <- model$sample(
      data = stan_data_nu
      , chains = 4
      , parallel_chains = 4
      , iter_warmup = 2000
      , iter_sampling = 2000
      , seed = 202
      , adapt_delta = 0.99
    )
    d <- fit_nu$draws(format = "df")
  }

  nu_results <- bind_rows(nu_results, tibble(
    prior = cfg$label
    , mean_vet = mean(d$beta_vet)
    , prob_pos = mean(d$beta_vet > 0)
    , median_nu = median(d$nu)
    , nu_lower_90 = quantile(d$nu, 0.05)
    , nu_upper_90 = quantile(d$nu, 0.95)
  ))

  nu_draws_all <- bind_rows(nu_draws_all, tibble(
    prior = cfg$label
    , nu = d$nu
    , beta_vet = d$beta_vet
  ))
}

nu_results

p_nu <- nu_draws_all |>
  ggplot(aes(x = nu, fill = prior)) +
  geom_density(alpha = 0.4) +
  usaid_plot() +
  scale_fill_manual(values = c("#D9565CFF", "#3B9AB2", "#172869FF")) +
  labs(
    x = "Degrees of Freedom (nu)"
    , y = "Density"
    , fill = "Nu Prior"
    , title = "Nu Prior Sensitivity: Is the Data Informative About Tail Heaviness?"
    , subtitle = "If posteriors overlap, the data dominates the prior"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("vet_nu_sensitivity.png", p_nu, height = 10, width = 14
       , dpi = 300)


# Alternative exposure measure ----
# this is a way to see how robust our results are
# I learned this as like a placebo test in econ but it works here in a less
# extreme way

model_df_alt <- model_df |> filter(!is.na(weighted_avg_exp))
stan_data_alt <- make_stan_data(model_df_alt, exposure_col = "weighted_avg_exp")

draws_alt <- NULL
tryCatch({
  fit_alt <- model$sample(
    data = stan_data_alt
    , chains = 4
    , parallel_chains = 4
    , iter_warmup = 2000
    , iter_sampling = 2000
    , seed = 202
    , adapt_delta = 0.99
  )

  draws_alt <- fit_alt$draws(format = "df")
  message("Alt exposure (weighted_avg_exp):")
  message("  Mean: ", round(mean(draws_alt$beta_vet), 3))
  message("  P(> 0): ", round(mean(draws_alt$beta_vet > 0), 3))
}, error = function(e) {
  message("Alt exposure model failed: ", e$message)
})


# bring it all together
alt_row <- if (!is.null(draws_alt)) {
  tibble(
    Test = "Alt exposure (weighted_avg_exp)"
    , `beta_vet Mean` = round(mean(draws_alt$beta_vet), 2)
    , `95% CI` = paste0("[", round(quantile(draws_alt$beta_vet, 0.025), 2)
                        , ", ", round(quantile(draws_alt$beta_vet, 0.975), 2), "]")
    , `P(>0)` = round(mean(draws_alt$beta_vet > 0), 3)
    , `Median nu` = round(median(draws_alt$nu), 1)
  )
} else {
  tibble(
    Test = "Alt exposure (weighted_avg_exp)"
    , `beta_vet Mean` = NA_real_
    , `95% CI` = "model failed"
    , `P(>0)` = NA_real_
    , `Median nu` = NA_real_
  )
}

final_summary <- bind_rows(
  tibble(
    Test = "Main (both RE + win%)"
    , `beta_vet Mean` = round(mean(draws_df$beta_vet), 2)
    , `95% CI` = paste0("[", round(quantile(draws_df$beta_vet, 0.025), 2)
                        , ", ", round(quantile(draws_df$beta_vet, 0.975), 2), "]")
    , `P(>0)` = round(prob_positive, 3)
    , `Median nu` = round(median(draws_df$nu), 1)
  )
  , spec_results |>
    filter(spec != "Both (main model)") |>
    transmute(
      Test = paste0("Spec: ", spec)
      , `beta_vet Mean` = round(mean_vet, 2)
      , `95% CI` = paste0("[", round(lower_95, 2), ", ", round(upper_95, 2), "]")
      , `P(>0)` = round(prob_pos, 3)
      , `Median nu` = round(median_nu, 1)
    )
  , if (nrow(causal_results) > 0) {
    causal_results |>
      transmute(
        Test = spec
        , `beta_vet Mean` = round(mean_vet, 2)
        , `95% CI` = paste0("[", round(lower_95, 2), ", ", round(upper_95, 2), "]")
        , `P(>0)` = round(prob_pos, 3)
        , `Median nu` = NA_real_
      )
  }
  , scale_results |>
    filter(scale != "2x (main)") |>
    transmute(
      Test = paste0("Prior scale ", scale)
      , `beta_vet Mean` = round(mean_vet, 2)
      , `95% CI` = paste0("[", round(lower_95, 2), ", ", round(upper_95, 2), "]")
      , `P(>0)` = round(prob_pos, 3)
      , `Median nu` = round(median_nu, 1)
    )
  , nu_results |>
    filter(prior != "gamma(2, 0.1) — main") |>
    transmute(
      Test = paste0("Nu: ", prior)
      , `beta_vet Mean` = round(mean_vet, 2)
      , `95% CI` = NA_character_
      , `P(>0)` = round(prob_pos, 3)
      , `Median nu` = round(median_nu, 1)
    )
  , alt_row
)

final_summary

if (!is.null(loo_comp)) loo_comp