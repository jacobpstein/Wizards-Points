##############################
# Quarterly stats for top 10 players
# Data pulled using 
# Session Info:
# R version 4.4.2 (2024-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sequoia 15.6.1
##############################

# there are a few ways to get quarterly data, I'm commenting all of these out because
# i ultimately took the coward's way out and just copied the data from bball ref, but
# these have worked in the past

library(hoopR)
library(stringr)
library(tidyverse)
library(janitor)
library(ggbump)

# 
# # Get Wizards quarter-level stats from play-by-play data
# get_wizards_quarter_stats <- function(seasons = 2026) {
#   
#   cat("Loading play-by-play data for season(s):", seasons, "\n")
#   cat("This may take a few minutes...\n")
#   
#   # Load play-by-play data
#   pbp <- load_nba_pbp(seasons = seasons)
#   
#   cat("Loaded", nrow(pbp), "plays\n")
#   
#   # Filter for Wizards games only
#   wiz_games <- pbp %>%
#     filter(home_team_name == "Washington" | away_team_name == "Washington")
#   
#   cat("Found", length(unique(wiz_games$game_id)), "Wizards games\n")
#   
#   # Calculate quarter-level player stats
#   cat("Calculating player quarter stats...\n")
#   player_quarter_stats <- calculate_player_quarter_stats(wiz_games)
#   
#   # Calculate quarter-level team stats
#   cat("Calculating team quarter stats...\n")
#   team_quarter_stats <- calculate_team_quarter_stats(wiz_games)
#   
#   return(list(
#     player_quarter_stats = player_quarter_stats,
#     team_quarter_stats = team_quarter_stats,
#     raw_pbp = wiz_games
#   ))
# }
# 
# # Calculate player-level quarter statistics
# calculate_player_quarter_stats <- function(pbp_data) {
#   
#   # Determine if Wizards are home or away for each game
#   game_info <- pbp_data %>%
#     distinct(game_id, game_date, home_team_name, away_team_name) %>%
#     mutate(
#       wiz_location = if_else(home_team_name == "Washington", "Home", "Away"),
#       wiz_team_id = if_else(home_team_name == "Washington", home_team_id, away_team_id),
#       opponent = if_else(home_team_name == "Washington", 
#                          paste(away_team_name, away_team_mascot), 
#                          paste(home_team_name, home_team_mascot))
#     )
#   
#   # Get Wizards team ID(s)
#   wiz_team_ids <- unique(game_info$wiz_team_id)
#   
#   # Filter for plays involving Wizards players
#   wiz_plays <- pbp_data %>%
#     filter(team_id %in% wiz_team_ids)
#   
#   # Aggregate statistics by player and quarter
#   player_stats <- wiz_plays %>%
#     group_by(game_id, period_number, athlete_id_1) %>%
#     summarise(
#       # Shooting stats
#       fga = sum(shooting_play == TRUE & !is.na(shooting_play), na.rm = TRUE),
#       fgm = sum(shooting_play == TRUE & scoring_play == TRUE & 
#                   !is.na(shooting_play) & !is.na(scoring_play), na.rm = TRUE),
#       
#       # Three pointers
#       three_pa = sum(shooting_play == TRUE & points_attempted == 3, na.rm = TRUE),
#       three_pm = sum(shooting_play == TRUE & scoring_play == TRUE & 
#                        points_attempted == 3, na.rm = TRUE),
#       
#       # Two pointers
#       two_pa = sum(shooting_play == TRUE & points_attempted == 2, na.rm = TRUE),
#       two_pm = sum(shooting_play == TRUE & scoring_play == TRUE & 
#                      points_attempted == 2, na.rm = TRUE),
#       
#       # Free throws (type_text contains "Free Throw")
#       fta = sum(str_detect(type_text, "Free Throw"), na.rm = TRUE),
#       ftm = sum(str_detect(type_text, "Free Throw") & scoring_play == TRUE, na.rm = TRUE),
#       
#       # Rebounds
#       rebounds = sum(str_detect(type_text, "Rebound"), na.rm = TRUE),
#       off_reb = sum(str_detect(type_text, "Offensive Rebound"), na.rm = TRUE),
#       def_reb = sum(str_detect(type_text, "Defensive Rebound"), na.rm = TRUE),
#       
#       # Assists
#       assists = sum(str_detect(type_text, "Assist"), na.rm = TRUE),
#       
#       # Steals
#       steals = sum(str_detect(type_text, "Steal"), na.rm = TRUE),
#       
#       # Blocks
#       blocks = sum(str_detect(type_text, "Block"), na.rm = TRUE),
#       
#       # Turnovers
#       turnovers = sum(str_detect(type_text, "Turnover"), na.rm = TRUE),
#       
#       # Fouls
#       fouls = sum(str_detect(type_text, "Foul"), na.rm = TRUE),
#       
#       # Points scored
#       points = sum(score_value, na.rm = TRUE),
#       
#       .groups = "drop"
#     ) %>%
#     rename(
#       athlete_id = athlete_id_1,
#       quarter = period_number
#     ) %>%
#     filter(!is.na(athlete_id))
#   
#   # Add calculated percentages
#   player_stats <- player_stats %>%
#     mutate(
#       fg_pct = if_else(fga > 0, fgm / fga, NA_real_),
#       three_p_pct = if_else(three_pa > 0, three_pm / three_pa, NA_real_),
#       two_p_pct = if_else(two_pa > 0, two_pm / two_pa, NA_real_),
#       ft_pct = if_else(fta > 0, ftm / fta, NA_real_),
#       efg_pct = if_else(fga > 0, (fgm + 0.5 * three_pm) / fga, NA_real_)
#     )
#   
#   # Join with game info
#   player_stats <- player_stats %>%
#     left_join(game_info %>% select(game_id, game_date, wiz_location, opponent), 
#               by = "game_id")
#   
#   # Reorder columns
#   player_stats <- player_stats %>%
#     select(game_id, game_date, quarter, wiz_location, opponent, athlete_id,
#            points, fgm, fga, fg_pct, 
#            three_pm, three_pa, three_p_pct,
#            two_pm, two_pa, two_p_pct,
#            ftm, fta, ft_pct,
#            rebounds, off_reb, def_reb,
#            assists, steals, blocks, turnovers, fouls,
#            efg_pct)
#   
#   return(player_stats)
# }
# 
# # Calculate team-level quarter statistics
# calculate_team_quarter_stats <- function(pbp_data) {
#   
#   # Determine if Wizards are home or away for each game
#   game_info <- pbp_data %>%
#     distinct(game_id, game_date, home_team_name, home_team_id, 
#              away_team_name, away_team_id) %>%
#     mutate(
#       wiz_location = if_else(home_team_name == "Washington", "Home", "Away"),
#       wiz_team_id = if_else(home_team_name == "Washington", home_team_id, away_team_id),
#       opp_team_id = if_else(home_team_name == "Washington", away_team_id, home_team_id),
#       opponent = if_else(home_team_name == "Washington", 
#                          paste(away_team_name, away_team_mascot), 
#                          paste(home_team_name, home_team_mascot))
#     )
#   
#   # Calculate quarter-level scoring
#   team_stats <- pbp_data %>%
#     group_by(game_id, period_number) %>%
#     summarise(
#       wiz_points = max(if_else(home_team_name == "Washington", 
#                                home_score, away_score), na.rm = TRUE) - 
#         lag(max(if_else(home_team_name == "Washington", 
#                         home_score, away_score), na.rm = TRUE), 
#             default = 0),
#       opp_points = max(if_else(home_team_name == "Washington", 
#                                away_score, home_score), na.rm = TRUE) - 
#         lag(max(if_else(home_team_name == "Washington", 
#                         away_score, home_score), na.rm = TRUE), 
#             default = 0),
#       .groups = "drop"
#     )
#   
#   # Alternative: Calculate from play-by-play score changes
#   team_stats_alt <- pbp_data %>%
#     left_join(game_info %>% select(game_id, wiz_team_id, opp_team_id), 
#               by = "game_id") %>%
#     filter(scoring_play == TRUE) %>%
#     group_by(game_id, period_number) %>%
#     summarise(
#       wiz_points = sum(if_else(team_id == wiz_team_id, score_value, 0), na.rm = TRUE),
#       opp_points = sum(if_else(team_id == opp_team_id, score_value, 0), na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   # Use the alternative calculation (more reliable)
#   team_stats <- team_stats_alt
#   
#   # Add game info and calculate differentials
#   team_stats <- team_stats %>%
#     left_join(game_info %>% select(game_id, game_date, wiz_location, opponent), 
#               by = "game_id") %>%
#     mutate(
#       point_diff = wiz_points - opp_points,
#       quarter = period_number
#     ) %>%
#     select(game_id, game_date, quarter, wiz_location, opponent,
#            wiz_points, opp_points, point_diff)
#   
#   return(team_stats)
# }
# 
# # Convenience function to get just this season's data
# get_current_season_stats <- function() {
#   current_season <- most_recent_nba_season()
#   cat("Fetching current season:", current_season, "\n")
#   get_wizards_quarter_stats(seasons = current_season)
# }
# 


# load quarterly data copied from basketball-reference tables

quarterly_data <- read_csv("quarterly_data.csv") |> clean_names() |> mutate(player = ifelse(player == "Cham Whittmore", "Cam Whitmore", player))

# do a lil cleaning
df <- quarterly_data |> 
  mutate(quarter = factor(per, levels = c("Q1", "Q2", "Q3", "Q4", "OT1"))) |> 
  select(player, quarter, opp, c(mp:pts)) |> 
  pivot_longer(cols = c(mp:pts)) |> filter(quarter != "OT1")

# summary stats for the post
df |> filter(name == "fga") |> 
  mutate(half = ifelse(quarter %in% c("Q1", "Q2"), "First Half", "Second Half")) |> 
  group_by(player, half) |> 
  summarise(value = mean(value, na.rm=T)) |> 
  group_by(player) |> 
  mutate(diff = value-lag(value)) |> 
  drop_na() |>
  arrange( diff)



# more summary stats
df |> filter(name == "fg_percent") |> 
  mutate(half = ifelse(quarter %in% c("Q1", "Q2"), "First Half", "Second Half")) |> 
  group_by(player, half) |> 
  summarise(value = mean(value, na.rm=T)) |> 
  group_by(player) |> 
  mutate(diff = value-lag(value)) |> 
  drop_na() |>
  arrange( diff)

# and minutes played for the post

df |> filter(name == "mp") |> 
  mutate(half = ifelse(quarter %in% c("Q1", "Q2"), "First Half", "Second Half")) |> 
  group_by(player, half) |> 
  summarise(value = sum(value, na.rm=T)) |> 
  group_by(player) |> 
  mutate(diff = value-lag(value)) |> 
  drop_na() |>
  arrange( diff)

# viz for FGA for top 10 players

p1 <- df |> filter( name %in% c("fga")) |> 
  ggplot(aes(x = quarter, y = value)) +
  geom_bump(color = "#EE404E", alpha = 0.2, aes(group = opp)) +
  geom_smooth(aes(group = player, col = "#440154FF"), se = F) +
  usaidplot::usaid_plot(ppt = T) +
  theme(panel.grid.major.x = element_blank()) +
  facet_wrap(~player, ncol = 5
             , scales = "free_y"
  ) + labs(x = "", y = "Field Goal Attempts"
           , title = "Field Goal Attempts by Quarter for the Wizards Main 10 Players"
           , subtitle = "Only Marvin Bagley III and Justin Champagnie has seen higher average second half attempts compared to their first half numbers"
           , caption = "Data: basketball-reference.com\nwizardspoints.substack.com")


ggsave('quarterly.png', p1, width = 16, height = 12, dpi = 600)


# first half-second half total attempts with minute sizing

p2 <- quarterly_data |> select(player, per, opp, mp, fga) |> 
  mutate(quarter = factor(per, levels = c("Q1", "Q2", "Q3", "Q4", "OT1"))) |> 
  filter(quarter!= "OT1") |> 
  mutate(half = ifelse(quarter %in% c("Q1", "Q2"), "First Half", "Second Half")) |> 
  group_by(player, half) |> 
  summarise(fga = sum(fga, na.rm=T)
            , mp = sum(mp, na.rm=T)) |> 
  group_by(player) |> 
  mutate(diff = fga-lag(fga)) |> 
  drop_na() |>
  arrange( diff) |> 
  ggplot(aes(x = diff, y = reorder(player, diff))) +
  geom_linerange(aes(group = player, xmax = diff, xmin = 0)) +
  geom_point(aes(size = mp, col = mp)) +
  geom_text(aes(label = player, hjust = ifelse(diff > 0, 1.05, -0.28)), vjust = -0.6) +
  scale_size_continuous(range = c(1, 12)) +
  usaidplot::usaid_plot(ppt = T, data_type = 'continuous') +
  theme(axis.text.y = element_blank()) +
  labs(x = "First Half-Second Half Difference in Total Field Goal Attempts", y = ""
       , title = "Difference in field goal attempts by the Wizards main core\nbetween the first half and second half of games"
       , subtitle = "Points are sized and shaded by minutes per player\nEven guys getting second half minutes aren't going for more shots"
       , caption = "Data: basketball-reference.com\nwizardspoints.substack.com")

ggsave('halves.png', p2, width = 12, height = 8, dpi = 600)


# same as above but with mean instead of totals
df2 <- quarterly_data |> select(player, per, opp, mp, fga) |> 
  mutate(quarter = factor(per, levels = c("Q1", "Q2", "Q3", "Q4", "OT1"))) |> 
  filter(quarter!= "OT1") |> 
  mutate(quarter = as.numeric(quarter)) |> 
  select(-per, -opp, -player)

quarterly_data |> select(player, per, opp, mp, fga) |> 
  mutate(quarter = factor(per, levels = c("Q1", "Q2", "Q3", "Q4", "OT1"))) |> 
  filter(quarter!= "OT1") |> 
  mutate(half = ifelse(quarter %in% c("Q1", "Q2"), "First Half", "Second Half")) |> 
  group_by(player, half) |> 
  summarise(fga = mean(fga, na.rm=T)
            , mp = mean(mp, na.rm=T)) |> 
  ggplot(aes(x = half, y = fga)) +
  geom_point(aes(size = mp)) +
  geom_line(aes(group = player)) +
  geom_label(aes(label = player))

ggsave('halves_mean.png', p2, width = 12, height = 8, dpi = 600)

