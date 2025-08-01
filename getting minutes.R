############################
# This file looks at rookie minutes over time
# Starting in 2006 when rookies had to go to college
# Session Info:
# R version 4.4.2 (2024-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sequoia 15.5
##############################


# load libraries
library(nbastatR)
library(hoopR)
library(tidyverse)
library(janitor)
library(htmltab)
library(gt)

# set seed
set.seed(202)

Sys.setenv(VROOM_CONNECTION_SIZE = 3613107200*3)

# get rookie seasons and player ids
rookie_info <- nba_playerindex()$PlayerIndex |> 
  filter(FROM_YEAR>=2006) |> 
  clean_names() |> 
  mutate(from_year = as.numeric(from_year)
         , draft_number = as.numeric(draft_number)
         , draft_round = as.numeric(draft_round)
         , draft_round = ifelse(person_id == "202536", 0, draft_round)
         ) |> 
  # there is something wacky in the draft round data, let's fix it
  mutate(draft_round_char = case_when(draft_round == 1 ~ "First Round"
                                      , draft_round == 2 ~ "Second Round"
                                      , TRUE ~ "Undrafted")
  )

# get rookie season info
rookie_seasons <- 2006:2025

# create an empty DF list
df_list <- list()

# should probably update this to a map function
for(i in seq_along(rookie_seasons)) {
  season_str <- year_to_season(rookie_seasons[i])

  stats <- nba_leaguedashplayerstats(
    season = season_str
    , season_types = "Regular Season"
    , per_mode = "PerGame"
    , measure_type = "Advanced"
  )
  
  stats_data <- stats$LeagueDashPlayerStats
  stats_data$season <- season_str
  
  df_list[[i]] <- stats_data
}

df_all <- bind_rows(df_list) |> 
  mutate(season_year = as.numeric(substr(season, 1, 4))
         , MIN = as.numeric(MIN)) |> clean_names() |> 
  rename(person_id = player_id) |> 
  mutate(across(.cols = c(6:78),  as.numeric))

# combine our minute data with our rookie info:
rookie_min <- rookie_info |> 
  left_join(df_all |> select(person_id
                             , min
                            , team_id
                             , team_abbreviation
                             , "from_year" = season_year
                             , season
                             , efg_pct
                             , net_rating
                             , w_pct), by = c("person_id", "from_year")) |> 
  mutate(wiz = case_when(team_abbreviation.y == "WAS" ~ "Wizards"
                         , TRUE ~ "Other Teams")) 

# graph 1--rookie minutes over time
p1 <- rookie_min |> filter(from_year<2025) |> 
  ggplot(aes(x = from_year, y = min, size = min
  )) +
  geom_point(aes(alpha = wiz
                 , fill = wiz
                 , color = wiz
  ), shape = 21, color = "white") +
  stat_summary(geom = "line", fun = "mean", lwd = 1, color = "#D9565CFF") +
  scale_size_continuous(range = c(.2, 7)) +
  scale_alpha_manual(values= c(0.1, 0.8)) +
  usaidplot::usaid_plot(ppt=T) +
  scale_fill_manual(values = c("#D9565CFF", "#172869FF")) +
  labs(x = "", y = "Avg. Minutes per Game" 
       , title =  "First Season Average Rookie Minutes Per Game"
       , subtitle = "The Wizards rookies played above league average minutes\nin 11 of the past 20 seasons"
       , caption = "Data: NBA.com/stats\nwizardspoints.substack.com") +
  geom_text(data = data.frame(x = 2006.14199821774, y = 10.5, label = "League\nAverage"),
            mapping = aes(x = x, y = y, label = label),
            colour = "#D9565CFF", family = "Gill Sans", fontface = 2, inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = 2009.89462668678, y = 40.9329282306026, label = "Each blue point is a Wizards rookie"),
            mapping = aes(x = x, y = y, label = label),
            colour = "#172869FF", family = "Gill Sans", inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = c(2018.90763613473, 2008.31986295424 ),
                              y = c(37.4762422145739, 2.77194547560737),
                              label = c("Large dots mean\nhigher avg. minutes", "Small dots mean\nlower avg. minutes")),
            mapping = aes(x = x, y = y, label = label),
            family = "Gill Sans", inherit.aes = FALSE)

ggsave("minutes_overall.png", p1, dpi = 500, width = 10, height = 8)


rookie_min |>
  filter(!is.na(season)) |>
  group_by(wiz) |>
  summarise(mean_min = mean(min, na.rm = TRUE)
            , sd = sd(min, na.rm =T)
            , .groups = "drop")

p2 <- rookie_min |>
  filter(!is.na(season)) |>
  group_by(wiz, from_year) |>
  summarise(mean_min = mean(min, na.rm = TRUE), .groups = "drop") |>
  group_by(from_year) |>
  mutate(
    vjust_pos = ifelse(mean_min == max(mean_min), -1.1, 1.7)
  ) |>
  ggplot(aes(y = from_year, x = mean_min, fill = wiz)) +
  geom_line(position = "identity", aes(group = from_year), color = "#666666") +
  geom_point(color = "white", shape = 21, aes(size = mean_min)) +
  geom_text(aes(label = round(mean_min), vjust = vjust_pos), family = "Gill Sans") +
  usaidplot::usaid_plot(ppt = TRUE) +
  scale_fill_manual(values = c("#D9565CFF", "#172869FF")) +
  scale_size_continuous(range = c(.2, 7)) +
  coord_flip() +
  labs(y = "", x = "Avg. Minutes per Game" 
       , title =  "First Season Average Rookie Minutes Per Game by Season"
       , subtitle = "The 24-25 season represents a break from the general trend"
       , caption = "Data: NBA.com/stats\nwizardspoints.substack.com") +
  geom_segment(aes(x = 31, xend = 36, y = 2008, yend = 2008), color = "#666666") +
  geom_point(aes(x = 31, y = 2008), fill = "#D9565CFF", color = "white", shape = 21, size = 4) +
  geom_point(aes(x = 36, y = 2008), fill = "#172869FF", color = "white", shape = 21, size = 4) +
  annotate("text", x = 31, y = 2007.8, label = "All other teams", hjust = 1, size = 3.5, family = "Gill Sans") +
  annotate("text", x = 36, y = 2008.2, label = "Wizards", hjust = 0, size = 3.5, family = "Gill Sans") +
  geom_text(data = data.frame(x = 29.7659026776501, y = 2014.7, label = "Bradley Beal was the only rookie\non the team and averaged\n31 min per game!"),
            mapping = aes(x = x, y = y, label = label),
            family = "Gill Sans", inherit.aes = FALSE)
  
ggsave("average_minues.png", p2, dpi = 500, width = 10, height = 8)

# minutes by draft round-----
p3 <- rookie_min |> 
  filter(!is.na(season)) |>
  ggplot(aes(x = from_year, y = min
             ,  fill = wiz
             , color = wiz
             , group = wiz
             )) +
  geom_point(aes(alpha = wiz, size = min), shape = 21, color = "white") +
  geom_smooth(aes(color = wiz)) + 
  scale_size_continuous(range = c(.2, 7)) +
  scale_alpha_manual(values= c(0.1, 0.8)) +
  facet_wrap(~draft_round_char) +
  usaidplot::usaid_plot(ppt = TRUE) +
  scale_fill_manual(values = c("#D9565CFF", "#172869FF")) +
  scale_color_manual(values = c("#D9565CFF", "#172869FF")) +
  scale_size_continuous(range = c(.2, 7), guide = FALSE) +
  theme(legend.position = "top") +
  labs(x = "",  y= "Avg. Minutes per Game" 
       , title =  "First Season Average Rookie Minutes Per Game by Draft Round"
       , subtitle = "Wizards first rounders see more court time than the average rookie"
       , caption = "Data: NBA.com/stats\nwizardspoints.substack.com")

ggsave("minutes_by_round.png", p3, dpi = 500, width = 12, height = 8)


rookie_min |> 
  filter(!is.na(season)) |> 
  group_by(draft_round_char) |> 
  summarise(mean_min = mean(min, na.rm=T))


# if you are in the top ten how do you compare to the second around----
rookie_min |> 
  filter(draft_number <=10 & draft_round_char == "First Round" & from_year==2024) |> 
  summarise(minutes = mean(min, na.rm=T))

rookie_min |> 
  filter(draft_round_char == "First Round" & from_year==2024) |> 
  summarise(minutes = mean(min, na.rm=T))

rookie_min |> 
  filter(draft_round_char == "Second Round" & from_year==2024) |> 
  summarise(minutes = mean(min, na.rm=T))

# 2025 rookie min by team----
rookie_min |> 
  filter(season == "2024-25") |> 
  group_by(team_abbreviation.y) |> 
  summarise(mean_min = mean(min, na.rm=T))


rookie_min |> 
  filter(draft_round_char != "Undrafted") |> 
  ggplot(aes(x = draft_number
             , y = min, fill = draft_round_char, colour = draft_round_char)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggpubr::stat_cor()


# get awards for rookies----
url <- "https://www.basketball-reference.com/awards/roy.html#roy_NBA"


roy_df <- htmltab(doc = url, which = 1) |> clean_names() |> head(n=19)

# average min by season and add in player ids and overall NBA rookie averages
roy_df2 <- rookie_min |> 
  filter(draft_round_char == "First Round") |> 
  group_by(season) |> 
  summarise(mean_min = mean(min, na.rm=T)) |> drop_na() |> 
  arrange(desc(season)) |> 
  bind_cols(roy_df |> select(player, per_game_mp)) |> 
  left_join(rookie_min |> mutate(player = paste0(player_first_name, " ", player_last_name)) |>  
              select(player, person_id)) 

rookie_min |> 
  filter(draft_round_char == "First Round") |> 
  group_by(season) |> 
  summarise(mean_min = mean(min, na.rm=T)) |> drop_na() |> 
  arrange(desc(season)) |> 
  bind_cols(roy_df |> select(player, per_game_mp)) |> 
  left_join(rookie_min |> mutate(player = paste0(player_first_name, " ", player_last_name)) |>  
              select(player, person_id)) |> summarise(mean_league = mean(mean_min, na.rm=T), mean_roy = mean(as.numeric(per_game_mp), na.rm=T))

roy_df2$url <- 
nba_playerheadshot(player_id = roy_df2$person_id)

tab1 <- roy_df2 |> head(n=10) |> 
  select(season
         , player
         , url
         , per_game_mp
         ,  mean_min) |> 
  mutate(mean_min = round(mean_min, 1)) |> 
  gt() |> 
  cols_label(
    season = md("**Season**")
    , player = md("**Player**")
    , url = md("")
    , per_game_mp = md("**Min. Per Game**")
    , mean_min = md("**First Round Avg.**")
  )  |> 
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x)
    }
  ) |> 
  tab_source_note(
    md("source: [basketball-reference.com](https://www.basketball-reference.com)")
  )  %>%
  tab_source_note(
    md("[wizardspoints.substack.com](wizardspoints.substack.com)")
  )

tab1 |>
  gtsave(
    "roy.png", expand = 10
  )

  

rookie_min |> 
  filter(from_year<=2020 & !is.na(position)) |> 
  ggplot(aes(x = from_year, y = net_rating, size = min
             ,  fill = wiz
             , color = wiz
  )) +
  geom_point(alpha = 0.2, shape = 21) +
  stat_summary(geom = "line", fun = "median", lwd = 1) +
  facet_wrap(~draft_round_char)

# rookie performance, team performance-----
rookie_team_min <- rookie_min |> 
  group_by(team_abbreviation.y, team_id.y, from_year, season) |> 
  summarise(mean_min = mean(min, na.rm=T))

# create an empty DF list
df_team <- list()

# should probably update this to a map function
for(i in seq_along(rookie_seasons)) {
  season_str <- year_to_season(rookie_seasons[i])

  stats <- nba_leaguedashteamstats(
    season = season_str
  )
  
  stats_data <- stats$LeagueDashTeamStats
  stats_data$season <- season_str
  
  df_team[[i]] <- stats_data
}

df_team_rookies <- bind_rows(df_team) |> 
  clean_names() |> 
  select(team_id
    , team_name
    , w_pct
    , season
     ) |> 
  mutate(w_pct = as.numeric(w_pct)) |> 
  left_join(rookie_team_min, by = c("team_id" = "team_id.y", "season"))

p4 <- df_team_rookies |> mutate(wiz = ifelse(team_name== "Washington Wizards","Wiz", "Not Wiz")) |> 
  ggplot(aes(x = mean_min, y = w_pct)) +
  geom_point(aes(fill =wiz), shape = 21, alpha = 0.5, size = 5, color = "white") +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent_format()) +
  usaidplot::usaid_plot(ppt = T) +
  scale_fill_manual(values = c("#D9565CFF", "#172869FF")) +
  ggpubr::stat_cor(aes(label = ..r.label..), label.x.npc = "middle") +
  facet_wrap(~season) +
  labs(x = "Average Rookie Minutes Per Game"
       , y = "Team Win %"
       , title = "The relationship between average rookie minutes per game and\nteam win percentage was strong last season but has been weaker in past years"
       , subtitle = "Dark points represent the Wizards average for each season"
       , caption = "Data: NBA.com/stats\nwizardspoints.substack.com")

# without wiz
  # df_team_rookies |> 
  # ggplot(aes(x = mean_min, y = w_pct)) +
  # geom_point(fill = "#D9565CFF", color = "#D9565CFF", shape = 21, alpha = 0.4, size = 5) +
  # geom_smooth(method = "lm", color = "#172869FF") +
  # scale_y_continuous(labels = scales::percent_format()) +
  # usaidplot::usaid_plot(ppt = T) +
  # ggpubr::stat_cor(aes(label = ..r.label..), label.x.npc = "middle") +
  # facet_wrap(~season) +
  # labs(x = "Average Rookie Minutes Per Game"
  #      , y = "Team Win %"
  #      , title = "The relationship between average rookie minutes per game and\nteam win percentage was strong last season but has been weaker in past years"
  #      , caption = "Data: NBA.com/stats\nwizardspoints.substack.com")
  # 
  # 
ggsave("rookie_min_win_pct.png", p4, dpi = 500, width = 14, height = 10)
