##############################################################################
# This file looks at coach Wes Unseld Jr.'s performance
# R version 4.3.2 (2023-10-31)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sonoma 14.2.1
##############################################################################

# load libraries
library(tidyverse)
library(nbastatR)
library(viridis)
library(readr)
library(janitor)
library(usaidplot)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# import coaching data
coach_df <- read_csv("wiz_coaches.csv") |> 
  clean_names() |> 
  mutate_at(.vars = c(1, 4, 5, 6, 7), as.numeric) |> 
  tail(-8) |> 
  mutate(name_season = paste0(name, " ", season))

p1 <- coach_df |> 
  arrange(win_percent) |> 
  filter(win_percent!=0) |> 
  mutate(wes = ifelse(name == "Wes Unseld Jr.", "Wes", "Other")) |> 
  ggplot(aes(x = win_percent, y = reorder(name, win_percent))) +
  geom_col(aes(fill = wes), col = "white") + 
  usaid_plot() +
  scale_fill_manual(values = c("#6b6e6e","#b50d0d")) +
  scale_y_discrete(expand = c(0,0)) +
  geom_text(aes(label = paste0(round(win_percent*100, 1), "%")), family = "Gill Sans", hjust = 1.1, color = "white", size = 8) +
  theme(panel.grid.major.y = element_blank()
        , axis.text.x = element_blank()
        , panel.grid.major.x = element_blank()
        , axis.text.y = element_text(size = 26)
        , title = element_text(size = 30)
  ) +
  labs(x = "", y = ""
       , title = "Win Percentage by Coach 1973-2024"
       , caption = "data: basketball-reference.com\nwizardspoints.substack.com"
  ) + coord_cartesian(expand = FALSE)

ggsave("coach win percent.png", p1, w = 16, h = 14, dpi = 300)

# get game data
df_game <- game_logs(seasons = c(1973:2024), result_types = c("team"), season_types = c("Regular Season")) 

# wiz games only
df_wiz <- df_game |> filter(slugTeam == "WAS" | slugOpponent == "WAS")

# set up data for formula
df_wiz_wide <- df_wiz |> 
  filter(slugTeam != "WAS") |> 
  select(yearSeason
         , slugSeason
         , idGame
         , "opp_points" = ptsTeam) |> 
  left_join(df_wiz |> 
              filter(slugTeam == "WAS") |> 
              select(yearSeason
                     , slugSeason
                     , idGame
                     , slugTeam
                     # , slugOpponent
                     , "wiz_points" = ptsTeam)
  ) |> select(-slugTeam) |> 
  ungroup() |> 
  left_join(df_game |> ungroup() |> select(idGame, slugSeason, yearSeason, dateGame)) |> unique() |> 
  mutate(yearSeason = ifelse(dateGame>= "2012-01-25" & dateGame<"2016-10-27", paste0(yearSeason, "RW"), yearSeason)
        , slugSeason =  ifelse(dateGame>= "2012-01-25" & dateGame<"2016-10-27", paste0(slugSeason, "RW"), slugSeason)                  
          )
            
# set up formula
k <- 14 # Pythagorean exponent 

# a df just with our expected win %
expected_df <- df_wiz_wide %>%
  group_by(yearSeason, slugSeason) |>
  summarize(opp_points = sum(opp_points, na.rm=T)
            , wiz_points = sum(wiz_points, na.rm=T)
            , games = n()
            ) |> 
  ungroup() |> 
  mutate(expected_wins = round(games*((wiz_points^k) / ((wiz_points^k) + (opp_points^k))), 0)
         , expected_win_percentage = expected_wins/games
         )

# now lets get actual win %

actual_df <- df_game |> filter(slugTeam == "WAS") |> 
  mutate(yearSeason = ifelse(dateGame>= "2012-01-25" & dateGame<"2016-10-27", paste0(yearSeason, "RW"), yearSeason)
         , slugSeason =  ifelse(dateGame>= "2012-01-25" & dateGame<"2016-10-27", paste0(slugSeason, "RW"), slugSeason)                  
  ) |> 
  group_by(yearSeason, slugSeason) |> 
  summarise(win_percentage = mean(isWin)) 


coach_df2 <- actual_df |> 
  left_join(expected_df |> select(yearSeason, slugSeason, expected_win_percentage)) |> 
  mutate(coach = case_when(yearSeason %in% c("2010", "2011", "2012") ~ "Flip Saunders"
                           , yearSeason %in% c(paste0(seq(from= 2012, to = 2016, by =1), "RW")) ~ "Randy Wittman"
                           , yearSeason %in% c("2017", "2018", "2019", "2020", "2021") ~ "Scott Brooks"
                           , yearSeason %in% c("2022", "2023", "2024") ~ "Wes Unseld Jr."
                           )
  ) |> 
  drop_na()

coach_df3 <- coach_df2 |> left_join(expected_df) |> 
  mutate(expected_losses = games-expected_wins) |> 
  group_by(coach) |> summarize(total_expected_wins = sum(expected_wins), total_expected_losses = sum(expected_losses), total_games = sum(games)) |> 
  mutate(expected_win_pct = total_expected_wins/total_games) |> 
  ungroup() |> 
  select("name" = coach, expected_win_pct) |> 
  left_join(coach_df)

  
p2 <- coach_df3 |> 
  pivot_longer(cols = c(expected_win_pct, win_percent), names_to = "type", values_to = "pct") |> 
  mutate(type = ifelse(type == "expected_win_pct", "Expected Win %", "Actual Win %")) |> 
  ggplot(aes(y = reorder(name, pct), x = pct, fill = type)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5) +
  usaid_plot() +
  geom_text(aes(label = paste0(round(pct*100, 1), "%")),
            position = position_dodge(width = 0.5)
            , hjust = 1
            , family = "Gill Sans"
            , color = "white"
            , size = 8
  ) +
  theme(legend.position = "top"
        , panel.grid.major.y = element_blank()
        , axis.text.x = element_blank()
        , panel.grid.major.x = element_blank()
        , axis.text.y = element_text(size = 26)
        , title = element_text(size = 30)
  ) +
  labs(x = "", y = ""
       , title = "Expected and Actual Win Percentage Since 2009"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 

ggsave("expected coach win percent.png", p2, w = 14, h = 10, dpi = 300)

# 15+ points losses
df_wiz_wide |> 
     filter(yearSeason>=2010) |> 
       left_join(coach_df2 |> select(yearSeason, coach)) |> 
       group_by(dateGame, slugSeason, yearSeason, coach) |> 
       summarize(diff = wiz_points-opp_points) |> filter(diff<=-15) |> group_by(coach) |> count()


df_wiz_wide |> 
  filter(yearSeason>=2010) |> 
  left_join(coach_df2 |> select(yearSeason, coach)) |> 
  group_by(dateGame, slugSeason, yearSeason, coach) |> 
  summarize(diff = wiz_points-opp_points) |> filter(diff>=-5 & diff<0) |> group_by(coach) |> count()


p3 <- df_wiz_wide |> 
  filter(yearSeason>=2010) |> 
  left_join(coach_df2 |> select(yearSeason, coach)) |> 
  group_by(dateGame, slugSeason, yearSeason, coach) |> 
  summarize(diff = wiz_points-opp_points) |> 
  group_by(coach) |> 
  mutate(min = min(diff)
            , max = max(diff)
            , mean = mean(diff)) |> 
  ggplot() + 
  geom_pointrange(aes(xmin = min, xmax = max, x = mean, y = reorder(coach, mean)), alpha = 0.2, size = 0.5) +
  geom_jitter(aes(x = diff, y = reorder(coach, mean), color = diff), height = 0.2, alpha = 0.35, size = 4) +
  geom_label(aes(x = mean, y = reorder(coach, mean), label = round(mean, 2), color = mean), family = "Gill Sans", size = 8) +
  scale_color_viridis(option = "magma") +
  # scale_fill_viridis(option = "magma") +
  theme(legend.position = "NA", 
        legend.background = ggplot2::element_blank()
        , legend.title = ggplot2::element_blank(), 
        legend.key = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
        axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = ggplot2::element_line(color = "#CFCDC9"), 
        panel.background = ggplot2::element_blank(), strip.background = ggplot2::element_rect(fill = "white"), 
        plot.title.position = "plot"
        , axis.text.y = element_text(size = 26, family = "Gill Sans")
        , title = element_text(size = 30, family = "Gill Sans")
        , text = element_text(family = "Gill Sans")
        , axis.text.x = element_text(size = 18, family = "Gill Sans")
  ) +
  labs(x = "Point Differential", y = ""
       , title = "Point differential by Wizards coach since 2009"
       , subtitle = "Average shown for each coach"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  )   

ggsave("point differential by coach.png", p3, w = 14, h = 10, dpi = 300)


p4 <- df_wiz_wide |> 
  filter(yearSeason>=2010) |> 
  left_join(coach_df2 |> select(yearSeason, coach)) |> 
  group_by(dateGame, slugSeason, yearSeason, coach) |> 
  summarize(diff = wiz_points-opp_points) |> 
  group_by(coach) |> 
  mutate(min = min(diff)
         , max = max(diff)
         , mean = mean(diff)) |> 
  ggplot() + 
  ggbeeswarm::geom_quasirandom(aes(x = diff, y = reorder(coach, mean), fill = diff), size = 4, color = "#666666", shape = 21) +
  geom_label(aes(x = mean, y = reorder(coach, mean), label = round(mean, 2)),color = "#c451a5", family = "Gill Sans", size = 8) +
  scale_color_viridis(option = "magma") +
  scale_fill_viridis(option = "magma") +
  theme(legend.position = "NA", 
        legend.background = ggplot2::element_blank()
        , legend.title = ggplot2::element_blank(), 
        legend.key = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
        axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = ggplot2::element_line(color = "#CFCDC9"), 
        panel.background = ggplot2::element_blank(), strip.background = ggplot2::element_rect(fill = "white"), 
        plot.title.position = "plot"
        , axis.text.y = element_text(size = 26, family = "Gill Sans")
        , title = element_text(size = 30, family = "Gill Sans")
        , text = element_text(family = "Gill Sans")
        , axis.text.x = element_text(size = 18, family = "Gill Sans")
  ) +
  labs(x = "Point Differential", y = ""
       , title = "Point differential by Wizards coach since 2009"
       , subtitle = "Average shown for each coach"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  )   

ggsave("point differential by coach beeswarm.png", p4, w = 14, h = 10, dpi = 300)
