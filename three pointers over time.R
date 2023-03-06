###############################################
# Three Point Shooting
# Session Info:
# R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
# Copyright (C) 2022 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20 (64-bit)
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(rvest)
library(janitor)
library(hablar)
library(jsonlite)
library(httr)
library(vroom)
library(lubridate)
library(ggridges)
library(ggrepel)
library(rstanarm)
library(lme4)
library(waffle)
library(viridis)


# set seed
set.seed(20222712)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# get game ids
game_logs(seasons = 2023, result_types = c("team", "player")) 
wiz_game_ids <- dataGameLogsTeam %>% filter(nameTeam == "Washington Wizards")

wiz_games <- wiz_game_ids %>% 
  mutate(newdate = gsub(x=dateGame, pattern = "-", replacement="")
         , current_game = paste0(newdate, "0", slugTeamWinner)
         , lower_name = tolower(slugOpponent)
         , lower_wiz = tolower(slugTeam)
         , match_up = case_when(locationGame== "H" ~ paste0(lower_name, "-vs-", lower_wiz)
                                , locationGame == "A" ~ paste0(lower_wiz, "-vs-", lower_name))
  ) 

# let's look at 3 point attempts over time

wiz_games %>% 
  select(dateGame, fgmTeam, fgaTeam) %>% 
  pivot_longer(cols = c(2:3)) %>% 
  ggplot(aes(x = dateGame, y = value)) +
  geom_area(aes(fill = name)) +
  theme_classic()

id <- wiz_games$idGame


p1 <- wiz_games %>% 
  mutate(month = factor(lubridate::month(dateGame, label = TRUE), levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))) %>% 
  group_by(month) %>% 
  summarize(mean3s = mean(pctFG3Team, na.rm=T)) %>% 
  ggplot(aes(x = month, y = mean3s)) +
  geom_hline(yintercept = 0.36, linetype = 2) +
  geom_linerange(aes(ymin = 0.3, ymax = mean3s, xmin = month, xmax = month)) +
  geom_point(size = 22, shape = 21, stroke = 5, col = '#081d58', fill = "white") +
  geom_text(aes(label = paste0(round(mean3s, 3)*100, "%")), size = 6, family = "Times New Roman") +
  annotate("text", x = 2, y = .362,  label = "League Average", family = "Times New Roman", size = 7) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")) +
  labs(x = "", y = ""
       , title = "Washington Wizards Three Point Percentage by Month"
       , subtitle = "Things are looking...better"
       , caption = "data: basketball-reference.com\nwizardspoints.substack.com"
  )

ggsave("3pt perct by month.png", p1, w = 14, h = 12, dpi = 300, type = "cairo")

# now let's look at player level three point shooting
points_df <- dataGameLogsPlayer %>% 
  filter(idGame %in% id & nameTeam == "Washington Wizards") %>% 
  left_join(wiz_games) %>% 
  select(idGame, namePlayer, outcomeGame, dateGame, fg3m, fg3mTeam, fg3aTeam, pctFG3Team) %>% 
  mutate(player_pct = round(fg3m/fg3mTeam, 3)
         , gameMonth = factor(lubridate::month(dateGame, label = TRUE), levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")) 
  ) 

# huh, KP only made 14 3s in December
# link: https://www.nba.com/stats/player/204001/boxscores-traditional?DateFrom=12%2F01%2F2022&DateTo=12%2F31%2F2022
p2 <- points_df %>% 
  filter(gameMonth %in% c("Dec", "Jan", "Feb")) %>% 
  ungroup() %>% 
  group_by(gameMonth, namePlayer) %>%
  summarize(shots = sum(fg3m, na.rm=T)) %>% 
  group_by(gameMonth) %>% 
  mutate(rank = dense_rank(desc(shots))
         , Players = ifelse(rank<=5, namePlayer, "Everybody Else")) %>% 
  arrange(gameMonth, desc(shots)) %>%
  ggplot() +
  geom_waffle(aes(values = shots, fill = Players)
              , n_rows = 10
              , flip = TRUE
              , color = "white"
                , size = 0.33) +
  coord_equal() +
  facet_wrap(~gameMonth) +
  theme_minimal() +
  theme_enhance_waffle() +
  scale_fill_manual(values = c("#00265B"
    , "#6C3674"
    , "#B94B75"
    , "#C6CFD5"
    , "#EF7767"
    , "#FFA3E5"
    , "#FFB45B"
    , "#E41134"
    , "#55061A"
  )) +
  theme(legend.position = "NA"
        , text = element_text(size = 28, family = "Times New Roman")
        , plot.background = element_rect(fill = "white", color = "white")
  ) +
  labs(x = "", y = ""
       , title = "Top-Five Players Making Threes\nDecember through February"
       , caption = "data: basketball-reference.com\nwizardspoints.substack.com"
  )

ggsave("total 3s by player and month.png", p2, w = 10, h = 8, dpi = 300, type = "cairo")

# percentage of shots belonging to Kuzma
points_df %>% 
  filter(gameMonth %in% c("Dec", "Jan", "Feb")) %>% 
  ungroup() %>% 
  group_by(gameMonth, namePlayer) %>%
  summarize(shots = sum(fg3m, na.rm=T)) %>% group_by(gameMonth) %>% mutate(perct = shots/sum(shots)) %>% filter(namePlayer == "Kyle Kuzma")

# percentage for feb
points_df %>% 
  filter(gameMonth %in% c("Dec", "Jan", "Feb")) %>% 
  ungroup() %>% 
  group_by(gameMonth, namePlayer) %>%
  summarize(shots = sum(fg3m, na.rm=T)) %>% 
  group_by(gameMonth) %>% mutate(perct = shots/sum(shots)) %>% 
  filter(gameMonth == "Feb") %>% 
  arrange(desc(perct))


# let's look by quarter------------

# set up a header for the API

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# get data for all quarter 1s

quarter1 <- NULL

nums <- 1:5

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month="
                , nums[i], "&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=1&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&TeamID=1610612764&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1]) %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  quarter1[[i]] <- tmp_dat
  
}

quarter1_df <- bind_rows(quarter1, .id = "column_label")

names_quarter1 <- data.frame(headers = json_res2p$resultSets$headers[2])

names_quarter1_2 <- c(names_quarter1$c..GROUP_SET....PLAYER_ID....PLAYER_NAME....NICKNAME....GP...)


names(quarter1_df)[2:65] <- names_quarter1_2

quarter1_df <- quarter1_df %>% mutate(quarter = 1
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February")
                                      )

# this is hacky, but I'm just going to re-run the above for quarters 2-4

quarter2 <- NULL

nums <- 1:5

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month="
                , nums[i], "&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=2&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&TeamID=1610612764&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1]) %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  quarter2[[i]] <- tmp_dat
  
}

quarter2_df <- bind_rows(quarter2, .id = "column_label")

names_quarter2 <- data.frame(headers = json_res2p$resultSets$headers[2])

names_quarter2_2 <- c(names_quarter2$c..GROUP_SET....PLAYER_ID....PLAYER_NAME....NICKNAME....GP...)


names(quarter2_df)[2:65] <- names_quarter2_2


quarter2_df <- quarter2_df %>% mutate(quarter = 2
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February")
)
# quarter 3

quarter3 <- NULL

nums <- 1:5

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month="
                , nums[i], "&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=2&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&TeamID=1610612764&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1]) %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  quarter3[[i]] <- tmp_dat
  
}

quarter3_df <- bind_rows(quarter3, .id = "column_label")

names_quarter3 <- data.frame(headers = json_res2p$resultSets$headers[2])

names_quarter3_2 <- c(names_quarter3$c..GROUP_SET....PLAYER_ID....PLAYER_NAME....NICKNAME....GP...)


names(quarter3_df)[2:65] <- names_quarter3_2

quarter3_df <- quarter1_df %>% mutate(quarter = 3
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February")
)
# quarter 4

quarter4 <- NULL

nums <- 1:5

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month="
                , nums[i], "&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=2&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&TeamID=1610612764&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1]) %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  quarter4[[i]] <- tmp_dat
  
}

quarter4_df <- bind_rows(quarter4, .id = "column_label")

names_quarter4 <- data.frame(headers = json_res2p$resultSets$headers[2])

names_quarter4_2 <- c(names_quarter4$c..GROUP_SET....PLAYER_ID....PLAYER_NAME....NICKNAME....GP...)


names(quarter4_df)[2:65] <- names_quarter4_2


quarter4_df <- quarter4_df %>% mutate(quarter = 4
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February")
)

# combine

quarter_df <- quarter1_df %>% 
  bind_rows(quarter2_df) %>% 
  bind_rows(quarter3_df) %>% 
  bind_rows(quarter4_df) %>% 
  mutate_at(.vars = c(6:66), as.numeric) %>% 
  mutate(month = factor(month, levels = c("October", "November", "December", "January", "February"))
         , mon = case_when(month == "October" ~ 10
                           , month == "November" ~ 11
                           , month == "December" ~ 12
                           , month == "January" ~ 1
                           , month == "February" ~ 2)
         , period = ifelse(mon %in% c(10, 11, 12), paste0(mon, "-0", quarter, "-2022"), paste0(mon, "-", quarter, "-2023"))
         , date = as.Date(period, format = "%m-%d-%Y")
  )

# grab names of top five players
top5_df <- points_df %>% 
  filter(gameMonth %in% c("Oct", "Nov", "Dec", "Jan", "Feb")) %>% 
  ungroup() %>% 
  group_by(gameMonth, namePlayer) %>%
  summarize(shots = sum(fg3m, na.rm=T)) %>% 
  group_by(gameMonth) %>% 
  mutate(rank = dense_rank(desc(shots))
         , Players = ifelse(rank<=5, namePlayer, "Everybody Else")) %>% 
  ungroup() %>% 
  select(Players) %>% 
  unique() %>% 
  filter(Players != "Everybody Else" 
         & Players != "Rui Hachimura"
         & Players != "Will Barton"
         )

# let's take a look
quarter_df %>% 
  filter(PLAYER_NAME == "Bradley Beal") %>% 
  ggplot(aes(x = quarter, y = FG_PCT)) +
  geom_smooth(aes(col = PLAYER_NAME), se = F) +
  coord_cartesian(expand = T, clip = "off") +
  facet_grid(~month, space = 'free_x', scales = 'free_x', switch = 'x') +
  labs(x = "") +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  theme(strip.placement = 'outside',
        strip.background.x = element_blank())



# let's look at passing

box_scores(game_ids = id
           , result_types = "Team"
           , box_score_types = "Tracking"
           , join_data = TRUE
           , assign_to_environment = TRUE
           , return_message = TRUE)


  
  
pass1 <- NULL

nums <- 1:6

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamdashptpass?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month="
                , nums[i], "&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&TeamID=1610612764&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1]) %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  pass1[[i]] <- tmp_dat
  
}

pass1_df <- bind_rows(pass1, .id = "column_label")

names_pass1 <- data.frame(headers = json_res2p$resultSets$headers[1])

names_pass1_2 <- c(names_pass1$c..TEAM_ID....TEAM_NAME....PASS_TYPE....G....PASS_FROM....PASS_TEAMMATE_PLAYER_ID...)


names(pass1_df)[2:19] <- names_pass1_2


pass1_df <- pass1_df %>% 
  mutate_at(.vars = c(5, 7:19), as.numeric) %>% 
  mutate(month = case_when(column_label == 1 ~ "Oct"
                           , column_label == 2 ~ "Nov"
                           , column_label == 3 ~ "Dec"
                           , column_label == 4 ~ "Jan"
                           , column_label == 5 ~ "Feb"
                           , column_label == 6 ~ "Mar")
         , month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
  )

p3 <- pass1_df %>% 
  filter(!PASS_FROM %in% c("Carey Jr., Vernon", "Davis, Johnny", "Dotson, Devon", "Jackson, Quenton", "Schakel, Jordan", "Todd, Isaiah")) %>% 
  select(PASS_FROM, PASS, month, PASS_TYPE) %>% 
  mutate(PASS_FROM = sub("(\\w+),\\s(\\w+)","\\2 \\1", PASS_FROM)
         , PASS_TYPE = Hmisc::capitalize(PASS_TYPE)
         ) %>% 
  ggplot(aes(x = month, y = PASS, group = PASS_TYPE)) +
  geom_line(aes(col = PASS_TYPE), size = 1.1) +
  scale_color_manual(values = c("#E41134", "#00265B")
  ) +
  facet_wrap(~PASS_FROM) +
  theme_classic() +
  labs(x = "", y = "Average Number of Passes Per Game"
       , caption = "data: nba.com\nwizardspoints.substack.com"
       , title = "Passes Made and Received by Wizards Player"
       ) +
  theme(legend.position = "top"
        , legend.title = element_blank()
        , panel.grid.major.y = element_line(colour = "#C6CFD5")
        , text = element_text(size = 26, family = "Times New Roman"))

ggsave("passes made and received.png", p3, w = 14, h = 17, dpi = 300, type = "cairo")

pass_to_assist <- pass1_df %>% ggplot(aes(x = PASS, y = AST)) + geom_point(aes(col = month)) + 
    geom_smooth(method = "lm") + facet_wrap(~month) +
    ggpubr::stat_cor(method = "pearson") +
  theme_classic() +
  labs(x = "Passes", y = "Assists"
      , title = "Washington Wizards Passes to Assists by Month"
       , caption = "data: nba.com\nwizardspoints.substack.com"
       ) +
  theme(legend.position = "none")

ggsave("pass to assist.png", pass_to_assist, w = 10, h = 8, dpi = 300, type = "cairo")
