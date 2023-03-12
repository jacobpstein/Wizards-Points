###############################################
# Quarter-toquarter Shooting
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

id <- wiz_games$idGame


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

nums <- 1:6

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
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)

# this is hacky, but I'm just going to re-run the above for quarters 2-4

quarter2 <- NULL

nums <- 1:6

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
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)
# quarter 3

quarter3 <- NULL

nums <- 1:6

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
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)
# quarter 4

quarter4 <- NULL

nums <- 1:6

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
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)

# combine

quarter_df <- quarter1_df %>% 
  bind_rows(quarter2_df) %>% 
  bind_rows(quarter3_df) %>% 
  bind_rows(quarter4_df) %>% 
  mutate_at(.vars = c(6:66), as.numeric) %>% 
  mutate(month = factor(month, levels = c("October", "November", "December", "January", "February", "March"))
         , mon = case_when(month == "October" ~ 10
                           , month == "November" ~ 11
                           , month == "December" ~ 12
                           , month == "January" ~ 1
                           , month == "February" ~ 2
                           , month == "March" ~ 3)
         , period = ifelse(mon %in% c(10, 11, 12), paste0(mon, "-0", quarter, "-2022"), paste0(mon, "-", quarter, "-2023"))
         , date = as.Date(period, format = "%m-%d-%Y")
  )

# grab names of top five players
top5_df <- quarter_df %>% 
  filter(PLAYER_NAME!= "Washington Wizards") %>% 
  group_by(month, PLAYER_NAME) %>%
  summarize(shots = sum(FGM, na.rm=T)) %>% 
  group_by(month) %>% 
  mutate(rank = dense_rank(desc(shots))
         , Players = ifelse(rank<=5, PLAYER_NAME, "Everybody Else")) %>% 
  ungroup() %>% 
  select(Players) %>% 
  unique() %>% 
  filter(Players != "Everybody Else" 
         & Players != "Rui Hachimura"
         & Players != "Will Barton"
  )

# let's take a look
quarter_df %>% 
  filter(PLAYER_NAME == "Kristaps Porzingis") %>% 
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


# advanced stats-------------
quarter1 <- NULL

nums <- 1:6

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month="
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


names(quarter1_df)[2:77] <- names_quarter1_2

quarter1_df <- quarter1_df %>% mutate(quarter = 1
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)

# this is hacky, but I'm just going to re-run the above for quarters 2-4

quarter2 <- NULL

nums <- 1:6

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month="
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


names(quarter2_df)[2:77] <- names_quarter2_2


quarter2_df <- quarter2_df %>% mutate(quarter = 2
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)
# quarter 3

quarter3 <- NULL

nums <- 1:6

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month="
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


names(quarter3_df)[2:77] <- names_quarter3_2

quarter3_df <- quarter1_df %>% mutate(quarter = 3
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)
# quarter 4

quarter4 <- NULL

nums <- 1:6

for(i in 1:length(nums)){
  
  url <- paste0("https://stats.nba.com/stats/teamplayerdashboard?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month="
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


names(quarter4_df)[2:77] <- names_quarter4_2


quarter4_df <- quarter4_df %>% mutate(quarter = 4
                                      , month = case_when(column_label == 1 ~ "October"
                                                          , column_label == 2 ~ "November"
                                                          , column_label == 3 ~ "December"
                                                          , column_label == 4 ~ "January"
                                                          , column_label == 5 ~ "February"
                                                          , column_label == 6 ~ "March")
)

# combine

quarter_df <- quarter1_df %>% 
  bind_rows(quarter2_df) %>% 
  bind_rows(quarter3_df) %>% 
  bind_rows(quarter4_df) %>% 
  mutate_at(.vars = c(6:66), as.numeric) %>% 
  mutate(month = factor(month, levels = c("October", "November", "December", "January", "February", "March"))
         , mon = case_when(month == "October" ~ 10
                           , month == "November" ~ 11
                           , month == "December" ~ 12
                           , month == "January" ~ 1
                           , month == "February" ~ 2
                           , month == "March" ~ 3)
         , period = ifelse(mon %in% c(10, 11, 12), paste0(mon, "-0", quarter, "-2022"), paste0(mon, "-", quarter, "-2023"))
         , date = as.Date(period, format = "%m-%d-%Y")
  )


# let's take a look
p1 <- quarter_df %>% 
  filter(PLAYER_NAME == "Kristaps Porzingis") %>% 
  ggplot(aes(x = quarter, y = TS_PCT)) +
  geom_line(size =2, col = "#EF7767" ) +
  coord_cartesian(expand = T, clip = "off") +
  facet_grid(~month
             , space = 'free_x'
             , scales = 'free_x'
             # , switch = 'x'
             ) +
  labs(x = "") +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_hline(yintercept = .581, linetype = 2) +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  # theme(strip.placement = 'outside'
        # strip.background.x = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")) +
  labs(x = "Quarter", y = "True Shooting Percentage"
       , title = "Kristaps Porziņģis shooting by quarter and month"
       , subtitle = "The dotted line shows the overall league average"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )

ggsave("KP by qarter.png", p1, w = 14, h = 10, dpi = 300)

quarter_df %>% 
  filter(PLAYER_NAME == "Kristaps Porzingis") %>% 
  ggplot(aes(x = quarter, y = MIN)) +
  geom_line(size =2, col = "#EF7767" ) +
  coord_cartesian(expand = T, clip = "off") +
  facet_grid(~month
             , space = 'free_x'
             , scales = 'free_x'
             # , switch = 'x'
  ) +
  labs(x = "") +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  # theme(strip.placement = 'outside'
  # strip.background.x = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")) +
  labs(x = "Quarter", y = "Minutes"
       , title = "Kristaps Porziņģis minutes by quarter and month"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )

p2 <- quarter_df %>% 
  filter(PLAYER_NAME == "Washington Wizards") %>% 
  ggplot(aes(x = quarter, y = NET_RATING)) +
  geom_line(size =2, col = "#E41134" ) +
  coord_cartesian(expand = T, clip = "off") +
  facet_grid(~month
             , space = 'free_x'
             , scales = 'free_x'
             # , switch = 'x'
  ) +
  # scale_y_continuous(labels = scales::percent_format()) +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  # theme(strip.placement = 'outside'
  # strip.background.x = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")
        , panel.grid.major.y = element_line(color = "grey")) +
  labs(x = "Quarter", y = "Net Rating"
       , title = "Washington Wizards net rating by quarter and month"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )
ggsave("Net Rating by quarter.png", p2, w = 14, h = 10, dpi = 300)

# beal----------

# let's take a look
p3 <- quarter_df %>% 
  filter(PLAYER_NAME == "Bradley Beal") %>% 
  ggplot(aes(x = quarter, y = TS_PCT)) +
  geom_line(size =2, col = "#00265B" ) +
  coord_cartesian(expand = T, clip = "off") +
  facet_grid(~month
             , space = 'free_x'
             , scales = 'free_x'
             # , switch = 'x'
  ) +
  labs(x = "") +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_hline(yintercept = .581, linetype = 2) +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  # theme(strip.placement = 'outside'
  # strip.background.x = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")) +
  labs(x = "Quarter", y = "True Shooting Percentage"
       , title = "Bradley Beal shooting by quarter and month"
       , subtitle = "The dotted line shows the overall league average"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )

ggsave("Beal by quarter.png", p3, w = 14, h = 10, dpi = 300)

p4 <- quarter_df %>% 
  filter(PLAYER_NAME %in% c("Kristaps Porzingis", "Bradley Beal", "Monte Morris", "Daniel Gafford", "Kyle Kuzma")) %>% 
  ggplot(aes(x = quarter, y = TS_PCT)) +
  geom_line(aes(col = PLAYER_NAME), size =2) +
  coord_cartesian(expand = T, clip = "off") +
  facet_grid(~month
             , space = 'free_x'
             , scales = 'free_x'
             # , switch = 'x'
  ) +
  labs(x = "") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("#00265B", "#309975", "#EF7767", "#FFA3E5", "#FFB45B")) +
  geom_hline(yintercept = .581, linetype = 2) +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  # theme(strip.placement = 'outside'
  # strip.background.x = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")
        , legend.position = "top"
        , legend.title = element_blank()) +
  labs(x = "Quarter", y = "True Shooting Percentage"
       , title = "Current Wizards starters shooting by quarter and month"
       , subtitle = "The dotted line shows the overall league average"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )

ggsave("Starters by quarter.png", p4, w = 14, h = 8, dpi = 300)


p5 <- quarter_df %>% 
  filter(PLAYER_NAME %in% c("Jordan Goodwin", "Delon Wright", "Deni Avdija", "Corey Kispert")) %>% 
  ggplot(aes(x = quarter, y = TS_PCT)) +
  geom_line(aes(col = PLAYER_NAME), size =2) +
  coord_cartesian(expand = T, clip = "off") +
  facet_grid(~month
             , space = 'free_x'
             , scales = 'free_x'
             # , switch = 'x'
  ) +
  labs(x = "") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("#286b22", "#dad873", "#B94B75", "#305699")) +
  geom_hline(yintercept = .581, linetype = 2) +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  # theme(strip.placement = 'outside'
  # strip.background.x = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")
        , legend.position = "top"
        , legend.title = element_blank()) +
  labs(x = "Quarter", y = "True Shooting Percentage"
       , title = "Wizards bench shooting by quarter and month"
       , subtitle = "The dotted line shows the overall league average"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )

ggsave("bench by quarter.png", p5, w = 14, h = 8, dpi = 300)

p6 <- quarter_df %>% 
  filter(PLAYER_NAME %in% c("Jordan Goodwin", "Delon Wright", "Deni Avdija", "Corey Kispert")) %>% 
  ggplot(aes(x = (MIN), y = TS_PCT)) +
  geom_point(aes(col = PLAYER_NAME)) +
  geom_smooth(aes(col = PLAYER_NAME), method = "lm", se = F) +
  scale_color_manual(values = c("#286b22", "#dad873", "#B94B75", "#305699")) +
  facet_wrap(~quarter) +
  theme_classic() +
  theme(text = element_text(size = 28, family = "Times New Roman")
        , plot.title = element_text(face = "bold")
        , legend.position = "top"
        , legend.title = element_blank()) +
  labs(x = "Average Minutes", y = "True Shooting Percentage"
       , title = "True Shooting Percentage by Minutes Player for the Wizards Bench\nSplit by Quarter"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )

ggsave("bench minutes by ts.png", p6, w = 14, h = 8, dpi = 300)


# sds by player
quarter_df %>% 
  group_by(PLAYER_NAME) %>% 
  summarize(ts_sd = sd(TS_PCT)) %>% 
  arrange(ts_sd)
