###############################################
# PACE
# Session Info:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
###############################################

# Load packages and set things up-------
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ballr)
library(rvest)
library(httr)
library(jsonlite)
library(teamcolors)
library(readxl)
library(geomtextpath)
library(tidybayes)
library(rstanarm)
library(bayesplot)


# set seed
set.seed(20483789)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# headers for scraping
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

# Download initial data----

# get nba team ids
tms <- nba_teams()
tms <- tms %>% 
  filter(isNonNBATeam == 0) %>% 
  select(nameTeam, slugTeam, idTeam)

# game logs
dat_team <- game_logs(seasons = 2022, result_types = "team")

# get box scores
dat_team2 <- box_scores(game_ids = dat_team$idGame, league = "NBA", result_types = "team")
  
# data for players
dat_player <- game_logs(seasons = c(2022), season_types = "Regular Season", result_types = c("player"))

# filter for just the wizards
dat_player_wiz <- dat_player %>% filter(nameTeam == "Washington Wizards")

# get games with  Dinwiddie
dat_player_din <- dat_player_wiz %>% filter(namePlayer == "Spencer Dinwiddie") %>% 
  select(numberGameTeamSeason)

# games with  Beal
dat_player_beal <- dat_player_wiz %>% filter(namePlayer == "Bradley Beal") %>% 
  select(numberGameTeamSeason)

# games with Dinwiddie and no Beal
spence_games <- dat_player_din %>% filter(!numberGameTeamSeason %in% dat_player_beal$numberGameTeamSeason)

# Scrape NBA.com------
# get just Wiz in-game pace
res_wiz <- GET(url = "https://stats.nba.com/stats/teamdashboardbygamesplits?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&Split=ingame&TeamID=1610612764&VsConference=&VsDivision=", add_headers(.headers=headers))

json_resp <- fromJSON(content(res_wiz, "text"))

headings <- data.frame(json_resp$resultSets$headers[3])

dat1 <- data.frame(json_resp$resultSets$rowSet[3])  

names(dat1) <- headings$c..GROUP_SET....GROUP_VALUE....GP....W....L....W_PCT....MIN...

# take a look
dat1 %>% 
  rename(Quarter = GROUP_VALUE
         , Pace = PACE) %>% 
  mutate(Quarter = as.numeric(Quarter)
         , Pace = as.numeric(Pace)) %>% 
  filter(Quarter <5) %>% 
  ggplot(aes(x = Quarter, y = Pace, group = GROUP_SET)) +
  geom_step( size = 3) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) 

# Scrape the whole NBA-----
# now lets do that for every team
id <- tms$idTeam

team_pace <- NULL

for(i in 1:length(id)){
  
  url <- paste0("https://stats.nba.com/stats/teamdashboardbygamesplits?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&Split=ingame&TeamID=" 
                  , id[i], "&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[3]) %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[3]))
  
  tmp_dat$team <- id[i]
  
  team_pace[[i]] <- tmp_dat
  
  
}

pace_df <- bind_rows(team_pace, .id = "column_label")


names_pace <- data.frame(headers = json_res2p$resultSets$headers[3])

names_pace2 <- c(names_pace$c..GROUP_SET....GROUP_VALUE....GP....W....L....W_PCT....MIN...)

names(pace_df)[2:48] <- names_pace2

pace_df <- pace_df %>% left_join(tms, by = c("team" = "idTeam"))


# get team colors
tm.colors <- teamcolors
tm.colors <- tm.colors %>% 
  filter(league == "nba") %>% 
  select("nameTeam" = name, primary) %>% 
  mutate(primary = case_when(
    nameTeam == "Golden State Warriors" ~ "#1D428A",
    nameTeam == "Indiana Pacers" ~ "#002D62",
    nameTeam == "Los Angeles Lakers" ~ "#552583",
    nameTeam == "San Antonio Spurs" ~ "#000000",
    nameTeam == "Oklahoma City Thunder" ~ "#EF3B24",
    nameTeam == "Charlotte Hornets" ~ "#00788C",
    nameTeam == "Utah Jazz" ~ "#00471B",
    nameTeam == "New Orleans Pelicans" ~ "#0C2340",
    TRUE ~ primary
  )) %>% 
  bind_rows(tibble(nameTeam = "League Average", primary = "#666666"))

cols <- tm.colors %>% arrange(nameTeam) %>% select(primary)

# let's just look at league average vs. Wiz-----
cols_wiz_avg <- tm.colors %>% 
  filter(nameTeam %in% c("League Average", "Washington Wizards")) %>% 
           arrange(nameTeam) %>% 
           select(primary) # %>% 
  # bind_rows(tibble(primary = "#ed174c"))

last_game <- tibble(Quarter = c(1:4)
                    , Pace = c(96, 92, 84, 90)
                    , Team = "Wizards win vs. Sixers, Feb. 2")

p1 <- pace_df %>% 
  rename(Quarter = GROUP_VALUE
         , Pace = PACE) %>% 
  mutate(Quarter = as.numeric(Quarter)
         , Pace = as.numeric(Pace)
  ) %>% 
  filter(Quarter <5) %>% 
  group_by(Quarter) %>% 
  summarize(Pace = mean(Pace, na.rm=T)) %>% 
  mutate(Team = "League Average") %>% 
  bind_rows(dat1 %>% 
              rename(Quarter = GROUP_VALUE
                     , Pace = PACE) %>% 
              mutate(Quarter = as.numeric(Quarter)
                     , Pace = as.numeric(Pace)
                     , Team = "Wizards"
              ) %>% 
              filter(Quarter <5) %>% select(Quarter, Pace, Team)
  ) %>% 
  # bind_rows(last_game) %>% 
  ggplot(aes(x = Quarter, y = Pace, group = Team
             , col = Team
  )
  ) +
  geom_step( size = 2) +
  scale_color_manual(values = cols_wiz_avg$primary) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  facet_grid(~Team) + 
  labs(title = "NBA pace and Wizards pace by quarter for the season so far"
       , x = "Quarter", y = "Pace"
       , caption = "wizardspoints.substack.com\ndata: nba.com") 

ggsave("Wiz pace.png", p1, width = 10, height = 8, dpi = 300, type = 'cairo')


# true pace----
pace_df %>% 
  rename(Quarter = GROUP_VALUE
         , Pace = PACE) %>% 
  mutate(Quarter = as.numeric(Quarter)
         , Pace = as.numeric(Pace)
  ) %>% 
  filter(Quarter <5) %>% 
  group_by(Quarter) %>% 
  summarize(Pace = mean(Pace, na.rm=T)) %>% 
  mutate(Team = "League Average") %>% 
  bind_rows(dat1 %>% 
              rename(Quarter = GROUP_VALUE
                     , Pace = PACE) %>% 
              mutate(Quarter = as.numeric(Quarter)
                     , Pace = as.numeric(Pace)
                     , Team = "Wizards"
              ) %>% 
              filter(Quarter <5) %>% select(Quarter, Pace, Team)
  ) %>% pivot_wider(names_from = Team, values_from = Pace) %>% 
  mutate(pctdiff = ( Wizards-`League Average`)/((`League Average` + Wizards)/2)*100)

# pace across the league-----
p2 <- pace_df %>% 
  rename(Quarter = GROUP_VALUE
         , Pace = PACE) %>% 
  mutate(Quarter = as.numeric(Quarter)
         , Pace = as.numeric(Pace)
  ) %>% 
  filter(Quarter <5) %>% 
  group_by(Quarter) %>% 
  summarize(Pace = mean(Pace, na.rm=T)) %>% 
  mutate(Team = "League Average") %>% 
  bind_rows(pace_df %>% 
              rename(Quarter = GROUP_VALUE
                     , Pace = PACE) %>% 
              mutate(Quarter = as.numeric(Quarter)
                     , Pace = as.numeric(Pace)
                     , Team = nameTeam
              ) %>% 
              filter(Quarter <5) %>% select(Quarter, Pace, Team)
            ) %>% 
  left_join(tm.colors, by = c("Team" = "nameTeam")) %>% 
  ggplot(aes(x = Quarter, y = Pace, group = Team
             , col = Team
  )
  ) +
  geom_step( size = 2) +
  scale_color_manual(values = cols$primary) +
  
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  facet_wrap(~Team, labeller = label_wrap_gen(width=20)) +
  labs(title = "Pace across the NBA by quarter for the season so far"
       , x = "Quarter", y = "Pace"
       , caption = "wizardspoints.substack.com\ndata: nba.com") 


ggsave("NBA pace.png", p2, width = 14, height = 16, dpi = 300, type = 'cairo')

# last wizards game that was good (i.e., against Philly)----
p3 <- last_game %>% 
  ggplot(aes(x = Quarter, y = Pace)
  ) +
  geom_step( size = 2             
             , col = "#ed174c"
) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(title = "Pace for Wizards vs. Sixers on Feb. 2"
       , x = "Quarter", y = "Pace"
       , caption = "wizardspoints.substack.com\ndata: nba.com") 

ggsave("sixers pace.png", p3, width = 14, height = 12, dpi = 300, type = 'cairo')


# Wiz pace and other stats----
# data from https://www.basketball-reference.com/teams/WAS/2022/gamelog-advanced/
games <- read_excel("sportsref_download.xls.xlsx")

games2 <- games %>% 
  select(-Rk, -G, -Date, -at, -Opp...5, -`W/L`) %>% 
  rename("Opp_Points" = Opp...8
         , "Wiz_Points" = Tm
         , "Opponents_RB%" = `ORB%`
         , "Opponents_eFG%" = `OeFG%`
         , "Opponents_TOV%" = `OTOV%`
         , "Opponents_DRB%" = `ODRB%`
         , "Opponents_FT/FGA" = `OFT/FGA`
         ) %>% 
  mutate("NetRtg" = ORtg-DRtg)
  
  
p4 <- games %>% 
  select(-Rk, -G, -Date, -at, -Opp...5, -`W/L`) %>% 
  rename("Points (Opponents)" = Opp...8
         , "Points (Wizards)" = Tm
         , "3pt Attempt Rate" = `3PAr`
         , "Opponents RB%" = `ORB%`
         , "Opponents eFG%" = `OeFG%`
         , "Opponents TOV%" = `OTOV%`
         , "Opponents DRB%" = `ODRB%`
         , "Opponents FT/FGA" = `OFT/FGA`
         , "Free Throw Attempt Rate" = FTr 
  ) %>% 
  pivot_longer(cols = -Pace
               , names_to = "Stat"
               , values_to = "Values") %>%
  # filter(Stat %in% c("TOV%"
  #                    ,"Opponents TOV%"
  #                    , "FT/FGA"
  #                    , "Opponents FT/FGA"
  # )
  # ) %>%
  mutate(Values = ifelse(Stat %in% c("3pt Attempt Rate"
                                     , "FT/FGA"
                                     , "Free Throw Attempt Rate"
                                     , "eFG%"
                                     , "Opponents eFG%"
                                     , "Opponents FT/FGA"
                                     , "TS%"
                                     ), Values*100, Values)) %>%
  ggplot(aes(x = Pace, y = Values)) +
  geom_point(col = "#002b5c") + 
  geom_smooth(method = "lm", se = F, col = "#666666") +
  facet_wrap(~Stat, scales = "free_y", labeller = label_wrap_gen(width=20)) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  # scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Pace", y = ""
       , title = "Pace and everything else"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
  )

ggsave("other stats and pace.png", p4, width = 14, height = 12, dpi = 300, type = 'cairo')


# pace over time
# create a vector just with games that didn't have Beal in them
dates <- games %>% 
  mutate(Dinwiddie = ifelse(G %in% spence_games$numberGameTeamSeason, "Only Dinwiddie, no Beal", " ")
  ) %>% filter(Dinwiddie == "Only Dinwiddie, no Beal") %>% 
  select(Date)


p5 <- games %>% 
  mutate(Dinwiddie = ifelse(G %in% spence_games$numberGameTeamSeason, "Only Dinwiddie, no Beal", " ")
         ) %>%
  select(G, Dinwiddie, Date, Pace) %>% 
  ggplot(aes(Date, Pace)) +
  geom_rect(aes(xmin = dates$Date[1], xmax = dates$Date[1]+1
                , ymin = -Inf
                , ymax = Inf
                ), fill = "dark grey"
            , alpha = 1) +
  geom_rect(aes(xmin = dates$Date[2], xmax = dates$Date[3]
                , ymin = -Inf
                , ymax = Inf
  ), fill = "dark grey"
  , alpha = 0.02) +
  geom_rect(aes(xmin = dates$Date[4], xmax = dates$Date[6]
                , ymin = -Inf
                , ymax = Inf
  ), fill = "dark grey"
  , alpha = 0.02) +
  geom_rect(aes(xmin = dates$Date[7], xmax = dates$Date[9]
                , ymin = -Inf
                , ymax = Inf
  ), fill = "dark grey"
  , alpha = 0.02) +
  geom_rect(aes(xmin = dates$Date[10], xmax = dates$Date[13]
                , ymin = -Inf
                , ymax = Inf
  ), fill = "dark grey"
  , alpha = 0.02) +
  geom_step(size = 2, col = "#002b5c") +
  
  theme_minimal() + 
  theme(text = element_text(size = 20)) +

  labs(x = "", y = "Pace"
       , title = "Wizards pace by game over the season so far"
       , subtitle = "Grey areas denote games without Beal"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
)
  
ggsave("Pace for the season.png", p5, width = 14, height = 7, dpi = 300, type = 'cairo')

# Models----


prior_dist <- rstanarm::cauchy(location = 20)

m1 <- stan_glm(Pace ~ `Opponents_TOV%`, data = games2
               , prior_intercept = prior_dist, 
               prior = prior_dist
               )

plot(m1, pars = "`Opponents_TOV%`", "hist") + labs(x = "Opponents TOV %")

# Ridgelines version of the areas plot
plot(m1, "hist", regex_pars = "Wiz_Points")

  
set.seed(123)
  
# make the parsnip model
bayes_mod <-   
    linear_reg() %>% 
    set_engine("stan", 
               prior_intercept = prior_dist, 
               prior = prior_dist) 
  
  # train the model
bayes_fit <- 
    bayes_mod %>% 
    fit(Pace ~ `TOV%`, data = games2)
  
print(bayes_fit, digits = 5)

new_points <- expand.grid(Wiz_Points = 106.7547
                          )
new_points

bayes_plot_data <- 
  new_points %>% 
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = Wiz_Points)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "Pace") + 
  ggtitle("Bayesian model with t(1) prior distribution")

    
