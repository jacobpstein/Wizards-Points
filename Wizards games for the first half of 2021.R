###############################################
# Wizards games for the 2021-2022 season so far
# Session Info:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
###############################################

# load packages
library(lubridate)
library(tidyverse)
library(nbastatR)
library(teamcolors)

# increase download connection size
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)


# Get NBA teams and their names
# Lines 21 through 41 from the F5
# https://thef5.substack.com/p/hex-snowflake-charts
tms <- nba_teams()
tms <- tms %>% 
  filter(isNonNBATeam == 0) %>% 
  select(nameTeam, slugTeam)

# Get NBA team colors
tm.colors <- teamcolors
tm.colors <- tm.colors %>% 
  filter(league == "nba") %>% 
  select(name, primary) %>% 
  mutate(primary = case_when(
    name == "Golden State Warriors" ~ "#1D428A",
    name == "Indiana Pacers" ~ "#002D62",
    name == "Los Angeles Lakers" ~ "#552583",
    name == "San Antonio Spurs" ~ "#000000",
    name == "Oklahoma City Thunder" ~ "#EF3B24",
    name == "Charlotte Hornets" ~ "#00788C",
    name == "Utah Jazz" ~ "#00471B",
    name == "New Orleans Pelicans" ~ "#0C2340",
    TRUE ~ primary
  )) 

# let's get our games
wiz_game_ids <- game_logs(seasons = 2022, result_types = "team") %>% filter(nameTeam == "Washington Wizards")

# fix the game dates
# some of this is legacy work that I have left in for future use
wiz_games <- wiz_game_ids %>% 
  mutate(newdate = gsub(x=dateGame, pattern = "-", replacement="")
                            , current_game = paste0(newdate, "0", slugTeamWinner)
                              ) #%>%
  # select(dateGame, idGame, current_game, locationGame)


# get play-by-play data
wiz <- play_by_play_v2(game_ids = wiz_games$idGame, nest_data = F, return_message = T) 

# join play by play with some of our general team info 
wiz2 <- wiz %>% select("time" = minuteGame, score = slugScore, idGame) %>% 
  na.omit() %>% 
  left_join(select(wiz_games, slugMatchup, locationGame, idGame, slugTeam, slugOpponent, numberGameTeamSeason)) %>% 
  left_join(tms, by = c("slugOpponent" = "slugTeam")) %>% rename(Opponent = nameTeam)

# this next bit comes from https://statisticaloddsandends.wordpress.com/2018/12/13/recreating-the-nba-lead-tracker-graphic/
# split score into visitor and home score, get home advantage

wiz3 <- wiz2 %>% 
  separate(score, into = c("visitor", "home"), sep = " - ") %>%
  mutate(visitor = as.numeric(visitor), 
         home = as.numeric(home)
         , Wizards = case_when(locationGame == "A" ~ visitor
                               , locationGame == "H" ~ home
         )
         , Opps = case_when(locationGame == "A" ~ home
                            , locationGame == "H" ~ visitor)
  ) %>%
  mutate(visitor_adv = visitor - home
         , wiz_adv = Wizards - Opps
         , visitor_lead = pmax(visitor_adv, 0)
         , home_lead = pmin(visitor_adv, 0)
         , wiz_lead = pmax(wiz_adv, 0)
         , opp_lead = pmin(wiz_adv, 0)
  )

wiz_extraSteps <- wiz3 %>% 
  mutate(visitor_adv = lag(visitor_adv)
                                    , wiz_adv = lag(wiz_adv)
                                    , visitor_lead = lag(visitor_lead)
                                    , home_lead = lag(home_lead)
                                    , wiz_lead = lag(wiz_lead)
                                    , opp_lead = lag(opp_lead)
                                      )

wiz4 <- bind_rows(wiz_extraSteps, wiz3) %>%
  arrange(numberGameTeamSeason, time)

# create a dataframe with just our ids and colors for reference
ids_teams <- wiz4 %>% 
  select(idGame, Opponent, slugMatchup) %>% 
  unique() %>% 
  group_by(Opponent) %>% 
  mutate(game_number = seq_along(Opponent)) %>% 
  ungroup() %>% 
  mutate(label= ifelse(game_number == 1, slugMatchup, paste0(slugMatchup, " - Game ", game_number))) %>% 
  left_join(tm.colors, by = c("Opponent" = "name"))

# color vector
cols <- ids_teams$primary

# figure with Wizards on top
p1 <- wiz4 %>% 
  left_join(ids_teams) %>%
  mutate(label = factor(label)
         , label = fct_reorder(label, numberGameTeamSeason, min)) %>% 
  ggplot() +
  geom_ribbon(aes(x = time, ymin = 0, ymax = wiz_lead), fill = "#002b5c") +
  geom_ribbon(aes(x = time, ymin = opp_lead, ymax = 0, 
                  fill = label), alpha = 0.8) +
  geom_vline(xintercept = c(12, 24, 36, 48, 60), linetype = 2, col = "grey") +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60, 72), labels = c("Q1", "Q2", "Q3", "Q4", "OT1", "OT2")) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  theme(legend.position = "NA"
        , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , text = element_text(size = 10)
        , plot.title = element_text(hjust = 0.5, size = 20)
        
        ) +
  labs(title = "Every Wizards Game So Far This Season", x = "", y = "Score Differential"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       ) +
  facet_wrap(~label)

ggsave("Wizards Games So Far.png", p1, width = 10, height = 10, dpi = 300, type = 'cairo')

# season average

p2 <- wiz3 %>% 
  filter(time<=48) %>%
  ggplot() +
  geom_ribbon(aes(x = time, ymin = 0, ymax = wiz_lead), fill = "#002b5c") +
  geom_ribbon(aes(x = time, ymin = opp_lead, ymax = 0), 
              fill = "#BA0C2F") +
  geom_vline(xintercept = c(12, 24, 36, 48), linetype = 2, col = "grey") +
  scale_x_continuous(breaks = c(12, 24, 36, 48), labels = c("Q1", "Q2", "Q3", "Q4")) +
  theme_minimal() +
  theme(legend.position = "NA"
        , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , text = element_text(size = 20)
        , plot.title = element_text(hjust = 0.5, size = 20)
        
  ) +
  labs(title = "Every Wizards Game So Far in One Graph", x = "", y = "Score Differential"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com") 

ggsave("All of the Wizards Games So Far in One Graph.png", p2, width = 12, height = 8, dpi = 300, type = 'cairo')



# where things stand at each minute of the game
p3 <- wiz2 %>% 
  filter(time<=48) %>% 
  separate(score, into = c("visitor", "home"), sep = " - ") %>%
  mutate(visitor = as.numeric(visitor), 
         home = as.numeric(home)
         , Wizards = case_when(locationGame == "A" ~ visitor
                               , locationGame == "H" ~ home
         )
         , Opps = case_when(locationGame == "A" ~ home
                            , locationGame == "H" ~ visitor)
         , time_group = as.numeric(Hmisc::cut2(time, g = 48))
  ) %>%
  group_by(time_group) %>% 
  summarize(Wizards = mean(Wizards, na.rm=T)
            , Opps = mean(Opps, na.rm=T)) %>% mutate(diff = Wizards - Opps) %>%
  ggplot(aes(x = time_group, y = diff)) + 
  geom_col(aes(fill = ifelse(diff >0, "#002b5c", "#BA0C2F" )), col = "white") +
  geom_vline(xintercept = c(12, 24, 36, 48), linetype = 2, col = "grey") +
  scale_x_continuous(breaks = c(12, 24, 36, 48), labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_fill_manual(values = c("#BA0C2F", "#002b5c")) +
  theme_minimal() +
  theme(legend.position = "NA"
        , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , text = element_text(size = 20)
        , plot.title = element_text(hjust = 0.5, size = 20)
        
  ) +
  labs(title = "Average Score Differential by 60-second Intervals", x = "", y = "Score Differential"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com") 

ggsave("Average Score Differential by One-minute Intervals.png", p3, width = 12, height = 8, dpi = 300, type = 'cairo')
