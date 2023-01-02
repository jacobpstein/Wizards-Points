###############################################
# KP and fouls
# Session Info:
# R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
# Copyright (C) 2022 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20 (64-bit)
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(janitor)
library(lubridate)
library(ggridges)
library(ggrepel)
library(rstanarm)
library(lme4)

# set seed
set.seed(20222712)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# get game info------

# get game ids
wiz_game_ids <- game_logs(seasons = 2023, result_types = "team") %>% filter(nameTeam == "Washington Wizards")

wiz_games <- wiz_game_ids %>% 
  mutate(newdate = gsub(x=dateGame, pattern = "-", replacement="")
         , current_game = paste0(newdate, "0", slugTeamWinner)
         , lower_name = tolower(slugOpponent)
         , lower_wiz = tolower(slugTeam)
         , match_up = case_when(locationGame== "H" ~ paste0(lower_name, "-vs-", lower_wiz)
                                , locationGame == "A" ~ paste0(lower_wiz, "-vs-", lower_name))
  ) 


id <- wiz_games$idGame

opp <- wiz_games$match_up

box_scores(game_ids = id
           , box_score_types = c("Traditional"
                                 , "Advanced"
                                 , "Scoring"
                                 #, "Tracking"
                                 , "Misc"
                                 )
           , result_types = c("player"
                              #, "team"
                              )
           , join_data = TRUE
           , assign_to_environment = TRUE
           , return_message = TRUE)


wiz_box <- dataBoxScorePlayerNBA %>% filter(slugTeam == "WAS")

wiz_df <- wiz_box %>% left_join(wiz_games)

# fta by other box score metrics
wiz_df %>% 
  filter(namePlayer == "Kristaps Porzingis") %>% 
  select(outcomeGame
         , idGame
         , fta
         , everything()
         ) %>% 
  select_if(is.numeric) %>% 
  pivot_longer(cols = c(5:100)) %>% 
  ggplot(aes(x = fta, y = value)) +
  geom_point( shape = 21
             , fill = 'white'
               #,  alpha = 0.8
             #, stroke = 2
             #, size = 1
             ) +
  geom_smooth(method = "lm", se = F) +
  theme_classic() +
  facet_wrap(~name, scales = "free")

# looks like KP's fta have increased as the season has moved along

# let's look at that

p1 <- wiz_df %>% 
  filter(isStarter == "TRUE") %>% 
  mutate(kp = ifelse(namePlayer == "Kristaps Porzingis", "Kristaps Porziņģis", "Other Starters")
         ) %>% 
  group_by(numberGameTeamSeason, kp) %>% 
  summarize(pfd = sum(pfd, na.rm=T)
            ) %>% 
  group_by(numberGameTeamSeason) %>% 
  mutate(ratio = pfd/sum(pfd)) %>% 
  ggplot(aes(x=numberGameTeamSeason, y=ratio)) +
  geom_col(aes(fill = kp), size = 2) +
  geom_hline(yintercept = .5, color = "black") +
  scale_fill_manual(values = c("#002B5C", "#C4CED4")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "top"
        , legend.title = element_blank()
        , panel.grid.major.x = element_blank()
        , plot.background = element_rect(fill = "white")
        , text = element_text(size = 23)
        ) +
  labs(x = "Game Number", y = "Percentage of Fouls Drawn"
       , title = "Percentage of Fouls Drawn Among\nWizards Starters by Game Number"
       , caption = "data: nba.com\nwizardspoints.substack.com"
)

ggsave("fouls drawn percentage.png", p1, w = 12, h = 10, dpi = 300, type = "cairo")

# average percentage of fouls drawn among starters

wiz_df %>% 
  filter(isStarter == "TRUE") %>% 
  mutate(kp = ifelse(namePlayer == "Kristaps Porzingis", "Kristaps Porzingis", "Other Players")
  ) %>% 
  group_by(numberGameTeamSeason, kp) %>% 
  summarize(pfd = sum(pfd, na.rm=T)
  ) %>% 
  group_by(numberGameTeamSeason) %>% 
  mutate(ratio = pfd/sum(pfd)) %>% ungroup() %>% 
  group_by(kp) %>% summarize(mean = mean(ratio))

# and overall
wiz_df %>% 
  mutate(kp = ifelse(namePlayer == "Kristaps Porzingis", "Kristaps Porzingis", "Other Players")
  ) %>% 
  group_by(numberGameTeamSeason, kp) %>% 
  summarize(pfd = sum(pfd, na.rm=T)
  ) %>% 
  group_by(numberGameTeamSeason) %>% 
  mutate(ratio = pfd/sum(pfd)) %>% ungroup() %>% 
  group_by(kp) %>% summarize(mean = mean(ratio))

# ranking pfd
wiz_df %>% 
  group_by(namePlayer) %>% 
  summarize(pfd = sum(pfd, na.rm=T)) %>% 
  arrange(desc(pfd))


# pdf and usage
wiz_df %>% 
  filter(namePlayer == "Kristaps Porzingis") %>% 
select(outcomeGame
       , idGame
       , pfd
       , ptsTeam
       , pctFG
       , pctEFG
       , pts) %>% 
  pivot_longer(cols = c(4:7)) %>% 
  ggplot(aes(x = pfd, y = value)) +
  geom_point(aes(fill = outcomeGame)
             , shape = 21
             , col = 'white'
               #,  alpha = 0.8
             , stroke = 2, size = 5) +
  geom_smooth(method = "lm", se = F) +
  scale_fill_manual(values = c('#1d91c0','#081d58')) +
  theme_classic() +
  theme(legend.position = "top"
        , legend.title = element_blank()) +
  labs(x = "Total fouls drawn per game", y = "") +
  facet_wrap(~name, scales = "free") +
  ggpubr::stat_cor(method = "pearson", color = "black", geom = "label")


kp_df <-  wiz_df %>% 
  filter(namePlayer == "Kristaps Porzingis") 

cor.test(kp_df$pts, kp_df$pfd)

wiz_df %>% 
  filter(isStarter == "TRUE") %>% 
  mutate(kp = ifelse(namePlayer == "Kristaps Porzingis", "Kristaps Porzingis", "Other Players")
  ) %>% 
  group_by(dateGame, kp) %>% 
  summarize(pfd = median(pfd, na.rm=T)) %>% 
  ggplot(aes(x=dateGame, y=pfd)) +
  geom_step(aes(col = kp), size = 2) +
  theme_minimal() +
  theme(legend.position = "NA"
        , plot.background = element_rect(fill = "white")
  )
