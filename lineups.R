######################################
# This file visualizes the Wizards line-ups for the first
# 9 games of the 2022-23 season. Data are sourced from
# basketball-reference.com
# R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
# Copyright (C) 2022 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20 (64-bit)
#######################################

# load libraries
library(tidyverse)
library(ggbeeswarm)
library(packcircles)
library(readxl)
library(nbastatR)
library(ggforce)
library(ggridges)

# set seed
set.seed(20483789)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# load in data-----
# create a sheets object
sheets <- excel_sheets('lineups.xlsx')

# extract the sheet names
game_sheets <- sheets[grepl("game", sheets)]

# import and combine
lineup_df <- map_dfr(game_sheets, ~read_excel(path = 'lineups.xlsx', sheet = .x), id = .x)

# clean up from wider to long 
lineups <- lineup_df %>% 
              pivot_longer(cols = c(5:9), names_to = "post", values_to = "player")

# get total minutes so far this season
bref_players_stats(seasons = 2023, tables = c("totals"))

# filter out everyone except the Wizards
wiz <- dataBREFPlayerTotals %>% filter(slugTeamBREF== "WAS") %>% 
  mutate(namePlayer = ifelse(namePlayer == "Kristaps Porzingis", "Kristaps Porziņģis", namePlayer))

# get the game logs
games <- game_logs(seasons = 2023, result_types = c("player")) %>% 
  mutate(namePlayer = ifelse(namePlayer == "Kristaps Porzingis", "Kristaps Porziņģis", namePlayer)) %>% 
  filter(namePlayer %in% wiz$namePlayer) %>% 
  rename("player" = namePlayer
         , "game" = numberGameTeamSeason)

# combine all of our data sources
lineups <- lineups %>%
  left_join(select(wiz, "player" = namePlayer, "total" = minutesTotals )) %>% 
  left_join(select(games, player, game, minutes))

# create the first viz
# this was not used in for the substack
p1 <- lineups %>% 
  ggplot(aes(x = minute, y = reorder(player, minutes))) +
  ggbeeswarm::geom_quasirandom(aes(fill = player), shape = 21, size = 3, stroke = 1, col = "white"
                            , alpha = .7,
                            cex = 2,    
                            method = "pseudorandom" # "distance" of the points
                            # , priority = "ascending"  # you can play with the sorting as well
                            , groupOnX = FALSE
  ) + 
  scale_fill_manual(values = rep(c('#41b6c4','#1d91c0','#225ea8','#253494','#081d58'), 5)) +
  scale_x_continuous(breaks = c(12, 24, 32, 48), labels = c("Q1", "Q2", "Q3", "Q4")) +
  theme_classic() +
  theme(legend.position = 'none'
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.line = element_blank()
        , text = element_text(size = 20)) +
  labs(x = "",
       y = "",
       title = "Minute-by-Minute on-court presence for each Wizards player"
       ,caption = "data: basketball-reference.com\nwizardspoints.substack.com") 

ggsave("beswarm minutes.png", p1, w = 12, h = 10, dpi = 300, type = "cairo")


# create the same as above but without point stacking
p2 <- lineups %>% 
  group_by(player, minute) %>% 
  mutate(count = n()
            ) %>% 
  ggplot(aes(x=minute, y=reorder(player, total), size = count)) + 
  scale_size(range = c(3,18)) +
  geom_jitter(aes(fill = player), shape = 21, col = "white", alpha = 0.5, height = 0.0) +
  scale_fill_manual(values = rep(c('#41b6c4','#1d91c0','#225ea8','#253494','#081d58'), 5)) +
  scale_x_continuous(breaks = c(12, 24, 36, 48), labels = c("Q1", "Q2", "Q3", "Q4")) +
  theme_classic() +
  theme(legend.position = 'none'
        , panel.border = element_blank()
        , panel.grid.major.y =element_blank()
        , axis.line.y = element_blank()
        , panel.grid.major.x = element_line()
        , text = element_text(size = 20)) +
  labs(x = "",
       y = "",
       title = "Minute-by-minute on-court presence for each Wizards player"
       , subtitle = "Dots are sized by frequency of apparence at a given minute time-stamp"
       ,caption = "data: basketball-reference.com\nwizardspoints.substack.com") 

ggsave("circle minutes.png", p2,  w = 12, h = 10, dpi = 300, type = "cairo")

# same as above but by game
p3 <- lineups %>% 
  group_by(player, minute) %>% 
  mutate(count = n()
         , game = paste0("Game ", game, " - ", opponent)
  ) %>% 
  ggplot(aes(x=minute, y=reorder(player, minutes), size = 3)) + 
  # scale_size(range = c(3,18)) +
  geom_jitter(aes(fill = player), shape = 21, col = "white", alpha = 0.7, height = 0.0) +
  scale_fill_manual(values = rep(c('#41b6c4','#1d91c0','#225ea8','#253494','#081d58'), 5)) +
  scale_x_continuous(breaks = c(12, 24, 36, 48), labels = c("Q1", "Q2", "Q3", "Q4")) +
  theme_minimal() +
  theme(legend.position = 'none'
        , panel.border = element_blank()
        , panel.grid.major.y =element_blank()
        , axis.line.y = element_blank()
        , panel.grid.major.x = element_line()
        , text = element_text(size = 20)) +
  labs(x = "",
       y = "",
       title = "Minute-by-minute on-court presence for each Wizards player by game"
       ,caption = "data: basketball-reference.com\nwizardspoints.substack.com") +
  facet_wrap(~game, scales = "free_y", ncol = 2)

ggsave("circle minutes by game.png", p3,  w = 16, h = 20, dpi = 300, type = "cairo")

# looking at Deni and Gill
lineups %>% 
  filter(player %in% c("Deni Avdija", "Anthony Gill")) %>% 
  ggplot(aes(x=minute, y = factor(game))) + 
  geom_point(aes(fill = player, col = player), position = position_dodge(0.5)) + 
  theme(legend.position = "NA") +
  scale_color_manual(values = rep(c('#41b6c4','#081d58'), 5)) +
  scale_fill_manual(values = rep(c('#41b6c4','#081d58'), 5)) +
  scale_x_continuous(breaks = c(12, 24, 36, 48), labels = c("Q1", "Q2", "Q3", "Q4")) +
  theme_classic() +
  theme(legend.position = 'top'
        , legend.title = element_blank()
        , panel.border = element_blank()
        , panel.grid.major.y =element_blank()
        , axis.line.y = element_blank()
        , panel.grid.major.x = element_line()
        , text = element_text(size = 20)) +
  labs(x = "",
       y = "",
       title = "Minute-by-minute on-court presence for each Wizards player"
       , subtitle = "Points are sized by frequency of apparence at a given minute time-stamp"
       ,caption = "data: basketball-reference.com\nwizardspoints.substack.com") 


# create a data frame of just Deni and Gill
deni_gill <- lineups %>% 
  filter(player %in% c("Deni Avdija", "Anthony Gill")) 
deni_gill$g <- cumsum(apply(deni_gill, 1, anyNA))
ggplot(data=deni_gill,aes(minutes, group=g, col = player))+geom_bar(position = "dodge") + facet_wrap(~game)

# additional explortation
deni_gill %>% 
  ggplot(aes(x=minute, y = game)) + 
  geom_step(aes(col = player), size = 2) +
  theme(legend.position = "NA") +
  scale_color_manual(values = rep(c('#41b6c4','#081d58'), 5)) +
  scale_fill_manual(values = rep(c('#41b6c4','#081d58'), 5)) +
  scale_x_continuous(breaks = c(12, 24, 36, 48), labels = c("Q1", "Q2", "Q3", "Q4")) +
  theme_classic() +
  theme(legend.position = 'top'
        , legend.title = element_blank()
        , panel.border = element_blank()
        , panel.grid.major.y =element_blank()
        , axis.line.y = element_blank()
        , panel.grid.major.x = element_line()
        , text = element_text(size = 20)) +
  labs(x = "",
       y = "",
       title = "Minute-by-minute on-court presence for each Wizards player"
       , subtitle = "Points are sized by frequency of apparence at a given minute time-stamp"
       ,caption = "data: basketball-reference.com\nwizardspoints.substack.com") 





