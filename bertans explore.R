###############################################
# Davis Bertans three point shooting EDA
# Session Info:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ballr)
library(rvest)
library(janitor)
library(hablar)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)

# set seed
set.seed(20483783729)

# import game logs
bertans <- game_logs(seasons = c(2016:2022), season_types = "Regular Season", result_types = c("player", "team"))

# separate df for bertans
bertans2 <- dataGameLogsPlayer %>% filter(namePlayer == "Davis Bertans") %>% 
  # select(slugSeason, idTeam, idGame, dateGame, pctFG3, fg3a, fg3m, pctFG2, fg2a, fg2m) %>%
  mutate(Bertans = "Davis Bertans"
         , new_pct = fg3m/fg3a # game log 3pt% does not match basketball reference
         )

# take a look at minutes and games played
minutes <- dataGameLogsPlayer %>% group_by(namePlayer) %>% 
  summarize(mean_minutes = mean(minutes, na.rm=T), games_played = n(), attempts = mean(fg3a, na.rm=T))

# average 3pt% during Bertans career
dataGameLogsTeam %>% 
  group_by(slugSeason) %>% 
  summarize(mean = mean(pctFG3Team, na.rm=T))

# team stats 
team_stats <- dataGameLogsTeam %>% 
  group_by(slugSeason, idTeam, idGame, dateGame) %>% 
  summarize(pctFG3 = mean(pctFG3Team, na.rm=T)
            , fg3a = mean(fg3aTeam, na.rm=T)
            , fg3m = mean(fg3mTeam, na.rm=T)
            , new_pct = fg3m/fg3a 
            ) %>%
  mutate(Bertans = "NBA")

# combine team and bertans dfs
bertans3 <- bertans2 %>% bind_rows(team_stats)

# plot Bertans shooting by game
p1 <- bertans3 %>%
  filter(dateGame >=min(bertans2$dateGame)) %>% 
  ggplot(aes(x = dateGame, y = new_pct)) + 
  geom_point(aes(col = Bertans, size = fg3a, alpha = ifelse(Bertans=="Davis Bertans", 0.8, 0.2)), shape =21) +
  geom_smooth(aes(color = Bertans), size = 2, se = F, method = 'loess') +
  scale_color_manual(values = c("#E31837", "#6C6463")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "none"
        , text = element_text(size = 20, family = "Serif")) +
  annotate("text", x = as.Date("2019-07-10"), y = 0.45, label = "Bertans Average", col = "#E31837") +
  annotate("text", x = as.Date("2019-07-15"), y = 0.32, label = "NBA Average", col = "#6C6463") +
  labs(x = "Date", y = "3pt %"
       , caption = "wizardspoints.substack.com"
       , title =  "Three point percentage for every game of Bertans's regular season career"
       , subtitle = "Lines show the smoothed average\nCircles show individual games and are sized by 3pt attempts" )

ggsave("Bertans Average.png", p1, width = 12, height = 8, dpi = 300, type = 'cairo')

# bertans by season
bertans2 %>% 
  group_by(slugSeason) %>% 
  summarize(mean_a = mean(fg3a, na.rm=T)
            , mean_m = mean(fg3m, na.rm=T)
            , pct = mean_m/mean_a
            )

# Bertans ranking overall, uncomment for season rankings
dataGameLogsPlayer %>% 
  # filter(slugSeason == "2019-20") %>%
  group_by(namePlayer) %>% 
  summarize(mean_a = mean(fg3a, na.rm=T)
            , mean_m = mean(fg3m, na.rm=T)
            , pct = mean_m/mean_a
  ) %>% 
  filter(mean_a>=4) %>%
  arrange(desc(pct)) %>% print(n=55)

# just sandboxing here
ggplot(bertans2, aes(pctFG3, slugSeason, fill = slugSeason )) +
  ggbeeswarm::geom_beeswarm(aes(size = fg3a), shape = 21, alpha = .8, groupOnX = FALSE, priority = "random") +
  theme_minimal() +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none")

# opponents stats
bertans_shooting <- teams_players_stats(seasons = c(2016:2022), types = "player", season_types = "Regular Season", tables = c("general", "shots", "shot locations", "splits", "defense"))
  
glimpse(bertans_shooting)

# plus minus
bertans2 %>% 
  filter(minutes!=0 & nameTeam == "Washington Wizards") %>% 
mutate(linecol = ifelse(plusminus<0, "Negative", "Positive" )
       , xend = lead(dateGame)
         , yend=lead(plusminus, default = 0)
         ) %>% 
  ggplot(aes(x = dateGame, y = plusminus, fill = linecol, col = linecol)) + 
  geom_col(size = 0.25, position = "identity") +
  scale_fill_manual(values = c("#BA0C2F", "#002F6C")) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C")) +
  facet_grid(~slugSeason, drop = T, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Date",y = "Plus-Minus")

# Net rating figures
df_advanced_stats <- NBAPerGameAdvStatistics(season = 2022)

# Select each team's highest usage player (min. 1000 minutes).
# Exclude LaMelo Ball since he's injured
df_top_usage <- df_advanced_stats %>%
  filter(tm == "WAS")

# Get each team's Net Rating from basketball-reference
url <- "https://www.basketball-reference.com/leagues/NBA_2022.html"

bref_tables <- url %>%
  read_html() %>%
  html_table()

df_tm_net_rating <- bref_tables[[11]]

df_tm_net_rating2 <- df_tm_net_rating %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  filter(team %in% c("Washington Wizards", "League Average"))

df_tm_net_rating3 <- df_tm_net_rating2 %>% select(team, n_rtg) %>% mutate(tm = c("WAS","League Average"))

url <- "https://www.basketball-reference.com/leagues/NBA_2022_play-by-play.html"

df_player_on_off <- url %>%
  read_html() %>%
  html_table(fill = TRUE) %>%
  as.data.frame()

df_player_on_off <- df_player_on_off %>%
  row_to_names(row_number = 1) %>%
  clean_names()

df_player_on_off <- df_player_on_off %>%
  select(player, tm, mp, on_court, on_off)

# Merge player on/off with team net ratings
df <- left_join(df_player_on_off, df_tm_net_rating3, by = c("tm" = "tm"))


# Change character data to numeric
# code based on Owen Phillips 
# F5: https://thef5.substack.com/p/how-to-comet-plot
df <- df %>% mutate_at(.vars = 3:7, .funs = as.numeric)

df <- df %>%
  mutate(off_court = case_when(
    on_court >= 0 & on_off >= 0 ~ on_court - on_off,
    on_court >= 0 & on_off < 0 ~ on_court + abs(on_off),
    on_court < 0 & on_off >= 0 ~ on_court - on_off,
    on_court < 0 & on_off < 0 ~ on_court + abs(on_off),
    TRUE ~ 0
  ))

df_comet <- df_top_usage %>%
  left_join(., df, by = c("player", "tm")) %>%
  mutate(pos_neg = case_when(
    on_off >= 0 ~ "Better On",
    TRUE ~ "Worse on",
  ))

df_comet$player_tm <- paste0(df_comet$player, " - ", df_comet$tm)

comet_plot <- df_comet %>%
  ggplot() +
  geom_link(aes(x = off_court, y = fct_reorder(player, on_off), xend = on_court, yend = fct_reorder(player, on_off), color = pos_neg, size = stat(index))) +
  scale_color_manual(values = c("#00A087FF", "#E64B35FF")) +
  scale_size(range = c(.01, 4)) +
  # scale_x_continuous(labels = c("-10", "-5", "0", "+5", "+10"), breaks = seq(-10, 10, 5)) +
  geom_point(
    data = filter(df_comet, on_off > 0),
    aes(on_court, y = fct_reorder(player, on_off), color = pos_neg),
    shape = 21,
    fill = "white",
    size = 3.5
  )  +
  geom_point(
    data = filter(df_comet, on_off < 0),
    aes(on_court, y = fct_reorder(player, on_off), color = pos_neg),
    shape = 21,
    fill = "white",
    size = 3.5
  ) +
  annotate(geom = 'label', x = -9.5, y = 5, label = "Team is worse\nwith them On", color = "#E64B35FF", fontface = 'bold', fill = "floralwhite", label.size = 0, size = 3) +
  annotate(geom = 'label', x = 15.5, y = 10.5, label = "Team is better\nwith them On", color = "#00A087FF", fontface = 'bold', fill = "floralwhite", label.size = 0, size = 3) +
  theme_classic() +
  theme(legend.position = 'none',
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.line = element_blank()
        , text = element_text(size = 20)) +
  labs(x = "Net Rating With Player On Or Off The Court",
       y = "",
       title = "Net rating when Wizards players are on or off the court",
       caption = "Chart inspired by Owen Phillips\nwizardspoints.substack.com")

ggsave("Comet_Plot.png", comet_plot, w = 12, h = 7, dpi = 300, type = "cairo")

inset_plot <- df_comet %>%
  ggplot() +
  geom_link(aes(x = 0, y = 1, xend = 1, yend = 1, size = stat(index)), color = "#00A087FF")  +
  scale_size(range = c(.01, 3)) +
  scale_y_continuous(limits = c(.95, 1.05)) +
  scale_x_continuous(limits = c(-.2, 1.2)) +
  labs(title = "KEY") +
  coord_cartesian(clip = 'off') +
  geom_point(aes(x = 1, y = 1), color = "#00A087FF",shape = 21,fill = "white", size = 2.5) +
  # theme_classic() +
  theme(legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank(),
        , panel.grid.minor = ggplot2::element_blank()
        , panel.grid.major.y = ggplot2::element_blank()
        , panel.grid.major.x = ggplot2::element_blank()
        , panel.background = ggplot2::element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = .5, face = 'bold'),
        plot.background = element_rect(fill = 'white', color = "black")) +
  # geom_brace(0.005,1.0175,1.005,1.0175) +
  annotate(geom = 'text', x = 0.5, y = 1.025, label = "On/Off Differential", size = 2.5, hjust = .5, lineheight = 1)  +
  annotate(geom = 'text', x = 0, y = .985, label = "Net Rating\nWith Player OFF", size = 1.85, hjust = .5, lineheight = 1) +
  annotate(geom = 'text', x = 1, y = .985, label = "Net Rating\nWith Player ON", size = 1.85, hjust = .5, lineheight = 1)

ggsave("Inset.png",  inset_plot, w = 1.5, h = 1.5, dpi = 300, type = "cairo")


# Read in Inset plot
inset <- image_read("Inset.png")

# Read in Comet plot
graf <- image_read("Comet_Plot.png")

# Layer Inset plot on top of Comet plot
image_composite(graf, inset, offset = "+900+1300") %>% image_write("Comet_Plot.png")
