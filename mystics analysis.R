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
# library(ballr)
library(rvest)
library(janitor)
library(hablar)
library(ggforce)
# library(ggbrace)
library(magick)
library(ggtext)
library(geomtextpath)


# set seed
set.seed(20483783729)


# Get each team's Net Rating from basketball-reference
url <- "https://www.basketball-reference.com/wnba/years/2022.html"

bref_tables <- url %>%
  read_html() %>%
  html_table()

df_tm_net_rating <- bref_tables[[7]]

df_tm_net_rating2 <- df_tm_net_rating %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  filter(team %in% c("Washington Mystics*", "League Average"))

df_tm_net_rating3 <- df_tm_net_rating2 %>% select(team, n_rtg) %>% mutate(team = c("WAS","League Average"))

url <- "https://www.basketball-reference.com/wnba/years/2022_play_by_play.html"

df_player_on_off <- url %>%
  read_html() %>%
  html_table(fill = TRUE) %>%
  as.data.frame()

df_player_on_off <- df_player_on_off %>%
  row_to_names(row_number = 1) %>%
  clean_names()

df_player_on_off <- df_player_on_off %>%
  select(player, team, mp, on_court, on_off)

# Merge player on/off with team net ratings
df <- left_join(df_player_on_off, df_tm_net_rating3, by = c("team" = "team"))


# Change character data to numeric
# code based on Owen Phillips 
# F5: https://thef5.substack.com/p/how-to-comet-plot
df <- df %>% mutate_at(.vars = 3:6, .funs = as.numeric)

df <- df %>%
  mutate(off_court = case_when(
    on_court >= 0 & on_off >= 0 ~ on_court - on_off,
    on_court >= 0 & on_off < 0 ~ on_court + abs(on_off),
    on_court < 0 & on_off >= 0 ~ on_court - on_off,
    on_court < 0 & on_off < 0 ~ on_court + abs(on_off),
    TRUE ~ 0
  ))





df_comet <- df_top_usage %>%
  left_join(., df, by = c("player", "team")) %>%
  mutate(pos_neg = case_when(
    on_off >= 0 ~ "Better On",
    TRUE ~ "Worse on",
  )) %>% 
  filter(team == "WAS" & player!="Jazmine Jones") %>% 
  mutate(MP = as.numeric(MP)
         , G = as.numeric(G)
         )


df_comet$player_tm <- paste0(df_comet$player, " - ", df_comet$team)


comet_plot <- df_comet %>%
  ggplot() +
  geom_link(aes(x = off_court
                , y = fct_reorder(player, on_off)
                , xend = on_court
                , yend = fct_reorder(player, on_off)
                , color = pos_neg
                , size = stat(index))) +
  scale_color_manual(values = c("#0C2340", "#C8102E")) +
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
  annotate(geom = 'label', x = -9.5, y = 5, label = "Team is worse\nwith them On", color = "#C8102E", fontface = 'bold', fill = "floralwhite", label.size = 0, size = 3) +
  annotate(geom = 'label', x = 15.5, y = 10.5, label = "Team is better\nwith them On", color = "#0C2340", fontface = 'bold', fill = "floralwhite", label.size = 0, size = 3) +
  theme_classic() +
  theme(legend.position = 'none',
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.line = element_blank()
        , text = element_text(size = 20)) +
  labs(x = "Plus/Minus With Player On Or Off The Court",
       y = "",
       title = "Plus/Minus 100 Possessions When Mystics Are On or Off the Court",
       caption = "data: basketball-reference.com\nwizardspoints.substack.com")

ggsave("Mystics Comet_Plot.png", comet_plot, w = 12, h = 7, dpi = 300, type = "cairo")

url <- "https://www.basketball-reference.com/wnba/teams/WAS/2022/gamelog/"
  
game_logs <- url %>%
  read_html() %>%
  html_table() %>% 
  as.data.frame() 


game_logs_mystics <- game_logs %>% 
  select(1:26) %>% 
  row_to_names(row_number = 1) %>%
  clean_names() %>% 
  filter(rk!="" & rk!= "Rk")


game_logs_oppo <- game_logs %>% 
  select(1:5, 28:43) %>% 
  row_to_names(row_number = 1) %>%
  clean_names() %>% 
  filter(fg != "Opponent" & fg !="FG")

oppo_long <- game_logs_oppo %>% 
  pivot_longer(cols = c(6:21)) %>% 
  mutate(value = as.numeric(value)
         , date = as.Date(date)
         , team = "Opponent") %>% 
  select(-x)  

mystics_long <- game_logs_mystics %>% 
  pivot_longer(cols = c(11:26)) %>% 
  mutate(value = as.numeric(value)
         , date = as.Date(date)
         , team = "Mystics") %>% 
  select(-x, -opp, -na, -tm, -opp_2, -na_2)  

long_long <- mystics_long %>% bind_rows(oppo_long)

offensive_rebounds <- long_long %>% 
  filter(name %in% c("orb")) %>% 
  ggplot(aes(x = date, y = value)) +
  # geom_smooth(aes(col = team), se = F) +
  geom_point(aes(col = team), alpha = 0.1) +
  geom_textsmooth(aes(label = team, col = team), size = 10, linewidth = 1
                  , method = "loess", formula = y ~ x, span = 0.53, hjust = 0.3) +
  scale_color_manual(values = c("#0C2340", "#C8102E")) +
  theme_classic() +
  theme(legend.position = 'none'
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.line = element_blank()
        , text = element_text(size = 20)) +
  labs(x = "",
       y = "",
       title = "Mystics-Opponent Offensive Rebounds",
       caption = "data: basketball-reference.com\nwizardspoints.substack.com")  

ggsave("Mystics O Rebounds.png", offensive_rebounds, w = 12, h = 7, dpi = 300, type = "cairo")


pfs <- long_long %>% 
  filter(name %in% c("pf")) %>% 
  ggplot(aes(x = date, y = value)) +
  # geom_smooth(aes(col = team), se = F) +
  geom_point(aes(col = team), alpha = 0.1) +
  geom_textsmooth(aes(label = team, col = team), size = 10, linewidth = 1
                  , method = "loess", formula = y ~ x, span = 0.53, hjust = 0.3) +
  scale_color_manual(values = c("#0C2340", "#C8102E")) +
  theme_classic() +
  theme(legend.position = 'none'
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.line = element_blank()
        , text = element_text(size = 20)) +
  labs(x = "",
       y = "",
       title = "Mystics-Opponent Personal Fouls",
       caption = "data: basketball-reference.com\nwizardspoints.substack.com")  

ggsave("Mystics pfs.png", pfs, w = 12, h = 7, dpi = 300, type = "cairo")


# delle donne games

url <- "https://www.basketball-reference.com/wnba/players/d/delleel01w/gamelog/2022/"


dd_games <- read_csv("Desktop/Git/Wizards-Points/sportsref_download.csv", 
                     col_types = cols(Date = col_date(format = "%m/%d/%y")))

dd_dates <- dd_games$Date

game_logs_mystics <- game_logs %>% 
  select(1:26) %>% 
  row_to_names(row_number = 1) %>%
  clean_names() %>% 
  filter(rk!="" & rk!= "Rk")

game_logs_mystics %>% mutate(date = as.Date(date)) %>% filter(!date %in% dd_dates) %>% 
  group_by(w_l) %>% 
  count()


no_dd <- game_logs_mystics %>% mutate(date = as.Date(date)) %>% filter(!date %in% dd_dates)


wnba_pbp <- wehoop::load_wnba_pbp()

wnba_pbp <- wnba_pbp %>% filter(away_team_abbrev == "WSH" | home_team_abbrev == "WSH")

wnba_pbp %>% 
  filter(str_detect(text,
    "Elena Delle Donne|Natasha Cloud|Ariel Atkins|Shakira Austin|Alysha Clark|Tianna Hawkins|Myisha Hines-Allen|Rui Machida|Shatori Walker-Kimbrough|Evina Westbrook|Elizabeth Williams|Jazmine Jones"
    )) %>% 
  group_by(type_text) %>% 
  count() %>% 
  arrange(desc(n))
  
  filter(type_text == "Offensive Rebound") %>% View()

