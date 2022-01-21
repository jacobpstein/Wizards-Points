###############################################
# 15th man of the year
# Session Info:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(gt)

# set seed
set.seed(20483789)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# get team stats
test <- teams_rosters(seasons = 2022, nest_data = F, return_message = T)

# defensive points saved, offensive points added
# offensive, denfensive rating
#TPA by minutes


# players_tables(seasons = 2022, players = c("Anthony Gill"), tables = c("game logs", )  modes = c("PerGame", "Per36", "Per100Possessions"), measures = c("Base", "Advanced"), assign_to_environment = TRUE)


bref_players_stats(seasons = 2022, tables = c("advanced", "totals", "per_game", "per_minute", "per_poss"), only_totals = FALSE)

gill_advanced <- dataBREFPlayerAdvanced %>%
  filter(namePlayer %in% c(
    "Anthony Gill" # 1 Wizards
    , "Jevon Carter" # 2 Nets
    , "Charles Bassey" # 3 Sixers
    , "Dalano Banton" # 4 Raptors
    , "Miles McBride" # 5 Knicks
    , "Bruno Fernando" # 6 Celtics
    , "Matt Thomas" # 7 Bulls
    , "Rodney Hood" # 8 Bucks
    , "Dylan Windler" # 9 Cavs
    , "Goga Bitadze" # 10 Pacers
    , "Saben Lee" # 11 Pistons
    , "KZ Okpala" # 12 Heat
    , "Kai Jones" # 13 Hornets
    , "Timothe Luwawu-Cabarrot" # 14 Hawks
    , "Moritz Wagner" # 15 Magic
    , "Elijah Hughes" # 16 Jazz
    , "Bol Bol" # 17 Nuggets
    , "Leandro Bolmaro" # 18 Timberwolves
    , "CJ Elleby" # 19 Trailblazers
    , "Isaiah Roby" # 20 Thunder
    , "Elfrid Payton" # 21 Suns, Payton is actually 14 but 15 has been injured since November
    , "Moses Moody" # 22 Warriors
    , "DeAndre Jordan" # 23 Lakers
    , "Justise Winslow" # 24 Clippers
    , "Jahmi'us Ramsey"  # 25 Kings
    , "Jarrett Culver" # 26 Grizzlies
    , "Marquese Chriss" # 27 Mavericks Just signed a two year deal after several 10 days with the Mavs
    , "Didi Louzada" # 28 Pelicans
    , "Jock Landale" # 29 Spurs
    , "Usman Garuba" # 30 Rockets
  )) %>%
  mutate(bref_url = glue::glue("https://www.basketball-reference.com/players/{stringr::str_sub(idPlayerNBA, 1, 1)}/{idPlayerNBA}.html")
                  # , bref_link = glue::glue('<a href="{bref_url}">{namePlayer}</a>')
         # , stat = "Advanced"

           )

gill_pergame <- dataBREFPlayerPerGame %>%
  filter(namePlayer %in% c(
    "Anthony Gill" # 1 Wizards
    , "Jevon Carter" # 2 Nets
    , "Charles Bassey" # 3 Sixers
    , "Dalano Banton" # 4 Raptors
    , "Miles McBride" # 5 Knicks
    , "Bruno Fernando" # 6 Celtics
    , "Matt Thomas" # 7 Bulls
    , "Rodney Hood" # 8 Bucks
    , "Dylan Windler" # 9 Cavs
    , "Goga Bitadze" # 10 Pacers
    , "Saben Lee" # 11 Pistons
    , "KZ Okpala" # 12 Heat
    , "Kai Jones" # 13 Hornets
    , "Timothe Luwawu-Cabarrot" # 14 Hawks
    , "Moritz Wagner" # 15 Magic
    , "Elijah Hughes" # 16 Jazz
    , "Bol Bol" # 17 Nuggets
    , "Leandro Bolmaro" # 18 Timberwolves
    , "CJ Elleby" # 19 Trailblazers
    , "Isaiah Roby" # 20 Thunder
    , "Elfrid Payton" # 21 Suns, Payton is actually 14 but 15 has been injured since November
    , "Moses Moody" # 22 Warriors
    , "DeAndre Jordan" # 23 Lakers
    , "Justise Winslow" # 24 Clippers
    , "Jahmi'us Ramsey"  # 25 Kings
    , "Jarrett Culver" # 26 Grizzlies
    , "Marquese Chriss" # 27 Mavericks Just signed a two year deal after several 10 days with the Mavs
    , "Didi Louzada" # 28 Pelicans
    , "Jock Landale" # 29 Spurs
    , "Usman Garuba" # 30 Rockets
  )) %>%
  mutate(bref_url = glue::glue("https://www.basketball-reference.com/players/{stringr::str_sub(idPlayerNBA, 1, 1)}/{idPlayerNBA}.html")
         # , bref_link = glue::glue('<a href="{bref_url}">{namePlayer}</a>')
         # , stat = "Per Game"

         )



gill_perminute <- dataBREFPlayerPerMinute %>%
  filter(namePlayer %in% c(
    "Anthony Gill" # 1 Wizards
    , "Jevon Carter" # 2 Nets
    , "Charles Bassey" # 3 Sixers
    , "Dalano Banton" # 4 Raptors
    , "Miles McBride" # 5 Knicks
    , "Bruno Fernando" # 6 Celtics
    , "Matt Thomas" # 7 Bulls
    , "Rodney Hood" # 8 Bucks
    , "Dylan Windler" # 9 Cavs
    , "Goga Bitadze" # 10 Pacers
    , "Saben Lee" # 11 Pistons
    , "KZ Okpala" # 12 Heat
    , "Kai Jones" # 13 Hornets
    , "Timothe Luwawu-Cabarrot" # 14 Hawks
    , "Moritz Wagner" # 15 Magic
    , "Elijah Hughes" # 16 Jazz
    , "Bol Bol" # 17 Nuggets
    , "Leandro Bolmaro" # 18 Timberwolves
    , "CJ Elleby" # 19 Trailblazers
    , "Isaiah Roby" # 20 Thunder
    , "Elfrid Payton" # 21 Suns, Payton is actually 14 but 15 has been injured since November
    , "Moses Moody" # 22 Warriors
    , "DeAndre Jordan" # 23 Lakers
    , "Justise Winslow" # 24 Clippers
    , "Jahmi'us Ramsey"  # 25 Kings
    , "Jarrett Culver" # 26 Grizzlies
    , "Marquese Chriss" # 27 Mavericks Just signed a two year deal after several 10 days with the Mavs
    , "Didi Louzada" # 28 Pelicans
    , "Jock Landale" # 29 Spurs
    , "Usman Garuba" # 30 Rockets
  )) %>%
  mutate(bref_url = glue::glue("https://www.basketball-reference.com/players/{stringr::str_sub(idPlayerNBA, 1, 1)}/{idPlayerNBA}.html")
         # , bref_link = glue::glue('<a href="{bref_url}">{namePlayer}</a>')
         # , stat = "Per 36"

  )

# code inspired by Mara Averick
# https://rpubs.com/maraaverick/479895

gill_advanced %>%
  left_join(select(gill_perminute, namePlayer, ptsPerMinute, astPerMinute, pfPerMinute)) %>%
  left_join(select(gill_pergame, namePlayer, minutesPerGame)) %>%
  select(namePlayer
         , urlPlayerHeadshot
         , slugTeamBREF
         , slugPosition
         , minutesPerGame
         , ptsPerMinute
         , astPerMinute
         , ratioBPM
         # ,  "bref_url" = urlPlayerStats
         ) %>%
  arrange(desc(ratioBPM)) %>%
  # gt::gt(rowname_col = "namePlayer") %>%
  gt() %>%
  tab_header(
    title = md("**15th best player on each team**")
  ) %>%
  cols_label(
    namePlayer = md("**Player**")
    , urlPlayerHeadshot = md("")
    , slugTeamBREF = md("**Team**")
    , slugPosition = md("**Position**")
    , minutesPerGame = md("**Minutes per game**")
    , ptsPerMinute = md("**Points per 36**")
    , astPerMinute = md("**Assists per 36**")
    , ratioBPM = md("**+/-**")
    # , bref_url = md("**Link**")
  ) %>%
  # text_transform(
  #   locations = cells_body(vars(bref_url)),
  #   fn = function(x) {
  #     sprintf("<a href=%s>profile</a>", x)
  #   }
  # ) %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x)
    }
  ) %>%
  tab_source_note(
    md("source: [basketball-reference.com](https://www.basketball-reference.com)")
  )  %>%
  tab_source_note(
    md("[wizardspoints.substack.com](wizardspoints.substack.com)")
  ) %>%
  gtsave(
    "table.png", expand = 10,
  )


gill_advanced_rank <- gill_advanced %>%
  left_join(gill_perminute) %>%
  column_to_rownames(., var = "namePlayer") %>%
  select(agePlayer
         , countGames
         , ratioPER
         , pctTrueShooting
         , pctUSG
         , ratioWSPer48
         , ratioVORP
         , pfPerMinute
         , ptsPerMinute
         , stlPerMinute
         , blkPerMinute
         , astPerMinute
         , trbPerMinute
         , slugPosition
          )

gill_advanced_ranks <- mutate_all(gill_advanced_rank[1:22], funs(rank(., ties.method="first")))



gill_long <- gill_advanced_rank %>%
  rownames_to_column(., var = "name") %>%
  rename("Age" = agePlayer
         , "Games Played" = countGames
         , "PER" = ratioPER
         , "True Shooting" = pctTrueShooting
         , "Usage" = pctUSG
         , "Win Shares per 48" = ratioWSPer48
         , "VORP" = ratioVORP
         , "Personal Fouls per 36 min." = pfPerMinute
         , "Points per 36 min." = ptsPerMinute
         , "Steals per 36 min." = stlPerMinute
         , "Blocks per 36 min." = blkPerMinute
         , "Assists per 36 min." = astPerMinute
         , "Total rebounds per 36 min." = trbPerMinute
         , "Position" = slugPosition
         ) %>%
  pivot_longer(
    cols = 2:14,
    names_to = "var"
    , values_to = "values") %>%
  mutate(stat = ifelse(var %in% c("VORP", "Win Shares per 48", "Usage", "True Shooting")==T, "Advanced"
                       , ifelse(var %in% c("Age", "Games Played")==T, "Count", "Per 36")))


p1 <- gill_long %>%
  filter(var %in% c("VORP", "Win Shares per 48", "Usage", "True Shooting")) %>%
  mutate(Gill = ifelse(name == "Anthony Gill", "Gill", "Not Gill")) %>%
  ggplot(aes(x = values, y = var)) +
  geom_point(aes(fill = Gill, col = Gill
                 , alpha = ifelse(name == "Anthony Gill", 1, 0.8)
                 ), size = 5, shape = 21) +
  scale_fill_manual(values = c("#BA0C2F", "black")) +
  scale_color_manual(values = c("#BA0C2F", "black")) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(x = "", y = ""
       , title = "15th Ranked Players Across Multiple Stats"
       , subtitle = "Anthony Gill is highlighted in red"
       # , caption = "Source: basketball-reference.com\nwizardspoints.substack.com"
       )


p2 <- gill_long %>%
  filter(!var %in% c("VORP", "Win Shares per 48", "Usage", "True Shooting", "Age", "Games Played")) %>%
  mutate(Gill = ifelse(name == "Anthony Gill", "Gill", "Not Gill")) %>%
  ggplot(aes(x = values, y = var)) +
  geom_point(aes(fill = Gill, col = Gill
                 , alpha = ifelse(name == "Anthony Gill", 1, 0.8)
  ), size = 5, shape = 21) +
  scale_fill_manual(values = c("#BA0C2F", "black")) +
  scale_color_manual(values = c("#BA0C2F", "black")) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(x = "", y = "", caption = "Source: basketball-reference.com\nwizardspoints.substack.com")

p1_p1 <- cowplot::plot_grid(plotlist = list(p1, p2)
                   , nrow = 2
                   , ncol = 1
                   , label_size = 14
                   , label_fontface = "plain"
                   , label_fontfamily = "Corbel")

ggsave("Gill Stats.png", p1_p1, width = 10, height = 8, dpi = 300, type = 'cairo')


gill_advanced %>%
  mutate(Gill = ifelse(namePlayer == "Anthony Gill", "Gill", "Not Gill")) %>%
  ggplot(aes(x =  ratioOBPM, y =  ratioDBPM)) +
  geom_point(aes(fill = Gill, col = Gill
                 , alpha = ifelse(namePlayer == "Anthony Gill", 1, 0.8)
  ), size = 5, shape = 21)


# Get each team's Net Rating from basketball-reference
url <- "https://www.basketball-reference.com/leagues/NBA_2022_per_poss.html"

bref_tables <- url %>%
  read_html() %>%
  html_table()

net_df <- bref_tables[[1]]


net_df2 <- net_df[net_df$Player %in% c(
  "Anthony Gill" # 1 Wizards
  , "Jevon Carter" # 2 Nets
  , "Charles Bassey" # 3 Sixers
  , "Dalano Banton" # 4 Raptors
  , "Miles McBride" # 5 Knicks
  , "Bruno Fernando" # 6 Celtics
  , "Matt Thomas" # 7 Bulls
  , "Rodney Hood" # 8 Bucks
  , "Dylan Windler" # 9 Cavs
  , "Goga Bitadze" # 10 Pacers
  , "Saben Lee" # 11 Pistons
  , "KZ Okpala" # 12 Heat
  , "Kai Jones" # 13 Hornets
  , "Timothé Luwawu-Cabarrot" # 14 Hawks
  , "Moritz Wagner" # 15 Magic
  , "Elijah Hughes" # 16 Jazz
  , "Bol Bol" # 17 Nuggets
  , "Leandro Bolmaro" # 18 Timberwolves
  , "CJ Elleby" # 19 Trailblazers
  , "Isaiah Roby" # 20 Thunder
  , "Elfrid Payton" # 21 Suns, Payton is actually 14 but 15 has been injured since November
  , "Moses Moody" # 22 Warriors
  , "DeAndre Jordan" # 23 Lakers
  , "Justise Winslow" # 24 Clippers
  , "Jahmi'us Ramsey"  # 25 Kings
  , "Jarrett Culver" # 26 Grizzlies
  , "Marquese Chriss" # 27 Mavericks Just signed a two year deal after several 10 days with the Mavs
  , "Didi Louzada" # 28 Pelicans
  , "Jock Landale" # 29 Spurs
  , "Usman Garuba" # 30 Rockets
  )
  , ]

net_df2 <- net_df2[,-30]

net_df2 <- net_df2 %>%
mutate(Gill = ifelse(Player == "Anthony Gill", "Gill", "Not Gill")
       , ORtg = as.numeric(ORtg)
       , DRtg = as.numeric(DRtg)
       )

net_df3 <- net_df2 %>% left_join(select(gill_advanced,  "Player" = namePlayer, urlPlayerHeadshot))

g1 <- subset(net_df2, Player == "Anthony Gill")


p4 <- net_df2 %>%
  ggplot(aes(x =  ORtg, y =  DRtg)) +
  geom_point(aes(fill = Gill, col = Gill
  ), size = 5, shape = 21) +
  scale_fill_manual(values = c("#BA0C2F", "grey")) +
  scale_color_manual(values = c("#BA0C2F", "grey")) +
  geom_smooth(method = "lm", se = F, col = "dark grey") +
  ggrepel::geom_text_repel(data=g1, label="Anthony Gill") +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(x = "Offensive Rating", y = "Defensive Rating", title = "15th Ranked Players by\nOffensive and Defensive Rating"
       , caption = "Source: basketball-reference.com\nwizardspoints.substack.com")

ggsave("Net Rating.png", p4, width = 10, height = 8, dpi = 300, type = 'cairo')
