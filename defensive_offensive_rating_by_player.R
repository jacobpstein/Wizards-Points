###############################################
# Team net rating
# Session Info:
# R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
# Copyright (C) 2022 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20 (64-bit)
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ggimage)

# set seed
set.seed(20222712)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# get game info------

# get game ids
wiz_game_ids <- game_logs(seasons = 2023, result_types = "team") %>% filter(nameTeam == "Washington Wizards")

all_games <- game_logs(seasons = 2023, result_types = "team") 

wiz_games <- wiz_game_ids %>% 
  mutate(newdate = gsub(x=dateGame, pattern = "-", replacement="")
         , current_game = paste0(newdate, "0", slugTeamWinner)
         , lower_name = tolower(slugOpponent)
         , lower_wiz = tolower(slugTeam)
         , match_up = case_when(locationGame== "H" ~ paste0(lower_name, "-vs-", lower_wiz)
                                , locationGame == "A" ~ paste0(lower_wiz, "-vs-", lower_name))
  ) 

# record of last 11
wiz_games %>% 
  tail(n=11) %>% 
  group_by(outcomeGame) %>% 
  count()

# point differential of last 11
wiz_games %>% 
  tail(n=11) %>% 
  group_by(outcomeGame) %>% 
  summarize(mean = mean(plusminusTeam, na.rm=T)
            , max = max(plusminusTeam, na.rm=T)
            , median = median(plusminusTeam, na.rm=T)
            )

id <- tail(wiz_games$idGame, n = 11)

all_dates <- all_games %>% group_by(dateGame) %>% count() %>% ungroup() %>% group_by(dateGame) %>% tail(n=11)

all_games_last11 <- all_games %>% filter(dateGame %in% all_dates$dateGame)

all_games_last11_ids <- unique(all_games_last11$idGame)

box_scores(game_ids = id
           , box_score_types = c("Advanced")
           , result_types = c("player"
                              #, "team"
           )
           , join_data = TRUE
           , assign_to_environment = TRUE
           , return_message = TRUE)

player_ids <- dataBoxScorePlayerNBA %>% filter(slugTeam == "WAS") %>% group_by(idPlayer) %>% count() %>% select(idPlayer)

bref_players_stats(seasons = 2023)

pics <- dataBREFPlayerAdvanced %>% filter(idPlayerNBA %in% player_ids$idPlayer) %>% 
  select(idPlayerNBA, urlPlayerHeadshot)

wiz_df <- dataBoxScorePlayerNBA %>% filter(slugTeam == "WAS") %>% 
              group_by(idPlayer, namePlayer) %>% 
              summarize("Defensive Rating" = mean(drtg, na.rm=T)
                        , "Offensive Rating" = mean(ortg, na.rm=T)
                        , "Usage %" = mean(pctUSG, na.rm=T)) %>% 
              left_join(pics, by = c("idPlayer" = "idPlayerNBA")) 

# get league numbers

all_nba <- box_scores(game_ids = all_games_last11_ids
                      , box_score_types = c("Advanced")
                      , result_types = c("player"
                      )
                      , join_data = TRUE
                      , assign_to_environment = TRUE
                      , return_message = TRUE)

#asp_ratio <- 1.618 

asp_ratio <- 1.8 



p1 <- ggplot(wiz_df, aes(`Defensive Rating`, `Offensive Rating`, size = `Usage %`)) + 
  geom_image(aes(image=urlPlayerHeadshot
                 #, size = I(`Usage %`/2)
                 )
             , size = 0.1
             , by = "width"
             , asp = asp_ratio
             ) +
  geom_hline(aes(yintercept = mean(dataBoxScorePlayerNBA$ortg))
             ) +
  geom_vline(aes(xintercept = mean(dataBoxScorePlayerNBA$drtg))) +
  annotate("text", x = 12, y = 110.5, label = "League Off. Rating Over Past 11", size = 4) +
  annotate("text", x = 111, y = 97, label = "League Def. Rating Over Past 11", size = 4, angle = 90) +
  theme(legend.position = "NA") +
  theme_minimal() +
  theme(legend.position = "top"
        , legend.title = element_blank()
        , plot.background = element_rect(fill = "white")
        , text = element_text(size = 20)
  ) +
  labs(title = "The Wizards Landscape"
       , subtitle = "Offensive and Defensive Rating Over the Past 11 Games"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )
  
ggsave("def and off rating.png", p1, w = 12, h = 10, dpi = 300, type = "cairo")

# all games

box_scores(game_ids = wiz_games$idGame
           , box_score_types = c("Advanced")
           , result_types = c("player"
                              #, "team"
           )
           , join_data = TRUE
           , assign_to_environment = TRUE
           , return_message = TRUE)



wiz_df2 <- dataBoxScorePlayerNBA %>% filter(slugTeam == "WAS") %>% 
  group_by(idPlayer, namePlayer) %>% 
  summarize("Defensive Rating" = mean(drtg, na.rm=T)
            , "Offensive Rating" = mean(ortg, na.rm=T)
            , "Usage %" = mean(pctUSG, na.rm=T)) %>% 
  left_join(pics, by = c("idPlayer" = "idPlayerNBA")) 


p2 <- ggplot(wiz_df2, aes(`Defensive Rating`, `Offensive Rating`, size = `Usage %`)) + 
  geom_image(aes(image=urlPlayerHeadshot
                 #, size = I(`Usage %`/2)
  )
  , size = 0.06
  , by = "width"
  , asp = asp_ratio
  ) +
  geom_hline(yintercept = 111.5) + # overall average looked up on basketball reference
  geom_vline(xintercept = 113.7) +
  annotate("text", x = 55, y = 110.5, label = "League Off. Rating", size = 4) +
  annotate("text", x = 113, y = 80, label = "League Def. Rating", size = 4, angle = 90) +
  theme(legend.position = "NA") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")
        , text = element_text(size = 20)
  ) +
  labs(title = "The Wizards Landscape"
       , subtitle = "Offensive and Defensive Rating Over the Past 40 Games"
       , caption = "data: nba.com\nwizardspoints.substack.com"
  )

ggsave("def and off rating overall for season.png", p2, w = 12, h = 10, dpi = 300, type = "cairo")



wiz_df3 <- dataBoxScorePlayerNBA %>% filter(slugTeam == "WAS") %>% 
  rename("Defensive Rating" = drtg
            , "Offensive Rating" = ortg
            , "Usage %" = pctUSG) %>% 
  left_join(pics, by = c("idPlayer" = "idPlayerNBA")) 

library(gganimate)

game_nums <- wiz_df3 %>% arrange(idGame) %>% 
  group_by(idGame) %>% 
  count() %>% 
  rownames_to_column(var = "gamecount") %>% 
  select(-n)

wiz_df3 <- wiz_df3 %>% left_join(game_nums)

p4 <- ggplot(wiz_df3, aes(`Defensive Rating`, `Offensive Rating`, size = `Usage %`)) + 
  geom_image(aes(image=urlPlayerHeadshot
                 #, size = I(`Usage %`/2)
  )
  , size = 0.06
  , by = "width"
  , asp = asp_ratio
  ) +
  geom_hline(yintercept = 111.5) + # overall average looked up on basketball reference
  geom_vline(xintercept = 113.7) +
  annotate("text", x = 55, y = 110.5, label = "League Off. Rating", size = 4) +
  annotate("text", x = 113, y = 80, label = "League Def. Rating", size = 4, angle = 90) +
  theme(legend.position = "NA") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")
        , text = element_text(size = 20)
  ) +
  labs(
       caption = "data: nba.com\nwizardspoints.substack.com"
  ) 

# annimated figure, this kind of makes me feel like I'm going insane
p4 + transition_states(gamecount, transition_length = 3, state_length = 1) +
  labs(title = 'The Wizards Landscape'
       , subtitle = "Offensive and Defensive Rating in Game {closest_state}"
       )


p5 <- ggplot(wiz_df3, aes(`Defensive Rating`, `Offensive Rating`, size = `Usage %`)) + 
  geom_point(aes(fill = netrtg), shape = 21, color = "white", stroke = 1, alpha = 0.7) +
  geom_hline(yintercept = 111.5) + # overall average looked up on basketball reference
  geom_vline(xintercept = 113.7) +
  viridis::scale_fill_viridis(option = 'rocket') +
  theme(legend.position = "NA") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")
        , text = element_text(size = 20)
        , legend.position = "NA"
  ) +
  labs(title = "The Wizards Landscape"
       , subtitle = "Offensive and Defensive Rating for Games 1-40\nDarker points suggest a higher overall net rating"
    , caption = "data: nba.com\nwizardspoints.substack.com"
  ) +
  facet_wrap(~namePlayer)

ggsave("net rating by player.png", p5, w = 12, h = 10, dpi = 300, type = "cairo")


# team stats
id <- (wiz_games$idGame)

box_scores(game_ids = id
           , box_score_types = c("Advanced")
           , result_types = c("team")
           , join_data = TRUE
           , assign_to_environment = TRUE
           , return_message = TRUE)
