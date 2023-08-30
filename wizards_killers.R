###############################################
# Wizards Killers
# Session Info:
# R version 4.2.1 (2022-06-23)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.3.1
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(janitor)
library(lubridate)
library(ggridges)
library(ggrepel)
library(viridis)
library(httr) # for directly querying the NBA API rather than using wrapper packages
library(jsonlite) # for dealing with JSON output from NBA.com

# set seed
set.seed(20222712)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# get game ids
games <- game_logs(seasons = c(2016:2023), result_types = c("player"), season_types = c("Regular Season", "Playoffs"))

team_games <- game_logs(seasons = c(2016:2023), result_types = c("team"), season_types = c("Regular Season", "Playoffs")) %>% filter(nameTeam == "Washington Wizards")

# get player totals---

# Set the headers
headers <- c(
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

# Specify the seasons of interest
start_season <- 2015
end_season <- 2022

# create null object
df <- NULL

for (season in start_season:end_season) {
  season_string <- paste0(season, "-", (season + 1) %% 100)  # Convert season to "yyyy-yy" format
  
  # create a url object, this can be updated depending on the NBA end point we want
  url <- paste0("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season="
                , season_string
                , "&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  # query the site
  res <- GET(url = url, add_headers(.headers=headers))
  
  # convert to json
  json_res <- fromJSON(content(res, "text"))
  
  # convert to a dataframe
  tmp_dat <- data.frame(json_res$resultSets$rowSet[[1]]) 
  
  # the json file contains multiple objects with headers and values split, add the headers back in
  names(tmp_dat) <- data.frame(json_res[["resultSets"]][["headers"]])$c..PLAYER_ID....PLAYER_NAME....NICKNAME....TEAM_ID....TEAM_ABBREVIATION...
  
  # toss all of our data frames for each year into a list
  df[[season_string]] <- tmp_dat
  
}

# we have the season name as the name of each list
# let's create a function to bring the season into each data frame
named_df_list <- Map(function(df, name) {transform(df, season = name)}, df, names(df))

# combine all of our list objects into a data frame
# this is more or less what I imagine we'll work with for analysis
overall_df <- do.call(rbind, named_df_list) %>% 
  mutate(across(-c(2, 3, 5, 67), as.numeric)
  ) %>% 
  dplyr::select(-contains("RANK"))

# export
# write.csv(games, "play by play 2016-2022.csv")
games <- read_csv("play by play 2016-2022.csv")
# write.csv(overall_df, "season stats 2016-2022.csv")
# overall_df <- read_csv("season stats 2016-2022.csv")
#limit just to Wizards ames
wiz_games <- games %>% filter(slugOpponent=="WAS" | slugTeam=="WAS")

wiz_play_in <- overall_df_pi %>% filter(slugOpponent=="WAS" | slugTeam=="WAS")

# just the Wiz killers
killer_df <- wiz_games %>% filter(pts>=30)

# filter out Wiz players 
killer_no_wiz <- killer_df %>% filter(nameTeam!= "Washington Wizards")

# lets see if Wiz killers have increased

killer_no_wiz %>% 
  group_by(slugSeason) %>% 
  count()

seasonal_df <- select(overall_df, "slugSeason" = season, "season_pts" = PTS, "idPlayer" = PLAYER_ID, "namePlayer" = "PLAYER_NAME", "min" = MIN)

# combine data frames---
combined_df <- killer_no_wiz %>% 
  mutate(numberGameTeamSeason = ifelse(typeSeason=="Playoffs", numberGameTeamSeason+82, numberGameTeamSeason)) %>% 
  dplyr::select(yearSeason
                , slugSeason
                , numberGamePlayerSeason
                , numberGameTeamSeason
                , idGame
                , nameTeam
                , slugTeam
                , namePlayer
                , idPlayer
                , outcomeGame
                , pts) %>% 
  left_join(seasonal_df) %>% 
  left_join(seasonal_df %>% group_by(namePlayer, idPlayer) %>% summarize(pts_sd = sd(season_pts, na.rm=T), mean_pts = mean(season_pts, na.rm=T)))

# take a look at how the 30+ game compares to players' typical performance
combined_df2 <- combined_df %>% 
  group_by(idGame) %>% 
  mutate(diff_avg = pts-season_pts # 30+ game relative to their season per game average
         , thirty_above_sd = pts - (pts_sd + season_pts)
         , thirty_above_twosd = pts - ((pts_sd*2) + season_pts)
         , thirty_above_threesd = pts - ((pts_sd*3) + season_pts)
         , mean_plus_sd = season_pts + pts_sd
         , mean_minus_sd = season_pts - pts_sd) %>%  # 30+ game relative to season average + career standard deviation
ungroup()

# look at the distribution of overall points, season points, and Wiz killer game points
combined_df2 %>% 
  select(mean_pts, pts, season_pts, namePlayer, slugSeason) %>% 
  pivot_longer(cols = c(1:3)) %>% 
  ggplot(aes(value)) +
  geom_density(aes(col = name))

# take a look at the number of games per season
combined_df2 %>% 
  group_by(slugSeason) %>% 
  count() %>% 
  bind_cols(data.frame(def_rating = c(98.5, 97.4, 108.7, 113.9, 115.5, 113, 114.5, 115.6)))

# who is in our data more than once
combined_df2 %>% 
  group_by(namePlayer) %>% 
  count() %>% 
  arrange(desc(n))

# let's look at the randoms
combined_df2 %>% 
  group_by(namePlayer) %>% 
  count() %>% 
  # filter(n==1) %>% 
  print(n=150)

combined_df2 %>% 
  select(yearSeason, namePlayer, pts, mean_pts, diff_avg) %>% 
  ggplot(aes(x = yearSeason, ymin = mean_pts, ymax = pts, y = pts)) +
  geom_linerange(position = "jitter", aes(col = namePlayer)) +theme(legend.position = "NA")


p1 <- combined_df2 %>%   
  filter(thirty_above_twosd>=15) %>% 
  ggplot() +
  geom_linerange(aes(x = reorder(namePlayer, -pts), ymin = mean_minus_sd, ymax = mean_plus_sd, col = pts), size = 1) +
  geom_point(aes(x = reorder(namePlayer, -pts), y = pts, size = diff_avg, col = pts)) +
  geom_text(aes(label = pts, x = reorder(namePlayer, -pts), y = pts), hjust = -0.7, size = 6) +
  geom_text(aes(label = round(mean_minus_sd, 0), x = reorder(namePlayer, -pts), y = mean_minus_sd), vjust = -0.7, size = 6) +
  geom_text(aes(label = round(mean_plus_sd, 0), x = reorder(namePlayer, -pts), y = mean_plus_sd), vjust = -0.7, size = 6) +
  coord_flip() +
  scale_size_continuous(range = c(3, 12), guide = FALSE) +
  scale_color_gradient(low = "#3E356BFF",  high = "#49C1ADFF") +
  theme_classic() +
  theme(legend.position = "NA"
        , text = element_text(size = 22)) +
  labs(y = "Points", x = ""
       , title = "Range of per game shooting and 30+ point game\namong players who shot way above their average, 2015-2023"
       , subtitle = "The range shown is for the season the player hit 30 or more against the Wizards"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com\nNote: Range here is defined as +/- one standard deviation\nfor the player's per game average in the season they score 30 or more points."
  )

# ggsave("plus 2sd 30+ club.png", p1, w = 18, h = 12, dpi = 300)

# let's look at where in the season 30+ games are happening----

# regions we want to shade
game_ranges <- data.frame(
  from=c(26), 
  to=c(55)
)

p2 <- killer_no_wiz %>% 
  mutate(numberGameTeamSeason = ifelse(typeSeason=="Playoffs", numberGameTeamSeason+82, numberGameTeamSeason)) %>% 
  group_by(numberGameTeamSeason
           ) %>% 
  summarize(n = n()) %>% 
  ggplot() + 
  # geom_smooth(aes(x = numberGameTeamSeason, y = n), se = F, alpha = 0.1, span = 0.5) +
  geom_line(aes(x = numberGameTeamSeason, y = n)) +
  geom_point(aes(x = numberGameTeamSeason, y = n), col = "#B25D91", fill = "#B25D91", size = 4) +
  geom_rect(data = game_ranges, aes(xmin = from-1, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.15) +
  annotate("text", x = 45, y = 8, label = "The battling time", size = 7) +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme_classic() +
  theme(text = element_text(size = 22)) +
  labs(x = "Game Number"
       , y = "Number of players with 30+ point games"
       , title = "Number of Players with 30+ Points Against the Wizards by Game of the Year, 2015-2023"
       , subtitle = "The number of players dropping 30+ points peaks at game 33, which tends to be at the end of December"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  )

p2

# ggsave("thirty by game number.png", p2, w = 16, h = 12, dpi = 300)



p3 <- combined_df2 %>% 
  mutate(highlights = ifelse(namePlayer == "Cam Thomas", "Cam Thomas", ifelse(namePlayer == "Kemba Walker" & diff_avg>30, "Kemba Walker", ifelse(namePlayer == "James Harden" & diff_avg<0, "James Harden", ifelse(namePlayer == "Shai Gilgeous-Alexander" & diff_avg<0, "Shai Gilgeous-Alexander", "Everyone Else"))))) %>% 
  ggplot() + 
  geom_linerange(aes(x = numberGameTeamSeason, ymax = diff_avg, ymin = 0), alpha = 0.3) +
  geom_point(aes(x = numberGameTeamSeason, y = diff_avg, col = highlights, size = highlights)) +
  geom_rect(data = game_ranges, aes(xmin = from-1, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.15) +
  annotate("text", x = 40, y = 35, label = "The battling time", size = 7) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c( "#cd1041","#0076BB","#F58426", "#4F5791", "#B25D91")) +
  scale_size_manual(values = c(4, 2, 4, 4, 4)) +
  gghighlight::gghighlight(highlights %in% c("Cam Thomas", "Kemba Walker", "James Harden", "Shai Gilgeous-Alexander"), unhighlighted_params = list(colour = alpha("#B25D91", 0.4)), label_params = list(size = 5)) +
  theme_classic() +
  theme(text = element_text(size = 26)
        , legend.position = "NA") +
  labs(x = "Game Number"
       , y = "Points above season average"
       , title = "Points Above Season Average in 30+ Point Game by Player and\nGame of the Year, 2015-2023"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  )

p3

# ggsave("thirty above season average by player.png", p3, w = 16, h = 12, dpi = 300)


# how many 30+ point games were in wins or losses?----
combined_df2 %>% 
  filter(diff_avg>20) %>% 
  group_by(idGame, outcomeGame) %>% 
  count() %>% 
  group_by(outcomeGame) %>% 
  count()
