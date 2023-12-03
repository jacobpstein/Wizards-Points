##############################
# This script looks at game-to-game changes in key metrics for the 2023-24 Wizards
# Date created: 12/3/2024
# R version 4.3.1 (2023-06-16)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sonoma 14.1.1
##############################

# Load packages
library(tidyverse) # the usual
library(janitor) # for easy cross-tabs and a few other aesthetic fixes
library(httr) # for directly querying the NBA API rather than using wrapper packages
library(jsonlite) # for dealing with JSON output from NBA.com
library(viridis)

# set seed
set.seed(20222712)

# going to need to expand the conneciton size for our api calls
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# pull game to game stats from NBA's API directly---------
# doing this on a per 100 posession basis
# this should take anywhere from one to five minutes or so depending on 
# your internet connection and probably some other stuff like how many
# people are checking stats

# Function to retrieve player data for 2011 through 2023
# this is pretty much a basic api function that could be wrapped up into 
# something more sophisticated

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

# Specify the seasons of interest---
start_season <- 2023
end_season <- 2023

# create null object----
df <- NULL

# we need to run this to get the advanced numbers and then to get totals just for minutes
for (season in start_season:end_season) {
  season_string <- paste0(season, "-", (season + 1) %% 100)  # Convert season to "yyyy-yy" format
  
  # create a url object, this can be updated depending on the NBA end point we want
  url <- paste0("https://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo=&GameSegment=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OppTeamID=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerID=&PlusMinus=N&Rank=N&Season=" 
                , season_string
                , "&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&TeamID=&VsConference=&VsDivision=")
  # query the site
  res <- GET(url = url, add_headers(.headers=headers))
  
  # convert to json
  json_res <- fromJSON(content(res, "text"))
  
  # convert to a dataframe
  tmp_dat <- data.frame(json_res$resultSets$rowSet[[1]]) 
  
  # the json file contains multiple objects with headers and values split, add the headers back in
  names(tmp_dat) <- data.frame(json_res[["resultSets"]][["headers"]])$c..SEASON_YEAR....PLAYER_ID....PLAYER_NAME....NICKNAME....TEAM_ID...
  
  # toss all of our data frames for each year into a list
  df[[season_string]] <- tmp_dat
  
}

# we have the season name as the name of each list
# let's create a function to bring the season into each data frame
named_df_list <- Map(function(df, name) {transform(df, season = name)}, df, names(df))

df <- do.call(rbind, named_df_list) |> clean_names() |> 
  mutate_at(.vars = c(12:54), .funs = as.numeric) |> 
  mutate(game_date = lubridate::as_date(game_date)) |> 
  filter(team_abbreviation == "WAS")

# minutes

p1 <- df |> 
  filter(!player_name %in% c("Patrick Baldwin Jr.", "Jared Butler")) |> 
  select(game_id, game_date, player_name, fga, min) |> 
  arrange(player_name, game_date) |> 
  group_by(player_name) |> 
  mutate(pct_change = (min-lag(min))/lag(min)
         # , pct_change = ifelse(pct_change %in% c(NaN, NA, Inf), 0, pct_change)
  ) |> 
  ggplot(aes(x = game_date, y = pct_change)) +
  geom_line(aes(col = player_name, group = player_name), size = 1, alpha = 0.7) +
  geom_point(aes(fill = player_name, size = min), shape = 21, color = "white", stroke = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_size_continuous(range = c(1, 5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis(discrete = T) +
  theme(legend.position = "NA"
  ) +
  facet_wrap(~player_name, scales = "free_y") +
  theme_classic() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)
        , axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1)) +
  labs(x = "", y = "Game-to-game % change in minutes"
       , title = "Game-to-game changes in minutes played so far in the 2023-24 season"
       , subtitle = "Dots are sized by minutes"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 

ggsave("Change in minutes.png", p1, w = 16, h = 14, dpi = 300)


p2 <- df |> 
  filter(!player_name %in% c("Patrick Baldwin Jr.", "Jared Butler")) |> 
  select(game_id, game_date, player_name, fga, min) |> 
  arrange(player_name, game_date) |> 
  group_by(player_name) |> 
  mutate(pct_change = (fga-lag(fga))/lag(fga)
         # , pct_change = ifelse(pct_change %in% c(NaN, NA, Inf), 0, pct_change)
  ) |> 
  ggplot(aes(x = game_date, y = pct_change)) +
  geom_line(aes(col = player_name, group = player_name), size = 1, alpha = 0.7) +
  geom_point(aes(fill = player_name, size = min), shape = 21, color = "white", stroke = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_size_continuous(range = c(1, 5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis(discrete = T) +
  theme_classic() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)
        , axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~player_name, scales = "free_y") +
  labs(x = "", y = "Game-to-game % change in field goal attempts"
       , title = "Game-to-game changes in field goal attempts so far in the 2023-24 season"
       , subtitle = "Dots are sized by minutes"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 

p2

ggsave("Change in fga.png", p2, w = 16, h = 14, dpi = 300)

# deni effective field goal in games with two or more quarters
df |> 
  filter(!player_name %in% c("Patrick Baldwin Jr.", "Jared Butler")) |> 
  select(game_id, game_date, player_name, fga, min, efg_pct) |> 
  arrange(player_name, game_date) |> 
  group_by(player_name) |> 
  mutate(pct_change = (fga-lag(fga))/lag(fga)
         # , pct_change = ifelse(pct_change %in% c(NaN, NA, Inf), 0, pct_change)
  ) |> filter(grepl("Deni", player_name) ) |> 
  filter(min>=24) |> summarize(mean(efg_pct))


p3 <- df |> 
  filter(!player_name %in% c("Patrick Baldwin Jr.", "Jared Butler")) |> 
  select(game_id, game_date, player_name, efg_pct, min) |> 
  arrange(player_name, game_date) |> 
  group_by(player_name) |> 
  mutate(pct_change = (efg_pct-lag(efg_pct))/lag(efg_pct)
         # , pct_change = ifelse(pct_change %in% c(NaN, NA, Inf), 0, pct_change)
  ) |> 
  ggplot(aes(x = game_date, y = pct_change)) +
  geom_line(aes(col = player_name, group = player_name), size = 1, alpha = 0.7) +
  geom_point(aes(fill = player_name, size = min), shape = 21, color = "white", stroke = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_size_continuous(range = c(1, 5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis(discrete = T) +
  theme_classic() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)
        , axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~player_name, scales = "free_y") +
  labs(x = "", y = "Game-to-game % change in effective field goal percentage"
       , title = "Game-to-game changes in effective field percentage so far in the 2023-24 season"
       , subtitle = "Dots are sized by minutes"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 

p3

ggsave("Change in effective field goal percentage.png", p3, w = 16, h = 14, dpi = 300)


# change in True Shooting %

df |> 
  filter(!player_name %in% c("Patrick Baldwin Jr.", "Jared Butler")) |> 
  select(game_id, game_date, player_name, ts_pct, min) |> 
  arrange(player_name, game_date) |> 
  group_by(player_name) |> 
  mutate(pct_change = (ts_pct-lag(ts_pct))/lag(ts_pct)
         # , pct_change = ifelse(pct_change %in% c(NaN, NA, Inf), 0, pct_change)
  ) |> 
  ggplot(aes(x = game_date, y = pct_change)) +
  geom_line(aes(col = player_name, group = player_name), size = 1, alpha = 0.7) +
  geom_point(aes(fill = player_name, size = min), shape = 21, color = "white", stroke = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_size_continuous(range = c(1, 5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis(discrete = T) +
  theme(legend.position = "NA"
  ) +
  facet_wrap(~player_name, scales = "free_y") +
  theme_classic() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(x = "", y = "Game-to-game % change in True Shooting"
       , title = "Game-to-game changes in true shooting percentage so far in the 2023-24 season"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 



# usage by player
df |> 
  filter(!player_name %in% c("Patrick Baldwin Jr.", "Jared Butler")) |> 
  select(game_id, game_date, player_name, usg_pct, min) |> 
  arrange(player_name, game_date) |> 
  group_by(player_name) |> 
  ggplot(aes(x = game_date, y = usg_pct)) +
  geom_line(aes(col = player_name, group = player_name), size = 1, alpha = 0.7) +
  geom_point(aes(fill = player_name, size = min), shape = 21, color = "white", stroke = 1) +
  scale_size_continuous(range = c(1, 5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis(discrete = T) +
  theme(legend.position = "NA"
  ) +
  facet_wrap(~player_name) +
  theme_classic() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(x = "", y = "Usage %"
       , title = "Game-to-game usage so far in the 2023-24 season"
       , subtitle = "Dots are sized by minutes"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 


