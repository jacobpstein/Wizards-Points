###############################################
# Load in Data for understanding Deni's tracking stats
# Session Info:
# R version 4.2.1 (2022-06-23)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.3.1
###############################################

# Load packages
library(tidyverse) # the usual
library(janitor) # for easy cross-tabs and a few other aesthetic fixes
library(httr) # for directly querying the NBA API rather than using wrapper packages
library(jsonlite) # for dealing with JSON output from NBA.com

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
start_season <- 2020
end_season <- 2023

# create null object----
df <- NULL

# we need to run this to get the advanced numbers and then to get totals just for minutes
for (season in start_season:end_season) {
  season_string <- paste0(season, "-", (season + 1) %% 100)  # Convert season to "yyyy-yy" format
  
  # create a url object, this can be updated depending on the NBA end point we want
  url <- paste0("https://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo=&GameSegment=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Scoring&Month=0&OppTeamID=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerID=1630166&PlusMinus=N&Rank=N&Season=" 
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

deni_df <- do.call(rbind, named_df_list) |> clean_names() |> 
  mutate_at(.vars = c(12:54), .funs = as.numeric) |> 
  mutate(num_of_paint_points = pct_pts_paint*fga
         , game_date = lubridate::as_date(game_date))


deni_long <- deni_df |> 
  select(season_year
         , game_date
         , game_id
         , wl
         , c(min:fg_pct)
         , num_of_paint_points
         ) |> 
  mutate(game_date = lubridate::as_date(game_date)) |> 
  pivot_longer(cols = c(5:24))

deni_long |> 
  filter(name == "pct_pts_paint") |> 
  group_by(season_year) |> 
  summarize(mean_pts = mean(value, na.rm=T))

p1 <- deni_df |> 
  filter(season_year == "2022-23") |>
  group_by(wl) |> 
  mutate(roll_avg = zoo::rollapply(pct_pts_paint, 5, mean, align = "left", fill = NA)) |> 
  ggplot(aes(x = game_date, y = pct_pts_paint)) +
  geom_hline(aes(yintercept = mean(pct_pts_paint, na.rm=T)), linetype = 3) + 
  geom_point(aes(fill = wl, col = wl), shape = 21, alpha = 0.3) +
  geom_line(aes(col = wl), alpha = 0.3) +
  geom_smooth(aes(y = roll_avg, col = wl), span = 0.1, size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(legend.position = "NA"
        , text = element_text(size = 26)) +
  labs(x = "", y = "Percentage of Points in the Paint"
       , title = "Deni Avdija's percentage of points in the paint for the 2022-23 season\nduring Wizards wins and loses"
       , subtitle = "Darker lines show five game rolling average"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
       ) +
  annotate("text", x = as.Date("2022-10-28"), y = 0.52, label = "Season avg.", size = 7) +
  annotate("text", x = as.Date("2022-11-14"), y = .59, label = "Wiz Lost", col = "#440154", size = 7, fontface = "bold") +
  annotate("text", x = as.Date("2022-11-19"), y = .38, label = "Wiz Won", col = "#21918c", size = 7, fontface = "bold") +
  scale_color_manual(values = c("#440154", "#21918c"))

ggsave("Deni in the paint.png", p1, w = 16, h = 14, dpi = 300)

  

deni_df |> 
  filter(season_year == "2022-23") |>
  group_by(wl) |> 
  mutate(roll_avg = zoo::rollapply(pct_pts_paint, 5, mean, align = "left", fill = NA)) |> 
  ggplot(aes(x = game_date, y = pct_pts_paint)) +
  geom_hline(aes(yintercept = mean(pct_pts_paint, na.rm=T)), linetype = 3) + 
  geom_point(aes(col = wl, size = num_of_paint_points), shape = 21, stroke = 2, alpha = 0.3) +
  geom_smooth(aes(y = roll_avg, col = wl), span = 0.1) +
  scale_size(range = c(.5, 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(legend.position = "NA") +
  labs(x = "", y = "Percentage of Points in the Paint"
       , subtitle = "Dots are sized by overall number of paint attempts")

deni_long |> 
  filter(name == "pct_pts_paint") |>
  filter(season_year == "2022-23") |>
  group_by(wl) |> 
  mutate(roll_avg = zoo::rollapply(value, 5, mean, align = "left", fill = NA)) |> 
  ggplot(aes(x = game_date, y = value)) +
  geom_hline(aes(yintercept = mean(value, na.rm=T), col = wl)) + 
  geom_point(aes(col = wl), shape = 21, stroke = 2, alpha = 0.3) +
  geom_smooth(aes(y = roll_avg, col = wl)) +
  theme_classic()
  # facet_wrap(~name, scales = "free_y")

deni_df |> 
  select(season_year
         , game_date
         , game_id
         , contains("rank")
  ) |> 
  mutate(game_date = lubridate::as_date(game_date)) |> 
  pivot_longer(cols = c(4:26)) |> 
  mutate(value = as.numeric(value)) |> 
  group_by(season_year, name) |> 
  summarize(mean = mean(value, na.rm=T))
  
