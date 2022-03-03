###############################################
# Where is everyone?
# Session Info:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
###############################################

# Load packages and set things up-------
library(tidyverse)
library(nbastatR)
library(extrafont)
library(rvest)
library(lubridate)

# set seed
set.seed(20483789)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

dat_team <- game_logs(seasons = 2022, result_types = "team")

dat_wiz <- dat_team %>% filter(nameTeam == "Washington Wizards")

five38 <- read_csv("https://projects.fivethirtyeight.com/nba-model/nba_elo.csv")


year <- c(2012:2022)
month <- rep(c("october", "november", "december", "january", "february", "march", "april", "may"), 11)


# loops to get data
get_urls <- function(year) {
  
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                "_games-", month, ".html")
  
  }

urls <- get_urls(year)

urls[31] <- "https://www.basketball-reference.com/leagues/NBA_2020_games-october-2019.html"

urls[9] <- "https://www.basketball-reference.com/leagues/NBA_2020_games-october-2020.html"


# clean up urls
urls2 <- urls[c(-1 # 2011-2012 season started in December, this is October
               , -34 # 2011-2012 season started in December, this is November
               , -64 # no May 2020 games due to Covid-19
               , -65 # no October 2021 games due to Covid-19
               , -10 # no November 2021 games to Covid-19
               , -55 # we haven't made it to April 2022 yet
               , -88 # we haven't made it to May 2022 yet
               , -22 # haven't made it to March 2022 yet
               )]



get_data <- function(urls2) {
  webpage <- read_html(urls2)
  
  col_names <- webpage %>% 
    html_nodes("table#schedule > thead > tr > th") %>% 
    html_attr("data-stat")    
  col_names <- c("game_id", col_names)
  
  dates <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>% 
    html_text()
  dates <- dates[dates != "Playoffs"]
  
  game_id <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>%
    html_attr("csk")
  game_id <- game_id[!is.na(game_id)]
  
  data <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > td") %>% 
    html_text() %>%
    matrix(ncol = length(col_names) - 2, byrow = TRUE)
  
  month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
  names(month_df) <- col_names
  
  month_df <- month_df %>% 
    filter(home_team_name == "Washington Wizards" | visitor_team_name == "Washington Wizards") %>%
    mutate(attendance = as.numeric(gsub(",", "", attendance))
           , date_game = mdy(date_game)
           , home_pts = as.numeric(home_pts)
           , visitor_pts = as.numeric(visitor_pts)
    )  
  
}

nba_list <- NULL
for(i in 1:length(urls2)){
  tmp <- get_data(urls2[i]) 
  nba_list[[i]] <- tmp
}

# collapse tables
nbastart_dat <- do.call(rbind, nba_list)

# glimpse(nbastart_dat)

nbastart_dat2 <- nbastart_dat %>% 
  mutate(result = case_when(visitor_team_name == "Washington Wizards" & visitor_pts > home_pts ~ "W"
                            , home_team_name == "Washington Wizards" & home_pts > visitor_pts ~ "W"
                            , visitor_team_name == "Washington Wizards" & visitor_pts < home_pts ~ "L"
                            , home_team_name == "Washington Wizards" & home_pts < visitor_pts ~ "L"
  )) %>% 
  select(-game_remarks)

get_streaks <- function(vec){
  x <- data.frame(result=vec)
  x <- x %>% mutate(lagged=lag(result)) %>%  #note: that's dplyr::lag, not stats::lag
    mutate(start=(result != lagged))
  x[1, "start"] <- TRUE
  x <- x %>% mutate(streak_id=cumsum(start))
  x <- x %>% group_by(streak_id) %>% mutate(streak=row_number()) %>%
    ungroup()
  return(x)
}
streaks <- get_streaks(nbastart_dat2$result) %>% 
  mutate(streak = streak * ifelse(result == "W", 1, -1))

nbastart_dat3 <- nbastart_dat2 %>% bind_cols(select(streaks, streak)) %>% 
  rename()

# update fivethirtyeight data
five38_wiz <- five38 %>% filter(season %in% year) %>% 
              filter(team1 == "WAS" | team2 == "WAS")


merge_wiz <- five38_wiz %>% 
                left_join(nbastart_dat3, by = c("date" = "date_game"))

# let's just look at attendance over time


# let's just look at overall attendance over time

# merge_wiz %>% 
#   filter(home_team_name== "Washington Wizards" 
#          & attendance!=0
#   ) %>% 
#   group_by(season) %>% 
#   mutate(mean_att = mean(attendance)) %>% 
#   ungroup() %>% 
#   ggplot() +  
#   geom_line(aes(date, attendance), alpha = 0.1) +
#   # geom_smooth(aes(x = date, y = mean_att), alpha = 0.1, col = "red", size = 1, se = F, span = 0.1) +
#   geom_smooth(aes(date, attendance), alpha = 1, span = 0.09, se = F, size = 2) +
#   theme_minimal() +
#   labs(x = "", y = "Attendance"
#        , title = "Overall average attendance"
#        , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
#   )
# 
# merge_wiz %>% 
#   filter(home_team_name== "Washington Wizards" 
#          & attendance!=0
#   ) %>% 
#   group_by(season) %>% 
#   mutate(mean_att = mean(attendance)) %>% 
#   ungroup() %>% 
#   ggplot() +
#   geom_point(aes(date, attendance), alpha = 0.2) +
#   geom_smooth(aes(x = date, y = mean_att), col = "red", size = 2, se = F, span = 0.1) +
#   theme_minimal() +
#   labs(x = "", y = "Attendance"
#      , title = "Overall average attendance"
#      , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
#      )

# by season
merge_wiz %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         & season!=2021
         ) %>% 
  # group_by(season) %>% 
  # mutate(mean_att = mean(attendance)) %>% 
  # ungroup() %>% 
  ggplot() +
  # geom_line(aes(x = date, y = mean_att), col = "red") +
  geom_line(aes(x = date, y = attendance, group = season), alpha = 0.3) +
  geom_smooth(aes(x = date, y = attendance, group = season), se = F) +
  scale_x_date(date_breaks = "2 month", date_labels =  "%b") +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_grid(~season, scales = "free_x", space = "free_x") +
  theme_minimal() +
  theme(panel.spacing = unit(0, 'lines')
       # , axis.text.x=element_text(angle=60, hjust=1)
       ) +
  labs(x = "", y = "Attendance"
       , title = "Attendance by Season"
       , subtitle = "2021 has been removed from the figure since there were only seven home games and limited seating was made available"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
       )

merge_wiz %>% 
  filter(home_team_name== "Washington Wizards") %>% 
  select(-playoff, -neutral, -team1,-date, -team2, -game_id, -game_start_time, -visitor_team_name, -overtimes, -home_team_name, -box_score_text) %>% 
  pivot_longer(cols = -c(attendance
                         , result)
               , names_to = "Stat", values_to= "Value") %>% 
  filter(Stat %in% c("elo_prob1", "carm-elo_prob1", "quality", "streak")) %>%
  mutate(Value = ifelse(Stat %in% c("quality", "streak")==T, Value/100, Value)) %>% 
  ggplot(aes(y = attendance, x = Value)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~Stat)

merge_wiz2 <- merge_wiz %>% filter(home_team_name== "Washington Wizards" 
                                   & attendance>0 ) %>% 
  mutate(log_att = log(attendance))


m1 <- lm(log_att ~ 
           elo_prob1
         + quality
         + lag(log_att)+ lag(log_att, 2) + season 
         + poly(streak, 2), data = merge_wiz2)
