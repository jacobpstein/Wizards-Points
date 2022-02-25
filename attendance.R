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


year <- c(2012:2022)
month <- rep(c("october", "november", "december", "january", "february", "march", "april", "may"), 11)


# loops to get data
get_urls <- function(year) {
  
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                "_games-", month, ".html")
  
  }

urls <- get_urls(year)

urls[31] <- "https://www.basketball-reference.com/leagues/NBA_2020_games-october-2019.html"

# clean up urls
urls <- urls[c(-1 # 2011-2012 season started in December
               , -56 # no 2012 games in May
               , -66
               , -32
               , -43
               )]



get_data <- function(urls) {
  webpage <- read_html(urls)
  
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
    filter(home_team_name == "Washington Wizards") %>% 
    mutate(attendance = as.numeric(gsub(",", "", attendance))
           , date_game = mdy(date_game)
    )  
  
}

nba_list <- NULL
for(i in 1:length(urls)){
  tmp <- get_data(urls[i]) 
  nba_list[[i]] <- tmp
}

# collapse tables
nbastart_dat <- do.call(rbind, nba_list)

glimpse(nbastart_dat)


nbastart_dat %>% 
  mutate(season = case_when(date_game>=2011-12-26 & date_game <=2012-03-30~ "2011-12 Season"
                            , date_game>=2012-11-03 & date_game <=2013-03-31 ~ "2012-13 Season"
                            , date_game>=2013-11-01 & date_game <=2014-03-29 ~ "2012-14 Season"
                            , date_game >=2014-11-01 & date_game <=2014-03-29





                            
                            ))

