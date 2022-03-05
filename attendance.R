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
library(RColorBrewer)
library(ggridges)
library(tidybayes)
library(rstanarm)

# set seed
set.seed(20483789)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

dat_team <- game_logs(seasons = c(2012:2022), result_types = "team")

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
                # , -22 # haven't made it to March 2022 yet
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


# update fivethirtyeight data
five38_wiz <- five38 %>% filter(season %in% year) %>% 
  filter(team1 == "WAS" | team2 == "WAS")


merge_wiz <- five38_wiz %>% 
  left_join(nbastart_dat2, by = c("date" = "date_game"))

# clean thing up a bit
merge_wiz2 <- merge_wiz %>% filter(home_team_name== "Washington Wizards" 
                                   & attendance>0 ) %>% 
  mutate(log_att = log(attendance)) %>% 
  left_join(dat_wiz, by = c("date" = "dateGame")) %>% 
  select(-c("carm-elo1_pre":"raptor_prob2"), -playoff
         , -neutral
         , -importance
         , -total_rating
         , -overtimes
         , -slugLeague
         , -urlTeamSeasonLogo
         , -idGame
         , -hasVideo
  )

# calculate streaks
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

# calculate streaks
streaks <- get_streaks(merge_wiz2$result) %>% 
  mutate(streak = streak * ifelse(result == "W", 1, -1))


# add in streaks
merge_wiz3 <- merge_wiz2 %>% bind_cols(select(streaks, streak)) %>% 
  mutate(month = month(date, label = T)
         , day = wday(date, label =T))



# weather data
weather_dat <- readr::read_csv("C:/Users/j.pattersonstein/Downloads/2894148.csv")

merge_wiz4 <- merge_wiz3 %>% left_join(weather_dat, by = c("date" = "DATE")) %>% 
  mutate(spread = home_pts-visitor_pts) %>% 
  bind_cols(fastDummies::dummy_cols(merge_wiz4$day)) %>% 
  select(-'.data') %>% 
  bind_cols(fastDummies::dummy_cols(merge_wiz4$month)) %>% 
    select(-'.data')


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


# attendance by team
# 
# merge_wiz3 %>% 
#   filter(home_team_name== "Washington Wizards" 
#          & attendance!=0
#          & season!=2021
#   ) %>% 
#   group_by(team2) %>% 
#   mutate(med = median(attendance, na.rm=T)) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = reorder(team2,med), y = attendance , group = team2)) +
#   geom_jitter(alpha = 0.3, width = .2) +
#   geom_boxplot(fill = NA) +
#   scale_y_continuous(labels = scales::comma_format()) +
#   # facet_grid(~season, scales = "free_x", space = "free_x") +
#   theme_minimal() +
#   # theme(panel.spacing = unit(0, 'lines')
#         # , axis.text.x=element_text(angle=60, hjust=1)
#   # ) +
#   labs(x = "", y = "Attendance"
#        , title = "Attendance by Team"
#        , subtitle = "2021 has been removed from the figure since there were only seven home games and limited seating was made available"
#        , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
#        
#   )

merge_wiz3 %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         & season!=2021
  ) %>% 
  group_by(team2) %>% 
  mutate(med = mean(attendance, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(team2,med), y = attendance , group = team2)) +
  stat_summary(fun.data = "mean_cl_boot", size = 2) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal() +
  labs(x = "", y = "Attendance"
       , title = "Attendance by Team"
       , subtitle = "2021 has been removed from the figure since there were only seven home games and limited seating was made available"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
  )


# attendance by day


merge_wiz3 %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         & season!=2021
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = day, y = attendance , group = day)) +
  stat_summary(fun.data = "mean_cl_boot", size = 2) +
  scale_y_continuous(labels = scales::comma_format()) +
  # facet_grid(~factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")),  scales = "free_x") +
  theme_minimal() +
    theme(panel.spacing = unit(0, 'lines')
    # , axis.text.x=element_text(angle=60, hjust=1)
    ) +
  labs(x = "", y = "Attendance"
       , title = "Attendance by Day and Month"
       , subtitle = "2021 has been removed from the figure since there were only seven home games and limited seating was made available"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
  )

# which games/teams had the most attendance
merge_wiz3 %>% 
  filter(season!=2021) %>% 
  group_by(visitor_team_name, date) %>% 
  summarize(max = max(attendance, na.rm=T)
            , perct = max/20476
            ) %>% 
  arrange(desc(max)) %>% 
  head(n = 50) %>% 
  ungroup() %>% 
  group_by(visitor_team_name) %>% 
    count() %>% 
  arrange(desc(n)) %>% 
  print(n=20)

# bottom
merge_wiz3 %>% 
  filter(season!=2021) %>% 
  group_by(visitor_team_name, date) %>% 
  summarize(max = max(attendance, na.rm=T)
            , perct = max/20476
  ) %>% 
  arrange(desc(max)) %>% 
  tail(n = 50) %>% print(n=50)

merge_wiz3 %>% 
  group_by(date) %>% 
  mutate(max = max(attendance, na.rm=T)
         , perct = max/20476
  ) %>% group_by(season) %>% summarize(mean = mean(perct))


cors <- cor(merge_wiz3[ , purrr::map_lgl(merge_wiz3, is.numeric)] # just keep the numeric variables
            , use="pairwise.complete.obs") # matrix completition

corrplot::corrplot(cors) # correlations

# linear model
m1 <- lm(log_att ~ 
           elo_prob1
         + quality
         + lag(log_att)
         + lag(log_att, 2)
         + PRCP
         + TMAX
         + lag(spread)
         # + streak
         # + plusminusTeam
         # + lag(plusminusTeam)
         # + slugOpponent
         + lag(outcomeGame)
         # + fgmTeam
         # + lag(fgmTeam)
         # + lag(fgaTeam)
         # + lag(astTeam)
         # + ptsTeam
         # + lag(ptsTeam)
         + month
         + day
         + factor(season)*slugOpponent
         , data = merge_wiz4)


summary(m1)

hist(predict(m1))

# bayes
m2 <- stan_glmer(log_att ~ 
            elo_prob1
          + quality
          + lag(log_att)
          + lag(log_att, 2)
          + streak
          + lag(outcomeGame)
          + .data_Mon
          + .data_Tue
          + .data_Wed
          + .data_Thu
          + .data_Fri
          + .data_Sat
          + .data_Jan
          + .data_Feb
          + .data_Mar
          + .data_Apr
          + .data_May
          + .data_Jun
          + .data_Jul
          + .data_Aug
          + .data_Sep
          + .data_Oct
          + .data_Nov
          + .data_Dec
          + (1|season)
          + (1|slugOpponent)
          , data = merge_wiz4
          )

summary(m2)

plot(m2)

m2.out <- m2 %>%
  spread_draws(`(Intercept)`, b[,slugOpponent]) %>%
  mutate(other = `(Intercept)` + b)

ggplot(m2.out, aes(y=fct_rev(slugOpponent), x=other, fill = slugOpponent)) + 
  stat_halfeye(alpha = 0.8) +
  geom_vline(aes(xintercept= mean(`(Intercept)`)), size=1.2, color="darkgrey", alpha=.4) +
  theme_minimal() +
  theme(legend.position = "NA")


# basic model
m3 <- stan_glmer(log_att ~ 
                   elo_prob1
                 + quality
                 + lag(log_att)
                 + lag(log_att, 2)
                 + streak
                 + lag(outcomeGame)
                 + .data_Mon
                 + .data_Tue
                 + .data_Wed
                 + .data_Thu
                 + .data_Fri
                 + .data_Sat
                 + .data_Jan
                 + .data_Feb
                 + .data_Mar
                 + .data_Apr
                 + .data_May
                 + .data_Jun
                 + .data_Jul
                 + .data_Aug
                 + .data_Sep
                 + .data_Oct
                 + .data_Nov
                 + .data_Dec
                 # + (1|season)
                 + (season|slugOpponent)
                 , data = merge_wiz4[merge_wiz4$season!=2021,]
)

summary(m3)

plot(m3)

m3.out <- m3 %>%
  spread_draws(`(Intercept)`, b[season,slugOpponent]) %>%
  mutate(other = `(Intercept)` + b)

ggplot(m3.out, aes(y=fct_rev(slugOpponent), x=other, fill = slugOpponent)) + 
  stat_halfeye(alpha = 0.8) +
  geom_vline(aes(xintercept= mean(`(Intercept)`)), size=1.2, color="darkgrey", alpha=.4) +
  theme_minimal() +
  theme(legend.position = "NA")
