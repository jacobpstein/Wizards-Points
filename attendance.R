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
library(teamcolors)
library(lme4)


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


# actuall get the data
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

# bring all seasons together into a list
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


# calculate streaks
# from https://www.r-bloggers.com/2020/06/detecting-streaks-in-r/
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

streaks <- get_streaks(merge_wiz$result[is.na(merge_wiz$result)!=T]) %>% 
  mutate(streak = streak * ifelse(result == "W", 1, -1)
  )


# clean thing up a bit
merge_wiz2 <- merge_wiz %>% filter(is.na(result)!=T) %>% 
  bind_cols(select(
    streaks
    ,streak)) %>% 
  filter(home_team_name== "Washington Wizards" 
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


# add in season win percentage
win_pct <- merge_wiz %>% 
  mutate(win = ifelse(result == "W", 1, 0)) %>% 
  filter(attendance>0) %>% group_by(season) %>%
  mutate(winpct = cummean(win)) %>% select(season, date, winpct, win)

# add in streaks
merge_wiz3 <- merge_wiz2 %>%
  mutate(month = month(date, label = T)
         , day = wday(date, label =T)) %>% 
  left_join(tm.colors, by = c("visitor_team_name" = "nameTeam")) %>% 
  left_join(select(win_pct, date, win, winpct))



# weather data from NOAA 
# https://www.ncdc.noaa.gov/cdo-web/datasets#GHCND
weather_dat <- readr::read_csv("2894148.csv")

merge_wiz4 <- merge_wiz3 %>% 
  left_join(weather_dat, by = c("date" = "DATE")) %>% 
  mutate(spread = home_pts-visitor_pts) %>% 
  bind_cols(fastDummies::dummy_cols(.$day)) %>% 
  select(-'.data') %>% 
  bind_cols(fastDummies::dummy_cols(.$month)) %>% 
  select(-'.data') %>% 
  filter(team2!="NJN"
         & typeSeason == "Regular Season"
  ) 



# let's just look at attendance over time

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

# overall average-----

merge_wiz4 %>% 
  filter(home_team_name== "Washington Wizards"
         & attendance!=0
         & season!=2021
         
  ) %>% 
  summarize(mean = mean(attendance, na.rm=T)) %>% 
  mutate(perct = mean/20476)


# by season-----

merge_wiz %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         # & season!=2021
  ) %>% 
  group_by(season) %>% 
  summarize(mean = mean(attendance, na.rm=T), median = median(attendance, na.rm=T), q25 = quantile(attendance, .25, na.rm=T), q75 = quantile(attendance, .75, na.rm=T)
            , perct = mean/20476
  ) %>% mutate(mean_perct = mean(perct))


# this season vs other seasons up to this point----
merge_wiz %>% 
  filter(home_team_name== "Washington Wizards" 
         & month %in% c("Oct", "Nov", "Dec", "Jan", "Feb")
         # & attendance!=0
         # & season!=2021
  ) %>% 
  mutate(this_season = ifelse(season == "2022", "This Season", "Other Seasons")) %>% 
  group_by(this_season) %>% 
  summarize(mean = mean(attendance, na.rm=T), median = median(attendance, na.rm=T), q25 = quantile(attendance, .25, na.rm=T), q75 = quantile(attendance, .75, na.rm=T)
            , perct = mean/20476
  ) 

merge_wiz4 %>% 
  filter(home_team_name== "Washington Wizards" 
         & month %in% c("Oct", "Nov", "Dec", "Jan", "Feb")
         # & attendance!=0
         # & season!=2021
  ) %>% 
  group_by(season) %>% 
  summarize(mean = mean(attendance, na.rm=T), median = median(attendance, na.rm=T), q25 = quantile(attendance, .25, na.rm=T), q75 = quantile(attendance, .75, na.rm=T)
            , perct = mean/20476
  ) 

# for this season only-----
merge_wiz4 %>% filter(season=="2022") %>%
  mutate(perct = attendance/20476) %>% 
  select(date, team1, team2, attendance, perct) %>% View()

merge_wiz4 %>% 
  filter(season=="2022") %>%
  mutate(perct = attendance/20476) %>% 
  select(date, team1, team2, attendance, perct) %>% 
  tail(n=10) %>% 
  summarize(mean=mean(perct))


p1 <- merge_wiz4 %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         & season!=2021
  ) %>% 
  ggplot() +
  geom_line(aes(x = date, y = attendance, group = season), alpha = 0.3) +
  geom_smooth(aes(x = date, y = attendance, group = season), se = F, col = "#002b5c") +
  scale_x_date(date_breaks = "2 month", date_labels =  "%b") +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_grid(~season, scales = "free_x", space = "free_x") +
  theme_minimal() +
  theme(panel.spacing = unit(0, 'lines')
        , text = element_text(size = 20)
        
  ) +
  labs(x = "", y = ""
       , title = "Attendance by Season"
       , subtitle = "The dark lines show a rolling average\n2021 has been removed since there were only seven home games with limited seating"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
  )

ggsave("Attendance 2011-2022.png", p1, width = 14, height = 7, dpi = 300, type = 'cairo')

# attendance percentage
p2 <- merge_wiz4 %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         # & season!=2021
  ) %>% 
  group_by(season) %>% 
  summarize(med = mean(attendance, na.rm=T)
            , perct = med/20476
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(season), y = perct , group = factor(season))) +
  geom_segment( aes(x=factor(season) ,xend=factor(season), y=0, yend=perct), color="grey") +
  geom_point(size=18,  color="#002b5c", shape = 21, stroke = 5, fill = "white") +
  geom_text(aes(label = paste0(round(perct*100, 0), "%"))
            , check_overlap = TRUE
            , fontface = "bold", size = 6, col = "black") +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_minimal() +
  theme( text = element_text(size = 20)) +
  labs(x = "", y = "% of Total Capacity"
       , title = "Average Capacity by Season"
       # , subtitle = "2021 has been removed from the figure since there were only seven home games and limited seating was made available"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
  )

ggsave("Attendance Percentage.png", p2, width = 14, height = 7, dpi = 300, type = 'cairo')


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

p3 <- merge_wiz4 %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         & season!=2021
         & team2!= "NJN"
  ) %>% 
  mutate(team2 = fct_reorder(team2, attendance, mean)
  ) %>% 
  group_by(team2) %>% 
  summarize(mean = mean(attendance, na.rm = TRUE)
            , sd = sd(attendance, na.rm = TRUE)
            , n = n()) %>%
  mutate(se = sd / sqrt(n)
         , lower = mean - qt(1 - (0.05 / 2), n - 1) * se
         , upper = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  ungroup() %>% 
  ggplot() +
  geom_linerange(aes(x = team2, ymin = lower, ymax = upper , group = team2, col = ifelse(lower>=17014, "Above Average", "Below Average")), size = 1) +
  geom_point(aes(x = team2, y = mean, col = ifelse(lower>=17014, "Above Average", "Below Average")), size = 4, shape = 19) +
  geom_hline(yintercept = 16890, linetype = "dotted") +
  annotate("text", x = 24, y = 16699, label = "10 year average", size = 6) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c("#E31837", "#6C6463")) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(x = "", y = "Average Attendance"
       , title = "Average Attendance by Team"
       , subtitle = "Red denotes teams who consistently have above average attendance"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
  )

ggsave("Attendance by Team.png", p3, width = 16, height = 7, dpi = 300, type = 'cairo')




# attendance by day
merge_wiz4 %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         & season!=2021
         & team2!= "NJN"
  ) %>% 
  group_by(day, month) %>% 
  summarize(mean = mean(attendance, na.rm = TRUE)
            , sd = sd(attendance, na.rm = TRUE)
            , n = n()) %>%
  mutate(se = sd / sqrt(n)
         , lower = mean - qt(1 - (0.05 / 2), n - 1) * se
         , upper = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  ungroup() %>% 
  ggplot() +
  geom_linerange(aes(x = day, ymin = lower, ymax = upper , group = day, col = ifelse(lower>=16890, "Above Average", "Below Average")), size = 1) +
  geom_point(aes(x = day, y = mean, col = ifelse(lower>=17014, "Above Average", "Below Average")), size = 4, shape = 19) +
  geom_hline(yintercept = 16890, linetype = "dotted") +
  annotate("text", x = 3, y = 16699, label = "10 year average", size = 6) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c("#E31837", "#6C6463")) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  labs(x = "", y = "Average Attendance"
       , title = "Average Attendance by Day and Month"
       , subtitle = "Red denotes teams who consistently have above average attendance"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
  ) 

p4 <- merge_wiz4 %>% 
  filter(home_team_name== "Washington Wizards" 
         & attendance!=0
         & season!=2021
  ) %>% 
  group_by(day, month) %>% 
  mutate(mean_att = gmodels::ci(attendance, na.rm=T)[2]
         , colors_att = ifelse(mean_att >=16890, "Above", "Below")) %>% 
  ungroup() %>% 
  ggplot(aes(x = day, y = attendance , group = day)) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, col = "#6C6463") +
  scale_y_continuous(labels = scales::comma_format()) +
  # scale_color_manual(values = c("#E31837", "#6C6463")) +
  facet_wrap(~factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")),  scales = "free_x") +
  theme_minimal() +
  theme(panel.spacing = unit(0, 'lines')
        , text = element_text(size = 20)) +
  labs(x = "", y = ""
       , title = "Attendance by Month and Day"
       # , subtitle = "2021 has been removed from the figure since there were only seven home games and limited seating was made available"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
  ) 
ggsave("Attendance by day and month.png", p4, width = 16, height = 10, dpi = 300, type = 'cairo')

# which games/teams had the most attendance
merge_wiz4 %>% 
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

# basic linear model
m1 <- stan_glm(log_att ~ 
                   elo_prob1
                 + quality
                 + lag(log_att)
                 # + PRCP
                 + TMAX
                 + lag(spread)
                 + streak
                 + plusminusTeam
                 + lag(plusminusTeam)
                 + lag(outcomeGame)
                 + fgmTeam
                 + lag(fgmTeam)
                 + lag(fgaTeam)
                 + lag(astTeam)
                 + ptsTeam
                 + lag(ptsTeam)
                 # + (1|team2)
                 +
                 winpct
                 + team2
                 + factor(season)
                 # + (1 | season)
                 # + (day | month)
                 , data = merge_wiz4[merge_wiz4$attendance!=0,])


summary(m1)

posteriors <- insight::get_parameters(m1)

ggplot(posteriors, aes(x = winpct)) +
  geom_density(fill = "#002F6C")


median(bayes_R2(m1))

hist(posterior_predict(m1))



# basic linear model with covariates, not a great model
m2 <- stan_glm(log_att ~ 
                 elo_prob1
               + quality
               + lag(log_att)
               # + PRCP
               + TMAX
               + lag(spread)
               + streak
               + plusminusTeam
               + lag(plusminusTeam)
               + lag(outcomeGame)
               + fgmTeam
               + lag(fgmTeam)
               + lag(fgaTeam)
               + lag(astTeam)
               + ptsTeam
               + lag(ptsTeam)
               + day
               + month
               # + (1|team2)
               + team2 
               +   factor(season)
               # + (1 | season)
               # + (day | month)
               , data = merge_wiz4[merge_wiz4$attendance!=0,])



plot(m2, plotfun = "hist")

pp_check(m2, plotfun = "hist", nreps = 5)


# coefficient plot
sjPlot::plot_model(m2
                   , bpe = "mean"
                   # , bpe.style = "dot"
                   , sort.est = TRUE
                   # , bpe.color  = "black"
                   , line.size = 3
                   , dot.size = 3
                   , value.size = 3)

# random effects


# basic linear model with covariates
m3 <- stan_glmer(log_att ~ 
                 elo_prob1
               + quality
               # + lag(log_att)
               # + PRCP
               + TMAX
               + lag(spread)
               + streak
               + lag(streak)
               + plusminusTeam
               + lag(plusminusTeam)
               + lag(outcomeGame)
               + fgmTeam
               + lag(fgmTeam)
               + lag(fgaTeam)
               + lag(astTeam)
               + ptsTeam
               + lag(ptsTeam)
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
               + .data_Oct
               + .data_Nov
               + .data_Dec
               + winpct
               + (1|team2)
               + (1 | season)
               # + (day | month)
               , data = merge_wiz4[merge_wiz4$attendance!=0,])



plot(m3, regex_pars = c(".data"), plotfun = "hist")

pp_check(m3, plotfun = "hist", nreps = 5)

median(bayes_R2(m3))


posteriors3 <- insight::get_parameters(m3)

ggplot(posteriors3, aes(x = winpct)) +
  geom_density(fill = "#002F6C")



# coefficient plot
sjPlot::plot_model(m3
                   , bpe = "mean"
                   # , bpe.style = "dot"
                   , sort.est = TRUE
                   # , bpe.color  = "black"
                   , line.size = 3
                   , dot.size = 3
                   , value.size = 3)




# individual days, month fixed effects 
m4 <- stan_glmer(log_att ~ 
                   elo_prob1
                 + quality
                 + lag(log_att)
                 + streak
                 + lag(outcomeGame)
                 + TMAX
                 # + lag(spread)
                 # + plusminusTeam
                 + lag(plusminusTeam)
                 # + lag(outcomeGame)
                 # + fgmTeam
                 + lag(fgmTeam)
                 # + lag(fgaTeam)
                 # + lag(astTeam)
                 # + ptsTeam
                 + lag(ptsTeam)
                 + winpct
                 + .data_Mon
                 + .data_Tue
                 + .data_Wed
                 + .data_Thu
                 + .data_Fri
                 + .data_Sat
                 + (1|month)
                 + (1|season)
                 + (1|slugOpponent)
                 , data = merge_wiz4[merge_wiz4$attendance!=0,]
)

summary(m4)

# 
# # check with poisson model
# m2_poisson <- stan_glmer(attendance ~ 
#                            elo_prob1
#                          + quality
#                          + lag(log_att)
#                          # + streak
#                          # + lag(outcomeGame)
#                          # + TMAX
#                          # + lag(spread)
#                          # + plusminusTeam
#                          # + lag(plusminusTeam)
#                          # + lag(outcomeGame)
#                          # + fgmTeam
#                          # + lag(fgmTeam)
#                          # + lag(fgaTeam)
#                          # + lag(astTeam)
#                          # + ptsTeam
#                          # + lag(ptsTeam)
#                          + .data_Mon
#                          + .data_Tue
#                          + .data_Wed
#                          + .data_Thu
#                          + .data_Fri
#                          + .data_Sat
#                          + .data_Jan
#                          + .data_Feb
#                          + .data_Mar
#                          + .data_Apr
#                          + .data_May
#                          + .data_Jun
#                          + .data_Jul
#                          + .data_Aug
#                          + .data_Sep
#                          + .data_Oct
#                          + .data_Nov
#                          + .data_Dec
#                          + (1|season)
#                          + (1|slugOpponent)
#                          , family = poisson
#                            , data = merge_wiz4
# )
# pp_check(m2_poisson, plotfun = "hist", nreps = 5)


# check fit

pp_check(m4, plotfun = "hist", nreps = 5)
# the model is a good fit to the data based on generated data yrep
# from the posterior predictive distribution. 
# It looks a lot like the observed data y. 
# That is, given y, the yrep we generate appears plausible

# check the distribution of a test quantity compared to the value of the quantity in the observed data
pp_check(m4, plotfun = "stat", stat = "mean")

# check R2
hist(bayes_R2(m4))


median(bayes_R2(m4))

# coefficient plot
sjPlot::plot_model(m4
                   , bpe = "mean"
                   # , bpe.style = "dot"
                   , sort.est = TRUE
                   # , bpe.color  = "black"
                   , line.size = 3
                   , dot.size = 3
                   , value.size = 3)


m4.out <- m4 %>% broom::tidy(conf.int = TRUE) %>% 
  mutate(var = c("Intercept"
                 , "538 Win Prob."
                 , "538 'Quality' Rating"
                 , "Previous Game's Attendance"
                 , "Monday"
                 , "Tuesday"
                 , "Wednesday"
                 , "Thursday"
                 , "Friday"
                 , "Saturday"
                 , "Jan"
                 , "Feb"
                 , "Mar"
                 , "Apr"
                 , "Oct"
                 , "Nov"
                 , "Dec"
  )) %>% 
  filter(var!= "Intercept") %>% 
  arrange(desc(estimate))

m4.out %>% 
  mutate(sig = case_when(conf.low<0 & conf.high>0 ~ "Too Noisy"
                         , conf.low<0 & conf.high<0 ~ "Negative"
                         , conf.low>0 & conf.high>0 ~ "Positive")) %>% 
  ggplot()+
  geom_linerange(aes(xmin = conf.low
                     , xmax = conf.high
                     , y = reorder(var, estimate), col = sig), size = 2) +
  geom_label(aes(x = estimate, y = reorder(var, estimate)
                 , label = paste0(round(estimate, 2)*100, "%"), col = sig)) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C", "#6C6463"))


# vary intercepts vary slopes
m5 <- stan_glmer(log_att ~
                   elo_prob1
                 + quality
                 # + lag(log_att)
                 + streak
                 + lag(streak)
                 + TMAX
                 + lag(spread)
                 + plusminusTeam
                 + lag(plusminusTeam)
                 + win 
                 + lag(win)
                 + fgmTeam
                 + lag(fgmTeam)
                 + fgaTeam
                 + lag(fgaTeam)
                 + astTeam
                 + lag(astTeam)
                 + lag(ptsTeam)
                 + ptsTeam
                 + winpct
                 + (1|day)
                 + (1|month)
                 + (1|season)
                 + (1|slugOpponent)
                 , data = merge_wiz4[merge_wiz4$attendance!=0,]
)


sjPlot::plot_model(m5
                   , bpe = "mean"
                   # , bpe.style = "dot"
                   , sort.est = TRUE
                   # , bpe.color  = "black"
                   , line.size = 3
                   , dot.size = 3
                   , value.size = 3)


p5 <- plot(m5, regex_pars = "win", plotfun = "hist") + 
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(x = "% Change in average attendance", y = ""
       , title = "Predicted attendance for Saturdays"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
       
  )

ggsave("Predicted attendance by win percentage.png", p5, width = 16, height = 7, dpi = 300, type = 'cairo')



m5.out <- m5 %>% broom.mixed::tidy(conf.int = TRUE) %>% 
  mutate(var = c("Intercept"
                 , "538 Win Prob."
                 , "538 'Quality' Rating"
                 , "Win/Loss Streak"
                 , "Win/Loss Streak (lagged)"
                 , "Previous game outcome (win)"
                 , "Local temperature"
                 , "Previous home game's point spread"
                 , "Plus/Minus"
                 , "FG made"
                 , "Previous home game's FG made"
                 , "Previous home game's FG attempts"
                 , "Previous home game's assists"
                 , "Wiz. Points"
                 , "Win %"
                 , "Wiz. Points (previous home game)"
                 
  )) %>% 
  filter(var!= "Intercept") %>% 
  arrange(desc(estimate))

m5.out %>% 
  mutate(sig = case_when(conf.low<0 & conf.high>0 ~ "Too Noisy"
                         , conf.low<0 & conf.high<0 ~ "Negative"
                         , conf.low>0 & conf.high>0 ~ "Positive")) %>% 
  ggplot()+
  geom_linerange(aes(xmin = conf.low
                     , xmax = conf.high
                     , y = reorder(var, estimate), col = sig), size = 2) +
  geom_label(aes(x = estimate, y = reorder(var, estimate)
                 , label = paste0(round(estimate, 2)*100, "%"), col = sig), size =3) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C", "#6C6463")) + 
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20))  +
  labs(x = "% Change in average attendance", y = ""
       , title = "Predicted attendance after accounting for game, season, and date variation"
       , caption = "wizardspoints.substack.com\ndata: basketball-reference.com"
  ) 


# Gamma distribution
m6 <- stan_glmer(log_att ~ 
                   elo_prob1
                 + quality
                 + lag(log_att)
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
                 , family = "Gamma"
)

summary(m6)

prior_summary(m6)

pp_check(m6, plotfun = "hist", nreps = 5)

# cluster day, month, and team by season
m7 <- stan_glmer(log_att ~ 
                     elo_prob1
                   + quality
                   # + lag(log_att)
                   + streak
                   + lag(outcomeGame)
                   + TMAX
                   + lag(spread)
                   + plusminusTeam
                   + lag(plusminusTeam)
                   + lag(outcomeGame)
                   + fgmTeam
                   + lag(fgmTeam)
                   + lag(fgaTeam)
                   + lag(astTeam)
                   + ptsTeam
                   + lag(ptsTeam)
                 # + (1|day)
                 # + (1|month)
                 + (day + month + slugOpponent|season)
                 # + (1|slugOpponent)
                 , data = merge_wiz4[merge_wiz4$attendance!=0,]
  )

pp_check(m7, plotfun = "hist", nreps = 5)


sjPlot::plot_model(m7
                   , bpe = "mean"
                   # , bpe.style = "dot"
                   , sort.est = TRUE
                   # , bpe.color  = "black"
                   , line.size = 3
                   , dot.size = 3
                   , value.size = 3)

shinystan::launch_shinystan(m7)
