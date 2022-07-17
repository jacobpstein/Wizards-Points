###############################################
# 1Change over time
# Session Info:
# R version 4.2.1 (2022-06-23)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Monterey 12.4
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ggrepel)
library(jsonlite)
library(httr)
library(vroom)
library(rvest)

# set seed
set.seed(20483789)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# shot selection over the season

# set up 
headers = c(
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

# 2021-2022 numbers
id <- 1:7

shots <- NULL

# scrape data from NBA.com/stats
for(i in 1:length(id)){
  url <- paste0("https://stats.nba.com/stats/teamdashboardbyshootingsplits?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month="
                , id[i], "&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=1610612764&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[4])  #%>% 
    # full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  shots[[i]] <- tmp_dat
  
}

# combine scraped data into a data frame
shot_df <- bind_rows(shots, .id = "column_label") 

# clean up headers
names_shots <- data.frame(headers = json_res2p$resultSets$headers[4])

names_shots2 <- names_shots$c..GROUP_SET....GROUP_VALUE....FGM....FGA....FG_PCT....FG3M... 

names(shot_df)[2:33] <- names_shots2

shot_df2 <- shot_df %>% 
              select("num_month" = column_label 
                     , "shot_area" = GROUP_VALUE 
                     , FGM
                     , FGA
                     , FG_PCT
                     , EFG_PCT
                     ) %>% 
  mutate(month = case_when(num_month == "1" ~ "Oct"
                           , num_month == "2" ~ "Nov"
                           , num_month == "3" ~ "Dec"
                           , num_month == "4" ~ "Jan"
                           , num_month == "5" ~ "Feb"
                           , num_month == "6" ~ "Mar"
                           , num_month == "7" ~ "Apr"
                           )
        , month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))
        , FGA = as.numeric(FGA) 
        , FGM = as.numeric(FGM) 
        , FG_PCT = as.numeric(FG_PCT)
        , EFG_PCT = as.numeric(EFG_PCT)
        ) 

glimpse(shot_df2)


p1 <- shot_df2 %>% 
  filter(shot_area!= "Backcourt") %>% 
  ggplot(aes(x=month, y=FGA, fill=shot_area
             , group = shot_area)) + 
  geom_area(alpha=0.6 , size=1, colour="black") + 
  facet_wrap(~shot_area) +
  viridis::scale_fill_viridis(discrete = T, option = "D")  +
  theme_minimal() +
  theme(legend.position = "NA"
        , legend.title = element_blank()
        # , panel.grid.major.y = element_blank()
        , text = element_text(size = 20)) +
  labs(title = "Wizards Field Goal Attempts 2021-22"
       , caption = "wizardspoints.substack.com\ndata: nba.com/stats"
       , y = "Total Field Goal Attempts"
      , x = ""
  )

ggsave("FGA.png", p1, width = 16, height = 7, dpi = 300, type = 'cairo')


# who was driving midrange in March?----
url_march <- "https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=173&CFPARAMS=Mid-Range&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=6&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID=0&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=1610612764&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID="
res <- GET(url = url_march, add_headers(.headers=headers))

json_res2p <- fromJSON(content(res, "text"))

tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1])  %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[2]))

shots[[i]] <- tmp_dat

mid_range1 <- tmp_dat %>% 
  group_by(X5) %>% 
  count() %>% 
  mutate(month = "March") %>% 
  arrange(desc(n))


mid_range1_pct <- tmp_dat %>% 
  filter(X5 %in% c("Kentavious Caldwell-Pope", "Ish Smith", "Kristaps Porzingis", "Rui Hachimura", "Kyle Kuzma"
                   # , "Tomas Satoransky", "Deni Avdija"
                   )) %>% 
  group_by(X5, X11) %>% 
  count() %>% 
  mutate(month = "March") %>% 
  # arrange(desc(n)) %>% 
  ungroup() %>% 
  group_by(X5) %>% 
  mutate(FG_Pct = n/sum(n)
            ) %>% 
  filter(X11 == "Made Shot")


# who was driving midrange in Feb?
url_march <- "https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=173&CFPARAMS=Mid-Range&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=5&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID=0&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=1610612764&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID="
res <- GET(url = url_march, add_headers(.headers=headers))

json_res2p <- fromJSON(content(res, "text"))

tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1])  %>% 
  full_join(data.frame(json_res2p$resultSets$rowSet[2]))

shots[[i]] <- tmp_dat

mid_range2 <- tmp_dat %>% 
  group_by(X5) %>% 
  count() %>% 
  mutate(month = "February") %>% 
  arrange(desc(n))


mid_range2_pct <- tmp_dat %>% 
  filter(X5 %in% c("Kentavious Caldwell-Pope"
                   , "Ish Smith"
                   , "Thomas Bryant", "Rui Hachimura", "Kyle Kuzma", "Montrezl Harrell")) %>% 
  group_by(X5, X11) %>% 
  count() %>% 
  mutate(month = "February") %>% 
  # arrange(desc(n)) %>% 
  ungroup() %>% 
  group_by(X5) %>% 
  mutate(FG_Pct = n/sum(n)
  ) %>% 
  filter(X11 == "Made Shot")


mid_range <- mid_range2 %>% bind_rows(mid_range1) %>% filter(n>=10 & X5 != "Deni Avdija" & X5 != "Tomas Satoransky") %>% 
  mutate(X5 = ifelse(X5 == "Kentavious Caldwell-Pope", "KCP", X5)) 
  

mid_range_pct <- mid_range2_pct %>% bind_rows(mid_range1_pct) %>%   mutate(X5 = ifelse(X5 == "Kentavious Caldwell-Pope", "KCP", X5)) %>% 



slop_theme <- list(  
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=20)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=20, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)     

p2 <- mid_range %>% 
  ggplot(aes(x = month, y = n, group = X5)) +
  geom_line(aes(color = X5, alpha = 1), size = 2) +
  #  geom_point(aes(color = Type, alpha = .1), size = 4) +
  geom_text_repel(data = mid_range %>% filter(month == "February"), 
                  aes(label = X5) , 
                  hjust = "left", 
                  fontface = "bold", 
                  size = 6, 
                  nudge_x = -.3,
                  min.segment.length = 3,
                  direction = "y") +
  geom_text_repel(data = mid_range %>% filter(month == "March"), 
                  aes(label = X5) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 6, 
                  nudge_x = .35,
                  min.segment.length = 4,
                  direction = "both") +
  geom_label(aes(label = n), 
             size = 7,  
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +
  slop_theme +
  viridis::scale_color_viridis(discrete = T, option = "D") +
  theme(legend.position = "NA"
        # , panel.grid.major.y = element_blank()
        , text = element_text(size = 20)) +
  labs(title = "Top 5 Midrange Field Goal Attempts from February to March 2022"
       , caption = "wizardspoints.substack.com\ndata: nba.com/stats"
       )

ggsave("Top 5 FGA.png", p2, width = 14, height = 12, dpi = 300, type = 'cairo')

p3 <- mid_range_pct %>% 
  ggplot(aes(x = month, y = FG_Pct, group = X5)) +
  geom_line(aes(color = X5, alpha = 1), size = 2) +
  #  geom_point(aes(color = Type, alpha = .1), size = 4) +
  geom_text_repel(data = mid_range_pct %>% filter(month == "February"), 
                  aes(label = X5) , 
                  hjust = "left", 
                  fontface = "bold", 
                  size = 6, 
                  nudge_x = -.4, 
                  direction = "y") +
  geom_text_repel(data = mid_range_pct %>% filter(month == "March"), 
                  aes(label = X5) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 6, 
                  nudge_x = .5, 
                  direction = "y") +
  geom_label(aes(label = paste0(round(FG_Pct, 2)*100, "%")), 
             size = 7, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +
  slop_theme +
  viridis::scale_color_viridis(discrete = T, option = "D") +
  theme(legend.position = "NA"
        # , panel.grid.major.y = element_blank()
        , text = element_text(size = 20)) +
  labs(title = "Midrange Field Goal % from February to March 2022"
       , subtitle = "Among Top 5 Field Goal Attempt Players"
       , caption = "wizardspoints.substack.com\ndata: nba.com/stats"
  )

ggsave("Top 5 FG PCT.png", p3, width = 14, height = 16, dpi = 300, type = 'cairo')


p2_p3 <- cowplot::plot_grid(plotlist = list(p2, p3)
                             , nrow = 1
                             , ncol = 2
                             , label_size = 20
                             , label_fontface = "plain"
                             , label_fontfamily = "Corbel")


ggsave("FGA and PF PCT.png", p2_p3, width = 26, height = 14, dpi = 300, type = 'cairo')


# 2020-2021 numbers

id2 <- 3:8

shots2 <- NULL

# scrape data from NBA.com/stats
for(i in 1:length(id2)){
  url <- paste0("https://stats.nba.com/stats/teamdashboardbyshootingsplits?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month="
                , id2[i], "&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=1610612764&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[4])  #%>% 
  # full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  shots2[[i]] <- tmp_dat
  
}

# combine scraped data into a data frame
shot_df20 <- bind_rows(shots2, .id = "column_label") 

# clean up headers
names_shots20 <- data.frame(headers = json_res2p$resultSets$headers[4])

names_shots2 <- names_shots20$c..GROUP_SET....GROUP_VALUE....FGM....FGA....FG_PCT....FG3M...

names(shot_df20)[2:33] <- names_shots2

shot_df202 <- shot_df20 %>% 
  select("num_month" = column_label 
         , "shot_area" = GROUP_VALUE 
         , FGM
         , FGA
         , FG_PCT
         , EFG_PCT
  ) %>% 
  mutate(month = case_when(num_month == "1" ~ "December"
                           , num_month == "2" ~ "January"
                           , num_month == "3" ~ "February"
                           , num_month == "4" ~ "March"
                           , num_month == "5" ~ "April"
                           , num_month == "6" ~ "May"  )
  , month = factor(month, levels = c("December", "January", "February", "March", "April", "May"))
  , FGA = as.numeric(FGA) 
  , FGM = as.numeric(FGM) 
  , FG_PCT = as.numeric(FG_PCT)
  , EFG_PCT = as.numeric(EFG_PCT)
  ) 

glimpse(shot_df2)


shot_df202 %>% 
  ggplot(aes(x=month, y=FGA, fill=shot_area
             , group = shot_area)) + 
  geom_area(alpha=0.6 , size=1, colour="black") + 
  facet_wrap(~shot_area)



shot_df2 %>% 
  group_by(month, shot_area) %>% 
  summarise(n = sum(FGA)) %>% 
  mutate(percentage = n/sum(n)) %>% 
ggplot(aes(x=month, y=percentage, fill=shot_area
                     , group = shot_area)) + 
  geom_area(alpha=0.6 , size=1, colour="black")


shot_df2 %>% 
  group_by(month, shot_area) %>% 
  summarise(n = sum(FGM)) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x=month, y=percentage, fill=shot_area
             , group = shot_area)) + 
  geom_area(alpha=0.6 , size=1, colour="black")





shot_df2 %>% 
  ggplot(aes(x=FGA, y=FG_PCT, col=month
             )) + 
  geom_point(alpha=0.6 , size=1) + 
  facet_wrap(~month)






# get team stats
nba_logs <- bref_players_stats(seasons = c(2019:2022), only_totals = TRUE, tables = c("advanced", "totals", "per_minute", "per_pos"))

# calculate season averages
season_averages <- dataBREFPlayerAdvanced %>% 
                      group_by(slugSeason, yearSeason) %>% 
                      summarize(pctTrueShooting = mean(pctTrueShooting, na.rm=T)) %>% 
  mutate(namePlayer = "NBA Average"
         , slugTeamBREF = "NBA Average")

# let's just look at the guys who will be on the upcoming team
wiz_kids <- dataBREFPlayerAdvanced %>% 
              select(slugSeason, yearSeason, namePlayer, pctTrueShooting, slugTeamBREF) %>% 
              filter(namePlayer %in% c("Rui Hachimura"
                                       , "Daniel Gafford"
                                       , "Kristaps Porzingis"
                                       , "Vernon Carey Jr."
                                       , "Deni Avdija"
                                       , "Monte Morris"	
                                       , "Kyle Kuzma"
                                       , "Corey Kispert"
                                       , "Will Barton"
                                       , "Isaiah Todd"
                                       , "Anthony Gill"
                                       , "Bradely Beal"
                                       , "Delon Wright"
                                       )) %>% 
  bind_rows(season_averages)
  
    

wiz_kids %>% 
  filter(slugSeason %in% c("2020-21", "2021-22")) %>% 
ggplot(aes(x = slugSeason, y = pctTrueShooting, group = namePlayer)) +
  geom_line(aes(color = namePlayer, alpha = 1), size = 1) +
  #  geom_point(aes(color = Type, alpha = .1), size = 4) +
  geom_text_repel(data = wiz_kids %>% filter(slugSeason == "2020-21"), 
                  aes(label = namePlayer) , 
                  hjust = "left", 
                  fontface = "bold", 
                  size = 3, 
                  nudge_x = -.45, 
                  direction = "y") +
  geom_text_repel(data = wiz_kids %>% filter(slugSeason == "2021-22"), 
                  aes(label = namePlayer) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 3, 
                  nudge_x = .5, 
                  direction = "y") +
  geom_label(aes(label = pctTrueShooting), 
             size = 2.5, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +
  slop_theme +
  labs(
    title = "Estimates of Percent Survival Rates",
    subtitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
    caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk"
  )
