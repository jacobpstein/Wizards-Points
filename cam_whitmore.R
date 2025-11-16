#####################
# This script visualizes Cam Whitmore's 
# shot quality from the 24-25 season
# Data are taken from pbpstats here: 
# https://www.pbpstats.com/game-logs/nba/player?Season=2024-25&SeasonType=Regular+Season&EntityId=1641715&EntityType=Player&Table=Shooting&StatType=Totals
# and here:
# https://www.pbpstats.com/game-logs/nba/player?Season=2024-25&SeasonType=Regular%2BSeason&EntityId=1641715
#
# Session Info:
# R version 4.4.2 (2024-10-31)
# Platform: aarch64-apple-darwin20
# # Running under: macOS Sequoia 15.6.1
####################

# load libraries
library(tidyverse)
library(nbastatR)
library(janitor)
library(hms)
library(ggbump)

# data downloaded, but you could read them in from the above link
cam_df <- read_csv("cam_df.csv"
                   , col_types = cols(Date = col_date(format = "%m/%d/%y")
                                      , MP = col_time(format = "%H:%M"))) |> 
  clean_names() |> 
  mutate(mp_num = as.numeric(mp))

cam_df2 <- read_csv('cam.csv') |> clean_names()

col_sds <- sapply(cam_df2[ , sapply(cam_df2, is.numeric)]
                  , sd
                  , na.rm = TRUE
)

sort(col_sds)
names(which.min(col_sds))

# cam_df is our overall counting stats data frame, cam_df2 has the shot quality data
cam_merge <- cam_df |> left_join(cam_df2 |> select(date, shot_quality_avg)) 

# viz it
p <- cam_merge |> 
  filter(fga>0) |> 
  ggplot(aes(x = date, y = shot_quality_avg)) +
  geom_hline(aes(yintercept = mean(shot_quality_avg, na.rm=T)), linetype = "dashed") + 
  geom_bump(lwd = 1.2, color = "#3B9AB2") +
  geom_text(data = data.frame(x = as.Date("2024-11-17"),
                              y = 0.544364821157319, label = "Overall Average = 0.55"),
            mapping = aes(x = x, y = y, label = label),
            size = 5.64, family = "Gill Sans", inherit.aes = FALSE) +
  usaidplot::usaid_plot() +
  labs(x = "", y= "Average Shot Quality", title = "Cam Whitmore had pretty reliable shot quality last season"
       , subtitle = "Could the quality have been better? Sure, but overall it was one of his most consistent stats"
       , caption =  "Note: This figure only shows shot quality for games where he attempted at least one shot\nData: pbpstats.com\nwizardspoints.substack.com")

ggsave("cam_hitmore_shot_quality.png", p, height = 9, width = 16, dpi = 600)