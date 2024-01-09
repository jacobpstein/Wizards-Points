##############################################################################
# This file identifies metrics that are most likely to improve for key players
# # it uses data pulled from NBA.com via the nbastatR package
# R version 4.3.2 (2023-10-31)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sonoma 14.2.1
##############################################################################


# load libraries
library(tidyverse)
library(nbastatR)
library(cluster)
library(randomForest) 
library(caret)
library(factoextra)
library(viridis)
library(gganimate)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# get boxscore stats
df1 <- game_logs(seasons = c(2019:2024), result_types = c("player"), season_types = c("Regular Season")) |> 
  dplyr::select(namePlayer
                , yearSeason
                , pctFG
                , pctFG3
                , pctFT
                # , minutes
                , oreb
                , dreb
                , ast
                , stl
                , blk
                , tov
                , pf
                , pts) 

# collapse to annual data for clustering to reduce our units
df2 <- df1 |> 
  group_by(yearSeason, namePlayer) |> 
  summarize_all(.funs = mean, na.rm=T) |> 
  # column_to_rownames(var = "namePlayer") |> 
  mutate(pctFG = ifelse(is.na(pctFG)==T, 0, pctFG)
         , pctFT = ifelse(is.na(pctFT)==T, 0, pctFT)
  )
  
# just a bit of cleaning
df2$player_name <- df2$namePlayer

# we're going to loop through and create a bunch of plots, then extract the clusters
# from those plots and use that info to calculate season-to-season changes for each player

# create a list to store ggplot objects
plots <- list()
  
# Loop through unique seasons
for (i in 1:length(unique(df2$yearSeason))) {
    # Extract data for the current yearSeason
    current_yearSeason <- unique(df2$yearSeason)[i]
    current_yearSeason_data <- df2[df2$yearSeason == current_yearSeason, ]
    
    current_yearSeason_data <- current_yearSeason_data |> column_to_rownames("namePlayer")
    
    # unsupervised random forest model for the current season
    rf_current <- randomForest(x = current_yearSeason_data[,-c(1)], mtry = 2, ntree = 2000, proximity = TRUE)
    
    # get the proximity matrix
    prox_current <- rf_current$proximity
    
    # cluster it!
    pam_rf_current <- pam(prox_current, 5)
    
    # Create clustering plot for the current season
    plot_current_season <- fviz_cluster(pam_rf_current) +
      viridis::scale_color_viridis(discrete = TRUE) +
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme_minimal() +
      ggtitle(paste("Clustering for the ", current_yearSeason, "season"))
    
    # Add the plot to the list
    plots[[i]] <- plot_current_season
}

# extract the data from the plot data frames
all_clusters <- plots[[1]]$data |> select(-name) |> rownames_to_column("name") |> mutate(season = 2022) |> 
  bind_rows(plots[[2]]$data |> select(-name) |> rownames_to_column("name") |> mutate(season = 2023)) |> 
  bind_rows(plots[[3]]$data |> select(-name) |> rownames_to_column("name") |> mutate(season = 2024)) 


# create a new column that shows just the initial cluster for each player in their period 1
all_clusters2 <- all_clusters |> left_join(
(all_clusters |> 
  group_by(name) |> 
  slice(which.min(season)) |> 
  select(name, "initial_cluster" = cluster))
 , by = "name"
) 

# create an animated graph
g <- all_clusters2 |> 
  ggplot() +
  geom_text(aes(x = x, y = y, color = initial_cluster, label = name)) +
  viridis::scale_color_viridis(discrete = TRUE) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(legend.position = "NA") +

# animate
animated_g <- g + 
  transition_time(as.integer(season))+
  labs(title='Clustering Players for the {frame_time} season'
       , subtitle = "Players shaded based on initial cluster")

# initialize the animation
anim <- animate(animated_g, nframes=55)            

# and save it
magick::image_write(anim, path="player_clusters.gif")

# bring in our clusters to our data frame-------------------


