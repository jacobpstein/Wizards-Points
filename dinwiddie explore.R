###############################################
# Spencer Dinwiddie and passing
# Session Info:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
###############################################

# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(rvest)
library(janitor)
library(hablar)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(ggraph)
library(tidygraph)
library(ggnet)
library(ggnetwork)
library(sna)
library(igraph)
library(jsonlite)
library(httr)
library(vroom)
library(extrafont)
library(lubridate)
library(ggfx)
library(ggridges)
library(ggrepel)

# set seed
set.seed(20483789)

# for downloads
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# test <- players_tables(players = c("Spencer Dinwiddie"))

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

# url <- "https://stats.nba.com/stats/playerdashptpass?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerID=203915&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision="
# res <- GET(url = url, add_headers(.headers=headers))
# 
# json_resp <- fromJSON(content(res, "text"))
# 
# passes <- data.frame(json_resp$resultSets$rowSet)
# 
# names_passes <- data.frame(pass_made = json_resp$resultSets$headers[1], pass_received = json_resp$resultSets$headers[2])
# 
# names_passes2 <- c(names_passes$c..PLAYER_ID....PLAYER_NAME_LAST_FIRST....TEAM_NAME....TEAM_ID..., names_passes$c..PLAYER_ID....PLAYER_NAME_LAST_FIRST....TEAM_NAME....TEAM_ID....1)
# 
# names(passes) <- names_passes2
# 
# pass_to <- passes %>%
#   select("PASS_FROM" =2 , PASS_TO, "pass_n"=11) 
# 
# pass_from <- passes %>% 
#   select(PASS_FROM, "PASS_TO" = 23, "pass_n" = 32)
# 
# spencer_passes <- pass_to %>% bind_rows(pass_from)
# 
# edges <- spencer_passes
# 
# ids <- passes %>% select(name = 8, "id" = 9) %>% bind_rows(tibble(name = "Dinwiddie, Spencer", id = "203915"))

# 
# nodes <- ids
# 
# # library(igraph)
# 
# g <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
# g
# 
# 
# routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# get player ids

shots <- teams_shots(teams = "Washington Wizards", season_types = "Regular Season", seasons = 2022)

ids <- shots %>% select(namePlayer, idPlayer) %>% unique()

# scaleb
id <- ids$idPlayer

player_passes <- NULL

for(i in 1:length(id)){
  
  url <- paste0("https://stats.nba.com/stats/playerdashptpass?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerID="
                , id[i], "&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[1]) %>% 
    full_join(data.frame(json_res2p$resultSets$rowSet[2]))
  
  player_passes[[i]] <- tmp_dat
  
}

pass_df <- bind_rows(player_passes, .id = "column_label")

# need to get column names

names_passes <- data.frame(headers = json_res2p$resultSets$headers[1])

names_passes2 <- c(names_passes$c..PLAYER_ID....PLAYER_NAME_LAST_FIRST....TEAM_NAME....TEAM_ID..., names_passes$c..PLAYER_ID....PLAYER_NAME_LAST_FIRST....TEAM_NAME....TEAM_ID....1)


names(pass_df)[2:22] <- names_passes2

# add in first name first names
pass_df2 <- pass_df %>% mutate(PLAYER_ID = as.numeric(PLAYER_ID)
                               , PASS_TEAMMATE_PLAYER_ID = as.numeric(PASS_TEAMMATE_PLAYER_ID)) %>% 
  left_join(ids, by = c("PLAYER_ID" = "idPlayer")) %>% 
  left_join(ids, by = c("PASS_TEAMMATE_PLAYER_ID" = "idPlayer")) %>% 
  rename(From = namePlayer.x 
         , To = namePlayer.y)

# pass_df2 <- pass_df %>% filter(PASS>1)

all_passes <- pass_df2 %>% 
  select(From, To, PASS_TYPE, PASS) %>%
  mutate(PASS = as.numeric(PASS))

pass_to <- all_passes %>% filter(PASS_TYPE == "made")

pass_from <- all_passes %>% filter(PASS_TYPE == "received")


edges <- pass_to %>% mutate(To = ifelse(is.na(To)==T, "Bad Pass", To))

nodes <- ids %>% left_join(select(pass_df2, PLAYER_ID, PASS), by = c("idPlayer" = "PLAYER_ID"))

g <- graph.data.frame(edges, directed = T)
plot(g)

V(g)$color <- ifelse(get.vertex.attribute(g)$name == "Spencer Dinwiddie", "#BA0C2F", "light grey")

names_order <- get.vertex.attribute(g)$name
values_din <- edges %>% filter(From == "Spencer Dinwiddie") %>% select("name" = To, PASS) 

values_din$name <- factor(values_din$name, levels = names_order)

E(g)$color <- ifelse(get.edgelist(g)[,1] == "Spencer Dinwiddie"| get.edgelist(g)[,2] == "Spencer Dinwiddie" , "#BA0C2F", "light grey")

passes_vector <- c(values_din$PASS)
names_vector <- c(values_din$name)
node.size <- setNames(c(passes_vector),c(names_vector))


plot(g, layout = layout_with_graphopt, edge.arrow.size = 0.2)
# 
# library(tidygraph)
# 
# graph <- as_tbl_graph(
#   data.frame(
#     from = pass_to$From,
#     to = pass_to$To,
#     weight = as.numeric(pass_to$PASS)
#   )
# )


ggraph(graph, layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point() + 
  theme(legend.position = 'bottom') 

as_tbl_graph(g) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link2(aes(edge_width = PASS, edge_color = color), alpha = 0.5) +
  geom_node_point(aes(fill = color), color = "black",  shape = 21, size = c(5.88, 6.62, 3, 11.96, 5.81, 3.23, 1.04, 7.88, 3.62, 2.04, 1.69, 1, 1, 1, 1))  + 
  geom_node_label(aes(label = name), repel = TRUE) +
  theme_graph() + 
  scale_color_manual(values = c("#BA0C2F", "dark grey")) +
  scale_fill_manual(values = c("#BA0C2F", "dark grey")) +
  scale_edge_color_manual(values = c("#BA0C2F", "dark grey"))+
  theme(legend.position = 'NA') 


p <- as_tbl_graph(g) %>% 
  ggraph(layout = 'graphopt') + 
  geom_edge_link2(aes(edge_width = PASS, edge_color = color), alpha = 0.5) +
  geom_node_point(aes(fill = color), color = "black",  shape = 21, size = c(5.88, 6.62, 3, 11.96, 5.81, 3.23, 1.04, 7.88, 3.62, 2.04, 1.69, 1, 1, 1, 1))  + 
  geom_node_label(aes(label = name), repel = TRUE) +
  theme_graph() + 
  scale_color_manual(values = c("#BA0C2F", "dark grey")) +
  scale_fill_manual(values = c("#BA0C2F", "dark grey")) +
  scale_edge_color_manual(values = c("#BA0C2F", "dark grey"))+
  theme(legend.position = 'NA') 

ggsave("Dinwiddie Passing.png", p, width = 12, height = 8, dpi = 300, type = 'cairo')



# usage
res <- GET(url = "https://stats.nba.com/stats/playerVsPlayer?DateFrom=&DateTo=&GameSegment=&LastNGames=0&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=203915&PlayerID1=0&PlayerID2=0&PlayerID3=0&PlayerID4=0&PlayerID5=0&PlayerTeamID=%7B%22id%22:%221610612764%22,%22abbr%22:%22WAS%22,%22code%22:%22wizards%22,%22city%22:%22Washington%22,%22name%22:%22Wizards%22,%22conference%22:%22Eastern%22,%22division%22:%22Southeast%22,%22special%22:false,%22Season%22:%222021-22%22,%22seasonyear%22:2021,%22SeasonType%22:%22Regular+Season%22,%22color%22:%22%23002b5c%22,%22colors%22:%5B%22%23002b5c%22,%22%23e31837%22%5D%7D&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=%7B%22id%22:%221610612764%22,%22abbr%22:%22WAS%22,%22code%22:%22wizards%22,%22city%22:%22Washington%22,%22name%22:%22Wizards%22,%22conference%22:%22Eastern%22,%22division%22:%22Southeast%22,%22special%22:false,%22Season%22:%222021-22%22,%22seasonyear%22:2021,%22SeasonType%22:%22Regular+Season%22,%22color%22:%22%23002b5c%22,%22colors%22:%5B%22%23002b5c%22,%22%23e31837%22%5D%7D&VsConference=&VsDivision=&VsPlayerID=203078&VsPlayerID1=0&VsPlayerID2=0&VsPlayerID3=0&VsPlayerID4=0&VsPlayerID5=0&VsTeamID=%7B%22id%22:%221610612764%22,%22abbr%22:%22WAS%22,%22code%22:%22wizards%22,%22city%22:%22Washington%22,%22name%22:%22Wizards%22,%22conference%22:%22Eastern%22,%22division%22:%22Southeast%22,%22special%22:false,%22Season%22:%222021-22%22,%22seasonyear%22:2021,%22SeasonType%22:%22Regular+Season%22,%22color%22:%22%23002b5c%22,%22colors%22:%5B%22%23002b5c%22,%22%23e31837%22%5D%7D", add_headers(.headers=headers))

json_res2p <- fromJSON(content(res, "text"))

headings <- data.frame(json_res2p$resultSets$headers[2])

test <- data.frame(json_res2p$resultSets$rowSet[2])  

names(test) <- headings$c..GROUP_SET....PLAYER_ID....PLAYER_NAME....VS_PLAYER_ID....VS_PLAYER_NAME...



id <- ids$idPlayer

on_off <- NULL

for(i in 1:length(id)){
  
  url <- paste0("https://stats.nba.com/stats/playerVsPlayer?DateFrom=&DateTo=&GameSegment=&LastNGames=0&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=", id[i], "&PlayerID1=0&PlayerID2=0&PlayerID3=0&PlayerID4=0&PlayerID5=0&PlayerTeamID=%7B%22id%22:%221610612764%22,%22abbr%22:%22WAS%22,%22code%22:%22wizards%22,%22city%22:%22Washington%22,%22name%22:%22Wizards%22,%22conference%22:%22Eastern%22,%22division%22:%22Southeast%22,%22special%22:false,%22Season%22:%222021-22%22,%22seasonyear%22:2021,%22SeasonType%22:%22Regular+Season%22,%22color%22:%22%23002b5c%22,%22colors%22:%5B%22%23002b5c%22,%22%23e31837%22%5D%7D&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=%7B%22id%22:%221610612764%22,%22abbr%22:%22WAS%22,%22code%22:%22wizards%22,%22city%22:%22Washington%22,%22name%22:%22Wizards%22,%22conference%22:%22Eastern%22,%22division%22:%22Southeast%22,%22special%22:false,%22Season%22:%222021-22%22,%22seasonyear%22:2021,%22SeasonType%22:%22Regular+Season%22,%22color%22:%22%23002b5c%22,%22colors%22:%5B%22%23002b5c%22,%22%23e31837%22%5D%7D&VsConference=&VsDivision=&VsPlayerID="
                , "203915", "&VsPlayerID1=0&VsPlayerID2=0&VsPlayerID3=0&VsPlayerID4=0&VsPlayerID5=0&VsTeamID=%7B%22id%22:%221610612764%22,%22abbr%22:%22WAS%22,%22code%22:%22wizards%22,%22city%22:%22Washington%22,%22name%22:%22Wizards%22,%22conference%22:%22Eastern%22,%22division%22:%22Southeast%22,%22special%22:false,%22Season%22:%222021-22%22,%22seasonyear%22:2021,%22SeasonType%22:%22Regular+Season%22,%22color%22:%22%23002b5c%22,%22colors%22:%5B%22%23002b5c%22,%22%23e31837%22%5D%7D")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res2p <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res2p$resultSets$rowSet[2])
  
  on_off[[i]] <- tmp_dat
  
}

on_off_df <- bind_rows(on_off, .id = "column_label") %>% 
  select("Name" = X3, "teammate" = X5, "Status" = X6, "GP" = X7, "Min" = X11, "NetRtg" = X20, "USG" = X31)

p1 <- on_off_df %>% 
  select(Name, Status, USG, Min) %>% 
  mutate(Status = ifelse(Status == "On", "Dinwiddie On", "Dinwiddie Off")
         , Min = as.numeric(Min)
         , USG = as.numeric(USG)
         , Name = factor(Name)
         , Name = fct_reorder(Name, Min, .desc = F)
        ) %>% 
  filter(Name != "Spencer Dinwiddie") %>% 
  arrange(USG) %>% 
  ggplot(aes(x = USG, y = Name)) +
  geom_line(aes(group = Name), size = 1) + 
  geom_point(aes(col = Status), shape = 21, stroke = 5, fill = "white", size = 13) +
  geom_text(aes(label = paste0(round(USG*100, 0), "%")), check_overlap = TRUE, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#BA0C2F", "#002F6C")) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C")) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1L)) +
  theme_minimal() +
  theme(legend.position = "top"
        , legend.title = element_blank()
        # , panel.grid.major.y = element_blank()
        , text = element_text(size = 20)) +
  scale_size(guide = "none") +
  labs(x = "Usage %",y = "")

ggsave("Dinwiddie On-off.png", p1, width = 12, height = 10, dpi = 300, type = 'cairo')


p2 <- on_off_df %>% 
  select(Name, Status, NetRtg, Min) %>% 
  mutate(Status = ifelse(Status == "On", "Dinwiddie On", "Dinwiddie Off")
         , Min = as.numeric(Min)
         , NetRtg = as.numeric(NetRtg)
         , Name = factor(Name)
         , Name = fct_reorder(Name, Min, .desc = F)
  ) %>% 
  filter(Name != "Spencer Dinwiddie") %>% 
  arrange(NetRtg) %>% 
  ggplot(aes(x = NetRtg, y = Name)) +
  geom_line(aes(group = Name), size = 1) + 
  geom_point(aes(col = Status), shape = 21, stroke = 5, fill = "white", size = 13) +
  geom_text(aes(label = NetRtg), check_overlap = TRUE, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#BA0C2F", "#002F6C")) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C")) + 
  scale_x_continuous(limits = c(-20, 62)) +
  theme_minimal() +
  theme(legend.position = "top"
        , legend.title = element_blank()
        # , panel.grid.major.y = element_blank()
        , text = element_text(size = 20)) +
  scale_size(guide = "none") +
  labs(x = "Net Rating",y = "")

ggsave("Dinwiddie Net Rating.png", p2, width = 12, height = 10, dpi = 300, type = 'cairo')

library(readxl)
PG_data <- read_excel("PG data.xlsx", sheet = "Sheet6")
View(PG_data)

p3 <- PG_data %>% 
  filter(Minutes>=10) %>% 
  ggplot(aes(y = Def_Pts_per100, x = Pts_Per100, size = Minutes)) + 
  geom_point(fill = "#002F6C", col = "black", shape = 21, fill.alpha = 0.6, stroke = 2) + 
  geom_hline(yintercept = 100) + 
  geom_vline(xintercept = 100) +   
  scale_size_continuous(range = c(1, 14)) +
  theme_minimal() +
  theme(legend.position = "NA"
        , text = element_text(size = 20)) +
  annotate("text", x = 50, y = 175, label = "The Bad Place") +
  annotate("text", x = 140, y = 50, label = "The Good Place") +
  annotate("text", x = 140, y = 175, label = "This is Fine") +
  annotate("text", x = 50, y = 50, label = "Garbage Time") +
  labs(x = "Wizards Points per 100 Possessions", y = "Opponents Points per 100 Possessions"
       , caption = "wizardspoints.substack.com\ndata: Clearingtheglass.com")
  
ggsave("Dinwiddie points.png", p3, width = 12, height = 10, dpi = 300, type = 'cairo')



# import game logs
spence <- game_logs(seasons = c(2022), season_types = "Regular Season", result_types = c("player"))

# separate df for bertans
spence2 <- spence %>% filter(namePlayer == "Spencer Dinwiddie") 

p4 <- ggplot(spence2, aes(x = dateGame, y = fga)) + 
  geom_col(alpha = 0.5) +
  geom_smooth(se=F, col = "#BA0C2F") + theme_minimal() +
  theme(text = element_text(size = 20)) +
  labs(x = "", y = "Dinwiddie Field Goal Attempts"
       , caption = "wizardspoints.substack.com\ndata: NBA.com/stats")

ggsave("Dinwiddie fga.png", p4, width = 12, height = 10, dpi = 300, type = 'cairo')

spence2 %>% filter(month(dateGame)<12) %>% summarize(mean = mean(fga, na.rm=T))

spence2 %>% filter(month(dateGame)==12) %>% summarize(mean = mean(fga, na.rm=T))
