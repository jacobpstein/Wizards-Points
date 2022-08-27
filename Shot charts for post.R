###########################
# This file creates graphs of the Wizards shot selection
#
#
#
###########################




# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(hexbin)
library(prismatic)
library(teamcolors)
library(cowplot)
library(ggridges)


# Get NBA teams and their names
tms <- nba_teams()
tms <- tms %>% 
  filter(isNonNBATeam == 0) %>% 
  select(nameTeam, slugTeam)

# Get NBA team colors
tm.colors <- teamcolors
tm.colors <- tm.colors %>% 
  filter(league == "nba") %>% 
  select(name, primary) %>% 
  mutate(primary = case_when(
    name == "Golden State Warriors" ~ "#1D428A",
    name == "Indiana Pacers" ~ "#002D62",
    name == "Los Angeles Lakers" ~ "#552583",
    name == "San Antonio Spurs" ~ "#000000",
    name == "Oklahoma City Thunder" ~ "#EF3B24",
    name == "Charlotte Hornets" ~ "#00788C",
    name == "Utah Jazz" ~ "#00471B",
    name == "New Orleans Pelicans" ~ "#0C2340",
    TRUE ~ primary
  ))

# Load NBA court dimensions from github
devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")

# Get shots
df <- teams_shots(teams = "Washington Wizards", season_types = "Regular Season", seasons = 2022)

# join w/ team dataset so that we can find out which team is on offense/defense
df <- left_join(df, tms)

# if slugTeam is the home team, then the defense must be the away team (visa versa)
df <- df %>% 
  mutate(defense = case_when(
    slugTeam == slugTeamHome ~ slugTeamAway,
    TRUE ~ slugTeamHome
  ))

# get the full name of the defensive team 
df <- left_join(df, tms, by = c("defense" = "slugTeam"))

# rename to distinugish between offensive and defensive team
df <- df %>% 
  rename("nameTeamOffense" = "nameTeam.x", 
         "nameTeamDefense" = "nameTeam.y")

# transform the location to fit the dimensions of the court, rename variables 
df <- df %>% 
  mutate(locationX = as.numeric(as.character(locationX)) / 10,
         locationY = as.numeric(as.character(locationY)) / 10 + hoop_center_y) %>% 
  rename("loc_x" = "locationX", 
         "loc_y" = "locationY")

# flip values along the y-axis
df$loc_x <- df$loc_x * -1 


# Filter out backcourt shots or anything more than 35 feet
df <- df %>% 
  filter(zoneBasic != "Backcourt" & distanceShot <= 35)

# Create a function that helps create our custom hexs
hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

# Set the size of the hex
binwidths <- 3.5

# Calculate the area of the court that we're going to divide into hexagons
xbnds <- hex_bounds(df$loc_x, binwidths)
xbins <- diff(xbnds) / binwidths
ybnds <- hex_bounds(df$loc_y, binwidths)
ybins <- diff(ybnds) / binwidths

# Create a hexbin based on the dimensions of our court
hb <- hexbin(
  x = df$loc_x,
  y = df$loc_y,
  xbins = xbins,
  xbnds = xbnds,
  ybnds = ybnds,
  shape = ybins / xbins,
  IDs = TRUE
)

# map our hexbins onto our dataframe of shot attempts
df <- mutate(df, hexbin_id = hb@cID)

# find the leauge avg % of shots coming from each hex
la <- df %>%
  group_by(hexbin_id) %>%
  summarize(hex_attempts = n()) %>% 
  ungroup() %>% 
  mutate(hex_pct = hex_attempts / sum(hex_attempts, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename("league_average" = "hex_pct") %>% 
  select(-hex_attempts)

ids_teams <- df %>% select(idGame, nameTeamDefense) %>% unique() %>% 
  group_by(nameTeamDefense) %>% 
  mutate(game_number = seq_along(nameTeamDefense)) %>% 
  ungroup() %>% 
  mutate(outcome = c("W", "W", "L", "W", "W", "W", "L", "L", "W", "W", "W", "W", "W", "L", "L", "W")
         , label= ifelse(game_number==1, paste0(nameTeamDefense, "-", outcome), paste0(nameTeamDefense, " Game ", game_number, "-", outcome)))

df2 <- df %>% left_join(ids_teams)

df2 %>%  
  # filter(isShotMade==TRUE) %>%
  ggplot(aes(x = minutesRemaining)) + 
  # geom_density_ridges(aes(col = outcome, fill = outcome), alpha = 0.3, scale = 0.9, size = 1)
  geom_density(aes(col = outcome), size = 1) + 
  facet_wrap(~zoneBasic)


df3 <- df2 %>% mutate(label = fct_reorder(label, idGame, min))


p <- ggplot() +
  geom_point(data = df3, 
             aes(x = loc_x, y = loc_y
                 , fill = isShotMade
                 , color = isShotMade), 
             size = 3, alpha = 0.4) +
  scale_fill_manual(values = c("#BA0C2F", "#002F6C")) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C")) +
  facet_wrap(~label
             , nrow = 3
             , strip.position = 'top') +
  scale_alpha_continuous(range = c(.05, 1)) +
  theme(legend.position = 'none',
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.spacing  = unit(-.25, "lines"), 
        plot.title = element_text(face = 'bold', hjust= .5, size = 15, color = 'black'),
        plot.subtitle = element_text(hjust= .5, size = 12, color = 'black'),
        
        plot.caption  = element_text(size = 6, hjust= .5, color = 'black'),
        strip.text = element_text(size = 10, vjust = -1)
        , strip.background = element_blank()
       , panel.background=element_blank()
        ) + 
  scale_y_continuous(limits = c(-2.5, 42)) +
  scale_x_continuous(limits = c(-30, 30))  +
  coord_fixed(clip = 'off') +
  labs(title =  "Where the Wizards are shooting", subtitle = "Blue circles are made shots, red circles are missed shots") +
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype = dash),
            color = "black", size = .25) 

p <- ggdraw(p) + 
  theme(plot.background = element_rect(fill="white", color = NA))

p

ggsave("Wizards shot chart.png", p, width = 10, height = 10, type = 'cairo')

# Opponents-----------

teams <- unique(df$nameTeamDefense)

games <- unique(df$idGame)


# Get shots
df_opp <- teams_shots(teams = teams, season_types = "Regular Season", seasons = 2022)

# join w/ team dataset so that we can find out which team is on offense/defense
df_opp <- left_join(df_opp, tms) %>% 
  filter(idGame %in% games)

# transform the location to fit the dimensions of the court, rename variables 
df_opp <- df_opp %>% 
  mutate(locationX = as.numeric(as.character(locationX)) / 10,
         locationY = as.numeric(as.character(locationY)) / 10 + hoop_center_y) %>% 
  rename("loc_x" = "locationX", 
         "loc_y" = "locationY")

# flip values along the y-axis
df_opp$loc_x <- df_opp$loc_x * -1 

# Filter out backcourt shots or anything more than 35 feet
df_opp <- df_opp %>% 
  filter(zoneBasic != "Backcourt" & distanceShot <= 35)

df_opp2 <- df_opp %>% left_join(ids_teams) 

df_opp3 <- df_opp2 %>% mutate(label = fct_reorder(label, idGame, min))

df_opp3 <- df_opp3 %>% 
  mutate(defense = case_when(
    slugTeam == slugTeamHome ~ slugTeamAway,
    TRUE ~ slugTeamHome
  )) %>% 
  filter(idGame %in% games)


p2 <- ggplot() +
  geom_point(data = df_opp3, 
             aes(x = loc_x, y = loc_y
                 , fill = isShotMade
                 , color = isShotMade), 
             size = 3, alpha = 0.4) +
  scale_fill_manual(values = c("#BA0C2F", "#002F6C")) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C")) +
  facet_wrap(~label
             , nrow = 3
             , strip.position = 'top') +
  scale_alpha_continuous(range = c(.05, 1)) +
  theme(legend.position = 'none',
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.spacing  = unit(-.25, "lines"), 
        plot.title = element_text(face = 'bold', hjust= .5, size = 15, color = 'black'),
        plot.subtitle = element_text(hjust= .5, size = 12, color = 'black'),
        
        plot.caption  = element_text(size = 6, hjust= .5, color = 'black'),
        strip.text = element_text(size = 10, vjust = -1)
        , strip.background = element_blank()
        , panel.background=element_blank()
  ) + 
  scale_y_continuous(limits = c(-2.5, 42)) +
  scale_x_continuous(limits = c(-30, 30))  +
  coord_fixed(clip = 'off') +
  labs(title =  "Where the Wizards opponents are shooting", subtitle = "Blue circles are made shots, red circles are missed shots") +
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype = dash),
            color = "black", size = .25) 

p2 <- ggdraw(p2) + 
  theme(plot.background = element_rect(fill="white", color = NA))

p2

ggsave("Opponents shot chart.png", p2, width = 10, height = 10, type = 'cairo')

# point in paint by game
df_opp3 %>% 
  group_by(label, zoneBasic) %>% 
  count() %>% 
  ggplot(aes(x = label, y = n)) +
  geom_col() +
  facet_wrap(~zoneBasic)


p3 <- df_opp3 %>%  
  ggplot(aes(x = minutesRemaining)) + 
  geom_density(aes(col = outcome), size = 2) + 
  # scale_fill_manual(values = c("#002F6C", "#BA0C2F")) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C")) +
  facet_wrap(~zoneBasic) +
  theme(legend.position = 'none',
        line = element_blank(),
        plot.title = element_text(face = 'bold', hjust= .5, size = 15, color = 'black'),
        plot.subtitle = element_text(hjust= .5, size = 12, color = 'black'),
        strip.text = element_text(size = 10, vjust = -1)
        , strip.background = element_blank()
        , panel.background=element_blank()
  ) +
  labs(title =  "Distribution of opponents shooting by location", 
       subtitle = "Blue lines are Wizards wins, red lines are Wizards losses"
       , x = "Minutes remaining", y = "")

ggsave("Opponents distribution chart.png", p3, width = 10, height = 10, type = 'cairo')


# heat

df_heat<- df3 %>% filter(defense=="MIA")

df_heat %>% 
  group_by(outcome, zoneBasic
           , minutesRemaining
           ) %>% 
  count() %>% 
  ggplot(aes(x = minutesRemaining, y = n)) +
  geom_smooth(aes(col = outcome), se = F) +
  facet_wrap(~zoneBasic)

df_heat %>% 
  ggplot(aes(x=minutesRemaining)) +
  geom_density(aes(col = zoneBasic)) +
  facet_wrap(~label)

# Charlotte
df_cha <- df3 %>% filter(defense=="CHA" & namePlayer == "Kyle Kuzma")

p3 <- ggplot() +
  geom_point(data = df_cha, 
             aes(x = loc_x, y = loc_y
                 , fill = isShotMade
                 , color = isShotMade), 
             size = 3, alpha = 0.4) +
  scale_fill_manual(values = c("#BA0C2F", "#002F6C")) +
  scale_color_manual(values = c("#BA0C2F", "#002F6C")) +
  scale_alpha_continuous(range = c(.05, 1)) +
  theme(legend.position = 'none',
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.spacing  = unit(-.25, "lines"), 
        plot.title = element_text(face = 'bold', hjust= .5, size = 15, color = 'black'),
        plot.subtitle = element_text(hjust= .5, size = 12, color = 'black'),
        
        plot.caption  = element_text(size = 6, hjust= .5, color = 'black'),
        strip.text = element_text(size = 10, vjust = -1)
        , strip.background = element_blank()
        , panel.background=element_blank()
  ) + 
  scale_y_continuous(limits = c(-2.5, 42)) +
  scale_x_continuous(limits = c(-30, 30))  +
  coord_fixed(clip = 'off') +
  labs(title =  "Where the Kuzma hit against the Hornets", subtitle = "Blue circles are made shots, red circles are missed shots") +
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype = dash),
            color = "black", size = .25) 

p3 <- ggdraw(p3) + 
  theme(plot.background = element_rect(fill="white", color = NA))

p3

