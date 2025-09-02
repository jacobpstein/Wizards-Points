# load libraries

library(nbastatR)
library(tidyverse)
library(rstanarm)
library(hoopR)
library(tidyverse)
library(janitor)
library(WeightIt)
library(cobalt)
library(tidybayes)
library(ggridges)
library(ggbump)
library(cowplot)
library(usaidplot)
library(ggrepel)
library(viridis)

# set seed
set.seed(202)

# expand thte amount of data we can download
Sys.setenv(VROOM_CONNECTION_SIZE = 3613107200*3)

# we'll be looking at the past 20 years
rookie_seasons <- 2006:2025

# get rookie seasons and player ids
rookie_info <- nba_playerindex()$PlayerIndex |> 
  filter(FROM_YEAR>=2006) |> 
  clean_names() |> 
  mutate(from_year = as.numeric(from_year)
         , draft_number = as.numeric(draft_number)
         , draft_round = as.numeric(draft_round)
         , draft_round = ifelse(person_id == "202536", 0, draft_round)
  ) |> 
  # there is something wacky in the draft round data, let's fix it
  mutate(draft_round_char = case_when(draft_round == 1 ~ "First Round"
                                      , draft_round == 2 ~ "Second Round"
                                      , TRUE ~ "Undrafted")
  )

# create an empty DF list
df_list <- list()

# should probably update this to a map function
for(i in seq_along(rookie_seasons)) {
  season_str <- year_to_season(rookie_seasons[i])
  
  stats <- nba_leaguedashplayerstats(
    season = season_str
    , season_types = "Regular Season"
    , per_mode = "PerGame"
    , measure_type = "Advanced"
  )
  
  stats_data <- stats$LeagueDashPlayerStats
  stats_data$season <- season_str
  
  df_list[[i]] <- stats_data
}

df_all <- bind_rows(df_list) |> 
  mutate(season_year = as.numeric(substr(season, 1, 4))
         , MIN = as.numeric(MIN)) |> clean_names() |> 
  rename(person_id = player_id) |> 
  mutate(across(.cols = c(6:78),  as.numeric))

df_all2 <- df_all |>
  mutate(person_id = as.character(person_id)
  ) |>
  left_join(
    rookie_info |>
      dplyr::select(person_id
              , "rookie_year" = from_year
             , draft_year
             , draft_round
             , draft_number
             , draft_round_char
             , position
             , college
             , height
             , weight
      )
    , by = "person_id"
  ) |>
  mutate(season_index = if_else(!is.na(rookie_year)
                                , season_year - rookie_year + 1
                                , NA_real_
  )
  , is_rookie = season_index == 1
  , is_fourth = season_index == 4
  , season_label = case_when(season_index == 1 ~ "Rookie"
                             , season_index == 4 ~ "Fourth"
                             , TRUE ~ NA_character_
  )
  , wiz = case_when(team_abbreviation == "WAS" ~ "Wizards"
                    , TRUE ~ "Other Teams")
  ) |> 
  arrange(person_id)

# probability of making to year four---

last_season_year <- max(df_all2$season_year, na.rm = TRUE)
cutoff_rookie_year <- last_season_year - 3

player_status <- df_all2 |>
  filter(!is.na(rookie_year)) |>
  group_by(person_id) |> 
  ungroup() |> 
  group_by(person_id
           , rookie_year
           , draft_round_char
  ) |>
  summarise(has_year4 = any(season_index == 4)
            , .groups = "drop"
  ) |>
  mutate(eligible = rookie_year <= cutoff_rookie_year)

eligible_players <- player_status |>
  filter(eligible)

# overall probability + exact (Clopper–Pearson) 95% CI
overall <- eligible_players |>
  summarise(n_rookies = n()
            , made_year4 = sum(has_year4)
            , p_hat = made_year4 / n_rookies
  ) |>
  mutate(ci = list(binom.test(made_year4, n_rookies)$conf.int)
         , p_low = ci[[1]][1]
         , p_high = ci[[1]][2]
  ) |>
  select(-ci)

# by draft round
by_round <- eligible_players |>
  group_by(draft_round_char) |>
  summarise(n_rookies = n()
            , made_year4 = sum(has_year4)
            , p_hat = made_year4 / n_rookies
            , .groups = "drop"
  ) |>
  mutate(p_low = map2_dbl(made_year4, n_rookies
                          , ~ binom.test(.x, .y)$conf.int[1]
  )
  , p_high = map2_dbl(made_year4, n_rookies
                      , ~ binom.test(.x, .y)$conf.int[2]
  )
  )



# let's look at rookie stats and year four stats
df_all2 |> 
  filter(season_label %in% c("Rookie", "Fourth")) |> 
  select(person_id, min, season_label) |> 
  pivot_wider(names_from = season_label, values_from = min) |> 
  ggplot(aes(x = (Rookie), y = (Fourth))) +
  geom_point() +
  geom_smooth() +
  ggpubr::stat_cor()


df_all2 |> 
  filter(season_label %in% c("Rookie", "Fourth")) |> 
  select(person_id, net_rating, season_label, draft_round_char) |> 
  pivot_wider(names_from = season_label, values_from = net_rating) |> 
  # keep rookie minutes alongside
  left_join(df_all2 |> 
              filter(season_label == "Rookie") |> 
              select(person_id, min) |> 
              rename(min_rookie = min),
            by = "person_id") |> 
  mutate(
    # define movers as change net_rating| >= 5
    delta = Fourth - Rookie,
    is_mover = abs(delta) >= 5
  ) |> 
  ggplot(aes(x = Rookie, y = Fourth)) +
  # dashed 45 line (no change)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  
  # segments from rookie -> fourth (faint if low minutes)
  geom_segment(aes(x = Rookie, xend = Rookie,
                   y = Rookie, yend = Fourth,
                   ),
                alpha = 0.9
               , col = "grey60") +
  
  geom_point(aes(size = min_rookie, fill = draft_round_char
                ),
             , col = "white", shape = 21, alpha = 0.4) +
  scale_size_continuous(name = "Rookie minutes", range = c(1, 6)) +
  scale_alpha_continuous(guide = "none") +
  scale_color_brewer(palette = "Set1", name = "Draft Round") +
  xlim(-100, 100) + ylim(-100, 100) +
  facet_wrap(~draft_round_char) +
  labs(
    x = "Rookie Season Net Rating",
    y = "4th Season Net Rating",
    title = "Rookie vs. 4th Season Net Rating",
    subtitle = "Arrows show movement; color highlights movers (Δ ≥ 5 net rating)"
  ) +
  theme_minimal(base_size = 14)

df_all2 |> 
  filter(is.na(draft_number)!=T) |> 
  group_by(season_index, draft_number) |> 
  count() |> 
  ggplot(aes(x = season_index, y = n)) +
  geom_col(aes(fill = factor(draft_number)), position = 'fill', stat = 'identity')



# first graph----
# let's just look at Wizards rookies years one through four
df_wiz <- df_all2 |> filter(wiz == "Wizards" & rookie_year!=2024) |> 
  group_by(person_id, player_name) |>
  filter(min(season_index) == 1) |>   
  filter(season_index %in% 1:15) |>
  ungroup() |> 
  select(player_name, season_index, net_rating, off_rating, def_rating, draft_round_char) |> 
  filter(player_name!="Anžejs Pasečņiks") # this dude


p1 <- df_wiz |> 
  filter(draft_round_char == "First Round") |> 
ggplot(aes(season_index, net_rating, color = player_name)) +
  geom_point(size = 5) +
  geom_bump(size = 1, smooth = 8) +
  geom_label_repel(data = df_wiz |> group_by(player_name) |> slice(which.max(season_index)) |> filter(draft_round_char == "First Round" & season_index>=2),
                  aes(x = season_index, label = player_name), size = 8, max.overlaps = 15, hjust = 0.5, fill = "#DDDDDD") +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  # geom_label_repel(data = df_wiz |> group_by(player_name) |> slice(which.min(season_index)) |> filter(season_index==1 & draft_round_char == "First Round"),
                  # aes(x = season_index, label = player_name), size = 5, max.overlaps = 15) +
  
  scale_x_continuous(limits = c(1, 11),
                     breaks = seq(1, 11, 1)) +
  usaid_plot(ppt = TRUE) +
  scale_color_manual(values = rep(c("#440154FF","#EE404E", "#414487FF",  "#C70E7B", "#2A788EFF", "#D84D16", "#22A884FF"), 4)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()
        , text = element_text(size = 30)
        , plot.title = element_text(size = 32)
        , plot.subtitle = element_text(size = 30)
        , axis.text = element_text(size = 30)
        , axis.title = element_text(size = 30)) +
  labs(y = "Net Rating",
       x = "Seasons with Wizards"
       , title = "Net Rating for Wizards First Round Picks by Season"
       , subtitle = "A lot of players improve, but remain net negatives into season four"
       , caption = "Data: NBA.com/stats\nwizardspoints.substack.com") 

ggsave("Wiz_Rookie_Perf.png", p1, height = 18, width = 18, dpi = 600)

# second graph

p2 <- df_all2 |> 
  filter(season_label %in% c("Rookie", "Fourth")) |> 
  select(person_id, net_rating, season_label, draft_round_char) |> 
  pivot_wider(names_from = season_label, values_from = net_rating) |> 
  left_join(df_all2 |> filter(season_label == "Rookie") |> select(person_id, min)) |> 
  ggplot(aes(x = (Rookie), y = (Fourth), col = draft_round_char)) +
  geom_point(aes(size = min, fill = draft_round_char), col = "white", shape = 21, stroke = 2, alpha = 0.5) +
  usaid_plot(ppt = TRUE) +
  scale_fill_manual(values = rep(c("#440154FF","#EE404E", "#414487FF",  "#C70E7B", "#2A788EFF", "#D84D16", "#22A884FF"), 4)) +
  geom_smooth(method = "lm") + xlim(-20, 20) + facet_wrap(~draft_round_char) +
  labs(x = "Rookie Season Net Rating"
       , y = "Season Four Net Rating"
       , title = "Relationship between rookie season and fourth season net rating by draft round"
       , subtitle = "Points are sized by average minutes per game, so larger dots mean more minutes on average"
       , caption = "Data: NBA.com/stats\nwizardspoints.substack.com") +
  ggpubr::stat_cor()

ggsave("net_rating_cor.png", p2, height = 9, width = 14, dpi = 600)



df_wiz |> 
  filter(draft_round_char == "First Round") |> group_by(player_name) |> slice(which.min(season_index), which.max(season_index)) |> 
  select(player_name, season_index, net_rating) |> 
  mutate(diff = abs(net_rating)-lag(abs(net_rating))) |> drop_na() |> 
  arrange(diff)
  
# let's model----
df <- df_all2

# Drop rows with missing "is_fourth" or rookie minutes
df <- df |>
  group_by(person_id) |> 
  mutate(same_team_fourth = (team_id == first(team_id)) & is_fourth
  ) |>
  ungroup() |> 
  mutate(draft_round = factor(draft_round_char)
         , rookie_net_rating = ifelse(season_year == rookie_year
                                      , net_rating
                                      , NA
         )
         , is_fourth = ifelse(is_fourth == TRUE, 1, 0)
  ) |> 
  group_by(person_id) |> 
  fill(rookie_net_rating, .direction = "down")

# identify rookie seasons in our data
# this gives us 1630 players
rookie_stats <- df |>
  filter(season_index == 1 | is_rookie == TRUE) |>  # identify rookie seasons
  select(person_id, rookie_min = min
         , rookie_net_rating = net_rating
         , draft_round_char, team_id, position, season, team_abbreviation, wiz, age) |>
  filter(!is.na(rookie_min), !is.na(draft_round_char))

# 749 make it to year four
year_four_stats <- df |>
  filter(is_fourth == 1) |>
  select(person_id, year_four_net_rating = net_rating, same_team_fourth) |>
  filter(!is.na(year_four_net_rating))

# Merge datasets
analysis_df <- year_four_stats |>
  inner_join(rookie_stats, by = "person_id") |>
  filter(!is.na(rookie_min), !is.na(year_four_net_rating)
         , !is.na(draft_round_char), !is.na(position)) |>
  mutate(draft_round = factor(draft_round_char))


# Estimate Generalized Propensity Score (GPS)
# Model how rookie minutes are determined by pre-treatment confounders
gps_fit <- stan_glmer(rookie_min ~ draft_round 
                         + rookie_net_rating 
                        + position + (1 | team_id)
                      , data = analysis_df
                      , family = gaussian()
                      , chains = 2, iter = 1000)  


# get GPS predictions and residual variance
gps_mu <- posterior_epred(gps_fit) |> 
  apply(2, mean)  

# Residual standard deviation
gps_sigma <- sigma(gps_fit)

# Generalized propensity score for each player
analysis_df$gps <- dnorm(
  analysis_df$rookie_min
  , mean = gps_mu, sd = gps_sigma
)

analysis_df$gps_weights <- 1 / analysis_df$gps

# check proportion of extreme weights, more than 10% is bad 
# we're at 8.6% which is borderline, but I can live with it
round(100 * sum(analysis_df$gps_weights > 100) / nrow(analysis_df), 1)

# we have some extreme weights in our data, largely due to Damien Lillard and John Wall,
# who both had a ton of rookie minutes and way over performed what would be predicted
# let's trim our sample. I don't love this, but it helps stabilize our estimates 
analysis_df |> 
  filter(gps_weights > 100) |>
  arrange(desc(gps_weights))

weight_95th <- quantile(analysis_df$gps_weights, 0.95)

analysis_df_trimmed <- analysis_df |>
  filter(gps_weights <= weight_95th)


bayes_fit <-  stan_glmer(year_four_net_rating ~ rookie_min
                         + draft_round_char
                         + rookie_net_rating
                         + same_team_fourth
                         + age
                         + (1 | team_id)
                         + (1 | position)
                         + (1 | season)
                         , data = analysis_df_trimmed
                         , weights = gps_weights
                         , prior_intercept = normal(0, 5)
                         , prior = normal(0, 2.5)
                         , chains = 4, iter = 2000, cores = 4)

summary(bayes_fit)

# Extract posterior for rookie minutes effect
min_posterior <- bayes_fit |>
  spread_draws(rookie_min) |>
  rename(rookie_min_effect = rookie_min)

min_summary <- min_posterior |>
  median_qi(rookie_min_effect, .width = c(0.8, 0.95))

p3 <- as_draws_df(bayes_fit)[, "rookie_min"] |> 
ggplot(aes(x = rookie_min)) +
  geom_histogram(aes(y = ..density..)
                 , bins = 40
                 , fill = "#440154FF"
                 , color = "white"
                 , alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  labs(
    x = "Distribution of Estimated Effects"
    , y = ""
    , title = "Effect of rookie minutes on year four net rating"
    ,   caption = "Data: NBA.com/stats\nwizardspoints.substack.com"
  ) +
  usaid_plot(ppt = T) +
  theme(axis.text.y = element_blank())

ggsave("model_est.png", p3, height = 9, width = 14, dpi = 600)

