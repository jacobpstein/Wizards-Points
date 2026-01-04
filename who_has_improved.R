#
#
#
#
#

# kyshawn george is 94.9th percentile in isolation defense, with 7.9% of his defensive minutes coming in isolation situations and opponents shooting 18.2%
# https://www.nba.com/stats/players/isolation?TypeGrouping=defensive&dir=D&sort=PERCENTILE

# George is 79.8th percentile in defending off screens, with oppoents scoring 33% of the time when he is defending off a screen. 

# He has recoverd 17 loose balls, including 9 on defense, which is a team highest
# https://www.nba.com/stats/players/hustle?PerMode=Totals


# bub leads the team in loose balls recovered at 18
# https://www.nba.com/stats/players/hustle?PerMode=Totals&dir=D&sort=LOOSE_BALLS_RECOVERED

library(tidyverse)
library(hoopR)
library(rstan)
library(tidybayes)
library(slider)
library(ggtext)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

wizards_id <- 1610612764


# get shooting
get_wizards_game_box <- function(game_id) {
  
  box <- nba_boxscoretraditionalv3(
    game_id = game_id
  )
  
  bind_rows(
    box$home_team_player_traditional
    , box$away_team_player_traditional
  ) |>
    tibble() |>
    filter(team_id == wizards_id) |>
    dplyr::select(
      game_id 
      , player_id = person_id
      , first_name
      , family_name
      , min = minutes
      , three_att = three_pointers_attempted
      , three_made = three_pointers_made
      , ft_att = free_throws_attempted
      , ft_made = free_throws_made
    )
}

get_wizards_games <- function(season) {
  
  game_ids <- nba_leaguegamefinder(
    season = season
    , team_id = wizards_id
  ) |>
    tibble() |>
    unnest() |>
    pull(GAME_ID) |>
    unique()
  
  purrr::map_dfr(
    game_ids
    , get_wizards_game_box
  ) |>
    mutate(
      season = season
    )
}

games_last <- get_wizards_games("2024-25")
games_this <- get_wizards_games("2025-26")

long_df <- bind_rows(games_last, games_this) |>
  mutate(player_name = paste0(first_name, " ", family_name)
         , min = as.numeric(ms(min)) / 60
         ) |> 
  mutate(min = ifelse(is.na(min), 0, min)) |> 
  pivot_longer(
    cols = c(three_att, three_made, ft_att, ft_made)
    , names_to = c("shot_type", ".value")
    , names_pattern = "(three|ft)_(att|made)"
  ) |>
  filter(att > 0) |>
  mutate(
    season_id = if_else(season == "2024-25", 0L, 1L)
    , shot_type_id = if_else(shot_type == "ft", 0L, 1L)
  )

player_lookup <- long_df |>
  distinct(player_id, player_name) |>
  mutate(
    player_index = row_number()
  )

long_df <- long_df |>
  left_join(
    player_lookup
    , by = c("player_id", "player_name")
  ) |>
  mutate(
    min_z = as.numeric(scale(min))
  )



# prep data for stan model
stan_data <- list(
  N_obs = nrow(long_df)
  , N_players = nrow(player_lookup)
  
  , player = long_df$player_index
  , season = long_df$season_id
  , shot_type = long_df$shot_type_id
  
  , attempts = long_df$att
  , makes = long_df$made
  
  , minutes = long_df$min_z
)


# fit model
fit <- stan(
  file = "who_has_improved.stan"
  , data = stan_data
  , iter = 4000
  , warmup = 2000
  , chains = 4
  , control = list(
    adapt_delta = 0.95
  )
)

# take a look
print(
  fit
  , pars = c(
    "beta_ft_to_3"
    , "alpha_3"
    , "sigma_evol_3"
  )
)

# player-level improvement
posterior <- rstan::extract(fit)

# limit to just the carry overs
carry_overs <- long_df |> dplyr::select(player_name, season) |> distinct() |> 
  group_by(player_name) |> 
  count() |> filter(n ==2)

player_improvement <- tibble(
  player = player_lookup$player_name
  , mean_3pt_change = colMeans(posterior$p3_diff)
) |> arrange(desc(mean_3pt_change)) |> filter(player %in% carry_overs$player_name)

p3_draws <- posterior$p3_diff |>
  as_tibble() |>
  setNames(player_lookup$player_name) |>
  pivot_longer(
    cols = everything()
    , names_to = "player"
    , values_to = "p3_change"
  ) |> 
  filter(player %in% carry_overs$player_name) 
  

p1 <- ggplot(
  p3_draws
  , aes(
    x = p3_change
    , y = reorder(player, p3_change)
  )
) +
  stat_halfeye(
    aes(
      fill = after_stat(x)
    )
    , .width = c(0.5, 0.8, 0.95)
  ) +
  geom_vline(
    xintercept = 0
    , linetype = "dashed"
  ) +
  labs(
    title = "Season-over-season improvement in three point shooting among Wizards players since last year"
    , subtitle = "Results account for free-throw development, share information across players, and avoid overreacting to small samples or streaks"
    , x = "Change in Three Point %"
    , y = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    
  ) +
  usaidplot::usaid_plot() +
  scale_fill_gradient2(
    low = "#702082" 
    , mid = "#3B9AB2" 
    , high = "#0000FF80"
    , midpoint = 0
  ) 
  
p1 + ggview::canvas(width = 16, height = 10)

ggsave("model_results.png", p1, width = 16, height = 10, dpi = 300)

prob_improved <- p3_draws |>
  group_by(player) |>
  summarise(
    prob_improve = mean(p3_change > 0)
  ) |> 
  arrange(desc(prob_improve))

posterior$p3_diff |>
  as_tibble() |>
  setNames(player_lookup$player_name) |>
  pivot_longer(
    cols = everything()
    , names_to = "player"
    , values_to = "p3_change"
  ) |>
  group_by(player) |>
  summarise(
    mean_change = mean(p3_change)
    , median_change = median(p3_change)
    , lower_95 = quantile(p3_change, 0.025)
    , upper_95 = quantile(p3_change, 0.975)
    , prob_improve = mean(p3_change > 0)
  ) |>
  arrange(desc(mean_change))

# descriptive graph---
games <- nba_leaguegamefinder(
  season = c("2025-26")
  , team_id = wizards_id
) |>
  tibble() |>
  unnest() |>
  dplyr::select(GAME_ID, GAME_DATE) |>
  unique() |> 
  janitor::clean_names() |> 
  bind_rows(
    nba_leaguegamefinder(
      season = c("2024-25")
      , team_id = wizards_id
    ) |>
      tibble() |>
      unnest() |>
      dplyr::select(GAME_ID, GAME_DATE) |>
      unique() |> 
      janitor::clean_names()
  )


plot_df <-
  long_df |> 
  filter(
    shot_type == "three"
    , family_name == "Carrington"
    , season %in% c("2024-25", "2025-26")
  ) |> 
  mutate(
    pct = made / att
  ) |> 
  left_join(games) |> 
  arrange(season, game_date) |> 
  group_by(season) |> 
  mutate(
    `Game Number` = row_number()
    , pct_roll = slide_dbl(
      pct
      , mean
      , .before = 4
      , .complete = FALSE
      , na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  filter(
    season != "2024-25" | `Game Number` < 33
  ) |> 
  dplyr::select(`Game Number`, season, pct_roll) |> 
  pivot_wider(
    names_from = season
    , values_from = pct_roll
  )


p2 <- ggplot(plot_df, aes(x = `Game Number`)) +
  geom_ribbon(
    aes(
      ymin = `2024-25`
      , ymax = `2025-26`
    )
    , fill = "lightgrey"
    , alpha = .3
  ) +
  geom_line(
    aes(y = `2024-25`)
    , color = "#3B9AB2"
    , linewidth = 1
  ) +
  geom_line(
    aes(y = `2025-26`)
    , color = "maroon"
    , linewidth = 1
  ) +
  usaidplot::usaid_plot() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = ""
    , y = "Three Point %"
    , title = "Bub Carrington's Rolling Three Point % Average <span style='color:maroon;'>This</span> Season and <span style='color:#3B9AB2;'>Last</span> Season"
    , subtitle = "We use a five game rolling average to help smooth out streakiness\nBub is shooting much better 32 games into the season"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(
    plot.title = ggtext::element_markdown()
  )

p2 + ggview::canvas(width = 14, height = 10)

ggsave('this_year_and_last_year.png', p2, width = 14, height = 10, dpi = 600)


# attempts this season---
long_df |> 
  filter(
    shot_type == "three"
    , family_name == "Carrington"
    , season %in% c("2024-25", "2025-26")
  ) |> 
  mutate(
    pct = made / att
  ) |> 
  left_join(games) |> 
  arrange(season, game_date) |> 
  group_by(season) |> 
  mutate(
    `Game Number` = row_number()
    , pct_roll = slide_dbl(
      pct
      , mean
      , .before = 4
      , .complete = FALSE
      , na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  filter(
    season != "2024-25" | `Game Number` < 33
  ) |> group_by(season) |> 
  summarise(mean= mean(att))


# ft pct this season---
long_df |> 
  filter(
    shot_type == "ft"
    , family_name == "Carrington"
    , season %in% c("2024-25", "2025-26")
  ) |> 
  mutate(
    pct = made / att
  ) |> 
  left_join(games) |> 
  arrange(season, game_date) |> 
  group_by(season) |> 
  mutate(
    `Game Number` = row_number()
    , pct_roll = slide_dbl(
      pct
      , mean
      , .before = 4
      , .complete = FALSE
      , na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  filter(
    season != "2024-25" | `Game Number` < 33
  ) |> group_by(season) |> 
  summarise(mean= mean(pct))
