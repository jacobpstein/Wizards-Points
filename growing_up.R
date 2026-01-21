

# load libraries
library(tidyverse)
library(hoopR)
library(tidybayes)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggridges)
library(ggbump)
library(ggbeeswarm)
library(ggtext)




# increase buffer size
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 3) 


# create a vector of seasons we want to look at
seasons <- 2000:2025

# grab the data
bio_raw <- map_dfr(
  seasons
  , ~ nba_leaguedashplayerbiostats(
    season = paste0(.x, "-", substr(.x + 1, 3, 4))
  )$LeagueDashPlayerBioStats |>
    mutate(season_start = .x)
)

# Convert to a usable player-season panel
bio_df <- bio_raw |>
  transmute(
    player_id = PLAYER_ID
    , player_name = PLAYER_NAME
    , team_abbrev = TEAM_ABBREVIATION
    , season_start
    , age = parse_number(AGE)
    , height_in = parse_number(PLAYER_HEIGHT_INCHES)
    , weight_lb = parse_number(PLAYER_WEIGHT)
    , gp = parse_number(GP)
  ) |>
  filter(
    !is.na(height_in)
    , !is.na(weight_lb)
  )

# Create a career season number so we can track where guys are
bio_df <- bio_df |>
  arrange(player_id, season_start) |>
  group_by(player_id) |>
  mutate(
    career_season = row_number() - 1 # we will do this just for modeling purposes
    , career_season_non_mod = row_number()
    , first_season = min(season_start)
    , last_season = max(season_start)
    , career_length = n()
  ) |>
  ungroup()

# Within-player variance stabilization
bio_df <- bio_df |>
  group_by(player_id) |>
  mutate(
    delta_height = height_in - lag(height_in)
    , delta_weight = weight_lb - lag(weight_lb)
  ) |>
  ungroup()

steady_summary <- bio_df |>
  group_by(career_season) |>
  summarise(
    mean_abs_dh = mean(abs(delta_height), na.rm = TRUE)
    , mean_abs_dw = mean(abs(delta_weight), na.rm = TRUE)
    , mean_height = mean(height_in, na.rm = TRUE)
    , mean_weight = mean(weight_lb, na.rm = T)
    , n = n()
  )

# let's take a look
steady_summary |> filter(career_season<19) |> 
  ggplot(aes(x = career_season, y = mean_abs_dh)) +
  geom_line()

steady_summary |> 
  ggplot(aes(x = career_season, y = mean_abs_dw)) +
  geom_line()

#

# let's do this at a player level
# For each player, estimate a changepoint in levels using grid search

estimate_tau <- function(df, y_var) {
  taus <- 2:(max(df$career_season) - 1)
  
  rss <- map_dbl(
    taus
    , ~ {
      tau <- .x
      df |>
        mutate(
          y_hat = if_else(
            career_season <= tau
            , career_season
            , tau
          )
        ) |>
        summarise(
          rss = sum(
            (get(y_var) - lm(get(y_var) ~ y_hat)$residuals)^2
          )
        ) |>
        pull(rss)
    }
  )
  
  taus[which.min(rss)]
}
  

# let's look at differences by position

positions_df <- nba_playerindex(historical = 1)$PlayerIndex |> select(player_id = PERSON_ID, position = POSITION)


# saveRDS(positions_df, "player_positions.rds")

bio_df_post <- bio_df |>
  left_join(positions_df, by = "player_id") |> 
  mutate(position = ifelse(player_id == "202197", "F-G"
                    ,ifelse(player_id == "2130", "G", position ))
  ) |> 
  mutate(position = case_when(position %in% c("C", "C-F", "F-C") ~ "Center"
                              , position %in% c("F", "F-G", "G-F") ~ "Forward"
                              , position %in% c("G") ~ "Guard"
                              ))

p <- ggplot(bio_df_post)  +
  geom_bump(aes(x = career_season, y = delta_weight, group = player_id)
            , alpha = 0.6
            , color = "#DDDDDD"
            ) +
  geom_bump(data = bio_df_post |> group_by(career_season, position) |> summarise(delta_weight = mean(delta_weight, na.rm=T))
            , aes(x = career_season, y = delta_weight)
            , alpha = 1
            , color = "maroon") +
  usaidplot::usaid_plot() +
  facet_wrap(~position, scale = "free_x") +
  labs(x = "Season Number", y = "Change in Weight (lbs)"
       , title = "Average Weight in the NBA Doesn't Change That Much"
       , subtitle = "But there's a ton of variation by position and even within specific players"
       , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
       ) +
  geom_text(data = data.frame(x = 5.8, y = 52.4846232650999, label = "Wang Zhizhi +55lbs", position = "Center"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = "#666666") +
  geom_text(data = data.frame(x = 8, y = 38, label = "Wizards legend\nDaniel Gafford + 38lbs", position = "Center"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = "#666666") +
  geom_text(data = data.frame(x = 5, y = -42, label = "Yaroslav Korolev -42lbs", position = "Forward"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = "#666666") +
  geom_text(data = data.frame(x = 18, y = 16, label = "Jamal Crawford +15lbs", position = "Guard"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = "#666666") +
  geom_text(data = data.frame(x = 4, y = 2, label = "Average Change", position = "Forward"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = "maroon", fontface = 'bold') 

ggsave("weight_changes.png", p, height = 10, width = 16, dpi = 300)

# Wang Zhi-zhi gained 55 lbs in his third season
# Forward Yaroslav Korolev lost 42 lbs in his second season
# Forward Zach Randolph lost 38 lbs in his sixth season
# Daniel Gafford gained 31 lbs in his seventh season




# Do players by position reach steady state earlier?
tau_df <- bio_df_post |>
  group_by(player_id, position) |>
  summarise(
    steady_season = estimate_tau(cur_data(), "weight_lb")
    , .groups = "drop"
  )


tau_df |>
  group_by(position) |>
  summarise(
    mean_tau = mean(steady_season, na.rm = TRUE)
    , sd_tau = sd(steady_season, na.rm = TRUE)
    , n = n()
  )

tau_prior_df <- tau_df |>
  group_by(position) |>
  summarise(mean_tau = mean(steady_season), .groups = "drop")


# model prep----
stan_df <- bio_df_post |>
  filter(
    !is.na(weight_lb)
    , !is.na(career_season)
    , !is.na(position)
  ) |>
  arrange(player_id, career_season) |>
  mutate(
    player_index = as.integer(factor(player_id))
    , pos_index = as.integer(factor(position))
  )

player_lookup <- stan_df |>
  distinct(player_index, player_id)

pos_lookup <- stan_df |>
  distinct(pos_index, position)


alpha_prior_df <- tibble(
  position = c("C", "F", "G"),
  alpha_prior_mean = c(250, 225, 195)
)

alpha_prior_vec <- pos_lookup |>
  left_join(alpha_prior_df, by = "position") |>
  arrange(pos_index) |>
  pull(alpha_prior_mean)

logit <- function(p) log(p / (1 - p))

tau_prior_mean_unbounded <- pos_lookup |>
  left_join(tau_prior_df, by = "position") |>
  mutate(
    tau_proportion = (mean_tau - 1) / 5,  # Map [1, 6] to [0, 1]
    tau_proportion_bounded = pmax(0.01, pmin(0.99, tau_proportion)),
    logit_tau = logit(tau_proportion_bounded)
  ) |>
  arrange(pos_index) |>
  pull(logit_tau)


# prep data into list for Stan
stan_data <- list(
  N = nrow(stan_df)
  , N_players = max(stan_df$player_index)
  , N_pos = max(stan_df$pos_index)
  
  , player_id = stan_df$player_index
  , pos_id = stan_df |>
    distinct(player_index, pos_index) |>
    arrange(player_index) |>
    pull(pos_index)
  
  , career_season = stan_df$career_season
  , weight = stan_df$weight_lb
  , logit_tau_prior_mean = tau_prior_mean_unbounded
  , alpha_prior_mean = alpha_prior_vec
)

# weight_mod <- cmdstan_model(
#   "weight_model.stan"
# )
weight_mod <- cmdstan_model("weight_model_tau.stan")


fit <- weight_mod$sample(
  data = stan_data
  , seed = 202
  , chains = 4
  , parallel_chains = 4
  , iter_warmup = 3000
  , iter_sampling = 3000
  , adapt_delta = 0.995
  , max_treedepth = 13
)

# convergence diagnostics ----
fit$diagnostic_summary()

# Check for any problematic parameters
fit$summary() |>
  filter(rhat > 1.01 | ess_bulk < 400)

# Focused check on key parameters
fit$summary(variables = c("mu_logit_tau", "sigma_log_tau", "sigma_y")) |>
  select(variable, mean, sd, rhat, ess_bulk, ess_tail)

# posterior predictive check ----
y_rep <- fit$draws("weight_rep", format = "matrix")
ppc_dens_overlay(stan_df$weight_lb, y_rep[1:100, ]) +
  labs(title = "Posterior Predictive Check: Does the model capture the data distribution?")

# Parameter relationships ----
# Check for pathological correlations
bayesplot::mcmc_pairs(
  fit$draws(format = "draws_df"),
  pars = c("gamma[1]", "tau[1]", "alpha[1]"),
  off_diag_fun = "hex"
)

# EXTRACT----
# Position-level summaries ----
pos_summary <- fit$summary(variables = c("mu_alpha", "mu_gamma", "mu_logit_tau")) |>
  mutate(
    param = str_extract(variable, "^[^\\[]+"),
    pos_index = as.integer(str_extract(variable, "\\d+"))
  ) |>
  left_join(pos_lookup, by = "pos_index") |>
  select(position, param, mean, median, sd, q5, q95)

# Player-level tau summaries ----
tau_summary <- fit$summary("tau") |>
  mutate(player_index = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(player_lookup, by = "player_index") |>
  left_join(stan_df |> distinct(player_index, pos_index), by = "player_index") |>
  left_join(pos_lookup, by = "pos_index")

# Position-level tau summaries ----
tau_by_position <- tau_summary |>
  group_by(position) |>
  summarise(
    median_tau = median(median),
    mean_tau = mean(mean),
    p25 = quantile(median, 0.25),
    p75 = quantile(median, 0.75),
    .groups = "drop"
  )

tau_by_position

# let's compare our model to our grid search

comparison <- tau_df |>
  left_join(
    tau_summary |> select(player_id, bayes_median = median, bayes_mean = mean),
    by = "player_id"
  )

# Summary statistics
comparison |>
  group_by(position) |> 
  summarise(
    correlation = cor(steady_season, bayes_median, use = "complete.obs"),
    mean_abs_diff = mean(abs(bayes_median - steady_season), na.rm = TRUE),
    median_abs_diff = median(abs(bayes_median - steady_season), na.rm = TRUE)
    , mean_tau = mean(bayes_median),
    , steady_season = mean(steady_season)
  )

# viz
ggplot(comparison, aes(x = steady_season, y = bayes_median)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "maroon") +
  coord_cartesian(xlim = c(1, 10), ylim = c(1, 10)) +
  labs(
    x = "Grid Search tau",
    y = "Bayesian tau (posterior median)",
    title = "Model Validation: Bayesian vs Grid Search Estimates",
    subtitle = "Points near the line indicate agreement"
  ) +
  theme_minimal()


# time to maturity by position ----
tau_draws <- fit$draws("tau", format = "draws_df") |>
  pivot_longer(
    cols = starts_with("tau"),
    names_to = "variable",
    values_to = "tau"
  ) |>
  mutate(player_index = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(stan_df |> distinct(player_index, pos_index), by = "player_index") |>
  left_join(pos_lookup, by = "pos_index")

tau_draws |>
  ggplot(aes(x = tau, y = position, fill = position)) +
  stat_density_ridges(
    quantile_lines = TRUE,
    quantiles = c(0.25, 0.5, 0.75),
    alpha = 0.7
  ) +
  scale_fill_viridis_d() +
  labs(
    x = "Seasons to 95% Steady State Weight",
    y = NULL,
    title = "When Do NBA Players Reach Their Steady State Weight?",
    subtitle = "Posterior distributions by position"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Get player-level medians
player_tau <- tau_summary |>
  select(player_index, pos_index, player_median = median) |>
  left_join(pos_lookup, by = "pos_index") |> 
  mutate(position = case_when(position == "C" ~ "Center"
                              , position == "F" ~ "Forward"
                              , position == "G" ~ "Guard")
  )

# Get position means
pos_tau_summary <- fit$summary("mu_logit_tau") |>
  mutate(
    pos_index = as.integer(str_extract(variable, "\\d+"))
  ) |>
  left_join(pos_lookup, by = "pos_index") |>
  mutate(
    across(c(mean, median, q5, q95), 
           ~1 + 5 * plogis(.x), 
           .names = "tau_{.col}")
  ) |> 
  mutate(position = case_when(position == "C" ~ "Center"
                              , position == "F" ~ "Forward"
                              , position == "G" ~ "Guard")
  )

p2 <- ggplot() +
  geom_quasirandom(
    data = player_tau,
    aes(x = player_median, y = position, color = player_median),
    alpha = 0.25,
    size = 4
  ) +
  geom_pointrange(
    data = pos_tau_summary,
    aes(x = tau_median, y = position, xmin = tau_q5, xmax = tau_q95),
    color = "maroon",  
    size = 1,
    fatten = 2,
    linewidth = 1.5
  ) +
  scale_x_continuous(breaks = 1:6) +
  usaidplot::usaid_plot(data_type = "continuous") +
  scale_color_viridis_c(option = "cividis") + 
  coord_cartesian(xlim = c(0.8, 6.2)) +
  labs(
    x = "Seasons to 95% Steady State Weight",
    y = NULL,
    title = "NBA Player Physical Development by Position",
    subtitle = "The maroon point with the line shows the position median (with 90% credible intervals)\nIndividual dots are player estimates after controlling for year 1 weight"
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  theme(panel.grid.major.y = element_blank())

ggsave("model_results_for_weight.png", p2, height = 10, width = 16, dpi = 300)

# Individual player trajectories ----
# Extract posterior means efficiently from a random group
player_params <- fit$summary(c("alpha", "gamma", "tau")) |>
  mutate(
    param = str_extract(variable, "^[^\\[]+"),
    player_index = as.integer(str_extract(variable, "\\d+"))
  ) |>
  select(player_index, param, mean) |>
  pivot_wider(names_from = param, values_from = mean) |>
  mutate(kappa = log(20) / tau)

# Sample 16 players for visualization
sample_players <- sample(unique(stan_df$player_index), 16)

# Create prediction grid
pred_grid <- expand_grid(
  player_index = sample_players,
  career_season = seq(0, 15, by = 0.1)
) |>
  left_join(player_params, by = "player_index") |>
  mutate(
    weight_pred = alpha + gamma * (1 - exp(-kappa * career_season))
  ) |>
  left_join(player_lookup, by = "player_index") |>
  left_join(
    stan_df |> distinct(player_id, player_name = player_id),  # Add actual names if you have them
    by = "player_id"
  )

# Actual data for these players
actual_data <- stan_df |>
  filter(player_index %in% sample_players) |>
  left_join(player_lookup, by = "player_index")

# Plot
ggplot() +
  geom_line(data = pred_grid, aes(x = career_season, y = weight_pred), 
            color = "steelblue", linewidth = 1) +
  geom_point(data = actual_data, aes(x = career_season, y = weight_lb), 
             alpha = 0.6, size = 1.5) +
  facet_wrap(~player_index, scales = "free_y", ncol = 4) +
  labs(
    x = "Career Season",
    y = "Weight (lbs)",
    title = "Individual Player Weight Trajectories",
    subtitle = "Blue line: model fit | Points: observed data"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))


# side quest- should I include height in the model?----
# check the structure of column names
colnames(post)[1:20]  

# robust extraction of posterior means
# This handles the column naming 
post_summary <- post |>
  as_draws_df() |>
  summarise(across(everything(), mean)) |>
  select(starts_with("alpha"), starts_with("gamma"), starts_with("kappa"))

# Check what we got
glimpse(post_summary)

#Convert to long format and extract player indices
post_means_clean <- post_summary |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "estimate"
  ) |>
  # Extract parameter name and index
  mutate(
    parameter = str_remove(variable, "\\[.*\\]"),
    player_index = as.integer(str_extract(variable, "(?<=\\[)\\d+(?=\\])"))
  ) |>
  filter(!is.na(player_index)) |>
  select(parameter, player_index, estimate) |>
  pivot_wider(names_from = parameter, values_from = estimate)

# Check if this worked
head(post_means_clean)
nrow(post_means_clean)  # Should equal N_players

# Step 4: Verify player indices match
cat("Player indices in post_means:", range(post_means_clean$player_index), "\n")
cat("Player indices in stan_df:", range(stan_df$player_index), "\n")

# join and compute residuals
fitted_weights <- stan_df |>
  left_join(post_means_clean, by = "player_index") |>
  mutate(
    weight_pred = alpha + gamma * (1 - exp(-kappa * career_season)),
    residual = weight_lb - weight_pred
  )

# Check for NAs
cat("NAs in weight_pred:", sum(is.na(fitted_weights$weight_pred)), "\n")
cat("NAs in residual:", sum(is.na(fitted_weights$residual)), "\n")

# If there are NAs, investigate
if(sum(is.na(fitted_weights$weight_pred)) > 0) {
  fitted_weights |>
    filter(is.na(weight_pred)) |>
    select(player_index, alpha, gamma, kappa, career_season) |>
    head()
}

# create summary for regression
height_residual_check <- fitted_weights |>
  filter(!is.na(residual), !is.na(height_in)) |>  # Explicit NA filtering
  group_by(player_index) |>
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    mean_height = mean(height_in, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) |>
  filter(!is.na(mean_residual), !is.na(mean_height))


# Now plot and regress
if(nrow(height_residual_check) > 0) {
  # Plot
  p <- ggplot(height_residual_check, aes(x = mean_height, y = mean_residual)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "maroon") +
    labs(
      title = "Does height predict model residuals?",
      x = "Player Height (inches)",
      y = "Mean Residual (lbs)"
    ) +
    theme_minimal()

  print(p)
  
  # Regression
  height_lm <- lm(mean_residual ~ mean_height, data = height_residual_check)
  print(summary(height_lm))
  
  # Interpretation
  coef_height <- coef(height_lm)[2]
  p_value <- summary(height_lm)$coefficients[2, 4]
  r_squared <- summary(height_lm)$r.squared
  
  cat(sprintf("Slope: %.3f lbs per inch (p = %.4f)\n", coef_height, p_value))
  cat(sprintf("R²: %.3f\n", r_squared))
  
  if(p_value < 0.01 && r_squared > 0.05) {
    cat("\nHeight appears to matter - consider including it in your model\n")
  } else {
    cat("\nHeight doesn't strongly predict residuals - current model is fine\n")
  }
  
} else {
  cat("ERROR: No valid cases for regression. Investigate the joins above.\n")
}


# Wizards players projections
current_wizards <- nba_commonteamroster(
  season = "2025-26",
  team_id = "1610612764" 
)$CommonTeamRoster

wizards_roster <- current_wizards |>
  transmute(
    player_id = as.character(PLAYER_ID),
    player_name = PLAYER,
    position = POSITION,
    height = HEIGHT,
    weight = WEIGHT,
    age = AGE
  )

# Function to project or report steady state for one player
project_wizards_player <- function(player_id_input) {
  
  # Get player index
  player_idx <- player_lookup |>
    filter(player_id == player_id_input) |>
    pull(player_index)
  
  if(length(player_idx) == 0) {
    return(tibble(
      player_id = player_id_input,
      status = "Not in model",
      steady_state_weight = NA,
      ss_q5 = NA,
      ss_q95 = NA,
      tau_median = NA,
      current_season = NA,
      seasons_remaining = NA
    ))
  }
  
  # Get current season for this player
  current_season <- stan_df |>
    filter(player_id == player_id_input) |>
    summarise(max_season = max(career_season)) |>
    pull(max_season)
  
  # Get posterior estimates
  player_params <- fit$summary(c(
    paste0("alpha[", player_idx, "]"),
    paste0("gamma[", player_idx, "]"),
    paste0("tau[", player_idx, "]")
  )) |>
    mutate(param = str_extract(variable, "^[^\\[]+")) |>
    select(param, median, q5, q95) |>
    pivot_wider(names_from = param, values_from = c(median, q5, q95))
  
  # Get current observed weight
  current_weight <- stan_df |>
    filter(player_id == player_id_input) |>
    arrange(desc(career_season)) |>
    slice(1) |>
    pull(weight_lb)
  
  # Calculate steady state
  steady_state <- player_params$median_alpha + player_params$median_gamma
  ss_q5 <- player_params$q5_alpha + player_params$q5_gamma
  ss_q95 <- player_params$q95_alpha + player_params$q95_gamma
  
  # Determine status
  already_reached <- current_season >= player_params$median_tau
  
  tibble(
    player_id = player_id_input,
    status = if_else(already_reached, "At steady state", "Still developing"),
    current_season = current_season,
    current_weight = current_weight,
    steady_state_weight = round(steady_state, 1),
    ss_q5 = round(ss_q5, 1),
    ss_q95 = round(ss_q95, 1),
    weight_change_projected = round(steady_state - current_weight, 1),
    tau_median = round(player_params$median_tau, 1),
    tau_q5 = round(player_params$q5_tau, 1),
    tau_q95 = round(player_params$q95_tau, 1),
    seasons_remaining = round(pmax(0, player_params$median_tau - current_season), 1),
    pct_complete = round(
      (current_weight - player_params$median_alpha) / 
        player_params$median_gamma * 100, 1
    )
  )
}

# Apply to all Wizards in the model
wizards_projections <- wizards_in_model |>
  pull(player_id) |>
  map_dfr(project_wizards_player) |>
  left_join(wizards_roster, by = "player_id") |>
  arrange(desc(seasons_remaining))


# Summary table for presentation
wizards_summary <- wizards_projections |>
  filter(status != "Not in model") |>
  transmute(
    Player = player_name,
    Position = position,
    `Current Season` = current_season,
    `Current Weight` = current_weight,
    `Projected Steady State` = paste0(
      steady_state_weight, " lbs [", ss_q5, "-", ss_q95, "]"
    ),
    `Change Expected` = paste0(
      ifelse(weight_change_projected > 0, "+", ""), 
      weight_change_projected, " lbs"
    ),
    `Seasons to 95%` = paste0(
      tau_median, " [", tau_q5, "-", tau_q95, "]"
    ),
    `Seasons Remaining` = seasons_remaining,
    `Development %` = paste0(pct_complete, "%"),
    Status = status
  ) |>
  arrange(desc(`Seasons Remaining`))

wizards_summary |> gt::gt()

# Trajectory plot for all Wizards
wizards_trajectories <- wizards_projections |>
  filter(status != "Not in model") |>
  crossing(future_season = seq(0, 5, by = 0.1)) |>
  left_join(
    # Get alpha, gamma, tau for each player
    wizards_in_model |>
      pull(player_index) |>
      map_dfr(~{
        idx <- .x
        params <- fit$summary(c(
          paste0("alpha[", idx, "]"),
          paste0("gamma[", idx, "]"),
          paste0("tau[", idx, "]")
        )) |>
          mutate(param = str_extract(variable, "^[^\\[]+")) |>
          select(param, median) |>
          pivot_wider(names_from = param, values_from = median) |>
          mutate(
            player_index = idx,
            kappa = log(20) / tau
          )
      }) |>
      left_join(player_lookup, by = "player_index"),
    by = "player_id"
  ) |>
  mutate(
    projected_weight = alpha + gamma * (1 - exp(-kappa * future_season))
  )

# Plot all trajectories
wizards_viz <- wizards_projections |>
  mutate(
    player_label = paste0(player_name, " (", position, ")"),
    # Create a label for seasons remaining
    seasons_label = case_when(
      seasons_remaining == 0 ~ "At steady state",
      seasons_remaining < 1 ~ "Likely still growing",
      TRUE ~ "Likely still growing"
    ),
    # Label for weight change
    weight_change_label = case_when(
      abs(weight_change_projected) < 0.5 ~ "0 lbs",
      weight_change_projected > 0 ~ paste0("+", round(weight_change_projected, 0), " lbs"),
      TRUE ~ paste0(round(weight_change_projected, 0), " lbs")
    )
  ) |>
  arrange(seasons_remaining, player_name)

p_proj <- ggplot(wizards_viz, aes(y = reorder(player_label, seasons_remaining))) +
  # Uncertainty interval for projected weight
  geom_segment(
    aes(x = ss_q5, xend = ss_q95, yend = player_label),
    color = "lightblue",
    linewidth = 2,
    alpha = 0.5
  ) +
  # Line connecting current to projected
  geom_segment(
    aes(x = current_weight, xend = steady_state_weight, yend = player_label),
    color = "gray60",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  # Current weight point
  geom_point(
    aes(x = current_weight),
    size = 4,
    color = "maroon",
    shape = 16
  ) +
  # Projected weight point
  geom_point(
    aes(x = steady_state_weight),
    size = 4,
    color = "steelblue",
    shape = 18  # Diamond
  ) +
  # Weight change label near diamond
  geom_text(
    aes(x = steady_state_weight, label = weight_change_label),
    hjust = -0.3,
    vjust = -0.7,
    size = 3,
    color = "gray30"
  ) +
  # Add seasons remaining as text
  # geom_text(
    # aes(x = max(ss_q95) + 2, label = seasons_label),
    # size = 4,
    # hjust = 0,
    # color = "gray30"
  # ) +
  scale_x_continuous(
    limits = c(min(wizards_viz$current_weight) - 30, 
               max(wizards_viz$ss_q95) + 10)
    , breaks = c(140, 180, 220, 260)
  ) +
  labs(
    x = "Weight (lbs)",
    y = NULL,
    title = "<span style='color:maroon;'>Current</span> and <span style='color:steelblue;'>Projected</span> Weights for the 2025-26 Wizards",
    subtitle = "The lightblue lines show the 90% credible range around the projections.",
    caption = "wizardspoints.substack.com"
  ) +
 usaidplot::usaid_plot() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_markdown(),
    
  )

p_proj + ggview::canvas(height = 10, width = 16)

ggsave("growth_projections.png", p_proj, height = 10, width = 16, dpi = 300)