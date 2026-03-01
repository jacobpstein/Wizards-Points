//
// Hierarchical model for veteran effect on young player development
// Outcome: year-over-year change in net rating (delta_net_rating)
// Key predictor: share of team minutes played by veterans (vet_min_share)
// Random effects: team, season, position, player
//
// Tinkering with Student-t or Gaussian likelihood via use_student_t flag
// Includes log_lik for LOO cross-validation
//

data {
  int<lower=1> N;                 // number of young player-seasons
  int<lower=1> N_teams;           // number of teams
  int<lower=1> N_seasons;         // number of seasons
  int<lower=1> N_positions;       // number of positions
  int<lower=1> N_players;         // number of unique players

  // grouping indices
  array[N] int<lower=1, upper=N_teams> team_id;
  array[N] int<lower=1, upper=N_seasons> season_id;
  array[N] int<lower=1, upper=N_positions> position_id;
  array[N] int<lower=1, upper=N_players> player_id;

  // outcome
  vector[N] delta_net_rating;

  // predictors
  vector[N] vet_min_share;
  vector[N] prev_net_rating;
  vector[N] career_season;
  vector[N] minutes;
  vector[N] games_played;
  array[N] int<lower=0, upper=1> first_round;
  array[N] int<lower=0, upper=1> second_round;
  vector[N] team_win_pct;

  // empirical prior inputs (scale controlled in my R code)
  real prior_intercept_mean;
  real<lower=0> prior_intercept_sd;
  real prior_vet_mean;
  real<lower=0> prior_vet_sd;
  real prior_prev_mean;
  real<lower=0> prior_prev_sd;
  real prior_career_mean;
  real<lower=0> prior_career_sd;

  // nu (degrees of freedom) prior hyperparameters
  real<lower=0> nu_prior_alpha;
  real<lower=0> nu_prior_beta;

  // likelihood switch: 1 = student-t, 0 = gaussian
  int<lower=0, upper=1> use_student_t;

  // specification switches
  int<lower=0, upper=1> use_team_re;
  int<lower=0, upper=1> use_win_pct;
}

parameters {
  // fixed effects
  real alpha;                      // intercept
  real beta_vet;                   // veteran exposure effect
  real beta_prev;                  // prior net rating (to assess regression to mean)
  real beta_career;                // career season effect
  real beta_min;                   // minutes played
  real beta_gp;                    // games played
  real beta_first;                 // first round pick
  real beta_second;                // second round pick
  real beta_win_pct;               // team win percentage

  // random effects (non-centered)
  vector[N_teams] z_team;
  vector[N_seasons] z_season;
  vector[N_positions] z_position;
  vector[N_players] z_player;

  // hierarchical scales
  real<lower=0> sigma_team;
  real<lower=0> sigma_season;
  real<lower=0> sigma_position;
  real<lower=0> sigma_player;

  // residual SD
  real<lower=0> sigma_y;

  // degrees of freedom (sampled even when use_student_t=0, just from prior)
  real<lower=2> nu;
}

transformed parameters {
  vector[N_teams] re_team = sigma_team * z_team;
  vector[N_seasons] re_season = sigma_season * z_season;
  vector[N_positions] re_position = sigma_position * z_position;
  vector[N_players] re_player = sigma_player * z_player;
}

model {
  // ----------------------------
  // PRIORS (empirically informed)
  // ----------------------------

  alpha ~ normal(prior_intercept_mean, prior_intercept_sd);
  beta_vet ~ normal(prior_vet_mean, prior_vet_sd);
  beta_prev ~ normal(prior_prev_mean, prior_prev_sd);
  beta_career ~ normal(prior_career_mean, prior_career_sd);

  // weakly informative for controls
  beta_min ~ normal(0, 2.5);
  beta_gp ~ normal(0, 2.5);
  beta_first ~ normal(0, 2.5);
  beta_second ~ normal(0, 2.5);
  beta_win_pct ~ normal(0, 2.5);

  // random effect priors (non-centered parameterization)
  z_team ~ std_normal();
  z_season ~ std_normal();
  z_position ~ std_normal();
  z_player ~ std_normal();

  // hyperpriors on scales
  sigma_team ~ normal(0, 3);
  sigma_season ~ normal(0, 3);
  sigma_position ~ normal(0, 3);
  sigma_player ~ normal(0, 3);

  // residual
  sigma_y ~ normal(0, 5);

  // degrees of freedom
  nu ~ gamma(nu_prior_alpha, nu_prior_beta);

  // ----------------------------
  // LIKELIHOOD
  // ----------------------------

  {
    vector[N] mu;
    for (n in 1:N) {
      mu[n] = alpha
        + beta_vet * vet_min_share[n]
        + beta_prev * prev_net_rating[n]
        + beta_career * career_season[n]
        + beta_min * minutes[n]
        + beta_gp * games_played[n]
        + beta_first * first_round[n]
        + beta_second * second_round[n]
        + re_season[season_id[n]]
        + re_position[position_id[n]]
        + re_player[player_id[n]];
      if (use_win_pct == 1)
        mu[n] += beta_win_pct * team_win_pct[n];
      if (use_team_re == 1)
        mu[n] += re_team[team_id[n]];
    }

    if (use_student_t == 1) {
      delta_net_rating ~ student_t(nu, mu, sigma_y);
    } else {
      delta_net_rating ~ normal(mu, sigma_y);
    }
  }
}

generated quantities {
  vector[N] log_lik;     // pointwise log-likelihood for LOO
  vector[N] delta_rep;   // posterior predictive samples

  for (n in 1:N) {
    real mu_n = alpha
      + beta_vet * vet_min_share[n]
      + beta_prev * prev_net_rating[n]
      + beta_career * career_season[n]
      + beta_min * minutes[n]
      + beta_gp * games_played[n]
      + beta_first * first_round[n]
      + beta_second * second_round[n]
      + re_season[season_id[n]]
      + re_position[position_id[n]]
      + re_player[player_id[n]];
    if (use_win_pct == 1)
      mu_n += beta_win_pct * team_win_pct[n];
    if (use_team_re == 1)
      mu_n += re_team[team_id[n]];

    if (use_student_t == 1) {
      log_lik[n] = student_t_lpdf(delta_net_rating[n] | nu, mu_n, sigma_y);
      delta_rep[n] = student_t_rng(nu, mu_n, sigma_y);
    } else {
      log_lik[n] = normal_lpdf(delta_net_rating[n] | mu_n, sigma_y);
      delta_rep[n] = normal_rng(mu_n, sigma_y);
    }
  }
}
