
// Hierarchical Defensive Impact Model for Wizards Players
// Estimates each player's baseline defensive ability and game-over-game trend
// with hierarchical pooling across players
//
// Lower theta = better defender (defensive rating scale)
// Noise scales as sigma_y / sqrt(minutes / mean_minutes):
//   short-minute observations are less reliable than long ones.
//   This treats minutes as a precision weight, not a predictor,
//   which is appropriate because defensive rating is already
//   per-100-possessions normalized.
//
// Includes log_lik for LOO-CV and posterior predictive checks
//

data {
  int<lower=1> N;                    // total observations (player-games)
  int<lower=1> N_players;           // number of players

  array[N] int<lower=1, upper=N_players> player;  // player index
  vector[N] game_num;               // standardized game number within season
  vector<lower=0>[N] minutes_raw;   // raw minutes played per game
  real<lower=0> mean_minutes;       // reference for scaling: sigma_y is at this minute level
  vector[N] def_rating;             // observed defensive rating
}

parameters {
  // player-level baseline defensive rating (lower = better)
  vector[N_players] theta_raw;

  // player-level trend over the season
  vector[N_players] beta_trend_raw;

  // hyperparameters
  real mu_theta;
  real<lower=0> sigma_theta;
  real mu_trend;
  real<lower=0> sigma_trend;

  // residual SD (at mean_minutes level)
  real<lower=0> sigma_y;
}

transformed parameters {
  // non-centered parameterization
  vector[N_players] theta = mu_theta + sigma_theta * theta_raw;
  vector[N_players] beta_trend = mu_trend + sigma_trend * beta_trend_raw;
}

model {
  // hyperpriors
  mu_theta ~ normal(112, 5);
  sigma_theta ~ normal(0, 5);
  mu_trend ~ normal(0, 0.5);
  sigma_trend ~ exponential(2);

  // residual noise prior
  sigma_y ~ normal(0, 5);

  // non-centered random effects
  theta_raw ~ std_normal();
  beta_trend_raw ~ std_normal();

  // likelihood with precision-weighted noise
  {
    vector[N] mu;
    vector[N] sigma_n;
    for (n in 1:N) {
      mu[n]      = theta[player[n]] + beta_trend[player[n]] * game_num[n];
      sigma_n[n] = sigma_y / sqrt(minutes_raw[n] / mean_minutes);
    }
    def_rating ~ normal(mu, sigma_n);
  }
}

generated quantities {
  vector[N] log_lik;
  vector[N] def_rep;

  for (n in 1:N) {
    real mu_n      = theta[player[n]] + beta_trend[player[n]] * game_num[n];
    real sigma_n   = sigma_y / sqrt(minutes_raw[n] / mean_minutes);
    log_lik[n]     = normal_lpdf(def_rating[n] | mu_n, sigma_n);
    def_rep[n]     = normal_rng(mu_n, sigma_n);
  }
}


