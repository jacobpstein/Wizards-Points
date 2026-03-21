// Bayesian Regularized Adjusted Plus-Minus (RAPM) for Wizards Players
// Estimates each player's per-minute impact on point differential
// Hierarchical priors provide ridge-like regularization
// Separate groups for Wizards vs opponent players
//
// Positive beta = positive impact (good)
// Includes log_lik for LOO-CV
//

data {
  int<lower=1> N;                    // number of stints
  int<lower=1> N_wiz;               // number of Wizards players
  int<lower=1> N_opp;               // number of opponent players

  // sparse representation: which 5 players from each team per stint
  array[N, 5] int<lower=1, upper=N_wiz> wiz_on;
  array[N, 5] int<lower=1, upper=N_opp> opp_on;

  vector[N] y;                       // point differential per minute
  vector<lower=0>[N] weights;        // stint duration in minutes
  vector[N] home;                    // +1 if WAS home, -1 if away
}

parameters {
  // player effects (non-centered)
  vector[N_wiz] beta_wiz_raw;
  vector[N_opp] beta_opp_raw;

  // hierarchical hyperparameters
  real mu_wiz;
  real<lower=0> tau_wiz;
  real mu_opp;
  real<lower=0> tau_opp;

  // home court advantage
  real alpha_home;

  // residual SD
  real<lower=0> sigma_y;
}

transformed parameters {
  vector[N_wiz] beta_wiz = mu_wiz + tau_wiz * beta_wiz_raw;
  vector[N_opp] beta_opp = mu_opp + tau_opp * beta_opp_raw;
}

model {
  // hyperpriors
  mu_wiz ~ normal(0, 2);
  tau_wiz ~ normal(0, 2);
  mu_opp ~ normal(0, 2);
  tau_opp ~ normal(0, 2);

  // non-centered random effects
  beta_wiz_raw ~ std_normal();
  beta_opp_raw ~ std_normal();

  // home court
  alpha_home ~ normal(0, 3);

  // residual
  sigma_y ~ normal(0, 10);

  // likelihood (weighted by stint duration via precision scaling)
  for (s in 1:N) {
    real mu_s = alpha_home * home[s];
    for (k in 1:5)
      mu_s += beta_wiz[wiz_on[s, k]];
    for (k in 1:5)
      mu_s -= beta_opp[opp_on[s, k]];

    target += normal_lpdf(y[s] | mu_s, sigma_y / sqrt(weights[s]));
  }
}

generated quantities {
  vector[N] log_lik;

  for (s in 1:N) {
    real mu_s = alpha_home * home[s];
    for (k in 1:5)
      mu_s += beta_wiz[wiz_on[s, k]];
    for (k in 1:5)
      mu_s -= beta_opp[opp_on[s, k]];

    log_lik[s] = normal_lpdf(y[s] | mu_s, sigma_y / sqrt(weights[s]));
  }
}
