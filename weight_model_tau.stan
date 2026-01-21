data {
  int<lower=1> N;                     // observations
  int<lower=1> N_players;             // players
  int<lower=1> N_pos;                 // positions

  array[N] int<lower=1, upper=N_players> player_id;
  array[N_players] int<lower=1, upper=N_pos> pos_id;

  vector[N] career_season;            // season index
  vector[N] weight;                   // weight

  vector[N_pos] alpha_prior_mean;     // rookie weight by position
  vector[N_pos] logit_tau_prior_mean; 

}

parameters {
  // Position-level means
  vector[N_pos] mu_alpha;
  vector[N_pos] mu_gamma;
  vector[N_pos] mu_logit_tau;

  // Player deviations
  vector[N_players] alpha_raw;
  vector[N_players] gamma_raw;
  vector[N_players] log_tau_raw;

  // Hierarchical scales
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_gamma;
  real<lower=0> sigma_log_tau;

  real<lower=0> sigma_y;
}

transformed parameters {
  vector[N_players] alpha;
  vector[N_players] gamma;
  vector[N_players] tau;
  vector[N_players] kappa;
  for (i in 1:N_players) {
    alpha[i] = mu_alpha[pos_id[i]] + sigma_alpha * alpha_raw[i];
    gamma[i] = mu_gamma[pos_id[i]] + sigma_gamma * gamma_raw[i];
    
    // Bound tau between 1 and 10 seasons (tighter, more realistic)
    tau[i] = 1 + 5 * inv_logit(
      mu_logit_tau[pos_id[i]] + sigma_log_tau * log_tau_raw[i]
    );
    
    kappa[i] = log(20) / tau[i];
  }
}

model {
  // ----------------------------
  // POSITION-LEVEL PRIORS
  // ----------------------------

  mu_alpha ~ normal(alpha_prior_mean, 10);
  mu_gamma ~ normal(0, 8);
  mu_logit_tau ~ normal(logit_tau_prior_mean, 0.5); 

  // ----------------------------
  // PLAYER DEVIATIONS
  // ----------------------------

  alpha_raw ~ normal(0, 1);
  gamma_raw ~ normal(0, 1);
  log_tau_raw ~ normal(0, 1);

  // ----------------------------
  // HYPERPRIORS
  // ----------------------------

  sigma_alpha ~ normal(0, 8);
  sigma_gamma ~ normal(0, 6);
  sigma_log_tau ~ normal(0, 0.5);  // this will regularize our estimates

  sigma_y ~ normal(0, 4);

  // ----------------------------
  // LIKELIHOOD
  // ----------------------------

    vector[N] mu;
  for (n in 1:N) {
    int i = player_id[n];
    mu[n] = alpha[i] + gamma[i] * (1 - exp(-kappa[i] * career_season[n]));
  }
  weight ~ normal(mu, sigma_y);
}


generated quantities {
   vector[N_players] weight_steady_state;
  vector[N_players] t95;
  vector[N] weight_rep;  // Posterior predictive samples
  
  for (i in 1:N_players) {
    weight_steady_state[i] = alpha[i] + gamma[i];
    t95[i] = tau[i];
  }
  
  // Simulate replicated data for posterior predictive checks
  for (n in 1:N) {
    int i = player_id[n];
    weight_rep[n] = normal_rng(
      alpha[i] + gamma[i] * (1 - exp(-kappa[i] * career_season[n])),
      sigma_y
    );
  }
}
