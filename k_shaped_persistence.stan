// k_shaped_persistence.stan
// Hierarchical logistic regression: P(outcome) ~ prior_wins_z + delta_wins_z
// delta_wins_z captures win trajectory (momentum) independent of level
// Non-centered parameterization for team and season random intercepts

data {
  int<lower=0> N;
  int<lower=0> N_teams;
  int<lower=0> N_seasons;
  array[N] int<lower=0, upper=1> y;
  vector[N] prior_wins_z;
  vector[N] delta_wins_z;   // change in wins from t-2 to t-1, standardized
  array[N] int<lower=1, upper=N_teams> team_id;
  array[N] int<lower=1, upper=N_seasons> season_id;
}

parameters {
  real alpha;
  real beta;
  real beta_delta;           // momentum effect
  real<lower=0> sigma_team;
  real<lower=0> sigma_season;
  vector[N_teams] z_team;
  vector[N_seasons] z_season;
}

transformed parameters {
  vector[N_teams]   team_re   = z_team   * sigma_team;
  vector[N_seasons] season_re = z_season * sigma_season;
}

model {
  alpha        ~ normal(0, 2);
  beta         ~ normal(0, 2);
  beta_delta   ~ normal(0, 2);
  sigma_team   ~ exponential(1);
  sigma_season ~ exponential(1);
  z_team       ~ std_normal();
  z_season     ~ std_normal();

  vector[N] lp;
  for (n in 1:N)
    lp[n] = alpha
            + beta       * prior_wins_z[n]
            + beta_delta * delta_wins_z[n]
            + team_re[team_id[n]]
            + season_re[season_id[n]];

  y ~ bernoulli_logit(lp);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N)
    log_lik[n] = bernoulli_logit_lpmf(
      y[n] | alpha
             + beta       * prior_wins_z[n]
             + beta_delta * delta_wins_z[n]
             + team_re[team_id[n]]
             + season_re[season_id[n]]
    );
}
