//
// This Stan program sets up a hierarchical model to estimate
// changes in three point shooting for the Wizards from 2024-25 to 2025-26
//


data {
  int<lower=1> N_obs;
  int<lower=1> N_players;

  int<lower=1, upper=N_players> player[N_obs];
  int<lower=0, upper=1> season[N_obs];     // 0 last, 1 this
  int<lower=0, upper=1> shot_type[N_obs];  // 0 FT, 1 3PT

  int<lower=0> attempts[N_obs];
  int<lower=0> makes[N_obs];

  vector[N_obs] minutes;
}

parameters {
  vector[N_players] theta_3_last;
  vector[N_players] theta_3_this;

  vector[N_players] theta_ft_last;
  vector[N_players] theta_ft_this;

  real mu_3;
  real mu_ft;

  real<lower=0> sigma_3;
  real<lower=0> sigma_ft;

  real alpha_3;
  real beta_ft_to_3;
  real beta_minutes;

  real<lower=0> sigma_evol_3;
}

model {
  // Priors
  mu_3 ~ normal(-1, 1);
  mu_ft ~ normal(0, 1);

  sigma_3 ~ exponential(2);
  sigma_ft ~ exponential(2);
  sigma_evol_3 ~ exponential(2);

  alpha_3 ~ normal(0, 0.3);
  beta_ft_to_3 ~ normal(0, 0.3);
  beta_minutes ~ normal(0, 0.2);

  // Last season skills
  theta_3_last ~ normal(mu_3, sigma_3);
  theta_ft_last ~ normal(mu_ft, sigma_ft);

  // FT evolution
  theta_ft_this ~ normal(theta_ft_last, sigma_ft);

  // 3PT evolution
  theta_3_this ~ normal(
    theta_3_last
    + alpha_3
    + beta_ft_to_3 .* (theta_ft_this - theta_ft_last)
  , sigma_evol_3
  );

  // Likelihood
  for (n in 1:N_obs) {
    real theta;

    if (shot_type[n] == 0) {
      theta = season[n] == 0
        ? theta_ft_last[player[n]]
        : theta_ft_this[player[n]];
    } else {
      theta = season[n] == 0
        ? theta_3_last[player[n]]
        : theta_3_this[player[n]];
    }

    makes[n] ~ binomial_logit(
      attempts[n]
    , theta + beta_minutes * minutes[n]
    );
  }
}

generated quantities {
  vector[N_players] p3_diff =
    inv_logit(theta_3_this) - inv_logit(theta_3_last);
}

