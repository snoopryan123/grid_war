data {
  int<lower=0> n;             // number of observations == number of innings (or, games)
  int<lower=0> n_p;           // number of parks
  int<lower=0> n_oy;          // number of offensive-team-seasons
  int<lower=0> n_dy;          // number of defensive-team-seasons
  
  int<lower=0> y[n];          // outcome vector
  int<lower=0> P[n];          // park vector
  int<lower=0> OY[n];         // offensive-team-season vector
  int<lower=0> DY[n];         // defensive-team-season vector
  int<lower=0> OYG[n];        // offensive-team-season-game vector
  int<lower=0> DYG[n];        // defensive-team-season-game vector
}
parameters {
  real mu_p;
  real mu_o;
  real mu_d;
  real<lower=0> sigma_p;
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real<lower=0> tau_o;
  real<lower=0> tau_d;
  real alpha;
  
  vector[n_p-1] beta_p_raw;
  vector[n-1] beta_oyg_raw;
  vector[n-1] beta_dyg_raw;
  vector[n_oy-1] theta_oy_raw;
  vector[n_dy-1] theta_dy_raw;
}
transformed parameters {
  vector[n] eta; 
  vector[n] lambda;
  //// for identifiability
  vector[n_p] beta_p;
  vector[n] beta_oyg;
  vector[n] beta_dyg;
  vector[n_oy] theta_oy;
  vector[n_dy] theta_dy;
  
  beta_p   = append_row(0, beta_p_raw);
  beta_oyg = append_row(0, beta_oyg_raw);
  beta_dyg = append_row(0, beta_dyg_raw);
  theta_oy = append_row(0, theta_oy_raw);
  theta_dy = append_row(0, theta_dy_raw);

  for (i in 1:n) {
    eta[i] = alpha + beta_p[P[i]] + beta_oyg[OYG[i]] + beta_dyg[DYG[i]];
  }
  lambda = exp(eta);
}
model {
  alpha ~ normal(0,2);
  mu_p ~ normal(0,2);
  mu_o ~ normal(0,2);
  mu_d ~ normal(0,2);
  sigma_p ~ inv_gamma(0.5, 0.5);
  sigma_o ~ inv_gamma(0.5, 0.5);
  sigma_d ~ inv_gamma(0.5, 0.5);
  tau_o ~ inv_gamma(0.5, 0.5);
  tau_d ~ inv_gamma(0.5, 0.5);
  
  for (i in 1:n) {
    if (OY[i] != 1) {
      theta_oy_raw[OY[i]-1]  ~ normal(mu_o, sigma_o);
    }
    if (DY[i] != 1) {
      theta_dy_raw[DY[i]-1]  ~ normal(mu_d, sigma_d);
    }
    if (P[i] != 1) {
      beta_p_raw[P[i]-1]  ~ normal(mu_p, sigma_p);
    }
    if (OYG[i] != 1) {
      beta_oyg_raw[OYG[i]-1]  ~ normal(theta_oy[OY[i]], tau_o);
    }
    if (DYG[i] != 1) {
      beta_dyg_raw[DYG[i]-1]  ~ normal(theta_dy[DY[i]], tau_d);
    }
  }
  y ~ poisson(lambda);
}



