data {
  int<lower=0> n;             // number of observations == number of innings (or, games)
  int<lower=0> n_p;           // number of parks
  int<lower=0> n_oy;          // number of offensive-team-seasons
  int<lower=0> n_dy;          // number of defensive-team-seasons

  int<lower=0> y[n];          // full outcome vector
  int<lower=0> y_ro[n];       // road-offense outcome vector
  int<lower=0> y_rd[n];       // road-defense outcome vector
  int<lower=0> P[n];          // park vector
  int<lower=0> OY[n];         // offensive-team-season vector
  int<lower=0> DY[n];         // defensive-team-season vector
}
parameters {
  real mu_p;
  real mu_o;
  real mu_d;
  real<lower=0> sigma_p;
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real alpha;
  
  vector[n_p-1] beta_p_raw;
  vector[n_oy-1] beta_oy_raw;
  vector[n_dy-1] beta_dy_raw;
}
transformed parameters {
  vector[n] eta; 
  vector[n] lambda;
  //// for identifiability
  vector[n_p] beta_p;
  vector[n_oy] beta_oy;
  vector[n_dy] beta_dy;
  
  beta_p   = append_row(0, beta_p_raw);
  beta_oy = append_row(0, beta_oy_raw);
  beta_dy = append_row(0, beta_dy_raw);

  for (i in 1:n) {
    eta[i] = alpha + beta_p[P[i]] + beta_oy[OY[i]] + beta_dy[DY[i]];
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
  
  for (i in 1:n) {
    if (P[i] != 1) {
      beta_p_raw[P[i]-1]  ~ normal(mu_p, sigma_p);
    }
    if (OY[i] != 1) {
      beta_oy_raw[OY[i]-1]  ~ normal(mu_o, sigma_o);
    }
    if (DY[i] != 1) {
      beta_dy_raw[DY[i]-1]  ~ normal(mu_d, sigma_d);
    }
  }
  y ~ poisson(lambda);
  y_ro ~ poisson(lambda);
  y_rd ~ poisson(lambda);
}



