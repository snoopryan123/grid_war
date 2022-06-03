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
  
  real<lower=0> alpha;

  vector<lower=0>[n_p] beta_p;
  vector<lower=0>[n] beta_oyg;
  vector<lower=0>[n] beta_dyg;
  vector<lower=0>[n_oy] theta_oy;
  vector<lower=0>[n_dy] theta_dy;
}
transformed parameters {
  vector[n] eta;    
  // eta = log(lambda), y ~ poisson(lambda)
  for (i in 1:n) {
    eta[i] = log(alpha + beta_p[P[i]] + beta_oyg[OYG[i]] + beta_dyg[DYG[i]]);
  }
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
    theta_oy[OY[i]]  ~ normal(mu_o, sigma_o);
    theta_dy[DY[i]]  ~ normal(mu_d, sigma_d);
    beta_p[P[i]]     ~ normal(mu_p, sigma_p);
    beta_oyg[OYG[i]] ~ normal(theta_oy[OY[i]], tau_o);
    beta_dyg[DYG[i]] ~ normal(theta_dy[DY[i]], tau_d);
  }
  y ~ poisson_log(eta);
}



