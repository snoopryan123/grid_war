data {
  int<lower=0> n;             // number of observations == number of games
  int<lower=0> n_p;           // number of parks
  int<lower=0> n_oy;          // number of offensive-team-seasons
  int<lower=0> n_dy;          // number of defensive-team-seasons
  //int<lower=0> n_oyg;         // number of offensive-team-season-games
  //int<lower=0> n_dyg;         // number of defensive-team-season-games

  int<lower=0> y[n];          // outcome vector
  int<lower=0> P[n];          // park vector
  int<lower=0> OY[n];         // offensive-team-season vector
  int<lower=0> DY[n];         // defensive-team-season vector
  int<lower=0> OYG[n];        // offensive-team-season-game vector
  int<lower=0> DYG[n];        // defensive-team-season-game vector
}
// transformed data {
// }
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
  real<lower=0> beta_p[n_p];
  real<lower=0> theta_oy[n_oy];
  real<lower=0> theta_dy[n_dy];
  real<lower=0> beta_oyg[n];
  real<lower=0> beta_dyg[n];
  // real alpha;
  // real beta_p[n_p];
  // real theta_oy[n_oy];
  // real theta_dy[n_dy];
  // real beta_oyg[n];
  // real beta_dyg[n];
  
  // real<lower=0> beta_p_raw[n_p-1];
  // real<lower=0> theta_oy_raw[n_oy-1];
  // real<lower=0> theta_dy_raw[n_dy-1];
  // real<lower=0> beta_oyg_raw[n-1];
  // real<lower=0> beta_dyg_raw[n-1];
}
transformed parameters {
  //// for identifiability
  // real<lower=0> beta_p[n_p];
  // real<lower=0> theta_oy[n_oy];
  // real<lower=0> theta_dy[n_dy];
  // real<lower=0> beta_oyg[n];
  // real<lower=0> beta_dyg[n];
  // beta_p   = append_row(0, beta_p_raw);
  //theta_oy = append_row(0, theta_oy_raw);
  // theta_dy = append_row(0, theta_dy_raw);
  // beta_oyg = append_row(0, beta_oyg_raw);
  // beta_dyg = append_row(0, beta_dyg_raw);

  // eta = log(lambda), y ~ poisson(lambda)
  vector[n] eta;               

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
  
  for (i in 2:n) {
    theta_oy[OY[i]]  ~ normal(mu_o, sigma_o);
    theta_dy[DY[i]]  ~ normal(mu_d, sigma_d);
    beta_p[P[i]]     ~ normal(mu_p, sigma_p);

    beta_oyg[OYG[i]] ~ normal(theta_oy[OY[i]], tau_o);
    beta_dyg[DYG[i]] ~ normal(theta_dy[DY[i]], tau_d);
    
    y[i] ~ poisson_log(eta[i]);
  }
}



