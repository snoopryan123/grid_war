library(tidyverse)
library(splines)
library(ggthemes)
library(latex2exp)
library(glmnet)
library(plotly)
library(cowplot)
library(lme4)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = "./plots/"
library(rstan)
if(!interactive()) pdf(NULL)
rstan_options(auto_write = TRUE)
##### uncomment these if working on my computer #####
# cores = 1
# NUM_ITS = 10
#### options(mc.cores = parallel::detectCores())
#####################################################
####### uncomment these if working on HPCC ##########
cores=strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
NUM_ITS = 1000 #FIXME

############
### data ###
############

war2 <- read_csv("../war2.csv") ### dataset

### runs scored thru 8 innings ~ park_effect + team_off_q + team_def_q + spline(time)
### time is a fixed effect, other parameters are random intercepts
park_df0 = war2 %>% 
  select(GAME_ID, YEAR, DAYS_SINCE_SZN_START, BAT_HOME_IND, INNING, HOME_TEAM_ID, AWAY_TEAM_ID, PARK, CUM_RUNS) %>% #,INN_RUNS) %>%
  # filter(ifelse(BAT_HOME_IND == 1, INNING <= 8, INNING <= 9)) %>%
  filter(INNING <= 8) %>%
  # group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  slice_tail() %>%
  ungroup() %>%
  mutate(OFF_TEAM_ID = ifelse(BAT_HOME_IND == 1, HOME_TEAM_ID, AWAY_TEAM_ID),
         DEF_TEAM_ID = ifelse(BAT_HOME_IND == 1, AWAY_TEAM_ID, HOME_TEAM_ID)) %>%
  arrange(BAT_HOME_IND, GAME_ID, INNING) 
park_df = park_df0 %>% left_join(park_df0 %>% group_by(PARK) %>% summarise(park.count = n()) %>% 
                                   arrange(park.count)) %>% filter(park.count > 300) %>% select(-park.count)
park_df[1:18,]

#####################################
### Hierarchical Simulation Setup ###
#####################################

sim_df1 = park_df %>% filter(2018 <= YEAR & YEAR <= 2019) %>%
  mutate(
    # CUM_RUNS_OBS = CUM_RUNS,
    P = factor(PARK),
    OY = factor(OFF_TEAM_ID):factor(YEAR),
    DY = factor(DEF_TEAM_ID):factor(YEAR),
    G = factor(GAME_ID),
    OYG = factor(as.character(OY:G)), ## do not change this line...
    DYG = factor(as.character(DY:G))  ## do not change this line...
  ) %>%
  select(-c(PARK,OFF_TEAM_ID,DEF_TEAM_ID,INNING,HOME_TEAM_ID,AWAY_TEAM_ID,CUM_RUNS,GAME_ID,YEAR,G))
sim_df1

sim_df2 = sim_df1 %>% 
  mutate(
    P = unclass(P),
    OY = unclass(OY),
    DY = unclass(DY),
    OYG = unclass(OYG),#unclass(OYG),
    DYG = unclass(DYG)
  )
sim_df2
# sort(unique(sim_df2$OYG)) ##BAD
# sort(unique(sim_df2$DYG)) ##BAD

SIM_DF = sim_df2

n = nrow(SIM_DF)
n_p = max(SIM_DF$P)
n_oy = max(SIM_DF$OY)
n_dy = max(SIM_DF$DY)
n_oyg = max(SIM_DF$OYG)
n_dyg = max(SIM_DF$DYG)

mu_p = 0
mu_oy = 0
mu_dy = 0
sig_p = 0.2
sig_o = 0.2
sig_d = 0.1
tau_o = 0.4
tau_d = 0.2
alpha = 0.75

beta_p = rnorm(n_p, mean=mu_p, sd=sig_p)
theta_oy = rnorm(n_oy, mean=mu_oy, sd=sig_o)
theta_dy = rnorm(n_dy, mean=mu_dy, sd=sig_d)

beta_oyg = numeric(n_oyg)
for (g in 1:n_oyg) {
  oy = SIM_DF$OY[g]
  beta_oyg[g] = rnorm(1, mean=theta_oy[oy], sd=tau_o)
}

beta_dyg = numeric(n_dyg)
for (g in 1:n_dyg) {
  dy = SIM_DF$DY[g]
  beta_dyg[g] = rnorm(1, mean=theta_dy[dy], sd=tau_d)
}

# ### identifiability
# theta_oy[1] = 0
# theta_dy[1] = 0
# beta_p[1] = 0
# beta_oyg[1] = 0
# beta_dyg[1] = 0

### generate y
eta = alpha + beta_p[SIM_DF$P] + beta_oyg[SIM_DF$OYG] + beta_dyg[SIM_DF$DYG]
lambda = exp(eta)
y = numeric(n)
for (i in 1:n) {
  y[i] = rpois(1, lambda[i])
}
# y

data_train <- list(
  n=n, n_p=n_p, n_oy=n_oy, n_dy=n_dy,
  y=y, P=SIM_DF$P, OY=SIM_DF$OY, DY=SIM_DF$DY, OYG=SIM_DF$OYG, DYG=SIM_DF$DYG
)
params_true <- list(
  mu_p=mu_p, mu_oy=mu_oy, mu_dy=mu_dy, 
  sig_p=sig_p, sig_o=sig_o, sig_d=sig_d, 
  tau_o=tau_o, tau_d=tau_d, alpha=alpha,
  theta_oy=theta_oy, theta_dy=theta_dy,
  # beta_oyg=beta_oyg, beta_dyg=beta_dyg,
  beta_p=beta_p 
)

##################
### Stan Model ###
##################

model1 <- stan_model(file = "parkFx1.stan", model_name = "parkFx1")

fit_model1 <- function() { #fold_num=NA
  # train the model
  fit <- sampling(model1,
                  data = data_train,
                  iter = NUM_ITS,
                  pars=c("beta_p_raw","theta_oy_raw","theta_dy_raw",
                         "beta_oyg_raw","beta_dyg_raw",
                         # "eta",
                         "beta_oyg", "beta_dyg"), 
                  include=FALSE,
                  chains = cores, #1 #cores, 
                  cores = cores, # HPCC
                  seed = 12345)
  fit
}



