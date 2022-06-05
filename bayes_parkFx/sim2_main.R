library(tidyverse)
library(rstan)
library(splines)
# library(ggthemes)
# library(latex2exp)
# library(glmnet)
# library(plotly)
# library(cowplot)
# library(lme4)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)
rstan_options(auto_write = TRUE)
NUM_CHAINS = 1
##### uncomment these if working on my computer #####
# cores = 1
# NUM_ITS = 10
#### options(mc.cores = parallel::detectCores())
#####################################################
####### uncomment these if working on HPCC ##########
cores=strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
NUM_ITS = 6000 #FIXME

############
### data ###
############

war2 <- read_csv("../war2.csv") ### dataset

### runs scored thru 8 innings ~ park_effect + team_off_q + team_def_q + spline(time)
### time is a fixed effect, other parameters are random intercepts
park_df0 = war2 %>% 
  select(GAME_ID, YEAR, DAYS_SINCE_SZN_START, BAT_HOME_IND, INNING, HOME_TEAM_ID, AWAY_TEAM_ID, PARK, INN_RUNS, CUM_RUNS) %>% 
  ####################################################################
  ##### if you want INNING-BY-INNING, uncomment the next 2 lines #####
  # filter(ifelse(BAT_HOME_IND == 1, INNING <= 8, INNING <= 9)) %>%
  # group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  ####################################################################
  ####### if you want GAME-BY-GAME, uncomment the next 2 lines #######
  filter(INNING <= 8) %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  ####################################################################
  slice_tail() %>%
  ungroup() %>%
  mutate(OFF_TEAM_ID = ifelse(BAT_HOME_IND == 1, HOME_TEAM_ID, AWAY_TEAM_ID),
         DEF_TEAM_ID = ifelse(BAT_HOME_IND == 1, AWAY_TEAM_ID, HOME_TEAM_ID)) %>%
  arrange(BAT_HOME_IND, GAME_ID, INNING) 
park_df = park_df0 %>% left_join(park_df0 %>% group_by(PARK) %>% summarise(park.count = n()) %>% arrange(park.count)) %>% 
          filter(park.count > 300) %>%  ### >300 innings (or games) per park
          select(-park.count)
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
    # OYG = factor(as.character(OY:G)), ## do not change this line...
    # DYG = factor(as.character(DY:G))  ## do not change this line...
  ) %>%
  select(-c(PARK,OFF_TEAM_ID,DEF_TEAM_ID,HOME_TEAM_ID,AWAY_TEAM_ID,CUM_RUNS,INN_RUNS,GAME_ID,YEAR,G)) #INNING
sim_df1

sim_df2 = sim_df1 %>% 
  mutate(
    P = unclass(P),
    OY = unclass(OY),
    DY = unclass(DY),
    # OYG = unclass(OYG),#unclass(OYG),
    # DYG = unclass(DYG)
  )
sim_df2
# sort(unique(sim_df2$OYG)) ##BAD
# sort(unique(sim_df2$DYG)) ##BAD

SIM_DF = sim_df2

n = nrow(SIM_DF)
n_p = max(SIM_DF$P)
n_oy = max(SIM_DF$OY)
n_dy = max(SIM_DF$DY)
# n_oyg = max(SIM_DF$OYG)
# n_dyg = max(SIM_DF$DYG)

### GAME-BY-GAME parameters
# mu_p = 0
# mu_oy = 0
# mu_dy = 0
# sig_p = 0.2
# sig_o = 0.2
# sig_d = 0.1
# alpha = 0.75
mu_p = 0
mu_oy = 0
mu_dy = 0
sig_p = 1/5
sig_o = 1/5
sig_d = 1/20
alpha = 1.25

# ### INNING-BY-INNING parameters
# mu_p = 0
# mu_oy = 0
# mu_dy = 0
# sig_p = 0.02
# sig_o = 0.02
# sig_d = 0.01
# tau_o = 0.04
# tau_d = 0.02
# alpha = 0.075

beta_p = rnorm(n_p, mean=mu_p, sd=sig_p)
beta_oy = rnorm(n_oy, mean=mu_oy, sd=sig_o)
beta_dy = rnorm(n_dy, mean=mu_dy, sd=sig_d)

# ### identifiability
# theta_oy[1] = 0
# theta_dy[1] = 0
beta_p[1] = 0
beta_oy[1] = 0
beta_dy[1] = 0

### generate y
eta = alpha + beta_p[SIM_DF$P] + beta_oy[SIM_DF$OY] + beta_dy[SIM_DF$DY]
lambda = exp(eta)
y = numeric(n)
for (i in 1:n) {
  y[i] = rpois(1, lambda[i])
}
# y

data_train <- list(
  n=n, n_p=n_p, n_oy=n_oy, n_dy=n_dy,
  y=y, P=SIM_DF$P, 
  OY=SIM_DF$OY, DY=SIM_DF$DY
)
params_true <- list(
  mu_p=mu_p, mu_oy=mu_oy, mu_dy=mu_dy, 
  sig_p=sig_p, sig_o=sig_o, sig_d=sig_d, 
  alpha=alpha,
  beta_oy=beta_oy, beta_dy=beta_dy,
  beta_p=beta_p 
)

##################
### Stan Model ###
##################

model2 <- stan_model(file = "parkFx2.stan", model_name = "parkFx2")

fit_model <- function(model_stan) { #fold_num=NA
  # train the model
  fit <- sampling(model_stan,
                  data = data_train,
                  iter = NUM_ITS,
                  pars=c("beta_p_raw",
                         "lambda", "eta", ###
                         "beta_oy_raw","beta_dy_raw"), 
                  include=FALSE,
                  chains = NUM_CHAINS,
                  cores = cores, # HPCC
                  seed = 12345)
  fit
}



