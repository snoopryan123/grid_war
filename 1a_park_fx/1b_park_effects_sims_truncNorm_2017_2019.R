library(tidyverse)
library(glmnet)
library(lme4)
library(splines)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = "./plots/"
set.seed(12345)

war2 <- read_csv("../war2.csv") ### dataset

### runs scored in an inning ~ park_effect + team_off_q + team_def_q + spline(time)
### time is a fixed effect, other parameters are random intercepts
park_df0 = war2 %>% 
  select(GAME_ID, YEAR, DAYS_SINCE_SZN_START, BAT_HOME_IND, INNING, HOME_TEAM_ID, 
         AWAY_TEAM_ID, PARK, INN_RUNS, CUM_RUNS, HOME_DIV, AWAY_DIV) %>%
  filter(ifelse(BAT_HOME_IND == 1, INNING <= 8, INNING <= 9)) %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  slice_tail() %>%
  ungroup() %>%
  mutate(OFF_TEAM_ID = ifelse(BAT_HOME_IND == 1, HOME_TEAM_ID, AWAY_TEAM_ID),
         DEF_TEAM_ID = ifelse(BAT_HOME_IND == 1, AWAY_TEAM_ID, HOME_TEAM_ID)) %>%
  arrange(BAT_HOME_IND, GAME_ID, INNING)
park_df0[1:18,]

park_df = park_df0 %>% left_join(park_df0 %>% group_by(PARK) %>% summarise(count = n()) %>% arrange(count)) %>% filter(count > 300)

##################################
### 2017-2019 Simulation Setup ###
##################################

### data matrix 
X_df = park_df %>% filter(YEAR >= 2017) %>% 
  mutate(OT_YR = paste0(OFF_TEAM_ID, YEAR),
         DT_YR = paste0(DEF_TEAM_ID, YEAR))
X_batAway_df = X_df %>% filter(BAT_HOME_IND == 0)
X_batHome_df = X_df %>% filter(BAT_HOME_IND == 1)
### everything relative to ANA park and ANA2017 
X = as.matrix(modelr::model_matrix(~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_df))

# ### get a sense of the scale of the coefficients via OLS
# m1 = lm(INN_RUNS ~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=park_df %>% filter(YEAR == 2017))
# coeffs.fit = coefficients(m1)
# alpha.fit = coeffs.fit[1]
# beta.oq.fit = coeffs.fit[str_detect(names(coeffs.fit), "OFF_TEAM_ID")]
# beta.dq.fit = coeffs.fit[str_detect(names(coeffs.fit), "DEF_TEAM_ID")]
# beta.pk.fit = coeffs.fit[str_detect(names(coeffs.fit), "PARK")]
# alpha.fit
# c(mean(beta.oq.fit), sd(beta.oq.fit))
# c(mean(beta.dq.fit), sd(beta.dq.fit))
# c(mean(beta.pk.fit), sd(beta.pk.fit))
# sigma(m1)

### sim
NUM.SIMS = 25 #25 #5

beta.pk.df = tibble(
  PARK = str_sub(colnames(X)[str_detect(colnames(X), "PARK")], -5, -1)
)

beta.oq.df0 = tibble(
  OT_YR = str_sub(colnames(X)[str_detect(colnames(X), "OT")], -7, -1)
) %>% mutate(
  TEAM = str_sub(OT_YR,1,3),
  YEAR = as.numeric(str_sub(OT_YR,4,8)),
) 
beta.oq.df = beta.oq.df0 %>% left_join(
  X_df %>% select(PARK, HOME_TEAM_ID, YEAR, HOME_DIV) %>% distinct() %>%
    rename(DIV=HOME_DIV, TEAM=HOME_TEAM_ID) %>%
    filter(!(PARK == "STP01" | (TEAM == "MIA" & PARK != "MIL06"))) ## for 2017-2019 sim...
)
beta.oq.df

beta.dq.df = beta.oq.df %>% rename(DT_YR = OT_YR)
beta.dq.df

beta.df = tibble()

for (i in 1:NUM.SIMS) {
  alpha = 0.15 #0.4
  
  beta.pk.df = beta.pk.df %>% 
    mutate(coeff_type = "PARK") %>%
    # mutate(!!paste0("beta.pk.sim",i) :=  rnorm(n(), 0.04, 0.065)  )
    mutate(!!paste0("beta.pk.sim",i) := ifelse(PARK=="DEN02", 0.32, rnorm(n(), 0.04, 0.065))  )
  
  beta.oq.df = beta.oq.df %>% 
    distinct(DIV,YEAR) %>% 
    mutate(coeff_oq_div = rnorm(n(), 0.02, 0.05)) %>% 
    mutate(coeff_type = "OQ") %>%
    right_join(beta.oq.df) %>%
    mutate(!!paste0("beta.oq.sim",i)  := rnorm(coeff_oq_div, coeff_oq_div, 0.02) ) %>%
    select(-coeff_oq_div) 
  
  beta.dq.df = beta.dq.df %>% 
    distinct(DIV,YEAR) %>% 
    mutate(coeff_dq_div = rnorm(n(), 0.03, 0.06)) %>% 
    mutate(coeff_type = "DQ") %>%
    right_join(beta.dq.df) %>%
    mutate(!!paste0("beta.dq.sim",i)  := rnorm(coeff_dq_div, coeff_dq_div, 0.033) ) %>%
    select(-coeff_dq_div) 

  if (i==1) {
    beta.df = tibble(X_name =  c("(Intercept)",
                                paste0("factor(PARK)", beta.pk.df$PARK),
                                paste0("factor(OT_YR)", beta.oq.df$OT_YR),
                                paste0("factor(DT_YR)", beta.dq.df$DT_YR)),
                     coeff_name =  c("intercept", beta.pk.df$PARK,beta.oq.df$OT_YR, beta.dq.df$DT_YR),
                     coeff_type = c("intercept",
                                    rep("PARK", length(beta.pk.df$PARK)),
                                    rep("OQ", length(beta.oq.df$OT_YR)),
                                    rep("DQ", length(beta.dq.df$DT_YR)) )
                     ) %>% mutate(ordering = row_number())
  }

  beta.df[[paste0("beta.sim",i)]] = c(
    alpha,
    beta.pk.df[[paste0("beta.pk.sim",i)]],
    beta.oq.df[[paste0("beta.oq.sim",i)]],
    beta.dq.df[[paste0("beta.dq.sim",i)]]
  )
  
}

### problem: cols of X and rows of beta.df have a different order
### lets reorder the rows beta.df to match!
beta.df1 = beta.df %>% slice(order(factor(X_name, levels = colnames(X))))
dim(X); dim(beta.df1)

### generate the simulated y
Xb = X %*% as.matrix(beta.df1[,5:ncol(beta.df1)])
eps = matrix(nrow=nrow(Xb), ncol=ncol(Xb))
for (i in 1:nrow(Xb)) {
  for (j in 1:ncol(Xb)) {
    eps[i,j] = truncnorm::rtruncnorm(n=1, a=0, mean = Xb[i,j], sd = 1)
  }
}
y = Xb + eps
y = round(y) # integer-valued response
y

#####################
### Method 1: OLS ###
#####################

park_names = (beta.df %>% filter(coeff_type == "PARK"))$coeff_name
NUM.PARKS = length(park_names)
coeffs_pk = matrix(nrow=NUM.PARKS, ncol=NUM.SIMS )
rownames(coeffs_pk) = park_names
colnames(coeffs_pk) = paste0("sim", 1:NUM.SIMS)
# se_pk = matrix(nrow=NUM.PARKS, ncol=NUM.SIMS )
# rownames(se_pk) = park_names
# colnames(se_pk) = paste0("sim", 1:NUM.SIMS)

for (i in 1:NUM.SIMS) {
  print(paste0("sim ", i))
  
  X_i = X_df %>% mutate(y = y[,i])
  # OLS
  lm_i = lm(y ~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_i)
  
  coeffs_pk[,i] = coefficients(lm_i)[str_detect(names(coefficients(lm_i)), "PARK")]
  # se_pk[,i] = summary(lm_i)$coefficients[,2][str_detect(names(coefficients(lm_i)), "PARK")]
}

# mean( 
#   apply( (coeffs_pk - beta.pk.df[3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) ) 
# )

mean( 
  apply( (coeffs_pk[rownames(coeffs_pk) != "DEN02",] - 
            beta.pk.df[beta.pk.df$PARK != "DEN02", 3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) ) 
)

mean(
  as.numeric(abs(coeffs_pk["DEN02",] - (beta.pk.df %>% filter(PARK=="DEN02"))[3:ncol(beta.pk.df)]))
)

############################
### Method 2: 3 Part OLS ###
############################

coeffs_pk2 = matrix(nrow=NUM.PARKS, ncol=NUM.SIMS )
rownames(coeffs_pk2) = park_names
colnames(coeffs_pk2) = paste0("sim", 1:NUM.SIMS)

for (i in 1:NUM.SIMS) {
  print(paste0("sim ", i))

  X_i = X_df %>% mutate(y = y[,i])
  X_batAway_i = X_i %>% filter(BAT_HOME_IND == 0)
  X_batHome_i = X_i %>% filter(BAT_HOME_IND == 1)
  # 3 part OLS
  ### get OQ, DQ
  lm_oq_i = lm(y ~ factor(OT_YR) + factor(PARK) , data=X_batAway_i)
  lm_dq_i = lm(y ~ factor(DT_YR) + factor(PARK) , data=X_batHome_i)
  coeffs_oq_i = coefficients(lm_oq_i)[str_detect(names(coefficients(lm_oq_i)), "OT")]
  coeffs_dq_i = coefficients(lm_dq_i)[str_detect(names(coefficients(lm_dq_i)), "DT")]

  ### get PARK
  curr_oq_i = data.frame(
    OT_YR = str_sub(names(coeffs_oq_i), -7, -1),
    beta_hat_OQ = unname(coeffs_oq_i)
  ) %>% bind_rows(tibble(OT_YR = "ANA2017", beta_hat_OQ = 0)) %>% arrange(OT_YR)
  curr_dq_i = data.frame(
    DT_YR = str_sub(names(coeffs_dq_i), -7, -1),
    beta_hat_DQ = unname(coeffs_dq_i)
  ) %>% bind_rows(tibble(DT_YR = "ANA2017", beta_hat_DQ = 0)) %>% arrange(DT_YR)
  X_pk_i = X_i %>% left_join(curr_oq_i) %>% left_join(curr_dq_i)

  lm_park_i = lm(y - beta_hat_OQ - beta_hat_DQ ~ factor(PARK) , data=X_pk_i)
  coeffs_pk2[,i] = coefficients(lm_park_i)[str_detect(names(coefficients(lm_park_i)), "PARK")]
}

# mean(
#   apply( (coeffs_pk2 - beta.pk.df[3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) )
# )

mean(
  apply( (coeffs_pk2[rownames(coeffs_pk3) != "DEN02",] -
            beta.pk.df[beta.pk.df$PARK != "DEN02", 3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) )
)

mean(
  as.numeric(abs(coeffs_pk2["DEN02",] - (beta.pk.df %>% filter(PARK=="DEN02"))[3:ncol(beta.pk.df)]))
)


#######################
### Method 3: Ridge ###
#######################

coeffs_pk3 = matrix(nrow=NUM.PARKS, ncol=NUM.SIMS )
rownames(coeffs_pk3) = park_names
colnames(coeffs_pk3) = paste0("sim", 1:NUM.SIMS)

for (i in 1:NUM.SIMS) {
  print(paste0("sim ", i))
  
  X_i = X_df %>% mutate(y = y[,i])
  ridge3_i = glmnet(
    x = model.matrix(~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=X_i),
    y = X_i$y, alpha = 0, family="gaussian", lambda = 0.25
  )
  coeffs_ridge3_i = coef(ridge3_i)[,1]
  coeffs_pk3[,i]  = coeffs_ridge3_i[str_detect(names(coeffs_ridge3_i), "PARK")]
}

# mean( 
#   apply( (coeffs_pk3 - beta.pk.df[3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) ) 
# )

mean( 
  apply( (coeffs_pk3[rownames(coeffs_pk3) != "DEN02",] - 
            beta.pk.df[beta.pk.df$PARK != "DEN02", 3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) ) 
)

mean(
  as.numeric(abs(coeffs_pk3["DEN02",] - (beta.pk.df %>% filter(PARK=="DEN02"))[3:ncol(beta.pk.df)]))
)

#######################################################################
### Method 4: Shane - OLS, ignoring offensive and defensive quality ###
#######################################################################

coeffs_pk4 = matrix(nrow=NUM.PARKS, ncol=NUM.SIMS )
rownames(coeffs_pk4) = park_names
colnames(coeffs_pk4) = paste0("sim", 1:NUM.SIMS)

for (i in 1:NUM.SIMS) {
  print(paste0("sim ", i))
  
  X_i = X_df %>% mutate(y = y[,i])
  # OLS
  lm_i = lm(y ~ factor(PARK), data=X_i)
  
  coeffs_pk4[,i] = coefficients(lm_i)[str_detect(names(coefficients(lm_i)), "PARK")]
}

mean( 
  apply( (coeffs_pk4[rownames(coeffs_pk4) != "DEN02",] - 
            beta.pk.df[beta.pk.df$PARK != "DEN02", 3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) ) 
)

mean(
  as.numeric(abs(coeffs_pk4["DEN02",] - (beta.pk.df %>% filter(PARK=="DEN02"))[3:ncol(beta.pk.df)]))
)

#############################################################
### Method 5: OLS with biased adjustments of team quality ###
#############################################################

coeffs_pk5 = matrix(nrow=NUM.PARKS, ncol=NUM.SIMS )
rownames(coeffs_pk5) = park_names
colnames(coeffs_pk5) = paste0("sim", 1:NUM.SIMS)

for (i in 1:NUM.SIMS) {
  print(paste0("sim ", i))
  
  X_i = X_df %>% mutate(y = y[,i])
  oq_i =  X_i %>% group_by(OT_YR) %>% 
    summarise(toq = sum(y)) %>%
    # mutate(toq = (toq - mean(toq)) / (2*sd(toq)) )
    mutate(toq = (toq - toq[1]) / (2*sd(toq)) )
  dq_i = X_i %>% group_by(DT_YR) %>% 
    summarise(tdq = sum(y)) %>%
    # mutate(tdq = (tdq - mean(tdq)) / (2*sd(tdq)) )
    mutate(tdq = (tdq - tdq[1]) / (2*sd(tdq)) )
  
  ### get PARK
  X_pk_i = X_i %>% left_join(oq_i) %>% left_join(dq_i) 
  
  lm_park_i = lm(y - toq - tdq ~ factor(PARK) , data=X_pk_i)
  coeffs_pk5[,i] = coefficients(lm_park_i)[str_detect(names(coefficients(lm_park_i)), "PARK")]
}

mean( 
  apply( (coeffs_pk5[rownames(coeffs_pk5) != "DEN02",] - 
            beta.pk.df[beta.pk.df$PARK != "DEN02", 3:ncol(beta.pk.df)])**2, 2, function(x) sqrt(mean(x)) ) 
)

mean(
  as.numeric(abs(coeffs_pk5["DEN02",] - (beta.pk.df %>% filter(PARK=="DEN02"))[3:ncol(beta.pk.df)]))
)


####################
### Plot 1 vs. 3 ###
####################

j = 5 #5 #3 #11
PARK_df_1.1 = data.frame(
  PARK = beta.pk.df$PARK,
  beta_hat_PARK = unname(coeffs_pk)[,j],
  beta.pk = beta.pk.df[,j+2][[1]],
  method="OLS"
)

# PARK_df_2.1 = data.frame(
#   PARK = beta.pk.df$PARK,
#   beta_hat_PARK = unname(coeffs_pk2)[,j],
#   beta.pk = beta.pk.df[,j+2][[1]],
#   method="3 Part OLS"
# )

PARK_df_3.1 = data.frame(
  PARK = beta.pk.df$PARK,
  beta_hat_PARK = unname(coeffs_pk3)[,j],
  beta.pk = beta.pk.df[,j+2][[1]],
  method="Ridge"
)

### rowMeans(coeffs_pk)
### rowMeans(as.matrix(beta_pk_df[,2:(NUM.SIMS+1)]))

plot_13 = bind_rows(PARK_df_3.1, PARK_df_1.1) %>%
  ggplot(aes(y=beta.pk, x=beta_hat_PARK, color=method, shape=method)) +
  geom_point(size=3.5) +
  # geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), width=0.01) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values=c("dodgerblue2", "firebrick")) +
  labs(y='"true" park effect', x="fitted park effect")
plot_13
# ggsave(paste0("plot_sim2_compare13tn_", j,"_1719.png"), plot_13, width=8, height=6)

# plot_13a = bind_rows(PARK_df_3.1, PARK_df_1.1) %>%
#   filter(PARK != "DEN02") %>%
#   ggplot(aes(x=beta.pk, y=beta_hat_PARK, color=method, shape=method)) +
#   geom_point(size=3.5) +
#   # geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), width=0.01) +
#   geom_abline(intercept = 0, slope = 1) +
#   scale_color_manual(values=c("dodgerblue2", "firebrick")) +
#   labs(x='"true" park effect', y="fitted park effect")
# plot_13a

# plot_12 = bind_rows(PARK_df_2.1, PARK_df_1.1) %>%
#   ggplot(aes(x=beta.pk, y=beta_hat_PARK, color=method)) +
#   geom_point(size=2) +
#   # geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), width=0.01) +
#   geom_abline(intercept = 0, slope = 1) +
#   scale_color_manual(values=c("dodgerblue2", "firebrick")) +
#   labs(x='"true" park effect', y="fitted park effect")
# plot_12

# ggsave("plot_sim2_12tn_1719.png", plot_12, width=8, height=6)

#############################################################
### plot the simmed betas and see if they look reasonable ###
#############################################################

# plot_oq18_sim5 = beta.oq.df %>% filter(endsWith(OT_YR, "2018")) %>% ggplot() +
#   geom_point(aes(x=beta.oq.sim5, y=fct_reorder(OT_YR, beta.oq.sim5, .desc=TRUE)), size=2) +
#   ylab("") + xlab(' "true" team offense effect')
# plot_oq18_sim5
# plot_dq18_sim5 = beta.dq.df %>% filter(endsWith(DT_YR, "2018")) %>% ggplot() +
#   geom_point(aes(x=beta.dq.sim5, y=fct_reorder(DT_YR, beta.dq.sim5, .desc=TRUE)), size=2) +
#   ylab("") + xlab(' "true" team defense effect')
# plot_dq18_sim5
# plot_pk_sim5 = beta.pk.df %>% ggplot() +
#   geom_point(aes(x=beta.pk.sim5, y=fct_reorder(PARK, beta.pk.sim5, .desc=TRUE)), size=2) +
#   ylab("") + xlab(' "true" park effect')
# plot_pk_sim5
# 
# ggsave("plot_sim2_oq18_sim5.png", plot_oq18_sim5, width=8, height=7)
# ggsave("plot_sim2_dq18_sim5.png", plot_dq18_sim5, width=8, height=7)
# ggsave("plot_sim2_pk_sim5.png", plot_pk_sim5, width=8, height=7)

###############################################
### Run best model (Ridge) on observed data ###
###############################################

ridge3_obs = glmnet(
  x = model.matrix(~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=X_df),
  y = X_df$INN_RUNS, alpha = 0, lambda = 0.25, family="gaussian"
)
coeffs_ridge3_obs = coef(ridge3_obs)[,1]
coeffs_pk3_obs  = coeffs_ridge3_obs[str_detect(names(coeffs_ridge3_obs), "PARK")]
coeffs_pk3_obs_df = as_tibble(coeffs_pk3_obs) %>% 
  rename(fitted_coeff = value) %>%
  mutate(PARK = str_sub(names(coeffs_pk3_obs), -5, -1) ) %>%
  bind_rows(tibble(fitted_coeff = 0, PARK="ANA01")) %>% arrange(PARK) %>%
  mutate(park_factor = fitted_coeff - mean(fitted_coeff))

plot_pk_fitted_obs = coeffs_pk3_obs_df %>% 
  ggplot() + 
  geom_point(aes(x=park_factor, y=fct_reorder(PARK, park_factor, .desc=TRUE)), size=2) +
  ylab("") + xlab(' fitted park factor')
plot_pk_fitted_obs
# ggsave("plot_pk_fitted_obs.png", plot_pk_fitted_obs, width=8, height=7)


