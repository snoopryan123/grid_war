library(tidyverse)
library(glmnet)
library(lme4)
library(splines)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)

require(foreign)
require(MASS)
require(reshape2)
# require(Hmisc)

theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = "./plots/"

war2 <- read_csv("../war2.csv") ### dataset

### runs scored in an inning ~ park_effect + team_off_q + team_def_q + spline(time)
### time is a fixed effect, other parameters are random intercepts
park_df0 = war2 %>% 
  select(GAME_ID, YEAR, DAYS_SINCE_SZN_START, BAT_HOME_IND, INNING, HOME_TEAM_ID, AWAY_TEAM_ID, PARK, INN_RUNS, CUM_RUNS) %>%
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
X_df$INN_RUNS_f = factor(X_df$INN_RUNS)
X_batAway_df = X_df %>% filter(BAT_HOME_IND == 0)
X_batHome_df = X_df %>% filter(BAT_HOME_IND == 1)
### everything relative to ANA park and ANA2017 
X0 = as.matrix(modelr::model_matrix(~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_df))
X = X0[,2:ncol(X)]

# ### get a sense of the scale of the coefficients via DWreg
# m1 <- polr(INN_RUNS_f ~ factor(PARK) + factor(OT_YR) + factor(DT_YR),
#            data = X_df, #(X_df %>% filter(YEAR == 2017)), 
#            Hess=FALSE, control = list(maxit = 500)
# )
# coeffs.fit = m1$coefficients
# zeta.fit = m1$zeta
# beta.oq.fit = coeffs.fit[str_detect(names(coeffs.fit), "OT_YR")]
# beta.dq.fit = coeffs.fit[str_detect(names(coeffs.fit), "DT_YR")]
# beta.pk.fit = coeffs.fit[str_detect(names(coeffs.fit), "PARK")]
# zeta.fit
# c(mean(beta.oq.fit), sd(beta.oq.fit))
# c(mean(beta.dq.fit), sd(beta.dq.fit))
# c(mean(beta.pk.fit), sd(beta.pk.fit))

### sim
NUM.SIMS = 2#25
beta.df = matrix(nrow=length(colnames(X)), ncol=NUM.SIMS)
rownames(beta.df) = colnames(X)
colnames(beta.df) = paste0("sim", 1:NUM.SIMS)
zeta.df = matrix(nrow=max(X_df$INN_RUNS)+2, ncol=NUM.SIMS)
colnames(zeta.df) = paste0("sim", 1:NUM.SIMS)

for (i in 1:NUM.SIMS) {
  beta.oq = rnorm(sum(str_detect(colnames(X), "OT")), 0, 0.1)
  beta.dq = rnorm(sum(str_detect(colnames(X), "DT")), 0, 0.1)
  beta.pk = rnorm(sum(str_detect(colnames(X), "PARK")), 0.1, 0.1)
  beta.df[,i] = matrix(c(beta.oq, beta.dq, beta.pk))
  
  zeta0 = 1:max(X_df$INN_RUNS)
  zeta1 = sort(zeta0 + rnorm(n=max(X_df$INN_RUNS), mean=0, sd=0.3))
  zeta.df[,i] = c(-Inf, zeta1, Inf)*0.75
}

beta_pk_df = as_tibble( beta.df[rownames(beta.df)[str_detect(rownames(beta.df), "PARK")], ] )
beta_pk_df = beta_pk_df %>% 
  mutate(PARK = str_sub(rownames(beta.df)[str_detect(rownames(beta.df), "PARK")], -5, -1)) %>%
  relocate(PARK, .before=sim1)

beta_oq_df = as_tibble( beta.df[rownames(beta.df)[str_detect(rownames(beta.df), "OT")], ] )
beta_oq_df = beta_oq_df %>% 
  mutate(OT_YR = str_sub(rownames(beta.df)[str_detect(rownames(beta.df), "OT")], -7, -1)) %>%
  relocate(OT_YR, .before=sim1)

beta_dq_df = as_tibble( beta.df[rownames(beta.df)[str_detect(rownames(beta.df), "DT")], ] )
beta_dq_df = beta_dq_df %>% 
  mutate(DT_YR = str_sub(rownames(beta.df)[str_detect(rownames(beta.df), "DT")], -7, -1)) %>%
  relocate(DT_YR, .before=sim1)

eps = MASS::mvrnorm(n=dim(X)[1], mu=rep(0,NUM.SIMS), Sigma=diag(1,NUM.SIMS) )
Xb = X %*% beta.df
z = Xb + eps

for (i in 1:NUM.SIMS) {
  ff = function(j) { Position(function(x) x > j, zeta.df[,i]) - 2 }
  y[,i] = sapply(z[,i], ff)
}

head(y,100)

#############################################
### Method 1: Ordinal Logistic Regression ###
#############################################

park_names = str_sub(rownames(beta.df)[str_detect(rownames(beta.df), "PARK")], -5, -1)
NUM.PARKS = length(park_names)
coeffs_pk = matrix(nrow=NUM.PARKS, ncol=NUM.SIMS )
rownames(coeffs_pk) = park_names
colnames(coeffs_pk) = paste0("sim", 1:NUM.SIMS)

for (i in 1:NUM.SIMS) {
  print(paste0("sim ", i))
  
  X_i = X_df %>% mutate(y = y[,i])
  # OLR
  olr_i <- polr(INN_RUNS_f ~ factor(PARK) + factor(OT_YR) + factor(DT_YR),
             data = X_i, 
             Hess=FALSE, control = list(maxit = 500)
  )
  coeffs_pk[,i] = coefficients(olr_i)[str_detect(names(coefficients(olr_i)), "PARK")]
}

mean( 
  apply( (coeffs_pk - beta_pk_df[2:ncol(beta_pk_df)])**2, 2, function(x) sqrt(mean(x)) ) 
)


j = 1
PARK_df_1.1 = data.frame(
  PARK = str_sub(beta_pk_df$PARK, -5, -3),
  beta_hat_PARK = unname(coeffs_pk)[,j],
  beta.pk = beta_pk_df[,j+1][[1]],
  method="OLR"
)
bind_rows(PARK_df_1.1) %>%
  ggplot(aes(x=beta.pk, y=beta_hat_PARK, color=method)) +
  geom_point(size=2) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values=c("dodgerblue2", "firebrick")) +
  labs(x="true park coeff", y="fitted park coeff")

# doesnt work...






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

mean( 
  apply( (coeffs_pk2 - beta_pk_df[2:ncol(beta_pk_df)])**2, 2, function(x) sqrt(mean(x)) ) 
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
    y = X_i$y, alpha = 0, lambda = 0.035, family="gaussian"
  )
  coeffs_ridge3_i = coef(ridge3_i)[,1]
  coeffs_pk3[,i]  = coeffs_ridge3_i[str_detect(names(coeffs_ridge3_i), "PARK")]
}

mean( 
  apply( (coeffs_pk3 - beta_pk_df[2:ncol(beta_pk_df)])**2, 2, function(x) sqrt(mean(x)) ) 
)

####################
### Plot 1 vs. 3 ###
####################

j = 9
PARK_df_1.1 = data.frame(
  PARK = str_sub(beta_pk_df$PARK, -5, -3),
  beta_hat_PARK = unname(coeffs_pk)[,j],
  beta.pk = beta_pk_df[,j+1][[1]],
  method="OLS"
)

PARK_df_2.1 = data.frame(
  PARK = str_sub(beta_pk_df$PARK, -5, -3),
  beta_hat_PARK = unname(coeffs_pk2)[,j],
  beta.pk = beta_pk_df[,j+1][[1]],
  method="3 Part OLS"
)

PARK_df_3.1 = data.frame(
  PARK = str_sub(beta_pk_df$PARK, -5, -3),
  beta_hat_PARK = unname(coeffs_pk3)[,j],
  beta.pk = beta_pk_df[,j+1][[1]],
  method="Ridge"
)

### rowMeans(coeffs_pk)
### rowMeans(as.matrix(beta_pk_df[,2:(NUM.SIMS+1)]))

plot_13 = bind_rows(PARK_df_3.1, PARK_df_1.1) %>%
  ggplot(aes(x=beta.pk, y=beta_hat_PARK, color=method)) +
  geom_point(size=2) +
  # geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), width=0.01) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values=c("dodgerblue2", "firebrick")) +
  labs(x="true park effect", y="fitted park effect")
plot_13

plot_12 = bind_rows(PARK_df_2.1, PARK_df_1.1) %>%
  ggplot(aes(x=beta.pk, y=beta_hat_PARK, color=method)) +
  geom_point(size=2) +
  # geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), width=0.01) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values=c("dodgerblue2", "firebrick")) +
  labs(x="true park effect", y="fitted park effect")
plot_12

ggsave("plot_13_1719.png", plot_13, width=8, height=6)
ggsave("plot_12_1719.png", plot_12, width=8, height=6)
