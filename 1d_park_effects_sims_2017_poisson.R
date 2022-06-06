# library(tidyverse)
# library(lme4)
# library(splines)
# library(glmnet)
# library(plotly)
# library(ggthemes)
# library(cowplot)
# library(latex2exp)
# theme_set(theme_bw())
# theme_update(text = element_text(size=18))
# theme_update(plot.title = element_text(hjust = 0.5))
# output_folder = "./plots/"
# 
# war2 <- read_csv("war2.csv") ### dataset
# 
# ### runs scored in an inning ~ park_effect + team_off_q + team_def_q + spline(time)
# ### time is a fixed effect, other parameters are random intercepts
# park_df0 = war2 %>% 
#   select(GAME_ID, YEAR, DAYS_SINCE_SZN_START, BAT_HOME_IND, INNING, HOME_TEAM_ID, AWAY_TEAM_ID, PARK, INN_RUNS, CUM_RUNS) %>%
#   filter(ifelse(BAT_HOME_IND == 1, INNING <= 8, INNING <= 9)) %>%
#   group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
#   slice_tail() %>%
#   ungroup() %>%
#   mutate(OFF_TEAM_ID = ifelse(BAT_HOME_IND == 1, HOME_TEAM_ID, AWAY_TEAM_ID),
#          DEF_TEAM_ID = ifelse(BAT_HOME_IND == 1, AWAY_TEAM_ID, HOME_TEAM_ID)) %>%
#   arrange(BAT_HOME_IND, GAME_ID, INNING)
# park_df0[1:18,]
# 
# park_df = park_df0 %>% left_join(park_df0 %>% group_by(PARK) %>% summarise(count = n()) %>% arrange(count)) %>% filter(count > 300)
# 
# ####################################
# ### 2017 season Simulation Setup ###
# ####################################
# 
# ### actual
# # X0 = park_df %>% filter(YEAR == 2017)
# # m1 = glm(INN_RUNS ~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=X0, family="poisson")
# # coeffs.fit = coefficients(m1)
# # alpha.fit = coeffs.fit[1]
# # beta.oq.fit = coeffs.fit[str_detect(names(coeffs.fit), "OFF_TEAM_ID")]
# # beta.dq.fit = coeffs.fit[str_detect(names(coeffs.fit), "DEF_TEAM_ID")]
# # beta.pk.fit = coeffs.fit[str_detect(names(coeffs.fit), "PARK")]
# # alpha.fit
# # c(mean(beta.oq.fit), sd(beta.oq.fit))
# # c(mean(beta.dq.fit), sd(beta.dq.fit))
# # c(mean(beta.pk.fit), sd(beta.pk.fit))
# # sigma(m1)
# 
# ### sim
# X0 = park_df %>% filter(YEAR == 2017)
# X1_ = X0 %>% filter(BAT_HOME_IND == 0)
# X2_ = X0 %>% filter(BAT_HOME_IND == 1)
# 
# # X_ = bind_rows(X1_, X2_)
# # sum(X1_$PARK == X1_$OFF_TEAM_ID)
# # sum(X2_$PARK == X2_$DEF_TEAM_ID)
# 
# X1_$PARK = X1_$DEF_TEAM_ID
# X2_$PARK = X2_$OFF_TEAM_ID
# X_ = bind_rows(X1_, X2_)
# 
# P1 = as.matrix(modelr::model_matrix(~ factor(PARK) , data=X1_))
# D1 = as.matrix(modelr::model_matrix(~ factor(DEF_TEAM_ID) , data=X1_))
# O1 = as.matrix(modelr::model_matrix(~ factor(OFF_TEAM_ID) , data=X1_))
# P2 = as.matrix(modelr::model_matrix(~ factor(PARK) , data=X2_))
# D2 = as.matrix(modelr::model_matrix(~ factor(DEF_TEAM_ID) , data=X2_))
# O2 = as.matrix(modelr::model_matrix(~ factor(OFF_TEAM_ID) , data=X2_))
# # norm(P1-D1, type = "1")
# # norm(P2-O2, type = "1")
# # rankMatrix(P1-O1)
# # rankMatrix(P2-D2)
# 
# ### everything relative to ANA
# X = as.matrix(modelr::model_matrix(~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=X_)) # n x 88, where 88 = 1 + 29*3 
# alpha = -0.816
# beta.oq = rnorm(dim(O1)[2] - 1, 0.035, 0.085)
# beta.dq = rnorm(dim(D1)[2] - 1, 0.041, 0.141)
# beta.pk = rnorm(dim(P1)[2] - 1, 0.083, 0.122)
# beta = matrix(c(alpha, beta.oq, beta.dq, beta.pk))
# rownames(beta) = colnames(X)
# beta_df = tibble(
#   TEAM_ID = str_sub(rownames(beta)[str_detect(rownames(beta), "PARK")], -3, -1),
#   beta.oq, beta.dq, beta.pk
# ); beta_df

####################### simulate the data y ####################### 
### SIM STARTS HERE
# sigma = 1.226 
# eps = rnorm(dim(X)[1], 0, sigma)
# linpred = alpha + X %*% beta + eps
linpred = alpha + X %*% beta 
lambda = exp(linpred)
y = rpois(dim(X)[1], lambda)
X1_$y1 = y[1:dim(X1_)[1]]
X2_$y2 = y[(dim(X1_)[1]+1):(dim(X1_)[1]+dim(X2_)[1]) ]
X_$y = as.numeric(y)

################
### Method 2 ###
################

### get OQ, DQ
lm_oq = glm(y1 ~ factor(OFF_TEAM_ID) + factor(PARK) , data=X1_, family="poisson")
lm_dq = glm(y2 ~ factor(DEF_TEAM_ID) + factor(PARK) , data=X2_, family="poisson")
coeffs_oq = coefficients(lm_oq)[str_detect(names(coefficients(lm_oq)), "OFF")]
coeffs_dq = coefficients(lm_dq)[str_detect(names(coefficients(lm_dq)), "DEF")]
coeffs_p1 = coefficients(lm_oq)[str_detect(names(coefficients(lm_oq)), "PARK")]
coeffs_p2 = coefficients(lm_dq)[str_detect(names(coefficients(lm_dq)), "PARK")]
se_oq = summary(lm_oq)$coefficients[,2][str_detect(names(coefficients(lm_oq)), "OFF")]
se_dq = summary(lm_dq)$coefficients[,2][str_detect(names(coefficients(lm_dq)), "DEF")]
oqdq_df2 = data.frame(
  TEAM = str_sub(names(coeffs_p1), -3, -1),
  beta_hat_OQ = unname(coeffs_oq),
  beta.oq,
  beta_hat_DQ = unname(coeffs_dq),
  beta.dq, se_oq, se_dq
  # beta_hat_p1 = unname(coeffs_p1),
  # beta_hat_p2 = unname(coeffs_p2),
  # beta.pk
) %>% mutate(beta_hat_OQ_L = beta_hat_OQ - 2*se_oq, beta_hat_OQ_U = beta_hat_OQ + 2*se_oq,
             beta_hat_DQ_L = beta_hat_DQ - 2*se_dq, beta_hat_DQ_U = beta_hat_DQ + 2*se_dq)
oqdq_df2
### OQ, DQ coefficients are recovered!
oqdq_df2 %>% ggplot(aes(x=beta.oq, y=beta_hat_OQ)) +
  geom_point(color="dodgerblue2") +
  geom_errorbar(aes(ymin = beta_hat_OQ_L, ymax = beta_hat_OQ_U), width=0.005, size=0.5, color="dodgerblue2") +
  geom_abline(intercept = 0, slope = 1) 
oqdq_df2 %>% ggplot(aes(x=beta.dq, y=beta_hat_DQ)) +
  geom_point(color="dodgerblue2") +
  geom_errorbar(aes(ymin = beta_hat_DQ_L, ymax = beta_hat_DQ_U), width=0.005, size=0.5, color="dodgerblue2") +
  geom_abline(intercept = 0, slope = 1) 



### get PARK
curr_oq = data.frame(
  OFF_TEAM_ID = str_sub(names(coeffs_oq), -3, -1),
  beta_hat_OQ = unname(coeffs_oq)
) %>% bind_rows(tibble(OFF_TEAM_ID = "ANA", beta_hat_OQ = 0))
curr_dq = data.frame(
  DEF_TEAM_ID = str_sub(names(coeffs_dq), -3, -1),
  beta_hat_DQ = unname(coeffs_dq)
) %>% bind_rows(tibble(DEF_TEAM_ID = "ANA", beta_hat_DQ = 0))
X_pk = X_ %>% left_join(curr_oq) %>% left_join(curr_dq) 

lm_park = glm(y ~ beta_hat_OQ + beta_hat_DQ + factor(PARK) , data=X_pk, family="poisson")
# lm_park
coeffs_pk = coefficients(lm_park)[str_detect(names(coefficients(lm_park)), "PARK")]
se_pk = summary(lm_park)$coefficients[,2][str_detect(names(coefficients(lm_park)), "PARK")]

PARK_df2 = data.frame(
  PARK = str_sub(names(coeffs_pk), -3, -1),
  beta_hat_PARK = unname(coeffs_pk),
  beta.pk,
  se = unname(se_pk)
) %>% mutate(
  beta_hat_PARK_L = beta_hat_PARK - 2*se,
  beta_hat_PARK_U = beta_hat_PARK + 2*se
) %>% relocate(beta_hat_PARK_L, .before = beta_hat_PARK) %>%
  relocate(beta_hat_PARK_U, .after = beta_hat_PARK) 
PARK_df2

### evaluate how well we recovered the coeffs




mean(abs(PARK_df2$beta_hat_PARK - PARK_df2$beta.pk)) 

# PARK_df2 %>% ggplot(aes(x=beta.pk, y=beta_hat_PARK )) +
#   geom_point(color="dodgerblue2") +
#   geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), 
#                 width=0.005, size=0.5, color="dodgerblue2") +
#   geom_abline(intercept = 0, slope = 1) 




#####################
### Method 1: OLS ###
#####################

lm1 = glm(y ~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=X_, family="poisson")

coeffs_pk1 = coefficients(lm1)[str_detect(names(coefficients(lm1)), "PARK")]
se_pk1 = summary(lm1)$coefficients[,2][str_detect(names(coefficients(lm1)), "PARK")]

PARK_df1 = data.frame(
  PARK = str_sub(names(coeffs_pk1), -3, -1),
  beta_hat_PARK = unname(coeffs_pk1),
  beta.pk,
  se = unname(se_pk1)
) %>% mutate(
  beta_hat_PARK_L = beta_hat_PARK - 2*se,
  beta_hat_PARK_U = beta_hat_PARK + 2*se
) %>% relocate(beta_hat_PARK_L, .before = beta_hat_PARK) %>%
  relocate(beta_hat_PARK_U, .after = beta_hat_PARK) 
PARK_df1

mean(abs(PARK_df1$beta_hat_PARK - PARK_df1$beta.pk)) 
PARK_df1$method = "ols"
PARK_df2$method = "ols.3.steps"

# plot_2017_parkEffectResults = bind_rows(PARK_df1, PARK_df2) %>% 
#   ggplot(aes(x=exp(beta.pk), y=exp(beta_hat_PARK), color=method)) +
#   geom_point(size=1) +
#   geom_errorbar(aes(ymin = exp(beta_hat_PARK_L), ymax = exp(beta_hat_PARK_U)), width=0.01) +
#   geom_abline(intercept = 0, slope = 1) +
#   scale_color_manual(values=c("dodgerblue2", "firebrick")) +
#   labs(x="true park effect", y="fitted park effect")
# plot_2017_parkEffectResults

# ggsave("plot_2017_poisson_parkEffectResults.png", plot_2017_parkEffectResults, width=8, height=6)

#######################
### Method 3: ridge ###
#######################

ridge3 = glmnet(
  x = model.matrix(~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=X_),
  y = X_$y, alpha = 0, lambda = 0.035, family="poisson"
)

coeffs_ridge3_all = coef(ridge3)[,1]
coeffs_ridge3 = coeffs_ridge3_all[str_detect(names(coeffs_ridge3_all), "PARK")]

PARK_df3 = data.frame(
  PARK = str_sub(names(coeffs_ridge3), -3, -1),
  beta_hat_PARK = unname(coeffs_ridge3),
  beta.pk,
  method="ridge.3"
  # se = unname(se_pk3)
) 
# %>% mutate(
#   beta_hat_PARK_L = beta_hat_PARK - 2*se,
#   beta_hat_PARK_U = beta_hat_PARK + 2*se
# ) %>% relocate(beta_hat_PARK_L, .before = beta_hat_PARK) %>%
#   relocate(beta_hat_PARK_U, .after = beta_hat_PARK) 
PARK_df3

# norm(PARK_df1$beta.pk - PARK_df1$beta_hat_PARK, "2")
# norm(PARK_df2$beta.pk - PARK_df2$beta_hat_PARK, "2")
# norm(PARK_df3$beta.pk - PARK_df3$beta_hat_PARK, "2")
# 
# plot_2017_parkEffectResults = bind_rows(PARK_df1, PARK_df2, PARK_df3) %>% 
#   ggplot(aes(x=exp(beta.pk), y=exp(beta_hat_PARK), color=method)) +
#   geom_point(size=1) +
#   # geom_errorbar(aes(ymin = exp(beta_hat_PARK_L), ymax = exp(beta_hat_PARK_U)), width=0.01) +
#   geom_abline(intercept = 0, slope = 1) +
#   scale_color_manual(values=c("dodgerblue2", "firebrick", "black")) +
#   labs(x="true park effect", y="fitted park effect")
# plot_2017_parkEffectResults

#################################
### Method 4: ridge + 3 steps ###
#################################

### get OQ, DQ
LAMBDA.4 = 0.1
LAMBDA.4.2 = 0.1

ridge_oq = glmnet(
  x = model.matrix(~ factor(OFF_TEAM_ID) + factor(PARK), data=X1_),
  y = X1_$y1, alpha = 0, lambda = LAMBDA.4, family="poisson"
)
ridge_dq = glmnet(
  x = model.matrix(~ factor(DEF_TEAM_ID) + factor(PARK), data=X2_),
  y = X2_$y2, alpha = 0, lambda = LAMBDA.4, family="poisson"
)
coeffs_oq = coefficients(ridge_oq)[str_detect(rownames(coefficients(ridge_oq)), "OFF")]
coeffs_dq = coefficients(ridge_dq)[str_detect(rownames(coefficients(ridge_dq)), "DEF")]
coeffs_p1 = coefficients(ridge_oq)[str_detect(rownames(coefficients(ridge_oq)), "PARK")]
coeffs_p2 = coefficients(ridge_dq)[str_detect(rownames(coefficients(ridge_dq)), "PARK")]
se_oq = summary(ridge_oq)$coefficients[,2][str_detect(rownames(coefficients(ridge_oq)), "OFF")]
se_dq = summary(ridge_dq)$coefficients[,2][str_detect(rownames(coefficients(ridge_dq)), "DEF")]
oqdq_df2 = data.frame(
  TEAM = str_sub(names(coeffs_p1), -3, -1),
  beta_hat_OQ = unname(coeffs_oq),
  beta.oq,
  beta_hat_DQ = unname(coeffs_dq),
  beta.dq, se_oq, se_dq
  # beta_hat_p1 = unname(coeffs_p1),
  # beta_hat_p2 = unname(coeffs_p2),
  # beta.pk
) %>% mutate(beta_hat_OQ_L = beta_hat_OQ - 2*se_oq, beta_hat_OQ_U = beta_hat_OQ + 2*se_oq,
             beta_hat_DQ_L = beta_hat_DQ - 2*se_dq, beta_hat_DQ_U = beta_hat_DQ + 2*se_dq)
oqdq_df2
### OQ, DQ coefficients are recovered!
oqdq_df2 %>% ggplot(aes(x=beta.oq, y=beta_hat_OQ)) +
  geom_point(color="dodgerblue2") +
  geom_errorbar(aes(ymin = beta_hat_OQ_L, ymax = beta_hat_OQ_U), width=0.005, size=0.5, color="dodgerblue2") +
  geom_abline(intercept = 0, slope = 1) 
oqdq_df2 %>% ggplot(aes(x=beta.dq, y=beta_hat_DQ)) +
  geom_point(color="dodgerblue2") +
  geom_errorbar(aes(ymin = beta_hat_DQ_L, ymax = beta_hat_DQ_U), width=0.005, size=0.5, color="dodgerblue2") +
  geom_abline(intercept = 0, slope = 1) 

### get PARK
curr_oq = data.frame(
  OFF_TEAM_ID = str_sub(names(coeffs_oq), -3, -1),
  beta_hat_OQ = unname(coeffs_oq)
) %>% bind_rows(tibble(OFF_TEAM_ID = "ANA", beta_hat_OQ = 0))
curr_dq = data.frame(
  DEF_TEAM_ID = str_sub(names(coeffs_dq), -3, -1),
  beta_hat_DQ = unname(coeffs_dq)
) %>% bind_rows(tibble(DEF_TEAM_ID = "ANA", beta_hat_DQ = 0))
X_pk = X_ %>% left_join(curr_oq) %>% left_join(curr_dq) 

ridge_park = glmnet(
  x = model.matrix(~ beta_hat_OQ + beta_hat_DQ + factor(PARK), data=X_pk),
  y = X_pk$y, alpha = 0, lambda = LAMBDA.4.2, family="poisson"
)
coeffs_pk = coefficients(ridge_park)[str_detect(rownames(coefficients(ridge_park)), "PARK"),]
se_pk = summary(ridge_park)$coefficients[,2][str_detect(rownames(coefficients(ridge_park)), "PARK")]

PARK_df4 = data.frame(
  PARK = str_sub(names(coeffs_pk), -3, -1),
  beta_hat_PARK = unname(coeffs_pk),
  beta.pk,
  se = unname(se_pk)
) %>% mutate(
  beta_hat_PARK_L = beta_hat_PARK - 2*se,
  beta_hat_PARK_U = beta_hat_PARK + 2*se
) %>% relocate(beta_hat_PARK_L, .before = beta_hat_PARK) %>%
  relocate(beta_hat_PARK_U, .after = beta_hat_PARK) 
PARK_df4
PARK_df4$method = "ridge.3.steps"

plot_2017_parkEffectResults = 
  bind_rows(PARK_df1, PARK_df2, PARK_df3, PARK_df4) %>%
  # PARK_df3 %>%
  ggplot(aes(x=exp(beta.pk), y=exp(beta_hat_PARK), color=method)) +
  geom_point(size=1) +
  # geom_errorbar(aes(ymin = exp(beta_hat_PARK_L), ymax = exp(beta_hat_PARK_U)), width=0.01) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values=c("dodgerblue2", "firebrick", "black", "orange")) +
  labs(x="true park effect", y="fitted park effect")
plot_2017_parkEffectResults


print(norm(PARK_df1$beta.pk - PARK_df1$beta_hat_PARK, "2"))
print(norm(PARK_df2$beta.pk - PARK_df2$beta_hat_PARK, "2"))
print(norm(PARK_df3$beta.pk - PARK_df3$beta_hat_PARK, "2"))
print(norm(PARK_df4$beta.pk - PARK_df4$beta_hat_PARK, "2"))


