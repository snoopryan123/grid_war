library(tidyverse)
library(glmnet)
library(lme4)
library(splines)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)
library(rvest)
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

### train park factors on 2014-2016
X_1416 = park_df %>% filter(2014 <= YEAR & YEAR <= 2016) %>% 
  mutate(OT_YR = paste0(OFF_TEAM_ID, YEAR),
         DT_YR = paste0(DEF_TEAM_ID, YEAR))

### test park factors on 2017-2019 
X_1719 = park_df %>% filter(2017 <= YEAR & YEAR <= 2019) %>% 
  mutate(OT_YR = paste0(OFF_TEAM_ID, YEAR),
         DT_YR = paste0(DEF_TEAM_ID, YEAR))

############################################
### out-of-sample park factor comparison ###
############################################

### overall mean inning runs
ybar = mean(X_1719$INN_RUNS)
ybar

# ### Ridge (ecological)
# ridge_eco = glmnet(
#   x = model.matrix(~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=X_1416),
#   y = X_1416$INN_RUNS, alpha = 0, family="gaussian", lambda = 0.25,
# )
# coeffs_ridge_eco = coef(ridge_eco)[,1]
# coeffs_ridge_eco  = coeffs_ridge_eco[str_detect(names(coeffs_ridge_eco), "PARK")]
# coeffs_ridge_eco_df = as_tibble(coeffs_ridge_eco) %>%
#   rename(fitted_coeff = value) %>%
#   mutate(PARK = str_sub(names(coeffs_ridge_eco), -5, -1) ) %>%
#   bind_rows(tibble(fitted_coeff = 0, PARK="ANA01")) %>% arrange(PARK) %>%
#   mutate(park_factor = fitted_coeff - mean(fitted_coeff))
# coeffs_ridge_eco_df$method = "Ridge"
# write_csv(coeffs_ridge_eco_df, "eco_ridge_PF.csv")
ridge_eco = read_csv("eco_ridge_PF.csv")

# ### OLS (ecological)
# ols_eco = lm(INN_RUNS ~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_1416)
# olscoeffs = ols_eco$coefficients[str_detect(names(ols_eco$coefficients), "PARK")]
# ols_eco = tibble(fitted_coeff = c(0,olscoeffs), PARK = ridge_eco$PARK) %>%
#   mutate(park_factor = fitted_coeff - mean(fitted_coeff)) %>%
#   mutate(method = "OLS")
# write_csv(ols_eco, "eco_ols_pf.csv")
ols_eco = read_csv("eco_ols_pf.csv")

# ### ESPN (ecological)
# games_eco_df = X_1416 %>%
#   group_by(GAME_ID, BAT_HOME_IND) %>%
#   slice_tail() %>%
#   ungroup()
# espn_eco_df1 = games_eco_df %>%
#   group_by(HOME_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 1) %>%
#   summarise(homeRS = sum(CUM_RUNS),
#             homeG = n()) %>%
#   rename(TEAM = HOME_TEAM_ID)
# espn_eco_df2 = games_eco_df %>%
#   group_by(HOME_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 0) %>%
#   summarise(homeRA = sum(CUM_RUNS)) %>%
#   rename(TEAM = HOME_TEAM_ID)
# espn_eco_df3 = games_eco_df %>%
#   group_by(AWAY_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 0) %>%
#   summarise(roadRS = sum(CUM_RUNS),
#             roadG = n()) %>%
#   rename(TEAM = AWAY_TEAM_ID)
# espn_eco_df4 = games_eco_df %>%
#   group_by(AWAY_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 1) %>%
#   summarise(roadRA = sum(CUM_RUNS)) %>%
#   rename(TEAM = AWAY_TEAM_ID)
# espn_eco_df5 = espn_eco_df1 %>% left_join(espn_eco_df2) %>%
#   left_join(espn_eco_df3) %>% left_join(espn_eco_df4) %>%
#   mutate(Pf = ((homeRS + homeRA)/homeG) / ((roadRS + roadRA)/roadG)  ) %>%
#   mutate(park_factor = Pf - 1) %>%
#   arrange(desc(park_factor)) %>%
#   mutate(method = "ESPN") %>%
#   left_join(
#     X_1416 %>% select(HOME_TEAM_ID, PARK) %>%
#     rename(TEAM = HOME_TEAM_ID) %>% distinct() %>%
#     filter( !(TEAM == "BAL" & PARK == "STP01") ) ### glitch that BAL play in STP
#   )
# data.frame(espn_eco_df5)
# espn_eco_df5 = espn_eco_df5 %>%
#     mutate(park_factor_mult = park_factor) %>%
#     mutate(park_factor0 = park_factor_mult*ybar, ### convert multiplicative PF to additive PF
#            park_factor = park_factor0 - mean(park_factor0))
# write_csv(espn_eco_df5, "eco_espn_PF.csv")
espn_eco = read_csv("eco_espn_PF.csv")

# ### Fangraphs (ecological), re-created instead of scraped
# fg_eco_df0 = espn_eco_df5 %>%
#   select(TEAM, PARK, homeRS, homeRA, homeG, roadRS, roadRA, roadG) %>%
#   mutate(
#     homeRPG = (homeRS + homeRA)/homeG, #homeRS/homeG,
#     roadRPG = (roadRS + roadRA)/roadG, #roadRS/roadG,
#     numTeams = n(),
#     xi = (homeRPG - roadRPG)/numTeams,
#     PFraw = homeRPG / (roadRPG + xi),
#     iPF = (PFraw + 1)/2,
#     Pf = 1 - (1-iPF)*(0.8),
#     park_factor_mult = Pf - 1,
#     park_factor0 = park_factor_mult*ybar, ### convert multiplicative PF to additive PF
#     park_factor = park_factor0 - mean(park_factor0)
#   )
# fg_eco_df = fg_eco_df0 %>%
#   # select(PARK, park_factor_mult, park_factor) %>%
#   arrange(-park_factor)
# ### fg_eco_df %>% select(PARK, park_factor_mult, park_factor) %>% arrange(-park_factor)
# ### fg_eco_df %>% select(PARK, park_factor_mult, park_factor) %>% arrange(-park_factor)
# fg_eco_df$method = "FanGraphs"
# write_csv(fg_eco_df, "eco_fg_PF.csv")
fg_eco = read_csv("eco_fg_PF.csv")

##### plot 2014-2016 park factor comparison
pf_1416 = bind_rows(ridge_eco, ols_eco, espn_eco, fg_eco) %>% 
  select(PARK, method, park_factor) 
### check
data.frame(pf_1416 %>% arrange(PARK, method) %>% group_by(PARK) %>% summarise(count=n())) #%>%
plot_pf_comparison_1416 = pf_1416 %>% 
  mutate(PARK = factor(PARK, levels = (ridge_eco %>% arrange(park_factor))$PARK )) %>%
  select(PARK, park_factor, method) %>%
  ggplot() + 
  geom_point(aes(x=park_factor,y=PARK,color=method, shape=method), 
             size=4, alpha=0.75) + # alpha=0.85
  ylab("park") + xlab('2016 three-year park effect') +
  scale_shape_manual(values=c(16, 17, 18, 15, 19)) +
  scale_x_continuous(limits = c(-0.1,0.2), breaks = seq(-1,1,by=0.1)) +
  scale_color_brewer(palette="Set1")
plot_pf_comparison_1416
ggsave("plot_pf_comparison_1416.png", plot_pf_comparison_1416, width=9, height=7)


#####
test_df_ridge = X_1719 %>% left_join(ridge_eco %>% select(PARK, park_factor))
test_model_ridge = lm(
  INN_RUNS ~ park_factor + factor(OT_YR) + factor(DT_YR),
  data = test_df_ridge
)
#####
test_df_ols = X_1719 %>% left_join(ols_eco %>% select(PARK, park_factor))
test_model_ols = lm(
  INN_RUNS ~ park_factor + factor(OT_YR) + factor(DT_YR),
  data = test_df_ols
)
#####
ridge_DenOutlier_eco = ridge_eco %>% mutate(
  park_factor = ifelse(PARK == "DEN02", 
                       (ols_eco %>% filter(PARK=="DEN02"))$park_factor,
                       # (fg_eco %>% filter(PARK=="DEN02"))$park_factor,
                       park_factor),
  park_factor = park_factor - mean(park_factor),
  method="Ridge (DEN outlier)"
)
test_df_ridge_DenOutlier = X_1719 %>% left_join(ridge_DenOutlier_eco %>% select(PARK, park_factor))
test_model_ridge_DenOutlier = lm(
  INN_RUNS ~ park_factor + factor(OT_YR) + factor(DT_YR),
  data = test_df_ridge_DenOutlier
)
#####
test_df_fg = X_1719 %>% left_join(fg_eco %>% select(PARK, park_factor)) 
test_model_fg = lm(
  INN_RUNS ~ park_factor + factor(OT_YR) + factor(DT_YR),
  data = test_df_fg
)
#####
test_df_espn = X_1719 %>% left_join(espn_eco %>% select(PARK, park_factor)) 
test_model_espn = lm(
  INN_RUNS ~ park_factor + factor(OT_YR) + factor(DT_YR),
  data = test_df_espn
)

#########
X_test_df = X_1719 %>%
  mutate(
    ridge_preds = predict(test_model_ridge, test_df_ridge),
    ols_preds = predict(test_model_ols, test_df_ols),
    ridge_DenOutlier_preds = predict(test_model_ridge_DenOutlier, test_df_ridge_DenOutlier),
    fg_preds = predict(test_model_fg, test_df_fg),
    espn_preds = predict(test_model_espn, test_df_espn),
    ybar = ybar
  )

### out-of-sample rmse
rmse <- function(x,y) { sqrt(mean( (x-y)**2  )) }

rmse_df = X_test_df %>%
  drop_na() %>%
  summarise(
    ridge_rmse = rmse(INN_RUNS, ridge_preds),
    ols_rmse = rmse(INN_RUNS, ols_preds),
    fg_rmse = rmse(INN_RUNS, fg_preds),
    espn_rmse = rmse(INN_RUNS, espn_preds),
    ybar_rmse = rmse(INN_RUNS, ybar),
  ) %>% 
  mutate_if(is.numeric, round, 5)
rmse_df1 = rmse_df[,order(as.numeric(rmse_df[1,]), decreasing = T)]
rmse_df1
write_csv(rmse_df1, "ecoComp_rmse_compare_0.csv")

### out-of-sample ECOLOGICAL rmse
rmse_eco_df = X_test_df %>%
  drop_na() %>%
  group_by(PARK) %>%
  summarise(
    mean_inn_runs = mean(INN_RUNS),
    mean_ridge_pred = mean(ridge_preds),
    mean_ols_pred = mean(ols_preds),
    mean_ridge_DenOutlier_pred = mean(ridge_DenOutlier_preds),
    mean_fg_pred = mean(fg_preds),
    mean_espn_pred = mean(espn_preds)
  ) %>%
  summarise(
    eco_ridge_rmse = rmse(mean_inn_runs, mean_ridge_pred),
    eco_ols_rmse = rmse(mean_inn_runs, mean_ols_pred),
    eco_ridge_DenOutlier_pred = rmse(mean_inn_runs, mean_ridge_DenOutlier_pred),
    eco_fg_rmse = rmse(mean_inn_runs, mean_fg_pred),
    eco_espn_rmse = rmse(mean_inn_runs, mean_espn_pred),
    eco_ybar_rmse = rmse(mean_inn_runs, ybar)
  ) %>%
  mutate_if(is.numeric, round, 5)
rmse_eco_df1 = rmse_eco_df[,order(as.numeric(rmse_eco_df[1,]), decreasing = T)]
rmse_eco_df1
write_csv(rmse_eco_df1, "ecoComp_rmse_compare_eco.csv")




  

