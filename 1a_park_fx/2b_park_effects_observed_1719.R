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

### observed park factors on 2017-2019 
X_1719 = park_df %>% filter(2017 <= YEAR & YEAR <= 2019) %>% 
  mutate(OT_YR = paste0(OFF_TEAM_ID, YEAR),
         DT_YR = paste0(DEF_TEAM_ID, YEAR))

###########################################
### fitted park factors for 2017 - 2019 ###
###########################################

### overall mean inning runs
ybar = mean(X_1719$INN_RUNS)
ybar

# ### Ridge (observed)
# ridge_obs = glmnet(
#   x = model.matrix(~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=X_1719),
#   y = X_1719$INN_RUNS, alpha = 0, family="gaussian", lambda = 0.25,
# )
# coeffs_ridge_obs = coef(ridge_obs)[,1]
# coeffs_ridge_obs  = coeffs_ridge_obs[str_detect(names(coeffs_ridge_obs), "PARK")]
# coeffs_ridge_obs_df = as_tibble(coeffs_ridge_obs) %>%
#   rename(fitted_coeff = value) %>%
#   mutate(PARK = str_sub(names(coeffs_ridge_obs), -5, -1) ) %>%
#   bind_rows(tibble(fitted_coeff = 0, PARK="ANA01")) %>% arrange(PARK) %>%
#   mutate(park_factor = fitted_coeff - mean(fitted_coeff))
# coeffs_ridge_obs_df$method = "Ridge"
# write_csv(coeffs_ridge_obs_df, "obs_ridge_PF.csv")
ridge_obs = read_csv("obs_ridge_PF.csv")

# ### OLS (observed)
# ols_obs = lm(INN_RUNS ~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_1719)
# olscoeffs = ols_obs$coefficients[str_detect(names(ols_obs$coefficients), "PARK")]
# ols_obs = tibble(fitted_coeff = c(0,olscoeffs), PARK = ridge_obs$PARK) %>%
#   mutate(park_factor = fitted_coeff - mean(fitted_coeff)) %>%
#   mutate(method = "OLS")
# write_csv(ols_obs, "obs_ols_pf.csv")
ols_obs = read_csv("obs_ols_pf.csv")

# ### ESPN (observed)
# games_obs_df = X_1719 %>%
#   group_by(GAME_ID, BAT_HOME_IND) %>%
#   slice_tail() %>%
#   ungroup()
# espn_obs_df1 = games_obs_df %>%
#   group_by(HOME_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 1) %>%
#   summarise(homeRS = sum(CUM_RUNS),
#             homeG = n()) %>%
#   rename(TEAM = HOME_TEAM_ID)
# espn_obs_df2 = games_obs_df %>%
#   group_by(HOME_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 0) %>%
#   summarise(homeRA = sum(CUM_RUNS)) %>%
#   rename(TEAM = HOME_TEAM_ID)
# espn_obs_df3 = games_obs_df %>%
#   group_by(AWAY_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 0) %>%
#   summarise(roadRS = sum(CUM_RUNS),
#             roadG = n()) %>%
#   rename(TEAM = AWAY_TEAM_ID)
# espn_obs_df4 = games_obs_df %>%
#   group_by(AWAY_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 1) %>%
#   summarise(roadRA = sum(CUM_RUNS)) %>%
#   rename(TEAM = AWAY_TEAM_ID)
# espn_obs_df5 = espn_obs_df1 %>% left_join(espn_obs_df2) %>%
#   left_join(espn_obs_df3) %>% left_join(espn_obs_df4) %>%
#   mutate(Pf = ((homeRS + homeRA)/homeG) / ((roadRS + roadRA)/roadG)  ) %>%
#   mutate(park_factor = Pf - 1) %>%
#   arrange(desc(park_factor)) %>%
#   mutate(method = "ESPN") %>%
#   left_join(
#     X_1719 %>% select(HOME_TEAM_ID, PARK) %>%
#     rename(TEAM = HOME_TEAM_ID) %>% distinct() %>%
#       filter( !(TEAM == "TBA" & PARK == "NYC20") ) %>% 
#       filter( !(TEAM == "MIA" & PARK == "MIL06") ) %>%
#       filter( !(TEAM == "HOU" & PARK == "STP01") )
#   )
# data.frame(espn_obs_df5)
# espn_obs_df5 = espn_obs_df5 %>%
#     mutate(park_factor_mult = park_factor) %>%
#     mutate(park_factor0 = park_factor_mult*ybar, ### convert multiplicative PF to additive PF
#            park_factor = park_factor0 - mean(park_factor0))
# write_csv(espn_obs_df5, "obs_espn_PF.csv")
espn_obs = read_csv("obs_espn_PF.csv")

# ### Fangraphs (observed), re-created instead of scraped
# fg_obs_df0 = espn_obs_df5 %>%
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
# fg_obs_df = fg_obs_df0 %>%
#   # select(PARK, park_factor_mult, park_factor) %>%
#   arrange(-park_factor)
# ### fg_obs_df %>% select(PARK, park_factor_mult, park_factor) %>% arrange(-park_factor)
# ### fg_obs_df %>% select(PARK, park_factor_mult, park_factor) %>% arrange(-park_factor)
# fg_obs_df$method = "FanGraphs"
# write_csv(fg_obs_df, "obs_fg_PF.csv")
fg_obs = read_csv("obs_fg_PF.csv")

##### plot 2017-2019 park factor comparison
pf_1719 = bind_rows(ridge_obs, ols_obs, espn_obs, fg_obs) %>% 
  select(PARK, method, park_factor) 
### check
data.frame(pf_1719 %>% arrange(PARK, method) %>% group_by(PARK) %>% summarise(count=n())) #%>%
###
plot_pf_comparison_1719 = pf_1719 %>% 
  mutate(PARK = factor(PARK, levels = (ridge_obs %>% arrange(park_factor))$PARK )) %>%
  select(PARK, park_factor, method) %>%
  ggplot() + 
  geom_point(aes(x=park_factor,y=PARK,color=method, shape=method), 
             size=4, alpha=0.75) + # alpha=0.85
  ylab("park") + xlab('2019 three-year park effect') +
  scale_shape_manual(values=c(16, 17, 18, 15, 19)) +
  scale_x_continuous(
    # limits = c(-0.1,0.2), 
    breaks = seq(-1,1,by=0.05)
  ) +
  scale_color_brewer(palette="Set1")
plot_pf_comparison_1719
ggsave("plot_pf_comparison_1719.png", plot_pf_comparison_1719, width=9, height=7)

### plot ridge PF 2017-2019
plot_pf_ridge_1719 = pf_1719 %>% 
  mutate(PARK = factor(PARK, levels = (ridge_obs %>% arrange(park_factor))$PARK )) %>%
  select(PARK, park_factor, method) %>%
  filter(method=="Ridge") %>%
  ggplot() + 
  geom_point(aes(x=park_factor,y=PARK), size=4, alpha=0.75,
             # color="dodgerblue2" 
             ) + 
  ylab("park") + xlab('2019 three-year park effect') +
  scale_shape_manual(values=c(16, 17, 18, 15, 19)) +
  scale_x_continuous(limits = c(-0.08,0.11), breaks = seq(-1,1,by=0.05)) +
  scale_color_brewer(palette="Set1")
plot_pf_ridge_1719
ggsave("plot_pf_ridge_1719.png", plot_pf_ridge_1719, width=9, height=7)





  

