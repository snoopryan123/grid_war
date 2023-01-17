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

### data matrix 2017-2019 
X_df = park_df %>% filter(YEAR >= 2017) %>% 
  mutate(OT_YR = paste0(OFF_TEAM_ID, YEAR),
         DT_YR = paste0(DEF_TEAM_ID, YEAR))

###################################
### Run models on observed data ###
###################################

# ridge3_obs = glmnet(
#   x = model.matrix(~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=X_df),
#   y = X_df$INN_RUNS, alpha = 0, lambda = 0.25, family="gaussian"
# )
# coeffs_ridge3_obs = coef(ridge3_obs)[,1]
# coeffs_pk3_obs  = coeffs_ridge3_obs[str_detect(names(coeffs_ridge3_obs), "PARK")]
# coeffs_pk3_obs_df = as_tibble(coeffs_pk3_obs) %>%
#   rename(fitted_coeff = value) %>%
#   mutate(PARK = str_sub(names(coeffs_pk3_obs), -5, -1) ) %>%
#   bind_rows(tibble(fitted_coeff = 0, PARK="ANA01")) %>% arrange(PARK) %>%
#   mutate(park_factor = fitted_coeff - mean(fitted_coeff))
# coeffs_pk3_obs_df$method = "Ridge"
# write_csv(coeffs_pk3_obs_df, "Ridge_PF_2019_3yr.csv")
# saveRDS(ridge3_obs, "ridge3_obs.rds")
ridge3_obs = readRDS("ridge3_obs.rds")
Ridge_df = read_csv("Ridge_PF_2019_3yr.csv")
data.frame(Ridge_df)

# ols_obs = lm(INN_RUNS ~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_df)
# olscoeffs = ols_obs$coefficients[str_detect(names(ols_obs$coefficients), "PARK")]
# ols_obs_df = tibble(fitted_coeff = c(0,olscoeffs), PARK = Ridge_df$PARK) %>%
#   mutate(park_factor = fitted_coeff - mean(fitted_coeff)) %>%
#   mutate(method = "OLS")
# write_csv(ols_obs_df, "OLS_PF_2019_3yr.csv")
# saveRDS(ols_obs, "ols_obs.rds")
ols_obs_df = read_csv("OLS_PF_2019_3yr.csv")
ols_obs = readRDS("ols_obs.rds")


##### Fangraphs 2019 3-year park factors (from their website)
# Fangraphs_content <- read_html("https://www.fangraphs.com/guts.aspx?type=pf&season=2019&teamid=0&sort=3,d")
# Fangraphs_df <- html_table(Fangraphs_content)[[9]] %>%
#   rename(PF_3yr_2019 = `3yr`) %>%
#   select(Season, Team, PF_3yr_2019) %>%
#   mutate(park_factor = (PF_3yr_2019 - 100)/100) %>%
#   mutate(PARK = case_when(
#     Team == "Rockies" ~ "DEN02",
#     Team == "Rangers" ~ "ARL02",
#     Team == "Reds" ~ "CIN09",
#     Team == "Red Sox" ~ "BOS07",
#     Team == "Nationals" ~ "WAS11",
#     Team == "Royals" ~ "KAN06",
#     Team == "Braves" ~ "ATL03",
#     Team == "Indians" ~ "CLE08",
#     Team == "Phillies" ~ "PHI13",
#     Team == "Angels" ~ "ANA01",
#     Team == "Diamondbacks" ~ "PHO01",
#     Team == "Orioles" ~ "BAL12",
#     Team == "Tigers" ~ "DET05",
#     Team == "Brewers" ~ "MIL06",
#     Team == "Astros" ~ "HOU03",
#     Team == "Twins" ~ "MIN04",
#     Team == "Yankees" ~ "NYC21",
#     Team == "Blue Jays" ~ "TOR02",
#     Team == "White Sox" ~ "CHI12",
#     Team == "Pirates" ~ "PIT08",
#     Team == "Cubs" ~ "CHI11",
#     Team == "Padres" ~ "SAN02",
#     Team == "Cardinals" ~ "STL10",
#     Team == "Giants" ~ "SFO03",
#     Team == "Rays" ~ "STP01",
#     Team == "Mariners" ~ "SEA03",
#     Team == "Dodgers" ~ "LOS03",
#     Team == "Marlins" ~ "MIA02",
#     Team == "Athletics" ~ "OAK01",
#     Team == "Mets" ~ "NYC20"
#   ))
# Fangraphs_df$method = "Fangraphs"
# write_csv(Fangraphs_df, "Fangraphs_PF_2019_3yr.csv")
Fangraphs_df = read_csv("Fangraphs_PF_2019_3yr.csv")
data.frame(Fangraphs_df)

##### ESPN 2019 1-year park factors (from their website)
# Espn_content <- read_html("https://www.espn.com/mlb/stats/parkfactor/_/year/2019")
# Espn_df <- html_table(Espn_content)[[1]] %>%
#   select(X2, X3) %>%
#   filter(row_number() >= 3) %>%
#   rename(Team = X2, Pf = X3) %>%
#   mutate(park_factor = (as.numeric(Pf) - 1)) %>%
#   mutate(Team = ifelse(Pf == 0.966, "CHC", Team),
#          Team = ifelse(Pf == 0.931, "CWS", Team)) %>%
#   mutate(PARK = case_when(
#     Team == "null(Denver, Colorado)" ~ "DEN02",
#     Team == "Globe Life Park in Arlington(Arlington, Texas)" ~ "ARL02",
#     Team == "null(Cincinnati, Ohio)" ~ "CIN09",
#     Team == "null(Boston, Massachusetts)" ~ "BOS07",
#     Team == "null(Washington, District of Columbia)" ~ "WAS11",
#     Team == "null(Kansas City, Missouri)" ~ "KAN06",
#     Team == "null(Atlanta, Georgia)" ~ "ATL03",
#     Team == "null(Cleveland, Ohio)" ~ "CLE08",
#     Team == "null(Philadelphia, Pennsylvania)" ~ "PHI13",
#     Team == "null(Anaheim, California)" ~ "ANA01",
#     Team == "null(Phoenix, Arizona)" ~ "PHO01",
#     Team == "null(Baltimore, Maryland)" ~ "BAL12",
#     Team == "null(Detroit, Michigan)" ~ "DET05",
#     Team == "Miller Park(Milwaukee, Wisconsin)" ~ "MIL06",
#     Team == "null(Houston, Texas)" ~ "HOU03",
#     Team == "null(Minneapolis, Minnesota)" ~ "MIN04",
#     Team == "null(Bronx, New York)" ~ "NYC21",
#     Team == "Rogers Centre(Toronto, Ontario)" ~ "TOR02",
#     Team == "null(Pittsburgh, Pennsylvania)" ~ "PIT08",
#     Team == "null(San Diego, California)" ~ "SAN02",
#     Team == "null(St. Louis, Missouri)" ~ "STL10",
#     Team == "null(San Francisco, California)" ~ "SFO03",
#     Team == "null(St. Petersburg, Florida)" ~ "STP01",
#     Team == "null(Seattle, Washington)" ~ "SEA03",
#     Team == "null(Los Angeles, California)" ~ "LOS03",
#     Team == "Marlins Park(Miami, Florida)" ~ "MIA02",
#     Team == "null(Queens, New York)" ~ "NYC20",
#     Team == "CWS" ~ "CHI12",
#     Team == "CHC" ~ "CHI11",
#     Team == "Athletics" ~ "OAK01",
#   )) %>%
#   bind_rows(tibble(PARK = "OAK01", park_factor=NA))
# Espn_df$method = "ESPN"
# write_csv(Espn_df, "Espn_Pf_2019.csv")
# Espn_df_2019 = read_csv("Espn_Pf_2019.csv")
# data.frame(Espn_df_2019)

##### ESPN 2019 3-year park factors (computed here)
# games_df = X_df %>%
#   group_by(GAME_ID, BAT_HOME_IND) %>%
#   slice_tail() %>%
#   ungroup() 
# espn_df1 = games_df %>%
#   group_by(HOME_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 1) %>%
#   summarise(homeRS = sum(CUM_RUNS),
#             homeG = n()) %>%
#   rename(TEAM = HOME_TEAM_ID) 
# espn_df2 = games_df %>%
#   group_by(HOME_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 0) %>%
#   summarise(homeRA = sum(CUM_RUNS)) %>%
#   rename(TEAM = HOME_TEAM_ID)
# espn_df3 = games_df %>%
#   group_by(AWAY_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 0) %>%
#   summarise(roadRS = sum(CUM_RUNS),
#             roadG = n()) %>%
#   rename(TEAM = AWAY_TEAM_ID)
# espn_df4 = games_df %>%
#   group_by(AWAY_TEAM_ID) %>%
#   filter(BAT_HOME_IND == 1) %>%
#   summarise(roadRA = sum(CUM_RUNS)) %>%
#   rename(TEAM = AWAY_TEAM_ID)
# espn_df5 = espn_df1 %>% left_join(espn_df2) %>%
#   left_join(espn_df3) %>% left_join(espn_df4) %>%
#   mutate(Pf = ((homeRS + homeRA)/homeG) / ((roadRS + roadRA)/roadG)  ) %>%
#   mutate(park_factor = Pf - 1) %>%
#   arrange(desc(park_factor)) %>%
#   mutate(method = "ESPN") %>%
#   left_join(X_df %>% select(HOME_TEAM_ID, PARK) %>% 
#               rename(TEAM = HOME_TEAM_ID) %>% distinct() %>%
#               filter( !(TEAM == "TBA" & PARK == "NYC20") ) %>%
#               filter( !(TEAM == "MIA" & PARK == "MIL06") ) %>%
#               filter( !(TEAM == "HOU" & PARK == "STP01") ) )
# data.frame(espn_df5)
# write_csv(espn_df5, "Espn_Pf_2019_3yr.csv")
Espn_df_2019_3yr = read_csv("Espn_Pf_2019_3yr.csv")
data.frame(Espn_df_2019_3yr)


pf_df = bind_rows(Ridge_df, Fangraphs_df, Espn_df_2019_3yr, ols_obs_df) #Espn_df_2019
pf_df$PARK = factor(pf_df$PARK, levels = (Ridge_df %>% arrange(park_factor))$PARK)
  
plot_pf_comparison_obs = pf_df %>% 
  select(PARK, park_factor, method) %>%
  ggplot() + 
  geom_point(aes(x=park_factor, y=PARK, color=method, shape=method), 
             size=4, alpha=0.85) +
  ylab("park") + xlab('2019 three-year park effect') +
  scale_color_brewer(palette="Set1")
plot_pf_comparison_obs
ggsave("plot_pf_comparison_obs.png", plot_pf_comparison_obs, width=9, height=7)


plot_pf_obs = pf_df %>% 
  select(PARK, park_factor, method) %>%
  filter(method == "Ridge") %>%
  ggplot() + 
  geom_point(aes(x=park_factor, y=PARK), 
             size=4, alpha=1, color="dodgerblue2") +
  ylab("park") + xlab('2019 three-year park effect')
plot_pf_obs
ggsave("plot_pf_obs.png", plot_pf_obs, width=8, height=7)


#######################
### RMSE comparison ###
#######################

rmse <- function(x,y) { sqrt(mean( (x-y)**2  )) }
mae <- function(x,y) {   mean( abs(x-y) ) }

###
ridge3_preds = predict(
  ridge3_obs, 
  model.matrix(~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=X_df)
)
rmse(ridge3_preds, X_df$INN_RUNS)

###
ols_preds = predict(ols_obs, X_df)
rmse(ols_preds, X_df$INN_RUNS)

###
X_df_Fangraphs = X_df %>% left_join(Fangraphs_df %>% select(PARK, park_factor))
Fangraphs_model = lm(
  INN_RUNS ~ park_factor + factor(OT_YR) + factor(DT_YR),
  data = X_df_Fangraphs
)
Fangraphs_pk_preds = predict(Fangraphs_model, X_df_Fangraphs)
rmse(Fangraphs_pk_preds, X_df$INN_RUNS)

###
X_df_espn = X_df %>% left_join(Espn_df_2019_3yr %>% select(PARK, park_factor))
espn_model = lm(
  INN_RUNS ~ park_factor + factor(OT_YR) + factor(DT_YR),
  data = X_df_espn
)
espn_pk_preds = predict(espn_model, X_df_espn)
rmse(espn_pk_preds, X_df$INN_RUNS)

###
Xoq5 = X_df %>% group_by(OT_YR) %>% 
  summarise(toq = sum(INN_RUNS)) %>%
  mutate(toq = (toq - toq[1]) / (2*sd(toq)) )
Xdq5 = X_df %>% group_by(DT_YR) %>% 
  summarise(tdq = sum(INN_RUNS)) %>%
  mutate(tdq = (tdq - tdq[1]) / (2*sd(tdq)) )
### get PARK
X_pk = X_df %>% left_join(Xoq5) %>% left_join(Xdq5) 

# lm_park_i = lm(y - toq - tdq ~ factor(PARK) , data=X_pk_i)
lm_park_i = lm(INN_RUNS ~ toq + tdq + factor(PARK) , data=X_pk)
coeffs_pk5 = coefficients(lm_park_i)[str_detect(names(coefficients(lm_park_i)), "PARK")]

rmse(predict(lm_park_i, X_pk), X_df$INN_RUNS)

### overall mean
ybar = mean(X_df$INN_RUNS)


rmse(ybar, X_df$INN_RUNS)
rmse(ridge3_preds, X_df$INN_RUNS)
rmse(Fangraphs_pk_preds, X_df$INN_RUNS)
rmse(espn_pk_preds, X_df$INN_RUNS)

# mae(ridge3_preds, X_df$INN_RUNS)
# mae(Fangraphs_pk_preds, X_df$INN_RUNS)
# mae(espn_pk_preds, X_df$INN_RUNS)





  
  
