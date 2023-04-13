
source("0_load_stuff.R")

#################
### Load Data ###
#################

# df_war_comp = read_csv("df_FWAR_GWAR_AWAR_2010_2019_noParkFx_comp.csv")
df_war_comp = read_csv("df_FWAR_GWAR_AWAR_2010_2019_pf_TRUE_ac_FALSE_comp.csv")

df_war_comp = 
  df_war_comp %>%
  mutate(
    GWAR_per_game = GWAR/N,
    FWAR_per_game = FWAR/N
  ) %>%
  arrange(PIT_NAME,YEAR)
df_war_comp

# df_war_comp %>% ggplot(aes(x=FWAR, y = GWAR)) + geom_point() + geom_abline()
# df_war_comp %>% ggplot(aes(x=FWAR_per_game, y = GWAR_per_game)) + geom_point() + geom_abline()

pit_exits = read_csv("df_pitcher_exits_2010_2019_pf_TRUE_ac_FALSE.csv")
pit_exits_comp = 
  pit_exits %>%
  select(GAME_ID,YEAR,PIT_NAME,GWAR,INNING) %>%
  arrange(PIT_NAME, YEAR, GAME_ID) %>%
  left_join( df_war_comp %>% select(PIT_NAME, YEAR, FWAR_per_game) ) %>%
  drop_na()
pit_exits_comp

# ########## training and testing dataframes: rows are pitcher-seasons ########## 
# final_train_year = 2018 #FIXME
# df_train_byPitSzn = df_war_comp %>% filter(YEAR <= final_train_year)
# df_test_byPitSzn = df_war_comp %>% filter(YEAR > final_train_year)


################################################
### Empirical Bayes on game-by-game dataset  ###
################################################

################ training and testing dataframes: rows are games ###############
final_train_year = 2018 #FIXME
tuning_train_years = 2010:(final_train_year-2)
tuning_val_years = (final_train_year-1):final_train_year
train_years = 2010:final_train_year
test_years = (final_train_year+1):2019

df_byGame = pit_exits_comp
df_train_byGame = df_byGame %>% filter(YEAR <= final_train_year)


#################### Empirical Bayes for GWAR, game-by-game ####################
df_EB_byGame = 
  df_train_byGame %>%
  mutate(
    full_var.GWAR = var(GWAR),
  ) %>%
  group_by(PIT_NAME) %>%
  summarise(
    full_var.GWAR = unique(full_var.GWAR),
    var.GWAR = var(GWAR),
    sum.GWAR = sum(GWAR),
    mean.GWAR = mean(GWAR),
    num_games = n(),
  ) %>%
  ungroup() %>%
  mutate(
    mu_hat.GWAR = mean(mean.GWAR),
    sig_sq_hat.GWAR = var.GWAR,
    tau_sq_hat.GWAR = full_var.GWAR - mean(sig_sq_hat.GWAR)
  ) %>%
  mutate(
    mu_hat_p_EB.GWAR = ( sum.GWAR/sig_sq_hat.GWAR + mu_hat.GWAR/tau_sq_hat.GWAR ) / ( num_games/sig_sq_hat.GWAR + 1/tau_sq_hat.GWAR ),
  ) 
df_EB_byGame

df_EB_GWAR = df_EB_byGame %>%
  group_by(PIT_NAME) %>%
  summarise(mu_hat_p_EB.GWAR = unique(mu_hat_p_EB.GWAR))
df_EB_GWAR




# df_byGame_EB = df_byGame %>% 
#   left_join(df_EB_byGame) %>%
#   mutate(
#     mu_hat_p_EB.GWAR = ( sum.GWAR/sig_sq_hat.GWAR + mu_hat.GWAR/tau_sq_hat.GWAR ) / ( num_games/sig_sq_hat.GWAR + 1/tau_sq_hat.GWAR ),
#   ) 
# df_byGame_EB
# 
# df_byGame_EB_1 = 
#   df_byGame_EB %>% 
#   select(GAME_ID, YEAR, PIT_NAME, mu_hat_p_EB.GWAR, FWAR_per_game, GWAR) %>%
#   # select(GAME_ID, YEAR, PIT_NAME, mu_hat_p_EB.GWAR, mu_hat_p_EB.FWAR, FWAR_per_game, GWAR) %>%
#   drop_na()
# df_byGame_EB_1

########################### Empirical Bayes for FWAR ###########################

##### training and testing dataframes: rows are games
df_byPitSzn = pit_exits_comp %>%
  group_by(PIT_NAME,YEAR) %>%
  summarise(
    FWAR = sum(FWAR_per_game),
    num_games = n(),
    num_innings = sum(INNING)
  ) %>%
  ungroup()
df_byPitSzn

df_byPit <- function(years) {
  df_byPitSzn %>%
    filter(YEAR %in% years) %>%
    group_by(PIT_NAME) %>%
    summarise(
      FWAR = sum(FWAR),
      # I = sum(num_innings),
      N = sum(num_games)
    ) %>%
    mutate(
      # X_over_I = FWAR/I,
      X_over_N = FWAR/N
    )
}

df_EB.FWAR <- function(sig_sq_hat, years, trim_cols=F) {
  # browser()
  df_EB.FWAR = 
    df_byPit(years) %>%
    mutate(
      # mu_hat.FWAR = mean(X_over_I),
      # full_var.FWAR = var(X_over_I),
      mu_hat.FWAR = mean(X_over_N),
      full_var.FWAR = var(X_over_N),
      sig_sq_hat.FWAR = sig_sq_hat,
      tau_sq_hat.FWAR = full_var.FWAR - mean(sig_sq_hat.FWAR)
    ) %>%
    mutate(
      # mu_hat_p_EB.FWAR = ( FWAR/sig_sq_hat.FWAR + mu_hat.FWAR/tau_sq_hat.FWAR ) / ( I/sig_sq_hat.FWAR + 1/tau_sq_hat.FWAR ),
      mu_hat_p_EB.FWAR = ( FWAR/sig_sq_hat.FWAR + mu_hat.FWAR/tau_sq_hat.FWAR ) / ( N/sig_sq_hat.FWAR + 1/tau_sq_hat.FWAR ),
    ) 
  if (any(df_EB.FWAR$tau_sq_hat.FWAR <= 0)) {
    stop("tau_sq_hat can't be negative")
  }
  if (trim_cols) {
    df_EB.FWAR = df_EB.FWAR %>% select(-c(mu_hat.FWAR, full_var.FWAR, sig_sq_hat.FWAR, tau_sq_hat.FWAR))
  }
  df_EB.FWAR %>% relocate(mu_hat_p_EB.FWAR, .after=X_over_N)
}

# 0.00172
# df_EB.FWAR(0.00172, tuning_train_years)
# df_EB.FWAR(0.0015, tuning_train_years)
# df_EB.FWAR(0.001, tuning_train_years)

##### tune sig_sq_hat
# sig_sq_hats = seq(0.001, 0.00172, length=150)
sig_sq_hats = seq(0.00155, 0.00172, length=150)
rmse_vals = numeric(length(sig_sq_hats))
for (i in 1:length(sig_sq_hats)) {
  print(i)
  eb_i = df_EB.FWAR(sig_sq_hats[i], tuning_train_years, trim_cols=F)
  comp_i = left_join(
    df_byPit(tuning_val_years),
    eb_i %>% select(PIT_NAME, mu_hat_p_EB.FWAR)
  ) %>% drop_na()
  rmse_vals[i] = rmse(comp_i$X_over_N, comp_i$mu_hat_p_EB.FWAR)
}
plot(rmse_vals)
sig_sq_hat.FWAR = sig_sq_hats[which.min(rmse_vals)]
print(sig_sq_hat.FWAR)

##### FWAR Empirical Bayes with tuned sig_sq_hat.FWAR !!!
# df_EB.FWAR(sig_sq_hat.FWAR, train_years)
df_EB_FWAR = df_EB.FWAR(sig_sq_hat.FWAR, train_years, trim_cols = T) %>%
  select(PIT_NAME, mu_hat_p_EB.FWAR)
df_EB_FWAR

# %>% mutate(pit_rank_FWAR_EB = rank(-mu_hat_p_EB.FWAR))

########################### s ########################### 

df_EB_train = left_join(df_EB_GWAR, df_EB_FWAR)
df_EB_train

df_test = df_war_comp %>% 
  filter(YEAR > final_train_year) %>%
  left_join(df_EB_train) %>%
  drop_na() %>%
  mutate(
    rank_GWAR_pg = rank(-GWAR_per_game),
    rank_FWAR_pg = rank(-FWAR_per_game),
    rank_GWAR_pred = rank(-mu_hat_p_EB.GWAR),
    rank_FWAR_pred = rank(-mu_hat_p_EB.FWAR),
  )
df_test


rmse(df_test$rank_FWAR_pg, df_test$rank_FWAR_pred) ### old way: predict FWAR from FWAR
rmse(df_test$rank_GWAR_pg, df_test$rank_FWAR_pred) ### value lost since we should be predicting GWAR
rmse(df_test$rank_GWAR_pg, df_test$rank_GWAR_pred) ### what should happen: predict GWAR from GWAR (or, GWAR+FWAR)


