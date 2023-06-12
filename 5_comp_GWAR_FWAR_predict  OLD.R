
source("0_load_stuff.R")

#################
### Load Data ###
#################

# df_war_comp = read_csv("df_FWAR_GWAR_AWAR_2010_2019_noParkFx_comp.csv")
df_war_comp = read_csv("df_FWAR_GWAR_2010_2019_pf_ridge.csv")

df_war_comp = 
  df_war_comp %>%
  mutate(
    GWAR_per_game = GWAR/N,
    FWAR_FIP_per_game = FWAR_FIP/N,
    FWAR_RA9_per_game = FWAR_RA9/N
  ) %>%
  arrange(PIT_NAME,YEAR)
df_war_comp

# df_war_comp %>% ggplot(aes(x=FWAR, y = GWAR)) + geom_point() + geom_abline()
# df_war_comp %>% ggplot(aes(x=FWAR_per_game, y = GWAR_per_game)) + geom_point() + geom_abline()

pit_exits = read_csv("df_pitcher_exits_2010_2019_pf_ridge.csv")
pit_exits_comp = 
  pit_exits %>%
  select(GAME_ID,YEAR,PIT_NAME,GWAR,INNING) %>%
  arrange(PIT_NAME, YEAR, GAME_ID) %>%
  left_join( df_war_comp %>% select(PIT_NAME, YEAR, FWAR_FIP_per_game, FWAR_RA9_per_game) ) %>%
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

########################### Empirical Bayes for FWAR_RA9 ###########################

##### training and testing dataframes: rows are games
df_byPitSzn = pit_exits_comp %>%
  group_by(PIT_NAME,YEAR) %>%
  summarise(
    FWAR_RA9 = sum(FWAR_RA9_per_game),
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
      FWAR_RA9 = sum(FWAR_RA9),
      # I = sum(num_innings),
      N = sum(num_games)
    ) %>%
    mutate(
      # X_over_I = FWAR_RA9/I,
      X_over_N = FWAR_RA9/N
    )
}
df_byPit(2010:2019)

df_EB.FWAR_RA9 <- function(sig_sq_hat, years, trim_cols=F) {
  # browser()
  df_EB.FWAR_RA9 = 
    df_byPit(years) %>%
    mutate(
      # mu_hat.FWAR_RA9 = mean(X_over_I),
      # full_var.FWAR_RA9 = var(X_over_I),
      mu_hat.FWAR_RA9 = mean(X_over_N),
      full_var.FWAR_RA9 = var(X_over_N),
      sig_sq_hat.FWAR_RA9 = sig_sq_hat,
      tau_sq_hat.FWAR_RA9 = full_var.FWAR_RA9 - mean(sig_sq_hat.FWAR_RA9)
    ) %>%
    mutate(
      # mu_hat_p_EB.FWAR_RA9 = ( FWAR_RA9/sig_sq_hat.FWAR_RA9 + mu_hat.FWAR_RA9/tau_sq_hat.FWAR_RA9 ) / ( I/sig_sq_hat.FWAR_RA9 + 1/tau_sq_hat.FWAR_RA9 ),
      mu_hat_p_EB.FWAR_RA9 = ( FWAR_RA9/sig_sq_hat.FWAR_RA9 + mu_hat.FWAR_RA9/tau_sq_hat.FWAR_RA9 ) / ( N/sig_sq_hat.FWAR_RA9 + 1/tau_sq_hat.FWAR_RA9 ),
    ) 
  if (any(df_EB.FWAR_RA9$tau_sq_hat.FWAR_RA9 <= 0)) {
    stop("tau_sq_hat can't be negative")
  }
  if (trim_cols) {
    df_EB.FWAR_RA9 = df_EB.FWAR_RA9 %>% select(-c(mu_hat.FWAR_RA9, full_var.FWAR_RA9, sig_sq_hat.FWAR_RA9, tau_sq_hat.FWAR_RA9))
  }
  df_EB.FWAR_RA9 %>% relocate(mu_hat_p_EB.FWAR_RA9, .after=X_over_N)
}

# var(df_byPit(tuning_train_years)$X_over_N) == 0.00240226
df_EB.FWAR_RA9(0.00240226, tuning_train_years)
df_EB.FWAR_RA9(0.0014, tuning_train_years)
df_EB.FWAR_RA9(0.000014, tuning_train_years)


##### tune sig_sq_hat
sig_sq_hats = seq(0.00240226, 0.0023, length=50)
rmse_vals = numeric(length(sig_sq_hats))
for (i in 1:length(sig_sq_hats)) {
  print(i)
  eb_i = df_EB.FWAR_RA9(sig_sq_hats[i], tuning_train_years, trim_cols=F)
  comp_i = left_join(
    df_byPit(tuning_val_years),
    eb_i %>% select(PIT_NAME, mu_hat_p_EB.FWAR_RA9)
  ) %>% drop_na()
  rmse_vals[i] = rmse(comp_i$X_over_N, comp_i$mu_hat_p_EB.FWAR_RA9)
}
plot(rmse_vals)
sig_sq_hat.FWAR_RA9 = sig_sq_hats[which.min(rmse_vals)]
print(sig_sq_hat.FWAR_RA9)

##### FWAR_RA9 Empirical Bayes with tuned sig_sq_hat.FWAR_RA9 !!!
# df_EB.FWAR_RA9(sig_sq_hat.FWAR_RA9, train_years)
df_EB_FWAR_RA9 = df_EB.FWAR_RA9(sig_sq_hat.FWAR_RA9, train_years, trim_cols = T) %>%
  select(PIT_NAME, mu_hat_p_EB.FWAR_RA9)
df_EB_FWAR_RA9

# %>% mutate(pit_rank_FWAR_RA9_EB = rank(-mu_hat_p_EB.FWAR_RA9))

########################### Empirical Bayes for FWAR_FIP ###########################

##### training and testing dataframes: rows are games
df_byPitSzn = pit_exits_comp %>%
  group_by(PIT_NAME,YEAR) %>%
  summarise(
    FWAR_FIP = sum(FWAR_FIP_per_game),
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
      FWAR_FIP = sum(FWAR_FIP),
      # I = sum(num_innings),
      N = sum(num_games)
    ) %>%
    mutate(
      # X_over_I = FWAR_FIP/I,
      X_over_N = FWAR_FIP/N
    )
}

df_EB.FWAR_FIP <- function(sig_sq_hat, years, trim_cols=F) {
  # browser()
  df_EB.FWAR_FIP = 
    df_byPit(years) %>%
    mutate(
      # mu_hat.FWAR_FIP = mean(X_over_I),
      # full_var.FWAR_FIP = var(X_over_I),
      mu_hat.FWAR_FIP = mean(X_over_N),
      full_var.FWAR_FIP = var(X_over_N),
      sig_sq_hat.FWAR_FIP = sig_sq_hat,
      tau_sq_hat.FWAR_FIP = full_var.FWAR_FIP - mean(sig_sq_hat.FWAR_FIP)
    ) %>%
    mutate(
      # mu_hat_p_EB.FWAR_FIP = ( FWAR_FIP/sig_sq_hat.FWAR_FIP + mu_hat.FWAR_FIP/tau_sq_hat.FWAR_FIP ) / ( I/sig_sq_hat.FWAR_FIP + 1/tau_sq_hat.FWAR_FIP ),
      mu_hat_p_EB.FWAR_FIP = ( FWAR_FIP/sig_sq_hat.FWAR_FIP + mu_hat.FWAR_FIP/tau_sq_hat.FWAR_FIP ) / ( N/sig_sq_hat.FWAR_FIP + 1/tau_sq_hat.FWAR_FIP ),
    ) 
  if (any(df_EB.FWAR_FIP$tau_sq_hat.FWAR_FIP <= 0)) {
    stop("tau_sq_hat can't be negative")
  }
  if (trim_cols) {
    df_EB.FWAR_FIP = df_EB.FWAR_FIP %>% select(-c(mu_hat.FWAR_FIP, full_var.FWAR_FIP, sig_sq_hat.FWAR_FIP, tau_sq_hat.FWAR_FIP))
  }
  df_EB.FWAR_FIP %>% relocate(mu_hat_p_EB.FWAR_FIP, .after=X_over_N)
}

# var(df_byPit(tuning_train_years)$X_over_N) == 0.001722214
df_EB.FWAR_FIP(0.0017, tuning_train_years)
df_EB.FWAR_FIP(0.0015, tuning_train_years)

##### tune sig_sq_hat
# sig_sq_hats = seq(0.001, 0.00172, length=150)
sig_sq_hats = seq(0.0016, 0.00172, length=50)
rmse_vals = numeric(length(sig_sq_hats))
for (i in 1:length(sig_sq_hats)) {
  print(i)
  eb_i = df_EB.FWAR_FIP(sig_sq_hats[i], tuning_train_years, trim_cols=F)
  comp_i = left_join(
    df_byPit(tuning_val_years),
    eb_i %>% select(PIT_NAME, mu_hat_p_EB.FWAR_FIP)
  ) %>% drop_na()
  rmse_vals[i] = rmse(comp_i$X_over_N, comp_i$mu_hat_p_EB.FWAR_FIP)
}
plot(rmse_vals)
sig_sq_hat.FWAR_FIP = sig_sq_hats[which.min(rmse_vals)]
print(sig_sq_hat.FWAR_FIP)

##### FWAR_FIP Empirical Bayes with tuned sig_sq_hat.FWAR_FIP !!!
# df_EB.FWAR_FIP(sig_sq_hat.FWAR_FIP, train_years)
df_EB_FWAR_FIP = df_EB.FWAR_FIP(sig_sq_hat.FWAR_FIP, train_years, trim_cols = T) %>%
  select(PIT_NAME, mu_hat_p_EB.FWAR_FIP)
df_EB_FWAR_FIP

# %>% mutate(pit_rank_FWAR_FIP_EB = rank(-mu_hat_p_EB.FWAR_FIP))

########################### s ########################### 

df_EB_train = left_join(df_EB_GWAR, df_EB_FWAR_RA9) %>% left_join(df_EB_FWAR_FIP)
df_EB_train

df_test = df_war_comp %>% 
  filter(YEAR > final_train_year) %>%
  left_join(df_EB_train) %>%
  drop_na() %>%
  mutate(
    rank_GWAR_pg = rank(-GWAR_per_game),
    rank_FWAR_RA9_pg = rank(-FWAR_RA9_per_game),
    rank_FWAR_FIP_pg = rank(-FWAR_FIP_per_game),
    rank_GWAR_pred = rank(-mu_hat_p_EB.GWAR),
    rank_FWAR_RA9_pred = rank(-mu_hat_p_EB.FWAR_RA9),
    rank_FWAR_FIP_pred = rank(-mu_hat_p_EB.FWAR_FIP),
  )
df_test

df_test_rmses = df_test %>%
  summarise(
    rmse_GWAR.hat_GWAR = rmse(rank_GWAR_pg, rank_GWAR_pred),
    # rmse_FWAR_RA9.hat_FWAR_RA9 = rmse(rank_FWAR_RA9_pg, rank_FWAR_RA9_pred),
    # rmse_FWAR_FIP.hat_FWAR_FIP = rmse(rank_FWAR_FIP_pg, rank_FWAR_FIP_pred),
    rmse_GWAR.hat_FWAR_RA9 = rmse(rank_GWAR_pg, rank_FWAR_RA9_pred),
    rmse_GWAR.hat_FWAR_FIP = rmse(rank_GWAR_pg, rank_FWAR_FIP_pred),
  ) %>% 
  pivot_longer(everything()) %>%
  arrange(value)
df_test_rmses
gt::gtsave(gt::gt(df_test_rmses), paste0(output_folder,"plot_test_EB_rmse.png"))

# df_test %>%
#   ggplot(aes(y=fct_reorder(PIT_NAME,-rank_GWAR_pg))) +
#   geom_point(aes(x=rank_GWAR_pg)) +
#   geom_point(aes(x=rank_GWAR_pred), color="red") +
#   geom_point(aes(x=rank_FWAR_RA9_pred), color="blue") +
#   geom_point(aes(x=rank_FWAR_FIP_pred), color="green")

################################################
### Look at specific people  ###
################################################

# undervalued_pitchers = read_csv("df_undervalued_pitchers.csv") %>% select(PIT_NAME) %>% mutate(uov = "undervalued")
# overvalued_pitchers = read_csv("df_overvalued_pitchers.csv")  %>% select(PIT_NAME) %>% mutate(uov = "overvalued")
# df_test_1 = df_test %>% left_join(undervalued_pitchers) %>% left_join(overvalued_pitchers)
df_test_1 = df_test %>%
  mutate(
    GW_pred_minus_FW_RA9_pred = mu_hat_p_EB.GWAR - mu_hat_p_EB.FWAR_RA9,
    GW_pred_minus_FW_FIP_pred = mu_hat_p_EB.GWAR - mu_hat_p_EB.FWAR_FIP,
  ) 
df_test_1

### 
df_test_uv_RA9 = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  head(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_uvRA9 = rmse(rank_GWAR_pg, rank_GWAR_pred),
    rmse_GWAR.hat_FWAR_RA9_uvRA9 = rmse(rank_GWAR_pg, rank_FWAR_RA9_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_uv_RA9

###
plot_uv5_ra9 = df_test_1 %>%
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank_GWAR_pg)) %>%
  head(n=5) %>%
  select(PIT_NAME, rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_RA9_pred) %>%
  pivot_longer(cols=c(rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_RA9_pred)) %>%
  mutate(
    # size_ = case_when(
    #   name == "rank_GWAR_pg" ~ 5,
    #   name == "rank_GWAR_pred" ~ 4.5,
    #   name == "rank_FWAR_RA9_pred" ~ 3.5,
    # ),
    size_ = name == "rank_GWAR_pg",
    name0 = name,
    name = case_when(
      name == "rank_GWAR_pg" ~ "observed 2019\nGWAR rank\n",
      name == "rank_GWAR_pred" ~ "predicted 2019\nrank from GWAR\n",
      name == "rank_FWAR_RA9_pred" ~ "predicted 2019\nrank from FWAR (RA/9)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, size=name0)) +
  # scale_size_manual(values = c(4,7)) + 
  scale_size_manual(values = c(4,6,8)) + 
  guides(size = "none") +
  # geom_point(aes(x=rank_GWAR_pg), size=5) +
  # geom_point(aes(x=rank_GWAR_pred), color="red", size=3) +
  # geom_point(aes(x=rank_FWAR_RA9_pred), color="blue", size=3) +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  labs(title = "5 most undervalued starting pitchers\naccording to GWAR relative to FWAR (RA/9)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
# plot_uv5_ra9
ggsave("plots/plot_test_EB_comp_uv5_ra9.png", plot_uv5_ra9, width=9, height=5)

### 
df_test_ov_RA9 = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  tail(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_ovRA9 = rmse(rank_GWAR_pg, rank_GWAR_pred),
    rmse_GWAR.hat_FWAR_RA9_ovRA9 = rmse(rank_GWAR_pg, rank_FWAR_RA9_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_ov_RA9

###
plot_ov5_ra9 = df_test_1 %>%
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank_GWAR_pg)) %>%
  tail(n=5) %>%
  select(PIT_NAME, rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_RA9_pred) %>%
  pivot_longer(cols=c(rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_RA9_pred)) %>%
  mutate(
    # size = case_when(
    #   name == "rank_GWAR_pg" ~ 5,
    #   name == "rank_GWAR_pred" ~ 4.5,
    #   name == "rank_FWAR_RA9_pred" ~ 4.5,
    # ),
    size_ = name == "rank_GWAR_pg",
    name = case_when(
      name == "rank_GWAR_pg" ~ "observed 2019\nGWAR rank\n",
      name == "rank_GWAR_pred" ~ "predicted 2019\nrank from GWAR\n",
      name == "rank_FWAR_RA9_pred" ~ "predicted 2019\nrank from FWAR (RA/9)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, size=size_)) +
  scale_size_manual(values = c(4,7)) + guides(size = "none") +
  # geom_point(aes(x=rank_GWAR_pg), size=5) +
  # geom_point(aes(x=rank_GWAR_pred), color="red", size=3) +
  # geom_point(aes(x=rank_FWAR_RA9_pred), color="blue", size=3) +
  # scale_size_manual(values = c(4,7)) + guides(size = "none") +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  labs(title = "5 most overvalued starting pitchers\naccording to GWAR relative to FWAR (RA/9)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
# plot_ov5_ra9
ggsave("plots/plot_test_EB_comp_ov5_ra9.png", plot_ov5_ra9, width=9, height=5)

### 
df_test_uv_FIP = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_FIP_pred) %>% 
  head(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_uvFIP = rmse(rank_GWAR_pg, rank_GWAR_pred),
    rmse_GWAR.hat_FWAR_FIP_uvFIP = rmse(rank_GWAR_pg, rank_FWAR_FIP_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_uv_FIP

###
plot_uv5_fip = df_test_1 %>%
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank_GWAR_pg)) %>%
  head(n=5) %>%
  select(PIT_NAME, rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_FIP_pred) %>%
  pivot_longer(cols=c(rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_FIP_pred)) %>%
  mutate(
    # size = case_when(
    #   name == "rank_GWAR_pg" ~ 5,
    #   name == "rank_GWAR_pred" ~ 4.5,
    #   name == "rank_FWAR_RA9_pred" ~ 4.5,
    # ),
    size_ = name == "rank_GWAR_pg",
    name0 = name,
    name = case_when(
      name == "rank_GWAR_pg" ~ "observed 2019\nGWAR rank\n",
      name == "rank_GWAR_pred" ~ "predicted 2019\nrank from GWAR\n",
      name == "rank_FWAR_FIP_pred" ~ "predicted 2019\nrank from FWAR (FIP)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, size=name0)) +
  scale_size_manual(values = c(4,6,8)) + 
  guides(size = "none") +
  # geom_point(aes(x=rank_GWAR_pg), size=5) +
  # geom_point(aes(x=rank_GWAR_pred), color="red", size=3) +
  # geom_point(aes(x=rank_FWAR_RA9_pred), color="blue", size=3) +
  # scale_size_manual(values = c(4,7)) + guides(size = "none") +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  labs(title = "5 most undervalued starting pitchers\naccording to GWAR relative to FWAR (FIP)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
# plot_uv5_fip
ggsave("plots/plot_test_EB_comp_uv5_fip.png", plot_uv5_fip, width=9, height=5)

### 
df_test_ov_FIP = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_FIP_pred) %>% 
  tail(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_ovFIP = rmse(rank_GWAR_pg, rank_GWAR_pred),
    rmse_GWAR.hat_FWAR_FIP_ovFIP = rmse(rank_GWAR_pg, rank_FWAR_FIP_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_ov_FIP

###
plot_ov5_fip = df_test_1 %>%
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank_GWAR_pg)) %>%
  tail(n=5) %>%
  select(PIT_NAME, rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_FIP_pred) %>%
  pivot_longer(cols=c(rank_GWAR_pg, rank_GWAR_pred, rank_FWAR_FIP_pred)) %>%
  mutate(
    # size = case_when(
    #   name == "rank_GWAR_pg" ~ 5,
    #   name == "rank_GWAR_pred" ~ 4.5,
    #   name == "rank_FWAR_RA9_pred" ~ 4.5,
    # ),
    size_ = name == "rank_GWAR_pg",
    name = case_when(
      name == "rank_GWAR_pg" ~ "observed 2019\nGWAR rank\n",
      name == "rank_GWAR_pred" ~ "predicted 2019\nrank from GWAR\n",
      name == "rank_FWAR_FIP_pred" ~ "predicted 2019\nrank from FWAR (FIP)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, size=size_)) +
  # geom_point(aes(x=rank_GWAR_pg), size=5) +
  # geom_point(aes(x=rank_GWAR_pred), color="red", size=3) +
  # geom_point(aes(x=rank_FWAR_RA9_pred), color="blue", size=3) +
  scale_size_manual(values = c(4,7)) + guides(size = "none") +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  labs(title = "5 most overvalued starting pitchers\naccording to GWAR relative to FWAR (FIP)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
# plot_ov5_fip
ggsave("plots/plot_test_EB_comp_ov5_fip.png", plot_ov5_fip, width=9, height=5)

###
df_test_rmses_uvov = bind_rows(df_test_uv_RA9,df_test_ov_RA9,df_test_uv_FIP,df_test_ov_FIP)
df_test_rmses_uvov
gt::gtsave(gt::gt(df_test_rmses_uvov), paste0(output_folder,"plot_test_EB_rmse_uvov.png"))




