
source("0_load_stuff.R")

#################
### Load Data ###
#################

df_war_pitSzn = read_csv("df_FWAR_GWAR_2010_2019_pf_ridge.csv") %>% arrange(PIT_NAME,YEAR)
df_war_pitSzn

pit_exits = read_csv("df_pitcher_exits_2010_2019_pf_ridge.csv")
df_war_pitExits = 
  pit_exits %>%
  select(GAME_ID,YEAR,PIT_NAME,GWAR,INNING) %>%
  arrange(PIT_NAME, YEAR, GAME_ID) %>%
  left_join(df_war_pitSzn %>% select(PIT_NAME,YEAR,FWAR_FIP,FWAR_RA9,N_fg,N)) %>%
  drop_na()
df_war_pitExits

################################################
### Empirical Bayes on game-by-game dataset  ###
################################################

################ training and testing dataframes: rows are games ###############
final_train_year = 2018 #FIXME
tuning_train_years = 2010:(final_train_year-2)
tuning_val_years = (final_train_year-1):final_train_year
train_years = 2010:final_train_year
test_years = (final_train_year+1):2019

df_byGame = df_war_pitExits
df_train_byGame = df_byGame %>% filter(YEAR <= final_train_year)

#################### Empirical Bayes for GWAR, game-by-game ####################

### histograms of a pitcher's game-level GWAR
unique(df_train_byGame$PIT_NAME)
pitnames = c(
  "Clayton Kershaw", "Ervin Santana", "Tanner Roark", "Rick Porcello"
)
# pitnames = c("Clayton Kershaw", "Yovani Gallardo", 
#              "Jake Arrieta", "Ervin Santana")
# plot_Xpg_GWAR = df_train_byGame %>%
#   # filter(PIT_NAME == "Clayton Kershaw") %>%
#   filter(PIT_NAME %in% pitnames) %>%
#   ggplot(aes(x=GWAR)) +
#   facet_wrap(~PIT_NAME, nrow=1) +
#   xlab("game GWAR") +
#   geom_histogram(fill="black")
plot_Xpg_GWAR = df_train_byGame %>%
  # filter(PIT_NAME == "Clayton Kershaw") %>%
  filter(PIT_NAME %in% pitnames) %>%
  ggplot(aes(x=GWAR, fill=PIT_NAME)) +
  xlab("game GWAR") +
  guides(fill=guide_legend(title=TeX(paste("pitcher")) )) +
  geom_density(alpha=0.5)
plot_Xpg_GWAR
ggsave(paste0(output_folder,"plot_Xpg_GWAR.png"),
       plot_Xpg_GWAR,width=8,height=5)



fit_params.EB.mle_GWAR <- function(epsilon=1e-5, df0=df_train_byGame) {
  ### first, fit the MLE hyperparams mu.hat, tau.sq.hat, sig.sq.p.hat
  ### initialize
  iter = 1
  df = df0 %>% mutate(X = GWAR) %>% select(PIT_NAME, X)
  mu.hat = df %>% group_by(PIT_NAME) %>% summarise(mu = mean(X)) %>% summarise(mu = mean(mu))
  full.var = var(df$X)
  params.df = df %>% group_by(PIT_NAME) %>% 
    summarise(sig.sq.p.hat = var(X)) %>% 
    mutate(sig.sq.p.hat = mean(sig.sq.p.hat))
  params.df$mu.hat = mu.hat$mu
  params.df$tau.sq.hat = full.var - mean(params.df$sig.sq.p.hat)
  params.df
  train.df = df %>% left_join(params.df) 
  train.df
  
  ### iteratively solve for MLE mu.hat, tau.sq.hat, sig.sq.p.hat
  while (TRUE) {
    print(paste0("running iteration ", iter))
    ### save previous values
    train.df.prev = train.df
    ### solve for mu.hat
    mu.hat = train.df %>%
      summarise(
        t1 = sum(X / (sig.sq.p.hat + tau.sq.hat)),
        t2 = sum(1 / (sig.sq.p.hat + tau.sq.hat)),
      ) %>%
      mutate(
        mu.hat = t1/t2
      )
    mu.hat = mu.hat$mu.hat
    mu.hat
    train.df$mu.hat = mu.hat
    ### solve for tau.sq.hat
    solve.for.tau.sq <- function(tau.sq) {
      temp.df = 
        train.df %>% 
        select(-tau.sq.hat) %>% 
        mutate(
          t1 = 1 / (sig.sq.p.hat + tau.sq),
          t1.sq = t1^2,
          dd = (X - mu.hat)^2,
          t2 = dd*t1.sq,
          diff = t1 - t2
        ) 
      temp.df
      (temp.df %>% summarise(ans = sum(diff)))$ans
    }
    tau.sq.hat = uniroot(solve.for.tau.sq, c(0.0001,0.02))
    tau.sq.hat = tau.sq.hat$root
    tau.sq.hat
    train.df$tau.sq.hat = tau.sq.hat
    ### solve for sig.sq.p
    pitchers = unique(train.df$PIT_NAME)
    sig.sq.p.vec = numeric(length(pitchers))
    for (i in 1:length(pitchers)) {
      if (i %% 50 == 0) print(paste0("computing sig.sq.p.hat for pitcher i=",i,"/",length(pitchers)))
      pit_i = pitchers[i]
      train.df_i = train.df %>% filter(PIT_NAME == pit_i)
      
      solve.for.sig.sq <- function(sig.sq.p) {
        temp.df = 
          train.df_i %>% 
          select(-sig.sq.p.hat) %>% 
          mutate(
            dd = (X - mu.hat)^2,
            t1 = 1 / (sig.sq.p + tau.sq.hat),
            t1.sq = t1^2,
            t2 = dd*t1.sq,
            diff = t1 - t2
          ) 
        temp.df
        (temp.df %>% summarise(ans = sum(diff)))$ans
      }
      # solve.for.sig.sq(0.01)
      sig.sq.p.hat = uniroot(solve.for.sig.sq, c(0.0001,1))
      sig.sq.p.hat = sig.sq.p.hat$root
      sig.sq.p.vec[i] = sig.sq.p.hat
    }
    train.df = train.df %>% select(-sig.sq.p.hat) %>% left_join(
      tibble(PIT_NAME = pitchers, sig.sq.p.hat = sig.sq.p.vec)
    )
    ### check convergence
    mu.hat.conv = abs(unique(train.df$mu.hat) - unique(train.df.prev$mu.hat))
    tau.sq.hat.conv = abs(unique(train.df$tau.sq.hat) - unique(train.df.prev$tau.sq.hat))
    sig.sq.hat.df.prev = train.df.prev %>% group_by(PIT_NAME) %>% summarise(sig.sq.p.hat = unique(sig.sq.p.hat))
    sig.sq.hat.df = train.df %>% group_by(PIT_NAME) %>% summarise(sig.sq.p.hat = unique(sig.sq.p.hat))
    sig.sq.hat.conv = max(abs(sig.sq.hat.df$sig.sq.p.hat - sig.sq.hat.df.prev$sig.sq.p.hat))
    conv.num = max(c(mu.hat.conv, tau.sq.hat.conv, sig.sq.hat.conv))
    print(paste0("conv.num = ", conv.num))
    if (conv.num < epsilon) {
      break
    } 
    ### increment 
    iter = iter + 1
  }
  ### dataframe of fitted MLE hyperparams 
  df.hyperparams = train.df %>% distinct(PIT_NAME, mu.hat, tau.sq.hat, sig.sq.p.hat)
  df.hyperparams
  
  ### now, get the empirical Bayes pitcher quality estimates
  df.mu.hat.p = train.df %>%
    group_by(PIT_NAME) %>%
    summarise(
      tA = sum(X)/sig.sq.p.hat,
      tB = mu.hat/tau.sq.hat,
      tC = n()/sig.sq.p.hat,
      tD = 1/tau.sq.hat
    ) %>%
    distinct() %>%
    summarise(
      mu.hat.p = (tA + tB)/(tC + tD)
    )
  df.mu.hat.p
  
  ### result dataframe
  df.result = df.mu.hat.p %>% left_join(df.hyperparams)
  df.result$metric = "GWAR"
  df.result
  df.result %>% arrange(-mu.hat.p)
}

df.EB.GWAR = fit_params.EB.mle_GWAR()
df.EB.GWAR.1 = left_join(
  df.EB.GWAR,
  df_train_byGame %>% group_by(PIT_NAME) %>% summarise(mean_GWAR=mean(GWAR)) 
) %>%
  left_join(
    df_train_byGame %>% distinct(PIT_NAME,YEAR,N) %>% group_by(PIT_NAME) %>% summarise(N = sum(N))
  )

plot_EBGWAR = df.EB.GWAR.1 %>%
  ggplot(aes(x=mean_GWAR, y=mu.hat.p, color=N, size=N)) +
  ylab("mean game GWAR") +
  xlab(TeX("$\\hat{\\mu}_p$")) +
  geom_abline(intercept=0, slope=1, linewidth=1, linetype="dashed", color="gray60") +
  geom_point(alpha=0.6)
# plot_EBGWAR
ggsave(paste0(output_folder,"plot_EB_GWAR.png"), plot_EBGWAR, width=7, height=5)

#################### Empirical Bayes for FWAR ####################

### the following is irrelevant because we dont observe game-level FWAR:
# ### histograms of a pitcher's game-level FWAR
# unique(df_train_byGame$PIT_NAME)
# pitnames = c("Clayton Kershaw", "Yovani Gallardo")#, 
#              #"Jake Arrieta", "Ervin Santana")
# plot_Xpg_FWAR = df_train_byGame %>%
#   select(PIT_NAME, FWAR_FIP, FWAR_RA9) %>%
#   pivot_longer(cols = c(FWAR_FIP, FWAR_RA9)) %>%
#   rename(FWAR = value) %>% rename(FWAR_type = name) %>%
#   # filter(PIT_NAME == "Clayton Kershaw") %>%
#   filter(PIT_NAME %in% pitnames) %>%
#   ggplot(aes(x=FWAR)) +
#   facet_wrap(~PIT_NAME + FWAR_type, nrow=1) +
#   xlab("game FWAR") +
#   geom_histogram(fill="black")
# plot_Xpg_FWAR
# ggsave(paste0(output_folder,"plot_Xpg_FWAR.png"),plot_Xpg_GWAR,width=12,height=4)


fit_params.EB.mle_FWAR <- function(metric, epsilon=1e-5, df0=df_train_byGame, sig.sq=NULL) {
  ##### metric in {"FWAR_FIP", "FWAR_RA9"} #####
  df01 = df0 %>% mutate(X_p = .data[[metric]]) %>% distinct(PIT_NAME, YEAR, X_p, N_fg) 
  df = df01 %>% group_by(PIT_NAME) %>% summarise(N_fg = sum(N_fg), X_p = sum(X_p)) %>% mutate(X_over_N = X_p/N_fg)
  # browser()
  ### first, fit the MLE hyperparams mu.hat, tau.sq.hat
  ### initialize
  iter = 1
  params.df = df
  mu.hat = params.df %>% summarise(mu = mean(X_p))
  params.df$mu.hat = mu.hat$mu
  params.df$sig.sq.p.hat = sig.sq
  full.var = var(params.df$X_over_N)*100
  params.df$tau.sq.hat = full.var - mean(params.df$sig.sq.p.hat)
  train.df = params.df 
  train.df
  ### iteratively solve for MLE mu.hat, tau.sq.hat
  while (TRUE) {
    print(paste0("running iteration ", iter))
    ### save previous values
    train.df.prev = train.df
    ### solve for mu.hat
    mu.hat = train.df %>%
      mutate(
        t1 = sum(X_over_N / (sig.sq.p.hat + tau.sq.hat)),
        t2 = sum(1 / (sig.sq.p.hat + tau.sq.hat)),
      )
    mu.hat = mu.hat %>% summarise(mu.hat = sum(t1)/sum(t2))
    mu.hat = mu.hat$mu.hat
    mu.hat
    train.df$mu.hat = mu.hat
    ### solve for tau.sq.hat
    solve.for.tau.sq <- function(tau.sq) {
      temp.df = 
        train.df %>% 
        select(-tau.sq.hat) %>% 
        mutate(
          dd = (X_over_N - mu.hat)^2,
          t1 = 1 / (sig.sq.p.hat + tau.sq),
          t1.sq = t1^2,
          t2 = dd*t1.sq,
          diff = t1 - t2
        ) 
      temp.df
      (temp.df %>% summarise(ans = sum(diff)))$ans
    }
    tau.sq.hat = uniroot(solve.for.tau.sq, c(0,1000))
    tau.sq.hat = tau.sq.hat$root
    tau.sq.hat
    train.df$tau.sq.hat = tau.sq.hat
    ### check convergence
    mu.hat.conv = abs(unique(train.df$mu.hat) - unique(train.df.prev$mu.hat))
    tau.sq.hat.conv = abs(unique(train.df$tau.sq.hat) - unique(train.df.prev$tau.sq.hat))
    conv.num = max(c(mu.hat.conv, tau.sq.hat.conv))
    print(paste0("conv.num = ", conv.num))
    if (conv.num < epsilon) {
      break
    } 
    ### increment 
    iter = iter + 1
  }
  ### dataframe of fitted MLE hyperparams 
  # browser()
  df.hyperparams = train.df %>% 
    distinct(PIT_NAME, mu.hat, tau.sq.hat, sig.sq.p.hat, X_p, X_over_N, N_fg) %>%
    rename(mean_game_FWAR = X_over_N)
  df.hyperparams
  
  ### now, get the empirical Bayes pitcher quality estimates
  # browser()
  df.mu.hat.p = df.hyperparams %>%
    group_by(PIT_NAME) %>%
    summarise(
      tA = X_p/sig.sq.p.hat,
      tB = mu.hat/tau.sq.hat,
      tC = N_fg/sig.sq.p.hat,
      tD = 1/tau.sq.hat
    ) %>%
    mutate(
      mu.hat.p = (tA + tB)/(tC + tD)
    )
  df.mu.hat.p
  
  # df.mu.hat.p = train.df %>%
  #   group_by(PIT_NAME) %>%
  #   summarise(
  #     tA = sum(X_over_N)/sig.sq.p.hat,
  #     tB = mu.hat/tau.sq.hat,
  #     tC = N_fg/sig.sq.p.hat,
  #     tD = 1/tau.sq.hat
  #   ) %>%
  #   mutate(
  #     mu.hat.p = (tA + tB)/(tC + tD)
  #   )
  # df.mu.hat.p
  
  ### result dataframe
  df.result = df.mu.hat.p %>% left_join(df.hyperparams) %>% select(-c(tA,tB,tC,tD))
  df.result$metric = metric
  df.result %>% arrange(-mu.hat.p)
}

### check
fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=0.001)
fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=0.0013)
fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=0.0015)
fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=0.00163)
fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=0.00165)

### tune sig.sq
df0_tune = df_train_byGame %>% filter(YEAR %in% tuning_train_years)

df0_val = df_train_byGame %>% 
  filter(YEAR %in% tuning_val_years) %>%
  distinct(PIT_NAME, YEAR, FWAR_FIP, FWAR_RA9, N_fg) %>%
  group_by(PIT_NAME) %>%
  summarise(N_fg = sum(N_fg), FWAR_FIP = sum(FWAR_FIP), FWAR_RA9 = sum(FWAR_RA9)) %>%
  mutate(
    FWAR_FIP_over_N = FWAR_FIP/N_fg,
    FWAR_RA9_over_N = FWAR_RA9/N_fg
  ) 
df0_val 

### tune sig.sq.FIP
# sig.sq.vec = c(0.001, 0.01, 0.05, 0.075, 0.1)
sig.sq.vec = c(0.00075, 0.001, 0.0015, 0.005)
sig.sq.vec = seq(0.0001, 0.00165, length.out=15)
sig.sq.vec = seq(0.00165, 0.001662, length.out=15)

loss_FIP.vec = numeric(length(sig.sq.vec))
for (j in 1:length(sig.sq.vec)) {
  print(paste0("tuning sig.sq j=",j,"/",length(sig.sq.vec)))
  
  df.EB.FWAR_FIP_j = fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=sig.sq.vec[j], df0 = df0_tune)
  df.EB.FWAR_FIP_j
  val.df_FIP = df0_val %>% distinct(PIT_NAME, FWAR_FIP_over_N) %>% 
    left_join(df.EB.FWAR_FIP_j %>% select(PIT_NAME, mu.hat.p, metric)) %>%
    drop_na() %>%
    arrange(-mu.hat.p)
  val.df_FIP
  loss_FIP.vec[j] = rmse(val.df_FIP$FWAR_FIP_over_N, val.df_FIP$mu.hat.p)
}
loss_FIP.vec
plot(loss_FIP.vec)
sig.sq.FIP = sig.sq.vec[which(loss_FIP.vec == min(loss_FIP.vec))]
sig.sq.FIP

### check
fit_params.EB.mle_FWAR("FWAR_RA9", sig.sq=0.00165)
fit_params.EB.mle_FWAR("FWAR_RA9", sig.sq=0.0024)


### tune sig.sq.RA9
# sig.sq.vec = c(0.001, 0.01, 0.05, 0.075, 0.1)
# sig.sq.vec = c(0.001, 0.002)
sig.sq.vec = seq(0.0021, 0.00233, length.out=15)
loss_RA9.vec = numeric(length(sig.sq.vec))
for (j in 1:length(sig.sq.vec)) {
  print(paste0("tuning sig.sq j=",j,"/",length(sig.sq.vec)))
  
  df.EB.FWAR_RA9_j = fit_params.EB.mle_FWAR("FWAR_RA9", sig.sq=sig.sq.vec[j], df0 = df0_tune)
  df.EB.FWAR_RA9_j
  val.df_RA9 = df0_val %>% distinct(PIT_NAME, FWAR_RA9_over_N) %>% 
    left_join(df.EB.FWAR_RA9_j %>% select(PIT_NAME, mu.hat.p, metric)) %>%
    drop_na() %>%
    arrange(-mu.hat.p)
  val.df_RA9
  loss_RA9.vec[j] = rmse(val.df_RA9$FWAR_RA9_over_N, val.df_RA9$mu.hat.p)
}
loss_RA9.vec
plot(loss_RA9.vec)
sig.sq.RA9 = sig.sq.vec[which(loss_RA9.vec == min(loss_RA9.vec))]
sig.sq.RA9


### get FWAR pitcher quality predictions
df.EB.FWAR_FIP = fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=sig.sq.FIP, df0 = df0_tune)
# df.EB.FWAR_FIP = fit_params.EB.mle_FWAR("FWAR_FIP", sig.sq=0.00165, df0 = df0_tune)
df.EB.FWAR_FIP
plot_FWAR_FIP = df.EB.FWAR_FIP %>%
  rename(N = N_fg) %>%
  ggplot(aes(x=mean_game_FWAR, y=mu.hat.p, color=N, size=N)) +
  ylab("mean game FWAR (FIP)") +
  xlab(TeX("$\\hat{\\mu}_p$")) +
  geom_abline(intercept=0, slope=1, linewidth=1, linetype="dashed", color="gray60") +
  geom_point(alpha=0.6)
plot_FWAR_FIP
ggsave(paste0(output_folder,"plot_EB_FWAR_FIP.png"), plot_FWAR_FIP, width=7, height=5)


### get FWAR pitcher quality predictions
df.EB.FWAR_RA9 = fit_params.EB.mle_FWAR("FWAR_RA9", sig.sq=sig.sq.RA9, df0 = df0_tune)
# df.EB.FWAR_RA9 = fit_params.EB.mle_FWAR("FWAR_RA9", sig.sq=0.00165, df0 = df0_tune)
df.EB.FWAR_RA9
plot_FWAR_RA9 = df.EB.FWAR_RA9 %>%
  rename(N = N_fg) %>%
  ggplot(aes(x=mean_game_FWAR, y=mu.hat.p, color=N, size=N)) +
  ylab("mean game FWAR (RA9)") +
  xlab(TeX("$\\hat{\\mu}_p$")) +
  geom_abline(intercept=0, slope=1, linewidth=1, linetype="dashed", color="gray60") +
  geom_point(alpha=0.6)
plot_FWAR_RA9
ggsave(paste0(output_folder,"plot_EB_FWAR_RA9.png"), plot_FWAR_RA9, width=7, height=5)

########################### ########################### 

df.EB.GWAR.1
df.EB.FWAR_FIP
df.EB.FWAR_RA9

df.EB = left_join(
    df.EB.GWAR.1 %>% select(PIT_NAME, mu.hat.p) %>% rename(mu.hat.p.GWAR = mu.hat.p),
    df.EB.FWAR_FIP %>% select(PIT_NAME, mu.hat.p) %>% rename(mu.hat.p.FWAR_FIP = mu.hat.p),
  ) %>% left_join(
    df.EB.FWAR_RA9 %>% select(PIT_NAME, mu.hat.p) %>% rename(mu.hat.p.FWAR_RA9 = mu.hat.p),
  ) %>% 
  drop_na() %>%
  mutate(
    # pred_rank.GWAR = rank(-mu.hat.p.GWAR),
    # pred_rank.FWAR_FIP = rank(-mu.hat.p.FWAR_FIP),
    # pred_rank.FWAR_RA9 = rank(-mu.hat.p.FWAR_RA9),
  )
df.EB

df_test = df_war_pitSzn %>% 
  filter(YEAR > final_train_year) %>%
  # select(PIT_NAME, YEAR, GWAR) %>%
  group_by(PIT_NAME) %>%
  summarise(GWAR = sum(GWAR)) %>%
  left_join(df.EB) %>%
  drop_na() %>%
  mutate(
    rank.GWAR_obs = rank(-GWAR),
    rank.GWAR_pred = rank(-mu.hat.p.GWAR),
    rank.FWAR_FIP_pred = rank(-mu.hat.p.FWAR_FIP),
    rank.FWAR_RA9_pred = rank(-mu.hat.p.FWAR_RA9),
  )
df_test

df_test_A = df_test %>% select(c("PIT_NAME",starts_with("rank")))
df_test_A
nrow(df_test_A) # N

### compute RMSE
df_test_rmses = df_test_A %>%
  summarise(
    rmse_GWAR_pred = rmse(rank.GWAR_pred, rank.GWAR_obs),
    rmse_FWAR_FIP_pred = rmse(rank.FWAR_FIP_pred, rank.GWAR_obs),
    rmse_FWAR_RA9_pred = rmse(rank.FWAR_RA9_pred, rank.GWAR_obs),
  ) %>% 
  pivot_longer(everything()) %>%
  arrange(value)
df_test_rmses
gt::gtsave(gt::gt(df_test_rmses), paste0(output_folder,"plot_test_EB_rmse.png"))




# ### plot
# df_test %>%
#   mutate(PIT_NAME = fct_reorder(PIT_NAME, rank.GWAR_obs)) %>%
#   select(c("PIT_NAME",starts_with("rank"))) %>%
#   pivot_longer(-PIT_NAME) %>%
#   rename(rank = value) %>%
#   ggplot(aes(y = PIT_NAME)) +
#   geom_point(aes(x=rank, color=name))

### plot pitcher rankings
cbp1 <- c("#E69F00", "#56B4E9", "#CC79A7",  "#999999", "#009E73",
          "#0072B2", "#D55E00",  "#F0E442")

plot_EB_pitRankingsMu = df_test %>%
  mutate(PIT_NAME = fct_reorder(PIT_NAME, mu.hat.p.GWAR)) %>%
  select(c("PIT_NAME",starts_with("mu"))) %>%
  pivot_longer(-PIT_NAME, values_to = "mu.hat.p", names_to="metric") %>%
  mutate(metric = str_remove(metric, "mu.hat.p.")) %>%
  ggplot(aes(y = PIT_NAME)) +
  geom_point(aes(x=mu.hat.p, color=metric, shape=metric), size=5) +
  scale_colour_manual(values=cbp1) +
  xlab(TeX("$\\hat{\\mu}_p$")) + 
  ylab("pitcher") 
plot_EB_pitRankingsMu
ggsave(paste0(output_folder,"plot_EB_pitRankingsMu.png"), 
       plot_EB_pitRankingsMu, width=11, height=10)

plot_EB_pitRankingsRank = df_test %>%
  select(-rank.GWAR_obs) %>%
  mutate(PIT_NAME = fct_reorder(PIT_NAME, -rank.GWAR_pred)) %>%
  select(c("PIT_NAME",starts_with("rank"))) %>%
  pivot_longer(-PIT_NAME, values_to = "rank", names_to="metric") %>%
  mutate(metric = str_remove(metric, "rank.")) %>%
  mutate(metric = str_remove(metric, "_pred")) %>%
  ggplot(aes(y = PIT_NAME)) +
  geom_point(aes(x=rank, color=metric, shape=metric), size=5) +
  # xlab(TeX("$\\hat{\\mu}_p$")) + 
  scale_colour_manual(values=cbp1) +
  ylab("pitcher") 
plot_EB_pitRankingsRank
ggsave(paste0(output_folder,"plot_EB_pitRankingsRank.png"), 
       plot_EB_pitRankingsRank, width=11, height=10)

plot_EB_pitRankingsRank1 = df_test %>%
  mutate(PIT_NAME = fct_reorder(PIT_NAME, -rank.GWAR_obs)) %>%
  select(c("PIT_NAME",starts_with("rank"))) %>%
  pivot_longer(-PIT_NAME, values_to = "rank", names_to="metric") %>%
  mutate(metric = str_remove(metric, "rank.")) %>%
  mutate(metric = str_remove(metric, "_pred")) %>%
  ggplot(aes(y = PIT_NAME)) +
  geom_point(aes(x=rank, color=metric, shape=metric), size=5) +
  # xlab(TeX("$\\hat{\\mu}_p$")) + 
  scale_colour_manual(values=cbp1) +
  ylab("pitcher") 
plot_EB_pitRankingsRank1
ggsave(paste0(output_folder,"plot_EB_pitRankingsRank1.png"), 
       plot_EB_pitRankingsRank1, width=11, height=10)

####################################################
### dist of game-GWAR conditional on true talent ###
####################################################

# hist(df.EB$mu.hat.p.GWAR)

df_games_mu = df_train_byGame %>% left_join(df.EB)
df_games_mu

plot_gwar_talent_dist = df.EB %>%
  ggplot() +
  # geom_histogram(aes(x = mu.hat.p.GWAR ), fill="black", bins=40) +
  geom_density(aes(x = mu.hat.p.GWAR ), fill="black") +
  xlab(TeX("$\\hat{\\mu}_p^{GWAR}$")) +
  geom_vline(aes(xintercept = mean(mu.hat.p.GWAR)), linewidth=2, color="dodgerblue2")
plot_gwar_talent_dist
ggsave(paste0(output_folder,"plot_EB_gwarTalentDist.png"),
       plot_gwar_talent_dist, width=6, height=5)

appender <- function(x) TeX(paste("$\\hat{\\mu}_p^{GWAR} \\in $", unique(x))) 

plot_gameGwarDist_givenTalent = df_games_mu %>%
  drop_na(mu.hat.p.GWAR) %>%
  mutate(muhat_bin = cut(mu.hat.p.GWAR, 3)) %>%
  ggplot() + 
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank() 
  ) +
  xlab("game GWAR") +
  guides(fill=guide_legend(title=TeX(paste("$\\hat{\\mu}_p^{GWAR}$")) )) +
  geom_density(aes(x = GWAR, fill=muhat_bin, #after_stat(density)
  ),  alpha=0.5, #fill="black",
  )
plot_gameGwarDist_givenTalent
ggsave(paste0(output_folder,"plot_EB_gameGwarDist_givenTalent.png"), 
       plot_gameGwarDist_givenTalent, width=7, height=5)

# plot_gameGwarDist_givenTalent = df_games_mu %>%
#   drop_na(mu.hat.p.GWAR) %>%
#   mutate(muhat_bin = cut(mu.hat.p.GWAR, 3)) %>%
#   ggplot() + 
#   # facet_wrap(~muhat_bin) +
#   facet_wrap(~muhat_bin, nrow=1,
#              labeller = as_labeller(appender, default = label_parsed)) +
#   theme(
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank() 
#   ) +
#   xlab("game GWAR") +
#   geom_histogram(aes(x = GWAR, 
#                      after_stat(density)
#                  ), fill="black")
# plot_gameGwarDist_givenTalent
# ggsave(paste0(output_folder,"plot_EB_gameGwarDist_givenTalent.png"), 
#        plot_gameGwarDist_givenTalent, width=10, height=3)




plot_gwar_game_dist = df_games_mu %>%
  ggplot() +
  # geom_histogram(aes(x = GWAR ), fill="black", bins=40) +
  geom_density(aes(x = GWAR ), fill="black") +
  xlab("game GWAR") +
  geom_vline(aes(xintercept = mean(GWAR)), linewidth=2, color="dodgerblue2") 
plot_gwar_game_dist
ggsave(paste0(output_folder,"plot_EB_gwarGameDist.png"), 
       plot_gwar_game_dist, width=6, height=5)

appender1 <- function(x) TeX(paste0("game GWAR $\\in$ ", unique(x)))

plot_gwarTalentDist_givenGameGwar = df_games_mu %>%
  drop_na(mu.hat.p.GWAR) %>%
  mutate(gwar_game_bin = cut(GWAR, 3)) %>%
  ggplot() + 
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) +
  guides(fill=guide_legend(title=TeX(paste("game GWAR")) )) +
  geom_density(aes(x = mu.hat.p.GWAR, fill = gwar_game_bin), alpha=0.5) +
  # geom_vline(aes(xintercept = mean(mu.hat.p.GWAR)), linewidth=1, color="black") +
  xlab(TeX("$\\hat{\\mu}_p^{GWAR}$")) 
plot_gwarTalentDist_givenGameGwar
ggsave(paste0(output_folder,"plot_EB_plot_gwarTalentDist_givenGameGwar.png"), 
       plot_gwarTalentDist_givenGameGwar, width=7, height=5)

# plot_gwarTalentDist_givenGameGwar = df_games_mu %>%
#   drop_na(mu.hat.p.GWAR) %>%
#   mutate(gwar_game_bin = cut(GWAR, 3)) %>%
#   ggplot() + 
#   # facet_wrap(~muhat_bin) +
#   facet_wrap(~gwar_game_bin, nrow=1,
#              labeller = as_labeller(appender1, default = label_parsed)) +
#   theme(
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) +
#   xlab(TeX("$\\hat{\\mu}_p^{GWAR}$")) +
#   geom_histogram(aes(x = mu.hat.p.GWAR, 
#                      after_stat(density)
#   ),bins=30, fill="black") +
#   geom_vline(aes(xintercept = mean(mu.hat.p.GWAR)), linewidth=1, color="dodgerblue2")
# plot_gwarTalentDist_givenGameGwar
# ggsave(paste0(output_folder,"plot_EB_plot_gwarTalentDist_givenGameGwar.png"), 
#        plot_gwarTalentDist_givenGameGwar, width=10, height=3)

################################
### Look at specific people  ###
################################

# undervalued_pitchers = read_csv("df_undervalued_pitchers.csv") %>% select(PIT_NAME) %>% mutate(uov = "undervalued")
# overvalued_pitchers = read_csv("df_overvalued_pitchers.csv")  %>% select(PIT_NAME) %>% mutate(uov = "overvalued")
# df_test_1 = df_test %>% left_join(undervalued_pitchers) %>% left_join(overvalued_pitchers)
df_test_1 = df_test_A %>%
  mutate(
    GW_pred_minus_FW_RA9_pred = rank.GWAR_pred - rank.FWAR_RA9_pred,
    GW_pred_minus_FW_FIP_pred = rank.GWAR_pred - rank.FWAR_FIP_pred,
  ) 
df_test_1

### 
df_test_uv_RA9 = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  head(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_uvRA9 = rmse(rank.GWAR_obs, rank.GWAR_pred),
    rmse_GWAR.hat_FWAR_RA9_uvRA9 = rmse(rank.GWAR_obs, rank.FWAR_RA9_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_uv_RA9

###
plot_uv5_ra9 = 
  df_test_1 %>%
  mutate(PIT_NAME = str_replace_all(PIT_NAME, " ", "\n")) %>%
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank.GWAR_obs)) %>%
  # mutate(PIT_NAME = fct_reorder(PIT_NAME,GW_pred_minus_FW_RA9_pred)) %>%
  head(n=5) %>%
  select(PIT_NAME, rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_RA9_pred) %>%
  pivot_longer(cols=c(rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_RA9_pred)) %>%
  mutate(
    size_ = name == "rank.GWAR_obs",
    name0 = name,
    name = case_when(
      name == "rank.GWAR_obs" ~ "observed\n2019\nGWAR\nrank\n",
      name == "rank.GWAR_pred" ~ "predicted\n2019\nrank\nfrom\nGWAR\n",
      name == "rank.FWAR_RA9_pred" ~ "predicted\n2019\nrank\nfrom\nFWAR (RA/9)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, shape=metric), size=6, alpha=0.8) +
  guides(size = "none") +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  scale_shape_manual(name="", values=c(15,16,17)) +
  labs(title = "5 most undervalued starting pitchers\naccording to GWAR relative to FWAR (RA/9)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
plot_uv5_ra9
ggsave("plots/plot_test_EB_comp_uv5_ra9.png", plot_uv5_ra9, width=9, height=6)

### 
df_test_ov_RA9 = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_RA9_pred) %>% 
  tail(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_ovRA9 = rmse(rank.GWAR_obs, rank.GWAR_pred),
    rmse_GWAR.hat_FWAR_RA9_ovRA9 = rmse(rank.GWAR_obs, rank.FWAR_RA9_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_ov_RA9

###
plot_ov5_ra9 = 
  df_test_1 %>%
  mutate(PIT_NAME = str_replace_all(PIT_NAME, " ", "\n")) %>%
  arrange(GW_pred_minus_FW_RA9_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank.GWAR_obs)) %>%
  # mutate(PIT_NAME = fct_reorder(PIT_NAME,GW_pred_minus_FW_RA9_pred)) %>%
  head(n=5) %>%
  select(PIT_NAME, rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_RA9_pred) %>%
  pivot_longer(cols=c(rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_RA9_pred)) %>%
  mutate(
    size_ = name == "rank.GWAR_obs",
    name0 = name,
    name = case_when(
      name == "rank.GWAR_obs" ~ "observed\n2019\nGWAR\nrank\n",
      name == "rank.GWAR_pred" ~ "predicted\n2019\nrank\nfrom\nGWAR\n",
      name == "rank.FWAR_RA9_pred" ~ "predicted\n2019\nrank\nfrom\nFWAR (RA/9)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, shape=metric), size=6, alpha=0.8) +
  guides(size = "none") +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  scale_shape_manual(name="", values=c(15,16,17)) +
  labs(title = "5 most overvalued starting pitchers\naccording to GWAR relative to FWAR (RA/9)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
plot_ov5_ra9
ggsave("plots/plot_test_EB_comp_ov5_ra9.png", plot_ov5_ra9, width=9, height=6)

### 
df_test_uv_FIP = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_FIP_pred) %>% 
  head(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_uvFIP = rmse(rank.GWAR_obs, rank.GWAR_pred),
    rmse_GWAR.hat_FWAR_FIP_uvFIP = rmse(rank.GWAR_obs, rank.FWAR_FIP_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_uv_FIP

###
plot_uv5_fip = 
  df_test_1 %>%
  mutate(PIT_NAME = str_replace_all(PIT_NAME, " ", "\n")) %>%
  arrange(-GW_pred_minus_FW_FIP_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank.GWAR_obs)) %>%
  # mutate(PIT_NAME = fct_reorder(PIT_NAME,GW_pred_minus_FW_FIP_pred)) %>%
  head(n=5) %>%
  select(PIT_NAME, rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_FIP_pred) %>%
  pivot_longer(cols=c(rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_FIP_pred)) %>%
  mutate(
    size_ = name == "rank.GWAR_obs",
    name0 = name,
    name = case_when(
      name == "rank.GWAR_obs" ~ "observed\n2019\nGWAR\nrank\n",
      name == "rank.GWAR_pred" ~ "predicted\n2019\nrank\nfrom\nGWAR\n",
      name == "rank.FWAR_FIP_pred" ~ "predicted\n2019\nrank\nfrom\nFWAR (FIP)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, shape=metric), size=6, alpha=0.8) +
  guides(size = "none") +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  scale_shape_manual(name="", values=c(15,16,17)) +
  labs(title = "5 most undervalued starting pitchers\naccording to GWAR relative to FWAR (FIP)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
plot_uv5_fip
ggsave("plots/plot_test_EB_comp_uv5_fip.png", plot_uv5_fip, width=9, height=6)

### 
df_test_ov_FIP = 
  df_test_1 %>% 
  arrange(-GW_pred_minus_FW_FIP_pred) %>% 
  tail(n=5) %>%
  summarise(
    rmse_GWAR.hat_GWAR_ovFIP = rmse(rank.GWAR_obs, rank.GWAR_pred),
    rmse_GWAR.hat_FWAR_FIP_ovFIP = rmse(rank.GWAR_obs, rank.FWAR_FIP_pred),
  ) %>%
  pivot_longer(everything()) %>%
  arrange(value)
df_test_ov_FIP

###
plot_ov5_fip = 
  df_test_1 %>%
  mutate(PIT_NAME = str_replace_all(PIT_NAME, " ", "\n")) %>%
  arrange(GW_pred_minus_FW_FIP_pred) %>% 
  mutate(PIT_NAME = fct_reorder(PIT_NAME,-rank.GWAR_obs)) %>%
  # mutate(PIT_NAME = fct_reorder(PIT_NAME,GW_pred_minus_FW_FIP_pred)) %>%
  head(n=5) %>%
  select(PIT_NAME, rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_FIP_pred) %>%
  pivot_longer(cols=c(rank.GWAR_obs, rank.GWAR_pred, rank.FWAR_FIP_pred)) %>%
  mutate(
    size_ = name == "rank.GWAR_obs",
    name0 = name,
    name = case_when(
      name == "rank.GWAR_obs" ~ "observed\n2019\nGWAR\nrank\n",
      name == "rank.GWAR_pred" ~ "predicted\n2019\nrank\nfrom\nGWAR\n",
      name == "rank.FWAR_FIP_pred" ~ "predicted\n2019\nrank\nfrom\nFWAR (FIP)\n",
    ),
  ) %>%
  rename(metric="name") %>%
  rename(rank=value) %>%
  ggplot() + 
  geom_point(aes(y=PIT_NAME, x=rank, color = metric, shape=metric), size=6, alpha=0.8) +
  guides(size = "none") +
  scale_color_manual(name="", values=c("black", "firebrick", "dodgerblue2")) +
  scale_shape_manual(name="", values=c(15,16,17)) +
  labs(title = "5 most overvalued starting pitchers\naccording to GWAR relative to FWAR (FIP)") + 
  xlab("starting pitcher rank") +
  ylab("starting pitcher")
plot_ov5_fip
ggsave("plots/plot_test_EB_comp_ov5_fip.png", plot_ov5_fip, width=9, height=6)

###
df_test_rmses_uvov = bind_rows(df_test_uv_RA9,df_test_ov_RA9,df_test_uv_FIP,df_test_ov_FIP)
df_test_rmses_uvov
gt::gtsave(gt::gt(df_test_rmses_uvov), paste0(output_folder,"plot_test_EB_rmse_uvov.png"))

###
library(cowplot)
plot_ovuv5_fip = plot_grid(plot_uv5_fip + guides(color="none"), 
                           # get_legend(plot_ov5_fip),
                           plot_ov5_fip + guides(color="none"),
                           get_legend(plot_ov5_fip),
                           nrow=1)

plot_ovuv5_fip = plot_grid(plot_uv5_fip, plot_ov5_fip)
save_plot("plots/plot_test_EB_comp_uvov5_fip.png", plot_ovuv5_fip, base_width=18, base_height=6)

# plot_ovuv5_fip = plot_grid(
#   plot_uv5_fip + guides(color="none"), 
#   plot_ov5_fip + guides(color="none")
#   # plot_grid(get_legend(plot_ov5_fip)), nrow=1
# )
# plot_ovuv5_fip = plot_grid(
#   plot_ovuv5_fip,
#   plot_grid(get_legend(plot_ov5_fip))
# )
# plot_ovuv5_fip
# save_plot("plots/plot_test_EB_comp_uvov5_fip.png", plot_ovuv5_fip, base_width=18, base_height=6)
# save_plot("plots/plot_test_EB_comp_uvov5_fip_legend.png", plot_grid(get_legend(plot_ov5_fip)), base_width=6, base_height=6)


