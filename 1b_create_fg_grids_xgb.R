
source("0_load_stuff.R")
war2 = read_csv("war2.csv")

################################
#### data for f grid f(I,R) ####
################################

# every last play of inning
last_play_every_inning <- war2 %>% 
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  filter(row_number() == n()) %>%
  ungroup()

# Check
# View(last_play_every_inning %>% arrange(BAT_HOME_IND) %>% filter(GAME_ID == "ANA201804020") %>%
#        select(GAME_ID, BAT_HOME_IND, INNING,
#               EVENT_TX, EVENT_RUNS, CUM_RUNS))

df_f_grid <- last_play_every_inning %>% 
  mutate(HOME_LEAGUE = ifelse(BAT_HOME_IND, AWAY_LEAGUE, HOME_LEAGUE)) %>%
  select(GAME_ID, BAT_HOME_IND, HOME_LEAGUE, YEAR, INNING, CUM_RUNS, PIT_WINS) %>%
# in the 9th inning only use away batters, to avoid the bias that home batters in the 9th usually lose
  filter(!(INNING == 9 & BAT_HOME_IND == 1)) %>%
  filter(INNING <= 9) %>%
  filter(CUM_RUNS <= 10)
df_f_grid

### logistic regression with fixed effects
train_f_lrfe = function(df) {
  glm(PIT_WINS ~ factor(INNING) + factor(CUM_RUNS),
      data=df, family="binomial"(link="logit"))
}

### train empirical grid
train_empirical_f_grid = function(df) {
  f_grid_empirical = df %>%
    group_by(INNING,CUM_RUNS) %>%
    summarise(y_hat = mean(PIT_WINS)) %>%
    ungroup() 
  f_grid_empirical = reshape2::acast(f_grid_empirical, INNING~CUM_RUNS, value.var="y_hat")
  f_grid_empirical = ifelse(is.na(f_grid_empirical), 0, f_grid_empirical)
  f_grid_empirical
}

predict_f_grid_empirical = function(f_grid_empirical, INNING, CUM_RUNS) {
  f_grid_empirical[cbind(INNING, CUM_RUNS+1)]
}
# predict_f_grid_empirical(train_empirical_f_grid(df_f_grid), 1:3, 1:3)

### train xgboost model


### xgboost model
params = if (file.exists("xgb_f_grid_params.yaml")) list.load("xgb_f_grid_params.yaml") else NULL
fit_xgb_f_grid <- function(params, data_train, data_test=NULL, w=FALSE, param_tuning=FALSE) {
  
  go_xgb_trainMat = xgb.DMatrix(
    model.matrix(~ . + 0, data = data_train %>% select(all_of(go_xgb_features))), 
    label = data_train$PIT_WINS,
    weight = if (w) data_train$w else rep(1, nrow(data_train))
  )
  
  if (!is.null(data_test)) {
    go_xgb_testMat = xgb.DMatrix(
      model.matrix(~ . + 0, data = data_test %>% select(all_of(go_xgb_features))), 
      label = data_test$PIT_WINS
    )
    watchlist = list(train=go_xgb_trainMat, test=go_xgb_testMat)
  } else  {
    watchlist = list(train=go_xgb_trainMat)
  }
  
  nrounds = params$nrounds
  params = within(params, rm(nrounds))
  
  if (!param_tuning) {
    xgb_go_model <- xgb.train( 
      data = go_xgb_trainMat, 
      watchlist = watchlist,
      params = params,
      nrounds = nrounds,
      print_every_n = 50
    )
  } else {
    xgb_go_model <- xgb.train( 
      data = go_xgb_trainMat, 
      watchlist = watchlist,
      params = params,
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = 50
    )
  }
  
  return(xgb_go_model)
}
### other stuff `go_xgb_features` and `pred_xgb_f_grid` are in `0_load_stuff.R`

go_xgb_features = c("INNING", "CUM_RUNS")
pred_xgb_f_grid <- function(df, xgbm) {
  go_xgb_testMat = xgb.DMatrix(
    model.matrix(~ . + 0, data = df %>% select(all_of(go_xgb_features)))
  )
  predict(xgbm, go_xgb_testMat)
}

####################################
#### model selection for f(I,R) ####
####################################

set.seed(22) # Kershaw!
train_idxs = sort(sample(1:nrow(df_f_grid), size=round(nrow(df_f_grid)*1/2)))
test_idxs = setdiff(1:nrow(df_f_grid), train_idxs)
df_f_train = df_f_grid[train_idxs,]
df_f_test = df_f_grid[test_idxs,]

### logistic regression with fixed effects
model_f_lrfe = train_f_lrfe(df_f_train) 
  
### empirical grid
f_grid_empirical = train_empirical_f_grid(df_f_train) 

### XGBoost with monotonic constraints
go_xgb_trainMat = xgb.DMatrix(
  model.matrix(~ . + 0, data = df_f_train %>% select(all_of(go_xgb_features))), 
  label = df_f_train$PIT_WINS
)
go_xgb_testMat = xgb.DMatrix(
  model.matrix(~ . + 0, data = df_f_test %>% select(all_of(go_xgb_features))), 
  label = df_f_test$PIT_WINS
)

### Ben Baldwin's param tuning from https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
{
  ###############################################################
  library(dials)
  set.seed(30) ### Todd
  grid_size = 40
  go_xgb_param_grid = grid_latin_hypercube(
    dials::loss_reduction(),
    #################
    dials::min_n(),
    dials::finalize(dials::mtry(), df_f_train), # this finalize thing is because mtry depends on # of columns in data
    dials::tree_depth(),
    dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
    sample_size = dials::sample_prop(),
    #################
    # dials::min_n(range=c(20,30)),
    # dials::mtry(range = c(round(length(df_f_train) * 0.8), length(df_f_train))),
    # dials::tree_depth(range=c(3,4)),
    # dials::learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
    # sample_size = dials::sample_prop(range = c(0.8, 1)),
    #################
    size = grid_size
  ) %>% mutate(
    mtry = mtry / length(df_f_train),
    monotone_constraints = "(1, -1)"
  ) %>% rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )
  go_xgb_param_grid
  # function to perform xgb.cv for a given row in a hyperparameter grid
  get_row <- function(row) {
    params <-
      list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = c("logloss"),
        eta = row$eta,
        gamma = row$gamma,
        subsample = row$subsample,
        colsample_bytree = row$colsample_bytree,
        max_depth = row$max_depth,
        min_child_weight = row$min_child_weight,
        monotone_constraints = row$monotone_constraints
      )

    fold1 = sort(sample(1:nrow(df_f_train), replace=FALSE, size=0.5*nrow(df_f_train)))
    fold2 = setdiff(1:nrow(df_f_train), fold1)
    folds = list(Fold1 = fold1, Fold2 = fold2)

    # do the cross validation
    wp_cv_model <- xgboost::xgb.cv(
      data = go_xgb_trainMat,
      params = params,
      folds = folds,
      metrics = list("logloss"),
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = 50
    )

    # bundle up the results together for returning
    output <- params
    output$iter <- wp_cv_model$best_iteration
    output$logloss <- wp_cv_model$evaluation_log[output$iter]$test_logloss_mean

    row_result <- bind_rows(output)

    return(row_result)
  }
  # get results
  results = map_df(1:nrow(go_xgb_param_grid), function(x) {
    print(paste0("row ", x)); return(get_row(go_xgb_param_grid %>% dplyr::slice(x)))
  })
  # visualize param tuning
  results %>%
    dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
    tidyr::pivot_longer(
      eta:min_child_weight,
      values_to = "value",
      names_to = "parameter"
    ) %>%
    ggplot(aes(value, logloss, color = parameter)) +
    geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "logloss") +
    theme_minimal()
  # re-tune, with better param range based on these plots...
  # Collect best parameters
  results %>% arrange(logloss) %>% select(eta, subsample, colsample_bytree, max_depth, logloss, min_child_weight, iter)
  best_model <- results %>% arrange(logloss) %>% slice_head()
  best_model
  params <- list(
    booster = "gbtree",
    objective = "reg:logistic",
    # objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints,
    nrounds = best_model$iter
  )
  params
  list.save(params, "xgb_f_grid_params.yaml")
  ###############################################################
}

### fit xgboost model
params = list.load("xgb_f_grid_params.yaml")
xgb_f_grid = fit_xgb_f_grid(params, df_f_train, data_test=df_f_test)

### out-of-sample loss
loss_df = data.frame(
  loss_lrfe = logloss(df_f_test$PIT_WINS, predict(model_f_lrfe, df_f_test, type="response")),
  loss_empirical_f_grid = logloss(df_f_test$PIT_WINS, predict_f_grid_empirical(f_grid_empirical,df_f_test$INNING,df_f_test$CUM_RUNS)),
  loss_xgb = last(xgb_f_grid$evaluation_log[["test_logloss"]])
) 
loss_df
gt::gtsave(gt::gt(loss_df), "plots/plot_model_f_loss.png")

### XGBoost is the best!

#####################
#### PLOT f(I,R) ####
#####################

plot_WP_matrixIR <- function(WP) {
  ### WP is a matrix with 9 rows (innings) and Rmax+1 columns (runs allowed)
  
  WPi = as_tibble(t(WP))
  colnames(WPi) = paste0("inn",1:9)
  WPii = stack(WPi) 
  WPii$runs = rep(0:(nrow(WPi)-1), 9)
  
  pWPiis = WPii %>% filter(runs <= 13) %>%
    mutate(inning=str_sub(ind,start=4)) %>%
    ggplot(aes(x=runs,y=values,color=inning)) + 
    geom_point() + 
    geom_line(linewidth=1) +
    labs(
      # title=TeX("smoothed $f(I,R)$ as a function of $R$, for each $I$"),
      y="context-neutral win probability",
      x="runs allowed through the end of the given inning") +
    scale_x_continuous(breaks=seq(0,30,by=2)) +
    scale_y_continuous(breaks=seq(0,1,by=0.1))
  pWPiis
}

# ### train full models
# f_model_lrfe = train_f_lrfe(df_f_grid)
# f_grid_empirical = train_empirical_f_grid(df_f_grid)
# params = list.load("xgb_f_grid_params.yaml")
# f_model_xgb = fit_xgb_f_grid(params, df_f_grid)
# 
# max_r = 10#max(D$CUM_RUNS)
# WP_lrfe = matrix(nrow = 9, ncol = max_r+1) ### WP matrix for model_f_lrfe
# WP_xgb = matrix(nrow = 9, ncol = max_r+1) ### WP matrix for XGBoost
# for (inn in 1:9) {
#   test_df_inn = tibble(
#     INNING = inn, CUM_RUNS = 0:(max_r), BAT_HOME_IND = 1, HOME_LEAGUE = "AL", YEAR = 2019
#   )
#   WP_lrfe[inn,] = predict(f_model_lrfe, test_df_inn, type="response")
#   WP_xgb[inn,] = pred_xgb_f_grid(test_df_inn, f_model_xgb)
# }
# 
# plot_WP_lrfe = plot_WP_matrixIR(WP_lrfe)
# plot_WP_f_grid_empirical = plot_WP_matrixIR(f_grid_empirical)
# plot_WP_xgb = plot_WP_matrixIR(WP_xgb)
# 
# plot_WP_lrfe
# plot_WP_f_grid_empirical
# plot_WP_xgb
# 
# ggsave(paste0(output_folder,"plot_fIR_R_gridEmpirical.png"), plot_WP_f_grid_empirical, width=7, height=5)
# ggsave(paste0(output_folder,"plot_fIR_R_xgb.png"), plot_WP_xgb, width=7, height=5)

#######################################################################
### fit the best f(I,R) model, separately for each year and league ####
#######################################################################

xgb_to_grid <- function(f_xgb,yr,lg) {
  max_r = 10
  f_xgb_grid = matrix(nrow = 9, ncol = max_r+1) ### WP matrix for XGBoost
  for (inn in 1:9) {
    test_df_inn = tibble(INNING = inn, CUM_RUNS = 0:(max_r), HOME_LEAGUE = lg, YEAR = yr)
    f_xgb_grid[inn,] = pred_xgb_f_grid(test_df_inn, f_xgb)
  }
  rownames(f_xgb_grid) = paste0("INNING_",1:9)
  colnames(f_xgb_grid) = paste0("CUM_RUNS_",(0:(ncol(f_xgb_grid)-1)))
  return(f_xgb_grid)
}

params = list.load("xgb_f_grid_params.yaml")

### convert xgb models to grids
WP_xgb_grids = list()
for (yr in sort(unique(df_f_grid$YEAR))) {
  for (lg in sort(unique(df_f_grid$HOME_LEAGUE))) {
    # df_f_yl = df_f_grid %>% filter(YEAR == yr & HOME_LEAGUE == lg)
    alpha = 0.6 ### exponential decay weight
    df_f_yl = df_f_grid %>% 
      filter(YEAR <= yr & HOME_LEAGUE == lg) %>%
      mutate(w = alpha^(yr-YEAR) )
    table(df_f_yl$w)
    f_xgb_yl = fit_xgb_f_grid(params, df_f_yl, w=TRUE)
    # f_xgb_yl = fit_xgb_f_grid(params, df_f_yl)
    f_grid_yl = xgb_to_grid(f_xgb_yl,yr,lg)
    WP_xgb_grids[[paste0("f_grid_",yr,"_",lg)]] = f_grid_yl
    
    ### plot xgb grid
    WP_xgb_plot = plot_WP_matrixIR(f_grid_yl)
    ggsave(paste0(output_folder,"plot_fIR_R_xgb_",paste0(yr,"_",lg),".png"), WP_xgb_plot, width=7, height=5)
  }
}

### save xgb grids
saveRDS(WP_xgb_grids, "model_f.rds")

### plot empirical grid
yr = 2015; lg = "NL";
emp_f_grid = train_empirical_f_grid(df_f_grid %>% filter(YEAR == yr & HOME_LEAGUE == lg))
plot_WP_f_grid_empirical = plot_WP_matrixIR(emp_f_grid)
ggsave(paste0(output_folder,"plot_fIR_R_emp_grid_",paste0(yr,"_",lg),".png"), 
       plot_WP_f_grid_empirical, width=7, height=5)

# #################################################################
# #### save the grid output of the best f(I,R) model (XGBoost) ####
# #################################################################
# 
# f_grid_best = WP_xgb
# rownames(f_grid_best) = paste0("INNING_",1:9)
# colnames(f_grid_best) = paste0("CUM_RUNS_",(0:(ncol(f_grid_best)-1)))
# f_grid_best
# write.csv(as.data.frame(f_grid_best), "model_f.csv")

# ##################################
# #### Create GWAR grid: f(I,R) ####
# ##################################
# 
# ## f(I,R) grid without adjusting for league (since after 2021 NL has DH)
# model_f = glm(PIT_WINS ~ factor(INNING) + factor(CUM_RUNS),
#               data=df_f_grid, family="binomial"(link="logit"))
# save_lm(model_f, "model_f.rds")

####################################
#### Create GWAR grid: g(R|S,O) ####
####################################

df_g_grid <- war2 %>% 
  select(GAME_ID, BAT_HOME_IND, HOME_LEAGUE, YEAR, INNING, REST_INN_RUNS, INN_SITCH, inn_sitch_seq) %>%
  filter(INNING < 6)

df_g_grid_1 = df_g_grid %>%
  group_by(INN_SITCH, REST_INN_RUNS) %>%
  summarise(count=n()) %>%
  group_by(INN_SITCH) %>%
  mutate(p = count/sum(count)) %>%
  ungroup()

G_GRID = matrix(nrow=24, ncol=max(df_g_grid_1$REST_INN_RUNS))
seq_toINN_SITCH = df_g_grid %>% group_by(inn_sitch_seq) %>% slice_head() %>% select(inn_sitch_seq, INN_SITCH) %>% arrange(inn_sitch_seq)
rownames(G_GRID) <- seq_toINN_SITCH$INN_SITCH
colnames(G_GRID) <- 0:(ncol(G_GRID)-1)#paste0("rest_of_inn_runs", 0:(ncol(G_GRID)-1))

for (i in 1:nrow(G_GRID)) {
  for (j in 1:ncol(G_GRID)) {
    df_ij = df_g_grid_1 %>% filter(INN_SITCH == rownames(G_GRID)[i] & REST_INN_RUNS == j-1)
    G_GRID[i,j] = if (nrow(df_ij)==0) 0 else df_ij$p
  }
}

rowSums(G_GRID) ### should all be 1

write.csv(as.data.frame(G_GRID), "model_g.csv")

#############################
########## g PLOTS ##########
#############################

{
  ### plot g(R,S,O) as a function of R, with O = 0, for different base states S
  plot_gRSO <- function(O_) {
    g_0_df = as_tibble(reshape2::melt(G_GRID)) %>%
      rename(SO = Var1, R = Var2, p = value) %>%
      mutate(O = str_sub(SO,end=1),
             S = str_sub(SO,start=3)) %>%
      filter(O == O_) %>%
      mutate(`base state` = S)
    g_0_df
    g_0_df %>% ggplot(aes(color=`base state`,x=R,y=p)) +
      geom_point() +
      geom_line(size=1) +
      labs(
        # title=paste0("g(R|S,O=",O,") as a function of R, for different base states S"),
        x="runs allowed R from now until the end of this half inning",
        y="context-neutral probability") +
      scale_x_continuous(breaks = seq(0,13,by=2)) +
      scale_y_continuous(breaks=seq(0,1,by=0.1))
  }
  
  pg0 = plot_gRSO(0)
  pg0
  pg1 = plot_gRSO(1)
  pg1
  pg2 = plot_gRSO(2)
  pg2
  ggsave(paste0(output_folder,"plot_gRSO_R0.png"), pg0, width = 7, height=5)
  ggsave(paste0(output_folder,"plot_gRSO_R1.png"), pg1, width = 7, height=5)
  ggsave(paste0(output_folder,"plot_gRSO_R2.png"), pg2, width = 7, height=5)
}


