
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
train_f_LR = function(df) {
  # browser()
  LRmodel = glm(PIT_WINS ~ factor(INNING)*bs(log(CUM_RUNS+1),df=4),
      data=df, family="binomial"(link="logit"))
  LRmodel
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

####################################
#### model selection for f(I,R) ####
####################################

set.seed(22) # Kershaw!
train_idxs = sort(sample(1:nrow(df_f_grid), size=round(nrow(df_f_grid)*1/2)))
test_idxs = setdiff(1:nrow(df_f_grid), train_idxs)
df_f_train = df_f_grid[train_idxs,]
df_f_test = df_f_grid[test_idxs,]

### logistic regression
model_f_LR = train_f_LR(df_f_train) 
  
### empirical grid
f_grid_empirical = train_empirical_f_grid(df_f_train) 

### out-of-sample loss
loss_df = data.frame(
  loss_LR = logloss(df_f_test$PIT_WINS, predict(model_f_LR, df_f_test, type="response")),
  loss_empirical_f_grid = logloss(df_f_test$PIT_WINS, predict_f_grid_empirical(f_grid_empirical,df_f_test$INNING,df_f_test$CUM_RUNS))
  # loss_xgb = last(xgb_f_grid$evaluation_log[["test_logloss"]])
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

LR_to_grid <- function(f_model_LR,yr,lg) {
  max_r = 10
  f_LR_grid = matrix(nrow = 9, ncol = max_r+1) ### WP matrix for LRoost
  for (inn in 1:9) {
    test_df_inn = tibble(INNING = inn, CUM_RUNS = 0:(max_r), HOME_LEAGUE = lg, YEAR = yr)
    f_LR_grid[inn,] = predict(f_model_LR, test_df_inn, type="response")
  }
  rownames(f_LR_grid) = paste0("INNING_",1:9)
  colnames(f_LR_grid) = paste0("CUM_RUNS_",(0:(ncol(f_LR_grid)-1)))
  return(f_LR_grid)
}

### train full models
f_model_LR = train_f_LR(df_f_grid)
f_grid_empirical = train_empirical_f_grid(df_f_grid)

f_grid_LR = LR_to_grid(f_model_LR,2019,"AL")

plot_WP_matrixIR(f_grid_empirical)


library(splines)

f_model_LR = glm(PIT_WINS ~ factor(INNING)*bs(log(CUM_RUNS+1),df=4),
              data=df_f_grid, family="binomial"(link="logit"))
f_grid_LR = LR_to_grid(f_model_LR,2019,"AL")


plot_WP_matrixIR(f_grid_LR)


# 
# max_r = 10#max(D$CUM_RUNS)
# WP_LR = matrix(nrow = 9, ncol = max_r+1) ### WP matrix for model_f_LR
# WP_xgb = matrix(nrow = 9, ncol = max_r+1) ### WP matrix for XGBoost
# for (inn in 1:9) {
#   test_df_inn = tibble(
#     INNING = inn, CUM_RUNS = 0:(max_r), BAT_HOME_IND = 1, HOME_LEAGUE = "AL", YEAR = 2019
#   )
#   WP_LR[inn,] = predict(f_model_LR, test_df_inn, type="response")
#   WP_xgb[inn,] = pred_xgb_f_grid(test_df_inn, f_model_xgb)
# }
# 
# plot_WP_LR = plot_WP_matrixIR(WP_LR)
# plot_WP_f_grid_empirical = plot_WP_matrixIR(f_grid_empirical)
# plot_WP_xgb = plot_WP_matrixIR(WP_xgb)
# 
# plot_WP_LR
# plot_WP_f_grid_empirical
# plot_WP_xgb
# 
# ggsave(paste0(output_folder,"plot_fIR_R_gridEmpirical.png"), plot_WP_f_grid_empirical, width=7, height=5)
# ggsave(paste0(output_folder,"plot_fIR_R_xgb.png"), plot_WP_xgb, width=7, height=5)

#######################################################################
### fit the best f(I,R) model, separately for each year and league ####
#######################################################################


### convert LR models to grids
WP_LR_grids = list()
for (yr in sort(unique(df_f_grid$YEAR))) {
  for (lg in sort(unique(df_f_grid$HOME_LEAGUE))) {
    df_f_yl = df_f_grid %>% filter(YEAR == yr & HOME_LEAGUE == lg)
    # alpha = 0.6 ### exponential decay weight
    # df_f_yl = df_f_grid %>% 
    #   filter(YEAR <= yr & HOME_LEAGUE == lg) %>%
    #   mutate(w = alpha^(yr-YEAR) )
    # table(df_f_yl$w)
    # f_LR_yl = fit_LR_f_grid(params, df_f_yl, w=TRUE)
    f_LR_yl = train_f_LR(df_f_yl)
    f_grid_yl = LR_to_grid(f_LR_yl,yr,lg)
    WP_LR_grids[[paste0("f_grid_",yr,"_",lg)]] = f_grid_yl
    
    ### plot LR grid
    WP_LR_plot = plot_WP_matrixIR(f_grid_yl)
    ggsave(paste0(output_folder,"plot_fIR_R_LR_",paste0(yr,"_",lg),".png"), WP_LR_plot, width=7, height=5)
  }
}

### save LR grids
saveRDS(WP_LR_grids, "model_f.rds")

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


