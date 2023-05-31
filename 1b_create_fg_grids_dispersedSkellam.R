
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
  mutate(BAT_TEAM_ID = ifelse(BAT_HOME_IND, HOME_TEAM_ID, AWAY_TEAM_ID)) %>%
  mutate(HOME_LEAGUE = ifelse(BAT_HOME_IND, AWAY_LEAGUE, HOME_LEAGUE)) %>%
  select(GAME_ID, BAT_HOME_IND, BAT_TEAM_ID, HOME_LEAGUE, YEAR, INNING, CUM_RUNS, INN_RUNS, PIT_WINS) %>%
  # in the 9th inning only use away batters, to avoid the bias that home batters in the 9th usually lose
  filter(!(INNING == 9 & BAT_HOME_IND == 1)) %>%
  filter(INNING <= 9) %>%
  filter(CUM_RUNS <= 10)
df_f_grid

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

######################################################
#### f(I,R) using Poisson model: the Skellam grid ####
######################################################

### f(I,R) grid using Skellam distribution
library(skellam)
get_f_grid_Skellam <- function(lambda_X, lambda_Y, max_r = 10) {
  f_grid = matrix(nrow = 9, ncol = max_r+1) ### WP matrix
  rownames(f_grid) = paste0("INNING_",1:9)
  colnames(f_grid) = paste0("CUM_RUNS_",(0:(ncol(f_grid)-1)))
  for (R in 0:max_r) {
    for (I in 1:9) {
      if (I < 9) {
        t1 = pskellam(R, lambda1 = 9*lambda_X, lambda2 = (9-I)*lambda_Y, lower.tail = FALSE)
        t2 = dskellam(R, lambda1 = 9*lambda_X, lambda2 = (9-I)*lambda_Y)
        f_grid[I,R+1] = t1 + 1/2*t2
      } else {
        t1 = ppois(R, lambda = 9*lambda_X, lower.tail = FALSE)
        t2 = dpois(R, lambda = 9*lambda_X)
        f_grid[I,R+1] = t1 + 1/2*t2
      }
    }
  }
  return(f_grid)
}
# ### check
# mean((df_f_grid %>% filter(INNING==1))$CUM_RUNS)
# get_f_grid_Skellam(lambda_X = 0.54, lambda_Y = 0.54)

###
monte_carlo_f_grid <- function(lambda_hat, sigma_hat, B=100) {
  f_grid = NULL
  for (b in 1:B) {
    ### sample lambda_X and lambda_Y
    lambda_X_b = truncnorm::rtruncnorm(n=1, a=0, mean=lambda_hat, sd=sigma_hat)
    lambda_Y_b = truncnorm::rtruncnorm(n=1, a=0, mean=lambda_hat, sd=sigma_hat)
    ### get f(I,R) grid for this league-season and monte carlo sample b
    f_grid_b = get_f_grid_Skellam(lambda_X_b, lambda_Y_b, max_r = 10)
    if (b == 1) {
      ### initialize the grid f(I,R | yr,lg)
      f_grid = f_grid_b
    } else {
      ### running average of the grid
      f_grid = 1/b * f_grid_b + (b-1)/b * f_grid
    }
  }
  return(f_grid)
}

###
df_lambda_yr_lg = df_f_grid %>%
  group_by(YEAR,HOME_LEAGUE,BAT_TEAM_ID) %>%
  summarise(
    lambda = mean(INN_RUNS),
    sigma = sd(INN_RUNS)
  ) %>%
  group_by(YEAR,HOME_LEAGUE) %>%
  summarise(
    lambda = mean(lambda),
    sigma = mean(sigma)
  ) 
df_lambda_yr_lg

######################################################
#### tune k for the fit dispersed Skellam f(I,R)  ####
######################################################

lambdaF = mean(df_f_grid$INN_RUNS)
sigmaF = sd(df_f_grid$INN_RUNS)

# ks = seq(0.1,1,by=0.05)
ks = seq(0.2,0.3,by=0.01)
logLosses = numeric(length(ks))
for (i in 1:length(ks)) {
  k = ks[i]
  f_grid_k = monte_carlo_f_grid(lambdaF, sigmaF*k)
  f_grid_k1 = reshape2::melt(f_grid_k) %>%
    mutate(
      INNING = as.numeric(str_sub(Var1, start=8)),
      CUM_RUNS = as.numeric(str_sub(Var2, start=10)),
    ) %>%
    rename(wp_hat = value) %>%
    select(-c(Var1,Var2)) %>%
    as_tibble()
  eval_df_k = (df_f_grid %>%
    left_join(f_grid_k1) %>%
    summarise(logloss_ = logloss(PIT_WINS, wp_hat)))$logloss_
  logLosses[i] = eval_df_k
}
plot(logLosses)
ks[which(logLosses == min(logLosses))]

######################################################
#### fit dispersed Skellam f(I,R) grid and plot!  ####
######################################################

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

set.seed(5437154)
f_grids_Skellam = list()
# yr = 2019; lg = "NL"; {
for (yr in unique(df_f_grid$YEAR)) {
  for (lg in unique(df_f_grid$HOME_LEAGUE)) {
    print(paste0("computing f(I,R) grid for lg ", lg, " and szn ", yr))
    lambda_yr_lg = (df_lambda_yr_lg %>% filter(YEAR == yr & HOME_LEAGUE == lg))$lambda
    sigma_yr_lg = (df_lambda_yr_lg %>% filter(YEAR == yr & HOME_LEAGUE == lg))$sigma
    # f_grid_yr_lg = monte_carlo_f_grid(lambda_yr_lg, sigma_yr_lg)
    k = 0.28 # tuned above
    f_grid_yr_lg = monte_carlo_f_grid(lambda_yr_lg, sigma_yr_lg*k)
    ### save Skellam grid
    f_grids_Skellam[[paste0("f_grid_",yr,"_",lg)]] = f_grid_yr_lg
    ### plot Skellam grid
    plot_f_grid_yr_lg = plot_WP_matrixIR(f_grid_yr_lg)
    ggsave(paste0(output_folder,"plot_fIR_R_disperesedSkellam_",paste0(yr,"_",lg),".png"), plot_f_grid_yr_lg, width=7, height=5)
  }
}
### save Skellam f grids
saveRDS(f_grids_Skellam, "model_f_disperesedSkellam.rds")
# saveRDS(f_grids_Skellam, "model_f.rds")

# ### plot empirical grids
# for (yr in unique(df_f_grid$YEAR)) {
#   for (lg in unique(df_f_grid$HOME_LEAGUE)) {
#     f_grid_emp = train_empirical_f_grid(df_f_grid %>% filter(YEAR == yr & HOME_LEAGUE == lg))
#     plot_f_grid_yr_lg = plot_WP_matrixIR(f_grid_emp)
#     ggsave(paste0(output_folder,"plot_fIR_empiricalGrid_",paste0(yr,"_",lg),".png"), plot_f_grid_yr_lg, width=7, height=5)
#   }
# }

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


