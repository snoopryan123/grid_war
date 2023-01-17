
source("0_load_stuff.R")
war2 = read_csv("war2.csv")

##################################
#### Create GWAR grid: f(I,R) ####
##################################

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
  mutate(PIT_LEAGUE = ifelse(BAT_HOME_IND, AWAY_LEAGUE, HOME_LEAGUE)) %>%
  select(GAME_ID, BAT_HOME_IND, PIT_LEAGUE, YEAR, INNING, CUM_RUNS, PIT_WINS) %>%
# in the 9th inning only use away batters, to avoid the bias that home batters in the 9th usually lose
  filter(!(INNING == 9 & BAT_HOME_IND == 1))

## f(I,R) grid without adjustments
model_f_0 = glm(PIT_WINS ~ factor(INNING) + factor(CUM_RUNS),
                data=df_f_grid, family="binomial"(link="logit"))
# saveRDS(model_f_0, file = "model_f_0.rds")
save_lm(model_f_0, "model_f_0.rds")

### f(I,R) grid adjust for League, Home, and Year (fixed effects)
model_f = glm(PIT_WINS ~ factor(INNING) + factor(CUM_RUNS) +
                BAT_HOME_IND + factor(PIT_LEAGUE) + factor(YEAR),
           data=df_f_grid, family="binomial"(link="logit"))
# saveRDS(model_f, file = "model_f.rds")
save_lm(model_f, "model_f.rds")

### smoothed f(I,R) as a function of R, for each inning I
{
  max_r = 15#max(D$CUM_RUNS)
  WP = matrix(nrow = 9, ncol = max_r) # number of wins matrix
  for (inn in 1:9) {
    test_df_inn = tibble(
      INNING = inn, CUM_RUNS = 0:(max_r-1), BAT_HOME_IND = 1, PIT_LEAGUE = "AL", YEAR = 2019)
    predict(model_f, test_df_inn, type="response")
    WP[inn,] = predict(model_f, test_df_inn, type="response")
  }
  
  ### Examine WP vs Runs, for each inning
  WPi = as_tibble(t(WP))
  colnames(WPi) = paste0("inn",1:9)
  WPii = stack(WPi) 
  WPii$runs = rep(0:(nrow(WPi)-1), 9)
  
  pWPiis = WPii %>% filter(runs <= 13) %>%
    mutate(inning=str_sub(ind,start=4)) %>%
    ggplot(aes(x=runs,y=values,color=inning)) + 
    geom_point() + 
    geom_line(size=1) +
    labs(
      # title=TeX("smoothed $f(I,R)$ as a function of $R$, for each $I$"),
      y="context-neutral win probability",
      x="runs allowed through the end of the given inning") +
    scale_x_continuous(breaks=seq(0,30,by=2)) +
    scale_y_continuous(breaks=seq(0,1,by=0.1))
  pWPiis
  # plotly::ggplotly(pWPiis)
  ggsave(paste0(output_folder,"plot_fIR_R_smoothed.png"), pWPiis, width=9, height=8)
}

####################################
#### Create GWAR grid: g(R|S,O) ####
####################################

df_g_grid <- war2 %>% 
  select(GAME_ID, BAT_HOME_IND, PIT_LEAGUE, YEAR, INNING, REST_INN_RUNS, INN_SITCH, inn_sitch_seq) %>%
  filter(INNING < 6)

N_ER = matrix(nrow = 24, ncol = 1) # number of occurrences matrix
FF <- war2 %>% select(GAME_ID, BAT_HOME_IND, INNING, REST_INN_RUNS, INN_RUNS, CUM_RUNS2, OUTS_CT, BASE_STATE, INN_SITCH)
N_ER <- FF %>% group_by(INN_SITCH) %>% count()

E2 <- df_g_grid %>% select(GAME_ID, BAT_HOME_IND, INNING, REST_INN_RUNS, inn_sitch_seq)
N = matrix(nrow = 24, ncol = max(E2$REST_INN_RUNS)) # number of occurrences matrix
G_GRID = matrix(nrow = 24, ncol = max(E2$REST_INN_RUNS))
for (i in 1:nrow(N)) {
  for (j in 1:ncol(N)) {
    E = E2 %>% filter(inn_sitch_seq == i, REST_INN_RUNS == j-1)
    N[i,j] = nrow(E)
    G_GRID[i,j] = N[i,j]/N_ER$n[i]
  }
}
seq_toINN_SITCH = df_g_grid %>% group_by(inn_sitch_seq) %>% slice_head() %>% select(inn_sitch_seq, INN_SITCH) %>% arrange(inn_sitch_seq)
rownames(G_GRID) <- seq_toINN_SITCH$INN_SITCH
colnames(G_GRID) <- 0:(ncol(G_GRID)-1)#paste0("rest_of_inn_runs", 0:(ncol(G_GRID)-1))

write.csv(as.data.frame(G_GRID), "g_grid.csv")

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
  ggsave(paste0(output_folder,"plot_gRSO_R0.png"), pg0, width = 9, height=8)
  ggsave(paste0(output_folder,"plot_gRSO_R1.png"), pg1, width = 9, height=8)
  ggsave(paste0(output_folder,"plot_gRSO_R2.png"), pg2, width = 9, height=8)
}


