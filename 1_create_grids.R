library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
# theme_set(theme_solarized())
output_folder = "./plots/"

##filename = "retro_final_PA_1990-2000d.csv" # from dropbox. see README.md
filename = "../../TTO_/data/retro_final_PA_1990-2020d.csv"
war2 <- read_csv(filename) 

# PIT_WINS === 1{PIT's team wins this game}
war2 <- war2 %>% filter(2010 <= YEAR & YEAR <= 2019)
war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  mutate(bat_team_runs = sum(EVENT_RUNS))
war2 <- war2 %>%
  group_by(GAME_ID) %>%
  mutate(total_runs = sum(EVENT_RUNS))
war2 <- war2 %>%
  mutate(PIT_RUNS = total_runs - bat_team_runs) 
war2 <- war2 %>%
  mutate(PIT_WINS = case_when(
    PIT_RUNS > bat_team_runs ~ 1,
    PIT_RUNS < bat_team_runs ~ 0
  ))
war2 <- war2 %>% filter(!is.na(PIT_WINS)) # filter NA

# Accumulating runs
war2 <- war2 %>%
  ungroup() %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  mutate(CUM_RUNS = cumsum(EVENT_RUNS)) %>%
  ungroup()

# Filtering out every last play of inning
last_play_every_inning <- war2 %>% 
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  filter(row_number() == n()) %>%
  ungroup()


# Check
# View(last_play_every_inning %>% arrange(BAT_HOME_IND) %>% filter(GAME_ID == "ANA201804020") %>%
#        select(GAME_ID, BAT_HOME_IND, INNING,
#               EVENT_TX, EVENT_RUNS, CUM_RUNS))

#######################
#### CREATE GRID 1 ####
#######################

D0 <- last_play_every_inning %>% 
  mutate(PIT_LEAGUE = ifelse(BAT_HOME_IND, AWAY_LEAGUE, HOME_LEAGUE)) %>%
  select(GAME_ID, BAT_HOME_IND, PIT_LEAGUE, YEAR, INNING, CUM_RUNS, PIT_WINS)
# in the 9th inning only use away batters, to avoid the bias that home batters in the 9th usually lose
D = D0 %>% filter(!(INNING == 9 & BAT_HOME_IND == 1))
## adjust for League, Home, and Year (fixed effects)
f_lrm = glm(PIT_WINS ~ BAT_HOME_IND + factor(PIT_LEAGUE) + factor(YEAR) + 
           factor(INNING) + factor(CUM_RUNS),
           data=D,family="binomial"(link="logit"))
# saveRDS(f_lrm, file = "f_lrm.rds")

max_r = 15#max(D$CUM_RUNS)
WP = matrix(nrow = 9, ncol = max_r) # number of wins matrix
for (inn in 1:9) {
  test_df_inn = tibble(BAT_HOME_IND = 1, PIT_LEAGUE = "AL", YEAR = 2019, INNING = inn, CUM_RUNS = 0:(max_r-1))
  predict(f_lrm, test_df_inn, type="response")
  WP[inn,] = predict(f_lrm, test_df_inn, type="response")
}

### Examine WP vs Runs, for each inning
WPi = as_tibble(t(WP))
colnames(WPi) = paste0("inn",1:9)
WPii = stack(WPi) 
WPii$runs = rep(0:(nrow(WPi)-1), 9)

### smoothed f(I,R) as a function of R, for each inning I
{
  pWPiis = WPii %>% filter(runs <= 13) %>%
    mutate(inning=str_sub(ind,start=4)) %>%
    ggplot(aes(x=runs,y=values,color=inning)) + 
    geom_point() + 
    geom_line(size=1) +
    labs(
      # title=TeX("smoothed $f(I,R)$ as a function of $R$, for each $I$"),
      y="context-neutral win probability",
      x="runs allowed through the end of the given inning")
  pWPiis
  # plotly::ggplotly(pWPiis)
  # ggsave(paste0(output_folder,"plot_fIR_R_smoothed.png"), pWPiis)
}

#######################
#### CREATE GRID 2 ####
#######################

#Determining how many runs were scored during remainder of innings
war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  mutate(final = if_else(row_number() == n(), 1, 0)) %>% #determining last at-bat of innings
  mutate(start = if_else(row_number() == 1, 1, 0)) %>%
  mutate(inning_end_runs = case_when(final == 1 ~ CUM_RUNS)) %>%
  ungroup()

war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  mutate(CUM_RUNS_BY_INNING = cumsum(EVENT_RUNS)) %>%
  mutate(CUM_RUNS2 = CUM_RUNS_BY_INNING - EVENT_RUNS) %>%
  ungroup()

#add in total runs scored in the inning
war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  mutate(INN_RUNS = sum(EVENT_RUNS)) %>%
  ungroup()

#add in runs scored rest of the inning
war2 <- war2 %>%
  mutate(REST_INN_RUNS = INN_RUNS - CUM_RUNS2) %>%
  ungroup()

# Check
# View(war2 %>% select(GAME_ID,BAT_HOME_IND,INNING,CUM_RUNS,EVENT_RUNS,CUM_RUNS2,INN_RUNS,REST_INN_RUNS) %>% filter(row_number() <= 200))

#concat inning and base state
war2$INN_SITCH <- paste(war2$OUTS_CT, war2$BASE_STATE)

### Save war_2
write_csv(war2, "war2.csv")

E0 <- war2 %>%
  mutate(inn_sitch_seq = case_when(
    INN_SITCH == "0 000" ~ 1,
    INN_SITCH == "0 001" ~ 2,
    INN_SITCH == "0 010" ~ 3,
    INN_SITCH == "0 011" ~ 4,
    INN_SITCH == "0 100" ~ 5,
    INN_SITCH == "0 101" ~ 6,
    INN_SITCH == "0 110" ~ 7,
    INN_SITCH == "0 111" ~ 8,
    INN_SITCH == "1 000" ~ 9,
    INN_SITCH == "1 001" ~ 10,
    INN_SITCH == "1 010" ~ 11,
    INN_SITCH == "1 011" ~ 12,
    INN_SITCH == "1 100" ~ 13,
    INN_SITCH == "1 101" ~ 14,
    INN_SITCH == "1 110" ~ 15,
    INN_SITCH == "1 111" ~ 16,
    INN_SITCH == "2 000" ~ 17,
    INN_SITCH == "2 001" ~ 18,
    INN_SITCH == "2 010" ~ 19,
    INN_SITCH == "2 011" ~ 20,
    INN_SITCH == "2 100" ~ 21,
    INN_SITCH == "2 101" ~ 22,
    INN_SITCH == "2 110" ~ 23,
    INN_SITCH == "2 111" ~ 24
  ))

# Check
#View(E0 %>% group_by(INN_SITCH) %>% slice_head() %>% select(INN_SITCH, OUTS_CT, BASE_STATE) )

E1 <- E0 %>% 
  mutate(PIT_LEAGUE = ifelse(BAT_HOME_IND, AWAY_LEAGUE, HOME_LEAGUE)) %>%
  select(GAME_ID, BAT_HOME_IND, PIT_LEAGUE, YEAR, INNING, REST_INN_RUNS, INN_SITCH, inn_sitch_seq) %>%
  filter(INNING < 6)

N_ER = matrix(nrow = 24, ncol = 1) # number of occurrences matrix
FF <- E0 %>% select(GAME_ID, BAT_HOME_IND, INNING, REST_INN_RUNS, INN_RUNS, CUM_RUNS2, OUTS_CT, BASE_STATE, INN_SITCH)
N_ER <- FF %>% group_by(INN_SITCH) %>% count()

D <- Brill_starters %>% select(GAME_ID, BAT_HOME_IND, INNING, REST_INN_RUNS, sequence)
N = matrix(nrow = 24, ncol = max(D$REST_INN_RUNS)) # number of occurrences matrix
P = matrix(nrow = 24, ncol = max(D$REST_INN_RUNS))
for (i in 1:nrow(N)) {
  for (j in 1:ncol(N)) {
    E = D %>% filter(sequence == i, REST_INN_RUNS == j-1)
    N[i,j] = nrow(E)
    P[i,j] = N[i,j]/N_ER$n[i]
  }
}
seq_toINN_SITCH = Brill_starters %>% group_by(sequence) %>% slice_head() %>% select(sequence, INN_SITCH) %>% arrange(sequence)
rownames(P) <- seq_toINN_SITCH$INN_SITCH
colnames(P) <- 0:(ncol(P)-1)#paste0("rest_of_inn_runs", 0:(ncol(P)-1))

#############################
########## g PLOTS ##########
#############################

### plot g(R,S,O) as a function of R, with O = 0, for different base states S
plot_gRSO <- function(O_) {
  g_0_df = as_tibble(reshape2::melt(P)) %>%
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
      y="context-neutral probability")
}

{
  pg0 = plot_gRSO(0)
  pg0
  pg1 = plot_gRSO(1)
  pg1
  pg2 = plot_gRSO(2)
  pg2
  ggsave(paste0(output_folder,"plot_gRSO_R0.png"), pg0)
  ggsave(paste0(output_folder,"plot_gRSO_R1.png"), pg1)
  ggsave(paste0(output_folder,"plot_gRSO_R2.png"), pg2)
}



















# ######### POISSON REGRESSION ######### 
# g_prm = glm(REST_INN_RUNS ~ BAT_HOME_IND + factor(PIT_LEAGUE) + factor(YEAR) + 
#               factor(INN_SITCH),
#             data=E1,family="poisson")
# # saveRDS(g_prm, file = "g_prm.rds")
# 
# max_r = 13#max(E1$REST_INN_RUNS)
# inn_sitch_0 = c("0 000","0 100","0 010","0 001","0 110","0 101","0 011","0 111") 
# inn_sitch_1 = c("1 000","1 100","1 010","1 001","1 110","1 101","1 011","1 111") 
# inn_sitch_2 = c("2 000","2 100","2 010","2 001","2 110","2 101","2 011","2 111") 
# inn_sitches = c(inn_sitch_0,inn_sitch_1,inn_sitch_2)
# P = matrix(nrow = length(inn_sitches), ncol = max_r+1) # number of wins matrix
# rownames(P) = inn_sitches
# colnames(P) = 0:max_r
# for (i in 1:length(inn_sitches)) {
#   is = inn_sitches[i]
#   test_df_inn = tibble(BAT_HOME_IND = 1, PIT_LEAGUE = "AL", YEAR = 2019, INN_SITCH = is)
#   lambda = predict(g_lrm, test_df_inn, type="response")
#   P[i,] = dpois(0:max_r, lambda)
# }
# 
# ### plot g(R|S,O) for O = 0
# g_0_df = as_tibble(reshape2::melt(P)) %>%
#   rename(SO = Var1, R = Var2, p = value) %>%
#   mutate(O = str_sub(SO,end=1),
#          S = str_sub(SO,start=3)) %>%
#   filter(O == "0") 
# g_0_df
# g_0_df %>% ggplot(aes(color=S,x=R,y=p)) +
#   geom_point() +
#   geom_line(size=1) +
#   labs(
#     # title=paste0("g(R|S,O=",O,") as a function of R, for different base states S"),
#     x="runs allowed R from now until the end of this half inning",
#     y="context-neutral probability")

