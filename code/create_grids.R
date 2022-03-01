library(tidyverse)
filename = "retro_final_PA_1990-2000d.csv" # from dropbox. see README.md
war2 <- read_csv("BRILL.csv") 

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

D <- last_play_every_inning %>% select(GAME_ID, BAT_HOME_IND, INNING, CUM_RUNS, PIT_WINS)
#Make new column <- 1 if inning = 9 and bat team bats or 
N = matrix(nrow = 9, ncol = max(D$CUM_RUNS)) # number of occurences matrix
W = matrix(nrow = 9, ncol = max(D$CUM_RUNS)) # number of wins matrix
WP = matrix(nrow = 9, ncol = max(D$CUM_RUNS)) # context neutral win probability matrix
for (i in 1:nrow(N)) {
  for (j in 1:ncol(N)) {
    E = D %>% filter(INNING == i, CUM_RUNS == j-1) 
    N[i,j] = nrow(E) 
    W[i,j] = sum(E$PIT_WINS)
    WP[i,j] = W[i,j] / N[i,j]
  }
}

colnames(WP) = paste0(0:(ncol(WP)-1), " runs")
rownames(WP) = paste0(1:9, " innings")
WP = replace_na(WP,0)

### Examine WP vs Runs, for each inning
WPi = as_tibble(t(WP))
colnames(WPi) = paste0("inn",1:9)
WPii = stack(WPi) 
WPii$runs = rep(0:(nrow(WPi)-1), 9)
pWPii = WPii %>% filter(runs <= 13) %>%
  ggplot(aes(x=runs,y=values,color=ind)) + geom_point() + geom_line()
plotly::ggplotly(pWPii)

### Examine WP vs Runs, for each inning
WPr = as_tibble(WP)
colnames(WPr) = paste0("runs",0:(ncol(WP)-1))
WPrr = stack(WPr[,1:14]) 
WPrr$inn = rep(1:9, 14)
pWPrr = WPrr %>%
  ggplot(aes(x=inn,y=values,color=ind)) + geom_point() + geom_line()
plotly::ggplotly(pWPrr)

# write.csv(as.data.frame(WP), file = "f_grid.csv")

#######################
#### CREATE GRID 2 ####
#######################

#Determining how many runs were scored during remainder of innings
war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  mutate(final = if_else(row_number() == n(), 1, 0)) %>% #determining last at-bat of innings
  mutate(start = if_else(row_number() == 1, 1, 0)) %>%
  mutate(inning_end_runs = case_when(final == 1 ~ CUM_RUNS))

war2 <- war2 %>%
  ungroup() %>%
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

Brill_starters <- war2 %>% filter (INNING < 6)

Brill_starters <- Brill_starters %>%
  mutate(sequence = case_when(
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
#View(Brill_starters %>% group_by(INN_SITCH) %>% slice_head() %>% select(INN_SITCH, OUTS_CT, BASE_STATE) )

N_ER = matrix(nrow = 24, ncol = 1) # number of occurrences matrix
FF <- Brill_starters %>% select(GAME_ID, BAT_HOME_IND, INNING, REST_INN_RUNS, INN_RUNS, CUM_RUNS2, OUTS_CT, BASE_STATE, INN_SITCH)
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

# write.csv(as.data.frame(P), file = "g_grid.csv")

### Save war_2
# write_csv(war2, "war2.csv")






# ### Impute bottom of 9th inning PIT_WINS...
# last_play_every_inning1 <- last_play_every_inning %>%  
#   select(GAME_ID, BAT_HOME_IND, INNING, CUM_RUNS, PIT_WINS)
# 
# missing_bottom_9 = last_play_every_inning1 %>% 
#   filter(INNING == 9) %>% group_by(GAME_ID) %>% 
#   summarise(count=n()) %>% 
#   filter(count == 1) %>% select(-c(count)) %>% 
#   mutate(BAT_HOME_IND=1, INNING=9,CUM_RUNS=NA,PIT_WINS=0)
# 
# last_play_every_inning2 = bind_rows(last_play_every_inning1,missing_bottom_9) %>%
#   arrange(GAME_ID,INNING)
# 
# last_play_every_inning3 = last_play_every_inning2 %>% 
#   mutate(CUM_RUNS = ifelse(is.na(CUM_RUNS), lag(CUM_RUNS,n=2), CUM_RUNS))
# D <- last_play_every_inning3 %>% select(GAME_ID, BAT_HOME_IND, INNING, CUM_RUNS, PIT_WINS)





