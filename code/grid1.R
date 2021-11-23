library(tidyverse)

######################################################################
######### CREATE GRID 1:  WP(I,R) ####################################
######### I = inning, R = runs accumulated thru this inning ########## 
######################################################################

# read data
data_filepath = "../../TTO_/data/retro_final_PA_1990-2020b.csv" #FIXME
D0 <- read_csv(data_filepath)
D00 <- D0 %>% filter(YEAR >= 2010 & YEAR <= 2019)

# PIT_WINS, CUM_RUNS
D1 <- D00 %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  mutate(bat_team_runs = sum(EVENT_RUNS)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  mutate(game_total_runs = sum(EVENT_RUNS)) %>%
  ungroup() %>%
  mutate(PIT_WINS = as.numeric(bat_team_runs < game_total_runs/2)) %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  mutate(CUM_RUNS = cumsum(EVENT_RUNS)) %>%
  ungroup()
D1 <- D1 %>%
  #filter(SP_IND == TRUE) %>% # filtering out starting pitchers
  filter(!is.na(PIT_WINS)) # error in some games due to error in EVENT_RUNS and EVENT_ER_CT
V1 = D1 %>% filter(GAME_ID == "ANA201004050") %>% arrange(BAT_HOME_IND) %>% #ARI201504240
      select(BAT_HOME_IND, INNING, EVENT_RUNS, bat_team_runs, game_total_runs, PIT_WINS,CUM_RUNS)
# check
#View(V1)
#View(D1 %>% select(GAME_ID, BAT_HOME_IND, INNING,EVENT_RUNS, bat_team_runs, game_total_runs, PIT_WINS,CUM_RUNS))

# Filter last play of every half-inning
D2 <- D1 %>% 
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  filter(row_number() == n()) %>%
  ungroup()
# check
V2 = D2 %>% arrange(GAME_ID, BAT_HOME_IND) %>% select(GAME_ID, BAT_HOME_IND, INNING, PIT_WINS,CUM_RUNS)
#View(V2)

# select relevant columns
grid1_data <- D2 %>% select(GAME_ID, BAT_HOME_IND, INNING, PIT_WINS,CUM_RUNS,OUTS_CT) #will include BASE_CONFIG column when i add it

# create GRID_1: WP(innings, runs)
ni = 9
nr = max(grid1_data$CUM_RUNS)
N = matrix(nrow = ni, ncol = nr+1) # number of occurences matrix
W = matrix(nrow = ni, ncol = nr+1) # number of wins matrix
WP = matrix(nrow = ni, ncol = nr+1) # empirical win proportion matrix
runs_names = paste0(0:nr, "_runs")
innings_names = paste0("inn_", 1:ni)
colnames(N) = runs_names
colnames(W) = runs_names
colnames(WP) = runs_names
rownames(N) = innings_names
rownames(W) = innings_names
rownames(WP) = innings_names
for (i in 1:ni) {
  for (r in 0:nr) {
    E = grid1_data %>% filter(INNING == i, CUM_RUNS == r) 
    N[i,r+1] = nrow(E) 
    W[i,r+1] = sum(E$PIT_WINS)
    WP[i,r+1] = W[i,r+1] / N[i,r+1]
  }
}

N[,1:11]
WP[,1:11]
# N
# WP
N[1:8,1:11]
WP[1:8,1:11]

# write data
write_csv(grid1_data, "grid1_data.csv")
write_csv(WP, "WP1.csv")
write_csv(N, "N1.csv")


# View the grid nicely?
# library(gt)
# WP_gt = as_tibble(WP[,1:11]) %>% gt() 
# N_gt = as_tibble(N[,1:11]) %>% gt() 
# WP_gt
# gtsave(WP_gt, "grid1_WP.png")
# gtsave(N_gt, "grid1_N.png")

##########################################
######### logistic smoothing #############
##########################################

G1 <- grid1_data %>% filter(INNING == 5) %>% rename(S = CUM_RUNS)
  
lr <- glm(PIT_WINS ~ poly(S,10), data=G1, family="binomial")

xx=9
predict(lr, data.frame(S=0:10),type="response")[1:xx]
#WP[1,1:xx]


#predict(lr,data.frame(S=3.2),type="response")


#############################
######### mBART #############
#############################
# 
# # mBART packages
# library(Rcpp)
# library(remotes)
# library(mBART)
# 
# x_train = as.matrix(grid1_data %>% filter(INNING==1) %>% select(CUM_RUNS)) #select(INNING, CUM_RUNS)
# y_train = as.matrix(grid1_data %>% filter(INNING==1) %>% select(PIT_WINS))
# 
# set.seed(99)
# bfmc = monbart(x_train,y_train)
# # save the bfmc objects
# saveRDS(bfmc, file = "grid1_bfmc.rds")
# #bfmc <- readRDS("grid1_bfmc.rds")
# ## plot results
# plot(x_train,y_train)
# lines(x_train,bfmc$yhat.train.mean,col="blue",lwd=3,xlab="x",ylab="posterior mean of f(x)")



# # training matrices
# train_idx = E$train1
# good_idxs = complete.cases(X_, P_, D_) # remove NA's
# X = X_[train_idx & good_idxs,]
# P = P_[train_idx & good_idxs]
# D = D_[train_idx & good_idxs,]
# P2 = P2_[train_idx & good_idxs]
# 
# E_train = E %>% 
#   filter(train1) %>% 
#   filter(!kickoff_attempt) %>%
#   select(yardline_100, P2) %>%
#   arrange(yardline_100, P2)
# x_train = matrix(E_train$yardline_100, ncol=1)
# y_train = matrix(E_train$P2, ncol=1)
# 
# ## run monotonic bart
# set.seed(99)
# bfmc = monbart(x_train,y_train)
# # save the bfmc objects
# saveRDS(bfmc, file = "bakeoff_1_bfmc.rds")
# #bfmc <- readRDS("bakeoff_1_bfmc.rds") 
# ## plot results
# plot(x_train,y_train)
# lines(x_train,bfmc$yhat.train.mean,col="blue",lwd=3,xlab="x",ylab="posterior mean of f(x)")
# 
# a = bfmc$yhat.train
# aa=bfmc$yhat.train.mean
