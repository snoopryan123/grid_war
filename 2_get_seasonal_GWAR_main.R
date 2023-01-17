
source("0_load_stuff.R")

#######################################
### load f,g grids and other models ###
#######################################

# f_lrm <- readRDS("f_lrm.rds") 
# g_grid <- read.csv("g_grid.csv",row.names = 1, header= TRUE)

model_f = load_lm("model_f.rds")
model_f_0 = load_lm("model_f_0.rds")
model_g = read.csv("model_g.csv", header=T, row.names=1)

### Park Effects
park_fx_name = "ridge_PF" #FIXME
park_fx_df = read_csv("1d_park_fx/obs_ridge_PF.csv") %>% select(PARK, park_factor) #FIXME
# park_fx_name = "fangraphs_PF" #FIXME
# park_fx_df = read_csv("1a_park_fx/obs_fg_PF.csv") %>% select(PARK, park_factor) #FIXME
# park_fx_name = "espn_PF" #FIXME
# park_fx_df = read_csv("1a_park_fx/obs_espn_PF.csv") %>% select(PARK, park_factor) #FIXME

#################
### load data ###
#################

war2_ogg = read_csv("war2.csv")
war2_og <- war2_ogg %>% 
  filter(SP_IND | lag(SP_IND, default=FALSE)) %>%
  left_join(park_fx_df) %>%
  mutate(park_factor = replace_na(park_factor, 0)) %>%
  group_by(INNING, BAT_HOME_IND) %>%
  mutate(
    INN_SITCH_after = lead(INN_SITCH, default="0 000")
  ) %>%
  ungroup()

##################################
### grid abstraction functions ###
##################################

# start = identifying pitchers' first at-bat in an inning
# final = identifying pitchers' last at-bat
# pit_occurence = identifying every pitchers' number at-bat
# i = inning, j = base-out-state, k = accumulated runs so far, w = tracker (Rest of inning runs)

MAX_INNING_RUNS = 10  
f <- function(i,r,alpha, home,lg,yr, parkFx=TRUE,adjustConfounders=TRUE) {
  ### i == inning, r == runs, alpha == park effect
  ### confounders:  home, league, year
  
  r = ifelse(r+1 >= MAX_INNING_RUNS, MAX_INNING_RUNS, r)
  f_lrm = if (adjustConfounders) model_f else model_f_0
  
  if (parkFx) {
    ### park adjustment...
    r.minus.1 = ifelse(r > 0, r-1, 0)
    r.plus.1 = ifelse(r < MAX_INNING_RUNS, r+1, MAX_INNING_RUNS)
    f_ir.minus.1 = predict(f_lrm, 
                           tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r.minus.1), 
                           type="response")[[1]]
    f_ir = predict(f_lrm, 
                   tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r), 
                   type="response")[[1]]
    f_ir.plus.1 = predict(f_lrm, 
                          tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r.plus.1), 
                          type="response")[[1]]
    h = abs(alpha)*i
    h = ifelse(h > 1, 1, h)
    h = ifelse(h < 0, 0, h)
    ifelse(alpha < 0 & r < MAX_INNING_RUNS,   (1-h)*f_ir + h*f_ir.plus.1,
           ifelse(alpha > 0 & r > 0,                 (1-h)*f_ir + h*f_ir.minus.1,
                  ifelse(alpha > 0,                         (1+h)*f_ir - h*f_ir.plus.1,
                         (1+h)*f_ir - h*f_ir.minus.1 # alpha < 0
                  )))
  } else { ### no park effects
    f_ir = predict(f_lrm, 
                   tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r), 
                   type="response")[[1]]
    f_ir
  }
}
# f(1,3,0.05, 1,"AL",2019)
# f(1,3,-0.05, 1,"AL",2019)
# f(1,3,0, 1,"AL",2019)
# f(1,3,-0.05, 1,"AL",2019,parkFx=F)
# f(1,3,-0.05, 1,"AL",2019,parkFx=F,adjustConfounders=F)

g <- function(outs_base_state, r) {
  ### g(r|S,O) is the probability of allowing R runs through the rest of the inning if
  ### pitcher exits the inning with O outs and base-state S, where outs_base_state == (S,O)
  r = ifelse(r+1 >= MAX_INNING_RUNS, MAX_INNING_RUNS, r)
  model_g[outs_base_state, r+1]
}
# g(1,0)
# g(24,5)

##################################################
### compute Grid Wins (GW) and Grid WAR (GWAR) ###
##################################################

get_grid_wins_moi <- function(i,r,alpha, home,lg,yr, parkFx=TRUE,adjustConfounders=TRUE, inn_sitch) {
  getgw <- function(w) {
    fw = f(i=i, r=r+w, alpha=alpha, 
           home=home, lg=lg, yr=yr, 
           parkFx=parkFx, adjustConfounders=adjustConfounders) 
    gw = g(inn_sitch, r=w)
    fw*gw
  }
  sum(sapply(0:MAX_INNING_RUNS, FUN = getgw))
}

# f(5,5,0, 1,"AL",2019)
# get_grid_wins_moi(5,5,0, 1,"AL",2019, inn_sitch="2 000")


get_grid_wins <- function(pbp_df, years, parkFx=TRUE, adjustConfounders=TRUE) {
  df = pbp_df %>% filter(YEAR %in% years)
  
  result = df %>%
    rowwise() %>%
    mutate(
      Grid_Wins_eoi = ifelse(
        exit_at_end_of_inning == 0,
        0,
        f(i=INNING, r=CUM_RUNS, alpha=park_factor, 
          home=BAT_HOME_IND, lg=PIT_LEAGUE, yr=YEAR, 
          parkFx=parkFx, adjustConfounders=adjustConfounders) 
      )
    )
  result
  
  result = result %>%
    rowwise() %>%
    mutate(
      Grid_Wins_moi = ifelse(
        exit_in_middle == 0,
        0,
        get_grid_wins_moi(
          i=INNING, r=CUM_RUNS, alpha=park_factor,  ##r=lead(CUM_RUNS, default=0)
          home=BAT_HOME_IND, lg=PIT_LEAGUE, yr=YEAR, 
          parkFx=parkFx, adjustConfounders=adjustConfounders, 
          inn_sitch=INN_SITCH_after
        )
      )
    )
  
  result %>% ungroup()
}

get_pitcher_exits <- function(Grid_Wins_df) {
  pitcher_exits <- Grid_Wins_df %>% 
    filter(exit_in_middle == 1 | exit_at_end_of_inning == 1) %>%
    mutate(
      GW = Grid_Wins_eoi + Grid_Wins_moi, ### Grid Wins in a game
      # GWAR_game = CNWP_game - w_rep
    ) 
  pitcher_exits
}




################################################################################










################################################################################

expected_gwar_eoi <- function(i,j,exit_at_end, alpha,home,lg,yr, parkFx=TRUE,adjustConfounders=TRUE) {
  ### i == inning, j == runs
  result <- numeric(length(exit_at_end))
  for (aaa in 1:length(exit_at_end)) {
    e = exit_at_end[aaa]
    if (e==0) {
      #nothing
    } else {
      result[aaa] <- f(i[aaa], j[aaa], alpha=alpha[aaa],home=home[aaa],lg=lg[aaa],yr=yr[aaa],
                       parkFx=parkFx,adjustConfounders=adjustConfounders)
    }
  }
  return(result)
}

expected_gwar <- function(i,j,k,exit_in_mid, alpha,home,lg,yr, parkFx=TRUE,adjustConfounders=TRUE) {
  ### i == inning, k == runs prior to exiting, j == outs & base state
  result <- numeric(length(exit_in_mid))
  for (aaa in 1:length(exit_in_mid)) {
    if (aaa %% 3 == 0) { print(aaa) }
    e = exit_in_mid[aaa]
    ss <- 0
    if (e == 0) {
      # ignore
    } else { # e == 1
      #browser()
      for(w in 0:MAX_INNING_RUNS) {
        ss <- ss + f(i[aaa], k[aaa] + w, alpha=alpha[aaa],home=home[aaa],lg=lg[aaa],yr=yr[aaa],
                     parkFx=parkFx,adjustConfounders=adjustConfounders) * g(j[aaa], w) 
      }
    }
    result[aaa] = ss
  }
  return(result)
}

get_yearly_gwar_data <- function(year) {
  war2 = war2_og %>% filter(YEAR == year)
  
  war_all <- war2 %>%
    group_by(GAME_ID, PIT_NAME) %>%
    mutate(GWAR_eoi = expected_gwar_eoi(INNING,CUM_RUNS,exit_at_end_of_inning,
                                        alpha=park_factor,home=BAT_HOME_IND,lg=PIT_LEAGUE,yr=YEAR)) %>%
    ungroup() %>%
    group_by(GAME_ID, BAT_HOME_IND) %>%
    mutate(GWAR_moi = expected_gwar(INNING, lead(INN_SITCH, default="0 000"), lead(CUM_RUNS, default=0), exit_in_middle,
                                    alpha=park_factor,home=BAT_HOME_IND,lg=PIT_LEAGUE,yr=YEAR)) %>%
    ungroup()
  
  war_all
}

########################## TESTS ########################## 
# # "LAN201907190"
# expected_gwar(c(6), c("1 110"), c(0), c(1), home=1,lg="NL",yr=2019)
# expected_gwar(c(6), c("1 111"), c(1), c(1), home=1,lg="NL",yr=2019)
# # "CIN201908180"
# expected_gwar(c(6), c("0 100"), c(1), c(1), home=1,lg="NL",yr=2019)
# # expected_gwar_eoi(c(5),c(1),c(1))
# game_id_exs = c("HOU201904250", "OAK201904190", "CIN201908180", "CLE201909142", "LAN201907190")
# t = 3
# View(war_all_2019 %>% filter(GAME_ID == game_id_exs[t]) %>%
#        select(GAME_ID,SP_IND,PIT_ID,BAT_HOME_IND,INNING,INN_SITCH,CUM_RUNS,
#               exit_in_middle,exit_at_end_of_inning,GWAR_moi,GWAR_eoi))

