library(tidyverse)
f_lrm <- readRDS("f_lrm.rds") 
g_grid <- read.csv("g_grid.csv",row.names = 1, header= TRUE)
war2_ogg = read_csv("war2.csv")
park_fx_df = read_csv("1a_park_fx/Espn_Pf_2019_3yr.csv") %>% select(PARK, park_factor)
war2_og <- war2_ogg %>% 
  filter(SP_IND | lag(SP_IND, default=FALSE)) %>%
  mutate(PIT_LEAGUE = ifelse(BAT_HOME_IND, AWAY_LEAGUE, HOME_LEAGUE)) %>%
  left_join(park_fx_df) %>%
  mutate(park_factor = replace_na(park_factor, 0))

max_inning_runs=10 
f <- function(i,r,alpha, home,lg,yr) {
  ### i == inning, r == runs, alpha == park effect
  r = ifelse(r+1 >= max_inning_runs, max_inning_runs, r)
  
  ### park adjustment...
  ### espn park factor is different that ours because it's multiplicative!
  ### espn pf: f(I, R*1/(1+alpha))     our pf: f(I, R - I*alpha)
  ### here (espn):  h = R*1/(1+alpha) - R == R(alpha/(1+alpha)) , not I*alpha !!!
  r.minus.1 = ifelse(r > 0, r-1, 0)
  r.plus.1 = ifelse(r < max_inning_runs, r+1, max_inning_runs)
  f_ir.minus.1 = predict(f_lrm, 
                 tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r.minus.1), 
                 type="response")[[1]]
  f_ir = predict(f_lrm, 
                 tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r), 
                 type="response")[[1]]
  f_ir.plus.1 = predict(f_lrm, 
                 tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r.plus.1), 
                 type="response")[[1]]
  h = abs( r*(alpha/(1+alpha)) ) ##################
  
  ifelse(r > 0, 
         (1-h)*f_ir + h*f_ir.minus.1,
         (1+h)*f_ir - h*f_ir.plus.1
         )
}
# f(1,3,0.05, 1,"AL",2019)
# f(1,3,-0.05, 1,"AL",2019)

expected_gwar_eoi <- function(i,j,exit_at_end, alpha,home,lg,yr) {
  ### i == inning, j == runs
  result <- numeric(length(exit_at_end))
  for (aaa in 1:length(exit_at_end)) {
    e = exit_at_end[aaa]
    if (e==0) {
      #nothing
    } else {
      result[aaa] <- f(i[aaa], j[aaa], alpha=alpha[aaa],home=home[aaa],lg=lg[aaa],yr=yr[aaa])
    }
  }
  return(result)
}

g <- function(i,j) {
  j = ifelse(j+1 >= max_inning_runs, max_inning_runs, j)
  g_grid[i,j+1][[1]]
}

expected_gwar <- function(i,j,k,exit_in_mid, alpha,home,lg,yr) {
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
      for(w in 0:max_inning_runs) {
        ss <- ss + f(i[aaa], k[aaa] + w, alpha=alpha[aaa],home=home[aaa],lg=lg[aaa],yr=yr[aaa]) * g(j[aaa], w) 
      }
    }
    result[aaa] = ss
  }
  return(result)
}

#start = identifying pitchers' first at-bat in an inning
#final = identifying pitchers' last at-bat
#pit_occurence = identifying every pitchers' number at-bat
#i = inning, j = base state, k = accumulated runs so far, w = tracker (Rest of inning runs)

get_yearly_gwar_data <- function(year) {
  war2 = war2_og %>% filter(YEAR == year)
  
  war_all <- war2 %>%
    group_by(GAME_ID, PIT_NAME) %>%
    mutate(Pit_occurrence = row_number()) %>%
    mutate(max_row = if_else(Pit_occurrence == max(Pit_occurrence), 1, 0)) %>%
    ###mutate(first_pitch = if_else(Pit_occurrence == 1, 1, 0)) %>%
    ### mutate(enter_at_start_of_inning = if_else(start == 1 & first_pitch == 1, 1, 0)) %>%
    mutate(exit_at_end_of_inning = if_else(final == 1 & max_row == 1, 1, 0)) %>%
    mutate(exit_in_middle = if_else(final == 0 & max_row == 1, 1, 0)) %>%
    mutate(GWAR_eoi = expected_gwar_eoi(INNING,CUM_RUNS,exit_at_end_of_inning,
                                        alpha=park_factor,home=BAT_HOME_IND,lg=PIT_LEAGUE,yr=YEAR)) %>%
    ungroup() %>%
    group_by(GAME_ID, BAT_HOME_IND) %>%
    mutate(GWAR_moi = expected_gwar(INNING, lead(INN_SITCH, default="0 000"), lead(CUM_RUNS, default=0), exit_in_middle,
                                    alpha=park_factor,home=BAT_HOME_IND,lg=PIT_LEAGUE,yr=YEAR)) %>%
    ungroup()
  
  war_all
}

w_rep = 0.41 #FIXME #aribtrary
get_pitcher_exits <- function(war_all_szn) {
  pitcher_exits <- war_all_szn %>% 
    filter(SP_IND & (exit_in_middle == 1 | exit_at_end_of_inning == 1)) %>%
    mutate(CNWP_game = GWAR_moi + GWAR_eoi,
           GWAR_game = CNWP_game - w_rep) 
  pitcher_exits
}
get_seasonal_war <- function(pitcher_exits) {
  Seasonal_GWAR <- pitcher_exits %>%
    group_by(PIT_NAME) %>%
    summarise(GWAR = sum(GWAR_game)) %>%
    ungroup() #%>%select(GAME_ID,PIT_ID,BAT_HOME_IND,INNING,INN_SITCH,CUM_RUNS,GWAR)
  
  Seasonal_GWAR
}

# takes ~15 minutes 
war_all_2019 = get_yearly_gwar_data(2019)
pitcher_exits_2019 = get_pitcher_exits(war_all_2019)
GWAR_2019 = get_seasonal_war(pitcher_exits_2019)

write_csv(pitcher_exits_2019, "pitcher_exits_2019_espnParkFx.csv")
write_csv(GWAR_2019, "GWAR_2019_espnParkFx.csv")

# war_all_2014 = get_yearly_gwar_data(2014)
# pitcher_exits_2014 = get_pitcher_exits(war_all_2014)
# GWAR_2014 = get_seasonal_war(pitcher_exits_2014)

# write_csv(pitcher_exits_2014, "pitcher_exits_2014.csv")
# write_csv(GWAR_2014, "GWAR_2014.csv")


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
# pitcher_exits_2019 = get_pitcher_exits(war_all_2019)
# Check
# View(pitcher_exits_2019 %>% select(GAME_ID,PIT_NAME,PIT_ID,BAT_HOME_IND,INNING,INN_SITCH,
# CUM_RUNS,CNWP_game,GWAR_game,exit_in_middle,exit_at_end_of_inning,GWAR_moi,GWAR_eoi))
# GWAR_2019 = get_seasonal_war(pitcher_exits_2019)
#hist(GWAR_2019$GWAR)


