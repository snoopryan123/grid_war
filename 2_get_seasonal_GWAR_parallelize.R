
source("2_get_seasonal_GWAR_main.R")

##################################
### compute GWAR ###
##################################


################################################################################

# ### check
# grid_wins_2019_pa = get_grid_wins(pbp_df = war2_og %>% filter(exit_at_end_of_inning==1 | exit_in_middle==1) %>% 
#                                            filter(YEAR==2019) %>% arrange(PIT_NAME) %>% filter(row_number() <= 10),
#                                   2019, parkFx=TRUE, adjustConfounders=TRUE) 

grid_wins_2019_pa = get_grid_wins(pbp_df=war2_og, 2019, parkFx=TRUE, adjustConfounders=TRUE) ### takes 2 mins

grid_wins_2019_pa_1 = get_pitcher_exits(grid_wins_2019_pa)

grid_wins_2019_pa_1 %>% select(PIT_NAME, YEAR, GAME_ID, GW, INNING, CUM_RUNS,
                               exit_at_end_of_inning, exit_in_middle, BASE_STATE, OUTS_CT,
                               BAT_HOME_IND,PIT_LEAGUE,park_factor) %>% arrange(PIT_NAME,GAME_ID)


# f(5,5,0, 1,"AL",2019)
# f(4,5,0, 1,"AL",2019)
# f(7,1,0, 1,"AL",2019)*.57 + f(7,2,0, 1,"AL",2019)*.17 + f(7,3,0, 1,"AL",2019)*.13
# f(3,1,0, 1,"AL",2019)





# GWAR_game = CNWP_game - w_rep










##################################
### compute GWAR ###
##################################



w_rep = 0.41 #FIXME #aribtrary
# get_pitcher_exits <- function(war_all_szn) {
#   pitcher_exits <- war_all_szn %>% 
#     filter(SP_IND & (exit_in_middle == 1 | exit_at_end_of_inning == 1)) %>%
#     mutate(CNWP_game = GWAR_moi + GWAR_eoi,
#            GWAR_game = CNWP_game - w_rep) 
#   pitcher_exits
# }
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

write_csv(pitcher_exits_2019, paste0("pitcher_exits_2019_", park_fx_name, ".csv"))
write_csv(GWAR_2019, paste0("GWAR_2019_", park_fx_name, ".csv"))




# war_all_2014 = get_yearly_gwar_data(2014)
# pitcher_exits_2014 = get_pitcher_exits(war_all_2014)
# GWAR_2014 = get_seasonal_war(pitcher_exits_2014)

# write_csv(pitcher_exits_2014, paste0("pitcher_exits_2014_", park_fx_name, ".csv"))
# write_csv(GWAR_2014, paste0("GWAR_2014_", park_fx_name, ".csv"))

########################## TESTS ########################## 
# pitcher_exits_2019 = get_pitcher_exits(war_all_2019)
# Check
# View(pitcher_exits_2019 %>% select(GAME_ID,PIT_NAME,PIT_ID,BAT_HOME_IND,INNING,INN_SITCH,
# CUM_RUNS,CNWP_game,GWAR_game,exit_in_middle,exit_at_end_of_inning,GWAR_moi,GWAR_eoi))
# GWAR_2019 = get_seasonal_war(pitcher_exits_2019)
#hist(GWAR_2019$GWAR)


