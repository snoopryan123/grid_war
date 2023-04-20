
source("2_get_seasonal_GWAR_main.R")

PARK_FX = "ridge" #FIXME

##################
### Find w_rep ###
##################

### grid wins GW and (grid war GWAR) at the row (play) where the pitcher exits the game
grid_wins_2010_2019_ridge = get_grid_wins(pbp_df=war2_og, 2010:2019, parkFx=PARK_FX) ### takes 15 mins
df_pitcher_exits_2010_2019_ridge = get_pitcher_exits(grid_wins_2010_2019_ridge, war=TRUE)

### get Grid Wins 
grid_wins_2010_2019_pa_1 = get_pitcher_exits(df_pitcher_exits_2010_2019_ridge, war=FALSE)
grid_wins_2010_2019_pa_1s = get_pitcher_exits_shortened(grid_wins_2010_2019_pa_1)
grid_wins_2010_2019_pa_1s

# f(5,5,0, 1,"AL",2019)
# f(4,5,0, 1,"AL",2019)
# f(7,1,0, 1,"AL",2019)*.57 + f(7,2,0, 1,"AL",2019)*.17 + f(7,3,0, 1,"AL",2019)*.13
# f(3,1,0, 1,"AL",2019)

### determine w_rep for 2019
df_FWAR = read_csv("df_FWAR.csv")
df_GW1 = df_FWAR %>% 
  # filter(YEAR == 2019) %>% 
  select(PIT_NAME, YEAR, FWAR_RA9, N) %>%
  left_join(
    grid_wins_2010_2019_pa_1s %>% group_by(PIT_NAME,YEAR) %>% summarise(GW=sum(GW), N1=n())
  )
df_GW1 = df_GW1 %>% select(-N) %>% rename(N = N1) %>% drop_na()
df_GW1
w_rep = (sum(df_GW1$GW) - sum(df_GW1$FWAR_RA9)) / sum(df_GW1$N)
print(w_rep)
# write_csv(data.frame(w_rep), "df_w_rep.csv")

############################
### save 2019 GWAR, FWAR ###
############################

### grid wins GW and (grid war GWAR) at the row (play) where the pitcher exits the game
df_pitcher_exits_2019_ridge = df_pitcher_exits_2010_2019_ridge %>% filter(YEAR == 2019)
get_pitcher_exits_shortened(df_pitcher_exits_2019_ridge) ### view
write_csv(df_pitcher_exits_2019_ridge, paste0("df_pitcher_exits_2019_pf_ridge.csv"))

### get seasonal GWAR
df_gwar_2019_ridge = get_seasonal_war(df_pitcher_exits_2019_ridge)
df_gwar_2019_ridge
write_csv(df_gwar_2019_ridge, paste0("df_GWAR_2019_pf_ridge.csv"))

### dataframe for comparing GWAR and FWAR in 2019
df_FWAR_GWAR_comp = df_FWAR %>% filter(YEAR == 2019) %>% 
  # select(-N) %>% 
  rename(N_fg = N) %>%
  left_join(df_gwar_2019_ridge) 
df_FWAR_GWAR_comp
write_csv(df_FWAR_GWAR_comp, paste0("df_FWAR_GWAR_2019_pf_ridge.csv"))

###################################################
### compute 2019 GWAR with Various Park Effects ###
###################################################

### Grid Wins and GWAR for each game, for various park effects
park_fx_names = list(FALSE, "fg", "espn", "ridge")   ### list(FALSE, "fg", "espn", "ridge")   ### list(FALSE, "fg", "espn")

compute_GWAR_fullSuite <- function(years, parkFx=FALSE, war=TRUE) {
  # browser()
  
  ### compute Grid Wins
  yr_str = if (length(years)==1) years else paste0(min(years),"_",max(years))
  print(paste0("getting Grid Wins for years ", yr_str, " and park FX ", parkFx))
  df_grid_wins = get_grid_wins(pbp_df=war2_og, years, parkFx=parkFx) ### takes a few mins...
  
  ### grid wins GW and (grid war GWAR) at the row (play) where the pitcher exits the game
  df_pitcher_exits = get_pitcher_exits(df_grid_wins, war=war)
  get_pitcher_exits_shortened(df_pitcher_exits) ### view
  write_csv(df_pitcher_exits, paste0("df_pitcher_exits_", yr_str, "_pf_", parkFx, ".csv"))
  
  ### get seasonal GWAR
  df_gwar = get_seasonal_war(df_pitcher_exits)
  df_gwar
  write_csv(df_gwar, paste0("df_GWAR_", yr_str, "_pf_", parkFx, ".csv"))
}

for (park_fx_name in park_fx_names) {
  compute_GWAR_fullSuite(2019, parkFx=park_fx_name, war=TRUE)
}

######################################################
### compute 2010-2019 GWAR with ridge park effects ###
######################################################

compute_GWAR_fullSuite(2010:2019, parkFx=PARK_FX, war=TRUE) ### takes 10 mins

### dataframe for comparing GWAR, FWAR, AWAR 
df_FWAR = read_csv("df_FWAR.csv")
df_GWAR = read_csv("df_GWAR_2010_2019_pf_ridge.csv")
df_FWAR_GWAR_comp = 
  df_FWAR %>% 
  # select(-N) %>% 
  rename(N_fg = N) %>%
  left_join(df_GWAR %>% select(-GW,-w_rep)) 
df_FWAR_GWAR_comp
write_csv(df_FWAR_GWAR_comp, paste0("df_FWAR_GWAR_2010_2019_pf_ridge.csv"))


