
source("2_get_seasonal_GWAR_main.R")

##################
### Find w_rep ###
##################

### check
grid_wins_2019_pa = get_grid_wins(pbp_df = war2_og %>% filter(exit_at_end_of_inning==1 | exit_in_middle==1) %>%
                                           filter(YEAR==2019) %>% arrange(PIT_NAME) %>% filter(row_number() <= 10),
                                  2019, parkFx=TRUE, adjustConfounders=TRUE)

grid_wins_2019_ridge = get_grid_wins(pbp_df=war2_og, 2019, parkFx="ridge", adjustConfounders=TRUE) ### takes 2 mins

### get Grid Wins 
grid_wins_2019_pa_1 = get_pitcher_exits(grid_wins_2019_pa, war=FALSE)
grid_wins_2019_pa_1s = get_pitcher_exits_shortened(grid_wins_2019_pa_1)
grid_wins_2019_pa_1s

# f(5,5,0, 1,"AL",2019)
# f(4,5,0, 1,"AL",2019)
# f(7,1,0, 1,"AL",2019)*.57 + f(7,2,0, 1,"AL",2019)*.17 + f(7,3,0, 1,"AL",2019)*.13
# f(3,1,0, 1,"AL",2019)

### determine w_rep for 2019
df_FWAR1 = read_csv("df_FWAR1.csv")
df_GW1 = df_FWAR1 %>% filter(YEAR == 2019) %>% left_join(
  grid_wins_2019_pa_1s %>% group_by(PIT_NAME,YEAR) %>% summarise(GW=sum(GW), N1=n())
)
df_GW1 = df_GW1 %>% select(-N) %>% rename(N = N1) %>% drop_na()
df_GW1
w_rep = (sum(df_GW1$GW) - sum(df_GW1$FWAR)) / sum(df_GW1$N)
w_rep
# write_csv(data.frame(w_rep), "df_w_rep.csv")

w_rep = 0.423769

############################################################################
### compute GWAR and AWAR 2010-2019 without park effects and confounders ###
############################################################################

### grid wins GW and (grid war GWAR) at the row (play) where the pitcher exits the game
df_pitcher_exits_2019_ridge = get_pitcher_exits(grid_wins_2019_ridge, war=TRUE)

get_pitcher_exits_shortened(df_pitcher_exits_2019_ridge) ### view
write_csv(df_pitcher_exits_2019_ridge, paste0("df_pitcher_exits_2019_pf_ridge_ac_TRUE.csv"))

### get seasonal GWAR
df_gwar_2019_ridge = get_seasonal_war(df_pitcher_exits_2019_ridge)
df_gwar_2019_ridge
write_csv(df_gwar_2019_ridge, paste0("df_GWAR_2019_pf_ridge_ac_TRUE.csv"))

### dataframe for comparing GWAR and FWAR in 2019
df_FWAR_GWAR_comp = df_FWAR1 %>% filter(YEAR == 2019) %>% select(-N) %>% left_join(grid_wins_2019_ridge) 
df_FWAR_GWAR_comp
write_csv(df_FWAR_GWAR_comp, paste0("df_FWAR_GWAR_2019_pf_ridge_ac_TRUE.csv"))

#################################################################################
### compute 2019 GWAR with Various Park Effects and Adjusting for Confounders ###
#################################################################################

### Grid Wins and GWAR for each game, for various park effects
park_fx_names = list(FALSE, "fg", "espn", "ridge")   ### list(FALSE, "fg", "espn", "ridge")   ### list(FALSE, "fg", "espn")

compute_GWAR_fullSuite <- function(years, parkFx=FALSE, adjustConfounders=TRUE, war=TRUE) {
  # browser()
  
  ### compute Grid Wins
  yr_str = if (length(years)==1) years else paste0(min(years),"_",max(years))
  print(paste0("getting Grid Wins for years ", yr_str, " and park FX ", parkFx, " and adjustForConfounders ", adjustConfounders))
  df_grid_wins = get_grid_wins(pbp_df=war2_og, years, parkFx=parkFx, adjustConfounders=adjustConfounders) ### takes a few mins...
  
  ### grid wins GW and (grid war GWAR) at the row (play) where the pitcher exits the game
  df_pitcher_exits = get_pitcher_exits(df_grid_wins, war=war)
  get_pitcher_exits_shortened(df_pitcher_exits) ### view
  write_csv(df_pitcher_exits, paste0("df_pitcher_exits_", yr_str, "_pf_", parkFx, "_ac_", adjustConfounders, ".csv"))
  
  ### get seasonal GWAR
  df_gwar = get_seasonal_war(df_pitcher_exits)
  df_gwar
  write_csv(df_gwar, paste0("df_GWAR_", yr_str, "_pf_", parkFx, "_ac_", adjustConfounders, ".csv"))
}

for (park_fx_name in park_fx_names) {
  compute_GWAR_fullSuite(2019, parkFx=park_fx_name, adjustConfounders=TRUE, war=TRUE)
}

################################################################################
### compute 2010-2019 GWAR without park effects or adjusting for confounders ###
################################################################################

compute_GWAR_fullSuite(2010:2019, parkFx=FALSE, adjustConfounders=FALSE, war=TRUE)

### integrate AWAR
df_AWAR = read_csv("df_AWAR.csv")
df_AWAR
df_AWAR %>% filter(PIT_NAME == "Justin Verlander")
### dataframe for comparing GWAR, FWAR, AWAR 
df_FWAR1 = read_csv("df_FWAR1.csv")
df_GWAR1 = read_csv("df_GWAR_2010_2019_noParkFx.csv")
df_FWAR_GWAR_AWAR_comp = 
  df_FWAR1 %>% select(-N) %>% 
  left_join(df_GWAR1 %>% select(-GW,-w_rep)) %>% 
  left_join(df_AWAR %>% select(-R_,-I,-RABR)) %>%
  relocate(AWAR, .after=GWAR)
df_FWAR_GWAR_AWAR_comp
write_csv(df_FWAR_GWAR_AWAR_comp, paste0("df_FWAR_GWAR_AWAR_2010_2019_noParkFx_comp.csv"))

### visualize Jensen's inequality
plot_jensen = df_FWAR_GWAR_AWAR_comp %>%
  filter(YEAR==2019) %>%
  ggplot() +
  geom_point(aes(x=AWAR, y=GWAR)) +
  geom_abline(intercept = 0, slope = 1)
plot_jensen
ggsave("plots/plot_jensen.png", plot_jensen, width=6, height=6)

################################################################################
### compute 2010-2019 GWAR with ridge park effects and adjusting for confounders ###
################################################################################

compute_GWAR_fullSuite(2010:2019, parkFx=TRUE, adjustConfounders=TRUE, war=TRUE)

### dataframe for comparing GWAR, FWAR, AWAR 
df_FWAR1 = read_csv("df_FWAR1.csv")
df_GWAR1 = read_csv("df_GWAR_2010_2019_pf_TRUE_ac_TRUE.csv")
df_FWAR_GWAR_AWAR_comp = 
  df_FWAR1 %>% select(-N) %>% 
  left_join(df_GWAR1 %>% select(-GW,-w_rep)) 
df_FWAR_GWAR_AWAR_comp
write_csv(df_FWAR_GWAR_AWAR_comp, paste0("df_FWAR_GWAR_AWAR_2010_2019_pf_TRUE_ac_TRUE_comp.csv"))


################################################################################
### compute 2010-2019 GWAR with ridge park effects and without adjusting for confounders ###
################################################################################

compute_GWAR_fullSuite(2010:2019, parkFx=TRUE, adjustConfounders=FALSE, war=TRUE)

### dataframe for comparing GWAR, FWAR, AWAR 
df_FWAR1 = read_csv("df_FWAR1.csv")
df_GWAR1 = read_csv("df_GWAR_2010_2019_pf_TRUE_ac_FALSE.csv")
df_FWAR_GWAR_AWAR_comp = 
  df_FWAR1 %>% select(-N) %>% 
  left_join(df_GWAR1 %>% select(-GW,-w_rep)) 
df_FWAR_GWAR_AWAR_comp
write_csv(df_FWAR_GWAR_AWAR_comp, paste0("df_FWAR_GWAR_AWAR_2010_2019_pf_TRUE_ac_FALSE_comp.csv"))


