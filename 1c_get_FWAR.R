
source("0_load_stuff.R")

################################################################################
### download FanGraphs WAR leaderboard for starting pitchers for 2010-2019: ####
################################################################################

df_fg = baseballr::fg_pitcher_leaders(
  2010,
  2019,
  league = "all",
  qual = "y",
  pitcher_type = "sta",
  ind = 1
)
df_fg_1 = df_fg %>%
  select(playerid, `#`, Season, Name, Team, G, WAR, `RA9-WAR`) %>%
  rename(PIT_NAME = Name) %>%
  rename(YEAR = Season) %>%
  relocate(YEAR, .after=PIT_NAME) %>%
  arrange(-`RA9-WAR`) %>%
  rename(FWAR_FIP = WAR) %>%
  rename(FWAR_RA9 = `RA9-WAR`) %>%
  rename(N = G)
df_fg_1

### save FanGraphs WAR df_FWAR
write_csv(df_fg_1, "df_FWAR.csv")

       
       

       