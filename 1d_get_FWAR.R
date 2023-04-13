
source("0_load_stuff.R")

################################################################################
### download FanGraphs WAR leaderboard for starting pitchers for 2010-2019: ####
### "https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=y&type=8&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate="
################################################################################

df_FWAR = tibble()
for (t in 2010:2019) {
  df_t = read_csv(paste0("FanGraphs_datasets/","FanGraphs Leaderboard-", t, ".csv"))
  df_t$YEAR = t
  df_FWAR = bind_rows(df_FWAR, df_t)
}
# df_FWAR$Name2 = sapply(str_split(df_FWAR$Name, " "), function(x) paste0(str_sub(x,1,1)[1], ".", x[2]))
# df_FWAR = df_FWAR %>% relocate(Name2, .after=Name) %>% relocate(YEAR, .after=Name2) %>% arrange(-YEAR)
df_FWAR = df_FWAR %>% rename(PIT_NAME=Name) %>% relocate(YEAR, .after=PIT_NAME) %>% arrange(-YEAR)
df_FWAR
# df_FWAR1 = df_FWAR %>% select(Name,Name2,YEAR,G,WAR) %>% rename(N=G, FWAR=WAR)
df_FWAR1 = df_FWAR %>% select(PIT_NAME,YEAR,G,WAR) %>% rename(N=G, FWAR=WAR)
df_FWAR1

### save FanGraphs WAR df_FWAR
write_csv(df_FWAR1, "df_FWAR1.csv")


       
       

       