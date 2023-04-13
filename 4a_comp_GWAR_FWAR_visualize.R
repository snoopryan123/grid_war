
source("0_load_stuff.R")

source("3_comp_2019_main.R")

#################
### Load Data ###
#################

# pit_exits = read_csv("df_pitcher_exits_2010_2019_pf_TRUE_ac_TRUE.csv")
# df_war_comp = read_csv("df_FWAR_GWAR_AWAR_2010_2019_pf_TRUE_ac_TRUE_comp.csv")
pit_exits = read_csv("df_pitcher_exits_2010_2019_pf_TRUE_ac_FALSE.csv")
df_war_comp = read_csv("df_FWAR_GWAR_AWAR_2010_2019_pf_TRUE_ac_FALSE_comp.csv")
df_war_comp = df_war_comp %>% mutate(GWAR_FWAR_diff = GWAR-FWAR, GWAR_FWAR_diff_mag = abs(GWAR_FWAR_diff))
df_war_comp %>% arrange(-GWAR_FWAR_diff_mag)

###########################################################################################################
### EDA: Find pitchers who are undervalued according to GWAR, relative to FWAR, over their whole career ###
###########################################################################################################

plot_fwar_v_gwar = df_war_comp %>% ggplot(aes(x=FWAR, y = GWAR)) + geom_point() + geom_abline() +
  geom_text(aes(label=paste0(PIT_NAME, ":", YEAR)), size=2) 
ggsave("plot_fwar_v_gwar.png", plot_fwar_v_gwar, width=20, height=20)

df_war_comp_undervalued = df_war_comp %>%
  mutate(
    undervalued = GWAR - FWAR >= 1,
    overvalued =  FWAR - GWAR >= 1
  ) %>%
  group_by(PIT_NAME) %>%
  mutate(
    num_szns_undervalued = sum(undervalued),
    num_szns_overvalued = sum(overvalued)
  ) %>%
  ungroup() %>%
  arrange(-num_szns_undervalued,PIT_NAME,YEAR)

undervalued_pitchers = df_war_comp_undervalued %>%
  filter(num_szns_undervalued >= 4) %>%
  group_by(PIT_NAME, num_szns_undervalued) %>%
  summarise(sum_gwar_minus_fwar = sum(GWAR) - sum(FWAR)) %>%
  arrange(-num_szns_undervalued, -sum_gwar_minus_fwar) %>%
  ungroup() 
print(undervalued_pitchers)

overvalued_pitchers = df_war_comp_undervalued %>%
  filter(num_szns_overvalued >= 3) %>%
  group_by(PIT_NAME, num_szns_overvalued) %>%
  summarise(sum_gwar_minus_fwar = sum(GWAR) - sum(FWAR)) %>%
  arrange(-num_szns_overvalued, sum_gwar_minus_fwar) %>%
  # filter(sum_gwar_minus_fwar <= -3.85) %>%
  ungroup() 
print(overvalued_pitchers)

#### plots: pitchers who are undervalued over their whole career, according to GWAR
plot_undervalued_pitchers = gt(undervalued_pitchers) %>% 
  cols_label(
    PIT_NAME = "Pitcher",
    num_szns_undervalued = "Num Seasons Undervalued",
    sum_gwar_minus_fwar = "Career GWAR Minus FWAR"
  ) %>%
  fmt_number( 
    columns = sum_gwar_minus_fwar,
    decimals = 2 
  )
plot_undervalued_pitchers
gtsave(plot_undervalued_pitchers, "plots/plot_comp/plot_undervalued_pitchers.png")

# pit_v_pit_hists(c("Julio Teheran", "Gio Gonzalez", "James Shields", "Kyle Lohse"),
#                 diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)
pit_v_pit_hists(undervalued_pitchers$PIT_NAME[1:6],
                diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)
ggsave("plots/plot_comp/plot_undervalued_pit_hists.png", width=10, height=6)
# pit_v_pit_hists(c("Julio Teheran", "Gio Gonzalez"),
#                 diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)

plot_pit_yrs <- function(pit_name) {
  pit_yrs = df_war_comp %>%
    filter(PIT_NAME == pit_name) %>%
    select(PIT_NAME, YEAR, GWAR, FWAR, GWAR_FWAR_diff)
  plot_pit_yrs = gt(pit_yrs) %>% 
    cols_label(
      PIT_NAME = "Pitcher",
      YEAR = "Year",
      GWAR = "GWAR",
      FWAR = "FWAR",
      GWAR_FWAR_diff = "Diff"
    ) %>%
    fmt_number( 
      columns = c(GWAR_FWAR_diff,GWAR),
      decimals = 2 
    )
  gtsave(plot_pit_yrs, paste0("plots/plot_comp/plot_",pit_name,"_pitYears.png"))
  plot_pit_yrs
}

plot_pit_yrs_hists <- function(pit_name, undervalued=F, overvalued=F, width=10, height=6) {
  pit_yrs = df_war_comp %>% filter(PIT_NAME == pit_name) 
  if (undervalued) {
    pit_yrs = pit_yrs %>% filter(GWAR_FWAR_diff >= 1)
  }
  if (overvalued) {
    pit_yrs = pit_yrs %>% filter(GWAR_FWAR_diff <= -1)
  }
  pit_v_pit_hists(pit_name, facet_yr=T, diff=F, 
                  pitcher_exits=pit_exits %>% filter(YEAR %in% pit_yrs$YEAR), 
                  war_df=df_war_comp %>% filter(YEAR %in% pit_yrs$YEAR))
  ggsave(paste0("plots/plot_comp/plot_", pit_name, "_",
                if (undervalued) "undervalued" else "", 
                if (overvalued) "overvalued" else "", 
                "_pit_hists.png"), width=width, height=height)
}

plot_pit_yrs("Julio Teheran")
plot_pit_yrs_hists("Julio Teheran", undervalued=T)

plot_pit_yrs("Tim Hudson")
plot_pit_yrs_hists("Tim Hudson", undervalued=T)


#### plots: pitchers who are overvalued over their whole career, according to GWAR
plot_overvalued_pitchers = gt(overvalued_pitchers) %>% 
  cols_label(
    PIT_NAME = "Pitcher",
    num_szns_overvalued = "Num Seasons Overvalued",
    sum_gwar_minus_fwar = "Career GWAR Minus FWAR"
  ) %>%
  fmt_number( 
    columns = sum_gwar_minus_fwar,
    decimals = 2 
  )
plot_overvalued_pitchers
gtsave(plot_overvalued_pitchers, "plots/plot_comp/plot_overvalued_pitchers.png")


pit_v_pit_hists(overvalued_pitchers$PIT_NAME[1:3],
                diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)
ggsave("plots/plot_comp/plot_overvalued_pit_hists.png", width=8, height=3)


plot_pit_yrs("Rick Porcello")
plot_pit_yrs_hists("Rick Porcello", overvalued=T, width=8, height=5)

plot_pit_yrs("Jose Quintana")
plot_pit_yrs_hists("Jose Quintana", overvalued=T, width=8, height=3)

###################################################################################
### Does GWAR add predictive value for some pitchers? #############################
### Is there something sticky about runs gained distribution for some pitchers? ###
###################################################################################

df_war_comp




















# df_agg = df_war_comp %>%
#   arrange(PIT_NAME,YEAR) %>%
#   group_by(PIT_NAME) %>%
#   summarise(
#     GWAR_per_szn = mean(GWAR),
#     FWAR_per_szn = mean(FWAR),
#     GWAR = sum(GWAR),
#     FWAR = sum(FWAR),
#     yr0 = YEAR[1],
#     yr1 = YEAR[n()],
#     num_yrs = n()
#   ) %>% 
#   mutate(
#     GWAR_FWAR_diff = GWAR-FWAR, 
#     GWAR_FWAR_diff_mag = abs(GWAR_FWAR_diff),
#     GWAR_FWAR_mean_diff = GWAR_per_szn-FWAR_per_szn, 
#     GWAR_FWAR_mean_diff_mag = abs(GWAR_FWAR_mean_diff),
#   ) %>% 
#   arrange(-GWAR_FWAR_diff_mag)
# df_agg
# 
# View(df_agg)


