
source("0_load_stuff.R")

source("4_comp_2019_main.R")

#################
### Load Data ###
#################

df_war_comp = read_csv("df_FWAR_GWAR_2010_2019_pf_ridge.csv")
df_war_comp = df_war_comp %>% 
  ### re-scale GWAR within each year for fair comparison,
  ### because FWAR sums to a constant amount per year
  group_by(YEAR) %>%
  drop_na() %>%
  mutate(
    sum_fw = sum(FWAR_RA9),
    sum_gw = sum(GWAR),
    GWAR_og = GWAR,
    GWAR = GWAR_og * sum_fw/sum_gw
  ) %>%
  ungroup() %>%
  mutate(GWAR_FWAR_diff = GWAR-FWAR_RA9, GWAR_FWAR_diff_mag = abs(GWAR_FWAR_diff)) 
df_war_comp %>% arrange(-GWAR_FWAR_diff_mag)

pit_exits = read_csv("df_pitcher_exits_2010_2019_pf_ridge.csv")
pit_exits = pit_exits %>%
  ### get rescaled GWAR
  left_join( df_war_comp %>% select(YEAR,sum_fw,sum_gw) ) %>%
  mutate(
    GWAR_og = GWAR,
    GWAR = GWAR_og * sum_fw/sum_gw
  )

# ### check that sum is same
# sum(df_war_comp$GWAR)
# sum(df_war_comp$FWAR_RA9)

###############################################################################################################
### EDA: Find pitchers who are undervalued according to GWAR, relative to FWAR_RA9, over their whole career ###
###############################################################################################################

df_war_comp_1 = df_war_comp %>%
  group_by(PIT_NAME) %>%
  summarise(
    sum_gwar_rescaled_minus_fwar = sum(GWAR) - sum(FWAR_RA9),
    N_fg = sum(N_fg), 
    N = sum(N)
  ) %>%
  arrange(-sum_gwar_rescaled_minus_fwar) 
df_war_comp_1
# view(df_war_comp_1)

undervalued_pitchers = df_war_comp_1 %>%
  arrange(-sum_gwar_rescaled_minus_fwar) %>%
  head(n=6) %>%
  select(-c(N,N_fg)) %>%
  ungroup() 
print(undervalued_pitchers)
# gt::gtsave(gt::gt(undervalued_pitchers), paste0(output_folder, "plot_career_undervalued_pitchers.png"))

overvalued_pitchers = df_war_comp_1 %>%
  arrange(sum_gwar_rescaled_minus_fwar) %>%
  head(n=6) %>%
  select(-c(N,N_fg)) %>%
  ungroup() 
print(overvalued_pitchers)
 # gt::gtsave(gt::gt(overvalued_pitchers), paste0(output_folder, "plot_career_overvalued_pitchers.png"))

lm1 = lm(GWAR~FWAR_RA9, data=df_war_comp)
print(lm1)
plot_fwar_v_gwar = 
  df_war_comp %>% 
  ggplot(aes(x=FWAR_RA9, y = GWAR)) + 
  geom_point(shape=21) + 
  ylab("GWAR (rescaled each season)") +
  # ylab("GWAR") +
  xlab("FWAR (RA9)") +
  geom_abline(linewidth=3, aes(slope=1, intercept=0, color="y=x\n", linetype="y=x\n")) +
  geom_abline(aes(intercept = coef(lm1)[1], slope = coef(lm1)[2], 
                  color="regression\nline", linetype="regression\nline"),
              linewidth=3) +
  scale_linetype_manual(
    breaks = c("y=x\n", "regression\nline"),
    values = c("solid", "twodash")
  ) +
  scale_color_manual("color",
                      breaks = c("y=x\n", "regression\nline"),
                      values = c("black", "dodgerblue2")) 
plot_fwar_v_gwar
ggsave("plots/plot_comp_career/plot_career_fwar_v_gwar.png",
       plot_fwar_v_gwar, width=8, height=5)


 library(gt)

#### plots: pitchers who are undervalued over their whole career, according to GWAR
plot_undervalued_pitchers =
  gt(undervalued_pitchers) %>%
  cols_label(
    PIT_NAME = "Pitcher",
    # num_szns_undervalued = "Num Seasons Undervalued",
    sum_gwar_rescaled_minus_fwar = "Career GWAR (Rescaled) Minus FWAR (RA9)"
    # sum_gwar_rescaled_minus_fwar = "Career GWAR Minus FWAR (RA9)"
  ) %>%
  fmt_number( 
    columns = sum_gwar_rescaled_minus_fwar,
    decimals = 2 
  )
# plot_undervalued_pitchers
gtsave(plot_undervalued_pitchers, "plots/plot_comp_career/plot_undervalued_pitchers.png", vwidth = 1500, vheight = 1000)

# pit_v_pit_hists(undervalued_pitchers$PIT_NAME[1:6],
#                 diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)
# ggsave("plots/plot_comp_career/plot_undervalued_pit_hists_6.png", width=10, height=6)

# pit_v_pit_hists(undervalued_pitchers$PIT_NAME[1:3],
#                 diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)
# ggsave("plots/plot_comp_career/plot_undervalued_pit_hists_3.png", width=10, height=3)

plot_pit_yrs <- function(pit_name) {
  pit_yrs = df_war_comp %>%
    filter(PIT_NAME == pit_name) %>%
    select(PIT_NAME, YEAR, GWAR, FWAR_RA9, GWAR_FWAR_diff) %>%
    arrange(YEAR)
  plot_pit_yrs = gt(pit_yrs) %>% 
    cols_label(
      PIT_NAME = "Pitcher",
      YEAR = "Year",
      # GWAR = "GWAR",
      # GWAR = "GWAR (Rescaled)",
      GWAR = "GWAR",
      FWAR_RA9 = "FWAR (RA9)",
      GWAR_FWAR_diff = "Diff"
    ) %>%
    fmt_number( 
      columns = c(GWAR_FWAR_diff,GWAR),
      decimals = 2 
    ) 
  gtsave(plot_pit_yrs, paste0("plots/plot_comp_career/plot_",str_remove(pit_name, " "),"_pitYears.png"))
  plot_pit_yrs
}

plot_pit_yrs_hists <- function(pit_name, undervalued=F, overvalued=F, 
                               width=8, height=5, yrs=NULL) {
  pit_yrs = df_war_comp %>% filter(PIT_NAME == pit_name) 
  if (!is.null(yrs)) {
    pit_yrs = pit_yrs %>% filter(YEAR %in% yrs)
  }
  pit_yrs = pit_yrs %>% tail(n = 6)
  # if (undervalued) {
  #   pit_yrs = pit_yrs %>% filter(GWAR_FWAR_diff >= 1)
  # }
  # if (overvalued) {
  #   pit_yrs = pit_yrs %>% filter(GWAR_FWAR_diff <= -1)
  # }
  pit_v_pit_hists(pit_name, facet_yr=T, diff=F, 
                  pitcher_exits=pit_exits %>% filter(YEAR %in% pit_yrs$YEAR), 
                  war_df=df_war_comp %>% filter(YEAR %in% pit_yrs$YEAR))
  ggsave(paste0("plots/plot_comp_career/plot_", str_remove(pit_name, " "), "_",
                if (undervalued) "undervalued" else "", 
                if (overvalued) "overvalued" else "", 
                "_", length(yrs),
                "_pit_hists.png"), width=width, height=height)
}

plot_pit_yrs(undervalued_pitchers$PIT_NAME[1])
# plot_pit_yrs_hists(undervalued_pitchers$PIT_NAME[1], undervalued=T)
plot_pit_yrs_hists(undervalued_pitchers$PIT_NAME[1], undervalued=T, 
                   yrs=(df_war_comp %>% filter(PIT_NAME==undervalued_pitchers$PIT_NAME[1]) %>% arrange(-GWAR_FWAR_diff) %>% head(n=3))$YEAR,
                   height=3)

#### plots: pitchers who are overvalued over their whole career, according to GWAR
plot_overvalued_pitchers = gt(overvalued_pitchers) %>% 
  cols_label(
    PIT_NAME = "Pitcher",
    # num_szns_overvalued = "Num Seasons Overvalued",
    sum_gwar_rescaled_minus_fwar = "Career GWAR (Rescaled) Minus FWAR (RA9)"
    # sum_gwar_rescaled_minus_fwar = "Career GWAR Minus FWAR (RA9)"
  ) %>%
  fmt_number( 
    columns = sum_gwar_rescaled_minus_fwar,
    decimals = 2 
  )
# plot_overvalued_pitchers
gtsave(plot_overvalued_pitchers, "plots/plot_comp_career/plot_overvalued_pitchers.png", vwidth = 1500, vheight = 1000)


# pit_v_pit_hists(overvalued_pitchers$PIT_NAME[1:6],
#                 diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)
# ggsave("plots/plot_comp_career/plot_overvalued_pit_hists_6.png", width=8, height=6)

# pit_v_pit_hists(overvalued_pitchers$PIT_NAME[1:3],
#                 diff=F, pitcher_exits=pit_exits, war_df=df_war_comp)
# ggsave("plots/plot_comp_career/plot_overvalued_pit_hists_3.png", width=8, height=3)


plot_pit_yrs(overvalued_pitchers$PIT_NAME[1])
# plot_pit_yrs_hists(overvalued_pitchers$PIT_NAME[1], overvalued=T)
plot_pit_yrs_hists(overvalued_pitchers$PIT_NAME[1], 
                   yrs=(df_war_comp %>% filter(PIT_NAME==overvalued_pitchers$PIT_NAME[1]) %>% arrange(GWAR_FWAR_diff) %>% head(n=3))$YEAR,
                   overvalued=T, height=3)



