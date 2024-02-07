
source("0_load_stuff.R")

#################
### Load Data ###
#################

df_war_pitSzn = read_csv("df_FWAR_GWAR_2010_2019_pf_ridge.csv") %>% arrange(PIT_NAME,YEAR)
df_war_pitSzn

pit_exits = read_csv("df_pitcher_exits_2010_2019_pf_ridge.csv")
df_war_pitExits = 
  pit_exits %>%
  select(GAME_ID,YEAR,PIT_NAME,GWAR,INNING) %>%
  arrange(PIT_NAME, YEAR, GAME_ID) %>%
  left_join(df_war_pitSzn %>% select(PIT_NAME,YEAR,FWAR_FIP,FWAR_RA9,N_fg,N)) %>%
  drop_na()
df_war_pitExits

###############################################################################
###  ###
###############################################################################



###############################################################################
### R2 Comment 19a: the variability of GWAR and FWAR from season to season? ###
###############################################################################

df_szn_by_szn_variability =
  df_war_pitSzn %>%
  arrange(PIT_NAME, YEAR) %>%
  mutate(
    GWAR_prev = lag(GWAR),
    FWAR_FIP_prev = lag(FWAR_FIP),
    FWAR_RA9_prev = lag(FWAR_RA9),
  )
df_szn_by_szn_variability

df_szn_by_szn_variability_1 =
  bind_cols(
    df_szn_by_szn_variability %>%
      select(PIT_NAME, YEAR, GWAR, FWAR_FIP, FWAR_RA9) %>%
      pivot_longer(c(GWAR, FWAR_FIP, FWAR_RA9), names_to="metric", values_to="WAR"),
    df_szn_by_szn_variability %>%
      select(PIT_NAME, YEAR, GWAR_prev, FWAR_FIP_prev, FWAR_RA9_prev) %>%
      pivot_longer(c(GWAR_prev, FWAR_FIP_prev, FWAR_RA9_prev), names_to="metric", values_to="WAR_prev") %>%
      select(-c(PIT_NAME, YEAR, metric))
  ) %>%
  mutate(metric = case_when(
    metric == "FWAR_FIP" ~ "FWAR (FIP)",
    metric == "FWAR_RA9" ~ "FWAR (RA9)",
    TRUE ~ metric
  )) 
df_szn_by_szn_variability_1

require(plyr)
lm_eqn = function(df){
  df = df %>% mutate(y = WAR) %>% mutate(x = WAR_prev)
  m = lm(y ~ x, df);
  # browser()
  eq <- substitute(
    italic(slope)~"="~b,
    # italic(slope)~"="~b*","~~italic(r)^2~"="~r2, 
     list(a = format(unname(coef(m)[1]), digits = 2), 
          b = format(unname(coef(m)[2]), digits = 2), 
          r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}
eq <- ddply(df_szn_by_szn_variability_1, .(metric), lm_eqn)

plot_szn_by_szn_variability =
  df_szn_by_szn_variability_1 %>%
  drop_na() %>%
  ggplot(aes(x = WAR_prev, y = WAR)) +
  facet_wrap(~ metric) +
  geom_point(shape=21, size=1) +
  stat_smooth(method="lm",se=F,color="dodgerblue2", linewidth=1.5) +
  geom_text(data=eq, color="dodgerblue2", 
            aes(x = 4, y = 11,label=V1), size=8, parse = TRUE, inherit.aes=FALSE) +
  xlab("previous season's WAR") + ylim(c(-2,12))
# plot_szn_by_szn_variability
ggsave(paste0(output_folder, "plot_szn_by_szn_WAR_variability.png"), 
       plot_szn_by_szn_variability, width=12, height=4)



