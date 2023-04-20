
source("0_load_stuff.R")

#############################
########### DATA ############
#############################

year = 2019
# WAR_df_2019 = read_csv("df_FWAR_GWAR_2019_ridge_PFcomp.csv")
# pitcher_exits = read_csv("df_pitcher_exits_2019_ridge_PF.csv")
WAR_df_2019 = read_csv("df_FWAR_GWAR_2019_pf_ridge.csv") %>% drop_na()
pitcher_exits = read_csv("df_pitcher_exits_2019_pf_ridge.csv")

sum_fw = sum(WAR_df_2019$FWAR_RA9)
sum_gw = sum(WAR_df_2019$GWAR)
WAR_df_2019 = WAR_df_2019 %>% mutate(GWAR_og = GWAR, GWAR = GWAR_og * sum_fw/sum_gw )
pitcher_exits = pitcher_exits %>% mutate(GWAR_og = GWAR, GWAR = GWAR_og * sum_fw/sum_gw )

##################################################
########### GRID WAR VS. FANGRAPHS WAR ###########
##################################################

### Grid War vs. Fangraphs WAR for 2019
pit4a = (WAR_df_2019 %>% arrange(GWAR - FWAR_RA9) %>% slice_head(n=3))$PIT_NAME
pit4b = (WAR_df_2019 %>% arrange(abs(GWAR - FWAR_RA9)) %>% slice_head(n=3))$PIT_NAME
pit4c = (WAR_df_2019 %>% arrange(-GWAR + FWAR_RA9) %>% slice_head(n=5))$PIT_NAME
pit4d = (WAR_df_2019 %>% arrange(GWAR) %>% slice_head(n=2))$PIT_NAME
pit4e = (WAR_df_2019 %>% arrange(GWAR) %>% slice_tail(n=2))$PIT_NAME
pit4f = c("Lance Lynn", "Sonny Gray", "Sandy Alcantara")
pit4f = ""
pits = c(pit4a, pit4b, pit4c, pit4d, pit4e, pit4f)
# pits = setdiff(pits, c("Jeff Samardzija", "Ivan Nova", "Mike Fiers"))
WAR_df_2019$pit_examinef = WAR_df_2019$PIT_NAME %in% pits
############
data.frame(
  WAR_df_2019 %>% mutate(diff = (GWAR - FWAR_RA9)) %>% select(PIT_NAME, FWAR_RA9, GWAR, diff) %>%
    arrange(-FWAR_RA9)
)


pgf = WAR_df_2019 %>% 
  mutate(label = ifelse(pit_examinef, PIT_NAME, "")) %>%
  ggplot(aes(x=FWAR_RA9, y=GWAR, label = label)) + ##label = PIT_NAME
  geom_abline(slope=1, intercept=0) +
  geom_point() +
  geom_text(hjust=-.05, vjust=-0.05, size=2) +
  scale_x_continuous(name="Fangraphs RA/9 WAR", limits = c(0,8)) + 
  scale_y_continuous(name="rescaled Grid WAR", limits = c(1,8.5))  
  # scale_y_continuous(name="Grid WAR", limits = c(1,8.5))  
# pgf
ggsave(paste0(output_folder,"plot_GWAR_vs_FWAR_",year,".png"), pgf, width=6, height=5)


##################################################
######### pitcher vs. pitcher histograms #########
##################################################

pit1_vs_pit2_hists <- function(name1, name2, diff=TRUE) {
  drl = pitcher_exits %>%
    left_join(WAR_df_2019) %>%
    filter(PIT_NAME %in% c(name1, name2))
  # drl %>% group_by(PIT_NAME) %>% summarise(mr = mean(CUM_RUNS), sdr = sd(CUM_RUNS))
  drl1 = left_join(drl, drl %>% group_by(PIT_NAME,CUM_RUNS) %>% summarise(mi = mean(INNING), .groups="drop"))
  # dnf1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>% arrange(PIT_NAME,CUM_RUNS)
  drl2 = drl1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>%
    arrange(PIT_NAME,CUM_RUNS) %>%
    group_by(PIT_NAME,CUM_RUNS) %>%
    summarise(Count = n(), mi = mi, .groups="drop") %>%
    distinct() %>%
    mutate(mi = round(mi,1))
  drl1_name1 = drl2 %>% filter(PIT_NAME == name1) 
  drl1_name2 = drl2 %>% filter(PIT_NAME == name2)
  if (diff) {
    diff_df = merge(drl1_name1, drl1_name2, by="CUM_RUNS", all=T) %>%
      mutate(Count.x = replace_na(Count.x,0),
             Count.y = replace_na(Count.y,0)) %>%
      mutate(Count = Count.x - Count.y) %>%
      select(CUM_RUNS,Count) %>%
      mutate(PIT_NAME = "Difference", mi=NA)
    drl3 = bind_rows(drl2, diff_df)
    drl3$PIT_NAME = factor(drl3$PIT_NAME, levels=c(name1,name2,"Difference"))
    drl3 = drl3 %>% mutate(color =
                             ifelse(PIT_NAME=="Difference" & Count > 0, "darkgreen",
                                    ifelse(PIT_NAME=="Difference" & Count < 0, "firebrick",
                                           "black")))
  } else {
    drl3 = drl2
    drl3$color = "black"
  }
  prl = drl3 %>%
    # ggplot(aes(x=CUM_RUNS,y=Count,label=mi)) +
    ggplot(aes(x=CUM_RUNS,y=Count)) +
    facet_wrap(~PIT_NAME) +
    # geom_histogram() +
    geom_col(fill=drl3$color) +
    # geom_text(vjust = 1.5,color="white") +
    # geom_text(data=dnf2, aes( label = mi), vjust = -0.2)
    scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) +
    scale_y_continuous(name="Count",breaks=seq(-20,20,by=2))
  # labs(title="Distribution of Runs Allowed in a Game")
  prl
  # ggsave(paste0(output_folder,"plot_Lynn_Ryu_",year,".png"), prl)
}

###### FANGRAPHS
dfw19 = WAR_df_2019 %>% arrange(-FWAR_RA9) %>% select(PIT_NAME, FWAR_RA9, GWAR, GWAR_og, N, N_fg)
data.frame(dfw19)
dfw19a = dfw19 %>% 
  ### make sure our dataset matches Fangraphs in the number of games... 
  ### a few retrosheet games are missing...
  filter(N == N_fg) %>% 
  ### keep people who have similar FWAR as at least one other person
  group_by(FWAR_RA9) %>% mutate(count=n()) %>% filter(count >= 2) %>%
  mutate(max_gwar_diff = max(GWAR) - min(GWAR) ) %>%
  ungroup() %>%
  ### 3 groups: elite, decent, bad
  mutate(grp = c(rep(0, round(n()/3)), rep(1, round(n()/3)), rep(2, n()-2*round(n()/3))) ) %>%
  ### keep one comp per group
  group_by(grp) %>%
  filter(max_gwar_diff == max(max_gwar_diff)) %>%
  arrange(-FWAR_RA9, -GWAR)
data.frame(dfw19a)
dfw19b = dfw19a %>%
  group_by(grp) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  ungroup()
data.frame(dfw19b)

### good FWAR_RA9 pitchers
pf1 =
  pit1_vs_pit2_hists(
    first((dfw19b %>% filter(grp==0))$PIT_NAME), 
    last((dfw19b %>% filter(grp==0))$PIT_NAME)
)
# pf1

### decent FWAR_RA9 pitchers
pf2 = 
  pit1_vs_pit2_hists(
    first((dfw19b %>% filter(grp==1))$PIT_NAME), 
    last((dfw19b %>% filter(grp==1))$PIT_NAME)
)
# pf2

### bad FWAR_RA9 pitchers
pf3 =   
  pit1_vs_pit2_hists(
    first((dfw19b %>% filter(grp==2))$PIT_NAME), 
    last((dfw19b %>% filter(grp==2))$PIT_NAME)
)
# pf3

ggsave(paste0(output_folder,"pf1_",year,".png"), pf1, width=8.6, height=3.72)
ggsave(paste0(output_folder,"pf2_",year,".png"), pf2, width=8.6, height=3.72)
ggsave(paste0(output_folder,"pf3_",year,".png"), pf3, width=8.6, height=3.72)

# library(gt)
gt::gtsave(gt::gt(dfw19b), paste0(output_folder,"pf_",year,".png"))

#########################################
######### AGGREGATED HISTOGRAMS #########
#########################################

# agg_pit1_vs_pit2_hists <- function(names_overvalued, names_undervalued, diff=TRUE) {
#   # browser()
#   drl = pitcher_exits %>%
#     left_join(WAR_df_2019) %>%
#     filter(PIT_NAME %in% c(names_overvalued, names_undervalued)) %>%
#     mutate(overvalued = PIT_NAME %in% names_overvalued)
#   drl1 = left_join(drl, drl %>%
#                      group_by(overvalued,CUM_RUNS) %>% #group_by(PIT_NAME,CUM_RUNS) %>%
#                      summarise(mi = mean(INNING),.groups="drop"))
#   drl2 = drl1 %>%
#     select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi,overvalued) %>%
#     # select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi,overvalued) %>%
#     arrange(overvalued,CUM_RUNS) %>%
#     group_by(overvalued,CUM_RUNS) %>%
#     summarise(Count = n(), mi = mi, .groups="drop") %>%
#     distinct() %>%
#     mutate(mi = round(mi,1))
#   drl1_names_overvalued = drl2 %>% filter(overvalued)
#   drl1_names_undervalued = drl2 %>% filter(!overvalued)
#   if (diff) {
#     diff_df = merge(drl1_names_undervalued, drl1_names_overvalued, by="CUM_RUNS", all=T) %>%
#       mutate(Count.x = replace_na(Count.x,0),
#              Count.y = replace_na(Count.y,0)) %>%
#       mutate(Count = Count.x - Count.y) %>%
#       select(CUM_RUNS,Count) %>%
#       mutate(facet = "difference", mi=NA)
#     drl3 = bind_rows(drl2 %>% mutate(facet = ifelse(overvalued, "overvalued", "undervalued")) %>% select(-overvalued),
#                      diff_df)
#     drl3$facet = factor(drl3$facet, levels=c("undervalued","overvalued","difference"))
#     drl3 = drl3 %>% mutate(color =
#                              ifelse(facet=="difference" & Count > 0, "darkgreen",
#                                     ifelse(facet=="difference" & Count < 0, "firebrick",
#                                            "black")))
#   } else {
#     drl3 = drl2
#     drl3$color = "black"
#   }
#   prl = drl3 %>%
#     # ggplot(aes(x=CUM_RUNS,y=Count,label=mi)) +
#     ggplot(aes(x=CUM_RUNS,y=Count)) +
#     facet_wrap(~facet) +
#     # geom_histogram() +
#     geom_col(fill=drl3$color) +
#     # geom_text(vjust = 1.5,color="white") +
#     # geom_text(data=dnf2, aes( label = mi), vjust = -0.2)
#     scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) +
#     scale_y_continuous(name="Count",breaks=seq(-50,50,by=5))
#   # labs(title="Distribution of Runs Allowed in a Game")
#   prl
#   # ggsave(paste0(output_folder,"plot_Lynn_Ryu_",year,".png"), prl)
# }
# 
# ### Fangraphs
# ovf = WAR_df_2019 %>% select(PIT_NAME, GWAR, FWAR_RA9) %>% mutate(diff = GWAR - FWAR_RA9) %>% arrange(diff) %>% slice_head(n=5)
# uvf = WAR_df_2019 %>% select(PIT_NAME, GWAR, FWAR_RA9) %>% mutate(diff = GWAR - FWAR_RA9) %>% arrange(-diff) %>% slice_head(n=5)
# 
# pf_agg = agg_pit1_vs_pit2_hists(ovf$PIT_NAME, uvf$PIT_NAME)
# pf_agg
# 
# ### save
# ggsave(paste0(output_folder,"pf_agg_",year,".png"), pf_agg, width=8.6, height=3.72)
# # ggsave(paste0(output_folder,"pb_agg_",year,".png"), pb_agg, width=8.6, height=3.72)

#########################################
#### best_pitcher_2019's 2019 SEASON ####
#########################################

best_pitcher_2019 = (dfw19 %>% arrange(-GWAR_og))$PIT_NAME[1]
print(best_pitcher_2019)
print(dfw19 %>% arrange(-GWAR_og) %>% 
        filter(row_number() == 1) %>% 
        summarise(same_num_games = N_fg==N)) ### make sure this is true


plot_pit_szn <- function(pitname = best_pitcher_2019, game_idx_0 = 0, game_idx_1 = Inf) {
  
  # cole19a = pitcher_exits %>%
  #   left_join(WAR_df_2019) %>%
  #   filter(PIT_NAME == pitname) %>%
  #   mutate(
  #     OUTS_CT = ifelse(exit_at_end_of_inning==0, str_sub(INN_SITCH_after, end=1), ""),
  #     BASE_STATE = ifelse(exit_at_end_of_inning==0, str_sub(INN_SITCH_after, start=3), ""),
  #   ) %>%
  #   select(DATE,HOME_TEAM_ID,AWAY_TEAM_ID,PARK,CUM_RUNS,INNING,exit_at_end_of_inning,
  #          INN_SITCH_after,
  #          BASE_STATE,OUTS_CT,HOME_LEAGUE,GWAR_og, GWAR, GW) %>%
  #   arrange(-CUM_RUNS,-INNING,BASE_STATE,OUTS_CT,HOME_LEAGUE) %>%
  #   group_by(INNING,CUM_RUNS,BASE_STATE,OUTS_CT,HOME_LEAGUE) %>%
  #   mutate(count=n()) %>%
  #   filter(count >= 2)
  #  view(cole19a)
  # browser()
  
  
  cole190 = pitcher_exits %>%
    left_join(WAR_df_2019) %>%
    filter(PIT_NAME == pitname) %>%
    mutate(
      OUTS_CT = ifelse(exit_at_end_of_inning==0, str_sub(INN_SITCH_after, end=1), ""),
      BASE_STATE = ifelse(exit_at_end_of_inning==0, str_sub(INN_SITCH_after, start=3), ""),
    ) %>%
    select(DATE,HOME_TEAM_ID,AWAY_TEAM_ID,PARK,
           # HOME_LEAGUE,
           CUM_RUNS,INNING,exit_at_end_of_inning,
           BASE_STATE,OUTS_CT, GWAR_og) %>%
    mutate(
      BASE_STATE = as.character(BASE_STATE),
      OUTS_CT = as.character(OUTS_CT),
      BASE_STATE = ifelse(exit_at_end_of_inning, "", BASE_STATE),
      OUTS_CT = ifelse(exit_at_end_of_inning, "", OUTS_CT),
    ) %>%
    arrange(DATE) %>%
    rename(GWAR_og = GWAR_og,
           `HOME TEAM` = HOME_TEAM_ID,
           `AWAY TEAM` = AWAY_TEAM_ID,
           # LEAGUE = HOME_LEAGUE,
           `RUNS ALLOWED` = CUM_RUNS,
           `EXIT INNING` = INNING,
           `EXIT BASE STATE` = BASE_STATE,
           `EXIT OUTS` = OUTS_CT) %>%
    mutate(GWAR_og = round(GWAR_og,3)) %>%
    select(-exit_at_end_of_inning)  %>%
    rename(GWAR = GWAR_og) 
  cole19 = cole190 %>%
    filter(game_idx_0 <= row_number() & row_number() <= game_idx_1)
  data.frame(cole19)
  WAR_df_2019 %>% filter(PIT_NAME == "Gerrit Cole") %>% select(GWAR_og)
  
  
  plot_cole19 = 
    cole19 %>% 
    gt() %>%
    data_color(
      columns = GWAR,
      colors = scales::col_numeric(
        # palette = c("red", "green"),
        # domain = NULL
        # palette = c("#f8696b","#63be7b"), #"#ffeb84",
        palette = c("#f8696b","#ffeb84", "#63be7b"), 
        # palette = c("#b22222","#228B22"), #"#ffeb84",
        domain = c(min(cole190$GWAR), max(cole190$GWAR))
        # domain = c(-0.5, 0.5)
      )
      # colors = scales::col_numeric(
      #   palette = paletteer::paletteer_d(
      #     palette = "ggsci::green_material"
      #   ) %>% as.character(),
      #   domain = NULL
      # )
    ) %>%
    cols_align(align = "center")
  plot_cole19
}
# plot_pit_szn(best_pitcher_2019)

library(gt)
gtsave(plot_pit_szn(best_pitcher_2019), 
       paste0(output_folder,"plot_pitszn_", best_pitcher_2019, "_", year,".png"))
gtsave(plot_pit_szn(best_pitcher_2019, game_idx_0=9, game_idx_1=14), 
       paste0(output_folder,"plot_pitszn_", best_pitcher_2019, "6", "_", year,".png"))


#############################
### 2019 GWAR_og RANKINGS ###
#############################

GWAR_og_2019_rankings = WAR_df_2019 %>%
  drop_na() %>%
  select(PIT_NAME, GWAR_og) %>%
  arrange(-GWAR_og) %>%
  ggplot() +
  geom_point(aes(x = GWAR_og, y = fct_reorder(PIT_NAME, GWAR_og)), 
             size=2, color="black") + #dodgerblue2
  scale_x_continuous(breaks=seq(0,20,by=1), name="Grid WAR") +
  ylab("pitcher")
# GWAR_og_2019_rankings
ggsave(paste0(output_folder,"plot_GWAR_og_rankings_",year,".png"), width=12, height=17)

# data.frame(WAR_df_2019 %>% select(PIT_NAME, GWAR_og) %>% arrange(-GWAR_og))

GWAR_og_2019_rankings6 = WAR_df_2019 %>%
  drop_na() %>%
  select(PIT_NAME, GWAR_og) %>%
  arrange(-GWAR_og) %>%
  # filter(row_number() <= 3 | row_number() > n()-3) %>%
  filter(row_number() <= 6) %>%
  ggplot() +
  geom_point(aes(x = GWAR_og, y = fct_reorder(PIT_NAME, GWAR_og)), 
             size=2, color="black") + #dodgerblue2
  scale_x_continuous(breaks=seq(0,20,by=0.5), name="Grid WAR") +
  ylab("pitcher")
# GWAR_og_2019_rankings6
ggsave(paste0(output_folder,"plot_GWAR_og_rankings6_",year,".png"), width=6, height=3)

# data.frame(WAR_df_2019 %>% select(PIT_NAME, GWAR_og) %>% arrange(-GWAR_og))






