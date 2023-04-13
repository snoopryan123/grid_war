
source("0_load_stuff.R")

#############################
########### DATA ############
#############################

year = 2019
WAR_df_2019 = read_csv("df_FWAR_GWAR_2019_ridge_PFcomp.csv")
pitcher_exits = read_csv("df_pitcher_exits_2019_ridge_PF.csv")

##################################################
########### GRID WAR VS. FANGRAPHS WAR ###########
##################################################

### Grid War vs. Fangraphs WAR for 2019
pit4a = (WAR_df_2019 %>% arrange(GWAR - FWAR) %>% slice_head(n=5))$PIT_NAME
pit4b = (WAR_df_2019 %>% arrange(abs(GWAR - FWAR)) %>% slice_head(n=5))$PIT_NAME
pit4c = (WAR_df_2019 %>% arrange(-GWAR + FWAR) %>% slice_head(n=5))$PIT_NAME
pit4d = (WAR_df_2019 %>% arrange(GWAR) %>% slice_head(n=2))$PIT_NAME
pit4e = (WAR_df_2019 %>% arrange(GWAR) %>% slice_tail(n=2))$PIT_NAME
pit4f = c("Lance Lynn", "Sonny Gray", "Sandy Alcantara")
pits = c(pit4a, pit4b, pit4c, pit4d, pit4e, pit4f)
# pits = setdiff(pits, c("Jeff Samardzija", "Ivan Nova", "Mike Fiers"))
WAR_df_2019$pit_examinef = WAR_df_2019$PIT_NAME %in% pits
############
data.frame(
  WAR_df_2019 %>% mutate(diff = (GWAR - FWAR)) %>% select(PIT_NAME, FWAR, GWAR, diff) %>%
    arrange(-FWAR)
)

{
  pgf = WAR_df_2019 %>% 
    mutate(label = ifelse(pit_examinef, PIT_NAME, "")) %>%
    ggplot(aes(x=FWAR, y=GWAR, label = label)) + ##label = PIT_NAME
    geom_abline(slope=1, intercept=0) +
    geom_point() +
    geom_text(hjust=-.05, vjust=-0.05, size=3) +
    scale_x_continuous(name="Fangraphs WAR", limits = c(0,8)) + 
    scale_y_continuous(name="Grid WAR", limits = c(1,8.5))  
  pgf
  ggsave(paste0(output_folder,"plot_GWAR_vs_FWAR_",year,".png"), pgf, width=8, height=8)
}

# ###########################################################
# ########### GRID WAR VS. BASEBALL REFERENCE WAR ###########
# ###########################################################
# 
# ### Grid War vs. Baseball Reference WAR for 2019
# pit5a = (WAR_df_2019 %>% arrange(GWAR - BWAR) %>% slice_head(n=5))$PIT_NAME
# pit5b = (WAR_df_2019 %>% arrange(abs(GWAR - BWAR)) %>% slice_head(n=5))$PIT_NAME
# pit5c = (WAR_df_2019 %>% arrange(-GWAR + BWAR) %>% slice_head(n=5))$PIT_NAME
# pit5d = (WAR_df_2019 %>% arrange(GWAR) %>% slice_head(n=2))$PIT_NAME
# pit5e = (WAR_df_2019 %>% arrange(GWAR) %>% slice_tail(n=2))$PIT_NAME
# pit5f = c("Zack Greinke", "Lucas Giolito","Julio Teheran", 
#           "Anibal Sanchez","Wade Miley", "Robbie Ray")
# pits = c(pit5a, pit5b, pit5c, pit5d, pit5e, pit5f)
# pits = setdiff(pits,
#                c("Eduardo Rodriguez", "Mike Leake"))
# WAR_df_2019$pit_examine1 = WAR_df_2019$PIT_NAME %in% pits
# ############
# data.frame(
#   WAR_df_2019 %>% mutate(diff = (GWAR - BWAR)) %>% select(PIT_NAME, BWAR, GWAR, diff) %>%
#     arrange(-BWAR)
# )
# 
# {
#   pgb = WAR_df_2019 %>% 
#     mutate(label = ifelse(pit_examine1, PIT_NAME, "")) %>%
#     ggplot(aes(x=BWAR, y=GWAR, label = label)) + ##label = PIT_NAME
#     geom_abline(slope=1, intercept=0) +
#     geom_point() +
#     geom_text(hjust=-.05, vjust=-0.05, size=3) +
#     scale_x_continuous(name="Baseball Reference WAR", limits = c(0,8)) + 
#     scale_y_continuous(name="Grid WAR", limits = c(1,8.5))  
#   pgb
#   ggsave(paste0(output_folder,"plot_GWAR_vs_BWAR_",year,".png"), pgb, width=8, height=8)
# }
# 
# ### example histograms of GWAR vs. BWAR
# {
#   get_pit_histogram_exs <- function(pit_names) {
#     pitcher_exits %>%
#       filter(PIT_NAME %in% pit_names) %>%
#       select(PIT_NAME, CUM_RUNS) %>%
#       group_by(PIT_NAME, CUM_RUNS) %>%
#       summarise(count = n(), .groups="drop") %>%
#       ggplot() +
#       facet_wrap(~PIT_NAME, nrow=1) +
#       geom_col(aes(x = CUM_RUNS, y = count), fill="black", color="black") +
#       scale_x_continuous(breaks=(0:50)*2 ) +
#       scale_y_continuous(breaks=(0:50)*2 ) +
#       labs(title="",x="",y="")
#   }
#   
#   p5a = get_pit_histogram_exs(pit5a[c(1,2,5)])
#   p5a
#   p5b = get_pit_histogram_exs(pit5b[c(1,2,3)])
#   p5b
#   p5c = get_pit_histogram_exs(pit5c[c(1,2,5)])
#   p5c
#   
#   # ggsave(paste0(output_folder,"plot_3hists_GWvsBW_5a.png"), p5a, width=10, height=4)
#   # ggsave(paste0(output_folder,"plot_3hists_GWvsBW_5b.png"), p5b, width=10, height=4)
#   # ggsave(paste0(output_folder,"plot_3hists_GWvsBW_5c.png"), p5c, width=10, height=4)
# }

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
data.frame(WAR_df_2019 %>% arrange(-FWAR) %>% select(PIT_NAME, FWAR, GWAR))

### good FWAR pitchers
pf1 = pit1_vs_pit2_hists("Jacob deGrom", "Lance Lynn")
pf1

### decent FWAR pitchers
pf2 = pit1_vs_pit2_hists("Sonny Gray","Jose Berrios")
pf2

### bad FWAR pitchers
pf3 = pit1_vs_pit2_hists("Sandy Alcantara","Reynaldo Lopez")
pf3

# # ###### BASEBALL REFERENCE
# # data.frame(WAR_df_2019 %>% arrange(-BWAR) %>% select(PIT_NAME, BWAR, GWAR))
# # 
# # ### good BWAR pitchers
# # pb1 = pit1_vs_pit2_hists("Zack Greinke", "Lucas Giolito")
# # pb1
# # 
# # ### decent BWAR pitchers
# # pb2 = pit1_vs_pit2_hists("Julio Teheran", "Anibal Sanchez")
# # pb2
# # 
# # ### bad BWAR pitchers
# # pb3 = pit1_vs_pit2_hists("Wade Miley", "Robbie Ray")
# # pb3

ggsave(paste0(output_folder,"pf1_",year,".png"), pf1, width=8.6, height=3.72)
ggsave(paste0(output_folder,"pf2_",year,".png"), pf2, width=8.6, height=3.72)
ggsave(paste0(output_folder,"pf3_",year,".png"), pf3, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"pb1_",year,".png"), pb1, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"pb2_",year,".png"), pb2, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"pb3_",year,".png"), pb3, width=8.6, height=3.72)

#########################################
######### AGGREGATED HISTOGRAMS #########
#########################################

agg_pit1_vs_pit2_hists <- function(names_overvalued, names_undervalued, diff=TRUE) {
  # browser()
  drl = pitcher_exits %>%
    left_join(WAR_df_2019) %>%
    filter(PIT_NAME %in% c(names_overvalued, names_undervalued)) %>%
    mutate(overvalued = PIT_NAME %in% names_overvalued) 
  drl1 = left_join(drl, drl %>% 
                     group_by(overvalued,CUM_RUNS) %>% #group_by(PIT_NAME,CUM_RUNS) %>% 
                     summarise(mi = mean(INNING),.groups="drop"))
  drl2 = drl1 %>% 
    select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi,overvalued) %>%
    # select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi,overvalued) %>%
    arrange(overvalued,CUM_RUNS) %>%
    group_by(overvalued,CUM_RUNS) %>%
    summarise(Count = n(), mi = mi, .groups="drop") %>%
    distinct() %>%
    mutate(mi = round(mi,1))
  drl1_names_overvalued = drl2 %>% filter(overvalued)
  drl1_names_undervalued = drl2 %>% filter(!overvalued)
  if (diff) {
    diff_df = merge(drl1_names_undervalued, drl1_names_overvalued, by="CUM_RUNS", all=T) %>%
      mutate(Count.x = replace_na(Count.x,0),
             Count.y = replace_na(Count.y,0)) %>%
      mutate(Count = Count.x - Count.y) %>%
      select(CUM_RUNS,Count) %>%
      mutate(facet = "difference", mi=NA)
    drl3 = bind_rows(drl2 %>% mutate(facet = ifelse(overvalued, "overvalued", "undervalued")) %>% select(-overvalued), 
                     diff_df)
    drl3$facet = factor(drl3$facet, levels=c("undervalued","overvalued","difference"))
    drl3 = drl3 %>% mutate(color =
                             ifelse(facet=="difference" & Count > 0, "darkgreen",
                                    ifelse(facet=="difference" & Count < 0, "firebrick",
                                           "black")))
  } else {
    drl3 = drl2
    drl3$color = "black"
  }
  prl = drl3 %>%
    # ggplot(aes(x=CUM_RUNS,y=Count,label=mi)) +
    ggplot(aes(x=CUM_RUNS,y=Count)) +
    facet_wrap(~facet) +
    # geom_histogram() +
    geom_col(fill=drl3$color) +
    # geom_text(vjust = 1.5,color="white") +
    # geom_text(data=dnf2, aes( label = mi), vjust = -0.2)
    scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) +
    scale_y_continuous(name="Count",breaks=seq(-50,50,by=5))
  # labs(title="Distribution of Runs Allowed in a Game")
  prl
  # ggsave(paste0(output_folder,"plot_Lynn_Ryu_",year,".png"), prl)
}

### Fangraphs
ovf = WAR_df_2019 %>% select(PIT_NAME, GWAR, FWAR) %>% mutate(diff = GWAR - FWAR) %>% arrange(diff) %>% slice_head(n=5)
uvf = WAR_df_2019 %>% select(PIT_NAME, GWAR, FWAR) %>% mutate(diff = GWAR - FWAR) %>% arrange(-diff) %>% slice_head(n=5)

pf_agg = agg_pit1_vs_pit2_hists(ovf$PIT_NAME, uvf$PIT_NAME)
pf_agg

# ### Baseball Reference
# ovb = WAR_df_2019 %>% select(PIT_NAME, GWAR, BWAR) %>% mutate(diff = GWAR - BWAR) %>% arrange(diff) %>% slice_head(n=5)
# uvb = WAR_df_2019 %>% select(PIT_NAME, GWAR, BWAR) %>% mutate(diff = GWAR - BWAR) %>% arrange(-diff) %>% slice_head(n=5)
# 
# pb_agg = agg_pit1_vs_pit2_hists(ovb$PIT_NAME, uvb$PIT_NAME)
# pb_agg

### save
ggsave(paste0(output_folder,"pf_agg_",year,".png"), pf_agg, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"pb_agg_",year,".png"), pb_agg, width=8.6, height=3.72)

#########################################
####### GERRIT COLE'S 2019 SEASON #######
#########################################

pit1_vs_pit2_hists("Gerrit Cole", "Justin Verlander")
pit1_vs_pit2_hists("Lance Lynn", "Justin Verlander")


plot_pit_szn <- function(pitname = "Gerrit Cole") {
  cole19 = pitcher_exits %>%
    left_join(WAR_df_2019) %>%
    filter(PIT_NAME == pitname) %>%
    select(DATE,HOME_TEAM_ID,AWAY_TEAM_ID,CUM_RUNS,INNING,exit_at_end_of_inning,
           BASE_STATE,OUTS_CT, GWAR) %>%
    mutate(
      BASE_STATE = as.character(BASE_STATE),
      OUTS_CT = as.character(OUTS_CT),
      BASE_STATE = ifelse(exit_at_end_of_inning, "", BASE_STATE),
      OUTS_CT = ifelse(exit_at_end_of_inning, "", OUTS_CT),
    ) %>%
    arrange(DATE) %>%
    rename(GWAR = GWAR,
           `HOME TEAM` = HOME_TEAM_ID,
           `AWAY TEAM` = AWAY_TEAM_ID,
           `RUNS ALLOWED` = CUM_RUNS,
           `EXIT INNING` = INNING,
           `EXIT BASE STATE` = BASE_STATE,
           `EXIT OUTS` = OUTS_CT) %>%
    mutate(GWAR = round(GWAR,3)) %>%
    select(-exit_at_end_of_inning) 
  data.frame(cole19)
  WAR_df_2019 %>% filter(PIT_NAME == "Gerrit Cole") %>% select(GWAR)
  
  
  plot_cole19 = cole19 %>% gt() %>%
    data_color(
      columns = GWAR,
      colors = scales::col_numeric(
        # palette = c("red", "green"),
        # domain = NULL
        # palette = c("#f8696b","#63be7b"), #"#ffeb84",
        palette = c("#f8696b","#ffeb84", "#63be7b"), 
        # palette = c("#b22222","#228B22"), #"#ffeb84",
        domain = c(min(cole19$GWAR), max(cole19$GWAR))
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

library(gt)
gtsave(plot_pit_szn("Gerrit Cole"), 
       paste0(output_folder,"plot_pitszn_", "Cole", "_", year,".png"))
gtsave(plot_pit_szn("Lance Lynn"), 
       paste0(output_folder,"plot_pitszn_", "Lynn", "_", year,".png"))
# gtsave(plot_pit_szn("Zack Greinke"), 
#        paste0(output_folder,"plot_pitszn_", "Greinke", "_", year,".png"))
# gtsave(plot_pit_szn("Julio Teheran"), 
#        paste0(output_folder,"plot_pitszn_", "Teheran", "_", year,".png"))
# gtsave(plot_pit_szn("Jose Berrios"), 
#        paste0(output_folder,"plot_pitszn_", "Berrios", "_", year,".png"))
# gtsave(plot_pit_szn("Walker Buehler"), 
#        paste0(output_folder,"plot_pitszn_", "Buehler", "_", year,".png"))
# gtsave(plot_pit_szn("Miles Mikolas"), 
#        paste0(output_folder,"plot_pitszn_", "Mikolas", "_", year,".png"))

################################################
####### GERRIT COLE'S 2019 GWAR RANKINGS #######
################################################

gwar_2019_rankings = WAR_df_2019 %>%
  drop_na() %>%
  select(PIT_NAME, GWAR) %>%
  arrange(-GWAR) %>%
  ggplot() +
  geom_point(aes(x = GWAR, y = fct_reorder(PIT_NAME, GWAR)), 
             size=2, color="black") + #dodgerblue2
  scale_x_continuous(breaks=seq(0,20,by=1), name="Grid WAR") +
  ylab("pitcher")
# gwar_2019_rankings
ggsave(paste0(output_folder,"plot_gwar_rankings_",year,".png"), width=12, height=17)

data.frame(WAR_df_2019 %>% select(PIT_NAME, GWAR) %>% arrange(-GWAR))




