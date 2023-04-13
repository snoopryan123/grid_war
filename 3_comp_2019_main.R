
source("0_load_stuff.R")

##########################
### Plotting Functions ###
##########################

##################################################
######### pitcher vs. pitcher histograms #########
##################################################

pit_v_pit_hists <- function(pit_names, diff=TRUE, pitcher_exits=pitcher_exits, war_df=WAR_df_2019, facet_yr=F) {
  # browser()
  drl = pitcher_exits %>%
    left_join(war_df) %>%
    filter(PIT_NAME %in% pit_names)
  # drl %>% group_by(PIT_NAME) %>% summarise(mr = mean(CUM_RUNS), sdr = sd(CUM_RUNS))
  drl1 = left_join(drl, drl %>% group_by(PIT_NAME,CUM_RUNS) %>% summarise(mi = mean(INNING), .groups="drop"))
  # dnf1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>% arrange(PIT_NAME,CUM_RUNS)
  if (facet_yr) {
    drl2 = drl1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi,YEAR) %>%
      arrange(PIT_NAME,YEAR,CUM_RUNS) %>%
      group_by(PIT_NAME,YEAR,CUM_RUNS) %>%
      summarise(Count = n(), mi = mi, YEAR, .groups="drop") %>%
      distinct() %>%
      mutate(mi = round(mi,1))
  } else {
    drl2 = drl1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>%
      arrange(PIT_NAME,CUM_RUNS) %>%
      group_by(PIT_NAME,CUM_RUNS) %>%
      summarise(Count = n(), mi = mi, .groups="drop") %>%
      distinct() %>%
      mutate(mi = round(mi,1))
  }
  
  drl3a = tibble()
  for (pit_name in pit_names) {
    drl1_name1 = drl2 %>% filter(PIT_NAME == pit_name)
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
    drl3a = bind_rows(drl3a, drl3)
  }
  
  # prl = drl3 %>%
  #   # ggplot(aes(x=CUM_RUNS,y=Count,label=mi)) +
  #   ggplot(aes(x=CUM_RUNS,y=Count)) +
  #   facet_wrap(~PIT_NAME) +
  #   # geom_histogram() +
  #   geom_col(fill=drl3$color) +
  #   # scale_y_continuous(name="Count",breaks=seq(-100,100,by=2)) +
  #   scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) 
  # # labs(title="Distribution of Runs Allowed in a Game")
  
  prl = drl3 %>% ggplot(aes(x=CUM_RUNS,y=Count))
  if (facet_yr) {
    prl = prl + facet_wrap(~PIT_NAME+YEAR)
  } else {
    prl = prl + facet_wrap(~PIT_NAME) 
  }
  prl = prl +
    geom_col(fill=drl3$color) +
    scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) 
  prl
  # ggsave(paste0(output_folder,"plot_Lynn_Ryu_",year,".png"), prl)
}


pit1_vs_pit2_hists <- function(name1, name2, diff=TRUE, pitcher_exits=pitcher_exits, war_df=WAR_df_2019, facet_yr=F) {
  # browser()
  drl = pitcher_exits %>%
    left_join(war_df) %>%
    filter(PIT_NAME %in% c(name1, name2))
  # drl %>% group_by(PIT_NAME) %>% summarise(mr = mean(CUM_RUNS), sdr = sd(CUM_RUNS))
  drl1 = left_join(drl, drl %>% group_by(PIT_NAME,CUM_RUNS) %>% summarise(mi = mean(INNING), .groups="drop"))
  # dnf1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>% arrange(PIT_NAME,CUM_RUNS)
  if (facet_yr) {
    drl2 = drl1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi,YEAR) %>%
      arrange(PIT_NAME,YEAR,CUM_RUNS) %>%
      group_by(PIT_NAME,YEAR,CUM_RUNS) %>%
      summarise(Count = n(), mi = mi, YEAR, .groups="drop") %>%
      distinct() %>%
      mutate(mi = round(mi,1))
  } else {
    drl2 = drl1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>%
      arrange(PIT_NAME,CUM_RUNS) %>%
      group_by(PIT_NAME,CUM_RUNS) %>%
      summarise(Count = n(), mi = mi, .groups="drop") %>%
      distinct() %>%
      mutate(mi = round(mi,1))
  }
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
  # prl = drl3 %>%
  #   # ggplot(aes(x=CUM_RUNS,y=Count,label=mi)) +
  #   ggplot(aes(x=CUM_RUNS,y=Count)) +
  #   facet_wrap(~PIT_NAME) +
  #   # geom_histogram() +
  #   geom_col(fill=drl3$color) +
  #   # scale_y_continuous(name="Count",breaks=seq(-100,100,by=2)) +
  #   scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) 
  # # labs(title="Distribution of Runs Allowed in a Game")
  
  prl = drl3 %>% ggplot(aes(x=CUM_RUNS,y=Count))
  if (facet_yr) {
    prl = prl + facet_wrap(~PIT_NAME+YEAR)
  } else {
    prl = prl + facet_wrap(~PIT_NAME) 
  }
  prl = prl +
    geom_col(fill=drl3$color) +
    scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) 
  prl
  # ggsave(paste0(output_folder,"plot_Lynn_Ryu_",year,".png"), prl)
}

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

#########################################
####### GERRIT COLE'S 2019 SEASON #######
#########################################

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

# ################################################
# ####### GERRIT COLE'S 2019 GWAR RANKINGS #######
# ################################################
# 
# gwar_2019_rankings = WAR_df_2019 %>%
#   drop_na() %>%
#   select(PIT_NAME, GWAR) %>%
#   arrange(-GWAR) %>%
#   ggplot() +
#   geom_point(aes(x = GWAR, y = fct_reorder(PIT_NAME, GWAR)), 
#              size=2, color="black") + #dodgerblue2
#   scale_x_continuous(breaks=seq(0,20,by=1), name="Grid WAR") +
#   ylab("pitcher")
# # gwar_2019_rankings
# ggsave(paste0(output_folder,"plot_gwar_rankings_",year,".png"), width=12, height=17)
# 
# data.frame(WAR_df_2019 %>% select(PIT_NAME, GWAR) %>% arrange(-GWAR))
# 



