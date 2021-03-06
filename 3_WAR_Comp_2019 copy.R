library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
output_folder = "./plots/"
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
# theme_set(theme_solarized())

#############################
########### DATA ############
#############################

# ### https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=y&type=8&season=2014&month=0&season1=2014&ind=0
# GWAR_2014 <- read_csv("GWAR_2014.csv") %>% rename(GWAR_og = GWAR)
# FanGraphsLeaderboard_2014 <- read_csv("FanGraphsLeaderboard_2014.csv")
# FWAR_2014 = FanGraphsLeaderboard_2014 %>% select (Name, WAR) %>% rename(PIT_NAME = Name, FWAR = WAR)
# pitcher_exits = read_csv("pitcher_exits_2014.csv")
# year = 2014
# WAR_df_2019 <- left_join(FWAR_2014, GWAR_2014, by = "PIT_NAME") %>% na.omit() %>% mutate(year=year)

year = 2019
FWAR_2019 = read_csv("FanGraphsLeaderboard_2019.csv") %>% 
  select (Name, WAR) %>% rename(PIT_NAME = Name, FWAR = WAR)
BWAR_2019 = read_csv("BR_war_daily_pitch.txt") %>% #https://www.baseball-reference.com/data/war_daily_pitch.txt
  filter(year_ID == 2019) %>%
  mutate(WAR = as.numeric(WAR)) %>%
  group_by(name_common) %>% summarise(WAR = sum(WAR, na.rm=TRUE)) %>% ungroup() %>%
  rename(PIT_NAME = name_common, BWAR_og = WAR)
  
GWAR_2019 <- read_csv("GWAR_2019.csv") %>% rename(GWAR_og = GWAR)
GWAR_2019_woParkFx <- read_csv("GWAR_2019_withoutParkFx.csv") %>% rename(GWAR_woParkFx_og = GWAR)
GWAR_2019_espnParkFx <- read_csv("GWAR_2019_espnParkFx.csv") %>% rename(GWAR_espnParkFx_og = GWAR)
GWAR_2019_fangraphsParkFx <- read_csv("GWAR_2019_fangraphsParkFx.csv") %>% rename(GWAR_fangraphsParkFx_og = GWAR)
pitcher_exits = read_csv("pitcher_exits_2019.csv")

WAR_df_2019.0 <- GWAR_2019 %>% left_join(GWAR_2019_woParkFx) %>% left_join(GWAR_2019_espnParkFx) %>%
  left_join(GWAR_2019_fangraphsParkFx) %>% left_join(FWAR_2019) %>% left_join(BWAR_2019) 
WAR_df_2019.1 = WAR_df_2019.0 %>% drop_na()

## RESCALE WAR (because WAR is relative at this point)
# mu_f = mean(WAR_df_2019.1$FWAR); sig_f = sd(WAR_df_2019.1$FWAR); 
WAR_df_2019.1 = WAR_df_2019.1 %>% rename(BWAR = BWAR_og, FWAR_og = FWAR)
mu_b = mean(WAR_df_2019.1$BWAR); sig_b = sd(WAR_df_2019.1$BWAR); 

WAR_df_2019.1 = WAR_df_2019.1 %>% mutate(
  # BWAR = (BWAR_og - mean(BWAR_og))/sd(BWAR_og)*sig_b + mu_b,
  FWAR = (FWAR_og - mean(FWAR_og))/sd(FWAR_og)*sig_b + mu_b,
  GWAR = (GWAR_og - mean(GWAR_og))/sd(GWAR_og)*sig_b + mu_b,
  GWAR_woParkFx = (GWAR_woParkFx_og - mean(GWAR_woParkFx_og))/sd(GWAR_woParkFx_og)*sig_b + mu_b,
  GWAR_espnParkFx = (GWAR_espnParkFx_og - mean(GWAR_espnParkFx_og))/sd(GWAR_espnParkFx_og)*sig_b + mu_b,
  GWAR_fangraphsParkFx = (GWAR_fangraphsParkFx_og - mean(GWAR_fangraphsParkFx_og))/sd(GWAR_fangraphsParkFx_og)*sig_b + mu_b
)
# WAR_df_2019 = WAR_df_2019.1 %>% select(-c(BWAR_og,GWAR_og,GWAR_woParkFx_og,GWAR_espnParkFx_og))
WAR_df_2019 = WAR_df_2019.1 %>% select(-c(FWAR_og,GWAR_og,GWAR_woParkFx_og,GWAR_espnParkFx_og))
WAR_df_2019
print(c(sum(WAR_df_2019$FWAR), sum(WAR_df_2019$BWAR), sum(WAR_df_2019$GWAR)))

#############################
########### PLOTS ###########
#############################

### Compare various park factors in 2019
{
  data.frame(
    WAR_df_2019 %>% 
      mutate(diff = abs(GWAR - GWAR_woParkFx)) %>%
      arrange(desc(diff))
  )
  data.frame(
    WAR_df_2019 %>% 
      mutate(diff = abs(GWAR_espnParkFx - GWAR_woParkFx)) %>%
      arrange(desc(diff))
  )
  data.frame(
    WAR_df_2019 %>% 
      mutate(diff = abs(GWAR_espnParkFx - GWAR)) %>%
      arrange(desc(diff))
  )
}

{
  pfcomp = WAR_df_2019 %>% 
    select(-BWAR, - FWAR) %>%
    reshape2::melt(c("PIT_NAME")) %>%
    rename(WAR = value, method = variable) %>%
    mutate(PIT_NAME = factor(PIT_NAME, 
           levels = (WAR_df_2019 %>% arrange(GWAR_woParkFx))$PIT_NAME)) %>%
    mutate(method = factor(method,
            levels = c("GWAR_woParkFx", "GWAR", "GWAR_espnParkFx"))) %>%
    drop_na() %>%
    ggplot() + 
    geom_point(aes(x=WAR, y=PIT_NAME, color=method, shape=method),
                    size=3, alpha=0.85) +
    ylab("Pitcher") +
    theme(axis.text.y = element_text(size=8)) +
    # scale_color_brewer(palette = "Set1") +
    # guides(color=guide_legend(title="New Legend Title"))
    scale_color_brewer(palette = "Set1",
                       name = "GWAR with",
                       labels = c("no park effects", "ridge park effects", "ESPN park effects")
    ) +
    scale_shape_manual(name = "GWAR with",
                       labels = c("no park effects", "ridge park effects", "ESPN park effects"),
                       values = c(17,15,19))
  
  pfcomp
  ggsave(paste0(output_folder,"plot_compare_parkFx_",year,".png"), pfcomp, width=11, height=8)
}


### Grid War vs. Baseball Reference WAR for 2019
pit5a = (WAR_df_2019 %>% arrange(GWAR - BWAR) %>% slice_head(n=5))$PIT_NAME
pit5b = (WAR_df_2019 %>% arrange(abs(GWAR - BWAR)) %>% slice_head(n=5))$PIT_NAME
pit5c = (WAR_df_2019 %>% arrange(-GWAR + BWAR) %>% slice_head(n=5))$PIT_NAME
pits = c(pit5a, pit5b, pit5c)
WAR_df_2019$pit_examine1 = WAR_df_2019$PIT_NAME %in% pits
data.frame(
  WAR_df_2019 %>% mutate(diff = (GWAR - BWAR)) %>% select(PIT_NAME, GWAR, BWAR, diff) %>%
    arrange(diff)
)

{
  pgb = WAR_df_2019 %>% 
    mutate(label = ifelse(pit_examine1, PIT_NAME, "")) %>%
    ggplot(aes(x=BWAR, y=GWAR, label = label)) + ##label = PIT_NAME
    geom_abline(slope=1, intercept=0) +
    geom_point() +
    geom_text(hjust=-.05, vjust=-0.05, size=3) +
    scale_x_continuous(name="Baseball Reference WAR", limits = c(0,8)) + 
    scale_y_continuous(name="Grid WAR", limits = c(1,8.5))  
  pgb
  # ggsave(paste0(output_folder,"plot_GWAR_vs_BWAR_",year,".png"), pgb, width=8, height=8)
}

### example histograms of GWAR vs. BWAR
{
  get_pit_histogram_exs <- function(pit_names) {
    pitcher_exits %>%
      filter(PIT_NAME %in% pit_names) %>%
      select(PIT_NAME, CUM_RUNS) %>%
      group_by(PIT_NAME, CUM_RUNS) %>%
      summarise(count = n(), .groups="drop") %>%
      ggplot() +
      facet_wrap(~PIT_NAME, nrow=1) +
      geom_col(aes(x = CUM_RUNS, y = count), fill="black", color="black") +
      scale_x_continuous(breaks=(0:50)*2 ) +
      scale_y_continuous(breaks=(0:50)*2 ) +
      labs(title="",x="",y="")
  }
  
  p5a = get_pit_histogram_exs(pit5a[c(1,2,5)])
  p5a
  p5b = get_pit_histogram_exs(pit5b[c(1,2,3)])
  p5b
  p5c = get_pit_histogram_exs(pit5c[c(1,2,5)])
  p5c
  
  # ggsave(paste0(output_folder,"plot_3hists_GWvsBW_5a.png"), p5a, width=10, height=4)
  # ggsave(paste0(output_folder,"plot_3hists_GWvsBW_5b.png"), p5b, width=10, height=4)
  # ggsave(paste0(output_folder,"plot_3hists_GWvsBW_5c.png"), p5c, width=10, height=4)
}

### Grid War (with Fangraphs park effects) vs. Fangraphs WAR for 2019
{
  data.frame(
    WAR_df_2019 %>% mutate(diff = (GWAR_fangraphsParkFx - FWAR)) %>%
      select(PIT_NAME, GWAR, BWAR, diff) %>% arrange(diff)
  )
  WAR_df_2019$pit_examine2 = WAR_df_2019$PIT_NAME %in% c(
    "Jose Berrios", "Jose Quintana", "Shane Bieber", "Reynaldo Lopez", #"Lucas Giolito",
    "Julio Teheran", "Dakota Hudson", "Jeff Samardzija", "Clayton Kershaw", #"Mike Fiers",
    "Tanner Roark", "Kyle Hendricks", "Rick Porcello", "Eduardo Rodriguez", "Sonny Gray"
  )
  
  pgf = WAR_df_2019 %>% 
    mutate(label = ifelse(pit_examine2, PIT_NAME, "")) %>%
    ggplot(aes(x=FWAR, y=GWAR_fangraphsParkFx, label = label)) + ##label = PIT_NAME
    geom_abline(slope=1, intercept=0) +
    geom_point() +
    geom_text(hjust=-.05, vjust=-0.05, size=3) +
    scale_x_continuous(name="Fangraphs WAR", limits = c(0,8)) +
    scale_y_continuous(name="Grid WAR (with Fangraphs park factors)",limits = c(1,8)) 
  pgf
  ggsave(paste0(output_folder,"plot_GWAR_vs_FWAR_",year,".png"), pgf, width=8, height=8)
 }







# ### Example Cum Runs Distributions, 2019
# {
#   df2 = pitcher_exits %>% 
#     left_join(WAR_df_2019) %>%
#     mutate(diff_GWAR_BWAR = GWAR - BWAR) %>%
#     filter(pit_examine1) %>%
#     arrange(-diff_GWAR_BWAR) 
#   p2L = df2 %>% 
#     filter(diff_GWAR_BWAR < -1) %>%
#     mutate(pitname_vertdist = paste0(PIT_NAME,
#           " (",round(diff_GWAR_BWAR,2),")")) %>%
#     ggplot(aes(x=CUM_RUNS)) +
#     facet_wrap(~pitname_vertdist, nrow=1) +
#     geom_histogram() +
#     labs(title="",x="",y="")
#   p2M = df2 %>% 
#     filter(abs(diff_GWAR_BWAR) < .25) %>%
#     mutate(pitname_vertdist = paste0(PIT_NAME,
#                                      " (",round(diff_GWAR_BWAR,2),")")) %>%
#     ggplot(aes(x=CUM_RUNS)) +
#     facet_wrap(~pitname_vertdist, nrow=1) +
#     geom_histogram() +
#     labs(title="",x="",y="")
#   p2U = df2 %>% 
#     filter(diff_GWAR_BWAR > 1) %>%
#     mutate(pitname_vertdist = paste0(PIT_NAME,
#                                      " (",round(diff_GWAR_BWAR,2),")")) %>%
#     ggplot(aes(x=CUM_RUNS)) +
#     facet_wrap(~pitname_vertdist, nrow=1) +
#     geom_histogram() + 
#     labs(title="",x="",y="")
#   p2 = plot_grid(p2L,p2M,p2U, nrow=3) 
#   p2
#   # ggsave(paste0(output_folder,"plot_ex_CumRunsDists_",year,".png"), p2)
# }
# 
# ### AGGREGATED HISTOGRAMS
# {
#   num_pits_per_bin = 7 #kk
#   l = floor(min(WAR_df_2019$vert_distance))
#   x0 = round((WAR_df_2019 %>% arrange(vert_distance) %>% filter(row_number() == num_pits_per_bin))$vert_distance,2)
#   s = round((WAR_df_2019 %>% arrange(abs(vert_distance)) %>% filter(row_number() == num_pits_per_bin))$vert_distance,2)
#   x1 = round((WAR_df_2019 %>% arrange(-vert_distance) %>% filter(row_number() == num_pits_per_bin))$vert_distance,2)
#   u = ceiling(max(WAR_df_2019$vert_distance))
#   df3 = WAR_df_2019 %>% mutate(bin = cut(vert_distance, 
#             breaks=c(l,x0,-abs(s),abs(s),x1,u))) %>%
#     select(PIT_NAME,bin,GWAR,FWAR) %>%
#     arrange(bin) %>%
#     select(PIT_NAME, bin) %>%
#     filter(bin != paste0("(",x0,",",-abs(s),"]") & bin != paste0("(",abs(s),",",x1,"]"))
#   levels(df3$bin) = c("overvalued","x0","equally valued","x1", "undervalued")
#   p3 = pitcher_exits %>% 
#     left_join(df3) %>%
#     filter(bin != "equally valued") %>% ### ????
#     filter(PIT_NAME %in% df3$PIT_NAME) %>%
#     ggplot(aes(x=CUM_RUNS)) +
#     facet_wrap(~bin) +
#     geom_histogram(aes(y=..density..),size=4,fill="black") + #geom_histogram() +
#     # labs(title="Distribution of Runs Allowed in a Game") +
#     scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2),)
#   p3
#   ggsave(paste0(output_folder,"plot_cumRuns_aggregated_",year,".png"), p3, width=8, height=4)
# }
# 
# 
# 
# 
# ### pitcher vs. pitcher histograms
# pit1_vs_pit2_hists <- function(name1, name2, diff=TRUE) {
#   drl = pitcher_exits %>%
#     left_join(WAR_df_2019) %>%
#     filter(PIT_NAME %in% c(name1, name2)) 
#   # drl %>% group_by(PIT_NAME) %>% summarise(mr = mean(CUM_RUNS), sdr = sd(CUM_RUNS))
#   drl1 = left_join(drl, drl %>% group_by(PIT_NAME,CUM_RUNS) %>% summarise(mi = mean(INNING), .groups="drop"))
#   # dnf1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>% arrange(PIT_NAME,CUM_RUNS)
#   drl2 = drl1 %>% select(PIT_NAME,GAME_ID,INNING,CUM_RUNS,mi) %>% 
#     arrange(PIT_NAME,CUM_RUNS) %>% 
#     group_by(PIT_NAME,CUM_RUNS) %>% 
#     summarise(Count = n(), mi = mi, .groups="drop") %>%
#     distinct() %>%
#     mutate(mi = round(mi,1))
#   drl1_name1 = drl2 %>% filter(PIT_NAME == name1)
#   drl1_name2 = drl2 %>% filter(PIT_NAME == name2)
#   if (diff) {
#     diff_df = merge(drl1_name1, drl1_name2, by="CUM_RUNS", all=T) %>%
#       mutate(Count.x = replace_na(Count.x,0),
#              Count.y = replace_na(Count.y,0)) %>%
#       mutate(Count = Count.x - Count.y) %>%
#       select(CUM_RUNS,Count) %>%
#       mutate(PIT_NAME = "Difference", mi=NA)
#     drl3 = bind_rows(drl2, diff_df)
#     drl3$PIT_NAME = factor(drl3$PIT_NAME, levels=c(name1,name2,"Difference"))
#     drl3 = drl3 %>% mutate(color = 
#                              ifelse(PIT_NAME=="Difference" & Count > 0, "darkgreen",
#                                     ifelse(PIT_NAME=="Difference" & Count < 0, "firebrick",
#                                            "black")))
#   } else {
#     drl3 = drl2
#     drl3$color = "black"
#   }
#   prl = drl3 %>%
#     # ggplot(aes(x=CUM_RUNS,y=Count,label=mi)) +
#     ggplot(aes(x=CUM_RUNS,y=Count)) +
#     facet_wrap(~PIT_NAME) +
#     # geom_histogram() + 
#     geom_col(fill=drl3$color) +
#     # geom_text(vjust = 1.5,color="white") +
#     # geom_text(data=dnf2, aes( label = mi), vjust = -0.2)
#     scale_x_continuous(name="Runs Allowed in a Game",breaks=seq(0,20,by=2)) +
#     scale_y_continuous(name="Count",breaks=seq(-20,20,by=2))
#   # labs(title="Distribution of Runs Allowed in a Game")
#   prl
#   # ggsave(paste0(output_folder,"plot_Lynn_Ryu_",year,".png"), prl)
# }
# 
# ### good FWAR pitchers
# pg1 = pit1_vs_pit2_hists("Justin Verlander", "Max Scherzer")
# pg1
# pg2 = pit1_vs_pit2_hists("Jacob deGrom", "Lance Lynn")
# pg2
# 
# ### decent FWAR pitchers
# pd1 = pit1_vs_pit2_hists("Sonny Gray","Jose Berrios")
# pd1
# pd2 = pit1_vs_pit2_hists("Mike Soroka","Marcus Stroman")
# pd2
# 
# ### bad pitchers
# pb1 = pit1_vs_pit2_hists("Sandy Alcantara","Reynaldo Lopez")
# pb1 
# pb2 = pit1_vs_pit2_hists("Dakota Hudson", "Mike Leake")
# pb2
# 
# ### most undervalued vs. most overvalued
# # puo1 = pit1_vs_pit2_hists("Julio Teheran","Jose Berrios",diff=FALSE)
# # puo1
# 
# # puo2 = pit1_vs_pit2_hists("Mike Fiers","Jose Quintana",diff=FALSE)
# # puo2
# # puo3 = pit1_vs_pit2_hists("Dakota Hudson","Jose Berrios",diff=FALSE)
# # puo3
# 
# WAR_df_2019 %>% arrange(-vert_distance) %>% head(kk)
# WAR_df_2019 %>% arrange(-vert_distance) %>% tail(kk)
# 
# puo1 = pit1_vs_pit2_hists("Julio Teheran","Clayton Kershaw",diff=FALSE)
# puo1
# puo2 = pit1_vs_pit2_hists("Jose Berrios","Jose Quintana",diff=FALSE)
# puo2
# # puo3 = pit1_vs_pit2_hists("Shane Bieber","Jose Berrios",diff=FALSE)
# # puo3
# 
# 
# 
# 
# # ggsave(paste0(output_folder,"p1_",year,".png"), pg1, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"p2_",year,".png"), pg2, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"p3_",year,".png"), pd1, width=8.6, height=3.72)
# # ggsave(paste0(output_folder,"p4_",year,".png"), pd2, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"p5_",year,".png"), pb1, width=8.6, height=3.72)
# # ggsave(paste0(output_folder,"p6_",year,".png"), pb2, width=8.6, height=3.72)
# ggsave(paste0(output_folder,"p7_",year,".png"), puo1, width=6.83, height=3.5)
# ggsave(paste0(output_folder,"p8_",year,".png"), puo2, width=6.83, height=3.5)







# #######################################
# ### CONVEXITY OF GWAR: CONSEQUENCES ###
# #######################################
# 
# f_grid <- read.csv("f_grid.csv",row.names = 1, header= TRUE) #read_csv("f_grid.csv", rown)
# get_f <- function(I_vec, R_vec) {
#   x = numeric(length(I_vec))
#   for (i in 1:length(I_vec)) {
#     # browser()
#     I = I_vec[i]
#     R = R_vec[i]
#     # I0 = floor(I)
#     # I1 = ceiling(I)
#     I = round(I)
#     R0 = floor(R)
#     R1 = ceiling(R)
#     f0 = f_grid[I,R0+1]
#     f1 = f_grid[I,R1+1]
#     x[i] <- f1 #f0 + (f1-f0)/(R1-R0) * (R-R0)
#   }
#   x
# }
# 
# w_rep = 0.41 #FIXME
# Jensen_df = pitcher_exits %>%
#   group_by(PIT_NAME) %>%
#   summarise(GWAR_szn = sum(GWAR_game),
#             # GWAR_game_avg = mean(GWAR_game),
#             INN_avg = mean(INNING),
#             # INN_avg_r = round(mean(INNING)),
#             RUN_avg = mean(CUM_RUNS),
#             num_games = n()) %>%
#   ungroup() %>%
#   mutate(avg_of_GWAR = GWAR_szn / num_games,
#          GWAR_of_avg = get_f(INN_avg, RUN_avg) - w_rep,
#          examine_pit = PIT_NAME %in% pit_names_examine)
# Jensen_df
# 
# ### Jensen's Inequality in 2019
# {
#   pj = Jensen_df %>% mutate(label = ifelse(examine_pit, PIT_NAME, "")) %>%
#     ggplot(aes(x=avg_of_GWAR, y=GWAR_of_avg, label=label)) +
#     geom_abline(slope=1, intercept=0, ) +
#     # geom_smooth(method='lm', formula= y~x, se = FALSE, color="dodgerblue") +
#     geom_point() +
#     # geom_text(hjust=-.05, vjust=0) +
#     geom_label(
#       label="y = x", 
#       x=.25,
#       y=.25,
#       label.padding = unit(0.55, "lines"), # Rectangle size around label
#       label.size = 0.35,
#       color = "black",
#       #fill="#69b3a2"
#     ) +
#     labs(title=paste0("The Convexity of Grid WAR in ",year)) +
#     scale_x_continuous(name="Average Grid WAR per Game", limits=c(-.3,.3)) +
#     scale_y_continuous(name="Grid WAR of an Average Game", limits=c(-.2,.25)) #,breaks = BREAKS
#   pj
#   # ggsave(paste0(output_folder,"plot_jensen_",year,".png"), pj)
#   # ggplotly(pgf)
# }

