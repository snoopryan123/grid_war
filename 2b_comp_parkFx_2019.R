library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
library(gt)
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
FWAR_2019 = read_csv("df_FWAR.csv") %>% filter(YEAR == 2019)
  #read_csv("FanGraphsLeaderboard_2019.csv") %>% 
  # select (Name, WAR) %>% rename(PIT_NAME = Name, FWAR = WAR)
# BWAR_2019 = read_csv("BR_war_daily_pitch.txt") %>% #https://www.baseball-reference.com/data/war_daily_pitch.txt
#   filter(year_ID == 2019) %>%
#   mutate(WAR = as.numeric(WAR)) %>%
#   group_by(name_common) %>% summarise(WAR = sum(WAR, na.rm=TRUE)) %>% ungroup() %>%
#   rename(PIT_NAME = name_common, BWAR_og = WAR)
  
GWAR_2019 <- read_csv("df_GWAR_2019_pf_ridge.csv") %>% rename(GWAR_ridgeParkFx_og = GWAR)
GWAR_2019_woParkFx <- read_csv("df_GWAR_2019_pf_FALSE.csv") %>% rename(GWAR_woParkFx_og = GWAR)
GWAR_2019_espnParkFx <- read_csv("df_GWAR_2019_pf_espn.csv") %>% rename(GWAR_espnParkFx_og = GWAR)
GWAR_2019_fangraphsParkFx <- read_csv("df_GWAR_2019_pf_FanGraphs.csv") %>% rename(GWAR_fangraphsParkFx_og = GWAR)
pitcher_exits = read_csv("df_pitcher_exits_2019_pf_ridge.csv")

WAR_df_2019.0 <- 
  (GWAR_2019 %>% select(-c(N,GW,w_rep))) %>% 
  left_join(GWAR_2019_woParkFx %>% select(-c(N,GW,w_rep))) %>% 
  left_join(GWAR_2019_espnParkFx %>% select(-c(N,GW,w_rep))) %>%
  left_join(GWAR_2019_fangraphsParkFx %>% select(-c(N,GW,w_rep))) %>% 
  left_join(FWAR_2019 %>% select(c(PIT_NAME, FWAR_FIP, FWAR_RA9))) #%>% left_join(BWAR_2019) 
WAR_df_2019.1 = WAR_df_2019.0 %>% drop_na()

##### RESCALE WAR (because WAR is relative at this point)
mu_f = mean(WAR_df_2019.1$FWAR_FIP); sig_f = sd(WAR_df_2019.1$FWAR_FIP);
WAR_df_2019.1 = WAR_df_2019.1 %>% rename(FWAR_og = FWAR_FIP) #rename(BWAR = BWAR_og, FWAR_og = FWAR)
# mu_b = mean(WAR_df_2019.1$BWAR); sig_b = sd(WAR_df_2019.1$BWAR);

WAR_df_2019.1 = WAR_df_2019.1 %>% mutate(
  # BWAR = (BWAR_og - mean(BWAR_og))/sd(BWAR_og)*sig_f + mu_f,
  GWAR_ridgeParkFx = (GWAR_ridgeParkFx_og - mean(GWAR_ridgeParkFx_og))/sd(GWAR_ridgeParkFx_og)*sig_f + mu_f,
  GWAR_woParkFx = (GWAR_woParkFx_og - mean(GWAR_woParkFx_og))/sd(GWAR_woParkFx_og)*sig_f + mu_f,
  GWAR_espnParkFx = (GWAR_espnParkFx_og - mean(GWAR_espnParkFx_og))/sd(GWAR_espnParkFx_og)*sig_f + mu_f,
  GWAR_fangraphsParkFx = (GWAR_fangraphsParkFx_og - mean(GWAR_fangraphsParkFx_og))/sd(GWAR_fangraphsParkFx_og)*sig_f + mu_f
  # FWAR = (FWAR_og - mean(FWAR_og))/sd(FWAR_og)*sig_f + mu_f,
  # GWAR = (GWAR_og - mean(GWAR_og))/sd(GWAR_og)*sig_b + mu_b,
  # GWAR_woParkFx = (GWAR_woParkFx_og - mean(GWAR_woParkFx_og))/sd(GWAR_woParkFx_og)*sig_b + mu_b,
  # GWAR_espnParkFx = (GWAR_espnParkFx_og - mean(GWAR_espnParkFx_og))/sd(GWAR_espnParkFx_og)*sig_b + mu_b,
  # GWAR_fangraphsParkFx = (GWAR_fangraphsParkFx_og - mean(GWAR_fangraphsParkFx_og))/sd(GWAR_fangraphsParkFx_og)*sig_b + mu_b
)
WAR_df_2019 = WAR_df_2019.1 %>% select(-c(GWAR_woParkFx_og,GWAR_espnParkFx_og)) #,BWAR_og
# WAR_df_2019 = WAR_df_2019.1 %>% select(-c(FWAR_og,GWAR_og,GWAR_woParkFx_og,GWAR_espnParkFx_og))
WAR_df_2019
print(c(sum(WAR_df_2019$FWAR_og), sum(WAR_df_2019$GWAR_ridgeParkFx)))
# print(c(sum(WAR_df_2019$FWAR), sum(WAR_df_2019$BWAR), sum(WAR_df_2019$GWAR)))

#############################
########### PLOTS ###########
#############################

#######################################################################
########### SHOW HOW DIFFERENT PARK EFFECTS CHANGE GRID WAR ###########
#######################################################################

### Compare various park factors in 2019

{
  data.frame(
    WAR_df_2019 %>% 
      mutate(diff = abs(GWAR_ridgeParkFx - GWAR_woParkFx)) %>%
      arrange(desc(diff))
  )
  data.frame(
    WAR_df_2019 %>% 
      mutate(diff = abs(GWAR_espnParkFx - GWAR_woParkFx)) %>%
      arrange(desc(diff))
  )
  data.frame(
    WAR_df_2019 %>% 
      mutate(diff = abs(GWAR_espnParkFx - GWAR_ridgeParkFx)) %>%
      arrange(desc(diff))
  )
}

{
  pfcomp1 = 
    WAR_df_2019.1 %>% 
    # select(-BWAR, - FWAR) %>%
    select(-starts_with("FWAR")) %>%
    reshape2::melt(c("PIT_NAME")) %>%
    rename(WAR = value, method = variable) %>%
    mutate(PIT_NAME = factor(PIT_NAME, 
                             levels = (WAR_df_2019.1 %>% arrange(GWAR_woParkFx_og))$PIT_NAME)) %>%
    mutate(method = factor(method,
                           levels = c("GWAR_woParkFx_og", "GWAR_ridgeParkFx_og", "GWAR_espnParkFx_og"))) %>%
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
                       labels = c("No park effects", "Ridge park effects", "ESPN park effects")
    ) +
    scale_shape_manual(name = "GWAR with",
                       labels = c("No park effects", "Ridge park effects", "ESPN park effects"),
                       values = c(17,15,19))
  
  pfcomp1
  ggsave(paste0(output_folder,"plot_compare_parkFx1_",year,".png"), pfcomp1, width=11, height=8)
}

{
  pfcomp = 
    WAR_df_2019 %>% 
    # WAR_df_2019.1 %>%
    # select(-BWAR, - FWAR) %>%
    select(-starts_with("FWAR")) %>%
    reshape2::melt(c("PIT_NAME")) %>%
    rename(WAR = value, method = variable) %>%
    mutate(PIT_NAME = factor(PIT_NAME, 
           levels = (WAR_df_2019 %>% arrange(GWAR_woParkFx))$PIT_NAME)) %>%
    mutate(method = factor(method,
            levels = c("GWAR_woParkFx", "GWAR_ridgeParkFx", "GWAR_espnParkFx")
            # levels = c("GWAR_woParkFx", "GWAR_ridgeParkFx", "GWAR_fangraphsParkFx")
          )) %>%
    drop_na() %>%
    ggplot() + 
    geom_point(aes(x=WAR, y=PIT_NAME, color=method, shape=method),
                    size=3, alpha=0.85) +
    ylab("Pitcher") +
    theme(axis.text.y = element_text(size=8)) +
    scale_color_brewer(palette = "Set1",
                       name = "GWAR with",
                       labels = c("No park effects", "Ridge park effects", "ESPN park effects")
    ) +
    scale_shape_manual(name = "GWAR with",
                       labels = c("No park effects", "Ridge park effects", "ESPN park effects"),
                       values = c(17,15,19))
  
  pfcomp
  # ggsave(paste0(output_folder,"plot_compare_parkFx_",year,".png"), pfcomp, width=11, height=8)
}

# ### Grid War (with Fangraphs park effects) vs. Fangraphs WAR for 2019
# {
#   data.frame(
#     WAR_df_2019 %>% mutate(diff = (GWAR_fangraphsParkFx - FWAR)) %>%
#       select(PIT_NAME, GWAR, BWAR, diff) %>% arrange(diff)
#   )
#   WAR_df_2019$pit_examine2 = WAR_df_2019$PIT_NAME %in% c(
#     "Jose Berrios", "Jose Quintana", "Shane Bieber", "Reynaldo Lopez", #"Lucas Giolito",
#     "Julio Teheran", "Dakota Hudson", "Jeff Samardzija", "Clayton Kershaw", #"Mike Fiers",
#     "Tanner Roark", "Kyle Hendricks", "Rick Porcello", "Eduardo Rodriguez", "Sonny Gray"
#   )
#   
#   pgf = WAR_df_2019 %>% 
#     mutate(label = ifelse(pit_examine2, PIT_NAME, "")) %>%
#     ggplot(aes(x=FWAR, y=GWAR_fangraphsParkFx, label = label)) + ##label = PIT_NAME
#     geom_abline(slope=1, intercept=0) +
#     geom_point() +
#     geom_text(hjust=-.05, vjust=-0.05, size=3) +
#     scale_x_continuous(name="Fangraphs WAR", limits = c(0,8)) +
#     scale_y_continuous(name="Grid WAR (with Fangraphs park factors)",limits = c(1,8)) 
#   pgf
#   ggsave(paste0(output_folder,"plot_GWAR_vs_FWAR_",year,".png"), pgf, width=8, height=8)
# }





