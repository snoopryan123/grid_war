library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
theme_set(theme_bw())
theme_update(text = element_text(size=16))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = "./plots/"

#############################
########### DATA ############
#############################

# GWAR_2019 <- read_csv("GWAR_2019.csv") %>% rename(GWAR_og = GWAR)
# FanGraphsLeaderboard_2019 <- read_csv("FanGraphsLeaderboard_2019.csv")
# FWAR_2019 = FanGraphsLeaderboard_2019 %>% select (Name, WAR) %>% rename(PIT_NAME = Name, FWAR = WAR)
# pitcher_exits = read_csv("pitcher_exits_2019.csv")
# year = 2019
# merged <- left_join(FWAR_2019, GWAR_2019, by = "PIT_NAME") %>% na.omit() %>% mutate(year=year)

### https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=y&type=8&season=2014&month=0&season1=2014&ind=0
GWAR_2014 <- read_csv("GWAR_2014.csv") %>% rename(GWAR_og = GWAR)
FanGraphsLeaderboard_2014 <- read_csv("FanGraphsLeaderboard_2014.csv")
FWAR_2014 = FanGraphsLeaderboard_2014 %>% select (Name, WAR) %>% rename(PIT_NAME = Name, FWAR = WAR)
pitcher_exits = read_csv("pitcher_exits_2014.csv")
year = 2014
merged <- left_join(FWAR_2014, GWAR_2014, by = "PIT_NAME") %>% na.omit() %>% mutate(year=year)

## RESCALE (because WAR is relative at this point)
sg = sum(merged$GWAR_og); sf = sum(merged$FWAR);
merged = merged %>% mutate(GWAR = GWAR_og/sg*sf)
### columns for easy plotting
merged = merged %>% mutate(vert_distance = GWAR - FWAR) ### x=FWAR, y=GWAR

### examine these pitchers
kk = 5
# merged %>% arrange(-vert_distance) %>% head(kk)
# merged %>% arrange(-vert_distance) %>% tail(kk)
# merged %>% arrange(-vert_distance) %>% filter(abs(vert_distance) <= cutoff)
pit_names_examine = (bind_rows(merged %>% arrange(-vert_distance) %>% head(kk),
                               merged %>% arrange(-vert_distance) %>% tail(kk),
                               merged %>% arrange(abs(vert_distance)) %>% head(kk)) %>%
                       select(PIT_NAME))$PIT_NAME
merged = merged %>% mutate(examine_pit = PIT_NAME %in% pit_names_examine)

#############################
########### PLOTS ###########
#############################

### Grid War vs. Fangraphs WAR for 2019
{
  pgf = merged %>% mutate(label = ifelse(examine_pit, PIT_NAME, "")) %>%
    ggplot(aes(x=FWAR,y=GWAR, label = label)) +
    theme_solarized() +
    geom_abline(slope=1, intercept=0) +
    # geom_smooth(method='lm', formula= y~x, se = FALSE, color="dodgerblue") +
    geom_point() +
    geom_text(hjust=-.05, vjust=0) +
    labs(title=paste0("Grid WAR vs. Fangraphs WAR for Starting Pitchers in ",year)) +
    scale_x_continuous(name="Fangraphs WAR", limits = c(0,8)) +
    scale_y_continuous(name="Grid WAR",limits = c(0,8)) #,breaks = BREAKS
  pgf
  # ggsave(paste0(output_folder,"plot_GWAR_vs_FWAR_",year,".png"), pgf)
  # ggplotly(pgf)
}

### Example Cum Runs Distributions, 2019
{
  df2 = pitcher_exits %>% 
    left_join(merged) %>%
    filter(examine_pit) %>%
    arrange(-vert_distance) 
  p2L = df2 %>% 
    filter(vert_distance < -1) %>%
    mutate(pitname_vertdist = paste0(PIT_NAME,
          " (",round(vert_distance,2),")")) %>%
    ggplot(aes(x=CUM_RUNS)) +
    theme_solarized() +
    facet_wrap(~pitname_vertdist, nrow=1) +
    geom_histogram() +
    labs(title="",x="",y="")
  p2M = df2 %>% 
    filter(abs(vert_distance) < .25) %>%
    mutate(pitname_vertdist = paste0(PIT_NAME,
                                     " (",round(vert_distance,2),")")) %>%
    ggplot(aes(x=CUM_RUNS)) +
    theme_solarized() +
    facet_wrap(~pitname_vertdist, nrow=1) +
    geom_histogram() +
    labs(title="",x="",y="")
  p2U = df2 %>% 
    filter(vert_distance > 1) %>%
    mutate(pitname_vertdist = paste0(PIT_NAME,
                                     " (",round(vert_distance,2),")")) %>%
    ggplot(aes(x=CUM_RUNS)) +
    theme_solarized() +
    facet_wrap(~pitname_vertdist, nrow=1) +
    geom_histogram() + 
    labs(title="",x="",y="")
  p2 = plot_grid(p2L,p2M,p2U, nrow=3) 
  p2
  # ggsave(paste0(output_folder,"plot_ex_CumRunsDists_",year,".png"), p2)
}

### AGGREGATED HISTOGRAMS
{
  l = floor(min(merged$vert_distance))
  s = round((merged %>% arrange(abs(vert_distance)) %>% filter(row_number() == kk))$vert_distance,2)
  u = ceiling(max(merged$vert_distance))
  df3 = merged %>% mutate(bin = cut(vert_distance, 
            breaks=c(l,-s-1,-s,s,s+1,u))) %>%
    select(PIT_NAME,bin,GWAR,FWAR) %>%
    arrange(bin) %>%
    select(PIT_NAME, bin) %>%
    filter(bin != paste0("(",-s-1,",",-s,"]") & bin != paste0("(",s,",",s+1,"]"))
  
  p3 = pitcher_exits %>% 
    left_join(df3) %>%
    filter(PIT_NAME %in% df3$PIT_NAME) %>%
    ggplot(aes(x=CUM_RUNS)) +
    theme_solarized() +
    facet_wrap(~bin) +
    geom_histogram()
  p3
  # ggsave(paste0(output_folder,"plot_cumRuns_aggregated_",year,".png"), p3)
}


