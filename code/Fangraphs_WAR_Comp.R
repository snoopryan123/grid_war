library(tidyverse)
library(plotly)
library(ggthemes)
theme_set(theme_bw())
theme_update(text = element_text(size=16))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = "./plots/"

#############################
########### DATA ############
#############################

FanGraphsLeaderboard_2019 <- read_csv("FanGraphsLeaderboard_2019.csv")
GWAR_2019 <- read_csv("GWAR_2019.csv") %>% rename(GWAR_og = GWAR)
FWAR_2019 = FanGraphsLeaderboard %>% select (Name, WAR) %>% rename(PIT_NAME = Name, FWAR = WAR)
pitcher_exits_2019 = read_csv("pitcher_exits_2019.csv")

# pitcher_exits_2014 = read_csv("pitcher_exits_2014.csv")

merged <- left_join(FWAR_2019, GWAR_2019, by = "PIT_NAME") %>% na.omit() 

## RESCALE (because WAR is relative at this point)
sg = sum(merged$GWAR_og); sf = sum(merged$FWAR);
merged = merged %>% mutate(GWAR = GWAR_og/sg*sf)
### columns for easy plotting
merged = merged %>% mutate(vert_distance = GWAR - FWAR) ### x=FWAR, y=GWAR

### examine these pitchers
kk = 5
merged %>% arrange(-vert_distance) %>% head(kk)
merged %>% arrange(-vert_distance) %>% tail(kk)
merged %>% arrange(-vert_distance) %>% filter(abs(vert_distance) <= 0.15)
pit_names_examine = (bind_rows(merged %>% arrange(-vert_distance) %>% head(kk),
                               merged %>% arrange(-vert_distance) %>% tail(kk),
                               merged %>% arrange(-vert_distance) %>% filter(abs(vert_distance) <= 0.15)) %>%
                       select(PIT_NAME))$PIT_NAME
merged = merged %>% mutate(examine_pit = PIT_NAME %in% pit_names_examine)

#############################
########### PLOTS ###########
#############################

### Grid War vs. Fangraphs WAR for 2019
pgf19 = merged %>% mutate(label = ifelse(examine_pit, PIT_NAME, "")) %>%
  ggplot(aes(x=FWAR,y=GWAR, label = label)) +
  theme_solarized() +
  geom_abline(slope=1, intercept=0) +
  # geom_smooth(method='lm', formula= y~x, se = FALSE, color="dodgerblue") +
  geom_point() +
  geom_text(hjust=-.05, vjust=0) +
  labs(title="Grid WAR vs. Fangraphs WAR for Starting Pitchers in 2019") +
  scale_x_continuous(name="Fangraphs WAR", limits = c(0,8)) +
  scale_y_continuous(name="Grid WAR",limits = c(0,8)) #,breaks = BREAKS
pgf19
ggsave(paste0(output_folder,"GWAR_vs_FWAR_2019.png"), pgf19)
# ggplotly(p1)


ggplot(nba, aes(x= MIN, y= PTS, colour="green", label=Name))+
  geom_point() +geom_text(hjust=0, vjust=0)




# k=1
# View(pitcher_exits_2019 %>% select(GAME_ID,INNING,PIT_NAME,CUM_RUNS,
#     exit_at_end_of_inning,exit_in_middle,PIT_WINS) %>% filter(PIT_NAME == pit_names_examine[k]))


pitcher_exits_2019 %>% 
  filter(PIT_NAME %in% pit_names_examine) %>%
  ggplot(aes(x=CUM_RUNS)) +
  facet_wrap(~PIT_NAME) +
  geom_histogram(alpha=.2)
  


### AGGREGATED HISTOGRAMS
pit_examine_agg = merged %>% mutate(bin = cut(vert_distance, breaks=c(-3,-1.25,-0.2,0.2,1.25,3))) %>%
  arrange(bin) %>%
  filter(!(bin %in% c("(-1.25,-0.2]", "(0.2,1.25]"))) %>% #%>% group_by(dist_bin) %>% #summarise(count=n())
  select(PIT_NAME, bin)
pit_examine_agg 

pitcher_exits_2019 %>% 
  left_join(pit_examine_agg) %>%
  filter(PIT_NAME %in% pit_examine_agg$PIT_NAME) %>%
  ggplot(aes(x=CUM_RUNS)) +
  facet_wrap(~bin) +
  geom_histogram(alpha=.2)



