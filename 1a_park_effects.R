library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = "./plots/"

war2 <- read_csv("war2.csv") ### dataset

##############################
### DEFENSIVE TEAM QUALITY ### 
##############################

### defensive production, measured on the road
### runs allowed by away teams ~ factor(defensive team)
### per 8 innings

DTQ1 = war2 %>% 
  select(GAME_ID, YEAR, INNING, HOME_TEAM_ID, AWAY_TEAM_ID, BAT_HOME_IND, BAT_ID, PIT_ID, PARK, EVENT_RUNS, CUM_RUNS) %>%
  filter(BAT_HOME_IND == 1) %>% ## the away team is on defense
  filter(INNING <= 8) ## first 8 innings, since home team is batting team, ignore bottom of 9th since not all games have this half inning
# View(DTQ1 %>% filter(GAME_ID == "ANA201904040"))

DTQ2 = DTQ1 %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  slice_tail() %>%
  ungroup()

dtq_lm = lm(CUM_RUNS ~ factor(AWAY_TEAM_ID):factor(YEAR) + 0, data=DTQ2)
dtq_effects = coefficients(dtq_lm)
names(dtq_effects) = str_remove(str_remove(names(dtq_effects), "factor\\(AWAY_TEAM_ID\\)"), "\\:factor\\(YEAR\\)")
dtq_effects1 = tibble(stack(dtq_effects)) %>% rename(dtq = values) %>%
  mutate(TEAM=str_sub(ind,1,3), YEAR=str_sub(ind,4)) %>% 
  select(-ind) %>% drop_na() %>%
  ### de-mean the def-team-quality for each season, and get dtq-per-8
  group_by(YEAR) %>%
  mutate(dtq = dtq - mean(dtq),
         dtq = dtq/8) %>%
  arrange(YEAR, dtq) %>%
  ungroup()
data.frame(dtq_effects1)

### plot 2019 dtq
dtq_plot_2019 = dtq_effects1 %>%
  filter(YEAR == 2019) %>%
  ggplot(aes(x=dtq, y=reorder(TEAM, dtq))) +
  geom_point(shape=21, size=3, fill="black") + 
  scale_x_continuous(breaks = seq(-1,1,by=0.05)) + #, limits = c(-0.15, 0.15)
  ylab("team") +
  labs(x="defensive team quality")
dtq_plot_2019
ggsave("plots/plot_2019_dtq.png", dtq_plot_2019, width=8, height=8)

##############################
### OFFENSIVE TEAM QUALITY ### 
##############################

### offensive production, measured on the road
### runs scored by away teams ~ factor(offesive team)
### per 9 innings

OTQ1 = war2 %>% 
  select(GAME_ID, YEAR, INNING, HOME_TEAM_ID, AWAY_TEAM_ID, BAT_HOME_IND, BAT_ID, PIT_ID, PARK, EVENT_RUNS, CUM_RUNS) %>%
  filter(BAT_HOME_IND == 0) %>% ## the away team is on offense
  filter(INNING <= 9) ## first 9 innings
# View(OTQ1 %>% filter(GAME_ID == "ANA201904040"))

OTQ2 = OTQ1 %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  slice_tail() %>%
  ungroup()

otq_lm = lm(CUM_RUNS ~ factor(AWAY_TEAM_ID):factor(YEAR) + 0, data=OTQ2)
otq_effects = coefficients(otq_lm)
names(otq_effects) = str_remove(str_remove(names(otq_effects), "factor\\(AWAY_TEAM_ID\\)"), "\\:factor\\(YEAR\\)")
otq_effects1 = tibble(stack(otq_effects)) %>% rename(otq = values) %>%
  mutate(TEAM=str_sub(ind,1,3), YEAR=str_sub(ind,4)) %>% 
  select(-ind) %>% drop_na() %>%
  ### de-mean the def-team-quality for each season, and get otq-per-9
  group_by(YEAR) %>%
  mutate(otq = otq - mean(otq),
         otq = otq/9) %>%
  arrange(YEAR, otq) %>%
  ungroup()
data.frame(otq_effects1)

### combine offensive and defensive team quality dataframes
tq_effects = left_join(dtq_effects1, otq_effects1)
tq_effects

### plot 2019 otq
otq_plot_2019 = otq_effects1 %>%
  filter(YEAR == 2019) %>%
  ggplot(aes(x=otq, y=reorder(TEAM, otq))) +
  geom_point(shape=21, size=3, fill="black") + 
  scale_x_continuous(breaks = seq(-1,1,by=0.05)) + #, limits = c(-0.15, 0.15)
  ylab("team") +
  labs(x="defensive team quality")
otq_plot_2019
ggsave("plots/plot_2019_otq.png", otq_plot_2019, width=8, height=8)

### plot 2019 dtq and otq
dtq_otq_plot_2019 = tq_effects %>%
  filter(YEAR == 2019) %>%
  ggplot(aes(x=dtq, y=otq, label=TEAM)) +
  geom_point() +
  geom_text(aes(hjust=0, vjust=1.5)) +
  scale_x_continuous(name = "defensive team quality", breaks = seq(-1,1,by=0.05)) +
  scale_y_continuous(name = "offensive team quality", breaks = seq(-1,1,by=0.05)) 
dtq_otq_plot_2019
ggsave("plots/plot_2019_otq_dtq.png", dtq_otq_plot_2019, width=8, height=8)

####################
### PARK EFFECTS ###
####################


unique(war2$PARK)




D1 = war2 %>% select(GAME_ID, YEAR, INNING, HOME_TEAM_ID, AWAY_TEAM_ID, BAT_HOME_IND, BAT_ID, PIT_ID, PARK, EVENT_RUNS, CUM_RUNS)
# View(D1)

D2 = D1 %>% filter(BAT_HOME_IND == 0) %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  slice_tail() %>%
  ungroup()
D3 = D2 %>% left_join(D2 %>% group_by(PARK) %>% summarise(count = n()) %>% arrange(count))
D4 = D3 %>% filter(count > 15)



park_lm = lm(CUM_RUNS ~ factor(PARK), data=D4)

park_factors_0 = sort(park_lm$coefficients[2:length(park_lm$coefficients)])
park_factors_0



#########################
### DEFENSIVE EFFECTS ###
#########################




