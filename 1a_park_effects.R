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
tq_effects = left_join(dtq_effects1, otq_effects1) %>% mutate(YEAR = as.numeric(YEAR))
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
  scale_y_continuous(name = "offensive team quality", breaks = seq(-1,1,by=0.05)) +
  geom_vline(aes(xintercept=0), linetype="dashed", alpha=0.5) +
  geom_hline(aes(yintercept=0), linetype="dashed", alpha=0.5) +
  annotate("text", x = -0.05, y=-0.10, label = "Good Defensive Team, \n Bad Offensive Team", color="dodgerblue2") +
  annotate("text", x = 0.05, y=-0.10, label = "Bad Defensive Team, \n Bad Offensive Team", color="dodgerblue2") +
  annotate("text", x = -0.05, y=0.10, label = "Good Defensive Team, \n Good Offensive Team", color="dodgerblue2") +
  annotate("text", x = 0.05, y=0.10, label = "Bad Defensive Team, \n Good Offensive Team", color="dodgerblue2")
dtq_otq_plot_2019
ggsave("plots/plot_2019_otq_dtq.png", dtq_otq_plot_2019, width=8, height=8)

####################
### PARK EFFECTS ###
####################

### road team's offensive production at each park
### runs scored by away teams ~ factor(park) + away_team_off_q + home_team_def_q
### per 9 innings

park_df = OTQ2 %>%
  left_join(tq_effects %>% select(TEAM, YEAR, otq) %>% rename(AWAY_TEAM_ID = TEAM, AWAY_TEAM_otq = otq)) %>%
  left_join(tq_effects %>% select(TEAM, YEAR, dtq) %>% rename(HOME_TEAM_ID = TEAM, HOME_TEAM_dtq = dtq))
park_df
park_df1 = park_df %>% left_join(park_df %>% group_by(PARK) %>% summarise(count = n()) %>% arrange(count))
park_df2 = park_df1 %>% filter(count > 15)

park_lm = lm(CUM_RUNS ~ factor(PARK) + AWAY_TEAM_otq + HOME_TEAM_dtq + 0, data=park_df2)
park_effects = coefficients(park_lm)
park_effects1 = tibble(stack(park_effects)) %>% 
  filter(ind != "AWAY_TEAM_otq" & ind != "HOME_TEAM_dtq") %>%
  rename(park_effect = values, PARK = ind) %>%
  mutate(PARK = str_remove(PARK, "factor\\(PARK\\)")) %>%
  ### de-mean the park effects, and get park-effects-per-9
  mutate(park_effect = park_effect - mean(park_effect),
         park_effect = park_effect/9
        ) %>%
  arrange(park_effect) 
data.frame(park_effects1)

### plot park effects
plot_park_effects = park_effects1 %>%
  ggplot(aes(x=park_effect, y=reorder(PARK, park_effect))) +
  geom_point(shape=21, size=3, fill="black") +
  # scale_x_continuous(breaks = seq(-1,1,by=0.05)) + 
  ylab("park") +
  labs(x="park effect")
plot_park_effects
ggsave("plots/plot_park_effects.png", plot_park_effects, width=8, height=8)

### save park effects
write_csv(park_effects1, "park_effects.csv")





