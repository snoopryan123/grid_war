
source("0_load_stuff.R")

################################################
#### Create Play-By-Play Dataset `war2.csv` ####
################################################

## filename = "retro_final_PA_1990-2000d.csv" # from dropbox. see README.md
filename = "../TTO_/data/retro_final_PA_1990-2020d.csv"
pbp <- read_csv(filename) 

# PIT_WINS === 1{PIT's team wins this game}
war2 <- pbp %>% filter(2010 <= YEAR & YEAR <= 2019)
war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  mutate(bat_team_runs = sum(EVENT_RUNS))
war2 <- war2 %>%
  group_by(GAME_ID) %>%
  mutate(total_runs = sum(EVENT_RUNS))
war2 <- war2 %>%
  mutate(PIT_RUNS = total_runs - bat_team_runs) 
war2 <- war2 %>%
  mutate(PIT_WINS = case_when(
    PIT_RUNS > bat_team_runs ~ 1,
    PIT_RUNS < bat_team_runs ~ 0
  ))
war2 <- war2 %>% filter(!is.na(PIT_WINS)) # filter NA

# Accumulating runs
war2 <- war2 %>%
  ungroup() %>%
  group_by(GAME_ID, BAT_HOME_IND) %>%
  mutate(CUM_RUNS = cumsum(EVENT_RUNS)) %>%
  ungroup()

# Filtering out every last play of inning
last_play_every_inning <- war2 %>% 
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  filter(row_number() == n()) %>%
  ungroup()

# Check
# View(last_play_every_inning %>% arrange(BAT_HOME_IND) %>% filter(GAME_ID == "ANA201804020") %>%
#        select(GAME_ID, BAT_HOME_IND, INNING,
#               EVENT_TX, EVENT_RUNS, CUM_RUNS))

#Determining how many runs were scored during remainder of innings
war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  mutate(final = if_else(row_number() == n(), 1, 0)) %>% #determining last at-bat of innings
  mutate(start = if_else(row_number() == 1, 1, 0)) %>%
  mutate(inning_end_runs = case_when(final == 1 ~ CUM_RUNS)) %>%
  ungroup()

war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  mutate(CUM_RUNS_BY_INNING = cumsum(EVENT_RUNS)) %>%
  mutate(CUM_RUNS2 = CUM_RUNS_BY_INNING - EVENT_RUNS) %>%
  ungroup()

#add in total runs scored in the inning
war2 <- war2 %>%
  group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
  mutate(INN_RUNS = sum(EVENT_RUNS)) %>%
  ungroup()

#add in runs scored rest of the inning
### CUM_RUNS is the number of cumulative runs allowed by the pitching team in the game for AFTER the current play
war2 <- war2 %>%
  mutate(REST_INN_RUNS = INN_RUNS - CUM_RUNS2) %>%
  ungroup()

# Check
# View(war2 %>% select(GAME_ID,BAT_HOME_IND,INNING,CUM_RUNS,EVENT_RUNS,CUM_RUNS2,INN_RUNS,REST_INN_RUNS) %>% filter(row_number() <= 200))

#concat inning and base state
war2$INN_SITCH <- paste(war2$OUTS_CT, war2$BASE_STATE)

war2 <- war2 %>%
  mutate(inn_sitch_seq = case_when(
    INN_SITCH == "0 000" ~ 1,
    INN_SITCH == "0 001" ~ 2,
    INN_SITCH == "0 010" ~ 3,
    INN_SITCH == "0 011" ~ 4,
    INN_SITCH == "0 100" ~ 5,
    INN_SITCH == "0 101" ~ 6,
    INN_SITCH == "0 110" ~ 7,
    INN_SITCH == "0 111" ~ 8,
    INN_SITCH == "1 000" ~ 9,
    INN_SITCH == "1 001" ~ 10,
    INN_SITCH == "1 010" ~ 11,
    INN_SITCH == "1 011" ~ 12,
    INN_SITCH == "1 100" ~ 13,
    INN_SITCH == "1 101" ~ 14,
    INN_SITCH == "1 110" ~ 15,
    INN_SITCH == "1 111" ~ 16,
    INN_SITCH == "2 000" ~ 17,
    INN_SITCH == "2 001" ~ 18,
    INN_SITCH == "2 010" ~ 19,
    INN_SITCH == "2 011" ~ 20,
    INN_SITCH == "2 100" ~ 21,
    INN_SITCH == "2 101" ~ 22,
    INN_SITCH == "2 110" ~ 23,
    INN_SITCH == "2 111" ~ 24
  ))

# Check
#View(war2 %>% group_by(INN_SITCH) %>% slice_head() %>% select(INN_SITCH, OUTS_CT, BASE_STATE) )

# PIT_LEAGUE
war2 = war2 %>% mutate(PIT_LEAGUE = ifelse(BAT_HOME_IND, AWAY_LEAGUE, HOME_LEAGUE))

### (exit_at_end_of_inning, exit_in_middle)
war2 = war2 %>%
  group_by(GAME_ID, PIT_NAME) %>%
  mutate(Pit_occurrence = row_number()) %>%
  mutate(max_row = if_else(Pit_occurrence == max(Pit_occurrence), 1, 0)) %>%
  ###mutate(first_pitch = if_else(Pit_occurrence == 1, 1, 0)) %>%
  ### mutate(enter_at_start_of_inning = if_else(start == 1 & first_pitch == 1, 1, 0)) %>%
  mutate(exit_at_end_of_inning = if_else(final == 1 & max_row == 1, 1, 0)) %>%
  mutate(exit_in_middle = if_else(final == 0 & max_row == 1, 1, 0)) %>%
  ungroup()

### Save war_2
write_csv(war2, "war2.csv")

