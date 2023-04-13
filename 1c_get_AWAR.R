
source("0_load_stuff.R")
war2 = read_csv("war2.csv")

##################################
#### Create AWAR grid: a(I,R) ####
##################################

### expected runs to the end of the inning as a function of base-state S and outs O
G_GRID = read.csv("model_g.csv", row.names = 1, header= TRUE)
expected_runs_SO = matrix(nrow=nrow(G_GRID), ncol=1)
rownames(expected_runs_SO) = rownames(G_GRID)
colnames(expected_runs_SO) = "expected_runs"
for (so in 1:nrow(G_GRID)) {
  r_ = 0:(ncol(G_GRID)-1)
  expected_runs_SO[so,1] = sum(G_GRID[so,] * r_)
}
# write.csv(as.data.frame(expected_runs_SO), "df_expected_runs_SO.csv")

get_expected_runs_toEndOfInning <- function(S,O) {
  ### base-state S, outs O
  expected_runs_SO[paste0(O," ",S),]
}
# get_expected_runs_toEndOfInning("001",2)

###
df_endOfGame = war2 %>%
  select(GAME_ID, PIT_NAME, exit_at_end_of_inning, exit_in_middle,
         INNING, CUM_RUNS, REST_INN_RUNS, PIT_WINS, BASE_STATE, OUTS_CT,
         BAT_HOME_IND, PIT_LEAGUE, YEAR) %>%
  filter(exit_at_end_of_inning==1 | exit_in_middle==1) %>%
  rowwise() %>%
  mutate(
    expected_runs_endOfInning = get_expected_runs_toEndOfInning(BASE_STATE, OUTS_CT),
    R_ = CUM_RUNS + expected_runs_endOfInning
  ) %>%
  relocate(expected_runs_endOfInning, .after=CUM_RUNS) %>%
  relocate(R_, .after=expected_runs_endOfInning)
df_endOfGame

model_f_0 = load_lm("model_f_0.rds")

################################################################################

df_AW_v2 = df_endOfGame %>%
  group_by(PIT_NAME, YEAR) %>%
  summarise(
    R_ = sum(R_),
    I = sum(INNING)
  ) %>% ungroup()
df_AW_v2

RPW = 9.07 ### from baseball reference
### Finding R_rep
df_FWAR1 = read_csv("df_FWAR1.csv")
# xx = df_FWAR1 %>% left_join(df_AW_v2) %>% filter(YEAR==2019) %>% drop_na()
# (sum(xx$FWAR)*RPW + sum(xx$R_)) / sum(xx$I) # R_rep
R_rep = 0.6431099

df_AW_v2a = df_AW_v2 %>%
  mutate(
    RABR = I*R_rep - R_,
    AWAR = RABR/RPW
  ) 
df_AW_v2a

write_csv(df_AW_v2a, "df_AWAR.csv")

# ### checks
# hist(df_AW_v2a$AWAR)
# df_FWAR1 %>% left_join(df_AW_v2a) %>% filter(YEAR==2019) %>% drop_na() %>%
#   summarise(AWAR_sum=sum(AWAR), FWAR_sum=sum(FWAR))







######################## OLD VERSION OF AWAR ###################################
################################################################################

# df_AW = df_endOfGame %>%
#   group_by(PIT_NAME, YEAR) %>%
#   summarise(
#     R_ = sum(R_),
#     I = sum(INNING),
#     N = n(),
#     R_per_N = R_/N,
#     I_per_N = I/N,
#     .groups="drop"
#   ) %>% mutate(
#     R_per_N = ifelse(R_per_N > 22, 22, R_per_N),
#     floor_R_per_N = floor(R_per_N),
#     floor_I_per_N = floor(I_per_N),
#     ceil_R_per_N = ceiling(R_per_N),
#     ceil_I_per_N = ceiling(I_per_N),
#     # r_ = ifelse(r_ > 22, 22, r_),
#     # r_floor = floor(r_),
#     # r_ceil = ceiling(r_),
#     # W = sum(PIT_WINS),
#   ) %>%
#   rowwise() %>%
#   mutate(
#     h1 = I_per_N - floor_I_per_N,
#     h2 = R_per_N - floor_R_per_N,
#     AW0 = (1-h1)*(1-h2)*predict(model_f_0, tibble(INNING=floor_I_per_N, CUM_RUNS=floor_R_per_N), type="response") +
#           (1-h1)*h2*predict(model_f_0, tibble(INNING=floor_I_per_N, CUM_RUNS=ceil_R_per_N), type="response") +
#           h1*(1-h2)*predict(model_f_0, tibble(INNING=ceil_I_per_N, CUM_RUNS=floor_R_per_N), type="response") +
#           h1*h2*predict(model_f_0, tibble(INNING=ceil_I_per_N, CUM_RUNS=ceil_R_per_N), type="response"),
#     AW = N*AW0
#   ) %>% relocate(AW, .before=R_)
# df_AW
# 
# df_AW1 = df_AW %>% select(-c(floor_R_per_N, floor_I_per_N ,ceil_R_per_N, ceil_I_per_N,    h1,    h2,   AW0))
# df_AW1

# hist(df_AW$AW)
# df_AW%>%filter(PIT_NAME=="Justin Verlander")
# df_AW%>%filter(PIT_NAME=="Jacob deGrom")
# 
# ###
# write_csv(df_AW1, "df_AW1.csv")



