
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
write.csv(as.data.frame(expected_runs_SO), "df_expected_runs_SO.csv")

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

df_endOfGame1 = df_endOfGame %>%
  group_by(PIT_NAME, YEAR) %>%
  summarise(
    R = sum(R_),
    I = sum(INNING),
    N = n(),
    W = sum(PIT_WINS),
    .groups="drop"
  ) %>%
  mutate(A = R/I*N)
df_endOfGame1

### save AWAR model and dataframe
awar_model = lm(W ~ A + 0, data=df_endOfGame1)
awar_model
# saveRDS(awar_model, file = "model_a.rds")
save_lm(awar_model, "model_a.rds")


df_endOfGame1$W_hat_A = predict(awar_model, df_endOfGame1)
write_csv(df_endOfGame1, "df_a_grid.csv")

###
plot_A = df_endOfGame1 %>%
  ggplot(aes(x=A, y=W)) +
  geom_point() +
  geom_abline(intercept = 0, slope = awar_model$coefficients[1],
              color="firebrick", linewidth=1)
plot_A
ggsave("plots/plot_A.png", plot_A, width=8, height=7)



