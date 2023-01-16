library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
# theme_set(theme_solarized())
output_folder = "./plots/"

war2 = read_csv("war2.csv")

##################################
#### Create AWAR grid: a(I,R) ####
##################################

### expected runs to the end of the inning as a function of base-state S and outs O
P = read.csv("g_grid.csv", row.names = 1, header= TRUE)
expected_runs_SO = matrix(nrow=nrow(P), ncol=1)
rownames(expected_runs_SO) = rownames(P)
colnames(expected_runs_SO) = "expected_runs"
for (so in 1:nrow(P)) {
  r_ = 0:(ncol(P)-1)
  expected_runs_SO[so,1] = sum(P[so,] * r_)
}
write.csv(as.data.frame(expected_runs_SO), "df_expected_runs_SO.csv")

get_expected_runs_toEndOfInning <- function(S,O) {
  ### base-state S, outs O
  expected_runs_SO[paste0(O," ",S),]
}
# get_expected_runs_toEndOfInning("001",2)

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
saveRDS(awar_model, file = "model_AWAR.rds")

df_endOfGame1$W_hat_A = predict(awar_model, df_endOfGame1)
write_csv(df_endOfGame1, "df_pre_AWAR.csv")

###
plot_AWAR = df_endOfGame1 %>%
  ggplot(aes(x=A, y=W)) +
  geom_point() +
  geom_abline(intercept = 0, slope = awar_model$coefficients[1],
              color="firebrick", linewidth=1)
plot_AWAR
ggsave("plots/plot_AWAR.png", plot_AWAR, width=8, height=7)



