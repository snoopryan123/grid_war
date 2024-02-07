
source("../0_load_stuff.R")

library(gt)
library(gtExtras)

##########################################
### best pitcher careers by total GWAR ###
##########################################

df1 = read_csv("best_pit_careers_total_gwar.csv")

plot1 = df1 %>%
  arrange(-GWAR) %>%
  filter(row_number() <= 15) %>%
  mutate(`#` = 1:n()) %>%
  relocate(`#`, .before = Pitcher) %>%
  gt()  %>%
  data_color(
    columns = GWAR,
    colors = scales::col_numeric(
      palette = c("red", "green"),
      domain = c(0,140)
    )
  )
plot1
gtsave_extra(
  plot1, "plot_best_pit_careers_total_gwar.png",
  zoom=10
)
        
###############################################
### best pitcher careers by GWAR efficiency ###
###############################################

df2 = read_csv("best_pit_careers_gwar_per_game_min100.csv")

plot2 = df2 %>%
  arrange(-`GWAR per start`) %>%
  filter(row_number() <= 15) %>%
  mutate(`#` = 1:n()) %>%
  relocate(`#`, .before = Pitcher) %>%
  gt()  %>%
  data_color(
    columns = `GWAR per start`,
    colors = scales::col_numeric(
      palette = c("red", "green"),
      domain = c(0.1,0.21)
    )
  )
plot2
gtsave_extra(
  plot2, "plot_best_pit_careers_gwar_per_game_min100.png",
  zoom=10
)

##########################################
### best pitcher seasons by total GWAR ###
##########################################

df3 = read_csv("best_pit_seasons_total_gwar.csv")

plot3 = df3 %>%
  select(-starts_with("FWAR")) %>%
  arrange(-GWAR) %>%
  filter(row_number() <= 15) %>%
  mutate(`#` = 1:n()) %>%
  relocate(`#`, .before = Year) %>%
  gt()  %>%
  data_color(
    columns = GWAR,
    colors = scales::col_numeric(
      palette = c("red", "green"),
      domain = c(0,12)
    )
  )
plot3
gtsave_extra(
  plot3, "plot_best_pit_seasons_total_gwar.png",
  zoom=10
)

###############################################
### best pitcher seasons by GWAR efficiency ###
###############################################

df4 = read_csv("best_pit_seasons_gwar_per_game_min25.csv")

plot4 = df4 %>%
  select(-starts_with("FWAR")) %>%
  arrange(-`GWAR per start`) %>%
  filter(row_number() <= 15) %>%
  mutate(`#` = 1:n()) %>%
  relocate(`#`, .before = Year) %>%
  gt()  %>%
  data_color(
    columns = `GWAR per start`,
    colors = scales::col_numeric(
      palette = c("red", "green"),
      domain = c(0.1,0.36)
    )
  )
plot4
gtsave_extra(
  plot4, "plot_best_pit_seasons_gwar_per_game_min25.png",
  zoom=10
)

######################################
### best pitcher 4 year peak + HOF ###
######################################

df5 = read_csv("best_pit_4_yr_peak_HOF_Adi.csv")

plot5 = df5 %>%
  arrange(`GEO MEAN  RANK`) %>%
  rename(Pitcher = PIT_NAME) %>%
  rename(`Rank Career` = `RANK CAREER`) %>%
  rename(`Rank Peak` = `RANK PEAK`) %>%
  rename(`Geo Mean Rank` = `GEO MEAN  RANK`) %>%
  mutate(HOF = replace_na(HOF, "")) %>%
  filter(row_number() <= 30) %>%
  mutate(`#` = 1:n()) %>%
  relocate(`#`, .before = Pitcher) %>%
  gt()  %>%
  data_color(
    columns = `Geo Mean Rank`,
    colors = scales::col_numeric(
      palette = c("green", "red"),
      domain = c(1,60)
    )
  )
plot5
gtsave_extra(
  plot5, "plot_best_pit_4_yr_peak_HOF_Adi.png",
  zoom=10
)




