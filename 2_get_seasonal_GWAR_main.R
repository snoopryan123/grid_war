
source("0_load_stuff.R")

w_rep = 0.423769

#######################################
### load f,g grids and other models ###
#######################################

# f_lrm <- readRDS("f_lrm.rds") 
# g_grid <- read.csv("g_grid.csv",row.names = 1, header= TRUE)

model_f = load_lm("model_f.rds")
model_f_0 = load_lm("model_f_0.rds")
model_g = read.csv("model_g.csv", header=T, row.names=1)

### Park Effects
park_fx_name = "ridge_PF" #FIXME

df_ridge_PF = read_csv("1e_park_fx/obs_ridge_PF.csv") %>% select(PARK, park_factor) %>% mutate(name = "ridge")
df_fg_PF = read_csv("1e_park_fx/obs_fg_PF.csv") %>% select(PARK, park_factor) %>% mutate(name = "fg")
df_espn_PF = read_csv("1e_park_fx/obs_espn_PF.csv") %>% select(PARK, park_factor) %>% mutate(name = "espn")
df_park_fx = bind_rows(df_ridge_PF, df_fg_PF, df_espn_PF)

#################
### load data ###
#################

war2_ogg = read_csv("war2.csv")
war2_og <- war2_ogg %>% 
  filter(SP_IND | lag(SP_IND, default=FALSE)) %>%
  # left_join(park_fx_df) %>%
  # mutate(park_factor = replace_na(park_factor, 0)) %>%
  group_by(INNING, BAT_HOME_IND) %>%
  mutate(
    INN_SITCH_after = lead(INN_SITCH, default="0 000")
  ) %>%
  ungroup()

##################################
### grid abstraction functions ###
##################################

# start = identifying pitchers' first at-bat in an inning
# final = identifying pitchers' last at-bat
# pit_occurence = identifying every pitchers' number at-bat
# i = inning, j = base-out-state, k = accumulated runs so far, w = tracker (Rest of inning runs)

MAX_INNING_RUNS = 10  
f <- function(i,r,alpha, home,lg,yr, parkFx=TRUE,adjustConfounders=TRUE) {
  ### i == inning, r == runs, alpha == park effect
  ### confounders:  home, league, year
  
  r = ifelse(r+1 >= MAX_INNING_RUNS, MAX_INNING_RUNS, r)
  f_lrm = if (adjustConfounders) model_f else model_f_0
  
  if (parkFx) {
    ### park adjustment...
    r.minus.1 = ifelse(r > 0, r-1, 0)
    r.plus.1 = ifelse(r < MAX_INNING_RUNS, r+1, MAX_INNING_RUNS)
    f_ir.minus.1 = predict(f_lrm, 
                           tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r.minus.1), 
                           type="response")[[1]]
    f_ir = predict(f_lrm, 
                   tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r), 
                   type="response")[[1]]
    f_ir.plus.1 = predict(f_lrm, 
                          tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r.plus.1), 
                          type="response")[[1]]
    h = abs(alpha)*i
    h = ifelse(h > 1, 1, h)
    h = ifelse(h < 0, 0, h)
    ifelse(alpha < 0 & r < MAX_INNING_RUNS,   (1-h)*f_ir + h*f_ir.plus.1,
           ifelse(alpha > 0 & r > 0,                 (1-h)*f_ir + h*f_ir.minus.1,
                  ifelse(alpha > 0,                         (1+h)*f_ir - h*f_ir.plus.1,
                         (1+h)*f_ir - h*f_ir.minus.1 # alpha < 0
                  )))
  } else { ### no park effects
    f_ir = predict(f_lrm, 
                   tibble(BAT_HOME_IND = home, PIT_LEAGUE = lg, YEAR = yr, INNING = i, CUM_RUNS = r), 
                   type="response")[[1]]
    f_ir
  }
}
# f(1,3,0.05, 1,"AL",2019)
# f(1,3,-0.05, 1,"AL",2019)
# f(1,3,0, 1,"AL",2019)
# f(1,3,-0.05, 1,"AL",2019,parkFx=F)
# f(1,3,-0.05, 1,"AL",2019,parkFx=F,adjustConfounders=F)

g <- function(outs_base_state, r) {
  ### g(r|S,O) is the probability of allowing R runs through the rest of the inning if
  ### pitcher exits the inning with O outs and base-state S, where outs_base_state == (S,O)
  r = ifelse(r+1 >= MAX_INNING_RUNS, MAX_INNING_RUNS, r)
  model_g[outs_base_state, r+1]
}
# g(1,0)
# g(24,5)

##################################################
### compute Grid Wins (GW) and Grid WAR (GWAR) ###
##################################################

get_grid_wins_moi <- function(i,r,alpha, home,lg,yr, parkFx=TRUE,adjustConfounders=TRUE, inn_sitch) {
  getgw <- function(w) {
    fw = f(i=i, r=r+w, alpha=alpha, 
           home=home, lg=lg, yr=yr, 
           parkFx=parkFx, adjustConfounders=adjustConfounders) 
    gw = g(inn_sitch, r=w)
    fw*gw
  }
  sum(sapply(0:MAX_INNING_RUNS, FUN = getgw))
}

# f(5,5,0, 1,"AL",2019)
# get_grid_wins_moi(5,5,0, 1,"AL",2019, inn_sitch="2 000")


get_grid_wins <- function(pbp_df, years, parkFx=FALSE, adjustConfounders=TRUE) {
  df = pbp_df %>% filter(YEAR %in% years)
  
  if (!isFALSE(parkFx)) { ### use park factors with name `parkFx`
    print(paste0("computing GWAR with ", parkFx, " park factors"))
    df = df %>%
      left_join(df_park_fx %>% filter(name == parkFx)) %>%
      mutate(park_factor = replace_na(park_factor, 0)) 
    parkFx = TRUE
  } else {
    print(paste0("computing GWAR without park factors"))
  }
  
  result = df %>%
    rowwise() %>%
    mutate(
      Grid_Wins_eoi = ifelse(
        exit_at_end_of_inning == 0,
        0,
        f(i=INNING, r=CUM_RUNS, alpha=park_factor, 
          home=BAT_HOME_IND, lg=PIT_LEAGUE, yr=YEAR, 
          parkFx=parkFx, adjustConfounders=adjustConfounders) 
      )
    )
  result
  
  result = result %>%
    rowwise() %>%
    mutate(
      Grid_Wins_moi = ifelse(
        exit_in_middle == 0,
        0,
        get_grid_wins_moi(
          i=INNING, r=CUM_RUNS, alpha=park_factor,  ##r=lead(CUM_RUNS, default=0)
          home=BAT_HOME_IND, lg=PIT_LEAGUE, yr=YEAR, 
          parkFx=parkFx, adjustConfounders=adjustConfounders, 
          inn_sitch=INN_SITCH_after
        )
      )
    )
  
  result %>% ungroup()
}

get_pitcher_exits <- function(Grid_Wins_df, war=FALSE) {
  pitcher_exits <- Grid_Wins_df %>% 
    filter(exit_in_middle == 1 | exit_at_end_of_inning == 1) %>%
    mutate(GW = Grid_Wins_eoi + Grid_Wins_moi) ### Grid Wins in a game
  if (war) {
    pitcher_exits <- pitcher_exits %>%
      mutate(GWAR = GW - w_rep)
  }
  # pitcher_exits %>% drop_na(GW)
  pitcher_exits
}

get_pitcher_exits_shortened <- function(pitcher_exits_df) {
  pitcher_exits_df %>% select_if(names(.) %in% c(
    "PIT_NAME", "GAME_ID", "YEAR", "GW", "GWAR", "INNING", "CUM_RUNS",
     "exit_at_end_of_inning", "exit_in_middle", "BASE_STATE", "OUTS_CT",
     "BAT_HOME_IND", "PIT_LEAGUE", "park_factor"
  )) %>% arrange(PIT_NAME,GAME_ID)
}

get_seasonal_war <- function(pitcher_exits_df) {
  pitcher_exits_df %>%
    group_by(PIT_NAME,YEAR) %>%
    summarise(
      GWAR = sum(GW - w_rep, na.rm=T),
      N = n(),
      GW = sum(GW, na.rm=T),
      w_rep = w_rep[1],
    ) %>%
    ungroup() 
}


