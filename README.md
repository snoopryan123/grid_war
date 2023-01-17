# Introducing `Grid War`: Rethinking WAR for Starting Pitchers
## `Grid WAR` Code

* Get dataset from dropbox
	`retro_final_PA_1990-2000d.csv`
	https://upenn.box.com/v/retrosheet-pa-1990-2000
* Run `1a_create_war2.R`
	output: `war2.csv`
* Run `1b_create_fg_grids.R`
	output: `model_f.rds` and `model_f_0.rds` and `model_g.csv`
* Run `1c_create_a_model.R`
	output: `df_a_grid.csv` and `model_a.rds` and `expected_runs_SO.csv`
* Run `2_get_seasonal_GWAR.R`
	output: pitcher_exits_2019_withoutParkFx.csv, GWAR_2019_withoutParkFx.csv
* From Fangraphs (link below), get FanGraphsLeaderboard_2019.csv
https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=y&type=8&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=
* Run (the relevant code blocks) from `3_Fangraphs_WAR_Comp.R`
	output: plots

