# Introducing `Grid War`: Rethinking WAR for Starting Pitchers
## `Grid WAR` Code

* Get dataset from dropbox
	`retro_final_PA_1990-2000d.csv`
	https://upenn.box.com/v/retrosheet-pa-1990-2000
* Run `1_create_grids.R` and then `1a_create_AW.R`
	creates the f,g grids for grid war (`f_lrm.rds`, `g_grid.csv`)
	creates the model `model_AWAR.rds` for AWAR and the dataframe `df_pre_AWAR.csv` for pitcher pre-AWAR stats
	creates the dataset `war2.csv` of pitcher stats relevant to GWAR
* Run `2_get_seasonal_GWAR.R`
	output: pitcher_exits_2019_withoutParkFx.csv, GWAR_2019_withoutParkFx.csv
* From Fangraphs (link below), get FanGraphsLeaderboard_2019.csv
https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=y&type=8&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=
* Run (the relevant code blocks) from `3_Fangraphs_WAR_Comp.R`
	output: plots

