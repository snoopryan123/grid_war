# Introducing `Grid War`: Rethinking WAR for Starting Pitchers
## `Grid WAR` Code

• Get dataset from dropbox
	`retro_final_PA_1990-2000d.csv`
	https://upenn.box.com/v/retrosheet-pa-1990-2000
• Run `1_create_grids.R`
	output: f_lrm.rds, g_grid.csv, war2.csv
• Run `2_get_seasonal_GWAR.R`
	output: pitcher_exits_2019.csv, GWAR_2019.csv
• From Fangraphs.com, get FanGraphsLeaderboard_2019.csv
• Run `3_Fangraphs_WAR_Comp.R`
	output: plots

