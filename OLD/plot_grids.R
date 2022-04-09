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

#############################
########### GRIDS ###########
#############################

f_grid <- read.csv("f_grid.csv",row.names = 1, header= TRUE)
g_grid <- read_csv("g_grid.csv") #read.csv("g_grid.csv",row.names = 1, header= TRUE)
names(g_grid) = c("c", paste0("r", 0:(ncol(g_grid)-2)) )

#############################
########## f PLOTS ##########
#############################

### f(I,R) as a function of R, for each inning I
{
  WPi = as_tibble(t(f_grid))
  colnames(WPi) = paste0("inn",1:9)
  WPii = stack(WPi) 
  WPii$runs = rep(0:(nrow(WPi)-1), 9)
  pWPii = WPii %>% filter(runs <= 13) %>%
    ggplot(aes(x=runs,y=values,color=ind)) + 
    geom_point() + 
    geom_line(size=1) +
    # geom_smooth(se=FALSE) +
    labs(
      # title=TeX("$f(I,R)$ as a function of $R$, for each $I$"),
      y="context-neutral win probability",
      x="runs allowed through the end of the given inning")
  pWPii
  # ggsave(paste0(output_folder,"plot_fIR_R.png"), pWPii)
  # plotly::ggplotly(pWPii)
}

### smoothed f(I,R) as a function of R, for each inning I
{
  pWPiis = WPii %>% filter(runs <= 13) %>%
    ggplot(aes(x=runs,y=values,color=ind)) + 
    geom_point() + 
    # geom_line(size=1) +
    geom_smooth(se=FALSE) +
    labs(
      # title=TeX("smoothed $f(I,R)$ as a function of $R$, for each $I$"),
      y="context-neutral win probability",
      x="runs allowed through the end of the given inning")
  pWPiis
  # ggsave(paste0(output_folder,"plot_fIR_R_smoothed.png"), pWPiis)
}

### f(I,R) as a function of I, for each R
{
  WPr = as_tibble(f_grid)
  colnames(WPr) = paste0("runs",0:(ncol(f_grid)-1))
  WPrr = stack(WPr[,1:14]) 
  WPrr$inn = rep(1:9, 14)
  pWPrr = WPrr %>% 
    ggplot(aes(x=inn,y=values,color=ind)) +
    geom_point() + 
    geom_line(size=1) +
    # geom_smooth(se=FALSE) +
    labs(
      title=TeX("$f(I,R)$ as a function of $R$, for each $I$"),
         y="context-neutral win probability",
         x="runs allowed through the end of the given inning")
  pWPrr
  # ggsave(paste0(output_folder,"plot_fIR_I.png"), pWPrr)
  # plotly::ggplotly(pWPrr)
}

### smoothed f(I,R) as a function of I, for each R
{
  pWPrrs = WPrr %>% 
    ggplot(aes(x=inn,y=values,color=ind)) +
    geom_point() + 
    # geom_line(size=1) +
    geom_smooth(se=FALSE) +
    labs(title=TeX("$f(I,R)$ as a function of $R$, for each $I$"),
         y="context-neutral win probability",
         x="runs allowed through the end of the given inning")
  pWPrrs
  # ggsave(paste0(output_folder,"plot_fIR_I_smoothed.png"), pWPrrs)
  # plotly::ggplotly(pWPrrs)
}


#############################
########## g PLOTS ##########
#############################

### plot g(R,S,O) as a function of R, with O = 0, for different base states S

plot_gRSO <- function(O) {
  RP0 = stack(g_grid[8*O + 1:8,2:ncol(g_grid)])
  RP0$S = g_grid$c[8*O + 1:8]
  RP0$r = as.numeric(str_sub( RP0$ind,2))
  pg0 = RP0 %>% ggplot(aes(color=S,x=r,y=values)) +
    geom_point() +
    geom_line(size=1) +
    labs(
      # title=paste0("g(R|S,O=",O,") as a function of R, for different base states S"),
      x="runs allowed R from now until the end of this half inning",
      y="context-neutral probability")
  pg0
}

{
  pg0 = plot_gRSO(0)
  pg0
  pg1 = plot_gRSO(1)
  pg1
  pg2 = plot_gRSO(2)
  pg2
  # ggsave(paste0(output_folder,"plot_gRSO_R0.png"), pg0)
  # ggsave(paste0(output_folder,"plot_gRSO_R1.png"), pg1)
  # ggsave(paste0(output_folder,"plot_gRSO_R2.png"), pg2)
}


