library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=16))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = "./plots/"

#############################
########### GRIDS ###########
#############################

f_grid <- read.csv("f_grid.csv",row.names = 1, header= TRUE)
g_grid <- read.csv("g_grid.csv",row.names = 1, header= TRUE)

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
    theme_solarized() +
    geom_point() + 
    geom_line(size=1) +
    # geom_smooth(se=FALSE) +
    labs(title=TeX("$f(I,R)$ as a function of $R$, for each $I$"),
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
    theme_solarized() +
    geom_point() + 
    # geom_line(size=1) +
    geom_smooth(se=FALSE) +
    labs(title=TeX("smoothed $f(I,R)$ as a function of $R$, for each $I$"),
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
    theme_solarized() +
    geom_point() + 
    geom_line(size=1) +
    # geom_smooth(se=FALSE) +
    labs(title=TeX("$f(I,R)$ as a function of $R$, for each $I$"),
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
    theme_solarized() +
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


