library(tidyverse)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)
library(gridExtra)
library(gt)
library(rlist)
library(xgboost)

# theme_set(theme_bw())
# theme_update(text = element_text(size=18))
# theme_update(plot.title = element_text(hjust = 0.5))
# # theme_set(theme_solarized())

### plotting pre-sets
theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20),
  panel.spacing = unit(2, "lines")
) 
output_folder = "./plots/"

rmse <- function(x,y) { sqrt(mean( (x-y)**2 ))}

logloss <- function(y,p) { 
  p = ifelse(p == 0, 1e-15, p)
  -mean( y*log(p) + (1-y)*log(1-p) ) 
}

df_to_png <- function(df, filename) {
  png(filename, height = 50*nrow(df), width = 200*ncol(df))
  grid.table(df)
  dev.off()
}

clean_lm <- function(cm) {
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  cm$model = c()
  
  cm
}

save_lm <- function(cm, filename) {
  cm = clean_lm(cm)
  attr(cm$terms, ".Environment") <- NULL
  attr(cm$formula,".Environment") <- NULL
  saveRDS(cm, filename)
}

load_lm <- function(filename) {
  cm = readRDS(filename)
  attr(cm$terms, ".Environment") <- globalenv()
  if ( any(str_detect(class(cm), "glm")) ) {
    attr(cm$formula, ".Environment") <- globalenv()
  }
  return(cm)
}


