library(tidyverse)
output_folder = './job_output/'
idx = 11 #FIXME
OUTPUT_FILE = paste0("sim2-",idx)

source("sim2_main.R")
fit = fit_model(model2) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
saveRDS(params_true, file = paste0(output_folder, "params_true_", OUTPUT_FILE, ".rds"))



############ examine fit ############ 

# fit = readRDS("job_output/sim2_15k_its/fit_sim2-1.rds")
# params_true = readRDS("job_output/sim2_15k_its/params_true_sim2-1.rds")
fit = readRDS("job_output/sim2a/fit_sim2-11.rds")
params_true = readRDS("job_output/sim2a/params_true_sim2-11.rds")

draws = as.matrix(fit)
beta_p_draws = draws[,str_detect(colnames(draws), "beta_p")]
colMeans(beta_p_draws)
params_true$beta_p


