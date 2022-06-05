library(tidyverse)
output_folder = './job_output/'
idx = 2 #FIXME
OUTPUT_FILE = paste0("sim1-",idx)


source("sim1_main.R")
fit = fit_model(model1a) ### 7000 sec / 1000 iters
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
saveRDS(params_true, file = paste0(output_folder, "params_true_", OUTPUT_FILE, ".rds"))


############ examine fit ############ 

# fit = readRDS("job_output/10k_its/fit_sim1-2.rds")
# params_true = readRDS("job_output/10k_its/params_true_sim1-2.rds")
# 
# draws = as.matrix(fit)
# beta_p_draws = draws[,str_detect(colnames(draws), "beta_p")]
# colMeans(beta_p_draws)
# params_true$beta_p


