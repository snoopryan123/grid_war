library(tidyverse)
output_folder = './job_output/'
idx = 1 #FIXME
OUTPUT_FILE = paste0("sim1-",idx)


source("sim1_main.R")
fit = fit_model(model1)
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
saveRDS(params_true, file = paste0(output_folder, "params_true_", OUTPUT_FILE, ".rds"))
