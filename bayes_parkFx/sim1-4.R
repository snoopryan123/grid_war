library(tidyverse)
output_folder = './job_output/'
SEED = 4 #FIXME
OUTPUT_FILE = paste0("sim1-",SEED)


source("sim1_main")
fit1 = fit_model1()
saveRDS(fit1, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
saveRDS(params_true, file = paste0(output_folder, "params_true_", OUTPUT_FILE, ".rds"))
