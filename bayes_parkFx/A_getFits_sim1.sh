#!/bin/bash
for i in {22..22}
do
   mv ./job_output/fit_sim1-$i.rds            ../../../Dropbox/HPCC/fit_sim1-$i.rds  
   mv ./job_output/y_sim1-$i.rds              ../../../Dropbox/HPCC/y_sim1-$i.rds  
   mv ./job_output/params_true_sim1-$i.rds    ../../../Dropbox/HPCC/params_true_sim1-$i.rds  
   boxup
   mv ../../../Dropbox/HPCC/fit_sim1-$i.rds            ./job_output/fit_sim1-$i.rds 
   mv ../../../Dropbox/HPCC/y_sim1-$i.rds              ./job_output/y_sim1-$i.rds 
   mv ../../../Dropbox/HPCC/params_true_sim1-$i.rds    ./job_output/params_true_sim1-$i.rds  
done

