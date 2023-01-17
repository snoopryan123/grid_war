#!/bin/bash
for i in {1..1}
do
   mv ./job_output/fit_sim2-$i.rds            ../../Dropbox/HPCC/fit_sim2-$i.rds  
   mv ./job_output/y_sim2-$i.rds              ../../Dropbox/HPCC/y_sim2-$i.rds  
   mv ./job_output/params_true_sim2-$i.rds    ../../Dropbox/HPCC/params_true_sim2-$i.rds  
   boxup
   mv ../../Dropbox/HPCC/fit_sim2-$i.rds            ./job_output/fit_sim2-$i.rds 
   mv ../../Dropbox/HPCC/y_sim2-$i.rds              ./job_output/y_sim2-$i.rds 
   mv ../../Dropbox/HPCC/params_true_sim2-$i.rds    ./job_output/params_true_sim2-$i.rds  
done

