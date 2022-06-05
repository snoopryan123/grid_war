#!/bin/bash
#$ -N AJ_sim1
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: 1,4,50,90
#$ -pe openmp 15

## ARRAY JOB
#$ -t 2-2
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla sim1-$SGE_TASK_ID.R