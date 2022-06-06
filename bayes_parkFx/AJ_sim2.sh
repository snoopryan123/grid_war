#!/bin/bash
#$ -N AJ_sim2
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: 1,4,50,90
#$ -pe openmp 8

## ARRAY JOB
#$ -t 1-1
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla sim2-$SGE_TASK_ID.R