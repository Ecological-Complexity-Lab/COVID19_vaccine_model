#! /bin/bash

#$ -q shai.q
#$ -cwd
#$ -N COVD19_IL

## -l h_vmem=2G

Rscript parse_sim_results.R $1 $2
