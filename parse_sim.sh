#! /bin/bash

#$ -q shai.q
#$ -cwd
#$ -N ParseRes

## -l h_vmem=1G

Rscript parse_sim_results.R $1 $2
