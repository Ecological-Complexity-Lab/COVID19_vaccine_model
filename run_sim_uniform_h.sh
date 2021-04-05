#! /bin/bash

#$ -q shai.q
#$ -cwd
#$ -N unif_h

#-l h_vmem=2G
Rscript create_code_for_uniform.R
Rscript main_hpc_uniform_h.R $1 
rm -f main_hpc_uniform_h.R
# $1 := experiment id
