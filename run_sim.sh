#! /bin/bash

#$ -q shai.q
#$ -cwd
#$ -N COVD19_IL

#-l h_vmem=2G

Rscript main_hpc.R Israel 50 0.4 7 3 2.1 1.5 0.95 0.24 $1 $2
# $1 := prop accepting vaccines
# $2 := comment