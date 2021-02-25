#! /bin/bash

#$ -q shai.q
#$ -cwd
#$ -N COVD19_IL

#-l h_vmem=2G

Rscript main_hpc.R Israel 30 0.4 7 6.4 1.5
