#! /bin/bash

#$ -q shai.q
#$ -cwd
#$ -N COVD19_IL

#-l h_vmem=2G

Rscript main_hpc.R Israel 30 0.4 7 5 1.5 0.92
