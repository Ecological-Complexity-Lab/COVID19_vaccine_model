#! /bin/bash

#$ -q shai.q
#$ -cwd
#$ -N IL

#-l h_vmem=2G

Rscript main_hpc.R $1 
# $1 := experiment id
