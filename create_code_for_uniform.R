#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")

library(tidyverse)
original <- read_lines('main_hpc.R')
str_detect(original, 'h <- Table_1$h')
loc <- which(original=="h <- Table_1$h # probability of hospitalization")
original[loc] <- "h <- rep(mean(Table_1$h), 9)"
write_lines(original, 'main_hpc_uniform_h.R')