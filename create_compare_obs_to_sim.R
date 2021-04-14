#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")

library(tidyverse)

original <- read_lines('main_hpc.R')
str_detect(original, '# yinit[29] <- 1 # For comparing to empirical data')
loc <- which(original=="yinit[iindex] <- round(active_infected*Table_1$prop_infected_total)")
original[loc+1] <- "yinit[29] <- 1 # For comparing to empirical data"

loc <- which(original=="vaccine_forcing <- set_forcing(effect = vacc_eff, effect_time = times)")
original[loc+1] <- "k_range_percent <- 0; SD_list <- 0; strat_ls <- list(list(vto='elderly_adults',SD_ls=SD_list,from=all_ages,to=all_ages))"

# Set the arguments
original[19] <- "e_id <- 40; JOB_ID <- 40; max_weeks=15"
original[11:18] <- ""

write_lines(original, 'main_hpc_compare_empirical_simulation.R')

