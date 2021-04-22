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


# Plot observed vs model data -------------------------------------------

# Compare fit to Israeli data. We first ran manually a simulation with a single
# infected individual, no SD and no vaccination. We arbitrartily and manually
# gave it some faux JOB_ID.
#
# To run this part of the code you first need to run the file empirical_data.R
#
# pdf('beta_fitting.pdf', 6, 6)
read_csv('40_results_Israel.csv') %>% 
  filter(SD==0, k_percent==0, from=='juveniles_adults_elderly', vto=='elderly_adults') %>% 
  filter(time<=35) %>%
  filter(state == 'I') %>%
  group_by(time, state) %>% 
  summarise(cases=sum(cases)) %>% 
  dplyr::select(running_day=time, cases) %>% 
  mutate(Source='Model') %>% 
  bind_rows(data_early %>% dplyr::select(running_day, cases) %>%  mutate(Source='Data')) %>% 
  bind_rows(data_early %>% dplyr::select(running_day, cases=pred_lm) %>%  mutate(Source='Fit')) %>% 
  ggplot(aes(running_day, cases, color=Source))+
  geom_line(size=1.1)+
  geom_point(size=3)+
  scale_color_manual(values=c('purple','blue','black'))+
  scale_x_continuous(n.breaks = 6)+
  scale_y_continuous(n.breaks = 6)+
  theme_bw()+
  labs(x='Day', y='Infected cases')+
  theme(panel.grid = element_blank(),
        legend.position = c(0.2,0.8),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
# dev.off()
