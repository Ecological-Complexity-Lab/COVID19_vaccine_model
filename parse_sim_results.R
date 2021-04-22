# Add 2 more targeted SD scenarios
# Analyze total hospitalizations.

#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")
print(sessionInfo())

library(magrittr)
library(tidyverse)
library(socialmixr)

# args <- c(8299551, 15)

if (length(commandArgs(trailingOnly=TRUE))==0) {
  stop('No arguments were found!')
} else {
  args <- commandArgs(trailingOnly=TRUE)
  JOB_ID <- as.numeric(args[1])
  max_weeks <- as.numeric(args[2])
}

# Get parameters of that run
run_summary <- read.csv(paste(JOB_ID,'run_summary.csv',sep='_'))
for (i in 1:nrow(run_summary)){
  v <- run_summary[i,1]
  if(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", run_summary[i,2])){
    assign(v, as.numeric(run_summary[i,2]))
    # print(paste(v,as.numeric(run_summary[i,2]),'numeric'))
  } else {
    assign(v, run_summary[i,2])
    # print(paste(v,run_summary[i,2],'not numeric'))
  }
}

# Get data
country_tbl <- read_csv(paste(JOB_ID,'_results_',current_country,'.csv',sep=''))
k_percent_max <- max(country_tbl$k_percent)
k_range_percent <- unique(country_tbl$k_percent)


# This can be used if subsetting the data to less than the max time model was run.
# country_tbl %<>% filter(time<=7*max_weeks)
# max_time <- country_tbl %>% slice(which.max(time))
# times <- 1:max_time$time

# Or this can be used if analyzing all the times the model was run:
max_time <- 7*max_weeks
times <- 1:max(country_tbl$time)

source('functions.R')
source('initialize_country.R')
N_age_groups <- N*age_structure$Proportion

# Compre control to vaccine dynamics across ages --------------------------
control <- 
country_tbl %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(vto=='elderly_adults') %>% 
  filter(SD==0) %>% 
  filter(k_percent==0) %>% 
  mutate(Proportion=cases/N) %>% 
  mutate(grp='Control') %>% 
  mutate(col=factor(col, levels = state_colors$col))

# Epidmeiological curves for control
pdf(plotname('SI_epidemiological_curves_control.pdf'), 10, 8)
ggplot(control %>% filter(state!='V'), aes(x=time, y=Proportion, color=col))+
  geom_line(size=1.2)+
  facet_wrap(~age_group, scales = 'free')+
  labs(y='Proportion of the population', x='Day', color='State', linetype='Group')+
  scale_color_identity(guide = 'legend', labels=state_colors$state)+
  scale_x_continuous(limits = c(0,max_time))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
dev.off()

vaccine <- 
  country_tbl %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(vto=='elderly_adults') %>% 
  filter(SD==0) %>% 
  filter(k_percent==k_percent_max) %>% 
  mutate(Proportion=cases/N) %>% 
  mutate(grp='Vaccine') %>% 
  mutate(col=factor(col, levels = state_colors$col))

# Epidmeiological curves for vaccine
pdf(plotname('SI_epidemiological_curves_vaccine.pdf'), 10, 8)
ggplot(vaccine, aes(x=time, y=Proportion, color=col))+
  geom_line(size=1.2)+
  facet_wrap(~age_group, scales = 'free')+
  labs(y='Proportion of the population', x='Day', color='State', linetype='Group')+
  scale_color_identity(guide = 'legend', labels=state_colors$state)+
  scale_x_continuous(limits = c(0,max_time))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
dev.off()

pdf(plotname('MT_epidemiological_curves_vaccine.pdf'), 10, 5)
country_tbl %>% 
  filter(SD==0) %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(vto=='elderly_adults') %>% 
  filter(age_group %in% c('0-9','30-39','60-69')) %>% 
  filter(k_percent == k_percent_max) %>%
  mutate(Proportion=cases/N) %>% 
  mutate(grp='Vaccine') %>% 
  mutate(col=factor(col, levels = state_colors$col)) %>% 
ggplot(aes(x=time, y=Proportion, color=col))+
  geom_line(size=1.2)+
  facet_wrap(~age_group)+
  labs(y='Proportion of the population', x='Day', color='State', linetype='Group')+
  scale_color_identity(guide = 'legend', labels=state_colors$state)+
  scale_x_continuous(limits = c(0,max_time))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
dev.off()

pdf(plotname('SI_IHV_dynamics_ctrl_vs_vaccine.pdf'), 10, 8)
bind_rows(control, vaccine) %>% 
  mutate(grp=factor(grp, levels = c('Vaccine','Control'))) %>% 
  filter(state %in% c('I','H','V')) %>% 
  ggplot(aes(x=time, y=Proportion, linetype=grp, color=col))+
  geom_line(size=1.2)+
  facet_wrap(~age_group, scales = 'free')+
  labs(y='Proportion of the population', x='Day', color='State', linetype='Group')+
  scale_color_identity(guide = 'legend', labels=c('I','H','V'))+
  scale_x_continuous(limits = c(0,max_time))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
dev.off()

# Dynamics comparison of two vaccination strategies with uniform SD -----------------------
vaccine_ea <-
  country_tbl %>%
  # filter (time<=7*12) %>%  #Plot 12 weeks for convenience
  filter(from=='juveniles_adults_elderly') %>%
  filter(vto=='elderly_adults') %>%
  filter(SD==0) %>%
  filter(k_percent==k_percent_max) %>%
  mutate(Proportion=cases/N) %>%
  mutate(grp='Elderly first')
vaccine_ae <-
  country_tbl %>%
  # filter (time<=7*12) %>%  #Plot 12 weeks for convenience
  filter(from=='juveniles_adults_elderly') %>%
  filter(vto=='adults_elderly') %>%
  filter(SD==0) %>%
  filter(k_percent==k_percent_max) %>%
  mutate(Proportion=cases/N) %>%
  mutate(grp='Adults first')

pdf(plotname('SI_IHV_dynamics_ea_vs_ae.pdf'), 10, 8)
bind_rows(vaccine_ae, vaccine_ea) %>%
  mutate(grp=factor(grp, levels = c('Adults first','Elderly first'))) %>%
  filter(state %in% c('I','H','V')) %>%
  ggplot(aes(x=time, y=Proportion, linetype=grp, color=col))+
  geom_line(size=1.2)+
  facet_wrap(~age_group, scales = 'free')+
  labs(y='Proportion of the population', x='Day', color='State', linetype='Vaccine strategy')+
  scale_color_identity(guide = 'legend', labels=c('I','H','V'))+
  scale_x_continuous(limits = c(0,max_time))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=14, color = 'black'),
        axis.text = element_text(size=14, color = 'black'))
dev.off()


# Targeted and Uniform SD -------------------------------------------------------------

H_control <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  # filter(from!='juveniles_adults_elderly') %>% 
  filter(k_percent==0) %>% 
  filter(time==max(times)) %>%
  dplyr::select(from, k_percent, SD, age_group, cases, vto)

H_vaccine <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  # filter(from!='juveniles_adults_elderly') %>% 
  filter(time==max(times)) %>%
  dplyr::select(from,k_percent, SD, age_group, cases, vto)

data_to_plot <- 
  left_join(H_vaccine, H_control, by=c("from","SD","age_group","vto")) %>% 
  dplyr::select(from, SD, vto, age_group, k_con=k_percent.y, k_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  group_by(from, SD, vto, k_con, k_vacc) %>% 
  summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
  mutate(k_vacc=as.factor(k_vacc)) %>% 
  mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
  ungroup() %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly')),
         from=factor(from, levels=c('adults','elderly','juveniles_adults_elderly')))

pdf(plotname('MT_strategy_comparison_Heff.pdf'), 12, 10)
data_to_plot %>% filter(k_vacc!=0) %>% 
  ggplot(aes(x=SD, y=H_eff, color=k_vacc))+
  geom_line(size=1)+
  facet_grid(from~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
                                           from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  # geom_hline(yintercept = 30, linetype='dotted', color='black', size=1)+
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  # scale_y_continuous(breaks = seq(0,60,10)) +
  scale_color_viridis_d()+
  labs(x='Proportion reduction in contact rates', y='% reduction in hospitalizations\n compared to no vaccines', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('SI_strategy_comparison_H.pdf'), 12, 8)
data_to_plot %>%
  ggplot(aes(x=SD, y=tot_cases_vac/N*100, color=k_vacc))+
  geom_line(size=1)+
  facet_grid(from~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
                                           from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  # scale_y_continuous(breaks = seq(0,60,10)) +
  scale_color_viridis_d()+
  labs(x='Proportion reduction in contact rates', y='% population hospitalized', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()


# # Percent change in I+A+P (r_p) ---------------------------------------------------
# # Calculate r_p=(z_t-z_{t-1})/z_{t-1}, where z=I+A+P
# 
# # Time to get r_p=0
# dat <- 
#   country_tbl %>%  
#   # filter(k_percent%in%c(0,k_range_percent[round(length(k_range_percent)/2)],k_percent_max)) %>% 
#   filter(k_percent%in%c(0,k_min, k_max)) %>% 
#   filter(state%in%c('I','A','P')) %>%
#   mutate(k_percent=factor(k_percent)) %>% 
#   mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
#   mutate(from=factor(from, levels = c('adults','elderly','juveniles_adults_elderly'))) %>% 
#   group_by(from, vto, SD, k_percent, time) %>% 
#   summarise(z=sum(cases)) %>% # Sum the I+A+P
#   ungroup() %>% 
#   group_by(from, vto, SD, k_percent) %>% 
#   mutate(r = (z-lag(z))/lag(z)) %>% 
#   # mutate(d = (z-lag(z))) %>% 
#   # mutate(g= (d/lag(d))) %>% 
#   filter(time>5) # Remove the initial dip, which is an artefact
# 
# # dat %>% ungroup() %>% distinct(from,vto)
# 
# # Plot dynamics of r_p
# pdf(plotname('MT_r_p_dynamics.pdf'), 12, 5)
# dat %>% 
#   filter(SD%in%c(0,0.5,0.8)) %>%
#   filter(k_percent==k_max) %>% 
#   ggplot(aes(time, r, color=as.factor(SD), linetype=vto))+
#   geom_line(size=1.2)+
#   facet_grid(~from, labeller = labeller(k_percent=c(`0`='No vaccine', `0.1`='Min. vaccination rate', `0.5`='Max. vaccination rate'),
#                                                  from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_manual(values = c('red','orange','brown'), labels=c('0','0.5','0.8'))+
#   scale_linetype_discrete(labels = c("Elderly first", "Adults first"))+
#   scale_x_continuous(limits=c(0,max(dat$time)))+
#   labs(x='Days', y='Proportion daily change in active cases', color='% reduction\n contacts', linetype='Vaccination\n strategy')+
#   theme_bw() + 
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()
# 
# 
# pdf(plotname('SI_r_heatmap.pdf'), 12, 8)
# country_tbl %>%  
#   filter(state%in%c('I','A','P')) %>%
#   mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
#   mutate(from=factor(from, levels = c('juveniles_adults_elderly', 'adults','elderly'))) %>% 
#   group_by(from, vto, SD, k_percent, time) %>% 
#   summarise(z=sum(cases)) %>% # Sum the I+A+P
#   ungroup() %>% 
#   group_by(from, vto, SD, k_percent) %>% 
#   mutate(r = (z-lag(z))/lag(z)) %>% 
#   filter(time>5) %>% # Remove the initial dip, which is an artefact
# 
#   group_by(from, vto, SD, k_percent) %>% slice(which(r<=0)) %>% top_n(1) %>%
#   
#   ggplot(aes(x=SD, y=k_percent, fill=time))+
#   facet_grid(vto~from, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'), from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
#   geom_tile(color='white')+
#   scale_fill_distiller(palette = "RdPu") +
#   # scale_fill_gradientn(colors=c('red','gray','blue'), limits=c(-max(abs(range(dat$diff_time))), max(abs(range(dat$diff_time)))))+
#   labs(x='Proportion reduction in contact rates', y='% vaccinated per day', fill='Time to r=0')+
#   coord_fixed()+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()


# Proportion of hospitalizations ------------------------------------------

N_per_age <- tibble(age_group=unique(country_tbl$age_group), N_j=N_age_groups)
d <- country_tbl %>% 
  filter(state=='H') %>% 
  mutate(k_percent=as.factor(k_percent)) %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
  mutate(from=factor(from, levels=c('juveniles_adults_elderly','adults','elderly'))) %>% 
  group_by(vto, from, SD, k_percent, time) %>% 
  summarise(tot_cases=sum(cases)) %>% 
  filter(tot_cases<=0.99*max(tot_cases)) %>% # Cannot take the maximum itself because it is a continous model and the max is always at the last time point.
  slice_tail(n=1)

pdf(plotname('MT_strategy_comparison_H_max_vaccination.pdf'), 12, 8)
d %>% 
  filter(k_percent == k_max) %>% 
  ggplot(aes(x=SD, y=tot_cases/N*100, color=from, linetype=vto))+
  geom_line(size=1)+
  geom_point(size=3, alpha=0.5)+
  scale_size(range=c(1,10))+
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  scale_y_continuous(limits = round(c(0,max(d$tot_cases/N*100))), n.breaks = 5) + 
  scale_linetype_manual(values = c('solid','dotted'), labels = c("Elderly first", "Adults first"))+
  scale_color_manual(values = c('black','orange','purple'), labels = c("Uniform", "Adults","Elderly"))+
  labs(x='Proportion reduction in contact rates', y='% population hospitalized', 
       color='SD strategy', linetype='Vaccination\n strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()


dat <- country_tbl %>% 
  filter(state=='H') %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
  mutate(from=factor(from, levels=c('adults','elderly','juveniles_adults_elderly'))) %>% 
  group_by(vto, from, SD, k_percent, time) %>% 
  summarise(tot_cases=sum(cases)) %>% 
  filter(tot_cases<=0.99*max(tot_cases)) %>% # Cannot take the maximum itself because it is a continous model and the max is always at the last time point.
  slice_tail(n=1) %>% 
  # filter(from=='juveniles_adults_elderly', SD==0.4, k_percent==0.5) %>% # For testing
  group_by(from, SD, k_percent) %>% 
  # Advatange of elderly over adult vaccination is calculated as the % reduction
  # (negative) or increase (positive): (H_elderly-H_adult)/H_elderly
  summarise(diff=(tot_cases[2]-tot_cases[1])/tot_cases[2]*100,
            diff_time=(time[2]-time[1])/time[2]*100)


pdf(plotname('MT_advantage_vto_H_heatmap.pdf'), 12, 8)
pos_diff <- dat$diff[which(dat$diff>0)]
lim <- round(max(abs(dat$diff)))+5
dat %>% 
  ggplot(aes(x=SD, y=k_percent, fill=diff))+
  facet_grid(~from, labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_tile(color='white')+
  geom_label(aes(label=ifelse(round(diff)>=quantile(pos_diff, 0.5),'+',NA)), size=4, label.size = 0)+
  # scale_fill_distiller(palette = "RdPu") +
  scale_fill_gradientn(colors=c('red','gray','blue'), limits=c(-lim,lim))+
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 4)+
  labs(x='Proportion reduction in contact rates', y='% vaccinated per day', fill='Elderly first\n advantage (%)')+
  # coord_equal()+
  coord_fixed()+
  theme_bw() +
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()  

pdf(plotname('SI_advantage_vto_H_heatmap_labels.pdf'), 12, 8)
lim <- round(max(abs(dat$diff)))+5
dat %>%
  ggplot(aes(x=SD, y=k_percent, fill=diff))+
  facet_grid(~from, labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_tile(color='white')+
  geom_label(aes(label=round(diff)), size=2.5)+
  # scale_fill_distiller(palette = "RdPu") +
  scale_fill_gradientn(colors=c('red','gray','blue'), limits=c(-lim,lim))+
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 4)+
  labs(x='Proportion reduction in contact rates', y='% vaccinated per day', fill='Elderly first\n advantage (%)')+
  # coord_equal()+
  coord_fixed()+
  theme_bw() +
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

# Calculate Rt ----------------------------------------------------------
library(R0)

d <- 
  country_tbl %>% 
  filter(k_percent%in%c(0,k_min, k_max)) %>% 
  filter(state%in%c('I','A','P')) %>%
  mutate(k_percent=factor(k_percent)) %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
  mutate(from=factor(from, levels = c('adults','elderly','juveniles_adults_elderly'))) %>% 
  group_by(from, vto, SD, k_percent, time) %>% 
  summarise(z=sum(cases))  # Sum the I+A+P
 
# d %>% 
#   filter(SD%in%c(0,0.5)) %>% 
#   ggplot(aes(time, z/N, color=as.factor(k_percent)))+
#   geom_line(size=1.3)+
#   facet_grid(from~vto+SD, 
#              labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
#   # scale_color_brewer(type='seq', palette = 5)+
#   # scale_color_viridis_d()+
#   scale_color_manual(values = c('red','orange','brown'))+
#   # scale_linetype_discrete(labels = c("Elderly first", "Adults first"))+
#   # scale_x_continuous(limits=c(0,max(dat$time)))+
#   labs(x='Days', y='Proportion of active cases', color='% reduction\n contacts', linetype='Vaccination\n strategy')+
#   theme_bw() + 
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))

# Estimate R_t following Cori et akl 2013
# https://www.gov.il/BlobFolder/reports/research-report-n211-r-calculate/he/research-report_research-report-n211-r-calculate.pdf
Cori <- function(y){
  w_s <- generation.time("gamma", c(4.5, 2.5), truncate = 7, p0 = F)$GT
  denom <- 0
  for (s in 1:length(y)){
    denom <- denom+w_s[s]*y[s]
  }
  return(denom)
}

Rt_Cori <-
  d  %>%
  group_by(from, vto,SD,k_percent) %>%
  mutate(seven_day_index=c(0, rep(1:(max(d$time)-1)%/%7))+1) %>%
  mutate(I_t=z-lag(z)) %>%
  filter(seven_day_index>1) %>%
  filter(seven_day_index<=21) %>% # Sometimes crashes in later weeks
  group_by(from, vto, SD, k_percent,seven_day_index) %>%
  summarise(denom=Cori(round(z)),
            I_t=last(z)) %>%
  mutate(R_t=I_t/denom)

pdf(plotname('MT_Rt.pdf'), 12, 6)
Rt_Cori %>%
  filter(SD %in% c(0,0.2,0.5)) %>%
  filter(from=='juveniles_adults_elderly') %>%
  # filter(vto=='elderly_adults') %>%
  ggplot(aes(seven_day_index, R_t, color=as.factor(k_percent), linetype=vto))+
  # geom_point()+
  geom_line(size=1.3)+
  geom_hline(yintercept = 1, linetype='dotted')+
  scale_color_viridis_d()+
  scale_linetype_discrete(labels = c("Elderly first", "Adults first"))+
  scale_x_continuous(n.breaks = 6, limits = c(1,max(Rt_Cori$seven_day_index)))+
  scale_y_continuous(n.breaks = 6)+
  labs(x='Week number', y='R_t', color='Vaccintion rate', linetype='Vaccination\n strategy')+
  facet_grid(~SD)+
  theme_bw()+
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('SI_Rt.pdf'), 12, 6)
Rt_Cori %>% 
  filter(SD %in% c(0,0.2,0.5)) %>%
  filter(seven_day_index>1) %>% 
  ggplot(aes(seven_day_index, R_t, color=as.factor(k_percent), linetype=vto))+
  geom_line(size=1.3)+
  geom_hline(yintercept = 1)+
  scale_color_viridis_d()+
  scale_linetype_discrete(labels = c("Elderly first", "Adults first"))+
  scale_x_continuous(n.breaks = 6, limits = c(1,max(Rt_Cori$seven_day_index)))+
  scale_y_continuous(n.breaks = 6)+
  labs(x='Week number', y='R_t', color='Vaccintion rate', linetype='Vaccination\n strategy')+
  facet_grid(from~SD,
             labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  theme_bw()+
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()


# Alternative R_t calculation.
# A function to calculate R_t for chunks of 7 days with method by Wallinga.
mGT <- generation.time("gamma", c(4.5, 2.5), truncate = 7, p0 = F)
get_Rt.EG <- function(y){
  R0 <- est.R0.EG(y, mGT, begin = 1, end = 7)
  return(R0$R)
}
Rt_EG <- d %>%
  group_by(from, vto,SD,k_percent) %>%
  mutate(seven_day_index=c(0, rep(1:(max(d$time)-1)%/%7))+1) %>%
  filter(SD%in%c(0,0.2,0.5)) %>%
  filter(seven_day_index<=21) %>% # Sometimes crashes in later weeks
  group_by(from, vto, SD, k_percent,seven_day_index) %>%
  summarise(R_eff=get_Rt.EG(round(z)))

pdf(plotname('MT_Rt_EG.pdf'), 12, 6)
Rt_EG %>%
  filter(seven_day_index>1) %>%
  filter(from=='juveniles_adults_elderly') %>%
  # filter(vto=='elderly_adults') %>%
  ggplot(aes(seven_day_index, R_eff, color=as.factor(k_percent), linetype=vto))+
  # geom_point()+
  geom_line(size=1.3)+
  geom_hline(yintercept = 1)+
  scale_color_viridis_d()+
  scale_linetype_discrete(labels = c("Elderly first", "Adults first"))+
  scale_x_continuous(n.breaks = 6, limits = c(1,max(Rt_EG$seven_day_index)))+
  scale_y_continuous(n.breaks = 6)+
  labs(x='Week number', y='R_t', color='Vaccintion rate', linetype='Vaccination\n strategy')+
  facet_grid(~SD)+
  theme_bw()+
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('SI_Rt_EG.pdf'), 12, 8)
Rt_EG %>%
  filter(seven_day_index>1) %>%
  ggplot(aes(seven_day_index, R_eff, color=as.factor(k_percent), linetype=vto))+
  geom_line(size=1.3)+
  geom_hline(yintercept = 1)+
  scale_color_viridis_d()+
  scale_linetype_discrete(labels = c("Elderly first", "Adults first"))+
  scale_x_continuous(n.breaks = 6, limits = c(1,max(Rt_EG$seven_day_index)))+
  scale_y_continuous(n.breaks = 6)+
  labs(x='Week number', y='R_t', color='Vaccintion rate', linetype='Vaccination\n strategy')+
  facet_grid(from~SD,
             labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  theme_bw()+
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()



# Arrange in a folder -----------------------------------------------------
print('Organizing into a folder...')
files <- list.files(pattern = as.character(JOB_ID))
files <- files[-which(str_detect(files, 'run_summary'))]
folder <- paste(JOB_ID, max_weeks,current_country,sep = '_')
dir.create(path = folder)
sapply(files, function(z) file.copy(z, folder))
sapply(files, unlink)
file.copy(paste(JOB_ID,'_run_summary.csv',sep=''), folder)




