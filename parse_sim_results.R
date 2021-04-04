# Add 2 more targeted SD scenarios
# Analyze total hospitalizations.

#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")
print(sessionInfo())

library(magrittr)
library(tidyverse)
library(socialmixr)

# args <- c(8008193, 15)

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
country_tbl %<>% filter(time<=7*max_weeks)
max_time <- country_tbl %>% slice(which.max(time))
times <- 1:max_time$time
k_percent_max <- max(country_tbl$k_percent)
k_range_percent <- unique(country_tbl$k_percent)

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
  filter(k_percent==0.4) %>% 
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
  filter(k_percent == 0.4) %>%
  mutate(Proportion=cases/N) %>% 
  mutate(grp='Vaccine') %>% 
  mutate(col=factor(col, levels = state_colors$col)) %>% 
ggplot(aes(x=time, y=Proportion, color=col))+
  geom_line(size=1.2)+
  facet_wrap(~age_group)+
  labs(y='Proportion of the population', x='Day', color='State', linetype='Group')+
  scale_color_identity(guide = 'legend', labels=state_colors$state)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
dev.off()

pdf(plotname('SI_IHV_dynamics_ctrl_vs_vaccine.pdf'), 10, 8)
bind_rows(control, vaccine) %>% 
  mutate(grp=factor(grp, levels = c('Vaccine','Control'))) %>% 
  filter(state %in% c('I','H','V')) %>% 
  filter(time<=15*7) %>% # Plot 15 weeks for clarity
  ggplot(aes(x=time, y=Proportion, linetype=grp, color=col))+
  geom_line(size=1.2)+
  facet_wrap(~age_group, scales = 'free')+
  labs(y='Proportion of the population', x='Day', color='State', linetype='Group')+
  scale_color_identity(guide = 'legend', labels=c('I','H','V'))+
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
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
dev.off()



# Compare two vaccination strategies with no SD -------------------------------------------------

H_control <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(SD==0) %>% 
  filter(k_percent==0) %>% 
  filter(time==max(times)) %>% 
  dplyr::select(k_percent, age_group, cases, vto)

H_vaccine <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(SD==0) %>% 
  filter(time==max(times)) %>% 
  dplyr::select(k_percent, age_group, cases, vto)

pdf(plotname('SI_compare_vaccination_strategies_SD0_Heff.pdf'), 12, 8)
left_join(H_vaccine, H_control, by=c("age_group", "vto")) %>% 
  dplyr::select(vto, age_group, k_percent_con=k_percent.y, k_percent_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  group_by(vto, k_percent_con, k_percent_vacc) %>% 
  summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
  mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
  ggplot(aes(x=k_percent_vacc, y=H_eff, color=vto))+
  geom_line(size=1.5)+geom_point(size=3)+
  geom_vline(xintercept = k_min, linetype='dotted')+
  scale_x_continuous(breaks = k_range_percent) +
  # scale_y_continuous(breaks = seq(0,30,5)) + 
  scale_color_manual(values = c('red','blue'), labels=c('Adults first','Elderly first'))+
  labs(x='% population vaccinated per day', y='% reduction in hospitalizations\n compared to no vaccines',
       color='Vaccine\n strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=20, color='black'),
        legend.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        legend.text = element_text(size=16, color='black'),
        axis.title = element_text(size=20, color='black'))
dev.off()

pdf(plotname('SI_compare_vaccination_strategies_SD0_prop_H.pdf'), 12, 8)
H_vaccine %>% 
  group_by(vto, k_percent) %>% 
  summarise(tot_cases=sum(cases)) %>% 
  ggplot(aes(x=k_percent, y=tot_cases/N*100, color=vto))+
  geom_line(size=1.5)+geom_point(size=3)+
  geom_vline(xintercept = k_min, linetype='dotted')+
  scale_color_manual(values = c('red','blue'), labels=c('Adults first','Elderly first'))+
  scale_x_continuous(breaks = k_range_percent) +
  labs(x='% population vaccinated per day', y='% population hospitalized', color='Vaccine\n strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=20, color='black'),
        panel.grid = element_blank(),
        legend.title = element_text(size=16, color='black'),
        legend.text = element_text(size=16, color='black'),
        axis.title = element_text(size=20, color='black'))
dev.off()

pdf(plotname('SI_compare_vaccination_strategies_SD0_Heff_by_age.pdf'), 12, 8)
left_join(H_vaccine, H_control, by=c("age_group", "vto")) %>% 
  dplyr::select(vto, age_group, k_percent_con=k_percent.y, k_percent_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  mutate(H_eff=(1-cases_vacc/cases_con)*100) %>% 
  ggplot(aes(x=k_percent_vacc, y=H_eff, color=vto))+
  geom_line()+geom_point()+
  geom_vline(xintercept = k_min, linetype='dotted')+
  facet_wrap(~age_group)+
  scale_x_continuous(breaks = k_range_percent) +
  scale_color_manual(values = c('red','blue'), labels=c('20-59 then 60+','60+ then 20-59'))+
  labs(x='% population vaccinated per day', y='% reduction in hospitalizations\n compared to no vaccines',
       color='Vaccine\n strategy')+  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('SI_compare_vaccination_strategies_SD0_prop_H_by_age.pdf'), 12, 8)
H_vaccine %>% 
  group_by(vto, age_group , k_percent) %>% 
  summarise(tot_cases=sum(cases)) %>% 
  ggplot(aes(x=k_percent, y=tot_cases/N*100, color=vto))+
  geom_line()+geom_point()+
  geom_vline(xintercept = k_min, linetype='dotted')+
  facet_wrap(~age_group)+
  scale_x_continuous(breaks = k_range_percent) +
  scale_color_manual(values = c('red','blue'), labels=c('Adults first','Elderly first'))+
  labs(x='% population vaccinated per day', y='% population hospitalized', color='Vaccine\n strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=20, color='black'),
        legend.title = element_text(size=16, color='black'),
        legend.text = element_text(size=16, color='black'),
        axis.title = element_text(size=20, color='black'))
dev.off()

# Uniform SD --------------------------------------------------------------
# H_control <- 
#   country_tbl %>% 
#   filter(state=='H') %>% 
#   filter(from=='juveniles_adults_elderly') %>% 
#   filter(k_percent==0) %>% 
#   filter(time==max(times)) # Number of cases at the end of the simulation
# H_vaccine <- 
#   country_tbl %>% 
#   filter(state=='H') %>% 
#   filter(from=='juveniles_adults_elderly') %>% 
#   filter(time==max(times)) # Number of cases at the end of the simulation
# 
# data_to_plot <- 
#   left_join(H_vaccine, H_control, by=c("age_group", "vto", "SD")) %>% 
#   dplyr::select(SD, vto, age_group, k_percent_con=k_percent.y, k_percent_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
#   group_by(vto, SD, k_percent_vacc) %>% 
#   summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
#   mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
#   mutate(k_percent_vacc=as.factor(k_percent_vacc)) %>% 
#   # filter(k_percent_vacc!=0) %>%
#   ungroup() %>% 
#   mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly')))
# 
# max_heff <- data_to_plot %>% 
#   group_by(vto) %>% 
#   filter(H_eff==max(H_eff)) %>% 
#   dplyr::select(vto, SD, H_eff)
# 
# pdf(plotname('uniform_sd.pdf'), 12, 8)
# data_to_plot %>% 
#   filter(k_percent_vacc!=0) %>% 
# ggplot(aes(x=SD, y=H_eff, color=k_percent_vacc))+
# facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first')))+
# geom_line(size=1.5)+
# geom_vline(data=max_heff, aes(xintercept = SD), linetype='dashed', size=1)+
# # geom_hline(yintercept = 30, linetype='dotted', color='black', size=1)+
# scale_x_continuous(breaks = seq(0,1,0.2)) + 
# scale_y_continuous(breaks = seq(0,100,10)) +
# scale_color_viridis_d()+
# labs(x='% reduction in contact rates', y='% reduction in hospitalizations\n compared to no vaccines', color='% vaccinated\n per day')+
# theme_bw() + 
# theme(axis.text = element_text(size=16, color='black'),
#       axis.title = element_text(size=16, color='black'),
#       panel.spacing = unit(2, "lines"))
# dev.off()


# ggplot(data=data_to_plot, aes(x=SD, y=tot_cases_vac/N*100, color=k_percent_vacc, linetype=vto))+
#   geom_line(size=1.5)+
#   scale_x_continuous(breaks = seq(0,1,0.2)) + 
#   scale_color_viridis_d()+
#   labs(x='% reduction in contact rates', y='% hospitalized', color='% vaccinated\n per day')+
#   theme_bw() + 
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.spacing = unit(2, "lines"))


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
  labs(x='% reduction in contact rates', y='% reduction in hospitalizations\n compared to no vaccines', color='% vaccinated\n per day')+
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
  labs(x='% reduction in contact rates', y='% population hospitalized', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()




# Percent change in I+A+P (r_p) ---------------------------------------------------
# Calculate r_p=(z_t-z_{t-1})/z_{t-1}, where z=I+A+P

# Time to get r_p=0
dat <- 
  country_tbl %>%  
  # filter(k_percent%in%c(0,k_range_percent[round(length(k_range_percent)/2)],k_percent_max)) %>% 
  filter(k_percent%in%c(0,k_min, k_max)) %>% 
  filter(state%in%c('I','A','P')) %>%
  mutate(k_percent=factor(k_percent)) %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
  mutate(from=factor(from, levels = c('adults','elderly','juveniles_adults_elderly'))) %>% 
  group_by(from, vto, SD, k_percent, time) %>% 
  summarise(z=sum(cases)) %>% # Sum the I+A+P
  ungroup() %>% 
  group_by(from, vto, SD, k_percent) %>% 
  mutate(r = (z-lag(z))/lag(z)) %>% 
  # mutate(d = (z-lag(z))) %>% 
  # mutate(g= (d/lag(d))) %>% 
  filter(time>5) # Remove the initial dip, which is an artefact

# dat %>% ungroup() %>% distinct(from,vto)

# Plot dynamics of r_p
pdf(plotname('MT_r_p_dynamics.pdf'), 12, 5)
dat %>% 
  filter(SD%in%c(0,0.5,0.8)) %>%
  filter(k_percent==k_max) %>% 
  ggplot(aes(time, r, color=as.factor(SD), linetype=vto))+
  geom_line(size=1.2)+
  facet_grid(~from, labeller = labeller(k_percent=c(`0`='No vaccine', `0.1`='Min. vaccination rate', `0.5`='Max. vaccination rate'),
                                                 from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_color_manual(values = c('red','orange','brown'), labels=c('0','0.5','0.8'))+
  scale_linetype_discrete(labels = c("Elderly first", "Adults first"))+
  scale_x_continuous(limits=c(0,max(dat$time)))+
  labs(x='Days', y='% daily change in active cases', color='% reduction\n contacts', linetype='Vaccination\n strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('SI_r_p_dynamics.pdf'), 12, 8)
dat %>% 
  filter(time>=10) %>% 
  filter(SD%in%c(0,0.5,0.8)) %>%
  # ggplot(aes(time,r, color=as.factor(k_percent)))+
  ggplot(aes(time, r, color=as.factor(SD), linetype=vto))+
  geom_line(size=1.2)+
  facet_grid(from~k_percent, labeller = labeller(k_percent=c(`0`='No vaccine', `0.1`='Min. vaccination rate', `0.5`='Max. vaccination rate'),
                                           from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_color_manual(values = c('red','orange','brown'), labels=c('0','0.5','0.8'))+
  scale_linetype_discrete(labels=c('Elderly first','Adults first'))+
  scale_x_continuous(limits=c(0,max(dat$time)))+
  labs(x='Days', y='% daily change in active cases', color='% reduction\n contacts', linetype='Vaccination\n strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()


pdf(plotname('SI_r_p_time.pdf'), 12, 8)
dat %>%
  group_by(from, vto, SD, k_percent) %>% slice(which(r<=0)) %>% top_n(1) %>% 
  ggplot(aes(y=time, x=SD, color=as.factor(k_percent)))+
  # geom_point(size=3)+
  geom_line(size=1.5)+
  facet_grid(from~vto,labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
                                           from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  scale_color_viridis_d()+
  labs(x='% reduction in contact rates', y='Days to reach r_p=0', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        legend.title = element_text(size=16, color='black'),
        legend.text = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('SI_r_heatmap.pdf'), 12, 8)
country_tbl %>%  
  filter(state%in%c('I','A','P')) %>%
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
  mutate(from=factor(from, levels = c('juveniles_adults_elderly', 'adults','elderly'))) %>% 
  group_by(from, vto, SD, k_percent, time) %>% 
  summarise(z=sum(cases)) %>% # Sum the I+A+P
  ungroup() %>% 
  group_by(from, vto, SD, k_percent) %>% 
  mutate(r = (z-lag(z))/lag(z)) %>% 
  filter(time>5) %>% # Remove the initial dip, which is an artefact

  group_by(from, vto, SD, k_percent) %>% slice(which(r<=0)) %>% top_n(1) %>%
  
  ggplot(aes(x=SD, y=k_percent, fill=time))+
  facet_grid(vto~from, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'), from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_tile(color='white')+
  scale_fill_distiller(palette = "RdPu") +
  # scale_fill_gradientn(colors=c('red','gray','blue'), limits=c(-max(abs(range(dat$diff_time))), max(abs(range(dat$diff_time)))))+
  labs(x='% reduction in contact rates', y='% vaccinated per day', fill='Time to r=0')+
  coord_fixed()+
  theme_bw() +
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

# dat <- 
#   country_tbl %>% 
#   # filter(from=='juveniles_adults_elderly') %>% 
#   filter(SD%in%c(0,0.5,0.75)) %>% 
#   filter(k_percent%in%c(0,k_range_percent[round(length(k_range_percent)/2)],k_percent_max)) %>% 
#   filter(state%in%c('I','A','P')) %>%
#   group_by(from, vto, SD, k_percent, time) %>% 
#   summarise(z=sum(cases)) %>% # Sum the I+A+P
#   ungroup() %>% 
#   group_by(vto, SD, k_percent) %>% 
#   mutate(r = (z-lag(z))/lag(z))




# growth rate -------------------------------------------------------------
# This is calculated from the equations
# 
# dat <-
#   country_tbl %>%
#   # filter(from=='juveniles_adults_elderly') %>%
#   filter(SD%in%c(0,0.5,0.8)) %>%
#   filter(k_percent%in%c(0,k_range_percent[round(length(k_range_percent)/2)],k_percent_max)) %>%
#   filter(state%in%c('I','E','A','P','S','U','V'))
# 
# L_state <- dat %>%
#   filter(state%in%c('E','A','S','U','P')) %>%
#   group_by(from, vto, SD, k_percent, age_group, time) %>%
#   summarise(L=sum(cases)) # L
# I_state <- dat %>%
#   filter(state=='I') %>%
#   dplyr::select(from, vto, SD, k_percent, age_group, time, I=cases)
# A_state <- dat %>%
#   filter(state=='A') %>%
#   dplyr::select(from, vto, SD, k_percent, age_group, time, A=cases)
# P_state <- dat %>%
#   filter(state=='P') %>%
#   dplyr::select(from, vto, SD, k_percent, age_group, time, P=cases)
# E_state <- dat %>%
#   filter(state=='E') %>%
#   dplyr::select(from, vto, SD, k_percent, age_group, time, E=cases)
# V_state <- dat %>%
#   filter(state=='V') %>%
#   dplyr::select(from, vto, SD, k_percent, age_group, time, V=cases)
# 
# toplot <-
#   left_join(I_state,A_state) %>%
#   left_join(P_state) %>%
#   left_join(E_state) %>%
#   left_join(L_state) %>%
#   left_join(V_state) %>%
#   mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
#   mutate(from=factor(from, levels=c('juveniles_adults_elderly','adults','elderly'))) %>% 
#   mutate(mu=V/L) %>% 
#   mutate(r_j=alpha*E-eta*I-gamma*A-mu*(A+P))  %>% 
#   mutate(r_j=r_j/N)
#  
# # toplot %>% ungroup() %>% distinct(from, vto, SD, k_percent, age_group)
# 
# 
# pdf(plotname('r_vto_SD_all_ages.pdf'), 12, 8)
# toplot %>%
#   filter(SD%in%c(0,0.5)) %>%
#   group_by(from, vto, SD, k_percent, time) %>% summarise(r=sum(r_j)) %>%
#   ggplot(aes(time,r, color=as.factor(k_percent), linetype=as.factor(SD)))+
#   geom_line(size=1.2)+
#   facet_grid(from~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
#                                            from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
#   
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day', linetype='SD strength')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()
# 
# pdf(plotname('r_vto_SD_60-69.pdf'), 12, 8)
# toplot %>%
#   filter(SD%in%c(0,0.5)) %>%
#   filter(age_group=='60-69') %>%
#   ggplot(aes(time,r_j, color=as.factor(k_percent), linetype=as.factor(SD)))+
#   geom_line(size=1.2)+
#   facet_grid(from~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
#                                            from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
#   
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day', linetype='SD strength')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()
# 
# pdf(plotname('r_vto_SD_70-79.pdf'), 12, 8)
# toplot %>%
#   filter(SD%in%c(0,0.5)) %>%
#   filter(age_group=='70-79') %>%
#   ggplot(aes(time,r_j, color=as.factor(k_percent), linetype=as.factor(SD)))+
#   geom_line(size=1.2)+
#   facet_grid(from~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
#                                            from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
#   
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day', linetype='SD strength')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()
# 
# 
# pdf(plotname('r_vto_SD_80.pdf'), 12, 8)
# toplot %>%
#   filter(SD%in%c(0,0.5)) %>%
#   filter(age_group=='80') %>%
#   ggplot(aes(time,r_j, color=as.factor(k_percent), linetype=as.factor(SD)))+
#   geom_line(size=1.2)+
#   facet_grid(from~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
#                                            from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
#   
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day', linetype='SD strength')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()


# pdf(plotname('ea_ae_r_SD0.pdf'), 12, 8)
# toplot %>%
#   filter(SD==0, vto=='elderly_adults') %>%
#   ggplot(aes(time,r, color=as.factor(k_percent), linetype=as.factor(SD)))+
#   geom_line(size=1.2)+
#   facet_wrap(~age_group)+
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()

# pdf(plotname('ea_ae_r_SD05.pdf'), 12, 8)
# toplot %>%
#   filter(SD==0.5, vto=='elderly_adults') %>%
#   ggplot(aes(time,r, color=as.factor(k_percent), linetype=as.factor(SD)))+
#   geom_line(size=1.2)+
#   facet_wrap(~age_group)+
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()

# pdf(plotname('ea_ae_r_SD08.pdf'), 12, 8)
# toplot %>%
#   filter(SD==0.8, vto=='elderly_adults') %>%
#   ggplot(aes(time,r, color=as.factor(k_percent), linetype=as.factor(SD)))+
#   geom_line(size=1.2)+
#   facet_wrap(~age_group)+
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))
# dev.off()

# I_state %>% filter(SD==0,vto=='elderly_adults') %>%
#   ggplot(aes(time,I, color=as.factor(k_percent)))+
#   geom_line(size=1.2)+
#   facet_wrap(~age_group)+
#   geom_hline(yintercept = 0, linetype='dashed')+
#   scale_color_viridis_d()+
#   labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day')+
#   theme_bw() +
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))


# Proportion of hospitalizations ------------------------------------------
# country_tbl %>% distinct(from)

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

pdf(plotname('MT_strategy_comparison_with_time_H.pdf'), 12, 8)
d %>% 
  filter(k_percent%in%c(0,k_min, k_max)) %>% 
  ggplot(aes(x=SD, y=tot_cases/N*100, color=from, linetype=vto))+
  facet_grid(~k_percent, labeller = labeller(k_percent=c(`0`='No vaccine', `0.1`='Min. vaccination rate', `0.5`='Max. vaccination rate')))+
  geom_line(size=1)+
  geom_point(aes(size=time), alpha=0.5)+
  scale_size(range=c(1,10))+
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  scale_linetype_manual(values = c('solid','dotted'), labels = c("Elderly first", "Adults first"))+
  scale_color_manual(values = c('black','orange','purple'), labels = c("Uniform", "Adults","Elderly"))+
  labs(x='% reduction in contact rates', y='% population hospitalized', 
       color='SD strategy', linetype='Vaccination\n strategy', size='Time to max\n hospitalizations')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('MT_strategy_comparison_with_time_H_max_vaccination.pdf'), 12, 8)
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
  labs(x='% reduction in contact rates', y='% population hospitalized', 
       color='SD strategy', linetype='Vaccination\n strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

pdf(plotname('SI_time_to_max_H.pdf'), 12, 8)
d %>% 
  ggplot(aes(SD, time, color=k_percent))+
  geom_point()+geom_line()+
  facet_grid(from~vto, labeller = labeller(vto=c(elderly_adults='Elderly first', adults_elderly='Adults first'),
                                           from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  labs(x='% reduction in contact rates', y='Days to reach max. hospitalizations', color='% vaccinated\n per day')+
  scale_color_viridis_d()+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
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

pdf(plotname('SI_advantage_vto_H.pdf'), 12, 8)
dat %>% 
  filter(k_percent%in%c(k_min, k_max)) %>% 
  ggplot(aes(x=SD, y=diff, color=from))+
  facet_grid(~k_percent)+
  geom_line(size=1.5)+
  # geom_point(aes(size=diff_time), alpha=0.5)+
  geom_point(size=3, alpha=0.5)+
  geom_hline(yintercept = 0, linetype='dotted', color='black')+
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  scale_y_continuous(n.breaks = 10)+
  scale_color_manual(values = c('black','orange','purple'), labels = c("Uniform", "Adults","Elderly"))+
  labs(x='% reduction in contact rates', y='% advantage for the "elderly first" strategy', 
       color='SD strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()


pdf(plotname('MT_advantage_vto_H_heatmap.pdf'), 12, 8)
dat %>% 
  ggplot(aes(x=SD, y=k_percent, fill=diff))+
  facet_grid(~from, labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_tile(color='white')+
  geom_label(aes(label=ifelse(round(diff)>=50,'+',NA)), size=4, label.size = 0)+
  # scale_fill_distiller(palette = "RdPu") +
  scale_fill_gradientn(colors=c('red','gray','blue'), limits=c(-100,100))+
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 4)+
  labs(x='% reduction in contact rates', y='% vaccinated per day', fill='Elderly first\n advantage (%)')+
  # coord_equal()+
  coord_fixed()+
  theme_bw() +
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()  

pdf(plotname('SI_advantage_vto_H_heatmap_labels.pdf'), 12, 8)
dat %>% 
  ggplot(aes(x=SD, y=k_percent, fill=diff))+
  facet_grid(~from, labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_tile(color='white')+
  geom_label(aes(label=round(diff)), size=2.5)+
  # scale_fill_distiller(palette = "RdPu") +
  scale_fill_gradientn(colors=c('red','gray','blue'), limits=c(-100,100))+
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 4)+
  labs(x='% reduction in contact rates', y='% vaccinated per day', fill='Elderly first\n advantage (%)')+
  # coord_equal()+
  coord_fixed()+
  theme_bw() +
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()  


pdf(plotname('MT_advantage_vto_time_H_heatmap.pdf'), 12, 8)
dat %>% 
  ggplot(aes(x=SD, y=k_percent, fill=diff_time))+
  facet_grid(~from, labeller = labeller(from=c(adults='SD for adults',elderly='SD for elderly', juveniles_adults_elderly='Uniform SD')))+
  geom_tile(color='white')+
  # scale_fill_distiller(palette = "RdPu") +
  scale_fill_gradientn(colors=c('red','gray','blue'), limits=c(-max(abs(range(dat$diff_time))), max(abs(range(dat$diff_time)))))+
  labs(x='% reduction in contact rates', y='% vaccinated per day', fill='Elderly first\n advantage')+
  # coord_equal()+
  coord_fixed()+
  theme_bw() +
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()  

# Arrange in a folder -----------------------------------------------------
print('Organizing into a folder...')
files <- list.files(pattern = as.character(JOB_ID))
files <- files[-which(str_detect(files, 'run_summary'))]
folder <- paste(JOB_ID, max_weeks,sep = '_')
dir.create(path = folder)
sapply(files, function(z) file.copy(z, folder))
sapply(files, unlink)
file.copy(paste(JOB_ID,'_run_summary.csv',sep=''), folder)

