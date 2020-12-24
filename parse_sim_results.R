#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")
print(sessionInfo())

library(magrittr)
library(tidyverse)
library(socialmixr)

# args <- c(4122972, 12)

if (length(commandArgs(trailingOnly=TRUE))==0) {
  stop('No arguments were found!')
} else {
  args <- commandArgs(trailingOnly=TRUE)
  JOB_ID <- as.numeric(args[1])
  max_weeks <- as.numeric(args[2])
}

run_summary <- read.csv(paste(JOB_ID,'run_summary.csv',sep='_'))
beta_matrix_no_interv <- as.matrix(read_csv(paste(JOB_ID,'beta_matrix_no_interv.csv',sep='_')))
rownames(beta_matrix_no_interv) <- colnames(beta_matrix_no_interv)

current_country <- run_summary[2,2]
N <-  as.numeric(run_summary[3,2])

country_tbl <- read_csv(paste(JOB_ID,'_results_',current_country,'.csv',sep=''))
country_tbl %<>% filter(time<=7*max_weeks)

max_time <- country_tbl %>% slice(which.max(time))
times <- 1:max_time$time

source('functions.R')


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
pdf(plotname('epidemiological_curves_control.pdf'), 10, 8)
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
  filter(k_percent==0.2) %>% 
  mutate(Proportion=cases/N) %>% 
  mutate(grp='Vaccine') %>% 
  mutate(col=factor(col, levels = state_colors$col))

# Epidmeiological curves for vaccine
pdf(plotname('epidemiological_curves_vaccine.pdf'), 10, 8)
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

pdf(plotname('IHV_dynamics_ctrl_vs_vaccine.pdf'), 10, 8)
bind_rows(control, vaccine) %>% 
  mutate(grp=factor(grp, levels = c('Vaccine','Control'))) %>% 
  filter(state %in% c('I','H','V')) %>% 
  filter(time<=12*7) %>% # Plot 12 weeks for clarity
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


# Dynamics comparison of two vaccination strategies -----------------------
vaccine_ea <- 
  country_tbl %>% 
  # filter (time<=7*12) %>%  #Plot 12 weeks for convenience
  filter(from=='juveniles_adults_elderly') %>% 
  filter(vto=='elderly_adults') %>% 
  filter(SD==0) %>% 
  filter(k_percent==0.2) %>% 
  mutate(Proportion=cases/N) %>% 
  mutate(grp='Elderly first')
vaccine_ae <- 
  country_tbl %>% 
  # filter (time<=7*12) %>%  #Plot 12 weeks for convenience
  filter(from=='juveniles_adults_elderly') %>% 
  filter(vto=='adults_elderly') %>% 
  filter(SD==0) %>% 
  filter(k_percent==0.2) %>% 
  mutate(Proportion=cases/N) %>% 
  mutate(grp='Adults first')

pdf(plotname('IHV_dynamics_ea_vs_ae.pdf'), 10, 8)
bind_rows(vaccine_ae, vaccine_ea) %>% 
  mutate(grp=factor(grp, levels = c('Adults first','Elderly first'))) %>% 
  filter(state %in% c('I','H','V')) %>% 
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



# Compare two strategies -------------------------------------------------

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

pdf(plotname('compare_strategies.pdf'), 12, 8)
left_join(H_vaccine, H_control, by=c("age_group", "vto")) %>% 
  dplyr::select(vto, age_group, k_percent_con=k_percent.y, k_percent_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  group_by(vto, k_percent_con, k_percent_vacc) %>% 
  summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
  mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
  ggplot(aes(x=k_percent_vacc, y=H_eff, color=vto))+geom_line(size=1.5)+
  # scale_x_continuous(breaks = seq(0,16000,4000)) + 
  scale_y_continuous(breaks = seq(0,30,5)) + 
  scale_color_manual(values = c('red','blue'), labels=c('20-59 then 60+','60+ then 20-59'))+
  labs(x='% population vaccinated per day', y='% reduction in hospitalizations\n compared to no vaccines', color='Strategy')+
  theme_bw() + 
  theme(axis.text = element_text(size=20, color='black'),
        legend.title = element_text(size=16, color='black'),
        legend.text = element_text(size=16, color='black'),
        axis.title = element_text(size=20, color='black'))
dev.off()

pdf(plotname('ea_ae_SD0_by_age.pdf'), 12, 8)
left_join(H_vaccine, H_control, by=c("age_group", "vto")) %>% 
  dplyr::select(vto, age_group, k_percent_con=k_percent.y, k_percent_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  mutate(H_eff=(1-cases_vacc/cases_con)*100) %>% 
  ggplot(aes(x=k_percent_vacc, y=H_eff, color=vto))+
  geom_line()+
  facet_wrap(~age_group)+
  scale_color_manual(values = c('red','blue'), labels=c('20-59 then 60+','60+ then 20-59'))+
  labs(x='% population vaccinated per day', y='% reduction in hospitalizations\n compared to no vaccines', color='Strategy')+  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()



# Uniform SD --------------------------------------------------------------
H_control <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(k_percent==0) %>% 
  filter(time==max(times)) # Number of cases at the end of the simulation
H_vaccine <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(time==max(times)) # Number of cases at the end of the simulation

data_to_plot <- 
  left_join(H_vaccine, H_control, by=c("age_group", "vto", "SD")) %>% 
  dplyr::select(SD, vto, age_group, k_percent_con=k_percent.y, k_percent_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  group_by(vto, SD, k_percent_vacc) %>% 
  summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
  mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
  mutate(k_percent_vacc=as.factor(k_percent_vacc)) %>% 
  filter(k_percent_vacc!=0) %>%
  ungroup() %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly')))

max_heff <- data_to_plot %>% 
  group_by(vto) %>% 
  filter(H_eff==max(H_eff)) %>% 
  dplyr::select(vto, SD, H_eff)

# pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_SD_sweep.pdf', 12, 8)
pdf(paste('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/country_figures/',current_country,'_uniform_sd.pdf',sep=''), 12, 8)
ggplot(data=data_to_plot, aes(x=SD, y=H_eff, color=k_percent_vacc))+
facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly then adults', adults_elderly='Adults then elderly')))+
geom_line(size=1.5)+
geom_vline(data=max_heff, aes(xintercept = SD), linetype='dashed', size=1)+
geom_hline(yintercept = 30, linetype='dotted', color='black', size=1)+
scale_x_continuous(breaks = seq(0,1,0.2)) + 
scale_y_continuous(breaks = seq(0,100,10)) +
scale_color_viridis_d()+
labs(x='% reduction in contact rates', y='% reduction in hospitalizations\n compared to no vaccines', color='% vaccinated\n per day')+
theme_bw() + 
theme(axis.text = element_text(size=16, color='black'),
      axis.title = element_text(size=16, color='black'),
      panel.spacing = unit(2, "lines"))
dev.off()



# Targeted SD -------------------------------------------------------------

H_control <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  filter(from!='juveniles_adults_elderly') %>% 
  filter(k_percent==0) %>% 
  filter(time==max(times)) %>% 
  dplyr::select(from, k_percent, SD, age_group, cases, vto)

H_vaccine <- 
  country_tbl %>% 
  filter(state=='H') %>% 
  filter(from!='juveniles_adults_elderly') %>% 
  filter(time==max(times)) %>% 
  dplyr::select(from,k_percent, SD, age_group, cases, vto)

data_to_plot <- 
left_join(H_vaccine, H_control, by=c("from","SD","age_group","vto")) %>% 
  dplyr::select(from, SD, vto, age_group, k_con=k_percent.y, k_vacc=k_percent.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  # distinct(from, vto)
  group_by(from, SD, vto, k_con, k_vacc) %>% 
  summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
  mutate(k_vacc=as.factor(k_vacc)) %>% 
  mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
  filter(k_vacc!=0) %>%  # This is unnecessary
  ungroup() %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly')))

# pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_targeted_SD_sweep.pdf', 12, 8)
pdf(paste('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/country_figures/',current_country,'_targeted_sd.pdf',sep=''), 12, 8)
ggplot(data= data_to_plot, aes(x=SD, y=H_eff, color=k_vacc))+
geom_line(size=1)+
facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly then adults; (SD for adults)', adults_elderly='Adults then elderly; (SD for elderly)')))+
geom_hline(yintercept = 30, linetype='dotted', color='black', size=1)+
scale_x_continuous(breaks = seq(0,1,0.2)) + 
scale_y_continuous(breaks = seq(0,60,10)) +
scale_color_viridis_d()+
labs(x='% reduction in contact rates', y='% reduction in hospitalizations\n compared to no vaccines', color='% vaccinated\n per day')+
theme_bw() + 
theme(axis.text = element_text(size=16, color='black'),
      axis.title = element_text(size=16, color='black'),
      panel.spacing = unit(2, "lines"))
dev.off()




# Percent change in I+A (r_p) ---------------------------------------------------
# Calculate r_p=(z_t-z_{t-1})/z_{t-1}, where z=I+A

# Time to get r_p=0
dat <- 
  country_tbl %>%  
  # filter(from=='juveniles_adults_elderly') %>%
  filter(k_percent%in%c(0,0.1,0.2)) %>% 
  filter(state%in%c('I','A')) %>%
  mutate(k_percent=factor(k_percent)) %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly'))) %>% 
  mutate(from=factor(from, levels = c('juveniles_adults_elderly','elderly','adults'))) %>% 
  group_by(from, vto, SD, k_percent, time) %>% 
  summarise(z=sum(cases)) %>% # Sum the I+A
  ungroup() %>% 
  group_by(from, vto, SD, k_percent) %>% 
  mutate(r = (z-lag(z))/lag(z)) %>% 
  filter(time>5) # Remove the initial dip

dat %>% ungroup() %>% 
  distinct(from,vto)

pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_time_to_r_p.pdf', 12, 8)
dat %>%
  group_by(from, vto, SD, k_percent) %>% slice(which(r<=0)) %>% top_n(1) %>% 
  ggplot(aes(y=time, x=SD, color=as.factor(k_percent)))+
  geom_point(size=3)+
  geom_line()+
  facet_wrap(from~vto, scales = 'free_y')+
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



dat <- 
  country_tbl %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(SD%in%c(0,0.5,0.75)) %>% 
  filter(k_percent%in%c(0,0.1,0.2)) %>% 
  filter(state%in%c('I','A')) %>%
  group_by(vto, SD, k_percent, time) %>% 
  summarise(z=sum(cases)) %>% # Sum the I+A
  ungroup() %>% 
  group_by(vto, SD, k_percent) %>% 
  mutate(r = (z-lag(z))/lag(z))

pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_r_p.pdf', 12, 8)
dat %>% filter(time>=10) %>% 
  ggplot(aes(time,r, color=as.factor(k_percent)))+
  geom_line(size=1.2)+
  facet_grid(SD~vto)+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_color_viridis_d()+
  labs(x='Days', y='r_p (% daily change in z=I+A)', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()


# growth rate -------------------------------------------------------------
# This is calculated from the equations

dat <- 
  country_tbl %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(SD%in%c(0,0.5,0.75)) %>% 
  filter(k_percent%in%c(0,0.1,0.2)) %>% 
  filter(state%in%c('I','E','A','S','U','V'))

L_state <- dat %>%
  filter(state%in%c('E','A','S','U')) %>% 
  group_by(vto, SD, k_percent, age_group, time) %>% 
  summarise(L=sum(cases)) # L
I_state <- dat %>% 
  filter(state=='I') %>% 
  dplyr::select(vto, SD, k_percent, age_group, time, I=cases)
A_state <- dat %>% 
  filter(state=='A') %>% 
  dplyr::select(vto, SD, k_percent, age_group, time, A=cases)
E_state <- dat %>% 
  filter(state=='E') %>% 
  dplyr::select(vto, SD, k_percent, age_group, time, E=cases)
V_state <- dat %>% 
  filter(state=='V') %>% 
  dplyr::select(vto, SD, k_percent, age_group, time,cases) %>% 
  group_by(vto, SD, k_percent, age_group) %>% 
  mutate(mu=cases-lag(cases))
# V_state %>% filter(SD==0,vto=='elderly_adults') %>%  ggplot(aes(x=time, y=mu, color=as.factor(k_percent)))+geom_line()+facet_wrap(~age_group)
toplot <- 
  left_join(I_state,A_state) %>%
  left_join(E_state) %>% 
  left_join(L_state) %>% 
  left_join(V_state) %>% 
  group_by(vto, SD, k_percent, age_group, time) %>% 
  mutate(r=alpha*E-eta*I-gamma*A-mu*A/L)

pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_r_SD075.pdf', 12, 8)
toplot %>% 
  filter(SD==0.5, vto=='elderly_adults') %>% 
  ggplot(aes(time,r, color=as.factor(k_percent), linetype=as.factor(SD)))+
  geom_line(size=1.2)+
  facet_wrap(~age_group)+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_color_viridis_d()+
  labs(x='Days', y='Growth rate (r)', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

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







# E+I+A -------------------------------------------------------------------
# SD=0.0; k=0
# SD=0.3; k=0
# SD=0.7; k=0
# SD=0.0; k=0.2
# SD=0.3; k=0.2
# SD=0.7; k=0.2
dat <-  
  country_tbl %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(SD%in%c(0,0.5,0.7)) %>% 
  filter(k_percent%in%c(0,0.2)) %>% 
  filter(state%in%c('I','E','A')) %>% 
  group_by(vto, SD, k_percent, time) %>% 
  summarise(Z=sum(cases)) %>% 
  mutate(below_1=ifelse(Z<1,T,F))
  
# plot Z
#c(0='0',0.5='50%',0.7='70%')
global_labeller <- labeller(
  vto = c(elderly_adults='Elderly then adults',adults_elderly='Adults then elderly'),
  .default = label_both
)
pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_Z.pdf', 12, 8)
ggplot(data = dat, aes(time,Z, color=as.factor(k_percent)))+
  geom_line(size=1.2)+
  facet_grid(SD~vto, labeller = global_labeller)+
  geom_point(data = dat %>% filter(below_1==T), aes(time,Z), color='green')+
  scale_color_viridis_d()+
  labs(x='Days', y='Number of I+A+E cases (Z)', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()


# Time to get I+A+E<1
dat <- 
  country_tbl %>% 
  filter(state%in%c('I','E','A')) %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(k_percent%in%c(0,0.1,0.2)) %>%
  group_by(vto,k_percent,SD,time) %>%
  summarise(Z=sum(cases)) %>% 
  ungroup() %>% 
  mutate(k_percent=factor(k_percent))

pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_t_Z.pdf', 12, 8)
dat %>% group_by(vto,k_percent,SD) %>% slice(which(Z<1)) %>% top_n(1) %>% 
  ggplot(aes(y=time, x=SD, color=k_percent))+
  geom_point(size=3)+
  facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly then adults', adults_elderly='Adults then elderly')))+
  scale_color_viridis_d()+
  labs(x='% reduction in contact rates', y='Days to reach I+A+E<1 (t_Z)', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

# # The minimum number of I+A+E after 30w
# dat %>% group_by(vto,k_percent,SD) %>% slice(which.min(Z)) %>% top_n(1) %>% 
#   ggplot(aes(y=Z, x=SD, color=k_percent))+geom_point()+
#   geom_line()+
#   facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly then adults', adults_elderly='Adults then elderly')))+
#   scale_color_viridis_d()+
#   labs(x='% reduction in contact rates', y='Minimum number of infected at the end of simulation', color='% vaccinated\n per day')+
#   theme_bw() + 
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))



# E+I+A targeted SD ------------------------------------------------------------
# SD=0.0; k=0
# SD=0.3; k=0
# SD=0.7; k=0
# SD=0.0; k=0.2
# SD=0.3; k=0.2
# SD=0.7; k=0.2
dat <-  
  country_tbl %>% 
  filter(from!='juveniles_adults_elderly') %>% 
  filter(SD%in%c(0,0.5,0.7)) %>% 
  filter(k_percent%in%c(0,0.2)) %>% 
  filter(state%in%c('I','E','A')) %>% 
  group_by(vto, SD, k_percent, time) %>% 
  summarise(Z=sum(cases)) %>% 
  mutate(below_1=ifelse(Z<1,T,F)) %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly')))


# plot Z
#c(0='0',0.5='50%',0.7='70%')
global_labeller <- labeller(
  vto = c(elderly_adults='Elderly then adults; (SD for adults)',adults_elderly='Adults then elderly; (SD for elderly)'),
  .default = label_both
)
pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_Z_targeted.pdf', 12, 8)
ggplot(data = dat, aes(time,Z, color=as.factor(k_percent)))+
  geom_line(size=1.2)+
  facet_grid(SD~vto, labeller = global_labeller)+
  geom_point(data = dat %>% filter(below_1==T), aes(time,Z), color='green')+
  scale_color_viridis_d()+
  labs(x='Days', y='Number of I+A+E cases (Z)', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

dat <- 
  country_tbl %>% 
  filter(state%in%c('I','E','A')) %>% 
  filter(from!='juveniles_adults_elderly') %>% 
  filter(k_percent%in%c(0,0.1,0.2)) %>%
  group_by(vto,k_percent,SD,time) %>%
  summarise(Z=sum(cases)) %>% 
  ungroup() %>% 
  mutate(k_percent=factor(k_percent)) %>% 
  mutate(vto=factor(vto, levels = c('elderly_adults','adults_elderly')))

# Time to get I+A+E<1
pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/ea_ae_t_Z_targeted.pdf', 12, 8)
dat %>% group_by(vto,k_percent,SD) %>% slice(which(Z<1)) %>% top_n(1) %>% 
  ggplot(aes(y=time, x=SD, color=k_percent))+
  geom_point(size=3)+
  geom_line()+
  facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly then adults; (SD for adults)', adults_elderly='Adults then elderly; (SD for elderly)')))+
  scale_color_viridis_d()+
  labs(x='% reduction in contact rates', y='Days to reach I+A+E<1 (t_Z)', color='% vaccinated\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))
dev.off()

# # The minimum number of I+A+E after 30w
# dat %>% group_by(vto,k_percent,SD) %>% slice(which.min(Z)) %>% top_n(1) %>% 
#   ggplot(aes(y=Z, x=SD, color=k_percent))+geom_point()+
#   geom_line()+
#   facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly then adults', adults_elderly='Adults then elderly')))+
#   scale_color_viridis_d()+
#   labs(x='% reduction in contact rates', y='Minimum number of infected at the end of simulation', color='% vaccinated\n per day')+
#   theme_bw() + 
#   theme(axis.text = element_text(size=16, color='black'),
#         axis.title = element_text(size=16, color='black'),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"))


# R_t ---------------------------------------------------------------------
k_percent_range <- country_tbl %>% distinct(k_percent)
k_percent_range <- k_percent_range$k_percent
k_percent_range <- c(0,0.2)

age_groups <- age_structure$age_group
out <- NULL
for (strategy in c('elderly_adults')){
for (x in k_percent_range){
  dat <- 
    country_tbl %>% 
    filter(state%in%c('S','V','E','A','U')) %>% 
    filter(from=='juveniles_adults_elderly') %>% 
    filter(vto==strategy) %>% 
    filter(SD==0) %>% 
    filter(k_percent==x)

  for (t in 2:max(times)){
    for (i in age_groups){
      tmp_i <- dat %>% filter(time%in%c(t, t-1), age_group==i)
      mu_i <- tmp_i$cases[10]-tmp_i$cases[9]
      N_i <- N*Table_1$Proportion[Table_1$Age==i]
      # L_i <- tmp_i %>% filter(time==t, state!='V') %>% summarise(L=sum(cases))
      # L_i <- L_i$L
      S_i <- tmp_i$cases[2]
      m_i <- Table_1$m[Table_1$Age==i]
      for (j in age_groups){
        print(paste(x,t,i,j,sep=' | '))
        N_j <- N*Table_1$Proportion[Table_1$Age==j]
        tmp_j <- dat %>% filter(time%in%c(t, t-1), age_group==j)
        # L_j <- tmp_j %>% filter(time==t, state!='V') %>% summarise(L=sum(cases))
        # L_j <- L_j$L
        mu_j <- tmp_j$cases[10]-tmp_j$cases[9]
        r <- beta_matrix_no_interv[i,j]*(alpha/(alpha+mu_i/N_i))*(S_i/N_j)*((1-m_i)/eta+m_i/(gamma+mu_j/N_j))
        out <- rbind(out, tibble(vto=strategy, k_percent=x, t=t, i=i, j=j, r=r))
      }
    }
  }
}
}

# pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/R_i_ea.pdf', 12, 8)
out %>% group_by(vto, k_percent, t, i) %>% 
  summarise(R_it=sum(r)) %>% 
  ungroup() %>% 
  mutate(k_percent=factor(k_percent)) %>% 
  ggplot(aes(t, R_it, color=k_percent))+
  geom_line()+
  facet_wrap(i~vto, scales = 'free_y')+
  geom_hline(yintercept = 1, linetype='dashed')+
  labs(title = 'Elderly-->adults')+
theme_bw()
# dev.off()

pdf('/Users/shai/Dropbox (BGU)/Apps/Overleaf/COVID19/R_i_ae.pdf', 12, 8)
out_ae %>% group_by(k_percent,t,i) %>% 
  summarise(R_it=sum(r)) %>% 
  ungroup() %>% 
  mutate(k_percent=factor(k_percent)) %>% 
  ggplot(aes(t, R_it, color=k_percent))+
  geom_line()+
  facet_wrap(~i, scales = 'free_y')+
  geom_hline(yintercept = 1, linetype='dashed')+
  labs(title = 'Adults-->elderly')+
  theme_bw()
dev.off()



# For MOST report ---------------------------------------------------------

H_control <- 
  country_tbl %>% 
  select(-X) %>% 
  filter(state=='H') %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(k==0) %>% 
  filter(time==max(times)) %>% 
  select(k, SD, age_group, cases, vto)

H_vaccine <- 
  country_tbl %>% 
  select(-X) %>% 
  filter(state=='H') %>% 
  filter(from=='juveniles_adults_elderly') %>% 
  filter(time==max(times)) %>% 
  select(k, SD, age_group, cases, vto)

png('plots for MOSt reports/Heff_uniform_SD.png', 2400, 1600, res = 200)
left_join(H_vaccine, H_control, by=c("SD","age_group", "vto")) %>% 
  select(SD, vto, age_group, k_con=k.y, k_vacc=k.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  group_by(SD, vto, k_con, k_vacc) %>% 
  summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
  mutate(k_vacc=as.factor(k_vacc)) %>% 
  mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
  filter(k_vacc!=0) %>% # This is unnecessary
  ggplot(aes(x=SD, y=H_eff, color=k_vacc))+
  geom_line(size=1)+
  facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly then adults', adults_elderly='Adults then elderly')))+
  scale_x_continuous(breaks = seq(0,1,0.25)) + 
  scale_y_continuous(breaks = seq(0,50,10)) +
  labs(x='% reduction in social contacts', y='% reduction in hospitalizations\n compared to no vaccines', color='Vaccines\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank())
dev.off()


H_control <- 
  country_tbl %>% 
  select(-X) %>% 
  filter(state=='H') %>% 
  filter(from!='juveniles_adults_elderly') %>% 
  filter(k==0) %>% 
  filter(time==max(times)) %>% 
  select(from, k, SD, age_group, cases, vto)

H_vaccine <- 
  country_tbl %>% 
  select(-X) %>% 
  filter(state=='H') %>% 
  filter(from!='juveniles_adults_elderly') %>% 
  filter(time==max(times)) %>% 
  select(from,k, SD, age_group, cases, vto)

png('plots for MOSt reports/Heff_non_uniform_SD.png', 2400, 1600, res = 200)
left_join(H_vaccine, H_control, by=c("from","SD","age_group","vto")) %>% 
  select(from, SD, vto, age_group, k_con=k.y, k_vacc=k.x, cases_con=cases.y, cases_vacc=cases.x) %>% 
  # distinct(from, vto)
  group_by(from, SD, vto, k_con, k_vacc) %>% 
  summarise(tot_cases_con=sum(cases_con), tot_cases_vac=sum(cases_vacc)) %>% 
  mutate(k_vacc=as.factor(k_vacc)) %>% 
  mutate(H_eff=(1-tot_cases_vac/tot_cases_con)*100) %>% 
  filter(k_vacc!=0) %>% # This is unnecessary
  ggplot(aes(x=SD, y=H_eff, color=k_vacc))+
  geom_line(size=1)+
  facet_wrap(~vto, labeller = labeller(vto=c(elderly_adults='Elderly-->adults; (SD for adults)', adults_elderly='Adults-->elderly; (SD for elderly)')))+
  scale_x_continuous(breaks = seq(0,1,0.25)) + 
  scale_y_continuous(breaks = seq(0,50,10)) +
  labs(x='% reduction in social contacts', y='% reduction in hospitalizations\n compared to no vaccines', color='Vaccines\n per day')+
  theme_bw() + 
  theme(axis.text = element_text(size=16, color='black'),
        axis.title = element_text(size=16, color='black'),
        panel.grid.minor = element_blank())
dev.off()


