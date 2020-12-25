#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")
print(sessionInfo())

library(magrittr)
library(tidyverse)
library(socialmixr)

# args <- c(4192476, 12)

if (length(commandArgs(trailingOnly=TRUE))==0) {
  stop('No arguments were found!')
} else {
  args <- commandArgs(trailingOnly=TRUE)
  JOB_ID <- as.numeric(args[1])
  max_weeks <- as.numeric(args[2])
}

# Get parameters of that run
run_summary <- read.csv(paste(JOB_ID,'run_summary.csv',sep='_'))
current_country <- run_summary[2,2]
for (i in 3:nrow(run_summary)){
  assign(run_summary[i,1], as.numeric(run_summary[i,2]))
}

# Get data
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
pdf(plotname('uniform_sd.pdf'), 12, 8)
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

pdf(plotname('targeted_sd.pdf'), 12, 8)
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

pdf(plotname('ea_ae_time_to_r_p.pdf'), 12, 8)
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

pdf(plotname('ea_ae_r_p.pdf'), 12, 8)
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
  filter(SD%in%c(0,0.5,0.8)) %>% 
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

pdf(plotname('ea_ae_r_SD0.pdf'), 12, 8)
toplot %>% 
  filter(SD==0, vto=='elderly_adults') %>% 
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

pdf(plotname('ea_ae_r_SD05.pdf'), 12, 8)
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

pdf(plotname('ea_ae_r_SD08.pdf'), 12, 8)
toplot %>% 
  filter(SD==0.8, vto=='elderly_adults') %>% 
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


# Arrange in a folder -----------------------------------------------------
print('Organizing into a folder...')
files <- list.files(pattern = as.character(JOB_ID))
files <- files[-which(str_detect(files, 'run_summary'))]
dir.create(path = as.character(JOB_ID))
sapply(files, function(z) file.copy(z, as.character(JOB_ID)))
sapply(files, unlink)









