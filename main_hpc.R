#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")
print(.libPaths())
print(sessionInfo())

library(deSolve)
library(magrittr)
library(tidyverse)
library(socialmixr)

# args <- c('Israel', 5, 0.4, 7, 3, 2.1, 1.5, 0.95)

if (length(commandArgs(trailingOnly=TRUE))==0) {
  stop('No arguments were found!')
} else {
  args <- commandArgs(trailingOnly=TRUE)
  current_country <- args[1]
  sim_weeks <- as.numeric(args[2])
  m <- as.numeric(args[3])
  gamma <- 1/as.numeric(args[4])
  alpha=1/as.numeric(args[5])
  phi <- 1/as.numeric(args[6])
  eta <- 1/as.numeric(args[7])
  vacc_eff <- as.numeric(args[8])
  prop_vacc <- as.numeric(args[9])
}

source('initialize_country.R')

JOB_ID <- Sys.getenv("JOB_ID")

source('functions.R')

write_lines('event log', paste(JOB_ID,'log.txt',sep='_'), append = F)

# Model definition --------------------------------------------------------
# vto is vaccine target order
# vtp is the current vaccine target population

full_model <- function (t, x, beta_matrix, k, vto,...){
  S <- x[sindex]
  E <- x[eindex]
  P <- x[pindex]
  I <- x[iindex]
  A <- x[aindex]
  R <- x[rindex] 
  U <- x[uindex] 
  H <- x[hindex] 
  V <- x[vindex] 
  
  S[S<0] <- 0
  E[E<0] <- 0
  P[P<0] <- 0
  I[I<0] <- 0
  A[A<0] <- 0
  R[R<0] <- 0
  U[U<0] <- 0
  H[H<0] <- 0
  V[V<0] <- 0
  
  # SD signal. SD_forcing 0 means no SD. 1 means full lockdown
  beta_matrix <- beta_matrix*(1-SD_forcing(t))
  
  # Select the vaccine target population
  if(stop_vaccination==F){
    vtp <- vto[[vacc_order]] #vtp := vaccine target population; vto := vaccine targeting order (the strategy)
    L <- round(S+E+P+A+U)[vtp] # number of people that can be vaccinated
    names(L) <- paste('L',vtp,sep='')
    
    switch_vacc_group <- F
    # In case there are not enough people to vaccinate switch to the next target.
    if(any(L<k)){
      write_lines(paste('[',ceiling(t),']', ' any(L<k): Switching vaccine target group: ',names(which(L<k)),'<',k,sep=''), paste(JOB_ID,'log.txt',sep='_'), append = T)
      switch_vacc_group <- T
    }
    # If reached the max proportion of the people willing to vaccinate.
    if(sum(V[vtp])>=prop_vacc*sum(N_age_groups[vtp])){
      write_lines(paste('[',ceiling(t),']', ' Switching vaccine target group: reached max prop willing to vaccinate. ',sum(V[vtp]),'>=',ceiling(prop_vacc*sum(N_age_groups[vtp])),sep=''), paste(JOB_ID,'log.txt',sep='_'), append = T)
      switch_vacc_group <- T
    }
    if(switch_vacc_group){
      vacc_order <<- vacc_order+1 # Vacc_order is a global parameter
      if(vacc_order>length(vto)){
        write_lines(paste('[',ceiling(t),']', ' No more people to vaccinate, stopping vaccination.',sep=''), paste(JOB_ID,'log.txt',sep='_'), append = T)
        stop_vaccination <<- T
      } else {
        # print('in here')
        vtp <- vto[[vacc_order]]
      }
    }
  }
 
  # Vaccine signal
  mu <- rep(0,9) # Initialize 9 age groups
  if(stop_vaccination==F){
    mu[vtp] <- vaccine_forcing(t)*k/length(vtp) # Divide the daily vaccines between the groups
  }
  
  # print(t)
  
  dsdt<-dedt<-dpdt<-didt<-dadt<-drdt<-dudt<-dhdt<-dvdt <- NULL
  for (j in 1:9){
    lambda_j <- 0
    for (l in 1:9){
      N_l <- N_age_groups[l]
      lambda_j <- lambda_j + beta_matrix[j,l]*((I[l]+A[l]+P[l])/N_l)*S[j]
    }
    L_j <- S[j]+E[j]+A[j]+P[j]+U[j]
    dsdt[j] <- -lambda_j-mu[j]*S[j]/L_j
    dedt[j] <- lambda_j-alpha*E[j]-mu[j]*E[j]/L_j
    dpdt[j] <- alpha*E[j]-phi*P[j]-mu[j]*P[j]/L_j
    didt[j] <- (1-m[j])*phi*P[j]-eta*I[j]
    dadt[j] <- m[j]*phi*P[j]-gamma*A[j]-mu[j]*A[j]/L_j
    drdt[j] <- (1-h[j])*eta*I[j]
    dudt[j] <- gamma*A[j]-mu[j]*U[j]/L_j
    dhdt[j] <- h[j]*eta*I[j]
    dvdt[j] <- mu[j]

    # if(stop_vaccination==F){
    # if (j %in% vtp){
    #   vaccines_by_state <<- bind_rows(vaccines_by_state, tibble(t=t,j=j,S=S[j],E=E[j],A=A[j],U=U[j],V=dvdt[j],L=L_j))
    # }}
  } # End for loop on j
  
  return(list(c(dsdt,dedt,dpdt,didt,dadt,drdt,dudt,dhdt,dvdt)))
}


# Set up to run the simulation --------------------------------------------

indices <- chunk(1:(n_groups*9),9)
sindex <- indices[[1]] # index numbers for age groups for susceptibles
eindex <- indices[[2]] # index numbers for age groups for exposed
pindex <- indices[[3]] # index numbers for age groups for presymptomatic
iindex <- indices[[4]] # index numbers for age groups for infected
aindex <- indices[[5]] # index numbers for age groups for asmptomatic
rindex <- indices[[6]] # index numbers for age groups for removed from infected (quarentined and then recovered)
uindex <- indices[[7]] # index numbers for age groups for recovered from asymptomatic
hindex <- indices[[8]] # index numbers for age groups for hospitalized
vindex <- indices[[9]] # index numbers for age groups for vaccinated

Table_1 <- age_structure %>% 
  select(i=age_group_id,
         Age=age_group,
         Proportion) %>% 
  mutate(h=read_csv('hospitalization_rate_of_symptomatic.csv')$mean, # This is the probability of hospitalization
         # The proportion of infected people at each age group, from
         # https://datadashboard.health.gov.il/COVID-19/general accessed
         # 2020-20-09 Used to distribute the actively infected (active_infected)
         # at the beginning of the simulation.
         prop_infected_total=c(0.106,0.189,0.209,0.142,0.128,0.101,0.067,0.034,0.024)) 

m <- rep(m, 9) # A vector for prob of asymptomatic infections.
h <- Table_1$h # probability of hospitalization

# Population size and initial size of age groups
active_infected <- 100
yinit <- initialize_population(N)
yinit[iindex] <- round(active_infected*Table_1$prop_infected_total)
N <- sum(yinit)
age_structure$yinit <- yinit[1:n_groups] # This is used later to calculate proportions

N_age_groups <- N*Table_1$Proportion

times <- seq(1, 7*sim_weeks, by = 1) #  1-day time-increments

# Range of vaccine deployment
k_range_percent <- c(0,seq(0.04,0.2,0.04)) # from 0.04% to 0.2% of the population a day
# k_range_percent <- c(0,seq(1,2,length.out = 11))

# Range of social distancing strength
SD_list <- seq(0,1,by=0.1)

all_ages <- 'juveniles_adults_elderly'
vto_ea_SDstrat_allall <- list(vto='elderly_adults',SD_ls=SD_list,from=all_ages,to=all_ages)
vto_ae_SDstrat_allall <- list(vto='adults_elderly',SD_ls=SD_list,from=all_ages,to=all_ages)
vto_ea_SDstrat_aall <- list(vto='elderly_adults',SD_ls=SD_list,from='adults',to=all_ages)
vto_ae_SDstrat_eall <- list(vto='adults_elderly',SD_ls=SD_list,from='elderly',to=all_ages)
strat_ls <- list(vto_ea_SDstrat_allall,vto_ae_SDstrat_allall,vto_ae_SDstrat_eall,vto_ea_SDstrat_aall)

vaccine_forcing <- set_forcing(effect = vacc_eff, effect_time = times)

# Run simulation ----------------------------------------------------
print('Running simulation')
write_lines('--- Running simulation ---', paste(JOB_ID,'log.txt',sep='_'), append = T)
curr_country_tbl <- create_country_tbl(k_range_percent,strat_ls)
print('Writing results')
write_lines('--- Writing results ---', paste(JOB_ID,'log.txt',sep='_'), append = T)
write_csv(curr_country_tbl, paste(JOB_ID,'_results_',current_country,'.csv',sep=''))


# Write parameters --------------------------------------------------------
print('Writing parameters summary')
run_summary <- data.frame(parameter=c('JOB_ID',
                                      'country',
                                      'N',
                                      'sim_weeks',
                                      'm',
                                      'beta',
                                      'q',
                                      'gamma',
                                      'alpha',
                                      'phi',
                                      'eta',
                                      'e',
                                      'initial_infected',
                                      'prop_vacc'),
                          value=c(JOB_ID,
                                  current_country,
                                  N,
                                  sim_weeks,
                                  m[1],
                                  beta,
                                  q,
                                  gamma,
                                  alpha,
                                  phi,
                                  eta,
                                  vacc_eff,
                                  active_infected,
                                  prop_vacc))
write_csv(run_summary, paste(JOB_ID,'run_summary.csv',sep='_'))
write_csv(as_tibble(beta_matrix_no_interv), paste(JOB_ID,'beta_matrix_no_interv.csv',sep='_'))
