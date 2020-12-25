#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")
print(.libPaths())
print(sessionInfo())

library(deSolve)
library(magrittr)
library(tidyverse)
library(socialmixr)

# args <- c('Israel', 1, 0.4, 7, 6.4, 1.5)

if (length(commandArgs(trailingOnly=TRUE))==0) {
  stop('No arguments were found!')
} else {
  args <- commandArgs(trailingOnly=TRUE)
  current_country <- args[1]
  sim_weeks <- as.numeric(args[2])
  m <- as.numeric(args[3])
  gamma <- 1/as.numeric(args[4])
  alpha=1/as.numeric(args[5])
  eta <- 1/as.numeric(args[6])
}

source('initialize_country.R')

JOB_ID <- Sys.getenv("JOB_ID")

run_summary <- tibble(parameter=c('JOB_ID','country','N','sim_weeks','m','beta','q','gamma','alpha','eta'),
                      value=c(JOB_ID,current_country,N,sim_weeks,m,beta,q,gamma,alpha,eta))
write_csv(run_summary, paste(JOB_ID,'run_summary.csv',sep='_'))
write_csv(as_tibble(beta_matrix_no_interv), paste(JOB_ID,'beta_matrix_no_interv.csv',sep='_'))


source('functions.R')

# Model definition --------------------------------------------------------
# vto is vaccine target order
# vtp is the current vaccine target population

SEIARUHV_2 <- function (t, x, beta_matrix, k, vto,...){
  S <- x[sindex]
  E <- x[eindex]
  I <- x[iindex]
  A <- x[aindex]
  R <- x[rindex] 
  U <- x[uindex] 
  H <- x[hindex] 
  V <- x[vindex] 
  
  S[S<0] <- 0
  E[E<0] <- 0
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
    vtp <- vto[[vacc_order]]
    L <- round(S+E+A+U)[vtp]
    names(L) <- paste('L',vtp,sep='')
    # print(L)
    
    # In case there are not enough people to vaccinate switch to the next target.
    if(any(L<k)){
      # print(paste(t, 'Switching vaccine target group', vacc_order))
      vacc_order <<- vacc_order+1 # Vacc_order is a global parameter
      if(vacc_order>length(vto)){
        # print('No more people to vaccinate, stopping vaccination')
        # Sys.sleep(2)
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
  
  dsdt<-dedt<-didt<-dadt<-drdt<-dudt<-dhdt<-dvdt <- NULL
  for (j in 1:9){
    lambda_j <- 0
    for (l in 1:9){
      N_l <- N*Table_1$Proportion[l]
      lambda_j <- lambda_j + beta_matrix[j,l]*((I[l]+A[l])/N_l)*S[j]
    }
    L_j <- S[j]+E[j]+A[j]+U[j]
    # In case there is no one to vaccinate - avoid division by zero
    if (L_j<1){
      # cat(paste('L_j=0 for age', j))
      dsdt[j] <- -lambda_j
      dedt[j] <- lambda_j-alpha*E[j]
      didt[j] <- (1-m[j])*alpha*E[j]-eta*I[j]
      dadt[j] <- m[j]*alpha*E[j]-gamma*A[j]
      drdt[j] <- (1-h[j])*eta*I[j]
      dudt[j] <- gamma*A[j]
      dhdt[j] <- h[j]*eta*I[j]
      dvdt[j] <- 0
    } else {
      dsdt[j] <- -lambda_j-mu[j]*S[j]/L_j
      dedt[j] <- lambda_j-alpha*E[j]-mu[j]*E[j]/L_j
      didt[j] <- (1-m[j])*alpha*E[j]-eta*I[j]
      dadt[j] <- m[j]*alpha*E[j]-gamma*A[j] -mu[j]*A[j]/L_j
      drdt[j] <- (1-h[j])*eta*I[j]
      dudt[j] <- gamma*A[j]-mu[j]*U[j]/L_j
      dhdt[j] <- h[j]*eta*I[j]
      dvdt[j] <- (S[j]+E[j]+A[j]+U[j])*mu[j]/L_j
    }
    # if(stop_vaccination==F){
    # if (j %in% vtp){
    #   vaccines_by_state <<- bind_rows(vaccines_by_state, tibble(t=t,j=j,S=S[j],E=E[j],A=A[j],U=U[j],V=dvdt[j],L=L_j))
    # }}
  } # End for loop on j
  
  # cat('\n')
  
  return(list(c(dsdt,dedt,didt,dadt,drdt,dudt,dhdt,dvdt)))
}



# Set up to run the simulation --------------------------------------------

indices <- chunk(1:(n_groups*8),8)
sindex <- indices[[1]] # index numbers for age groups for susceptibles
eindex <- indices[[2]] # index numbers for age groups for exposed
iindex <- indices[[3]] # index numbers for age groups for infected
aindex <- indices[[4]] # index numbers for age groups for asmptomatic
rindex <- indices[[5]] # index numbers for age groups for removed from infected (quarentined and then recovered)
uindex <- indices[[6]] # index numbers for age groups for recovered from asymptomatic
hindex <- indices[[7]] # index numbers for age groups for hospitalized
vindex <- indices[[8]] # index numbers for age groups for vaccinated

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


times <- seq(1, 7*sim_weeks, by = 1) #  1-day time-increments

# Range of vaccine deployment
k_range_percent <- c(0,seq(0.04,0.2,0.02)) # from 0.04% to 0.2% of the population a day

# Range of social distancing strength
SD_list <- seq(0,1,by=0.1)

all_ages <- 'juveniles_adults_elderly'
vto_ea_SDstrat_allall <- list(vto='elderly_adults',SD_ls=SD_list,from=all_ages,to=all_ages)
vto_ae_SDstrat_allall <- list(vto='adults_elderly',SD_ls=SD_list,from=all_ages,to=all_ages)
vto_ea_SDstrat_aall <- list(vto='elderly_adults',SD_ls=SD_list,from='adults',to=all_ages)
vto_ae_SDstrat_eall <- list(vto='adults_elderly',SD_ls=SD_list,from='elderly',to=all_ages)
strat_ls <- list(vto_ea_SDstrat_allall,vto_ae_SDstrat_allall,vto_ae_SDstrat_eall,vto_ea_SDstrat_aall)

vaccine_forcing <- set_forcing(effect = 1, effect_time = times)

# Run simulation ----------------------------------------------------
print('Running simulation')
curr_country_tbl <- create_country_tbl(k_range,strat_ls)
print('Writing results')
write_csv(curr_country_tbl, paste(JOB_ID,'_results_',current_country,'.csv',sep=''))

