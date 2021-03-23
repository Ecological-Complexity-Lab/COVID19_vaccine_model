#! /gpfs0/shai/projects/R4/R-4.0.3/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.0.3/lib64/R/library")
print(.libPaths())
print(sessionInfo())

library(deSolve)
library(magrittr)
library(tidyverse)
library(socialmixr)

# args <- 7; JOB_ID <- 270; max_weeks=15
JOB_ID <- Sys.getenv("JOB_ID")
if (length(commandArgs(trailingOnly=TRUE))==0) {
  stop('No arguments were found!')
} else {
  args <- commandArgs(trailingOnly=TRUE)
  e_id <- as.numeric(args[1])
}

write_lines(paste0('START EVENT LOG -- exp id: ',e_id), paste(JOB_ID,'log.txt',sep='_'), append = F)

experiments <- read_csv('experiments.csv')
experiment <- as.data.frame(subset(experiments, exp_id==e_id))
for (i in 2:ncol(experiment)){
  v <- names(experiment)[i]
  print(v)
  if(is.numeric(experiment[,i])){
    assign(v, as.numeric(experiment[,i]))
  } else {
    assign(v, experiment[,i])
  }
  write_lines(paste(v,experiment[,i],sep=': '), paste(JOB_ID,'log.txt',sep='_'), append = T)
}
write_lines('======================================', paste(JOB_ID,'log.txt',sep='_'), append = T)


source('initialize_country.R')
source('functions.R')


# beta depends on the probability of successful infection q and encounter rate, c: beta=qc.
# Spread the effect of beta across the contact matrix:
q <- beta/mean(contact_matrix_sym)
beta_matrix_no_interv <- q*contact_matrix_sym
diag(beta_matrix_no_interv) <- diag(beta_matrix_no_interv)/2 # Need to halve the diaginal to avoid double contacts within the same age group


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
  
  # Set vaccination deployment
  if(stop_vaccination==F){
    switch_vacc_group <- F
    vtp <- vto[[vacc_order]] #vtp := vaccine target population; vto := vaccine targeting order (the strategy)
  
    # Did we reach the limit of people that are willing to vaccinate
    write_lines(paste0('[',round(t),'] Total vaccinated in vtp: ',round(sum(V[vtp])),' out of ',round(prop_vacc*sum(N_age_groups[vtp])),' left: ',round(prop_vacc*sum(N_age_groups[vtp])-sum(V[vtp]))), paste(JOB_ID,'log.txt',sep='_'), append = T)
    if(sum(V[vtp])>=prop_vacc*sum(N_age_groups[vtp])){
      write_lines(paste0('[',round(t),']', ' Switching vaccine target group: reached max prop willing to vaccinate. ',round(sum(V[vtp])),'>=',round(prop_vacc*sum(N_age_groups[vtp]))), paste(JOB_ID,'log.txt',sep='_'), append = T)
      switch_vacc_group <- T
    }
    
    if (switch_vacc_group == F){
      # Check if there are people left to vaccinate
      L <- round(S+E+P+A+U)[vtp] # number of people that can be vaccinated
      if(all(L<=1000)){ # 1000 is a threshold for moving to next target group. Chosen arbitratily
        write_lines(paste0('[',round(t),']',' Switching vaccine target group: less than 1000 people to vaccinte in all age groups'), paste(JOB_ID,'log.txt',sep='_'), append = T)
        switch_vacc_group <- T
      }
    }
    
    # If switching to next vaccination target
    if(switch_vacc_group){
      vacc_order <<- vacc_order+1 # Vacc_order is a global parameter
      if(vacc_order>length(vto)){
        write_lines(paste0('[',round(t),']', ' No more people to vaccinate, stopping vaccination campaign.'), paste(JOB_ID,'log.txt',sep='_'), append = T)
        stop_vaccination <<- T
      } else {
        vtp <- vto[[vacc_order]]
      }
    }
  }
  
  if(stop_vaccination==F){
    # Need to calculate L again to asvoid a stupid error in the code (does not affect results)
    L <- round(S+E+P+A+U)[vtp] # number of people that can be vaccinated
    names(L) <- paste('L',vtp,sep='')
    mu <- rep(0,n_groups) # Initialize 9 age groups
    # If the total number of people left to vaccinate is less than the daily deployment, it will cause an error, so choose the minimum
    deployment <- min(k, sum(L))
    # Deply according to group size.
    mu[vtp] <- vaccine_forcing(t)*deployment*(L/sum(L)) # Divide the daily vaccines between the groups
    write_lines('/-----', paste(JOB_ID,'log.txt',sep='_'), append = T)
    write_lines(paste0('[',round(t),'] L=[',paste(round(L), collapse = ','),'] ',' VTP=[',paste(vtp, collapse = ','),']'), paste(JOB_ID,'log.txt',sep='_'), append = T)
    write_lines(paste0('[',round(t),'] k=',k, ' sum(L)=',sum(L),' deployment=',deployment), paste(JOB_ID,'log.txt',sep='_'), append = T)
    write_lines(paste0('[',round(t),'] mu=[',paste(round(mu), collapse = ','),']'), paste(JOB_ID,'log.txt',sep='_'), append = T)
   } else {
    write_lines(paste0('[',round(t),'] not giving more vaccines mu=0.'), paste(JOB_ID,'log.txt',sep='_'), append = T)
    # print('not giving more vaccines mu=0')
    mu <- rep(0,n_groups)
  }
  
  
  dsdt<-dedt<-dpdt<-didt<-dadt<-drdt<-dudt<-dhdt<-dvdt <- NULL
  for (j in 1:9){
    lambda_j <- 0
    for (l in 1:9){
      N_l <- N_age_groups[l]
      lambda_j <- lambda_j + beta_matrix[j,l]*((I[l]+b_p*A[l]+b_p*P[l])/N_l)*S[j]
    }
    # L_j <- S[j]+E[j]+A[j]+P[j]+U[j]
    mu[j] <- mu[j]/(S[j]+E[j]+A[j]+P[j]+U[j]) # Define mu as per-capita vaccination rate
    
    dsdt[j] <- -lambda_j-mu[j]*S[j]
    dedt[j] <- lambda_j-alpha*E[j]-mu[j]*E[j]
    dpdt[j] <- alpha*E[j]-phi*P[j]-mu[j]*P[j]
    didt[j] <- (1-m[j])*phi*P[j]-eta*I[j]
    dadt[j] <- m[j]*phi*P[j]-gamma*A[j]-mu[j]*A[j]
    drdt[j] <- (1-h[j])*eta*I[j]
    dudt[j] <- gamma*A[j]-mu[j]*U[j]
    dhdt[j] <- h[j]*eta*I[j]
    dvdt[j] <- mu[j]*(S[j]+E[j]+A[j]+P[j]+U[j])
    
    # if(stop_vaccination==F){
    # if (j %in% vtp){
    # vaccines_by_state <<- bind_rows(vaccines_by_state, tibble(t=t,j=j,S=S[j],E=E[j],P=P[j],A=A[j],U=U[j],V=V[j],L=L_j, mu=mu[j]))
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
  mutate(h=read_csv('hospitalization_rate.csv')$percent_hospitalized, # This is the probability of hospitalization
         # The proportion of infected people at each age group, from
         # https://datadashboard.health.gov.il/COVID-19/general accessed
         # 2020-20-09 Used to distribute the actively infected (active_infected)
         # at the beginning of the simulation.
         prop_infected_total=c(0.106,0.189,0.209,0.142,0.128,0.101,0.067,0.034,0.024)) 

m <- rep(m, 9) # A vector for prob of asymptomatic infections.
h <- Table_1$h # probability of hospitalization

# Population size and initial size of age groups
# active_infected <- 100 # Taken from experiments.csv
yinit <- initialize_population(N)
yinit[iindex] <- round(active_infected*Table_1$prop_infected_total)
N <- sum(yinit)
age_structure$yinit <- yinit[1:n_groups] # This is used later to calculate proportions

N_age_groups <- N*Table_1$Proportion

times <- seq(1, 7*sim_weeks, by = 1) #  1-day time-increments

# Range of vaccine deployment
# k_range_percent <- c(0,seq(0.04,0.2,0.04)) # from 0.04% to 0.2% of the population a day
k_range_percent <- c(0,seq(k_min,k_max,length.out = 5)) # from 0.04% to 0.2% of the population a day

# Range of social distancing strength
SD_list <- seq(0,1,by=0.1)

all_ages <- 'juveniles_adults_elderly'
vto_ea_SDstrat_allall <- list(vto='elderly_adults',SD_ls=SD_list,from=all_ages,to=all_ages)
vto_ae_SDstrat_allall <- list(vto='adults_elderly',SD_ls=SD_list,from=all_ages,to=all_ages)
vto_ea_SDstrat_aall <- list(vto='elderly_adults',SD_ls=SD_list,from='adults',to=all_ages)
vto_ae_SDstrat_eall <- list(vto='adults_elderly',SD_ls=SD_list,from='elderly',to=all_ages)
vto_ea_SDstrat_eall <- list(vto='elderly_adults',SD_ls=SD_list,from='elderly',to=all_ages)
vto_ae_SDstrat_aall <- list(vto='adults_elderly',SD_ls=SD_list,from='adults',to=all_ages)

strat_ls <- list(vto_ea_SDstrat_allall,
                 vto_ae_SDstrat_allall,
                 vto_ea_SDstrat_aall,
                 vto_ae_SDstrat_eall,
                 vto_ea_SDstrat_eall,
                 vto_ae_SDstrat_aall)

vaccine_forcing <- set_forcing(effect = vacc_eff, effect_time = times)

# k_range_percent <- c(0.12)
# strat_ls <- list(list(vto='elderly_adults',SD_ls=0,from=all_ages,to=all_ages))

 # Run simulation ----------------------------------------------------
print('Running simulation')
write_lines('--- Running simulation ---', paste(JOB_ID,'log.txt',sep='_'), append = T)
curr_country_tbl <- create_country_tbl(k_range_percent,strat_ls)
print('Writing results')
write_lines('--- Writing results ---', paste(JOB_ID,'log.txt',sep='_'), append = T)
write_csv(curr_country_tbl, paste(JOB_ID,'_results_',current_country,'.csv',sep=''))


# Write parameters --------------------------------------------------------
print('Writing parameters summary')
run_summary <- data.frame(parameter=c('JOB_ID', names(experiment)), value=c(JOB_ID,unlist(experiment[1,])))
rownames(run_summary) <- NULL
write_csv(run_summary, paste(JOB_ID,'run_summary.csv',sep='_'))
write_csv(as_tibble(beta_matrix_no_interv), paste(JOB_ID,'beta_matrix_no_interv.csv',sep='_'))
