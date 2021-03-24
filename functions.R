# Super-general parameters ------------------------------------------------
ages <- c(9, 19, 29, 39, 49, 59, 69, 79, 80) # upper end of age classes
juveniles <- 1:2
adults <- 3:6
elderly <- 7:9
n_groups <- length(ages) # Number of groups

# Model states
model_states <- factor(c('S','E','P','I','A','R','U','H','V','L'), levels = c('S','E','P','I','A','R','U','H','V','L'))
state_colors <- tibble(state=model_states,
                       col=factor(c('yellow','navy','orange','#c91212','pink','dark green','green','black','gray','cyan'), 
                                  levels=c('yellow','navy','orange','#c91212','pink','dark green','green','black','gray','cyan')))

gather_experiments <- function(all_params=F){
  files <- list.files(pattern = 'run_summary')
  experiments <- NULL
  for (f in files){
    suppressMessages(tmp <- read_csv(f))
    tmp %<>% 
      spread(parameter, value) %>% 
      select(JOB_ID, exp_id, everything())
    experiments %<>% bind_rows(tmp)
  }
  experiments %<>% relocate(comment, .after = last_col())
  if (all_params){ return(experiments) } else {
    return(experiments %>% select(JOB_ID, exp_id, current_country, sim_weeks, b_p, k_min, k_max, prop_vacc, active_infected))
  }
}

# Functions to run the model ----------------------------------------------

chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))

initialize_population <- function(N){
  x <- c(S=rep(0,n_groups),
         E=rep(0,n_groups),
         P=rep(0,n_groups),
         I=rep(0,n_groups),
         A=rep(0,n_groups),
         R=rep(0,n_groups),
         U=rep(0,n_groups),
         H=rep(0,n_groups),
         V=rep(0,n_groups)) #yinit is initial_values
  x[1:n_groups] <- round(Table_1$Proportion*N)
  return(x)
}

set_forcing <- function(effect, effect_time, plot_signal=F){
  signal <- data.frame(times = times, effect = 0) # Initialize with times (global parameter of the simulation)
  signal$effect[effect_time] <- effect # vaccine efficiency
  forcing <- approxfun(signal, rule = 2) # Get the forcing function
  if (plot_signal){
    x <- NULL
    for (i in signal$times){
      x <- c(x, forcing(i))
    }
    print(tibble(t=signal$times, value=x) %>% ggplot(aes(t,x))+geom_line()+labs(x='Time',y='Effect'))
  }
  return(forcing)
}

age_group_name_to_ls <- function(age_name,res_type){
  #takes the name of age groups in the format below and converts to list
  #the format is the names of the age groups divided by _, eg c('elderly_adults','juveniles_adults','adults')
  #res type is either list or vector - vector returns all the age groups as a simple vector, list returns a list of each age group as a separate inner list
  res <- NULL
  name_ls <- strsplit(age_name,'_')
  name_ls <- name_ls[[1]]
  for (v in name_ls){
    if (res_type=='list'){
      res <- append(res,list(get(v)))
    }
    else if (res_type=='vector'){
      tmp <- get(v)
      res <- c(res,tmp)
    }
  }
  return(res)
}

sd_specific <- function(x,from,to,force){
  #applies a force of SD to the relevant age groups in the matrix
  x[from,to] <- x[from,to]*(1-force)
  x[to,from] <- x[from,to]
  return(x)
}

get_run_res <- function(k_test=0,vtos,SD=0,mtx=beta_matrix_no_interv){
  #runs the actual model for a given k, vto, and matrix, and returns the parsed results
  #if running with a changed contact matrix, SD should be 0
  SD_forcing <<- set_forcing(SD, times)
  vacc_order <<- 1; vaccines_by_state <<- NULL; stop_vaccination <<- F
  pop <- as_tibble(ode(y=yinit,
                       times=times,
                       func=full_model,
                       beta_matrix=mtx,
                       k=k_test,
                       vto=vtos))
  pop_parse <- parse_model_results(pop)
  return (pop_parse)
}

parse_model_results <- function(x){
  x %>%
    mutate_all(as.numeric) %>% 
    gather(key = 'state_age', value=cases, -time) %>%
    mutate(state=case_when(
      str_detect(state_age,'S') ~ "S",
      str_detect(state_age,'E') ~ "E",
      str_detect(state_age,'P') ~ "P",
      str_detect(state_age,'I') ~ "I",
      str_detect(state_age,'A') ~ "A",
      str_detect(state_age,'R') ~ "R",
      str_detect(state_age,'U') ~ "U",
      str_detect(state_age,'H') ~ "H",
      str_detect(state_age,'V') ~ "V")) %>% 
    mutate(state=factor(state, levels = model_states)) %>%
    mutate(age_group_id=parse_number(state_age)) %>%
    left_join(age_structure, by='age_group_id') %>% 
    left_join(state_colors, by='state') %>%
    dplyr::select(time,state,age_group,cases,yinit, col) 
}





create_country_tbl <- function(k_range_percent,strat_ls){
  #returns a table with k as number of vaccines and as percent of population, age group, heff, sd, run_type (the value that goes in the legend), total h effective for all ages per k, and the ages the SD is applied to (from and to)
  #k_range_percent - series of numbers representing the % pop vaccinated per day - if you want a control of k0, 0 needs to be in the list!
  #vto list is a list of vtos in the format of the names divided by _, eg c('elderly_adults','juveniles_adults','adults')
  #strat_ls is a list of lists. each internal list has 4 indexes - vto, from, to and SD_ls
  fin_tbl <- NULL
  #for (vto_strat in vto_ls){
  for (strat in strat_ls){
    vto <- age_group_name_to_ls(strat$vto,'list')
    print(strat)
    write_lines(paste('--> strategy: ',strat,sep=''), paste(JOB_ID,'log.txt',sep='_'), append = T)
    for (SD in strat$SD_ls){
      to <- age_group_name_to_ls(strat$to,'vector')
      from <- age_group_name_to_ls(strat$from,'vector')
      m <- sd_specific(beta_matrix_no_interv,from,to,SD)
      for (k_percent in k_range_percent){
        write_lines(paste('|----> k_percent: ',k_percent,sep=''), paste(JOB_ID,'log.txt',sep='_'), append = T)
        k <- round(k_percent/100*N) # Calculate number of vaccines per day (kappa)
        k_res <- NULL
        k_res <- get_run_res(k_test=k, vtos=vto, SD=0, mtx=m) # The SD here refers to the external forcing function. the real SD is given directly in the contact matrix
        k_res$k <- k
        k_res$k_percent <- k_percent
        k_res$SD <- SD
        k_res$vto <- strat$vto
        k_res$from <- strat$from
        k_res$to <- strat$to
        fin_tbl <- bind_rows(fin_tbl,k_res)
      }
    }
  }
  return(fin_tbl)
}


# Functions for plotting --------------------------------------------------

plotname <- function(x){
  paste(JOB_ID,x,sep = '_')
}




