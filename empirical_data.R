library(tidyverse)
library(readxl)
library(lubridate)
library(R0)

current_country <- 'Luxembourg'

# Define functions
# Define functions
covid19_country <- function(the_country){
  x <- covid19%>%
    filter(country==the_country) %>%
    mutate(running_day=row_number())
  return(x)
}

R0_country <- function(the_country,...){
  # Estimate R0 from data
  mGT<-generation.time("gamma", c(3, 1.5))
  x <- covid19_country(the_country)
  y <- x$cases
  names(y) <- x$date
  R0 <- est.R0.EG(y, mGT, ...)
  R0
}

# Get data
covid19 <- read_csv('data/time_series_covid19_confirmed_global.csv')
covid19 %<>%
  dplyr::select(-`Province/State`, -Lat, -Long) %>%
  rename(country=`Country/Region`) %>%
  gather(key=d, value, -country) %>%
  group_by(country, d) %>%
  summarise(cases=sum(value)) %>%
  separate(d, into=c('month','day','year')) %>%
  mutate(date=as_date(paste(year,month,day,sep='-'))) %>%
  arrange(country,date)

early_period_limit <- 35
obs_data <- covid19_country(current_country) %>% filter(cases>0) %>% mutate(running_day=row_number())
# ggplot(ildata, aes(x=running_day, y=cases))+
#   geom_point()+geom_line()+
#   geom_vline(xintercept = early_period_limit, linetype='dashed')+
#   theme_bw()

# Estimate beta using R0
mGT<-generation.time("gamma", c(3, 1.5))
y <- obs_data$cases
names(y) <- obs_data$date
R0 <- est.R0.EG(y, mGT, begin=1, end=early_period_limit)
beta <- R0$R*(1/7) # beta=R0*gamma
print(beta)

# Plot observed vsnd model data -------------------------------------------
data_early <- obs_data %>% filter(running_day<=early_period_limit)

# Compare fit to Israeli data. We first ran manually a simulation with a single
# infected individual, no SD and no vaccination. We arbitrartily and manually
# gave it the faux JOB_ID 100.
d <- read_csv('100_results_Israel.csv')
pdf('beta_fitting.pdf', 6, 6)
d %>% 
  filter(SD==0, k_percent==0, from=='juveniles_adults_elderly', vto=='elderly_adults') %>% 
  filter(time<=35) %>%
  filter(state == 'I') %>%
  group_by(time, state) %>% 
  summarise(cases=sum(cases)) %>% 
  dplyr::select(running_day=time, cases) %>% 
  mutate(Source='Model') %>% 
  bind_rows(data_early %>% ungroup() %>%  dplyr::select(running_day, cases) %>%  mutate(Source='Data')) %>% 
  ggplot(aes(running_day, cases, color=Source))+
  geom_line(size=1.1)+
  geom_point(size=3)+
  scale_color_manual(values=c('purple','black'))+
  scale_x_continuous(n.breaks = 6)+
  scale_y_continuous(n.breaks = 6)+
  theme_bw()+
  labs(x='Day', y='Infected cases')+
  theme(panel.grid = element_blank(),
        legend.position = c(0.2,0.8),
        axis.title = element_text(size=14, color = 'black'), 
        axis.text = element_text(size=14, color = 'black'))
dev.off()


# Estimate beta using a linear model
# 
# model_lm <- lm(log(cases) ~ running_day, data=ildata_early) 
# s <- summary(model_lm)
# beta_gamma_coef <- coef(model_lm)[2]
# The coefficient is actually beta-gamma so calculate beta:
# beta <- beta_gamma_coef+gamma

# Check the fit of the model by plottign predicted values based on model estimation
# ildata_early$pred_lm <- exp(predict(model_lm))
# ggplot(ildata_early, aes(y=cases, x=running_day))+
#   geom_point()+geom_line()+
#   geom_line(aes(x = running_day, y = pred_lm), color='blue', size=1.3)+
#   theme_bw()

# ggplot(ildata_early, aes(y=cases, x=running_day))+
#   geom_point()+geom_line()+
#   geom_line(aes(x = running_day, y = pred_lm), color='blue', size=1.3)+
#   theme_bw()
