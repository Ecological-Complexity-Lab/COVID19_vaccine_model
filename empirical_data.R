library(tidyverse)
library(readxl)
library(lubridate)
library(R0)

current_country <- 'Israel'
gamma=1/7

# Define functions
# Define functions
covid19_country <- function(the_country){
  x <- covid19%>%
    filter(country==the_country) %>%
    mutate(running_day=row_number())
  return(x)
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
obs_data %>% 
  filter(running_day<=early_period_limit) %>%
  ggplot(aes(x=running_day, y=cases))+
  geom_point()+geom_line()+
  geom_vline(xintercept = early_period_limit, linetype='dashed')+
  theme_bw()

data_early <- obs_data %>% filter(running_day<=early_period_limit)

# Estimate beta using R0
mGT<-generation.time("gamma", c(4.5, 2.5)) # Values for generation time from Dattner et al 2021 PLOS Comp Biol.
y <- obs_data$cases
names(y) <- obs_data$date
R0 <- est.R0.EG(y, mGT, begin=1, end=early_period_limit)
beta <- R0$R*gamma # beta=R0*gamma
print(beta)

# Estimate beta using a linear model
model_lm <- lm(log(cases) ~ running_day, data=data_early)
s <- summary(model_lm)
coef <- coef(model_lm)[2]
# The coefficient is actually beta-gamma so calculate beta:
beta <- coef+gamma
print(beta)

# Check the fit of the model by plottign predicted values based on model estimation
data_early$pred_lm <- exp(predict(model_lm))
ggplot(data_early, aes(y=cases, x=running_day))+
  geom_point()+geom_line()+
  geom_line(aes(x = running_day, y = pred_lm), color='blue', size=1.3)+
  theme_bw()





# ggplot(ildata_early, aes(y=cases, x=running_day))+
#   geom_point()+geom_line()+
#   geom_line(aes(x = running_day, y = pred_lm), color='blue', size=1.3)+
#   theme_bw()
