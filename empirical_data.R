library(tidyverse)
library(readxl)
library(lubridate)

setwd('~/GitHub/ecomplab/COVID19_vaccine_model/Data/')
vac <- read_excel('vaccination_percent.xlsx')
vac <- read_excel('vaccination_cumulative.xlsx')
vac %>% gather(key = dose, value = percent, -date) %>% 
  mutate(date=as.Date(date, "%d-%m-%Y")) %>% 
  ggplot(aes(date, percent, color=dose))+geom_line()


vac %>%
 mutate(Diff_growth = dose1 - lag(dose1),
         vac_rate_percent = (Diff_growth)/dose1 * 100)  %>% 
  mutate(date=as.Date(date, "%d-%m-%Y")) %>% 
  filter(date>='2021-02-01') %>%
  
   # print(n=Inf)
  ggplot(aes(x=date, y=vac_rate_percent))+geom_line(size=1.3)
