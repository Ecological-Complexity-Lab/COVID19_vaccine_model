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
 mutate(Diff_growth = dose2 - lag(dose2),
         Rate_percent = (Diff_growth )/dose2 * 100)  %>%  print(n=Inf) # growth rate in percent
