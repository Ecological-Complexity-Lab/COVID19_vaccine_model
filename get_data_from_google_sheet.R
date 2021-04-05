library(xtable)
library(googlesheets4)
wb <- 'https://docs.google.com/spreadsheets/d/1k_31mMWl0WXTOw1a-59QuRIcL9P68z7Y2FfEBpSwFtc/edit#gid=1083629423'
experiments <- googlesheets4::read_sheet(wb, sheet = 1)
write_csv(experiments, 'experiments.csv')

Table_S2 <- googlesheets4::read_sheet(wb, sheet = 2)
Table_S2$Value <- unlist(Table_S2$Value)
Table_S2
xtable(Table_S2, caption = 'Study parameters.', label = 'parameters')
