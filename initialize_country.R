print(paste('Initializing for',current_country))

# Define population size
population_sizes <- tibble(country=c('Israel', 'Belgium', 'Finland', 'Germany',  'Italy', 'Luxembourg', 'Netherlands', 'Poland'),
                           N=c(8712900,11539.326*1000,5532.159*1000,83517.05*1000,60550.09*1000,615.73*1000,17097.12*1000,37887.77*1000))
N <-  subset(population_sizes, country==current_country)$N # Population size

# Load age structure
age_structure <- read_csv('country_age_structure.csv')
age_structure$Proportion <- age_structure[[current_country]]

# Re-estimate contact rates
contacts <- as_tibble(polymod$contacts)
participants <- as_tibble(polymod$participants)
country_contacts <- contacts %>% 
  left_join(participants, by='part_id') %>% 
  filter(country==ifelse(current_country=='Israel','Italy',current_country)) %>%  # For Israel use Italy's contact matrix. For other countries use the country's matrix
  # Define which contacts are relevant for COVID19
  mutate(COVID19_contact=ifelse((phys_contact==2 & duration_multi>=3) | phys_contact==1, T, F)) %>%
  filter(COVID19_contact==T)

contact_matrix <- 
  country_contacts %>% 
  dplyr::select(part_id, cont_id, cnt_age_exact, part_age) %>% 
  drop_na() %>%  # Remove rows with no exact ages.
  distinct(part_id, cont_id, part_age, cnt_age=cnt_age_exact) %>% 
  # Categorize participants into age groups
  mutate(part_age_group=case_when(
    (part_age>=0 & part_age<=9) ~ "0-9",
    (part_age>=10 & part_age<=19) ~ "10-19",
    (part_age>=20 & part_age<=29) ~ "20-29",
    (part_age>=30 & part_age<=39) ~ "30-39",
    (part_age>=40 & part_age<=49) ~ "40-49",
    (part_age>=50 & part_age<=59) ~ "50-59",
    (part_age>=60 & part_age<=69) ~ "60-69",
    (part_age>=70 & part_age<=79) ~ "70-79",
    (part_age>=80) ~ "80+")
  ) %>% 
  # Categorize contacts into age groups
  mutate(cnt_age_group=case_when(
    (cnt_age>=0 & cnt_age<=9) ~ "0-9",
    (cnt_age>=10 & cnt_age<=19) ~ "10-19",
    (cnt_age>=20 & cnt_age<=29) ~ "20-29",
    (cnt_age>=30 & cnt_age<=39) ~ "30-39",
    (cnt_age>=40 & cnt_age<=49) ~ "40-49",
    (cnt_age>=50 & cnt_age<=59) ~ "50-59",
    (cnt_age>=60 & cnt_age<=69) ~ "60-69",
    (cnt_age>=70 & cnt_age<=79) ~ "70-79",
    (cnt_age>=80) ~ "80+")
  ) %>% 
  # Count number of participants and contacts
  group_by(part_age_group, cnt_age_group) %>% 
  summarise(num_contacts=length(cont_id), num_participants=n_distinct(part_id)) %>% 
  # Calculate mean
  mutate(mean_contacts=num_contacts/num_participants) %>% 
  # Make a matrix
  dplyr::select(part_age_group, cnt_age_group, mean_contacts) %>% 
  spread(cnt_age_group, mean_contacts)
contact_matrix <- data.matrix(as.data.frame(contact_matrix)[,-1])
rownames(contact_matrix) <- colnames(contact_matrix)
# At least 1 contact per combination
contact_matrix[is.na(contact_matrix)] <- 1

# Make the matrix symmetric
contact_matrix_sym <- (contact_matrix+t(contact_matrix))/2
