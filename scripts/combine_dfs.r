library(dplyr)
library(readr)

#no_vax_results <- read_csv("output-data/no_vax_results.csv")
#hpv_past_results_df <- read_csv("output-data/hpv_past_results_df.csv")
#hpv_2020s_results <- read_csv("output-data/hpv_2020s_results.csv")

############ Begin with past results #############
file_list = list.files(path ="output-data/yearly-results", full.names = TRUE)

combined_results = data.frame(matrix(ncol = 24, nrow = 0))

for(file in file_list){
  print(substr(file, 28, nchar(file)-4))
  temp_data = read_csv(file)
  assign(substr(file, 28, nchar(file)-4), temp_data)
  combined_results = rbind(combined_results, temp_data)
}

# reading in 2021 and mixed_results separately, bc different data frames
results_2021 = read.csv("output-data/results_2021.csv")

combined_results = bind_rows(combined_results, results_2021)
combined_results %>% select(-c(1, 21)) -> combined_results

combined_results %>%
  select(country_name, region, income_group, year, current_cov,
         curr_cc_prev, curr_mort_prev) %>%
  rename(coverage = current_cov, cancer_prevented = curr_cc_prev,
         deaths_prevented = curr_mort_prev) %>%
  mutate(assumption_type = "Historical Results") -> hpv_20102021


######## Now read in future results ###########
file_list = list.files(path ="output-data/projections_with_gradual_90pct_rate", full.names = TRUE)

combined_results_WHO = data.frame(matrix(ncol = 24, nrow = 0))

for(file in file_list){
  print(substr(file, 28, nchar(file)-4))
  temp_data = read_csv(file)
  assign(substr(file, 28, nchar(file)-4), temp_data)
  combined_results_WHO = rbind(combined_results_WHO, temp_data)
}

file_list = list.files(path ="output-data/projections_with_2021_rate", full.names = TRUE)

combined_results_curr = data.frame(matrix(ncol = 24, nrow = 0))

for(file in file_list){
  print(substr(file, 28, nchar(file)-4))
  temp_data = read_csv(file)
  assign(substr(file, 28, nchar(file)-4), temp_data)
  combined_results_curr = rbind(combined_results_curr, temp_data)
}

combined_results = rbind(combined_results_WHO, combined_results_curr)

combined_results %>%
  select(country_name, region, income_group, year, cohort_size,
         current_cov, curr_cc_prev, curr_mort_prev) %>%
  rename(coverage = current_cov, cancer_prevented = curr_cc_prev, 
         deaths_prevented = curr_mort_prev) %>%
  mutate(assumption_type = c(rep('Attain WHO Coverage Goal', 1760), 
                             rep('Keep 2021 Coverage Rate', 1760))) -> hpv_20202030

#write.csv(combined_results, file = 'hpv_2020s_results.csv', row.names = FALSE)

######## Now read in 100% coverage results ###########

file_list = list.files(path ="output-data/case_and_death_estimates_without_vaccine", full.names = TRUE)

combined_results_100 = data.frame(matrix(ncol = 24, nrow = 0))

for(file in file_list){
  print(substr(file, 28, nchar(file)-4))
  temp_data = read_csv(file)
  assign(substr(file, 28, nchar(file)-4), temp_data)
  combined_results_100 = rbind(combined_results_100, temp_data)
}

combined_results_100 %>%
  rename(possible_cancer_cases = curr_cc_prev,
         possible_cancer_deaths = curr_mort_prev) %>%
  select(country_name, year, 
         possible_cancer_cases, possible_cancer_deaths) -> no_vax_results

hpv_20102021 %>%
  left_join(no_vax_results) -> hpv_past_results_df

hpv_20202030 %>%
  left_join(no_vax_results) -> hpv_2020s_results

write.csv(hpv_past_results_df, file = 'output-data/hpv_past_results.csv', row.names = FALSE)
write.csv(hpv_2020s_results, file = 'output-data/hpv_2020s_results.csv', row.names = FALSE)
