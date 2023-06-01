library(ggplot2)
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(scales)


# clean -------------------------------------------------------------------

# NOTE: I THINK WE CAN DELETE THIS BELOW AND JUST READ IN hpv_past_results.csv
# reading in yearly results data frames and combining into one
#file_list = list.files(path ="output-data/yearly-results", full.names = TRUE)

#combined_results = data.frame(matrix(ncol = 24, nrow = 0))

#for(file in file_list){
#  print(substr(file, 28, nchar(file)-4))
#  temp_data = read_csv(file)
#  assign(substr(file, 28, nchar(file)-4), temp_data)
#  combined_results = rbind(combined_results, temp_data)
#}

# reading in 2021 and mixed_results separately, bc different data frames
#results_2021 = read.csv("output-data/results_2021.csv")
#mixed_results = read.csv("output-data/mixed_results.csv")

#combined_results = bind_rows(combined_results, results_2021)
#combined_results %>% select(-c(1, 21)) -> combined_results

hpv_past_results = read.csv("output-data/hpv_past_results.csv")
hpv_2020s_results = read.csv("output-data/hpv_2020s_results.csv")


mixed_results = read.csv("output-data/mixed_results.csv")
results_2021 = read.csv("output-data/results_2021.csv")


# calculations ------------------------------------------------------------

# 1. additional number of cervical cancer incidents and deaths that could have been prevented with a 90%+ coverage rate in 2021
sum(results_2021$proj_cc_prev) - sum(results_2021$curr_cc_prev)

sum(results_2021$proj_mort_prev) - sum(results_2021$curr_mort_prev)

# 2. additional number of cervical cancer incidents and deaths that could have been prevented had the 2019 vaccination rates held in 2021 (could do cumulative with the 2020 cohort)
sum(mixed_results$curr_cc_prev) - sum(results_2021$curr_cc_prev)

sum(mixed_results$curr_mort_prev) - sum(results_2021$curr_mort_prev)



# plots -------------------------------------------------------------------
# 3. number of cervical cancer incidents & deaths prevented by HPV vaccination programs in each year 
df1 = aggregate(combined_results$curr_cc_prev, by = list(combined_results$year), sum)
colnames(df1) = c("Year", "quantity")
df1$aggregate_num = "cervical cancer incidents prevented"

df2 = aggregate(combined_results$curr_mort_prev, by = list(combined_results$year), sum)
colnames(df2) = c("Year", "quantity")
df2$aggregate_num = "cervical cancer deaths prevented"

df3 = aggregate(mixed_results$curr_cc_prev, by = list(mixed_results$year), sum)
colnames(df3) = c("Year", "quantity")
df3$aggregate_num = "cervical cancer incidents prevented (2019 rates)"

df4 = aggregate(mixed_results$curr_mort_prev, by = list(mixed_results$year), sum)
colnames(df4) = c("Year", "quantity")
df4$aggregate_num = "cervical cancer deaths prevented (2019 rates)"

aggregate_df = rbind(df1, df2, df3, df4) 

plot1 = ggplot(aggregate_df, 
               aes(x = factor(Year), 
                   y = quantity, 
                   group = aggregate_num, 
                   color = aggregate_num)) + 
  geom_line() + geom_point() + 
  ggtitle("Cervical Cancer Prevented* by HPV Vaccine per year") +
  ylab("cases/deaths") +
  xlab("Year") +
  labs(caption = "*in the cohort that is supposed to be vaccinated in that year") +
  theme(legend.title = element_blank()) 

ggsave("plots/cervical_cancer_per_year.svg", plot1)



# 4. vaccine coverage rates by country income-level, 2021
coverage_by_income = aggregate(combined_results$current_cov, 
                               by = list(combined_results$year, 
                                         combined_results$income_group), mean)
colnames(coverage_by_income) = c("Year", "income_group", "avg_coverage")

cohort_size_year = aggregate(combined_results$cohort_size,
                             by = list(combined_results$year, 
                                        combined_results$income_group), sum)
 
plot2 = coverage_by_income %>% 
  ggplot(aes(x = factor(Year), 
             y = avg_coverage, 
             group = income_group, 
             color = income_group)) + 
  geom_line() +
  ggtitle("HPV Vaccine Coverage Rates by Country Income Group") +
  ylab("Coverage Rate") +
  xlab("Year") + 
  ylim(0, 1) +
  geom_hline(yintercept = 0.90, linetype = "dashed") +
  geom_text(aes(5, 0.90, label = "90% coverage by 2030 to eliminate cervical cancer", vjust = -1))

ggsave("plots/vax_rates_by_income.svg", plot2)

# 5. 
Paho = c("Argentina","Bahamas, The", "Barbados", "Belize", "Bolivia",
         "Brazil", "Canada", "Chile", "Colombia", "Costa Rica",
         "Dominican Republic", "Ecuador", "El Salvador", "Guatemala",
         "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
         "Panama", "Paraguay", "Peru", "St. Lucia", "Suriname",
         "Trinidad and Tobago", "United States", "Uruguay")

combined_results %>% 
  mutate(income_group_rev = case_when(country_name %in% Paho ~ "PAHO",
                                      TRUE ~ income_group)) -> combined_results

coverage_by_income1 = aggregate(combined_results$current_cov, 
                                by = list(combined_results$year, 
                                          combined_results$income_group_rev), mean)
colnames(coverage_by_income1) = c("Year", "income_group", "avg_coverage")

plot2rev = coverage_by_income1 %>% 
  ggplot(aes(x = factor(Year), 
             y = avg_coverage, 
             group = income_group, 
             color = income_group)) + 
  geom_line() +
  ggtitle("HPV Vaccine Coverage Rates by Country Income Group") +
  ylab("Coverage Rate") +
  xlab("Year") + 
  ylim(0, 1) +
  geom_hline(yintercept = 0.90, linetype = "dashed") +
  geom_text(aes(5, 0.90, label = "90% coverage by 2030 to eliminate cervical cancer", vjust = -1))


# 6. CC incidence prevented by country income group
cc_incidence_by_income = aggregate(combined_results$curr_cc_prev, 
                                   by = list(combined_results$year, 
                                             combined_results$income_group), sum)
colnames(cc_incidence_by_income) = c("Year", "income_group", "num_people")

# cohort_size_year = aggregate(combined_results$cohort_size,
#                              by = list(combined_results$year, 
#                                        combined_results$income_group), sum)

cc_incidence_by_income$rate = (cc_incidence_by_income$num_people / cohort_size_year$x) * 100000

plot3 = cc_incidence_by_income %>% 
  ggplot(aes(x = factor(Year), 
             y = rate, 
             group = income_group, 
             color = income_group)) + 
  geom_line() +
  ggtitle("cervical cancer incidence per 100,000 prevented by country income group for each vaccination cohort") +
  ylab("cases") +
  xlab("Year")

ggsave("plots/cc_prevented_income.svg", plot3)

# 7. CC deaths prevented by income group 
cc_deaths_by_income = aggregate(combined_results$curr_mort_prev, 
                                by = list(combined_results$year, 
                                          combined_results$income_group), sum)
colnames(cc_deaths_by_income) = c("Year", "income_group", "num_people")

# cohort_size_year = aggregate(combined_results$cohort_size,
#                              by = list(combined_results$year, 
#                                        combined_results$income_group), sum)

cc_deaths_by_income$rate = (cc_deaths_by_income$num_people / cohort_size_year$x) * 100000

plot4 = cc_deaths_by_income %>% 
  ggplot(aes(x = factor(Year), 
             y = rate, 
             group = income_group, 
             color = income_group)) + 
  geom_line() +
  ggtitle("cervical cancer deaths per 100,000 prevented by country income group for each vaccination cohort") +
  ylab("cases") +
  xlab("Year")

ggsave("plots/cc_deaths_prevented_income.svg", plot4)


# ## Countries that haven't rolled it out
combined_results %>% 
  group_by(country_name) %>%
  mutate(vax_rolled_out = as.numeric(max(current_cov > 0))) %>%
  #ungroup() %>%
  group_by(income_group_rev) %>%
  summarise(pct_rolled_out = mean(vax_rolled_out))

combined_results %>% 
  filter(year == 2019, current_cov > 0) -> coverage_2019

ggplot(coverage_2019, aes(x = reorder(country_name,current_cov), y = current_cov)) +
  geom_bar(stat = "identity") + coord_flip()

############

# reading in yearly results data frames and combining into one
file_list = list.files(path ="~/Downloads/Archive/WHO", full.names = TRUE)

combined_results_WHO = data.frame(matrix(ncol = 24, nrow = 0))

for(file in file_list){
  print(substr(file, 28, nchar(file)-4))
  temp_data = read_csv(file)
  assign(substr(file, 28, nchar(file)-4), temp_data)
  combined_results_WHO = rbind(combined_results_WHO, temp_data)
}

file_list = list.files(path ="~/Downloads/Archive/Current", full.names = TRUE)

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
                             rep('Keep 2021 Coverage Rate', 1760))) -> combined_results

write.csv(combined_results, file = 'hpv_2020s_results.csv', row.names = FALSE)
