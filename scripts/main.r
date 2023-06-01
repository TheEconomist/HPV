library(ggplot2)
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(devtools)
library(rjags)
library(prime) #install_github("lshtm-vimc/prime")

data = read_excel("source-data/wuenic2021rev_hpv-estimates.xlsx")
load(file = "source-data/data.pop.rda") # from PRIME, here: https://github.com/lshtm-vimc/prime/blob/master/data/data.pop.rda
income_level = read_excel("source-data/country-incomes.xlsx")



# cleaning & merging ------------------------------------------------------

##' @description function to merge the data from WHO with data from PRIME.
##' Combining the country list from the PRIME data 
##' (so as to not run the model on a country not covered by the model)
##' with the coverage rate data from WHO. Also cleans columns.
##' 
##' @param data WHO data frame of country coverage rates by year and category
##' @param data.pop PRIME data frame of country vaccination cohort size by age/year
##' @param yr vector, years for analysis
##' @return data frame of country, cohort size, and vaccine coverage rate.
##' 
clean_data = function(data, data.pop, yr){
  # filtering to full cohort coverage rates and year of choice
  data %>% filter(`vaccine-description` == "HPV Vaccination program coverage, last dose, females" & 
                    year %in% yr) -> df
  
  # getting country codes for countries available in PRIME dataset
  data.pop %>% 
    filter(year %in% yr) %>% 
    select(1:3) %>% 
    distinct() -> country_codes
  
  # merging PRIME and WHO datasets by countries that appear in PRIME
  df = country_codes %>% left_join(df, by = c("country_code" = "iso-code"))
  
  # cleaning
  df = df %>% select(c(1:3, 5:10))
  df[["coverage"]][is.na(df[["coverage"]])] = 0
  colnames(df)[3] <- "country"
  
  return (df)
}



# PRIME function ----------------------------------------------------------

##' @description function to run the PRIME model, getting estimates for 
##' cost/cancer prevented, lives saved under the current coverage rate & 
##' under the 90% coverage rate targeted by WHO.
##' 
##' @param df data frame of countries and coverage rates
##' @param year numeric, year for analysis
##' @return data frame of cancer prevented, lives saved, cost of program, and
##' future costs prevented by vaccination for each country under the current and 
##' ideal (90%) coverage rates

prime_model = function(df, year){
  
  n = nrow(df)
  
  # creating empty dataframe to populate with results
  results = data.frame(country = rep(0,n),
                       cohort_size = rep(0,n),
                       current_cov = rep(0,n),
                       curr_vacc_cohort_size = rep(0,n),
                       future_cov = rep(0.90,n),
                       future_vacc_cohort_size = rep(0,n),
                       curr_cc_prev = rep(0,n), 
                       curr_mort_prev = rep(0,n),
                       curr_cost = rep(0,n), 
                       curr_cost_prev = rep(0,n),
                       proj_cc_prev = rep(0,n), 
                       proj_mort_prev = rep(0,n),
                       proj_cost = rep(0,n), 
                       proj_cost_prev = rep(0,n))
  
  # looping through countries for which PRIME has data
  for(i in 1:n){
    ctry_code = df$country_code[i]
    current_cov = df$coverage[i]/100
    results$country[i] = ctry_code
    results$current_cov[i] = current_cov
    
    # running the model under X year coverage rates
    if(current_cov > 0){
      result_current =  try(RunCountry (ctry_code, 
                                        analyseCosts = TRUE, 
                                        discounting = TRUE,
                                        agevac = 12, 
                                        agecohort = 12, 
                                        vaceff_beforesexdebut = 0.98,
                                        vaceff_aftersexdebut = 0, 
                                        cov = current_cov, 
                                        year_vac = year))
      
      # returning relevant variables from output for X year coverage
      results$curr_cc_prev[i] = try(result_current$undiscounted[6])
      results$curr_mort_prev[i] = try(result_current$undiscounted[7])
      results$curr_cost[i] = try(result_current$undiscounted[3])
      results$curr_cost_prev[i] = try(result_current$undiscounted[4])
      }
    
# running the model under a 90% coverage rate, as opposed to true coverage rate
    if(current_cov > 0.90){
        result_proj = result_current
        } 
    else{
        result_proj = try(RunCountry (ctry_code, 
                                      analyseCosts = TRUE, 
                                      discounting = TRUE,
                                      agevac = 12, 
                                      agecohort = 12, 
                                      vaceff_beforesexdebut = 0.98,
                                      vaceff_aftersexdebut = 0, 
                                      cov = 0.90, 
                                      year_vac = year))}
      
      # returning relevant variables from output for 90% coverage
      results$proj_cc_prev[i] = try(result_proj$undiscounted[6])
      results$proj_mort_prev[i] = try(result_proj$undiscounted[7])
      results$proj_cost[i] = try(result_proj$undiscounted[3])
      results$proj_cost_prev[i] = try(result_proj$undiscounted[4])
      results$future_vacc_cohort_size[i] = try(result_proj$undiscounted[2])
      results$curr_vacc_cohort_size[i] = try(result_proj$undiscounted[2])
      results$cohort_size[i] = try(result_proj$undiscounted[1])
    
    #print(results[i,])
  }

  # converting to numeric where necessary
  results$curr_cc_prev = as.numeric(results$curr_cc_prev)
  results$curr_mort_prev = as.numeric(results$curr_mort_prev)
  results$curr_cost = as.numeric(results$curr_cost)
  results$curr_cost_prev = as.numeric(results$curr_cost_prev)
  results$proj_cc_prev = as.numeric(results$proj_cc_prev)
  results$proj_mort_prev = as.numeric(results$proj_mort_prev)
  results$proj_cost = as.numeric(results$proj_cost)
  results$proj_cost_prev = as.numeric(results$proj_cost_prev)
  results$cohort_size = as.numeric(results$cohort_size)
  results$curr_vacc_cohort_size = as.numeric(results$curr_vacc_cohort_size)
  results$future_vacc_cohort_size = as.numeric(results$future_vacc_cohort_size)
  results$year = year
  
  return (results)
}


# clean PRIME results -----------------------------------------------------

##' @description function to clean PRIME results data frame and merge in 
##' country income level. Removes countries PRIME fails on (Syria, Cuba, etc.)
##' 
##' @param df data frame output from prime_model()
##' @param income_level data frame of World Bank country income levels
##' @return df data frame of cleaned PRIME results and World Bank country income data

clean_prime = function(df, income_level){
  # the current net cost (so cost at the current coverage rates) is equal to the 
  # current cost of vaccination minus current costs prevented by reducing cervical cancer
  df$current_net_cost = df$curr_cost - df$curr_cost_prev
  
  # adding country's income level (most recent World Bank assessments available)
  income_level %>% select(c(1:4)) -> income_level
  left_join(df, income_level, by = c("country" = "Code")) -> df
  
  # cleaning up
  names(df)[names(df) == "Economy"] = "country_name"
  names(df)[names(df) == "Region"] = "region"
  names(df)[names(df) == "Income group"] = "income_group"
  
  # fixing Kosovo 
  df$country_name[191] = "Kosovo"
  df$region[191] = "Europe & Central Asia"
  df$income_group[191] = "Upper middle income"
  
  # removing countries for which PRIME does not work
  df %>% filter(!(country %in% c("ATG", "CUB", "ERI", "GLP", "GRD", "GUF", 
                          "GUM", "MTQ", "NCL", "PRI", "PRK", "PYF",
                          "REU", "SSD", "SYC", "SYR", "VCT", "VEN", "YEM"))) -> df
  
  return (df)
  
}


# 2010 - 2020 data --------------------------------------------------------
##' for each year of interest: calls function to merge WHO and PRIME data for that
##' year, calls function to run PRIME model to get estimates, and calls function
##' to clean the results data and merge country income data
##'

years = c(2010:2020)

for (year in years){
  df = clean_data(data, data.pop, year)
  results = prime_model(df, year)
  results_cleaned = clean_prime(results, income_level)
  
  write.csv(results_cleaned, paste0("output-data/yearly-results/results_", year, ".csv"))
}



# 2021 data ---------------------------------------------------------------
# done separately because because of projected calculations columns
df_2021 = clean_data(data, data.pop, 2021)
results = prime_model(df_2021, 2021)

# calculating additional cervical cancer cases and deaths prevented
results$cancer_prevented = results$proj_cc_prev - results$curr_cc_prev
results$deaths_prevented = results$proj_mort_prev - results$curr_mort_prev

# the current net cost (so cost at the current coverage rates) is equal to the 
# current cost of vaccination minus current costs prevented by reducing cervical cancer
results$current_net_cost = results$curr_cost - results$curr_cost_prev

# the future net cost (so cost at the, for instance, 90% coverage rate) is equal to
# the cost of vaccination at 90% coverage minus the cost prevented at 90% coverage
results$future_net_cost = results$proj_cost - results$proj_cost_prev

# the additional cost is what countries would spend beyond what they are currently spending
# to achieve their current coverage rate if they ramped up to 90% coverage rate
results$additional_cost = (results$future_net_cost - results$current_net_cost) - (results$curr_cost - results$curr_cost_prev)

# adding country's income level (most recent World Bank assessments available)
income_level %>% select(c(1:4)) -> income_level

left_join(results, income_level, by = c("country" = "Code")) -> results

# cleaning up
names(results)[names(results) == "Economy"] = "country_name"
names(results)[names(results) == "Region"] = "region"
names(results)[names(results) == "Income group"] = "income_group"

# fixing Kosovo 
results$country_name[191] = "Kosovo"
results$region[191] = "Europe & Central Asia"
results$income_group[191] = "Upper middle income"

results %>% filter(!(country %in% c("ATG", "CUB", "ERI", "GLP", "GRD", "GUF", 
                                    "GUM", "MTQ", "NCL", "PRI", "PRK", "PYF",
                                    "REU", "SSD", "SYC", "SYR", "VCT", "VEN", "YEM"))) -> results

write.csv(results, "output-data/results_2021.csv")



# 2021 demographics w 2019 coverage rates ---------------------------------
# done separately because of mix and match
df_mixed = clean_data(data, data.pop, 2019)
results = prime_model(df_mixed, 2021)
results_cleaned = clean_prime(results, income_level)

write.csv(results_cleaned, "output-data/mixed_results.csv")


# 2020s data using WHO coverage goals and 2021 flat rates ----------------------

# Determine how much each country needs to improve each year to hit 90% by 2030
df_2021 = clean_data(data, data.pop, 2021)
df_2021 %>% select(1, 2, 3, 4, 9) %>%
  mutate(year = 2021,
         yr_increase = (90 - coverage) / 9) -> df_2021

df_future_WHO = df_2021

# Calculate the "needed" coverage rate for each year
for(i in 1:9){
  df_2021 %>% 
    mutate(year = year + i,
           coverage = case_when(coverage >= 90 ~ coverage,
                                TRUE ~ pmin(coverage + yr_increase * i, 90))) -> df_temp
  df_future_WHO = rbind(df_future_WHO, df_temp)
}

df_future_curr = df_2021

# Create future years coverage rates just copying the 2021 rate
for(i in 1:9){
  df_2021 %>% 
    mutate(year = year + i) -> df_temp
  
  df_future_curr = rbind(df_future_curr, df_temp)
}


# 2021 - 2030 data --------------------------------------------------------
##' for each year of interest: calls function to merge WHO and PRIME data for that
##' year, calls function to run PRIME model to get estimates, and calls function
##' to clean the results data and merge country income data
##'


for (i in 2021:2030){
  df_WHO = filter(df_future_WHO, year == i)
  df_curr = filter(df_future_curr, year == i)
  
  results_WHO = prime_model(df_WHO, i)
  results_WHO_cleaned = clean_prime(results_WHO, income_level)
  
  results_curr = prime_model(df_curr, i)
  results_curr_cleaned = clean_prime(results_curr, income_level)
  
  write.csv(results_WHO_cleaned, paste0("output-data/projections_with_gradual_90pct_rate/results_WHO_", i, ".csv"))
  write.csv(results_curr_cleaned, paste0("output-data/projections_with_2021_rate/results_curr_", i, ".csv"))
  
}


# 2010-2030 max number of cases/deaths calculations ----------------------------

# Adjust the prime model such that we are just calculating under 100% coverage
# rate with 100% efficacy
prime_model_100 = function(df, year){
  
  n = nrow(df)
  
  # creating empty dataframe to populate with results
  results = data.frame(country = rep(0,n),
                       current_cov = rep(0,n),
                       curr_cc_prev = rep(0,n),
                       curr_mort_prev = rep(0,n))
  
  # looping through countries for which PRIME has data
  for(i in 1:n){
    ctry_code = df$country_code[i]
    current_cov = df$coverage[i]/100
    results$country[i] = ctry_code
    results$current_cov[i] = 1
    
    # running the model under X year coverage rates
    result_current =  try(RunCountry (ctry_code,
                                      analyseCosts = TRUE,
                                      discounting = TRUE,
                                      agevac = 12,
                                      agecohort = 12,
                                      vaceff_beforesexdebut = 1,
                                      vaceff_aftersexdebut = 0,
                                      cov = 1,
                                      year_vac = year))
    
    # returning relevant variables from output for X year coverage
    results$curr_cc_prev[i] = try(result_current$undiscounted[6])
    results$curr_mort_prev[i] = try(result_current$undiscounted[7])
    
  }
  # converting to numeric where necessary
  results$curr_cc_prev = as.numeric(results$curr_cc_prev)
  results$curr_mort_prev = as.numeric(results$curr_mort_prev)
  results$year = year
  
  return (results)
}

# This cleans the PRIME output for 100% coverage (minor adjustments to previous function)
clean_prime_100 = function(df, income_level){
  
  # adding country's income level (most recent World Bank assessments available)
  income_level %>% select(c(1:4)) -> income_level
  left_join(df, income_level, by = c("country" = "Code")) -> df
  
  # cleaning up
  names(df)[names(df) == "Economy"] = "country_name"
  names(df)[names(df) == "Region"] = "region"
  names(df)[names(df) == "Income group"] = "income_group"
  
  # fixing Kosovo
  df$country_name[191] = "Kosovo"
  df$region[191] = "Europe & Central Asia"
  df$income_group[191] = "Upper middle income"
  
  # removing countries for which PRIME does not work
  df %>% filter(!(country %in% c("ATG", "CUB", "ERI", "GLP", "GRD", "GUF",
                                 "GUM", "MTQ", "NCL", "PRI", "PRK", "PYF",
                                 "REU", "SSD", "SYC", "SYR", "VCT", "VEN", "YEM"))) -> df
  
  return (df)
  
}


# Run for 2021-2030
for (i in 2021:2030){
  df_WHO = filter(df_future_WHO, year == i)
  results_100 = prime_model_100(df_WHO, i)
  results_100_cleaned = clean_prime_100(results_100, income_level)
  
  write.csv(results_100_cleaned, paste0("output-data/case_and_death_estimates_without_vaccine/results_100_", i, ".csv"))
}

# Run for 2010-2020 
for (i in 2010:2020){
  df = clean_data(data, data.pop, i)
  results_100 = prime_model_100(df, i)
  results_100_cleaned = clean_prime_100(results_100, income_level)
  
  write.csv(results_100_cleaned, paste0("output-data/case_and_death_estimates_without_vaccine/results_100_", i, ".csv"))
}
