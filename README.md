
## Cervical Cancer & HPV Vaccines

---- description ----
* This repo contains data and scripts to recreate the analysis and charts in the article "Cheap vaccines could prevent millions of deaths from cervical cancer. A turning-point looms in the campaign to eliminate the disease"
* article: https://www.economist.com/graphic-detail/2023/05/31/cheap-single-dose-hpv-vaccines-could-save-millions-of-lives
* author: Daniella Raz

---- source-data ----
* wuenic2021rev_hpv-estimates.xlsx
* data.pop.rda
* country_codes.csv
* country-incomes.xlsx
* cc_incidence_rates.csv
* 2019_2021_all_vaccine_rates.csv

---- scripts ----
* main.r
* analysis.r
* combine_dfs.r

---- output-data ----
* hpv_2020s_results.csv
* hpv_past_results.csv
* mixed_results.csv
* results_2021.csv
* folder: yearly-results
* folder: projections_with_gradual_90pct_rate
* folder: projections_with_2021_rate
* folder: case_and_death_estimates_without_vaccine

---- sources ----
* The World Health Organization / UNICEF
* Papillomavirus Rapid Interface for Modelling and Economics (PRIME): London School of Hygiene & Tropical Medicine, Mark Jit, Marc Brisson, Kaja Abbas, and Han Fu 

#### variable definitions

by age 15, first dose:

by age 15, last dose:

coverage, first dose, females:

coverage, last dose, females:

country = country code

cohort_size = total number of girls who are in the cohort that should be
getting vaccinated

current_cov = coverage in X year

curr_vacc_cohort_size = number vaccinated in that cohort

future_cov = 90% coverage rate

future_vacc_cohort_size = number vaccinated in cohort under new coverage
rate

curr_cc_prev: cervical cancer cases prevented under the current coverage

curr_mort_prev: deaths from cervical cancer prevented under the current
coverage

curr_cost: current cost of the program

curr_cost_prev: current costs prevented by reducing cervical cancer

proj_cc_prev, proj_mort_prev, proj_cost, proj_cost_prev are the same as
above but under the "projected" 90% coverage rate the WHO says is
necessary for elimination of cervical cancer
