# Bayesian Final Project

# SET UP
library(tidyverse)
library(foreach)
library(janitor)

# READ IN DATA
setwd("~/Desktop/A3SR/bayesian/final_project")
life_expectancy_data_raw <- read_csv("data/life_expectancy_data.csv")
gdp_data_raw <- read_csv("data/gdp_data.csv")

# PLOTTING DATA TO UNDERSTAND DISTRIBUTIONS
# Checking Relation between life expectancy and predictor variables
pairs(life_expectancy_data_raw[, 4:12])
pairs(life_expectancy_data_raw[, c(4, 13:22)])

#we have decided to log any variable that does not look linearly related to out outcome 
#(given that they all seem to have a funneling issue)

# looks like some variables are related differently with the outcome, going to separate
# developed and developing countries

# PREPPING GDP DATA FOR MERGING
gdp_year_range <- gdp_data_raw[, c(1, 42:57)]

gdp_clean <- gdp_year_range %>% 
  rename(Country = `Country Name`) %>% 
  pivot_longer(cols = !Country,
               names_to = "Year",
               values_to = "gdp") %>% 
  mutate(Year = as.numeric(Year))

# MERGING DATASETS
life_expectancy_data <- left_join(life_expectancy_data_raw, gdp_clean, by = c("Country", "Year"))

# DROPPING VARIABLES WITH TOO MANY MISSING VALUES
life_expectancy_data <- life_expectancy_data %>% 
  clean_names() %>% 
  dplyr::select(-hepatitis_b, -total_expenditure, -population, -income_composition_of_resources,
                -schooling)

# SEPARATING DATA BY YEAR
foreach(year = 2000:2015) %do% {
  temp <- life_expectancy_data_raw %>% filter(Year == year)
  temp_developed <- temp %>% filter(Status == "Developed")
  temp_developed <- temp_developed %>% na.omit()
  temp_developed <- temp_developed  %>% 
    clean_names() %>% 
    mutate(
      country = as.factor(country),
      year = as.factor(year),
      status = as.factor(status),
      life_expectancy = life_expectancy - mean(life_expectancy, na.rm = T),
      log_adult_mortality = log(adult_mortality) - mean(log(adult_mortality), na.rm = T),
      infant_deaths = infant_deaths - mean(infant_deaths, na.rm = T),
      alcohol = alcohol - mean(alcohol, na.rm = T),
      percentage_expenditure = percentage_expenditure - mean(percentage_expenditure, na.rm = T),
      hepatitis_b = hepatitis_b - mean(hepatitis_b, na.rm = T),
      measles = measles - mean(measles, na.rm = T),
      bmi = mean(bmi, na.rm = T),
      under_five_deaths = under_five_deaths - mean(under_five_deaths, na.rm = T),
      polio = polio - mean(polio, na.rm = T),
      total_expenditure = total_expenditure - mean(total_expenditure, na.rm = T),
      diphtheria = diphtheria - mean(diphtheria, na.rm = T),
      hiv_aids = hiv_aids - mean(hiv_aids, na.rm = T),
      log_gdp = log(gdp) - mean(log(gdp), na.rm = T),
      population = population - mean(population, na.rm = T),
      thinness_1_19_years = thinness_1_19_years - mean(thinness_1_19_years, na.rm = T),
      thinness_5_9_years = thinness_5_9_years - mean(thinness_5_9_years, na.rm = T),
      income_composition_of_resources = income_composition_of_resources - 
        mean(income_composition_of_resources, na.rm = T))
  
  temp_developing <- temp %>% filter(Status == "Developing")
  temp_developing <- temp_developing %>% na.omit()
  temp_developing <- temp_developing %>% 
    clean_names() %>% 
    mutate(
      country = as.factor(country),
      year = as.factor(year),
      status = as.factor(status),
      life_expectancy = life_expectancy - mean(life_expectancy, na.rm = T),
      log_adult_mortality = log(adult_mortality) - mean(log(adult_mortality), na.rm = T),
      infant_deaths = infant_deaths - mean(infant_deaths, na.rm = T),
      alcohol = alcohol - mean(alcohol, na.rm = T),
      percentage_expenditure = percentage_expenditure - mean(percentage_expenditure, na.rm = T),
      hepatitis_b = hepatitis_b - mean(hepatitis_b, na.rm = T),
      measles = measles - mean(measles, na.rm = T),
      bmi = mean(bmi, na.rm = T),
      under_five_deaths = under_five_deaths - mean(under_five_deaths, na.rm = T),
      polio = polio - mean(polio, na.rm = T),
      total_expenditure = total_expenditure - mean(total_expenditure, na.rm = T),
      diphtheria = diphtheria - mean(diphtheria, na.rm = T),
      hiv_aids = hiv_aids - mean(hiv_aids, na.rm = T),
      log_gdp = log(gdp) - mean(log(gdp), na.rm = T),
      population = population - mean(population, na.rm = T),
      thinness_1_19_years = thinness_1_19_years - mean(thinness_1_19_years, na.rm = T),
      thinness_5_9_years = thinness_5_9_years - mean(thinness_5_9_years, na.rm = T),
      income_composition_of_resources = income_composition_of_resources - 
        mean(income_composition_of_resources, na.rm = T))
  
  assign(paste0("life_exp_", year,"_developed"), temp_developed)
  assign(paste0("life_exp_", year,"_developing"), temp_developing)
}


