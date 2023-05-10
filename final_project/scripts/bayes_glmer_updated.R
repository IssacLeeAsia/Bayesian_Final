life_expectancy_data_raw <- read.csv("data/life_expectancy_data.csv")
gdp_data_raw <- read.csv("data/gdp_data.csv")
# SET UP
library(tidyverse)
library(foreach)
library(janitor)
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
  rename(Country = Country.Name) %>% 
  pivot_longer(cols = !Country,
               names_to = "Year",
               values_to = "gdp")
Years <- gsub('X', '', gdp_clean$Year)
gdp_clean <- gdp_clean %>%
  select(-Year) %>%
  mutate(Year = Years) %>%
  mutate(Year = as.numeric((Year)))

# MERGING DATASETS
life_expectancy_data <- left_join(life_expectancy_data_raw, gdp_clean, by = c("Country", "Year"))

# DROPPING VARIABLES WITH TOO MANY MISSING VALUES
life_expectancy_data <- life_expectancy_data %>% 
  clean_names() %>% 
  dplyr::select(-hepatitis_b, -total_expenditure, -population, -income_composition_of_resources,
                -schooling, -bmi, -hiv_aids)

# STANDARDIZE FUNCTION
standardize <- function(x) {
  x.std <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  x.std
}

# SEPARATING DATA 
# developed
life_expectancy_data_developed <- life_expectancy_data %>% 
  filter(status == "Developed") %>% 
  mutate(
    country = as.factor(country),
    year = as.factor(year),
    status = as.factor(status),
    life_expectancy = standardize(life_expectancy),
    log_adult_mortality = standardize(log(adult_mortality)),
    infant_deaths = standardize(infant_deaths),
    alcohol = standardize(alcohol),
    percentage_expenditure = standardize(percentage_expenditure),
    measles = standardize(measles),
    under_five_deaths = standardize(under_five_deaths),
    polio = standardize(polio),
    diphtheria = standardize(diphtheria),
    log_gdp = standardize(log(gdp)),
    thinness_1_19_years = standardize(thinness_1_19_years),
    thinness_5_9_years = standardize(thinness_5_9_years))

life_expectancy_data_developed <- life_expectancy_data_developed %>% 
  select(-gdp, -gdp_2) %>% 
  filter(complete.cases(life_expectancy_data_developed))


# developing
life_expectancy_data_developing <- life_expectancy_data %>% 
  filter(status == "Developing") %>% 
  mutate(
    country = as.factor(country),
    year = as.factor(year),
    status = as.factor(status),
    life_expectancy = standardize(life_expectancy),
    log_adult_mortality = standardize(log(adult_mortality)),
    infant_deaths = standardize(infant_deaths),
    alcohol = standardize(alcohol),
    percentage_expenditure = standardize(percentage_expenditure),
    measles = standardize(measles),
    under_five_deaths = standardize(under_five_deaths),
    polio = standardize(polio),
    diphtheria = standardize(diphtheria),
    log_gdp = standardize(log(gdp)),
    thinness_1_19_years = standardize(thinness_1_19_years),
    thinness_5_9_years = standardize(thinness_5_9_years))

life_expectancy_data_developing <- life_expectancy_data_developing %>% 
  select(-gdp, -gdp_2) %>% 
  filter(complete.cases(life_expectancy_data_developing))

#------------------------------------------------------------------------------#


library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(broom.mixed)
library(tidybayes)
library(tidyverse)
library(gridExtra)

# fitting the Bayesian models

# for models with predictors
model_developed_1 <- stan_glmer(
  life_expectancy ~ log_gdp + log_adult_mortality + thinness_1_19_years +
    thinness_5_9_years + diphtheria + polio + measles + percentage_expenditure + alcohol +
    infant_deaths + (1|country),
  data = life_expectancy_data_developed,
  family = gaussian,
  prior_intercept = normal(72, 8),
  prior = normal(0, 1),
  chains = 4, iter = 5000*2, seed = 101
)

model_developing_1 <- stan_glmer(
  life_expectancy ~ log_gdp + log_adult_mortality + thinness_1_19_years +
    thinness_5_9_years + diphtheria + polio + measles + percentage_expenditure + alcohol +
    infant_deaths + (1|country),
  data = life_expectancy_data_developing,
  family = gaussian,
  prior_intercept = normal(72, 8),
  prior = normal(0, 1),
  chains = 4, iter = 5000*2, seed = 101
)

# for models without predictors
model_developed_2 <- stan_glmer(
  life_expectancy ~ (1|country),
  data = life_expectancy_data_developed,
  family = gaussian,
  prior_intercept = normal(72, 8),
  prior = normal(0, 1),
  chains = 4, iter = 5000*2, seed = 101
)

model_developing_2 <- stan_glmer(
  life_expectancy ~ (1|country),
  data = life_expectancy_data_developing,
  family = gaussian,
  prior_intercept = normal(72, 8),
  prior = normal(0, 1),
  chains = 4, iter = 5000*2, seed = 101
)

#################### MCMC DIAGNOSTICS ############################

## model for developed country ##

mcmc_trace(model_developed_1) +
  ggtitle('mcmc trace plot for developed country')
mcmc_dens_overlay(model_developed_1) +
  ggtitle('mcmc density plot for developed country')
pp_check(model_developed_1)

tidy(model_developed_1, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.95)
rhat(model_developed_1)

####################

mcmc_trace(model_developed_2) +
  ggtitle('mcmc trace plot for developed country')
mcmc_dens_overlay(model_developed_2) +
  ggtitle('mcmc density plot for developed country')
pp_check(model_developed_2)

tidy(model_developed_2, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.95)
rhat(model_developed_2)

## model for developing country ##

mcmc_trace(model_developing_1, size = 0.1) +
  ggtitle('mcmc trace plot for developing country')
mcmc_dens_overlay(model_developing_1) +
  ggtitle('mcmc density plot for developing country')
pp_check(model_developing_1)

tidy(model_developing_1, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.95)
rhat(model_developing_1)

####################

mcmc_trace(model_developing_2, size = 0.1) +
  ggtitle('mcmc trace plot for developing country')
mcmc_dens_overlay(model_developing_2) +
  ggtitle('mcmc density plot for developing country')
pp_check(model_developing_2)

tidy(model_developing_2, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.95)
rhat(model_developing_2)

######################## POSTERIOR PREDICTIVE ##########################

# use posterior_predict and ppc_intervals to determine how accurate our 
# Bayesian models are

pred_1 <- posterior_predict(model_developed_1)
pred_2 <- posterior_predict(model_developing_1)
pred_3 <- posterior_predict(model_developed_2)
pred_4 <- posterior_predict(model_developing_2)

# developed, with predictors
p1 <- ppc_ribbon(life_expectancy_data_developed_1$life_expectancy, pred_1)
p2 <- ppc_intervals(life_expectancy_data_developed_1$life_expectancy, pred_1)
grid.arrange(p1, p2, nrow = 2)

# developing, with predictors
p3 <- ppc_ribbon(life_expectancy_data_developing_1$life_expectancy, pred_2)
p4 <- ppc_intervals(life_expectancy_data_developing_1$life_expectancy, pred_2)
grid.arrange(p3, p4, nrow = 2)

# developed, without predictors
p5 <- ppc_ribbon(life_expectancy_data_developed_2$life_expectancy, pred_3)
p6 <- ppc_intervals(life_expectancy_data_developed_2$life_expectancy, pred_3)
grid.arrange(p5, p6, nrow = 2)

# developing, without predictors
p7 <- ppc_ribbon(life_expectancy_data_developing_2$life_expectancy, pred_4)
p8 <- ppc_intervals(life_expectancy_data_developing_2$life_expectancy, pred_4)
grid.arrange(p7, p8, nrow = 2)


############################ MODEL SELECTION #########################

# use loo and loo_compare
# developed 
developed_1_loo <- loo(model_developed_1)
developed_2_loo <- loo(model_developed_2)
loo_compare(developed_1_loo, developed_2_loo)

# developing 
developing_1_loo <- loo(model_developing_1)
developing_2_loo <- loo(model_developing_2)
loo_compare(developing_1_loo, developing_2_loo)

