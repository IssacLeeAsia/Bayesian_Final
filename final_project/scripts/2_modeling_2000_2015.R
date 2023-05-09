# Bayesian Final Project
source("scripts/1_data_cleaning.R")


# SET UP
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(broom.mixed)
library(tidybayes)
library(tidyverse)
library(gridExtra)


########## USING FOR LOOP TO FIT BAYESIAN MODELS ###########
# create a dataset list to store the dataset 
data_list <- list()
data_list[[1]] <- life_exp_2000_developed
data_list[[2]] <- life_exp_2000_developing 
data_list[[3]] <- life_exp_2001_developed
data_list[[4]] <- life_exp_2001_developing 
data_list[[5]] <- life_exp_2002_developed
data_list[[6]] <- life_exp_2002_developing 
data_list[[7]] <- life_exp_2003_developed
data_list[[8]] <- life_exp_2003_developing 
data_list[[9]] <- life_exp_2004_developed
data_list[[10]] <- life_exp_2004_developing 
data_list[[11]] <- life_exp_2005_developed
data_list[[12]] <- life_exp_2005_developing 
data_list[[13]] <- life_exp_2006_developed
data_list[[14]] <- life_exp_2006_developing 
data_list[[15]] <- life_exp_2007_developed
data_list[[16]] <- life_exp_2007_developing 

data_list[[17]] <- life_exp_2008_developed
data_list[[18]] <- life_exp_2008_developing 
data_list[[19]] <- life_exp_2009_developed
data_list[[20]] <- life_exp_2009_developing 
data_list[[21]] <- life_exp_2010_developed
data_list[[22]] <- life_exp_2010_developing 
data_list[[23]] <- life_exp_2011_developed
data_list[[24]] <- life_exp_2011_developing 
data_list[[25]] <- life_exp_2012_developed
data_list[[26]] <- life_exp_2012_developing 
data_list[[27]] <- life_exp_2013_developed
data_list[[28]] <- life_exp_2013_developing 
data_list[[13]] <- life_exp_2014_developed
data_list[[14]] <- life_exp_2014_developing 
data_list[[15]] <- life_exp_2015_developed
data_list[[16]] <- life_exp_2015_developing 

# define the model formula
model_formula <- life_expectancy ~ log_gdp + log_adult_mortality + thinness_1_19_years +
  thinness_5_9_years + diphtheria + polio + measles + percentage_expenditure + alcohol +
  infant_deaths

# select the variables we need for modeling
to_select <- c("life_expectancy", "log_gdp")

# drop unnecessary columns
to_drop <- c("hepatitis_b", "total_expenditure", "population", 
             "income_composition_of_resources", "schooling", "bmi", "hiv_aids")

# set priors
priors <- normal(location = rep(0,10),
                 scale = rep(1,10))

# for loop
for (i in 1:length(data_list)) {
  
  # get the dataset
  dataset <- data_list[[i]]
  
  # extract year and status values from the dataset 
  year <- unique(dataset$year)
  status <- unique(dataset$status)
  
  # select the subset of columns
  dataset_mod <- dataset %>% select(-all_of(to_drop))
  
  # fit the Bayesian model
  model_fit <- stan_glm(
    model_formula,
    data = dataset_mod, family = gaussian,
    prior_intercept = normal(0,1),
    prior = priors,
    chains = 4, iter = 5000*2, seed = 84735)
  
  # construct the model name
  model_name <- paste0("model_", year, "_", status)
  
  # assign the model to a name
  assign(model_name, model_fit)
}

#################### MCMC DIAGNOSTICS ############################

## model_2000 ##
# MCMC density plots
mcmc_dens_overlay(model_2000_Developed) +
  ggtitle("mcmc density plot of model_2000_Developed")
mcmc_dens_overlay(model_2000_Developing) +
  ggtitle("mcmc density plot of model_2000_Developing")

# R hat
rhat(model_2000_Developed)
rhat(model_2000_Developing)

#------------------------------------------------------

## model_2001 ##
# MCMC density plots
mcmc_dens_overlay(model_2001_Developed) +
  ggtitle("mcmc density plot of model_2001_Developed")
mcmc_dens_overlay(model_2001_Developing) +
  ggtitle("mcmc density plot of model_2001_Developing")

# R hat
rhat(model_2001_Developed)
rhat(model_2001_Developing)

#------------------------------------------------------

## model_2002 ##
# MCMC density plots
mcmc_dens_overlay(model_2002_Developed) +
  ggtitle("mcmc density plot of model_2002_Developed")
mcmc_dens_overlay(model_2002_Developing) +
  ggtitle("mcmc density plot of model_2002_Developing")

# R hat
rhat(model_2002_Developed)
rhat(model_2002_Developing)

#------------------------------------------------------

## model_2003 ##
# MCMC density plots
mcmc_dens_overlay(model_2003_Developed) +
  ggtitle("mcmc density plot of model_2003_Developed")
mcmc_dens_overlay(model_2003_Developing) +
  ggtitle("mcmc density plot of model_2003_Developing")

# R hat
rhat(model_2003_Developed)
rhat(model_2003_Developing)

#------------------------------------------------------

## model_2004 ##
# MCMC density plots
mcmc_dens_overlay(model_2004_Developed) +
  ggtitle("mcmc density plot of model_2004_Developed")
mcmc_dens_overlay(model_2004_Developing) +
  ggtitle("mcmc density plot of model_2004_Developing")

# R hat
rhat(model_2004_Developed)
rhat(model_2004_Developing)

#------------------------------------------------------

## model_2005 ##
# MCMC density plots
mcmc_dens_overlay(model_2005_Developed) +
  ggtitle("mcmc density plot of model_2005_Developed")
mcmc_dens_overlay(model_2005_Developing) +
  ggtitle("mcmc density plot of model_2005_Developing")

# R hat
rhat(model_2005_Developed)
rhat(model_2005_Developing)

#------------------------------------------------------

## model_2006 ##
# MCMC density plots
mcmc_dens_overlay(model_2006_Developed) +
  ggtitle("mcmc density plot of model_2006_Developed")
mcmc_dens_overlay(model_2006_Developing) +
  ggtitle("mcmc density plot of model_2006_Developing")

# R hat
rhat(model_2006_Developed)
rhat(model_2006_Developing)

#------------------------------------------------------

## model_2007 ##
# MCMC density plots
mcmc_dens_overlay(model_2007_Developed) +
  ggtitle("mcmc density plot of model_2007_Developed")
mcmc_dens_overlay(model_2007_Developing) +
  ggtitle("mcmc density plot of model_2007_Developing")

# R hat
rhat(model_2007_Developed)
rhat(model_2007_Developing)

#------------------------------------------------------

## model_2008 ##
# MCMC density plots
mcmc_dens_overlay(model_2008_Developed) +
  ggtitle("mcmc density plot of model_2008_Developed")
mcmc_dens_overlay(model_2008_Developing) +
  ggtitle("mcmc density plot of model_2008_Developing")

# R hat
rhat(model_2008_Developed)
rhat(model_2008_Developing)

#------------------------------------------------------

## model_2009 ##
# MCMC density plots
mcmc_dens_overlay(model_2009_Developed) +
  ggtitle("mcmc density plot of model_2009_Developed")
mcmc_dens_overlay(model_2009_Developing) +
  ggtitle("mcmc density plot of model_2009_Developing")

# R hat
rhat(model_2009_Developed)
rhat(model_2009_Developing)

#------------------------------------------------------

## model_2010 ##
# MCMC density plots
mcmc_dens_overlay(model_2010_Developed) +
  ggtitle("mcmc density plot of model_2010_Developed")
mcmc_dens_overlay(model_2010_Developing) +
  ggtitle("mcmc density plot of model_2010_Developing")

# R hat
rhat(model_2010_Developed)
rhat(model_2010_Developing)

#------------------------------------------------------

## model_2011 ##
# MCMC density plots
mcmc_dens_overlay(model_2011_Developed) +
  ggtitle("mcmc density plot of model_2011_Developed")
mcmc_dens_overlay(model_2011_Developing) +
  ggtitle("mcmc density plot of model_2011_Developing")

# R hat
rhat(model_2011_Developed)
rhat(model_2011_Developing)

#------------------------------------------------------

## model_2012 ##
# MCMC density plots
mcmc_dens_overlay(model_2012_Developed) +
  ggtitle("mcmc density plot of model_2012_Developed")
mcmc_dens_overlay(model_2012_Developing) +
  ggtitle("mcmc density plot of model_2012_Developing")

# R hat
rhat(model_2012_Developed)
rhat(model_2012_Developing)

#------------------------------------------------------

## model_2013 ##
# MCMC density plots
mcmc_dens_overlay(model_2013_Developed) +
  ggtitle("mcmc density plot of model_2013_Developed")
mcmc_dens_overlay(model_2013_Developing) +
  ggtitle("mcmc density plot of model_2013_Developing")

# R hat
rhat(model_2013_Developed)
rhat(model_2013_Developing)

#------------------------------------------------------

## model_2014 ##
# MCMC density plots
mcmc_dens_overlay(model_2014_Developed) +
  ggtitle("mcmc density plot of model_2014_Developed")
mcmc_dens_overlay(model_2014_Developing) +
  ggtitle("mcmc density plot of model_2014_Developing")

# R hat
rhat(model_2014_Developed)
rhat(model_2014_Developing)

#------------------------------------------------------

## model_2015 ##
# MCMC density plots
mcmc_dens_overlay(model_2015_Developed) +
  ggtitle("mcmc density plot of model_2015_Developed")
mcmc_dens_overlay(model_2015_Developing) +
  ggtitle("mcmc density plot of model_2015_Developing")

# R hat
rhat(model_2015_Developed)
rhat(model_2015_Developing)




######################## POSTERIOR PREDICTIVE ##########################

# use posterior_predict and ppc_intervals to determine how accurate our 
# Bayesian models are

# create a model list
mod_list <- list()
mod_list[[1]] <- model_2000_Developed
mod_list[[2]] <- model_2000_Developing
mod_list[[3]] <- model_2001_Developed
mod_list[[4]] <- model_2001_Developing
mod_list[[5]] <- model_2002_Developed
mod_list[[6]] <- model_2002_Developing
mod_list[[7]] <- model_2003_Developed
mod_list[[8]] <- model_2003_Developing
mod_list[[9]] <- model_2004_Developed
mod_list[[10]] <- model_2004_Developing
mod_list[[11]] <- model_2005_Developed
mod_list[[12]] <- model_2005_Developing
mod_list[[13]] <- model_2006_Developed
mod_list[[14]] <- model_2006_Developing
mod_list[[15]] <- model_2007_Developed
mod_list[[16]] <- model_2007_Developing
mod_list[[17]] <- model_2008_Developed
mod_list[[18]] <- model_2008_Developing
mod_list[[19]] <- model_2009_Developed
mod_list[[20]] <- model_2009_Developing
mod_list[[21]] <- model_2010_Developed
mod_list[[22]] <- model_2010_Developing
mod_list[[23]] <- model_2011_Developed
mod_list[[24]] <- model_2011_Developing
mod_list[[25]] <- model_2012_Developed
mod_list[[26]] <- model_2012_Developing
mod_list[[27]] <- model_2013_Developed
mod_list[[28]] <- model_2013_Developing
mod_list[[29]] <- model_2014_Developed
mod_list[[30]] <- model_2014_Developing
mod_list[[31]] <- model_2015_Developed
mod_list[[32]] <- model_2015_Developing

# for loop
for (i in 1:length(mod_list)) {
  
  # get the model
  model <- mod_list[[i]]
  
  # get the dataset
  data <- data_list[[i]]
  
  # posterior predict
  pred <- posterior_predict(model, data)
  
  # plots
  p1 <- ppc_ribbon(data$life_expectancy, pred)
  p2 <- ppc_intervals(data$life_expectancy, pred)
  grid.arrange(p1, p2, nrow = 2)
}


#################### COEFFICIENTS FOR MODLES ########################

# build for loop to extract coefficients from different models 
for (i in 1:length(mod_list)) {
  
  # get the model 
  model <- mod_list[[i]]
  
  # Intercepts
  coef_intercept[i] <- model$coefficients[1]
  
  # log_gdp
  coef_log_gdp[i] <- model$coefficients[2]
  
  # log_adult_mortality 
  coef_log_adult_mortality[i] <- model$coefficients[3]
  
  # thinness_1_19_years
  coef_thinness_1_19_years[i] <- model$coefficients[4]
  
  # thinness_5_9_years
  coef_thinness_5_9_year[i] <- model$coefficients[5]
  
  # diphtheria
  coef_diphtheria[i] <- model$coefficients[6]
  
  # polio
  coef_polio[i] <- model$coefficients[7]
  
  # measles
  coef_measles[i] <- model$coefficients[8]
  
  # percentage_expenditure
  coef_percentage_expenditure[i] <- model$coefficients[9]
  
  # alcohol
  coef_alcohol[i] <- model$coefficients[10]
  
  # infant_deaths
  coef_infant_deaths[i] <- model$coefficients[11]
  
  # store them in a dataframe
  coef_dat <- data.frame(coef_intercept, coef_log_gdp, coef_log_adult_mortality,
                         coef_thinness_1_19_years, coef_thinness_5_9_year,
                         coef_diphtheria, coef_polio, coef_measles, 
                         coef_percentage_expenditure, coef_alcohol, coef_infant_deaths)
}

# add year and status to the coef_dat for plots
coef_dat <- coef_dat %>% 
  mutate(year = rep(2000:2015, each = 2),
         status = ifelse(row_number() %% 2 == 0, "Developing", "Developed"))

# plot those coefficients and see how they change across the datasets
p1 <- ggplot(data = coef_dat, aes(x = year, y = coef_intercept, color = status)) +
  geom_line()

p2 <- ggplot(data = coef_dat, aes(x = year, y = coef_log_gdp, color = status)) +
  geom_line()

p3 <- ggplot(data = coef_dat, aes(x = year, y = coef_log_adult_mortality, color = status)) +
  geom_line()

p4 <- ggplot(data = coef_dat, aes(x = year, y = coef_thinness_1_19_years, color = status)) +
  geom_line()

p5 <- ggplot(data = coef_dat, aes(x = year, y = coef_thinness_5_9_year, color = status)) +
  geom_line()

p6 <- ggplot(data = coef_dat, aes(x = year, y = coef_diphtheria, color = status)) +
  geom_line()

p7 <- ggplot(data = coef_dat, aes(x = year, y = coef_polio, color = status)) +
  geom_line()

p8 <- ggplot(data = coef_dat, aes(x = year, y = coef_measles, color = status)) +
  geom_line()

p9 <- ggplot(data = coef_dat, aes(x = year, y = coef_percentage_expenditure, color = status)) +
  geom_line()

p10 <- ggplot(data = coef_dat, aes(x = year, y = coef_alcohol, color = status)) +
  geom_line()

p11 <- ggplot(data = coef_dat, aes(x = year, y = coef_infant_deaths, color = status)) +
  geom_line()

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,
             ncol = 4, nrow = 3)