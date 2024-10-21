# Load necessary libraries
library(stats)
library(dplyr)
library(MASS)
library(tidyverse)

# Fit a GLM model on your data
combined_data$claim_nb %>% summary

# Create predicting columns
combined_data$severity <- combined_data$Total_claim_amount / combined_data$claim_nb
combined_data$frequency <- combined_data$claim_freq

# Features
features <- c(
  'pet_age_months',
  'nb_contribution',
  'nb_excess',
  'nb_address_type_adj_numerical',
  'nb_state_num',
  'nb_contribution_excess',
  'owner_age_years',
  'nb_number_of_breeds',
  'nb_average_breed_size',
  'nb_breed_type_num',
  'nb_breed_trait_num',
  'is_multi_pet_plan_num',
  'quote_time_group_num',
  'claim_nb',
  'earned_units',
  'owner_pet_age_interaction',
  'contribution_excess_interaction',
  'age_breed_bin'
)

features <- c(
  'nb_state_num',
  'nb_contribution_excess',
  'claim_nb',
  'age_breed_bin',
  'owners_age',
  'age_bucket'
)

# Join the features to form a formula with underscores between feature names
formula <- as.formula(paste("severity", "~", paste(features, collapse = " + "), sep = ""))

# Fit the GLM model
glm_model <- glm(formula, data = combined_data, family = gaussian())

# Summary of the GLM model to see the coefficients and model fit
summary(glm_model)

# Deviance and Null Deviance
deviance(glm_model)
glm_model$null.deviance
pseudo_r_squared <- 1 - (glm_model$deviance / glm_model$null.deviance)
pseudo_r_squared
AIC(glm_model)





#============================Frequency GLM======================================
###Frequency Distribution

plot(density(combined_data$frequency), 
     main = "Density Plot with Normal Distribution", 
     xlab = "Frequency", 
     ylab = "Density", 
     col = "blue", 
     xlim = c(0,1))


mean_val <- mean(combined_data$frequency)
sd_val <- sd(combined_data$frequency)

curve(dnorm(x, mean = mean_val, sd = sd_val), 
      col = "red", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data Density", "Normal Distribution"), 
       col = c("blue", "red"), lwd = 2) ###Heavy Skew, implicates use of gamma distribution for the GLM


#Training and Validation Set
set.seed(123)

training_val_frequency_id <- sample(1:nrow(combined_data), 0.7*nrow(combined_data)) #Rows of combined data to be used for the training set

cols.to.remove <- which(colnames(combined_data) %in% c("severity", "pet_de_sexed_age","nb_suburb", "nb_postcode", "pet_age_years", "nb_breed_name_unique", "nb_breed_name_unique_concat", "exposure_id_1", "exposure_id"))

training_val_frequency <- combined_data[training_val_frequency_id, -cols.to.remove] #training and validation set

str(training_val_frequency)


#Test Frequency
test_frequency <- combined_data[-training_val_id, "frequency"]

#Frequency GLM 
frequency.glm <- glm(frequency ~., data = training_val_frequency, family = poisson(link = "log"))


view(combined_data)
str(combined_data)
sd(combined_data$frequency)^2
mean(combined_data$frequency)
min(combined_data$frequency)
frequency.glm$aic
