# Load necessary libraries
library(stats)
library(dplyr)

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












