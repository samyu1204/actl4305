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





#============================Frequency GLM======================================###

###Assessing Polynomial GLM####




#>>>>Insert graphs of numeric predictors against claims frequency for polynomial relationships


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
       col = c("blue", "red"), lwd = 2) ###Heavy Skew, indicates use of gamma distribution for the GLM


#Training and Validation Set
set.seed(123)

cleaned_combined_data <- na.omit(combined_data)

cleaned_combined_data$pet_de_sexed <- factor(cleaned_combined_data$pet_de_sexed)
cleaned_combined_data$pet_de_sexed <- as.logical(cleaned_combined_data$pet_de_sexed == "true")


training_val_frequency_id <- sample(1:nrow(cleaned_combined_data), 0.7*nrow(cleaned_combined_data)) #Rows of combined data to be used for the training set

cols.to.remove <- which(colnames(cleaned_combined_data) %in% c("severity", "pet_de_sexed_age","nb_suburb", "nb_postcode", "pet_age_years", "nb_breed_name_unique", "nb_breed_name_unique_concat", "exposure_id_1", "exposure_id", "pet_is_switcher"))

training_val_frequency <- cleaned_combined_data[training_val_frequency_id, -cols.to.remove] #training and validation set


for (i in 1:nrow(training_val_frequency)) {
  if(training_val_frequency$frequency[i] == 0) {
    training_val_frequency$frequency[i] <- 0.001
  }
}


str(training_val_frequency)

#Test Frequency
test_frequency <- as.vector(cleaned_combined_data[-training_val_frequency_id, "frequency"])
test.set <- cleaned_combined_data[-training_val_frequency_id, -c(cols.to.remove, which(colnames(cleaned_combined_data) == "frequency"))]

#Frequency GLM Gamma
glm_gamma_frequency <- glm(frequency ~., data = training_val_frequency, family = Gamma(link = "log"))

#Frequency GLM inverse gaussian
glm_ig_frequency <- glm(frequency ~., data = training_val_frequency, family = inverse.gaussian(link = "log"))

###============Gamma Model Fit===================###

#Training Fit
(gamma_frequency_aic <- glm_gamma_frequency$aic)
(gamma_frequency_mean_SSE <- mean(glm_gamma_frequency$residuals^2))
(gamma_frequency_deviance <- glm_gamma_frequency$deviance)
glm_gamma_frequency$coefficients

#Formatting test set

test_frequency <- unlist(test_frequency)
test_frequency_numeric <- as.numeric(test_frequency)

#Test error

frequency_glm_prediction_gamma <- predict(glm_gamma_frequency, newdata = test.set, type = "response")


(test.mse.frequency_gamma <- mean((frequency_glm_prediction_gamma-test_frequency_numeric)^2, na.rm = TRUE))

plot(frequency_glm_prediction_gamma)


###=============Inverse Gaussian Model Fit===============###: Gamma is preferable over IG GLM

##Training Fit
(glm_ig_frequency_aic <- glm_ig_frequency$aic)
(glm_gamma_frequency_mean_SSE <- mean(glm_ig_frequency$residuals^2))
(glm_ig_frequency_deviance <-glm_ig_frequency$deviance)
glm_ig_frequency$coefficients

#Test Error
glm_ig_frequency_prediction <- predict(glm_ig_frequency, newdata = test.set, type = "response")

(test.mse.frequency_ig <- mean((glm_ig_frequency_prediction-test_frequency_numeric)^2, na.rm = TRUE))













#Creating Model Data Set




