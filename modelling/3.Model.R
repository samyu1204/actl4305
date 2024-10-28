# Load necessary libraries
library(stats)
library(dplyr)
library(glmnet)
library(dplyr)
library(gamlr)

# Fit a GLM model on your data
combined_data$claim_nb %>% summary

severity_data <- combined_data %>% filter(severity > 0)


features <- c(
  'nb_state',
  'nb_breed_trait_num_encoded',
  'pet_age_months',
  'nb_excess',
  'age_breed_interaction',
  'nb_contribution',
  'owner_age_years',
  'pet_gender',
  'pet_de_sexed',
  'pet_age_months',
  'nb_address_type_adj',
  'nb_breed_type',
  'is_multi_pet_plan',
  'qi'
)

features <- c(
  'nb_breed_trait_num_encoded',
  'pet_age_months',
  'nb_excess',
  'owner_age_years',
  'nb_address_type_adj',
  'density'
)
# hist(combined_data$severity, main = "Histogram of Severity", xlab = "Severity", breaks = 30)

# Join the features to form a formula with underscores between feature names
formula <- as.formula(paste("severity", "~", paste(features, collapse = " + "), sep = ""))

# Fit the GLM model
glm_model <- glm(formula, data = severity_data, family = Gamma(link = "log"))

# Summary of the GLM model to see the coefficients and model fit
summary(glm_model)

stepwise_model <- step(glm_model, direction = "both")
final_formula <- formula(stepwise_model)
print(final_formula)

# Deviance and Null Deviance
deviance(glm_model)
glm_model$null.deviance
pseudo_r_squared <- 1 - (glm_model$deviance / glm_model$null.deviance)
pseudo_r_squared
AIC(glm_model)

# Extract the actual severity values
actual_severity <- severity_data$severity

# Extract predicted values from the model
predicted_severity <- predict(glm_model, type = "response")

# Calculate residuals as the difference between actual and predicted values
actual_residuals <- actual_severity - predicted_severity

# Plot the residuals against the actual severity values
plot(actual_severity, actual_residuals, 
     xlab = "Actual Severity", 
     ylab = "Residuals",
     main = "Residuals vs. Actual Severity",
     pch = 16,         # Change the point type
     cex = 1.5,        # Increase point size
     col = "blue")     # Color of points

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

# Optionally adjust plot margins if needed
par(mar = c(5, 5, 4, 2) + 0.1)



library(gbm)
formula <- as.formula(paste("residuals_glm", "~", paste(features, collapse = " + "), sep = ""))

combined_data_residual <- combined_data %>% filter(severity > 0)

combined_data_residual$residuals_glm <- residuals(glm_model, type = "deviance")  # or "pearson" / "response" as needed


gbm_model <- gbm(formula,
                 data = combined_data_residual,
                 distribution = "gaussian",  # Gaussian is used because we're predicting residuals
                 n.trees = 1000,             # Number of boosting iterations (trees)
                 interaction.depth = 3,       # Depth of each tree
                 shrinkage = 0.01,            # Learning rate
                 cv.folds = 5,                # 5-fold cross-validation
                 verbose = FALSE)
glm_predictions <- predict(glm_model, type = "response")

gbm_predictions <- predict(gbm_model, n.trees = 1000, type = "response")  # Adjust `n.trees` if needed
combined_predictions <- glm_predictions + gbm_predictions

combined_data_residual$new_residuals <- combined_data_residual$severity - combined_predictions


plot(combined_predictions, combined_data_residual$new_residuals,
     xlab = "Combined Predictions",
     ylab = "New Residuals",
     main = "Residuals of Combined GLM + GBM Model",
     pch = 16, col = "blue")

abline(h = 0, col = "red", lty = 2)  # Add reference line at 0

