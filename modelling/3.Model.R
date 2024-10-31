# Load necessary libraries
library(stats)
library(dplyr)
library(glmnet)
library(dplyr)
library(gamlr)
library(tweedie)
library(mgcv)
library(statmod)
library(tweedie)
library(forcats)
library(tidyr)

seed(123)
# Fit a GLM model on your data
combined_data$severity%>% summary

severity_data <- combined_data %>% filter(severity > 0)


# Calculate the mean, ignoring NA values
mean_value <- mean(severity_data$owner_age_years, na.rm = TRUE)
severity_data$owner_age_years[is.na(severity_data$owner_age_years)] <- mean_value

mean_value <- mean(severity_data$density, na.rm = TRUE)
severity_data$density[is.na(severity_data$density)] <- mean_value

# features <- c(
#   'nb_state',
#   'nb_breed_trait_num_encoded',
#   'pet_age_months',
#   'nb_excess',
#   'age_breed_interaction',
#   'nb_contribution',
#   'owner_age_years',
#   'pet_gender',
#   'pet_de_sexed',
#   'pet_age_months',
#   'nb_address_type_adj',
#   'nb_breed_type',
#   'is_multi_pet_plan',
#   'qi'
# )

features <- c(
  'nb_breed_trait_num_encoded',
  'pet_age_months',
  'nb_excess',
  'owner_age_years',
  'density',
  'age_breed_interaction'
)


# hist(combined_data$severity, main = "Histogram of Severity", xlab = "Severity", breaks = 30)

# Join the features to form a formula with underscores between feature names
formula <- as.formula(paste("severity", "~", paste(features, collapse = " + "), sep = ""))

# Fit the GLM model
glm_model <- glm(formula, family = Gamma(link = "log"), data = severity_data)

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
BIC(glm_model)

# Extract the actual severity values
actual_severity <- severity_data$severity

# Calculate residuals as the difference between actual and predicted values
actual_residuals <- actual_severity - predict(glm_model, type = "response")

sqrt(mean((severity_data$severity - predict(glm_model, type = "response"))^2))  # RMSE calculation


# Create a data frame with actual severity and residuals for easy plotting
residual_data <- data.frame(
  Actual_Severity = actual_severity,
  Residuals = actual_residuals
)

# Plot using ggplot2
ggplot(residual_data, aes(x = Actual_Severity, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +  # Blue points with transparency and size
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Horizontal line at y=0
  labs(
    title = "Residuals vs. Actual Severity",
    x = "Actual Severity",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 15) +  # Clean minimal theme with larger base text
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold title
    axis.title.x = element_text(margin = margin(t = 10)),   # Adjust x-axis label margin
    axis.title.y = element_text(margin = margin(r = 10))    # Adjust y-axis label margin
  )



library(gbm)
severity_data$severity_difference <- severity_data$severity - predict(glm_model, type = "response")

formula <- as.formula(paste("severity_difference", "~", paste(features, collapse = " + "), sep = ""))

# Load necessary libraries
library(caret)
library(gbm)

# Define the hyperparameter grid
hyper_grid <- expand.grid(
  n.trees = c(500, 1000, 1500),       # Number of trees
  interaction.depth = c(2, 3, 4),     # Depth of each tree
  shrinkage = c(0.01, 0.05, 0.1),     # Learning rate
  n.minobsinnode = c(5, 10)           # Minimum number of observations in terminal nodes
)

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

# Train the GBM model using caret with cross-validation
gbm_tuned <- train(
  formula,
  data = severity_data,
  method = "gbm",
  distribution = "gaussian",
  trControl = train_control,
  tuneGrid = hyper_grid,
  verbose = FALSE
)



# Step 3: Make predictions with the tuned GBM model
severity_data$pred_gbm_diff <- predict(gbm_tuned, newdata = severity_data)

# Adjust GLM predictions with GBM residual predictions
severity_data$final_pred_severity <- severity_data$pred_glm + severity_data$pred_gbm_diff

# Step 4: Calculate new residuals
severity_data$new_residuals <- severity_data$severity - severity_data$final_pred_severity

# Plot the new residuals
ggplot(severity_data, aes(x = severity, y = new_residuals)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "New Residuals vs. Actual Severity",
    x = "Actual Severity",
    y = "New Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )















# Print the best tuning parameters
print("Best hyperparameters:")
print(gbm_tuned$bestTune)

# Use the best model to make predictions on the severity difference
severity_data$pred_gbm_diff <- predict(gbm_tuned, newdata = severity_data)
severity_data$final_pred_severity <- severity_data$pred_glm + severity_data$pred_gbm_diff

# Calculate residuals
severity_data$new_residuals <- severity_data$severity - severity_data$final_pred_severity

# Calculate metrics for the final model

# RMSE
rmse <- sqrt(mean(severity_data$new_residuals^2))
print(paste("RMSE:", rmse))

# MAE
mae <- mean(abs(severity_data$new_residuals))
print(paste("MAE:", mae))

# MAPE
mape <- mean(abs(severity_data$new_residuals / severity_data$severity)) * 100
print(paste("MAPE:", mape))

# R-squared
ss_total <- sum((severity_data$severity - mean(severity_data$severity))^2)
ss_residual <- sum(severity_data$new_residuals^2)
r_squared <- 1 - (ss_residual / ss_total)
print(paste("R-squared:", r_squared))
deviance_reduction <- 1 - (gbm_model$train.error[gbm_model$n.trees] / gbm_model$train.error[1])
print(paste("Explained Deviance:", deviance_reduction))

# Plot the residuals for visual assessment
ggplot(severity_data, aes(x = severity, y = new_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "New Residuals vs. Actual Severity",
       x = "Actual Severity",
       y = "New Residuals") +
  theme_minimal()

