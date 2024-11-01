# Load necessary libraries
library(stats)
library(dplyr)
library(gamlr)
library(tweedie)
library(mgcv)
library(statmod)
library(forcats)
library(tidyr)
library(randomForest)
library(glmnet)
library(caret)
library(gbm)
library(MASS)

# Set seed for the run
set.seed(123)

# Prepare severity data
severity_data <- combined_data %>% filter(severity > 0)

# Calculate the mean, ignoring NA values
mean_value <- mean(severity_data$owner_age_years, na.rm = TRUE)
severity_data$owner_age_years[is.na(severity_data$owner_age_years)] <- mean_value

mean_value <- mean(severity_data$density, na.rm = TRUE)
severity_data$density[is.na(severity_data$density)] <- mean_value

# Feature list
features <- c(
  'nb_breed_trait_num_encoded',
  'pet_age_months',
  'nb_excess',
  'owner_age_years',
  'density',
  'age_breed_interaction'
)


# Join the features to form a formula with underscores between feature names
formula <- as.formula(paste("severity", "~", paste(features, collapse = " + "), sep = ""))

# Fit the GLM model
glm_model_severity <- glm(formula, family = Gamma(link = "log"), data = severity_data)

# Gaussian GLM 
# glm_model <- glm(formula, family = gaussian, data = severity_data)

# Tweedie GLM 
# var_powers <- seq(1.1, 1.9, by = 0.1)
# 
# mse_results <- numeric(length(var_powers))
# 
# train_control <- trainControl(method = "cv", number = 5) 
# 
# for (i in seq_along(var_powers)) {
#   current_power <- var_powers[i]
#   
#   glm_model <- train(
#     formula, data = severity_data,  
#     method = "glm",
#     family = tweedie(var.power = current_power, link = "log"),
#     trControl = train_control,
#     metric = "RMSE"  
#   )
#   mse_results[i] <- mean(glm_model$resample$RMSE^2)
# }
# 
# optimal_var_power <- var_powers[which.min(mse_results)]
# optimal_var_power

# Summary
summary(glm_model_severity)

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
actual_residuals <- actual_severity - predict(glm_model_severity, type = "response")

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
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),   # Adjust x-axis label margin
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))    # Adjust y-axis label margin
  )

# MSE
mse_glm <- mean((actual_residuals)^2)

# ==================================================================================================
# GBM Training - Severity GBM
# ==================================================================================================

# Calculate residuals from the initial GLM model
severity_data$severity_difference <- severity_data$severity - predict(glm_model_severity, type = "response")

# Define the formula
formula <- as.formula(paste("severity_difference", "~", paste(features, collapse = " + "), sep = ""))

# Create a weight vector based on severity values (example: higher weights for top 25% of severity values)
severity_data$weights <- ifelse(severity_data$severity > quantile(severity_data$severity, 0.75), 1.5, 1)

# Define the hyperparameter grid
hyper_grid <- expand.grid(
  n.trees = c(500, 1000, 1500, 2000, 2500, 3000),
  interaction.depth = c(2),
  shrinkage = c(0.01),
  n.minobsinnode = c(10, 20, 30)
)

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

# Train the GBM model using caret with cross-validation and weights
gbm_tuned_severity <- train(
  formula,
  data = severity_data,
  method = "gbm",
  distribution = "gaussian",
  trControl = train_control,
  tuneGrid = hyper_grid,
  weights = severity_data$weights,
  verbose = FALSE
)

# Make prediction and compare on the GBM
# Make predictions using the GBM model for severity > 1000
severity_data$pred_gbm_diff <- ifelse(
  severity_data$severity > 1000,
  predict(gbm_tuned_severity, newdata = severity_data),
  0  # Set to 0 if severity is <= 1000
)

severity_data$pred_glm <- predict(glm_model_severity, type = "response")

# Add the GLM predictions to the GBM predictions
severity_data$final_pred_severity <- severity_data$pred_glm + severity_data$pred_gbm_diff

# Step 4: Calculate new residuals
severity_data$new_residuals <- severity_data$severity - severity_data$final_pred_severity

# Plot the new residuals
ggplot(severity_data, aes(x = severity, y = new_residuals)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "New Residuals vs. Actual Severity (Weighted for High Severity)",
    x = "Actual Severity",
    y = "New Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )


# Metrics =========================================================================
rmse <- sqrt(mean((severity_data$severity - severity_data$final_pred_severity)^2))
print(paste("RMSE:", rmse))

# R squared
ss_total <- sum((severity_data$severity - mean(severity_data$severity))^2)
ss_residual <- sum((severity_data$severity - severity_data$final_pred_severity)^2)
r_squared <- 1 - (ss_residual / ss_total)
print(paste("R-squared:", r_squared))

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

# R-squared
ss_total <- sum((severity_data$severity - mean(severity_data$severity))^2)
ss_residual <- sum(severity_data$new_residuals^2)
r_squared <- 1 - (ss_residual / ss_total)

# Plot the residuals for visual assessment
ggplot(severity_data, aes(x = severity, y = new_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "New Residuals vs. Actual Severity",
       x = "Actual Severity",
       y = "New Residuals") +
  theme_minimal()


# Final predictions after combining GLM and GBM
severity_data$final_pred_severity <- severity_data$pred_glm + severity_data$pred_gbm_diff


#################Claim Frequency Modelling#################

vars.to.remove <- c("exposure_id", "pet_gender", "pet_de_sexed_age", "pet_is_switcher", "nb_address_type_adj", "nb_suburb", "nb_state", "person_dob", "owner_age_years", "nb_breed_type",
                    "nb_breed_trait", "nb_breed_name_unique", "nb_breed_name_unique_concat", "exposure_id_1", "earned_units", "Total_Earned", "claim_nb", "Total_claim_amount", 
                    "Total_claim_paid", "severity", "frequency", "is_multi_plan", "quote_time_group", "sa2_code", "nb_postcode", "is_multi_pet_plan", "pet_age_year")

frequency.model.data <- combined_data[,-which(colnames(combined_data) %in% vars.to.remove)]



frequency.model.data$qi <- ifelse(frequency.model.data$qi == "Good", 1,
                                              ifelse(frequency.model.data$qi == "Acceptable", 2,
                                                     ifelse(frequency.model.data$qi == "Poor", 3, NA)))


frequency.model.data$qi <- as.numeric(frequency.model.data$qi)



frequency.model.data$pet_de_sexed <- ifelse(frequency.model.data$pet_de_sexed == "true", 2, ifelse(frequency.model.data$pet_de_sexed == "false", 1, NA))
frequency.model.data$pet_de_sexed <- as.numeric(frequency.model.data$pet_de_sexed)


frequency.model.data <- frequency.model.data %>%  #Convert all NA's to the mean of the column
  mutate_if(is.numeric, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

#Splitting into test and training

set.seed(2131)

freq_training_val_index <- sample(1:nrow(frequency.model.data), 0.7*nrow(frequency.model.data))

freq_training_val <- frequency.model.data[freq_training_val_index, ]

freq_test <- frequency.model.data[-freq_training_val_index, ]

#Assessing Distribution of claim_freq

mean_claim <- mean(frequency.model.data$claim_freq, na.rm = TRUE)
sd_claim <- sd(frequency.model.data$claim_freq, na.rm = TRUE)

ggplot(frequency.model.data, aes(x = claim_freq)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean_claim, sd = sd_claim), 
                color = "red", linetype = "dashed", size = 1) +
  labs(title = "Density Plot of Claim Frequency with Normal Curve",
       x = "Claim Frequency",
       y = "Density") +
  theme_minimal() +
  xlim(0, 1)


# ==================================================================================================
# GLM - Tweedie
# ==================================================================================================

var_powers <- seq(1, 2, by = 0.1)

mse_results <- numeric(length(var_powers))

train_control <- trainControl(method = "cv", number = 5) 

for (i in seq_along(var_powers)) {
  current_power <- var_powers[i]
  
  model <- train(
    claim_freq ~ ., data = freq_training_val,  
    method = "glm",
    family = tweedie(var.power = current_power, link = "log"),
    trControl = train_control,
    metric = "RMSE"  
  )
  mse_results[i] <- mean(model$resample$RMSE^2)
}

optimal_var_power <- var_powers[which.min(mse_results)]
optimal_var_power

tweedie_freq_model <- glm(claim_freq ~ ., data = freq_training_val, family = tweedie(var.power = optimal_var_power, link = "log"))

#Tweedie GLM Summary
summary(tweedie_freq_model)


#GLM Training Performance
tweedie_freq_model_training_predictions <- pmax(predict(tweedie_freq_model,  newdata = freq_training_val, type = "response"),0)
tweedie_freq_model_training_predictions <- as.vector(tweedie_freq_model_training_predictions)

training_MSE_tweedie <- mean((tweedie_freq_model_training_predictions-freq_training_val$claim_freq)^2)


#Performance on the test data 
tweedie_freq_model_prediction <- predict(tweedie_freq_model,  newdata = freq_test, type = "response")
tweedie_freq_model_prediction <- as.vector(tweedie_freq_model_prediction)

test_MSE_tweedie <- mean((tweedie_freq_model_prediction-freq_test$claim_freq)^2)

sdu <- summary(tweedie_freq_model)

tweedie_freq_model$deviance


#ADJ R^2
RSS <- sum((freq_training_val$claim_freq - tweedie_freq_model_training_predictions)^2)
SST <- sum(freq_training_val$claim_freq - mean(freq_training_val$claim_freq)^2)


R2 <- 1 - (RSS / SST)

n <- nrow(freq_training_val)
k <- length(coef(tweedie_freq_model)) - 1  

R2_adj <- 1 - ((1 - R2) * (n - 1) / (n - k - 1))


#GLM Test Performance

freq_glm_test_residuals <- freq_test$claim_freq - pmax(predict(tweedie_freq_model, newdata = freq_test, type = "response"),0)

glm_freq_test_results <- data.frame(test_residuals = freq_glm_test_residuals, Actual_Claim_Freq = freq_test$claim_freq)


ggplot(glm_freq_test_results, aes(x = Actual_Claim_Freq, y = test_residuals)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Tweedie GLM Residuals vs. Claim Frequency",
    x = "Actual Claim Frequency",
    y = "New Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )

mse_freq_glm <- mean((glm_freq_test_results$test_residuals)^2)


# Calculate residuals from the Tweedie GLM model
freq_training_val$residuals <- freq_training_val$claim_freq - predict(tweedie_freq_model, newdata = freq_training_val, type = "response")

# ==================================================================================================
# GBM Training - Frequency GBM
# ==================================================================================================


# Hyperparameter tuning
gbm_grid <- expand.grid(
  n.trees = c(500, 1000, 1500),
  interaction.depth = c(2, 3, 4),
  shrinkage = c(0.01, 0.05, 0.1),
  n.minobsinnode = c(5, 10)
)

# Train GBM model on residuals
gbm_residuals_model <- train(
  as.formula("residuals ~ ."),
  data = freq_training_val,
  method = "gbm",
  distribution = "gaussian",
  trControl = trainControl(method = "cv", number = 5, verboseIter = FALSE),
  tuneGrid = gbm_grid,
  verbose = FALSE
)



#Test Performance
predicted_test_freq_glm <- predict(tweedie_freq_model, newdata = freq_test, type = "response") #Predicting Claim_freq
freq_test_gbm_residuals <- predict(gbm_residuals_model, newdata = freq_test) #Predicting the residuals


final_test_freq_predictions_gbm <- predicted_test_freq_glm + freq_test_gbm_residuals #Final prediction = claim_freq+error
final_test_freq_predictions_gbm <- pmax(final_test_freq_predictions_gbm, 0) #claim_freq non negative

final_freq_test_residuals <- freq_test$claim_freq -  final_test_freq_predictions_gbm #Final Model with GBM residuals

test_gbm_freq_summary <- data.frame(test_residuals = final_freq_test_residuals, Actual_Claim_Freq = freq_test$claim_freq) #Data frame of actual claim_Freq and residuals


ggplot(test_gbm_freq_summary, aes(x = Actual_Claim_Freq, y = test_residuals)) +   ###Visualisation of glm with gbm performance on the test data
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "GLM with GBM Residuals vs. Claim Frequency",
    x = "Actual Claim Frequency",
    y = "New Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )


test_mse_final_gbm <- mean((final_freq_test_residuals)^2)

















# Predict residual for training set
freq_training_val$pred_gbm_residuals <- predict(gbm_residuals_model, newdata = freq_training_val)

# Final prediction adding on GBM predictions
freq_training_val$final_pred_claim_freq <- predict(tweedie_freq_model, newdata = freq_training_val, type = "response") + freq_training_val$pred_gbm_residuals

freq_training_val$final_pred_claim_freq <- pmax(freq_training_val$final_pred_claim_freq, 0)

# Calculate final residuals
freq_training_val$final_residuals <- freq_training_val$claim_freq - freq_training_val$final_pred_claim_freq

# Plot residual
ggplot(freq_training_val, aes(x = claim_freq, y = final_residuals)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Final Residuals vs. Actual Claim Frequency",
    x = "Actual Claim Frequency",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = unit(c(10, 0, 0, 0), "pt")),  # Top margin of 10 points
    axis.title.y = element_text(margin = unit(c(0, 10, 0, 0), "pt"))   # Right margin of 10 points
  )


######random forest model
freq_rf <- randomForest(claim_freq ~., data = freq_training_val, ntree = 100, importance = TRUE)


#Training fit

rf_training_pridictions <- predict(freq_rf, newdata = freq_training_val)
rf_training_pridictions <- as.vector(rf_training_pridictions)

training_mse_rf <- mean((rf_training_pridictions - freq_training_val$claim_freq)^2) 

#Variable Importance
importance_values <- importance(freq_rf)
mse_importance <- importance_values[, "%IncMSE"]

mse_df <- data.frame(Variable = rownames(importance_values), MSE = mse_importance)

ggplot(mse_df, aes(x = reorder(Variable, MSE), y = MSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flips the axes for better readability
  labs(title = "Variable Importance (Mean Decrease MSE)",
       x = "Variables",
       y = "Mean Decrease MSE") +
  theme_minimal()


#Test Performance
freq_rf_prediction <- predict(freq_rf, newdata = freq_test)
freq_rf_prediction <- as.vector(freq_rf_prediction)

freq_rf_prediction[freq_rf_prediction < 0] <- 0 #setting negative predictions to 0


freq_rf_test_mse <- mean((freq_rf_prediction - freq_test$claim_freq)^2)



#########Full Linear Regression########
full_frequency_LR <- lm(claim_freq~., data = freq_training_val) #training

#Summary and Training Performace
full_frequency_LR_summary <- summary(full_frequency_LR)

assumption_summaryplot <- plot(full_frequency_LR)

predictions <- predict(full_frequency_LR, newdata = freq_training_val)

full_freq_LR_training_MSE <- mean((freq_training_val$claim_freq - predictions)^2)


full_frequency_LR_summary$adj.r.squared

full_frequency_LR_summary$fstatistic

full_freq_AIC <- AIC(full_frequency_LR)

#Test Performance
full_freq_predicted_values <- predict(full_frequency_LR, newdata = freq_test, type = "response")
full_freq_predicted_values <- as.vector(full_freq_predicted_values)

full_freq_test_MSE <- mean((full_freq_predicted_values - freq_test$claim_freq)^2)


####Step wise Linear Regression####

stepwise_model_freq <- stepAIC(full_frequency_LR, direction = "both")

stepwise_model_freq_summary <- summary(stepwise_model_freq)

stepwise_model_freq_summary$adj.r.squared

predictions_stepwise <- predict(stepwise_model_freq, newdata = freq_training_val)
stepwise_model_freq_training_MSE <- mean((freq_training_val$claim_freq - predictions_stepwise)^2)

AIC(stepwise_model_freq)

#test performance

stepwise_model_freq_predicted_values <- predict(stepwise_model_freq, newdata = freq_test, type = "response")
stepwise_model_freq_predicted_values <- as.vector(stepwise_model_freq_predicted_values)

stepwise_freq_model_test_MSE <- mean((stepwise_model_freq_predicted_values-freq_test$claim_freq)^2)


####Lasso Model####
X.training <- model.matrix(claim_freq~., data = freq_training_val)[,-1]

Y.training <- freq_training_val$claim_freq

freq_lasso_model <- cv.glmnet(X.training, Y.training, alpha = 1)

lasso_optimal_lambda <- freq_lasso_model$lambda.min

final_freq_lasso_model <- glmnet(X.training, Y.training, lambda = lasso_optimal_lambda, alpha = 1)

coef(final_freq_lasso_model)


lasso_freq_predictions_training <- predict(final_freq_lasso_model, newx = X.training, type = "response")
lasso_freq_predictions_training <- as.vector(lasso_freq_predictions_training)
training_lasso_MSE <- mean((lasso_freq_predictions_training-Y.training)^2)


#Test Performance
x.test <- model.matrix(claim_freq~., data = freq_test)[,-1]
y.test <- freq_test$claim_freq


lasso_prediction_freq <- predict(final_freq_lasso_model, newx = x.test, type = "response")
lasso_prediction_freq <- as.vector(lasso_prediction_freq)

lasso_test_mse <- mean((lasso_prediction_freq - y.test)^2)

###Ridge###

freq_ridge_model <- cv.glmnet(X.training, Y.training, alpha = 0)
ridge_optimal_lambda <- freq_ridge_model$lambda.min

final_ridge_model_freq <- glmnet(X.training, Y.training, lambda = ridge_optimal_lambda, alpha = 0)

ridge_freq_predictions_training <- predict(final_ridge_model_freq, newx = X.training, type = "response")
ridge_freq_predictions_training <- as.vector(ridge_freq_predictions_training)
training_ridge_MSE <- mean((ridge_freq_predictions_training-Y.training)^2)


coef(final_ridge_model_freq)

#test performance#
ridge_freq_predictions <- predict(final_ridge_model_freq, newx = x.test, type = "response")
ridge_freq_predictions <- as.vector(ridge_freq_predictions) 

ridge_test_mse <- mean((ridge_freq_predictions - y.test)^2)



###Elastic Net###
set.seed(12313) 
alpha_values <- seq(0, 1, by = 0.1)
results <- data.frame(alpha = numeric(), lambda.min = numeric(), mse = numeric())

for (alpha in alpha_values) {
  cv_fit <- cv.glmnet(X.training, Y.training, alpha = alpha, nfolds = 10)
  results <- rbind(results, data.frame(alpha = alpha, 
                                       lambda.min = cv_fit$lambda.min, 
                                       mse = min(cv_fit$cvm)))
}
best_alpha <- results[which.min(results$mse), "alpha"]
best_lambda <- results[which.min(results$mse), "lambda.min"]

final_elastic_net_model <- glmnet(X.training, Y.training, alpha = best_alpha, lambda = best_lambda)

coef(final_elastic_net_model)
summary(final_elastic_net_model)


final_elastic_net_model_training_predictions <- predict(final_elastic_net_model, newx = X.training, type = "response")
final_elastic_net_model_training_predictions <- as.vector(final_elastic_net_model_training_predictions)
elastic_training_MSE <- mean((final_elastic_net_model_training_predictions-Y.training)^2)

elastic_net_cve <- min(results$mse)

##test performance
elastic_freq_predictions <- predict(final_elastic_net_model, newx = x.test, type = "response")
elastic_freq_predictions <- as.vector(elastic_freq_predictions)

elastic_test_mse <- mean((elastic_freq_predictions - y.test)^2)



