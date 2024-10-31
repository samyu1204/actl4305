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
library(MASS)

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






#################Claim Frequency Modelling#################


vars.to.remove <- c("exposure_id", "pet_gender", "pet_de_sexed_age", "pet_is_switcher", "nb_address_type_adj", "nb_suburb", "nb_state", "person_dob", "owner_age_years", "nb_breed_type",
                    "nb_breed_trait", "nb_breed_name_unique", "nb_breed_name_unique_concat", "exposure_id_1", "earned_units", "Total_Earned", "claim_nb", "Total_claim_amount", 
                    "Total_claim_paid", "severity", "frequency", "is_multi_plan", "quote_time_group", "sa2_code", "nb_postcode", "is_multi_pet_plan", "pet_age_year")

frequency.model.data <- combined_data[,-which(colnames(combined_data) %in% vars.to.remove)]

frequency.model.data$qi <- as.factor(frequency.model.data$qi)

frequency.model.data <- na.omit(frequency.model.data)

str(frequency.model.data)

#Splitting into test and training

set.seed(2131)

freq_training_val_index <- sample(1:nrow(frequency.model.data), 0.7*nrow(frequency.model.data))

freq_training_val <- frequency.model.data[freq_training_val_index, ]

freq_test <- frequency.model.data[-freq_training_val_index, ]



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
X.training <- model.matrix(claim_freq~., data = freq_training_val)

Y.training <- freq_training_val$claim_freq

freq_lasso_model <- cv.glmnet(X.training, Y.training, alpha = 1)

lasso_optimal_lambda <- freq_lasso_model$lambda.min

final_freq_lasso_model <- glmnet(X.training, Y.training, lambda = lasso_optimal_lambda, alpha = 1)

coef(final_freq_lasso_model)


lasso_freq_predictions_training <- predict(final_freq_lasso_model, newx = X.training, type = "response")
lasso_freq_predictions_training <- as.vector(lasso_freq_predictions_training)
training_lasso_MSE <- mean((lasso_freq_predictions_training-Y.training)^2)


#Test Performance
x.test <- model.matrix(claim_freq~., data = freq_test)
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


###tweedie GLM####

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

summary(tweedie_freq_model)

#Perfomance on the training data
tweedie_freq_model_training_predictions <- predict(tweedie_freq_model,  newdata = freq_training_val, type = "response")
tweedie_freq_model_training_predictions <- as.vector(tweedie_freq_model_training_predictions)

training_MSE_tweedie <- mean((tweedie_freq_model_training_predictions-freq_training_val$claim_freq)^2)


#Performance on the test data 
tweedie_freq_model_prediction <- predict(tweedie_freq_model,  newdata = freq_test, type = "response")
tweedie_freq_model_prediction <- as.vector(tweedie_freq_model_prediction)

test_MSE_tweedie <- mean((tweedie_freq_model_prediction-freq_test$claim_freq)^2)

sdu <- summary(tweedie_freq_model)

tweedie_freq_model$deviance


#ADJ R^2
RSS <- sum((freq_training_val$claim_freq - predictions)^2)

R2 <- 1 - (RSS / SST)

n <- nrow(freq_training_val)
k <- length(coef(tweedie_freq_model)) - 1  

R2_adj <- 1 - ((1 - R2) * (n - 1) / (n - k - 1))


##Assessing Residuals
fitted_values <- tweedie_freq_model$fitted.values

# Calculate residuals
residuals <- tweedie_freq_model$residuals

ggplot(data.frame(Fitted = fitted_values, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


#Actual Vs Predicted

ggplot(data.frame(Actual = freq_training_val$claim_freq, residuals = tweedie_freq_model$residuals), aes(x = freq_training_val$claim_freq, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual Frequency vs Residuals", x = "Actual Values", y = "Residuals") +
  theme_minimal() 