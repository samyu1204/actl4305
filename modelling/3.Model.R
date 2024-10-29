# Load necessary libraries
library(stats)
library(dplyr)
library(glmnet)
library(dplyr)
library(gamlr)
library(randomForest)
library(MASS)
library(glmnet)

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

















vars.to.remove <- c("exposure_id", "pet_gender", "pet_de_sexed_age", "pet_is_switcher", "nb_address_type_adj", "nb_suburb", "nb_state", "person_dob", "owner_age_years", "nb_breed_type",
                    "nb_breed_trait", "nb_breed_name_unique", "nb_breed_name_unique_concat", "exposure_id_1", "earned_units", "Total_Earned", "claim_nb", "Total_claim_amount", 
                    "Total_claim_paid", "severity", "frequency", "is_multi_plan", "quote_time_group", "sa2_code", "nb_postcode")

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


freq_rf$mse
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

full_freq_LR_training_MSE <- mean(full_frequency_LR$residuals^2)

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

stepwise_model_freq_training_MSE <- mean(stepwise_model_freq_summary$residuals^2)

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


## Shifting observations were claim_freq is observed to be 0 such that gamma is appropriate


freq_gamma_training_val <- freq_training_val

for (i in 1:nrow(freq_gamma_training_val)) {
  if (freq_gamma_training_val$claim_freq[i] == 0) {
    freq_gamma_training_val$claim_freq[i] <- 0.001
  }
  
}

#scaling predictors
#freq_gamma_training_val <- freq_gamma_training_val %>%
#mutate(across(where(is.numeric) & !starts_with("claim_freq"), scale))


###gamma frequency glm###:NOT WORKING!!!

gamma_frequency_glm <- glm(claim_freq ~., data = freq_gamma_training_val, family = Gamma(link = "log"))


#inverse gaussian
inverse_gaussian_glm <- glm(claim_freq ~., data = freq_gamma_training_val, family = inverse.gaussian(link = "log"))






