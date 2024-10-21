library(caret)
library(glmnet)
library(fastDummies)
library(sf)
library(ggplot2)
library(xgboost)
library(Matrix)
library(lubridate)

# ==============================================================================
# Feature engineering
# Numerical variable conversion
combined_data$pet_is_male <- ifelse(combined_data$pet_gender == 'male', 1, 0)

# Convert complex type into numerical encoding
combined_data$nb_address_type_adj <- as.factor(combined_data$nb_address_type_adj)
combined_data$nb_address_type_adj_numerical <- as.numeric(combined_data$nb_address_type_adj)

# Convert state
combined_data$nb_state <- as.factor(combined_data$nb_state)
combined_data$nb_state_num <- as.numeric(combined_data$nb_state)

# Convert breed type
combined_data$nb_breed_type <- as.factor(combined_data$nb_breed_type)
combined_data$nb_breed_type_num <- as.numeric(combined_data$nb_breed_type)

# Convert breed trait
combined_data$nb_breed_trait <- as.factor(combined_data$nb_breed_trait)
combined_data$nb_breed_trait_num <- as.numeric(combined_data$nb_breed_trait)

# Multi-pet plan
combined_data$is_multi_pet_plan <- as.factor(combined_data$is_multi_pet_plan)
combined_data$is_multi_pet_plan_num <- as.numeric(combined_data$is_multi_pet_plan)

# Quote average time
combined_data$quote_time_group <- as.factor(combined_data$quote_time_group)
combined_data$quote_time_group_num <- as.numeric(combined_data$quote_time_group)

# Breed interactions
combined_data$age_breed_interaction <- combined_data$pet_age_months * combined_data$nb_average_breed_size

# Person's age
combined_data$owners_age <- (as.numeric(floor(interval(as.Date(combined_data$person_dob), Sys.Date()) / years(1))))


# Bucketing owner's age into 4 groups and labeling them with numerical values
combined_data$age_bucket <- cut(combined_data$owners_age, 
                                breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),  # Define the age intervals
                                labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),  # Assign numerical labels for each bucket
                                right = FALSE)  # Include lower bound, exclude upper bound

combined_data$age_bucket <- pmin(pmax(combined_data$age_bucket, 5), 14)

ggplot(combined_data, aes(x = age_bucket, y = severity)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar chart
  labs(title = "Average Severity by Owner's Age", 
       x = "Owner's Age (Years)", 
       y = "Average Severity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###==== Added on 20 Oct 2024

## changing some variable type
combined_data$pet_de_sexed_age=as.factor(combined_data$pet_de_sexed_age)
combined_data$nb_suburb = as.factor(combined_data$nb_suburb)
combined_data$nb_contribution_excess_factor = as.factor(combined_data$nb_contribution_excess)


## interaction term

# pet_gender and pet_de_sexed
combined_data$pet_gender_de_sexed <- paste(combined_data$pet_gender, combined_data$pet_de_sexed, sep = "_")
(combined_data %>% select(pet_gender, pet_de_sexed, pet_gender_de_sexed))

ggplot(data = combined_data) +
  aes(y = claim_freq, x = as.factor(pet_gender_de_sexed)) +
  geom_violin() +ylim(0,0.5)

## checking
glm_test <- glm(claim_nb ~ pet_gender_de_sexed + nb_contribution_excess_factor +
                  age_bucket + owners_age + as.factor(nb_number_of_breeds),
                data = combined_data, subset = train_indices,
                offset = log(Total_Earned), family = poisson(link = "log"))
summary(glm_test)
###===

# ==============================================================================
# Special Feature creation
# 1. Create an owner's age group, and also a pet's age grouyp and look to feature engineer something out of that
# Currently the age is a bit scattered shown in the plot below
# Assuming your data frame is called 'data' and has columns 'age' and 'frequency'\
# Age bucket * breed size

# Scatter plot for age_breed_interaction vs frequency
combined_data$log_age_breed_interaction <- log1p(combined_data$age_breed_interaction)  # log1p handles zero values safely
combined_data$age_breed_bin <- as.numeric(cut(combined_data$log_age_breed_interaction,
                                              breaks = seq(0, max(combined_data$log_age_breed_interaction, na.rm = TRUE) + 1, by = 1), 
                                              include.lowest = TRUE, 
                                              right = FALSE))


# ========================================================================================
# Visualizing the relationship between binned Age * Breed Size and frequency
ggplot(combined_data, aes(x = age_breed_bin, y = claim_freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Binned Age-Breed Interaction vs. Claim Frequency Frequency", 
       x = "Binned Age * Breed Size (Every 20)", 
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity

# Check severity
# Step 1: Calculate the average claim cost for each age_breed_bin
combined_data_avg <- combined_data %>%
  group_by(age_breed_bin) %>%
  summarise(avg_claim_cost = mean(Total_claim_amount, na.rm = TRUE))

# Step 2: Visualize the relationship between binned Age * Breed Size and average claim cost
ggplot(combined_data_avg, aes(x = age_breed_bin, y = avg_claim_cost)) +
  geom_bar(stat = "identity") +
  labs(title = "Binned Age-Breed Interaction vs. Average Claim Cost", 
       x = "Binned Age * Breed Size", 
       y = "Average Claim Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity

combined_data$owner_pet_age_interaction <- combined_data$owner_age_years * (combined_data$pet_age_months / 12)



# Excess interactions
combined_data$contribution_excess_interaction <- combined_data$nb_contribution * combined_data$nb_excess



# 2. Get pet ownership density

features <- c(
  'pet_is_male',
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


x <- as.matrix(combined_data[, features])

# Define the target variable
y <- combined_data$claim_freq

# Fit the Lasso model
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Get the coefficients of the model at the optimal lambda (lambda.min)
coef_lasso <- coef(lasso_model, s = "lambda.min")

# Extract the names of selected features (those with non-zero coefficients)
selected_features <- rownames(coef_lasso)[which(coef_lasso != 0)]

# Display selected features
selected_features

# ===========================================================================
# Lasso graphing
# Extract the non-zero coefficients at lambda.min
coef_lasso <- coef(lasso_model, s = "lambda.min")

# Get the indices of non-zero coefficients
non_zero_indices <- which(coef_lasso != 0)
# Adjust the x-axis limits to zoom in around the key lambda region (adjust these values as necessary)
xlim <- c(-8, -5)  # Zooming into the region where lambda.min is located

# Adjust the y-axis limits to zoom into the region where coefficients change (adjust as needed)
ylim <- c(-0.02, 0.01)  # Focusing on the coefficient range where most shrinkage happens


# Create a plot for only the non-zero coefficients
plot(lasso_model$glmnet.fit, xvar = "lambda", label = TRUE, xlim = xlim, ylim = ylim)
title(main = "Lasso Coefficient Shrinkage - Non-Zero Coefficients", xlab = "Log(Lambda)", ylab = "Coefficients")

# Highlight the selected features
abline(v = log(lasso_model$lambda.min), col = "blue", lty = 2)

summary(lasso_model$glmnet.fit)




###Pet_de_sexed * Gender

to.include <- c("condition_category", "claim_paid", "pet_gender", "pet_de_sexed", 
                "pet_de_sexed_age", "pet_age_months", "pet_is_switcher", "nb_address_type_adj", "nb_state", "person_dob",
                "owner_age_years", "nb_number_of_breeds", "nb_average_breed_size", "nb_breed_type", "is_multi_pet_plan")

# Subset columns
Claims_With_Earned_new <- Claims_With_Earned[, which(colnames(Claims_With_Earned) %in% to.include)]

# Binary conversion for pet_gender and pet_de_sexed
Claims_With_Earned_new$pet_gender <- ifelse(Claims_With_Earned_new$pet_gender == "female", 1, 0)
Claims_With_Earned_new$pet_gender <- as.numeric(Claims_With_Earned_new$pet_gender)

Claims_With_Earned_new$pet_de_sexed <- ifelse(Claims_With_Earned_new$pet_de_sexed == "true", 1, 0)
Claims_With_Earned_new$pet_de_sexed <- as.numeric(Claims_With_Earned_new$pet_de_sexed)

# Create interaction term
Claims_With_Earned_new <- Claims_With_Earned_new %>%
  mutate(interaction = pet_gender * pet_de_sexed)

# Omit missing values
Claims_With_Earned_new <- na.omit(Claims_With_Earned_new)

# Separate features and target variable for model matrix
X <- Claims_With_Earned_new[, -which(colnames(Claims_With_Earned_new) == "claim_paid")]
X <- model.matrix(~., data = X)[,-1] # Model matrix excluding the intercept

Y <- Claims_With_Earned_new$claim_paid[!is.na(Claims_With_Earned_new$claim_paid)]

# Fit Lasso model using glmnet
fit <- cv.glmnet(x = X, y = Y, alpha = 1)

# Coefficients of the fit model
print(coef(fit, s = "lambda.min"))

# Plot interaction using ggplot
ggplot(Claims_With_Earned_new, aes(x = pet_gender, y = claim_paid, color = as.factor(pet_de_sexed))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = pet_de_sexed), se = FALSE) +
  labs(title = "Interaction Between Pet Gender and Desexed Status on Claim Paid")

#linear models with and without interaction
with.interaction <- lm(claim_paid ~ ., data = Claims_With_Earned_new)
without.interaction <- lm(claim_paid ~ ., data = Claims_With_Earned_new[, -which(colnames(Claims_With_Earned_new) == "interaction")])

# Compare models using AIC
AIC(with.interaction, without.interaction)

summary(with.interaction)

# Random forest model
rf.model <- randomForest(claim_paid ~ ., data = Claims_With_Earned_new, importance = TRUE)

importance_values <- importance(rf.model)[, "%IncMSE"]

par(mar = c(5,10,4,2))
barplot(sort(importance_values, decreasing = TRUE), 
        main = "%IncMSE Variable Importance of the Random Forest Model", 
        horiz = TRUE, 
        las = 1, 
        xlab = "% Increase in MSE", 
        col = "lightblue")

