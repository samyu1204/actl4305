library(caret)
library(glmnet)
library(fastDummies)
library(sf)
library(ggplot2)
library(xgboost)
library(Matrix)
library(caret)
library(lubridate)

# target columns:
# Create predicting columns
# combined_data$severity <- pmin(combined_data$Total_claim_amount / combined_data$claim_nb, 700)
combined_data$severity <- log(combined_data$Total_claim_amount / combined_data$claim_nb)
combined_data$frequency <- combined_data$claim_freq

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

# Convert pet desexed
combined_data$pet_de_sexed <- as.factor(combined_data$pet_de_sexed)
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

# Breed size encoding
combined_data <- combined_data %>%
  mutate(size_encoding = ifelse(nb_average_breed_size %in% c(1, 2, 3), 1, 0))
combined_data$size_encoding

# Owner age * pet age
combined_data$owner_x_pet_age <- combined_data$owner_age_years * combined_data$pet_age_months / 12

combined_data$owner_x_pet_age <- cut(combined_data$owner_x_pet_age, 
                                     breaks = c(seq(0, 700, by = 15), Inf), 
                                     labels = as.character(1:(length(seq(0, 700, by = 15)))))

combined_data$owner_x_pet_age <- pmin(as.numeric(combined_data$owner_x_pet_age), 10)

ggplot(combined_data, aes(x = owner_x_pet_age, y = severity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Severity by Age-Breed Interaction Bins",
       x = "Age-Breed Interaction Bins",
       y = "Average Severity") +
  theme_minimal()

combined_data$owner_age_years


# Breed interactions
combined_data$age_breed_interaction <- combined_data$pet_age_months * combined_data$nb_average_breed_size

combined_data$age_breed_interaction %>% summary
combined_data$nb_average_breed_size
combined_data$age_breed_interaction_bins <- cut(combined_data$age_breed_interaction, 
                                                breaks = c(seq(0, 200, by = 5), Inf), 
                                                labels = as.character(1:(length(seq(0, 200, by = 5)))))

combined_data$age_breed_interaction_bins <- as.numeric(combined_data$age_breed_interaction_bins)

ggplot(combined_data, aes(x = age_breed_interaction_bins, y = severity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Severity by Age-Breed Interaction Bins",
       x = "Age-Breed Interaction Bins",
       y = "Average Severity") +
  theme_minimal()
combined_data$severity %>% summary
combined_data$age_breed_interaction <- pmin(combined_data$age_breed_interaction, 10)

# Owner's age
current_date <- Sys.Date()
combined_data$person_age <- floor(interval(as.Date(combined_data$person_dob), current_date) / years(1))

combined_data$person_age_group <- cut(
  combined_data$person_age, 
  breaks = seq(0, 100, by = 20),   # Create intervals from 0 to 100, each of 5 years
  right = FALSE,                  # Ensure that the intervals are left-closed (e.g., [0,5), [5,10), etc.)
  labels = paste(seq(0, 95, by = 20), seq(5, 100, by = 20), sep = "-")  # Create labels like "0-5", "5-10", etc.
)
combined_data$person_age_group <- as.numeric(combined_data$person_age_group)

ggplot(combined_data, aes(x = person_age_group, y = severity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Severity by Age-Breed Interaction Bins",
       x = "Age-Breed Interaction Bins",
       y = "Average Severity") +
  theme_minimal()

# Pet age years
combined_data$pet_age_year <- combined_data$pet_age_months / 12

# Convert breed trait into numeric
# Step 1: Calculate the mean severity for each level of nb_breed_trait_num
average_severity_by_trait <- aggregate(severity ~ nb_breed_trait_num, data = combined_data, FUN = mean)

# Step 2: Order the nb_breed_trait_num levels based on the average severity
average_severity_by_trait <- average_severity_by_trait[order(average_severity_by_trait$severity), ]

# Step 3: Assign numeric encoding based on the ordered severity
combined_data$nb_breed_trait_num_encoded <- factor(
  combined_data$nb_breed_trait_num, 
  levels = average_severity_by_trait$nb_breed_trait_num,   # Ordering the levels based on severity
  ordered = TRUE                                           # Ensuring it's treated as an ordered factor
)

# Order the factors into numerical
combined_data$nb_breed_trait_num_encoded <- as.numeric(combined_data$nb_breed_trait_num_encoded)

ggplot(combined_data, aes(x = nb_breed_trait_num_encoded, y = severity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Severity by breed trait",
       x = "Breed Trait Group",
       y = "Average Severity") +
  theme_minimal()

# =========================================================================================
# Postcode features


summarized_data <- combined_data %>%
  group_by(qi) %>%
  summarize(avg_severity = mean(severity, na.rm = TRUE))

ggplot(summarized_data, aes(x = qi, y = avg_severity)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Severity by QI Level",
       x = "QI Level",
       y = "Average Severity") +
  theme_minimal()




# ========================================================================================
# Visualizing the relationship between binned Age * Breed Size and frequency
