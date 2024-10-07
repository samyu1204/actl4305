library("dplyr")
library("scales")
library("gapminder")

data <- UNSW_earned_data_adjusted_Sep27

aggr_data  <- left_join(data, UNSW_claims_data, by = c("exposure_id", "tenure"))

# Claim frequency by tenure
ggplot(UNSW_claims_data, aes(x = tenure)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Policy Tenure",
       x = "Policy Tenure (Months)",
       y = "Claim Frequency") +
  theme_minimal()

# Distribution of from the earned data
ggplot(UNSW_earned_data, aes(x = tenure)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Policy Tenure",
       x = "Policy Tenure (Months)",
       y = "Claim Frequency") +
  theme_minimal()

# Does owner age affect making a claim
ggplot(Earned_Data_NRE, aes(x = tenure)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Policy Tenure",
       x = "Policy Tenure (Months)",
       y = "Claim Frequency") +
  theme_minimal()

# Breed of the dog
# Create bar plot for categorical variable 'nb_breed_name_unique'
ggplot(Claims_With_Earned, aes(x = nb_breed_name_unique)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Breed",
       x = "Breed",
       y = "Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Frequency by breed
ggplot(Claims_With_Earned, aes(x = pet_age_months)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Breed",
       x = "Age",
       y = "Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Severity by age month
# Calculate total claim amount (severity) for each age group
severity_by_age <- Claims_With_Earned %>%
  group_by(pet_age_months) %>%
  summarise(total_claim_amount = sum(total_claim_amount, na.rm = TRUE))

# Plot total claim amount (severity) by pet age
Option(scipen = 999) 
ggplot(severity_by_age, aes(x = pet_age_months, y = total_claim_amount)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Severity of Claims by Pet Age (in months)",
       x = "Pet Age (Months)",
       y = "Total Claim Amount (Severity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
scale_y_continuous(labels = label_number()) # Avoid scientific notation

