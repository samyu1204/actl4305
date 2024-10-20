library("dplyr")
library("scales")
library("gapminder")
library("ggpubr")

data <- UNSW_earned_data_adjusted_Sep27

aggr_data  <- left_join(data, UNSW_claims_data, by = c("exposure_id", "tenure"))

# Data cleaningL
Claims_With_Earned <- Claims_With_Earned %>%
  mutate(age_group = cut(pet_age_months, 
                         breaks = seq(0, max(pet_age_months, na.rm = TRUE), by = 12), 
                         include.lowest = TRUE, 
                         right = FALSE, 
                         labels = paste(seq(0, max(pet_age_months, na.rm = TRUE) - 12, by = 12), 
                                        seq(12, max(pet_age_months, na.rm = TRUE), by = 12), 
                                        sep = "-")))

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
options(scipen = 999)
ggplot(severity_by_age, aes(x = pet_age_months, y = total_claim_amount)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Severity of Claims by Pet Age (in months)",
       x = "Pet Age (Months)",
       y = "Total Claim Amount (Severity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
scale_y_continuous(labels = label_number()) # Avoid scientific notation'


# For average claims:
# Calculate average claim amount (severity) for each age group
severity_by_age_avg <- Claims_With_Earned %>%
  group_by(pet_age_months) %>%
  summarise(avg_claim_amount = mean(claim_paid, na.rm = TRUE))

# Plot average claim amount (severity) by pet age
ggplot(severity_by_age_avg, aes(x = pet_age_months, y = avg_claim_amount)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Average Severity of Claims by Pet Age (in months)",
       x = "Pet Age (Months)",
       y = "Average Claim Amount (Severity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_number()) # Avoid scientific notation

# Year by year groups:
# Calculate average claim amount (severity) for each age group
severity_by_age_group_avg <- Claims_With_Earned %>%
  group_by(age_group) %>%
  summarise(avg_claim_amount = mean(claim_paid, na.rm = TRUE))

# Plot average claim amount by 12-month age group
ggplot(severity_by_age_group_avg, aes(x = age_group, y = avg_claim_amount)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Average Severity of Claims by Pet Age Group (12-month intervals)",
       x = "Pet Age Group (Months)",
       y = "Average Claim Amount (Severity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_number()) # Avoid scientific notation

# =============================================================================
# Pet gender severity and frequency
# Calculate frequency of claims and total claim amount (severity) for each gender
gender_stats <- Claims_With_Earned %>%
  group_by(pet_gender) %>%
  summarise(claim_frequency = n(), # Number of claims (frequency)
            total_claim_amount = sum(claim_paid, na.rm = TRUE), # Total claim amount (severity)
            avg_claim_amount = mean(claim_paid, na.rm = TRUE)) # Average claim amount (severity)

# Plot 1: Claim Frequency by Pet Gender
plot1 <- ggplot(gender_stats, aes(x = pet_gender, y = claim_frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Pet Gender",
       x = "Pet Gender",
       y = "Claim Frequency") +
  theme_minimal()

# Plot 2: Claim Severity by Pet Gender (Total Claim Amount)
plot2 <- ggplot(gender_stats, aes(x = pet_gender, y = total_claim_amount)) +
  geom_bar(stat = "identity", fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Total Severity of Claims by Pet Gender",
       x = "Pet Gender",
       y = "Total Claim Amount") +
  theme_minimal() +
  scale_y_continuous(labels = label_number())

# Plot 3: Average Claim Severity by Pet Gender
plot3 <- ggplot(gender_stats, aes(x = pet_gender, y = avg_claim_amount)) +
  geom_bar(stat = "identity", fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Average Severity of Claims by Pet Gender",
       x = "Pet Gender",
       y = "Average Claim Amount") +
  theme_minimal() +
  scale_y_continuous(labels = label_number())

# Arrange the two plots side by side
ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)


# Owner characteristics:
# Calculate claim frequency and total claim amount (severity) for each owner age group
age_stats <- Claims_With_Earned %>%
  group_by(owner_age_years) %>%
  summarise(claim_frequency = n(), # Number of claims (frequency)
            total_claim_amount = sum(claim_paid, na.rm = TRUE), # Total claim amount (severity)
            avg_claim_amount = mean(claim_paid, na.rm = TRUE)) # Average claim amount (severity)

# Plot 1: Claim Frequency by Owner Age
plot1 <- ggplot(age_stats, aes(x = owner_age_years, y = claim_frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Owner Age",
       x = "Owner Age (Years)",
       y = "Claim Frequency") +
  theme_minimal()

# Plot 2: Total Severity by Owner Age (Total Claim Amount)
plot2 <- ggplot(age_stats, aes(x = owner_age_years, y = total_claim_amount)) +
  geom_bar(stat = "identity", fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Total Severity of Claims by Owner Age",
       x = "Owner Age (Years)",
       y = "Total Claim Amount") +
  theme_minimal() +
  scale_y_continuous(labels = label_number())

# Plot 3: Average Severity by Owner Age (Average Claim Amount)
plot3 <- ggplot(age_stats, aes(x = owner_age_years, y = avg_claim_amount)) +
  geom_bar(stat = "identity", fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Average Severity of Claims by Owner Age",
       x = "Owner Age (Years)",
       y = "Average Claim Amount") +
  theme_minimal() +
  scale_y_continuous(labels = label_number())

# Arrange the plots side by side for easy comparison
ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)

# =============================================================================
# Breed size
# Calculate claim frequency and total claim amount (severity) for each breed size
breed_size_stats <- Claims_With_Earned %>%
  group_by(nb_average_breed_size) %>%
  summarise(claim_frequency = n(), # Number of claims (frequency)
            total_claim_amount = sum(claim_paid, na.rm = TRUE), # Total claim amount (severity)
            avg_claim_amount = mean(claim_paid, na.rm = TRUE)) # Average claim amount (severity)

# Plot 1: Claim Frequency by Breed Size
plot1 <- ggplot(breed_size_stats, aes(x = nb_average_breed_size, y = claim_frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Claim Frequency by Breed Size",
       x = "Breed Size",
       y = "Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Total Severity by Breed Size (Total Claim Amount)
plot2 <- ggplot(breed_size_stats, aes(x = nb_average_breed_size, y = total_claim_amount)) +
  geom_bar(stat = "identity", fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Total Severity of Claims by Breed Size",
       x = "Breed Size",
       y = "Total Claim Amount") +
  theme_minimal() +
  scale_y_continuous(labels = label_number()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Average Severity by Breed Size (Average Claim Amount)
plot3 <- ggplot(breed_size_stats, aes(x = nb_average_breed_size, y = avg_claim_amount)) +
  geom_bar(stat = "identity", fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Average Severity of Claims by Breed Size",
       x = "Breed Size",
       y = "Average Claim Amount") +
  theme_minimal() +
  scale_y_continuous(labels = label_number()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arrange the plots side by side for easy comparison
ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)