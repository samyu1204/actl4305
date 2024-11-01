library(readr)
sample <- read_csv("data/New_Customers_Pricing_Output_File.csv")

start_cols <- colnames(sample)

# ===============================================================================
# ==============================================================================
# Feature engineering
# Numerical variable conversion
sample$pet_is_male <- ifelse(sample$pet_gender == 'male', 1, 0)

# Convert complex type into numerical encoding
sample$nb_address_type_adj <- as.factor(sample$nb_address_type_adj)
sample$nb_address_type_adj_numerical <- as.numeric(sample$nb_address_type_adj)

# Convert state
sample$nb_state <- as.factor(sample$nb_state)
sample$nb_state_num <- as.numeric(sample$nb_state)

# Convert breed type
sample$nb_breed_type <- as.factor(sample$nb_breed_type)
sample$nb_breed_type_num <- as.numeric(sample$nb_breed_type)

# Convert pet desexed
sample$pet_de_sexed <- as.factor(sample$pet_de_sexed)
sample$nb_breed_type_num <- as.numeric(sample$nb_breed_type)

# Convert breed trait
sample$nb_breed_trait <- as.factor(sample$nb_breed_trait)
sample$nb_breed_trait_num <- as.numeric(sample$nb_breed_trait)

# Multi-pet plan
sample$is_multi_pet_plan <- as.factor(sample$is_multi_pet_plan)
sample$is_multi_pet_plan_num <- as.numeric(sample$is_multi_pet_plan)

# Quote average time
sample$quote_time_group <- as.factor(sample$quote_time_group)
sample$quote_time_group_num <- as.numeric(sample$quote_time_group)

# Breed size encoding
sample <- sample %>%
  mutate(size_encoding = ifelse(nb_average_breed_size %in% c(1, 2, 3), 1, 0))
sample$size_encoding

# Owner age * pet age
sample$owner_x_pet_age <- sample$owner_age_years * sample$pet_age_months / 12

sample$owner_x_pet_age <- cut(sample$owner_x_pet_age, 
                                     breaks = c(seq(0, 700, by = 15), Inf), 
                                     labels = as.character(1:(length(seq(0, 700, by = 15)))))

sample$owner_x_pet_age <- pmin(as.numeric(sample$owner_x_pet_age), 10)


sample$owner_age_years


# Breed interactions
sample$age_breed_interaction <- sample$pet_age_months * sample$nb_average_breed_size

sample$age_breed_interaction %>% summary
sample$nb_average_breed_size
sample$age_breed_interaction_bins <- cut(sample$age_breed_interaction, 
                                                breaks = c(seq(0, 200, by = 5), Inf), 
                                                labels = as.character(1:(length(seq(0, 200, by = 5)))))

sample$age_breed_interaction_bins <- as.numeric(sample$age_breed_interaction_bins)
sample$age_breed_interaction <- pmin(sample$age_breed_interaction, 10)

# Owner's age
current_date <- Sys.Date()
sample$person_age <- floor(interval(as.Date(sample$person_dob), current_date) / years(1))

sample$person_age_group <- cut(
  sample$person_age, 
  breaks = seq(0, 100, by = 20),   # Create intervals from 0 to 100, each of 5 years
  right = FALSE,                  # Ensure that the intervals are left-closed (e.g., [0,5), [5,10), etc.)
  labels = paste(seq(0, 95, by = 20), seq(5, 100, by = 20), sep = "-")  # Create labels like "0-5", "5-10", etc.
)
sample$person_age_group <- as.numeric(sample$person_age_group)

# Pet age years
sample$pet_age_year <- sample$pet_age_months / 12

# Order the factors into numerical
sample <- merge(sample, average_severity_by_trait[, c("nb_breed_trait_num", "nb_breed_trait_num_encoded")],
                   by = "nb_breed_trait_num", all.x = TRUE)

# =========
# Merge SA2 code onto it
sa2_mapping <- read_excel("data/sa2_mapping.xlsx")
sample$nb_postcode <- as.character(sample$nb_postcode)
sa2_mapping$nb_postcode <- as.character(sa2_mapping$nb_postcode)

sa2_mapping <- sa2_mapping %>%
  dplyr::group_by(nb_postcode) %>%
  dplyr::slice(1)

sa2_mapping_selected <- sa2_mapping %>%
  dplyr::select(nb_postcode, SA2_CODE)

# Perform the left join to get SA2 Code
sample <- left_join(sample, sa2_mapping_selected, by = "nb_postcode")

sample$sa2_code

names(sample)[names(sample) == "SA2_CODE"] <- "sa2_code"

sample$sa2_code <- as.character(sample$sa2_code)


# QI quality indicator of the postcode
qi <- read_excel("data/QI.xlsx")

qi <- qi %>%
  dplyr::group_by(sa2_code) %>%
  dplyr::slice(1)


# Join onto 
sample <- left_join(sample, qi, by = "sa2_code")

# Join pop density
sa2_pop_density <- read_excel("data/sa2_pop_density.xlsx")
sa2_pop_density$sa2_code <- as.character(sa2_pop_density$sa2_code)
sample <- left_join(sample, sa2_pop_density, by = "sa2_code")

sample$UW_Date <- as.Date(sample$quote_date)
sample$nb_policy_first_inception_date <- as.Date(sample$quote_date)
sample$lead_date_day <- as.Date(sample$quote_date)
sample$nb_breed_trait_num_encoded.x <- as.factor(sample$nb_breed_trait_num_encoded)
sample$nb_breed_trait_num_encoded.y <- as.factor(sample$nb_breed_trait_num_encoded)
sample$pet_de_sexed <- tolower(sample$pet_de_sexed) 
sample$is_multi_pet_plan <- tolower(sample$is_multi_pet_plan)
sample$tenure <- 1
# ===============================================================================
# Predict severity
sample$pred_severity <- predict(glm_model_severity, newdata = sample, type = "response")

sample$pred_severity %>% summary

# Predict frequency
sample$severity_gbm <- ifelse(
  sample$pred_severity > 1000,
  predict(gbm_tuned_severity, newdata = sample),
  0  # Set to 0 if severity is <= 1000
)

sample$pred_severity_final <- sample$pred_severity + sample$severity_gbm
sample$pred_severity_final %>% summary

# Give the min of the group to those who are predicted to have zero claim to spread the risk
sample$pred_severity_final[is.na(sample$pred_severity)] <- 212
sample$pred_severity_final
# ===============================================================================
# Freq prediction
sample$pred_freq <- predict(tweedie_freq_model, newdata = sample, type = "response")
sample$pred_freq %>% summary


