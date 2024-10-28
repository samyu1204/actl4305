library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)


setwd("C:/Users/samyu/code/actl4305")

UNSW_claims_data_raw = read.csv("data/UNSW_claims_data.csv", header=TRUE )
UNSW_claims_data = UNSW_claims_data_raw


UNSW_earned_data_raw = read.csv("data/UNSW_earned_data_adjusted_Sep27.csv", header=TRUE)
UNSW_earned_data = UNSW_earned_data_raw

#UNSW claims data

##Cleaning the data

UNSW_claims_data$claim_start_date = as.Date(UNSW_claims_data$claim_start_date)
UNSW_claims_data$claim_status = as.factor(UNSW_claims_data$claim_status)
UNSW_claims_data$condition_category = as.factor(UNSW_claims_data$condition_category)

## Removing weird case (tenure < 0 -> 2 row only)
#Removing negative tenures and 0 total claim amounts
UNSW_claims_data = 
  UNSW_claims_data %>%
  filter(tenure >= 0, total_claim_amount>0)

zero_claim_paid <- UNSW_claims_data %>%
  filter(claim_paid <= 0)


UNSW_earned_data %>% filter(exposure_id == "exposure_11489811-f986-4ef2-9090-46db3f423860")

## removing totally duplicated row
## get distinct  
UNSW_claims_data = #Removing Duplicate Rows in UNSW Claims Data
  UNSW_claims_data %>%
  distinct()

##look at the duplicated row
duplicated_claim_row = UNSW_claims_data[duplicated(UNSW_claims_data),]

UNSW_claims_data %>% filter(claim_id %in% duplicated_claim_row$claim_id)
#Investigating duplicated claim id
n=table(UNSW_claims_data$claim_id)
nrow(n[n!=1])

NumberOfRowsPerClaim =
  UNSW_claims_data %>%
  group_by(claim_id) %>%
  summarise(NumberOfAppearances = n()) %>%
  filter(NumberOfAppearances > 1)
VectorOfRepeatedClaims = NumberOfRowsPerClaim$claim_id
length(VectorOfRepeatedClaims)

UNSW_claims_data_ONLY_Repeated_claim_ids = UNSW_claims_data %>%
  filter(claim_id %in% VectorOfRepeatedClaims)

UNSW_claims_data %>% group_by(claim_status) %>% summarise(n = n(), Total_claim_paid = sum(claim_paid))

## this doesn't do anything...
Claims_Data_NRC = #Earned Data with No Repeated Claim IDs
  UNSW_claims_data %>%
  group_by(claim_id) 

#Investigating duplicate claims

n=table(UNSW_claims_data$claim_id)
nrow(n[n!=1])

NumberOfRowsPerClaim =
  UNSW_claims_data %>%
  group_by(claim_id) %>%
  summarise(NumberOfAppearances = n()) %>%
  filter(NumberOfAppearances > 1)
VectorOfRepeatedClaims = NumberOfRowsPerClaim$claim_id
length(VectorOfRepeatedClaims)

UNSW_claims_data_ONLY_Repeated_claim_ids = UNSW_claims_data %>%
  filter(claim_id %in% VectorOfRepeatedClaims)

Claims_Data_NRC = #Earned Data with No Repeated Claim IDs
  UNSW_claims_data %>%
  group_by(claim_id) 

Claim_Volume_Vs_Exposure_ID = #Table of Exposure Ids with their respective claim volumes
  UNSW_claims_data %>%
  group_by(exposure_id)%>%
  summarise(ClaimVolume = n())

##Investigating each variable

par(mfrow = c(2, 2))

#UNSW Earned Data

##Cleaning the data     
UNSW_earned_data$UW_Date= as.Date(UNSW_earned_data$UW_Date)
UNSW_earned_data$pet_gender = as.factor(UNSW_earned_data$pet_gender)
UNSW_earned_data$pet_de_sexed = as.factor(UNSW_earned_data$pet_de_sexed)
#UNSW_earned_data$pet_de_sexed_age=as.numeric(UNSW_earned_data$pet_de_sexed_age)
UNSW_earned_data$pet_is_switcher= as.factor(UNSW_earned_data$pet_is_switcher)
UNSW_earned_data$nb_policy_first_inception_date = as.Date(UNSW_earned_data$nb_policy_first_inception_date)
UNSW_earned_data$nb_address_type_adj = as.factor(UNSW_earned_data$nb_address_type_adj)
UNSW_earned_data$nb_breed_name_unique = as.factor(UNSW_earned_data$nb_breed_name_unique)
UNSW_earned_data$nb_breed_name_unique_concat = as.factor(UNSW_earned_data$nb_breed_name_unique_concat)
UNSW_earned_data$is_multi_pet_plan = as.factor(UNSW_earned_data$is_multi_pet_plan)
UNSW_earned_data = UNSW_earned_data %>% select(-pet_age_years, -X)
UNSW_earned_data$nb_state = as.factor(UNSW_earned_data$nb_state)
UNSW_earned_data$person_dob = as.Date(UNSW_earned_data$person_dob)
UNSW_earned_data$nb_breed_type = as.factor(UNSW_earned_data$nb_breed_type)
UNSW_earned_data$nb_breed_trait = as.factor(UNSW_earned_data$nb_breed_trait)
UNSW_earned_data$lead_date_day = as.Date(UNSW_earned_data$lead_date_day, origin = "1970-01-01")
UNSW_earned_data$quote_date= as.Date(UNSW_earned_data$quote_date, origin = "1970-01-01")
UNSW_earned_data$quote_time_group= as.factor(UNSW_earned_data$quote_time_group)
UNSW_earned_data$nb_postcode= as.factor(UNSW_earned_data$nb_postcode)

###Removing useless row number column
UNSW_earned_data = 
  UNSW_earned_data %>%
  select(-row_num)

## check for duplicate
sum(duplicated(UNSW_earned_data))

### check for num of unique exposure id
length(unique(UNSW_earned_data$exposure_id))
length(unique(UNSW_earned_data$exposure_id_1))


## calculate exposure units / earned units for each exposure id
exposure <- UNSW_earned_data %>%
  group_by(exposure_id) %>%
  summarise(Total_Earned = sum(earned_units))

## make exposure id collapse into 
#Ensuring that each row in UNSW_earned_data contains a unique exposure_id
collapsed_UNSW_earned_data = #Earned Data with No Repeated Exposure IDs
  UNSW_earned_data %>%
  group_by(exposure_id) %>%
  filter(tenure == max(tenure))

collapsed_UNSW_earned_data <- left_join(collapsed_UNSW_earned_data, exposure, by = "exposure_id")

## refund for cancellation before 4 days?
zero_earned_units <- UNSW_earned_data %>% filter(earned_units == 0)

## if zero total earned units then also zero claim number

#Removing negative tenures and 0 claim_paid for covered already paid
## keep not paid cover for now
UNSW_claims_data = 
  UNSW_claims_data %>%
  filter(claim_paid>0 | claim_status %in% c("covered_not_paid", "covered_with_exclusions_not_paid"))


## check for duplicated claim because of owner double claim / other owner related issues
## many dimension ## ignore for now
UNSW_claim_check = UNSW_claims_data %>%
  select(exposure_id, total_claim_amount)

##
claim_per_exposure_id = UNSW_claims_data %>%
  group_by(exposure_id) %>%
  summarise(claim_nb = n(), Total_claim_amount = sum(total_claim_amount),
            Total_claim_paid = sum(claim_paid), )

### Aggregate UNSW_earned_data and claim data (claim nb, claim paid and total amount)

UNSW_earned_data$claim_nb = 0
combined_data = left_join(collapsed_UNSW_earned_data, claim_per_exposure_id, by = c("exposure_id"))

combined_data = combined_data %>%
  mutate(claim_nb = ifelse(is.na(claim_nb)==TRUE, 0, claim_nb))


combined_data = combined_data %>%
  mutate(Total_claim_amount = ifelse(is.na(Total_claim_amount)==TRUE, 0, Total_claim_amount),
         Total_claim_paid = ifelse(is.na(Total_claim_paid)==TRUE, 0, Total_claim_paid))

combined_data = combined_data %>%
  filter(Total_Earned > 0) %>%
  mutate(claim_freq = claim_nb/Total_Earned)

# ========================================================================================
# Check columns with NA
na_columns_remaining <- colnames(combined_data)[colSums(is.na(combined_data)) > 0]
na_columns_remaining

for (col in colnames(combined_data)) {
  if (is.numeric(combined_data[[col]])) {
    combined_data[[col]][is.na(combined_data[[col]])] <- mean(combined_data[[col]], na.rm = TRUE)
  }
}

# =================================================================================
# Merge SA2 code onto it
sa2_mapping <- read_excel("data/sa2_mapping.xlsx")
names(combined_data$nb_post)

combined_data$nb_postcode <- as.numeric(combined_data$nb_postcode)
sa2_mapping$nb_postcode <- as.numeric(sa2_mapping$nb_postcode)

sa2_mapping <- sa2_mapping %>%
  group_by(nb_postcode) %>%
  slice(1)

sa2_mapping_selected <- sa2_mapping %>%
  select(nb_postcode, SA2_CODE)

# Perform the left join
combined_data <- left_join(combined_data, sa2_mapping_selected, by = "nb_postcode")

intersect(combined_data$nb_postcode, sa2_mapping$nb_postcode)

