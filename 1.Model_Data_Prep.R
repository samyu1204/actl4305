library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)


setwd("C:/Users/samyu/code/actl4305")

##claims data
UNSW_claims_data_raw = read.csv("data/UNSW_claims_data.csv", header=TRUE )
UNSW_claims_data = UNSW_claims_data_raw

##earned data
UNSW_earned_data_raw = read.csv("data/UNSW_earned_data_adjusted_Sep27.csv", header=TRUE)
UNSW_earned_data = UNSW_earned_data_raw


###Cleaning the data

## UNSW_claim_data
UNSW_claims_data$claim_start_date = as.Date(UNSW_claims_data$claim_start_date)
UNSW_claims_data$claim_status = as.factor(UNSW_claims_data$claim_status)
UNSW_claims_data$condition_category = as.factor(UNSW_claims_data$condition_category)

## Removing weird case (tenure < 0 -> 2 row only)
#Removing negative tenures and 0 total claim amounts
UNSW_claims_data = 
  UNSW_claims_data %>%
  filter(tenure >= 0, total_claim_amount>0)

#Removing negative tenures and 0 claim_paid for covered already paid
## keep not paid cover for now
UNSW_claims_data = 
  UNSW_claims_data %>%
  filter(claim_paid>0 | claim_status %in% c("covered_not_paid", "covered_with_exclusions_not_paid"))

## removing totally duplicated row
## get distinct  
UNSW_claims_data = #Removing Duplicate Rows in UNSW Claims Data
  UNSW_claims_data %>%
  distinct()

##checking duplicated claim_id
#duplicated_claim_id <- unique(UNSW_claims_data$claim_id[duplicated(UNSW_claims_data$claim_id)])
#View(UNSW_claims_data %>% filter(claim_id %in% duplicated_claim_id))


### checking - delete later
#zero_claim_paid <- UNSW_claims_data %>%
#  filter(claim_paid <= 0)
#View(zero_claim_paid)
#UNSW_earned_data %>% filter(exposure_id == "exposure_11489811-f986-4ef2-9090-46db3f423860")

## check for duplicated claim because of owner double claim / other owner related issues
## many dimension ## ignore for now
# UNSW_claim_check = UNSW_claims_data %>%
#   select(exposure_id, total_claim_amount)


##look at the duplicated row - delete later
#duplicated_claim_row = UNSW_claims_data[duplicated(UNSW_claims_data),]
#View(UNSW_claims_data %>% filter (claim_id %in% duplicated_claim_row$claim_id))





##### checking - delete later
# UNSW_claims_data %>% filter(claim_id %in% duplicated_claim_row$claim_id)
# #Investigating duplicated claim id
# n=table(UNSW_claims_data$claim_id)
# nrow(n[n!=1])
# 
# NumberOfRowsPerClaim =
#   UNSW_claims_data %>%
#   group_by(claim_id) %>%
#   summarise(NumberOfAppearances = n()) %>%
#   filter(NumberOfAppearances > 1)
# VectorOfRepeatedClaims = NumberOfRowsPerClaim$claim_id
# length(VectorOfRepeatedClaims)
# 
# UNSW_claims_data_ONLY_Repeated_claim_ids = UNSW_claims_data %>%
#   filter(claim_id %in% VectorOfRepeatedClaims)
# 
# UNSW_claims_data %>% group_by(claim_status) %>% summarise(n = n(), Total_claim_paid = sum(claim_paid))
# 
# ## this doesn't do anything...
# Claims_Data_NRC = #Earned Data with No Repeated Claim IDs
#   UNSW_claims_data %>%
#   group_by(claim_id)
# 
# #Investigating duplicate claims
# 
# n=table(UNSW_claims_data$claim_id)
# nrow(n[n!=1])
# 
# NumberOfRowsPerClaim =
#   UNSW_claims_data %>%
#   group_by(claim_id) %>%
#   summarise(NumberOfAppearances = n()) %>%
#   filter(NumberOfAppearances > 1)
# VectorOfRepeatedClaims = NumberOfRowsPerClaim$claim_id
# length(VectorOfRepeatedClaims)
# 
# UNSW_claims_data_ONLY_Repeated_claim_ids = UNSW_claims_data %>%
#   filter(claim_id %in% VectorOfRepeatedClaims)
# 
# Claims_Data_NRC = #Earned Data with No Repeated Claim IDs
#   UNSW_claims_data %>%
#   group_by(claim_id)
# 
# Claim_Volume_Vs_Exposure_ID = #Table of Exposure Ids with their respective claim volumes
#   UNSW_claims_data %>%
#   group_by(exposure_id)%>%
#   summarise(ClaimVolume = n())

##Investigating each variable

# par(mfrow = c(2, 2))

######################### delete above later


#UNSW Earned Data

##Cleaning the data
#remove unwanted columns
UNSW_earned_data = UNSW_earned_data %>% dplyr::select(-X, -row_num)

#change data type
UNSW_earned_data$UW_Date= month(as.Date(UNSW_earned_data$UW_Date), label = TRUE, abbr = TRUE)
UNSW_earned_data$pet_gender = as.factor(UNSW_earned_data$pet_gender)
UNSW_earned_data$pet_de_sexed = as.factor(UNSW_earned_data$pet_de_sexed)
UNSW_earned_data$pet_de_sexed_age=as.factor(UNSW_earned_data$pet_de_sexed_age)
UNSW_earned_data$pet_is_switcher= as.factor(UNSW_earned_data$pet_is_switcher)
UNSW_earned_data$nb_policy_first_inception_date = as.Date(UNSW_earned_data$nb_policy_first_inception_date)
UNSW_earned_data$nb_address_type_adj = as.factor(UNSW_earned_data$nb_address_type_adj)

UNSW_earned_data$nb_suburb = as.factor(UNSW_earned_data$nb_suburb)
UNSW_earned_data$nb_postcode = as.factor(UNSW_earned_data$nb_postcode)
UNSW_earned_data$nb_state = as.factor(UNSW_earned_data$nb_state)

UNSW_earned_data$person_dob = as.Date(UNSW_earned_data$person_dob)
UNSW_earned_data$nb_contribution_excess = as.factor(UNSW_earned_data$nb_contribution_excess)
UNSW_earned_data$pet_age_years = as.factor(UNSW_earned_data$pet_age_years)

UNSW_earned_data$nb_breed_type = as.factor(UNSW_earned_data$nb_breed_type)
UNSW_earned_data$nb_breed_trait = as.factor(UNSW_earned_data$nb_breed_trait)
UNSW_earned_data$nb_breed_name_unique = as.factor(UNSW_earned_data$nb_breed_name_unique)
UNSW_earned_data$nb_breed_name_unique_concat = as.factor(UNSW_earned_data$nb_breed_name_unique_concat)
UNSW_earned_data$is_multi_pet_plan = as.factor(UNSW_earned_data$is_multi_pet_plan)

#UNSW_earned_data$lead_date_day = as.Date(UNSW_earned_data$lead_date_day, origin = "1970-01-01")
UNSW_earned_data$quote_date= as.Date(UNSW_earned_data$quote_date)
UNSW_earned_data$quote_time_group= as.factor(UNSW_earned_data$quote_time_group)
#######

## checking negative tenure
# negative_tenure = UNSW_earned_data %>% filter(tenure < 0)
# View(UNSW_earned_data %>% filter(exposure_id %in% negative_tenure$exposure_id) %>%
#        select(UW_Date, exposure_id, tenure, nb_policy_first_inception_date, earned_units))

## will just remove negative tenure
UNSW_earned_data = UNSW_earned_data %>%
  filter(UNSW_earned_data$tenure >= 0)


## Remove 0 earned units 
UNSW_earned_data = UNSW_earned_data %>%
  filter(UNSW_earned_data$earned_units > 0.0001)
# View(UNSW_earned_data %>% filter(earned_units <= 0.03) %>%
#        select(UW_Date, exposure_id, tenure, nb_policy_first_inception_date, quote_date, earned_units))


## check for duplicate
sum(duplicated(UNSW_earned_data))

### check for num of unique exposure id 
length(unique(UNSW_earned_data$exposure_id))
length(unique(UNSW_earned_data$exposure_id_1))
sum(UNSW_earned_data$exposure_id == UNSW_earned_data$exposure_id_1)

##==============================================================
## Combining claim and earned data

### manipulate earned data
## calculate exposure units / earned units for each exposure id
exposure <- UNSW_earned_data %>%
  group_by(exposure_id) %>%
  summarise(Total_Earned = sum(earned_units))

## make exposure id collapse into one row only
#Ensuring that each row in UNSW_earned_data contains a unique exposure_id
collapsed_UNSW_earned_data = #Earned Data with No Repeated Exposure IDs
  UNSW_earned_data %>%
  group_by(exposure_id) %>%
  filter(tenure == max(tenure)) %>%
  dplyr::select(-earned_units)

collapsed_UNSW_earned_data <- left_join(collapsed_UNSW_earned_data, exposure, by = "exposure_id")


### manipulate claim data
##############
## aggregate claim by claim_id (will regard claims with same claim_id as 
## claims from the same root cause (so will regard those claims as only 1 claim))
## then
### aggregate claim by exposure id
claim_per_exposure_id = UNSW_claims_data %>%
  group_by(exposure_id) %>%
  summarise(claim_nb = length(unique(claim_id)), Total_claim_amount = sum(total_claim_amount),
            Total_claim_paid = sum(claim_paid))
# claim_per_exposure_id_old = UNSW_claims_data %>%
#   group_by(exposure_id) %>%
#   summarise(claim_nb = n(), Total_claim_amount = sum(total_claim_amount),
#             Total_claim_paid = sum(claim_paid))
#sum(claim_per_exposure_id_old$claim_nb)


sum(claim_per_exposure_id$claim_nb)



### Aggregate UNSW_earned_data and claim data (claim nb, claim paid and total amount)
combined_data = left_join(collapsed_UNSW_earned_data, claim_per_exposure_id, by = c("exposure_id"))

combined_data = combined_data %>%
  mutate(claim_nb = ifelse(is.na(claim_nb)==TRUE, 0, claim_nb))
combined_data = combined_data %>%
  mutate(Total_claim_amount = ifelse(is.na(Total_claim_amount)==TRUE, 0, Total_claim_amount),
         Total_claim_paid = ifelse(is.na(Total_claim_paid)==TRUE, 0, Total_claim_paid))

min(combined_data$Total_Earned) #= 0.3125 - no more 0 Total_Earned data
combined_data = combined_data %>%
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

#==========================================================================

## checking claim vs month and claim vs tenure -- not important rn, will do later
claim_nb_per_month  = UNSW_claims_data %>% 
  mutate(UW_Date = month(claim_start_date, label = TRUE)) %>%
  group_by(UW_Date) %>%
  summarise(claim_nb_per_month = n())

claim_nb_per_tenure = UNSW_claims_data %>% 
  group_by(tenure) %>%
  summarise(claim_nb_per_tenure = n())

total_active_policy_per_tenure = UNSW_earned_data %>% 
  group_by(tenure) %>%
  summarise(n = n())

total_active_policy_per_month = UNSW_earned_data %>% 
  group_by(UW_Date) %>%
  summarise(n = n())

left_join(claim_nb_per_month, total_active_policy_per_month, by = c("UW_Date")) %>%
  mutate(claim_per_active_policy = claim_nb_per_month/n)

left_join(claim_nb_per_tenure, total_active_policy_per_tenure, by = c("tenure")) %>%
  mutate(claim_per_tenure = claim_nb_per_tenure/n)
## what can we get from above analysis?

# =================================================================================
# Merge SA2 code onto it (needs to be fixed bcs same postcode has many SA2)
sa2_mapping <- read_excel("data/sa2_mapping.xlsx")
sa2_mapping$nb_postcode <- as.factor(sa2_mapping$nb_postcode)

names(combined_data)
combined_data_sa2 = left_join(combined_data, sa2_mapping, "SA2_CODE", by = c("nb_postcode"))


### IGNORE FOR NOW
## row 1 and 5409 has many-to-many relationship

combined_data_sa2[1,c("nb_postcode", "nb_suburb", "SA2_CODE", "nb_state")]
combined_data_sa2[5409,c("nb_postcode", "nb_suburb", "SA2_CODE", "nb_state")]

## manually resolve many-to-many relationship between nb_postcode 
## (resolve with manually checking suburb name)

# for Row 1, suburb name = Sunshine West, so SA_2 code = 213011338
combined_data_sa2[1,c("SA2_CODE")] <- 213011338

# for Row 5409, suburb name = Hamilton (in QLD), so SA_2 code = 305031124
combined_data_sa2[5409,c("SA2_CODE")] <- 305031124

