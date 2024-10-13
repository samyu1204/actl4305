library(dplyr)
library(ggplot2)
library(lubridate)


UNSW_claims_data_raw = read.csv("UNSW_claims_data.csv", header=TRUE )
UNSW_claims_data = UNSW_claims_data_raw


UNSW_earned_data_raw = read.csv("UNSW_earned_data_adjusted_Sep27.csv", header=TRUE)
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


  q=table(UNSW_claims_data$exposure_id)
  q=q[q>5]
  hist(q)
  max(q)
  
  Claim_Volume_Vs_Exposure_ID = #Table of Exposure Ids with their respective claim volumes
    UNSW_claims_data %>%
      group_by(exposure_id)%>%
      summarise(ClaimVolume = n())
  
##Investigating each variable

  summary(UNSW_claims_data)
  par(mfrow = c(2, 2))
  #Investigating claim_status
  ggplot(UNSW_claims_data, aes(x=claim_status)) +
      geom_bar(fill="blue", color="black")
  #Investigating Claims Cost
  
  ggplot(UNSW_claims_data, aes(x = claim_paid)) + 
    geom_histogram(binwidth = 50, fill = "blue", color = "black")
  #Investigating condition vs claims cost, removing claim_paid=0 as there are 3500 rows where this is the case
  p1 <- ggplot(data = filter(UNSW_claims_data,UNSW_claims_data$claim_paid != 0), mapping = aes(x = reorder(condition_category, -claim_paid, FUN = mean), y = claim_paid)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = c(0, 1000)) +
    labs(x = "Condition Category (ordered by mean claim paid)", y = "Claim Paid") +
    theme_minimal()
  p1
  
  #UNSW Earned Data
  
  ##Cleaning the data     
    summary(UNSW_earned_data)
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
    UNSW_earned_data$lead_date_day = as.Date(UNSW_earned_data$lead_date_day)
    UNSW_earned_data$quote_date= as.Date(UNSW_earned_data$quote_date)
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
View(UNSW_earned_data %>% filter(exposure_id %in% zero_earned_units$exposure_id))

## if zero total earned units then also zero claim number

###
claim_nb = UNSW_claims_data 

View(UNSW_claims_data %>% filter(claim_paid == 0))
UNSW_claims_data %>% filter(claim_paid == 0) %>%
  ggplot(data = .) +
  geom_bar(mapping=(aes(claim_status)))

ggplot(data = UNSW_claims_data) +
  geom_bar(mapping=(aes(claim_status)))

#Removing negative tenures and 0 claim_paid for covered already paid
## keep not paid cover for now
UNSW_claims_data = 
  UNSW_claims_data %>%
  filter(claim_paid>0 | claim_status %in% c("covered_not_paid", "covered_with_exclusions_not_paid"))


## check for duplicated claim because of owner double claim / other owner related issues
## many dimension ## ignore for now
UNSW_claim_check = UNSW_claims_data %>%
  select(exposure_id, total_claim_amount)

sum(duplicated(UNSW_claim_check))
duplicated(UNSW_claim_check)

possible_duplicated_claim = UNSW_claims_data[duplicated(UNSW_claim_check),]

View(UNSW_claims_data %>% filter(exposure_id %in% possible_duplicated_claim$total_claim_amount))


##
claim_per_exposure_id = UNSW_claims_data %>%
  group_by(exposure_id) %>%
  summarise(claim_nb = n(), Total_claim_amount = sum(total_claim_amount),
            Total_claim_paid = sum(claim_paid), )

length(unique(UNSW_claims_data$exposure_id))

### Aggregate UNSW_earned_data and claim data (claim nb, claim paid and total amount)

UNSW_earned_data$claim_nb = 0
combined_data = left_join(collapsed_UNSW_earned_data, claim_per_exposure_id, by = c("exposure_id"))

combined_data = combined_data %>%
  mutate(claim_nb = ifelse(is.na(claim_nb)==TRUE, 0, claim_nb))


combined_data = combined_data %>%
  mutate(Total_claim_amount = ifelse(is.na(Total_claim_amount)==TRUE, 0, Total_claim_amount),
         Total_claim_paid = ifelse(is.na(Total_claim_paid)==TRUE, 0, Total_claim_paid))

## check Total_Earned = 0 equal to claim_nb == 0 & Total_Earned == 0
nrow(combined_data %>% filter(claim_nb == 0 & Total_Earned == 0))
nrow(combined_data %>% filter(Total_Earned == 0))


combined_data = combined_data %>%
  filter(Total_Earned > 0) %>%
  mutate(claim_freq = claim_nb/Total_Earned)



##########

# EDA of Age vs Claim Rate / Claim Freq
par(mfrow = c(1,1))
ggplot(data = combined_data) +
  aes(x = pet_age_months, y = claim_freq) +
  geom_point(col = "blue", alpha = 0.5)

plot(combined_data$pet_age_months, combined_data$claim_freq)

###
claim_by_age <- combined_data %>%
  group_by(pet_age_months) %>%
  summarise(n = n(), exposure = sum(Total_Earned), 
            claim_nb = sum(claim_nb), total_claim_amt = sum(Total_claim_amount),
            median_claim_amt = quantile(Total_claim_amount, probs = 0.5),
            claim_paid = sum(Total_claim_paid))

claim_by_age <- claim_by_age %>%
  mutate(claim_freq = claim_nb/exposure)
View(claim_by_age)

theme_set(theme_bw())
theme_update(plot.title=element_text(hjust=0.5, size = 12, face = 'bold'))

age_vs_claim_freq = ggplot(data = claim_by_age) +
  aes(x = pet_age_months, y = claim_freq) +
  geom_point(alpha = 0.6, aes(size = n), col = 'blue') +
  ylim(c(0,0.35)) +
  labs(y = "Claim Frequency", x = "Pet Age (in months)", title = "Claim Frequency by Pet Age") +
  geom_smooth()
age_vs_claim_freq + geom_smooth()

library(plotly)
ggplotly(age_vs_claim_freq)

####

age_2_mth_data = combined_data %>% filter(pet_age_months == 2)
hist(na.omit(age_2_mth_data$Total_claim_amount/age_2_mth_data$claim_nb))

### find the avg severity per claim by age
claim_by_age %>% filter(claim_nb > 0) %>%
  mutate(avg_paid_per_claim = claim_paid/claim_nb) %>%
ggplot(data = .) +
  aes(x = pet_age_months, y = avg_paid_per_claim) +
  geom_col(col = "blue") +
  labs(x = "Pet Age (in months)", y = "Avg Claim Paid per claim",
       title = "Claim Paid by Pet Age") 

claim_by_age %>% filter(claim_nb > 0) %>%
  mutate(avg_paid_per_claim = claim_paid/claim_nb) %>%
  ggplot(data = .) +
  aes(x = pet_age_months, y = avg_paid_per_claim) +
  geom_col(aes(fill = n), col = "black") +
  labs(x = "Pet Age (in months)", y = "Avg Claim Paid per claim",
       title = "Claim Paid by Pet Age") +
  scale_fill_gradient(low = "white", high = "purple")

claim_by_age %>% filter(claim_nb > 0) %>%
  mutate(avg_sev_per_claim = total_claim_amt/claim_nb) %>%
  ggplot(data = .) +
  aes(x = pet_age_months, y = avg_sev_per_claim) +
  geom_point(aes(colour = n))

avg_vs_age <- claim_by_age %>% filter(claim_nb > 0) %>%
  mutate(avg_sev_per_claim = total_claim_amt/claim_nb) %>%
  ggplot(data = .) +
  aes(x = pet_age_months, y = avg_sev_per_claim) +
  geom_col(aes(fill = n)) +
  labs(x = "Pet Age (in months)", y = "Avg Claim Amount (Severity) per claim",
       title = "Claim Severity by Pet Age") 

claim_by_age %>% filter(claim_nb > 0) %>%
  mutate(avg_sev_per_claim = total_claim_amt/claim_nb) %>%
  ggplot(data = .) +
  aes(x = pet_age_months, y = avg_sev_per_claim) +
  geom_col(fill = "blue") +
  labs(x = "Pet Age (in months)", y = "Avg Claim Amount\nper claim",
       title = "Avg Claim Amount (Severity) by Pet Age")

ggplot(data = claim_by_age) +
  aes(x = pet_age_months, y = claim_freq) +
  geom_point(alpha = 0.6, aes(size = n), col = 'blue') +
  ylim(c(0,0.35)) +
  labs(y = "Claim Frequency", x = "Pet Age (in months)", title = "Claim Frequency by Pet Age")
age_vs_claim_freq

### used
claim_by_age %>% filter(claim_nb > 0) %>%
  mutate(avg_sev_per_claim = total_claim_amt/claim_nb) %>%
  ggplot(data = .) +
  aes(x = pet_age_months, y = avg_sev_per_claim) +
  geom_col(fill = "blue", alpha = 0.8) +
  labs(x = "Pet Age (in months)", y = "Avg Claim Amount\nper claim",
       title = "Avg Claim Amount (Severity) by Pet Age")

claim_by_age %>% filter(claim_nb > 0) %>%
  mutate(avg_paid_per_claim = claim_paid/claim_nb) %>%
  ggplot(data = .) +
  aes(x = pet_age_months, y = avg_paid_per_claim) +
  geom_col(fill = "blue", alpha = 0.8) +
  labs(x = "Pet Age (in months)", y = "Avg Claim Paid\nper claim",
       title = "Claim Paid by Pet Age")


library(forcats)
combined_data %>% group_by(nb_breed_type) %>%
  summarise(Total_claim_paid = sum(Total_claim_paid),
            Total_Exposure = sum(Total_Earned)) %>%
  mutate(premium = Total_claim_paid/Total_Exposure) %>%
ggplot(data = ., mapping = aes(x = fct_rev(fct_reorder(nb_breed_type, premium)), y = premium)) +
  geom_col() +
  geom_text(aes(label = round(premium,1)), vjust = -0.5) +
  ylim(c(0,40)) +
  labs(x = "Breed Type", y = "Avg Claim Sum per Exposure Unit") +
  ggtitle("Average Claim Sum per Exposure Unit by Breed Type")


#####
hist(combined_data$claim_)
 