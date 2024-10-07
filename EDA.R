library(dplyr)
library(ggplot2)
library(lubridate)

setwd("/Users/Alex/Documents/2022/UNSW/ACTL4305/Assignment")

UNSW_claims_data = read.csv("UNSW_claims_data.csv", header=TRUE )

UNSW_earned_data = read.csv("UNSW_earned_data_adjusted_Sep27.csv", header=TRUE)


#UNSW claims data

##Cleaning the data

UNSW_claims_data$claim_start_date = as.Date(UNSW_claims_data$claim_start_date)
UNSW_claims_data$claim_status = as.factor(UNSW_claims_data$claim_status)
UNSW_claims_data$condition_category = as.factor(UNSW_claims_data$condition_category)

#Removing negative tenures and 0 claim_paid
UNSW_claims_data = 
  UNSW_claims_data %>%
  filter(tenure >= 0, claim_paid>0)
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

  UNSW_claims_data$claim_start_date = as.Date(UNSW_claims_data$claim_start_date)
  UNSW_claims_data$claim_status = as.factor(UNSW_claims_data$claim_status)
  UNSW_claims_data$condition_category = as.factor(UNSW_claims_data$condition_category)
  
  #Removing negative tenures and 0 claim_paid
  UNSW_claims_data = 
    UNSW_claims_data %>%
      filter(tenure >= 0, claim_paid>0)
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
  
  UNSW_claims_data = #Removing Duplicate Rows in UNSW Claims Data
    UNSW_claims_data %>%
      distinct()
  
  Claims_Data_Grouped_By_Claim_ID = #Earned Data with Grouped by Claim ID
    UNSW_claims_data %>%
    group_by(claim_id) 

UNSW_claims_data$claim_start_date = as.Date(UNSW_claims_data$claim_start_date)
UNSW_claims_data$claim_status = as.factor(UNSW_claims_data$claim_status)
UNSW_claims_data$condition_category = as.factor(UNSW_claims_data$condition_category)


#Removing negative tenures and 0 claim_paid
UNSW_claims_data = 
  UNSW_claims_data %>%
  filter(tenure >= 0, claim_paid>0)
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
      
  
  ###Multiple exposure ids appear in both datasets
    table(UNSW_earned_data$exposure_id)
    table(UNSW_claims_data$exposure_id)
   
   #Ensuring that each row in UNSW_earned_data contains a unique exposure_id
    Earned_Data_NRE = #Earned Data with No Repeated Exposure IDs
      UNSW_earned_data %>%
        group_by(exposure_id) %>%
        filter(tenure == max(tenure)) %>%
        ungroup()
    
    t= table(Earned_Data_NRE$exposure_id)
    t[t==1]
    t[t!=1]
    
    NumberOfRowsPerExposure =
      UNSW_earned_data %>%
        group_by(exposure_id) %>%
        summarise(NumberOfAppearances = n()) %>%
        filter(NumberOfAppearances > 1)
    VectorOfRepeatedExposures = NumberOfRowsPerExposure$exposure_id
    VectorOfRepeatedExposures
        
  Claims_With_Earned <- left_join(UNSW_claims_data, Earned_Data_NRE, by = "exposure_id")
  table(Claims_With_Earned$tenure.y>Claims_With_Earned$tenure.x)
  m=table(Claims_With_Earned$claim_id)
  m[m!=1]
  

  ##Investigating Variables
   #Plotting Average Total Claims Cost Per Exposure by Pet Age split by dog breed / condition type
    Sum_of_Total_Claims_by_Exposure  = 
      Claims_With_Earned %>%
        group_by(exposure_id) %>%
        summarise(SumOfClaims = sum(claim_paid), NumberofClaims = n()) %>%
        ungroup()
    b=table(Sum_of_Total_Claims_by_Exposure$exposure_id)
    b[b==1]
    b[b>1]
    UNSW_earned_data_with_ClaimsSum = left_join(UNSW_earned_data, Sum_of_Total_Claims_by_Exposure, by = "exposure_id")
    Exposures_with_Aggregated_Claims_Data = #Dataset which has claim sum, total exposure units and average claim super per exposure unit - each calculated by exposure id
      left_join(
        Earned_Data_NRE,      
        UNSW_earned_data_with_ClaimsSum %>%
          group_by(exposure_id) %>%
          summarise(ClaimSum=mean(SumOfClaims), TotalExposureUnits = sum(earned_units), Volume = mean(NumberofClaims)) %>%
          mutate(AverageClaimSumPerExposureUnit = ClaimSum/TotalExposureUnits)
      )
    
    Aggregated_Claim_Data_by_Breed_Type_and_Pet_Age =
      Exposures_with_Aggregated_Claims_Data %>%
        group_by(nb_breed_type, pet_age_months) %>%
        summarise(AverageClaimSumPerExposureUnit = sum(ClaimSum, na.rm=TRUE)/sum(TotalExposureUnits, na.rm=TRUE))
    
    ggplot(Aggregated_Claim_Data_by_Breed_Type_and_Pet_Age, aes(x = pet_age_months,y=AverageClaimSumPerExposureUnit, color=nb_breed_type)) + geom_line()
    
    Aggregated_Claim_Data_by_Pet_Age =
      Exposures_with_Aggregated_Claims_Data %>%
      group_by(pet_age_months) %>%
      summarise(AverageClaimSumPerExposureUnit = sum(ClaimSum, na.rm=TRUE)/sum(TotalExposureUnits, na.rm=TRUE))

    ggplot(Aggregated_Claim_Data_by_Pet_Age, aes(x = pet_age_months,y=AverageClaimSumPerExposureUnit)) +
      geom_line()
    
    plot(Claims_With_Earned$nb_breed_type, Claims_With_Earned$claim_paid)
    
    ggplot(Exposures_with_Aggregated_Claims_Data, aes(x = AverageClaimSumPerExposureUnit, color = nb_breed_type)) +
      geom_histogram(binwidth = 500, position = "dodge") +  # Adjust bin width as needed
      labs(title = "Histogram of Average Claim Per Exposure Unit by Breed Type",
           x = "Claim Sum",
           y = "Average") +
      scale_fill_manual(values = c("Beagle" = "blue", "Labrador" = "green", "Poodle" = "red")) +  # Optional custom colors
      theme_minimal()  
    
    ggplot(Exposures_with_Aggregated_Claims_Data, mapping = aes(x = reorder(nb_breed_type, -AverageClaimSumPerExposureUnit , FUN = mean), y = AverageClaimSumPerExposureUnit)) +
      geom_boxplot() + #outlier.shape = NA)
      scale_y_continuous(limits = c(0, 600)) +
      labs(x = "Breed Type (ordered by mean Average Claim Sum Per Exposure Unit)", y = "Average Claim Sum Per Exposure Unit") +
      theme_minimal()
    
    Aggregated_Claim_Data_by_Breed_Type =
      Exposures_with_Aggregated_Claims_Data %>%
      group_by(nb_breed_type) %>%
      summarise(AverageClaimSumPerExposureUnit = sum(ClaimSum, na.rm=TRUE)/sum(TotalExposureUnits, na.rm=TRUE), SumOfClaims = sum(ClaimSum, na.rm=TRUE), NumberofPets = n(), TotalExposure =sum(TotalExposureUnits), ClaimVolume= sum(Volume, na.rm = TRUE)) %>%
      mutate(ClaimRate = ClaimVolume/TotalExposure)
    
    #ggplot(Aggregated_Claim_Data_by_Breed_Type, aes(x = ,y=AverageClaimSumPerExposureUnit)) +
     # geom_line()
  
=======
>>>>>>> Stashed changes
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
  labs(x = "Condition Category (ordered by median hwy)", y = "Claim Paid") +
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


###Multiple exposure ids appear in both datasets
table(UNSW_earned_data$exposure_id)
table(UNSW_claims_data$exposure_id)

#Ensuring that each row in UNSW_earned_data contains a unique exposure_id
Earned_Data_NRE = #Earned Data with No Repeated Exposure IDs
  UNSW_earned_data %>%
  group_by(exposure_id) %>%
  filter(tenure == max(tenure)) %>%
  ungroup()

t= table(Earned_Data_NRE$exposure_id)
t[t==1]
t[t!=1]

NumberOfRowsPerExposure =
  UNSW_earned_data %>%
  group_by(exposure_id) %>%
  summarise(NumberOfAppearances = n()) %>%
  filter(NumberOfAppearances > 1)
VectorOfRepeatedExposures = earned_data_repeated_exposures_only$exposure_id
VectorOfRepeatedExposures

Claims_With_Earned <- left_join(UNSW_claims_data, Earned_Data_NRE, by = "exposure_id")
table(Claims_With_Earned$tenure.y>Claims_With_Earned$tenure.x)
m=table(Claims_With_Earned$claim_id)
m[m!=1]
##Investigating Variables
#Plotting Claims costs by Pet Age split by dog breed / condition type


<<<<<<< Updated upstream
=======
>>>>>>> c052304c18c36e9478bbc146fae7d8682e1d24cf
>>>>>>> Stashed changes
