library(dplyr)
library(ggplot2)
library(lubridate)
library(MASS)
library(tidyverse)
library(glmnet)
library(randomForest)
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

<<<<<<< HEAD
#top 25% of claims by breed 
  Claim.Frequency <- Claims_With_Earned %>%
    group_by(exposure_id) %>%
    summarise(claim_frequency = n(), average_total_claim_amount = mean(total_claim_amount), average_claim_paid = mean(claim_paid))
  
  mutated_Claims_With_Earned <- Claims_With_Earned %>%
    left_join(Claim.Frequency, by = "exposure_id")
  
  
  #Number of levels for factor variables
  ommitted.na.claims_with_earned <- na.omit(Claims_With_Earned)
  
  categorical_columns <- ommitted.na.claims_with_earned %>% 
    select_if(is.factor)
  
  sapply(categorical_columns, function(x) length(unique(x)))
  
  
  #top 25% of claims by breed 
  Claims.Summary <- summary(mutated_Claims_With_Earned$average_total_claim_amount)
  Q3.average.total.claims <- 557.4
  
  top25claimsbybreed <- mutated_Claims_With_Earned %>%
    filter(average_total_claim_amount >= Q3.average.total.claims) %>%
    group_by(nb_breed_name_unique) %>%
    summarise(expected.total.claim.amount = mean(total_claim_amount))
  
  top15breeds <- top25claimsbybreed %>%
    arrange(desc(expected.total.claim.amount)) %>%
    slice_head(n = 15)
  
  top15breeds.bar <- ggplot(top15breeds, aes(x = reorder(nb_breed_name_unique, expected.total.claim.amount), 
                                             y = expected.total.claim.amount)) +
    geom_bar(stat = "identity", fill = "steelblue") +  
    coord_flip() +  
    labs(title = " Top 25% of Total Average Claims by Breed",
         x = "Breed",
         y = "Average Total Claims") +
    theme_minimal() +  
    theme(plot.title = element_text(hjust = 0.5))
  
  
  #Random Forest Model of Average total claims
  
  to.remove.claim.amount <- c("nb_postcode", "nb_breed_name_unique", 
                              "UW_Date", "nb_breed_name_unique_concat", 
                              "quote_time_group","claim_id", "total_claim_amount", 
                              "average_claim_paid", "claim_frequency", "tenure.x", 
                              "tenure.y", "claim_status", "earned_units", "exposure_id_1", 
                              "exposure_id", "nb_policy_first_inception_date", "quote_date", 
                              "claim_start_date", "lead_date_day", "claim_paid")
  
  claims.train <- mutated_Claims_With_Earned[,!(colnames(mutated_Claims_With_Earned) %in% to.remove.claim.amount)]
  claims.train <- na.omit(claims.train)
  
  claims.rf <- randomForest(average_total_claim_amount ~., data = claims.train, importance = TRUE)
  
  average.total.claim.importance <- varImpPlot(claims.rf) #nb_breed_trait, pet_age_months, condition_category, nb_suburb
  
  #Correlation Matrix of Features with Claim Size
  
  install.packages("corrplot") #for the visualization
  library("corrplot")
  
  par(mfrow = c(1,1))
  
  corr.matrix <- mutated_Claims_With_Earned %>%
    select_if(is.numeric) %>%
    cor() 
  
  inf.claims <- c("average_total_claim_amount", "claim_frequency", "average_claim_paid")
  
  corr.subset <- corr.matrix[inf.claims, ]
  
  corr.matrix.visualisation <- corrplot(corr.subset, method = "color", type = "upper",
                                        t1.col = "black", tl.srt = 45,
                                        addCoef.col = "black") 
  #Most correlated to average claim paid per exposure: pet_age_months, nb_contribution, nb_average_breed size
  
  
  Claims_With_Earned$nb_suburb
  
  
  bysuburb <- mutated_Claims_With_Earned %>%
    group_by(nb_suburb) %>%
    summarise(average.total.claims = mean(total_claim_amount), number_of_claims = n()) %>%
    filter(number_of_claims >= 3)
  
  bysuburb <- bysuburb %>%
    arrange(desc(average.total.claims)) %>%
    slice_head(n = 10)
  
  
  bysuburb.bar <- ggplot(bysuburb, aes(x = reorder(nb_suburb, average.total.claims), 
                                       y = average.total.claims)) +
    geom_bar(stat = "identity", fill = "darkkhaki") +  
    coord_flip() +  
    labs(title = "Average Total Claims by Suburb",
         x = "Suburb",
         y = "Expected Claim Amount") +
    theme_minimal() +  
    theme(plot.title = element_text(hjust = 0.5))
  
  
  number.of.claims.by.suburb <- ggplot(bysuburb, aes(x = nb_suburb, y = number_of_claims)) +
    geom_bar(stat = "identity", fill = "darkcyan") + 
    coord_flip() +  
    labs(title = "Number of Claims by Suburb",
         x = "Suburb",
         y = "Number of Claims") +
    theme_minimal() +  
    theme(plot.title = element_text(hjust = 0.5))
  
  par(mfrow = c(2,2))
  number.of.claims.by.suburb
  bysuburb.bartheme(plot.title = element_text(hjust = 0.5))

par(mfrow = c(2,2))
number.of.claims.by.suburb
bysuburb.bar


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
>>>>>>> 35f5348bab95a0b3bd90739943b2c10b303dc86a


#####

proportions.data <- read.csv("guidlinesbyage.csv", stringsAsFactors = TRUE)

proportion_data <- proportions.data %>%
  group_by(Age.Group) %>%
  mutate(expected.proportion.to.meet.guidlines = mean(Proportion)/100)

 
Claims_With_Earned

simulated.physical.guidlines <- function(owner_age_years) {
  # Check for NA values first
  if (is.na(owner_age_years)) {
    return("NA")
  }
  
  if (owner_age_years >= 15 && owner_age_years <= 17) {
    result <- ifelse(runif(1) <= 0.075, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  if (owner_age_years >= 18 && owner_age_years <= 24) {
    result <- ifelse(runif(1) <= 0.339, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  if (owner_age_years >= 25 && owner_age_years <= 34) {
    result <- ifelse(runif(1) <= 0.270, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  if (owner_age_years >= 35 && owner_age_years <= 44) {
    result <- ifelse(runif(1) <= 0.239, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  if (owner_age_years >= 45 && owner_age_years <= 54) {
    result <- ifelse(runif(1) <= 0.206, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  if (owner_age_years >= 55 && owner_age_years <= 64) {
    result <- ifelse(runif(1) <= 0.188, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  if (owner_age_years >= 65 && owner_age_years <= 74) {
    result <- ifelse(runif(1) <= 0.459, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  if (owner_age_years >= 75) {
    result <- ifelse(runif(1) <= 0.368, "Met Physical Guidelines", "Did Not Meet Physical Guidelines") 
    return(result)
  }
  return("NA")  # Return for ages not covered by the conditions
}


simulation <- Claims_With_Earned %>%
  mutate(Guidelines_Status = sapply(owner_age_years, simulated.physical.guidlines)) %>%
  group_by(exposure_id, Guidelines_Status) %>%
  summarise(mean_claim_paid = mean(claim_paid, na.rm = TRUE)) %>%
  filter(Guidelines_Status != "NA")

view(simulation)

t.test(mean_claim_paid ~ Guidelines_Status, data = simulation) #Not enough evidence to suggest the mean claim paid to exposures by Physical Guideline Status

simulation.numeric.guidlines <- ifelse(simulation$Guidelines_Status == "Met Physical Guidelines", 1, 0)
cor(simulation$mean_claim_paid, simulation.numeric.guidlines) #Almost no Correlation between physical guidline status and mean claim paid per exposure
  
ggplot(simulation, aes(x = Guidelines_Status, y = mean_claim_paid)) +
  geom_boxplot() +
  labs(title = "Mean Claim Paid by Guidelines Status", x = "Guidelines Status", y = "Mean Claim Paid")

# Bar Plot
ggplot(simulation, aes(x = Guidelines_Status, y = mean_claim_paid, fill = Guidelines_Status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Claim Paid by Guidelines Status", x = "Guidelines Status", y = "Average Claim Paid")



####Creative Pricing factor 2

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



