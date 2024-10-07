library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
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
  
  Claims_Data_Grouped_By_Claim_ID = #Earned Data with Grouped by Claim ID
    UNSW_claims_data %>%
    group_by(claim_id) 

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
    VectorOfRepeatedExposures = NumberOfRowsPerExposure$exposure_id
    VectorOfRepeatedExposures
        
  Claims_With_Earned <- left_join(UNSW_claims_data, Earned_Data_NRE, by = "exposure_id")
  table(Claims_With_Earned$tenure.y>Claims_With_Earned$tenure.x)
  m=table(Claims_With_Earned$claim_id)
  m[m!=1]
  ##Investigating Variables
   #Plotting Claims costs by Pet Age split by dog breed / condition type
  

  



#random Forrest model to assess variable importance

ommitted.na.claims_with_earned <- na.omit(Claims_With_Earned)

categorical_columns <- ommitted.na.claims_with_earned %>% 
  select_if(is.factor)

sapply(categorical_columns, function(x) length(unique(x)))

to.remove <- c("nb_postcode", "nb_breed_name_unique", "nb_breed_name_unique_concat", "claim_paid")


rf.training <- ommitted.na.claims_with_earned[,!(colnames(ommitted.na.claims_with_earned) %in% to.remove)]

total.claims.rf <- randomForest(total_claim_amount ~., data = rf.training, importance = TRUE)

claim.size.importance <- varImpPlot(total.claims.rf) 


#top 25% of claims by breed 
Claims.Summary <- summary(ommitted.na.claims_with_earned$total_claim_amount)
Q3.Total.Claims <- 447.525

top25claimsbybreed <- ommitted.na.claims_with_earned %>%
  filter(total_claim_amount >= Q3.Total.Claims) %>%
  group_by(nb_breed_name_unique) %>%
  summarise(percentage.frequency = 100*n()/nrow(ommitted.na.claims_with_earned))

top10breeds <- top25claimsbybreed %>%
  arrange(desc(percentage.frequency)) %>%
  slice_head(n = 10)

top10breeds.bar <- ggplot(top10breeds, aes(x = reorder(nb_breed_name_unique, percentage.frequency), 
                        y = percentage.frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +  
  coord_flip() +  
  labs(title = "Top 10 Breeds by Percentage Frequency of Claims for the Top 25% of Claims",
       x = "Breed",
       y = "Percentage Frequency (%)") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))



#Claims Frequency 

Claim.Frequency <- Claims_With_Earned %>%
  group_by(exposure_id) %>%
  summarise(claim_frequency = n(), average_total_claim_amount = mean(total_claim_amount), average_claim_paid = mean(claim_paid))

mutated_Claims_With_Earned <- Claims_With_Earned %>%
  left_join(Claim.Frequency, by = "exposure_id")


to.remove.2 <-c("nb_postcode", "nb_breed_name_unique", "nb_breed_name_unique_concat", "claim_paid", "total_claim_amount")

freq.train <- Claims_With_Earned_with_frequency[,!(colnames(Claims_With_Earned_with_frequency) %in% to.remove.2)]

freq.train <- na.omit(freq.train)

frequency.rf <- randomForest(claim_frequency ~., data = freq.train, importance = TRUE)

frequency.importance <- varImpPlot(frequency.rf)

#Correlation Matrix of Features with Claim Size

install.packages("corrplot") #for the visualisation
library("corrplot")

par(mfrow = c(1,1))

corr.matrix <- mutated_Claims_With_Earned %>%
  select_if(is.numeric) %>%
  cor() 

inf.claims <- c("average_total_claim_amount", "claim_frequency", "average_claim_paid")

corr.subset <- corr.matrix[inf.claims, ]

corrplot(corr.subset, method = "color", type = "upper",
         t1.col = "black", tl.srt = 45,
         addCoef.col = "black") 




