library(MASS) #for glm.nb and subset selection
library(statmod) #for qq random residuals
#library(pscl) #for zeroinfl model
library(countreg) #for zeroinfl or hurdle model
library(topmodels) #for visualisation of countreg models



#### Fitting a GLM model on your data
### Frequency modelling

mean(combined_data$claim_freq)
var(combined_data$claim_freq)

#checking claim_freq = 6.2 = 1/0.1612
# View(UNSW_earned_data %>% filter(exposure_id == "exposure_9b34b8d8-3a2b-4f89-9560-7e4911bf05fb"))
# View(UNSW_claims_data %>% filter(exposure_id == "exposure_9b34b8d8-3a2b-4f89-9560-7e4911bf05fb"))
# View(UNSW_earned_data_raw %>% filter(exposure_id == "exposure_9b34b8d8-3a2b-4f89-9560-7e4911bf05fb"))

#after checking decided to drop that row only for frequency modelling (drop row with highest claim freq (i.e., 6.2))
combined_data <- combined_data %>% filter(exposure_id != "exposure_9b34b8d8-3a2b-4f89-9560-7e4911bf05fb")

colnames(combined_data)[colSums(is.na(combined_data)) > 0]
## Preparing dataset
combined_data_full <- combined_data

combined_data <- combined_data_full
## removing variable that 100% cannot be used for modelling frequency and sev from combined_data
### 1. remove columns not predictor
not_predictor_cols <- c("UW_Date", "exposure_id", "tenure",
                        "nb_policy_first_inception_date", "person_dob", 
                        "lead_date_day", "quote_date",
                        "exposure_id_1")
## tenure and UW_Date could possibly be predictor, but given how the data is combined,
## the predictor effect is likely gone now

### 2. remove redundant columns (maybe also owner_age_years since we use age_bucket?)
redundant_cols <- c("pet_gender", "owners_age", "owner_age_years")
# redundant bcs there are other vars that represent the same stuff

### 3. remove factor vars in numerical format (not for glm)
factor_in_num_cols <- c("nb_address_type_adj_numerical",
                        "nb_state_num",
                        "nb_breed_type_num",
                        "nb_breed_trait_num",
                        "is_multi_pet_plan_num",
                        "quote_time_group_num")

### 4. remove high-dimensional variables (suburb, postcode)
## amount of data not enough to get info on those??
high_dim_cols <- c("nb_suburb", "nb_postcode", "nb_breed_name_unique_concat")
  
## total features removed
length(c(not_predictor_cols, redundant_cols, factor_in_num_cols, high_dim_cols))
  
combined_data_full %>% group_by(nb_suburb) %>% summarise(n = n()) %>%
  ggplot(data = .) +
  aes(y = n) +
  geom_bar() + coord_flip()


combined_data <- combined_data[, !names(combined_data) %in% c(not_predictor_cols, redundant_cols, 
                                                              factor_in_num_cols, high_dim_cols)]
names(combined_data_full)[(names(combined_data_full) %in% c(not_predictor_cols, redundant_cols, 
                                                            factor_in_num_cols, high_dim_cols))]

###### Ways to select feature for model building

## 1. EDA

## 2. Build Model - look at coef

## 3. Do lasso regression

## 4. forward / backward selection (stepAIC) / best subset selection


###### Possible Performance Measure of Models?
# 1. AIC / BIC
# 2. MSE / RMSE
# 3. the "score" (profitability score specified in assignment brief)


### other possible extra features
## 1. income, unemployment, education? => hard to combine coz one postcode has
## many SA2_code


### Data splitting
set.seed(2024)

#split 70 30
train_indices <- sample(1:nrow(combined_data), size = 0.7 * nrow(combined_data))
train_data <- combined_data[train_indices,]
test_data <- combined_data[-train_indices,]

sampling_indices <- sample(1:nrow(combined_data), size = 0.1 * nrow(combined_data))




## Modelling Frequency

# Use all features
not_features <- c("claim_nb", "Total_claim_amount", 
                  "Total_claim_paid", "claim_freq", 
                  "severity", "Total_Earned")
features_only_vec <- names(combined_data)[!names(combined_data) %in% not_features]
formula_full <- as.formula(paste("claim_nb", "~", paste(features_only_vec, collapse = " + "), sep = ""))
formula_full






# Join the features to form a formula with underscores between feature names
#formula <- as.formula(paste("severity", "~", paste(features, collapse = " + "), sep = ""))

########## Modelling

##### Fit the GLM model for Frequency
## possible collinear features
# 1. pet_de_sexed, pet_de_sexed_age, pet_gender_de_sexed, pet_is_male
# 2. pet_age_months, pet_age_years
# 3. nb_breed_type, nb_breed_trait, nb_breed_name_unique


## 1. Regular Poisson model = not good according to residuals plot

## use almost all features in features only vec (including the collinear features)
## but remove nb_breed_name_unique
glm_poisson_freq_full_features <- glm(claim_nb ~ pet_de_sexed + pet_de_sexed_age + pet_is_switcher + 
                                pet_age_months + nb_contribution + nb_excess + nb_address_type_adj + 
                                nb_state + nb_contribution_excess + pet_age_years + 
                                nb_average_breed_size + nb_breed_type + nb_breed_trait + 
                                is_multi_pet_plan + quote_time_group + 
                                pet_is_male + age_breed_interaction + age_bucket + pet_gender_de_sexed + 
                                log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction + 
                                contribution_excess_interaction,
                              data = combined_data, subset = train_indices,
                              offset = log(Total_Earned), 
                              family = poisson(link = "log"))

summary(glm_poisson_freq_full_features)
alias(glm_poisson_freq_full_features)

## from the features_only_vec, choose variables such that there is not colinearity
glm_poisson_freq_no_collinear_features <- glm(claim_nb ~  pet_is_switcher +
                                nb_address_type_adj + 
                                nb_state + nb_contribution_excess + pet_age_years + nb_number_of_breeds + 
                                nb_average_breed_size + nb_breed_trait + 
                                is_multi_pet_plan + quote_time_group + 
                                age_breed_interaction + age_bucket + pet_gender_de_sexed +
                                log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction,
                              data = combined_data, subset = train_indices,
                              offset = log(Total_Earned), 
                              family = poisson(link = "log"))
alias(glm_poisson_freq_no_collinear_features)
summary(glm_poisson_freq_no_collinear_features) #AIC = 9035.1
anova(glm_poisson_freq_no_collinear_features)

## from that I get a collinearity problem on nb_breed_trait
## problem with "unknown", "unnamed", ""?
## problem with "white fluffy"

## if nb_breed_type used instead of nb_breed_trait AIC = 9050.6
## from EDA, found out that nb_number_of_breeds might not be accurate (e.g., labradoodle regarded as 1 instead of 2)
glm_poisson_freq_selected_features <- glm(claim_nb ~  pet_is_switcher + 
                                nb_address_type_adj + 
                                nb_contribution_excess + pet_age_years + 
                                nb_average_breed_size + nb_breed_trait +
                                is_multi_pet_plan + 
                                age_breed_interaction + age_bucket + pet_gender_de_sexed + 
                                log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction,
                              data = combined_data, subset = train_indices,
                              offset = log(Total_Earned), 
                              family = poisson(link = "log"))

summary(glm_poisson_freq_selected_features)
AIC(glm_poisson_freq_selected_features)
BIC(glm_poisson_freq_selected_features)
anova(glm_poisson_freq_selected_features, test = "Chisq")
## nb_breed_trait better than nb_breed_type (at least according to AIC)

plot(glm_poisson_freq_selected_features)


#QQ plot of Randomized quantile residulas
qqnorm(qresiduals(glm_poisson_freq_selected_features), ylim = c(-10,10))
qqline(qresiduals(glm_poisson_freq_selected_features), col = "red")
qqrplot(glm_poisson_freq_selected_features, main = "Poisson Model")



poisson_test_pred <- predict(glm_poisson_freq_selected_features, newdata = test_data, type = "response")

unique(train_data$age_bucket)
unique(test_data$age_bucket)




### subset selection
glm_poisson_none <- glm(claim_nb ~  1,
                        data = combined_data, subset = train_indices,
                        offset = log(Total_Earned), 
                        family = poisson(link = "log"))


#AIC Backward
poisson_aic_back <- stepAIC(glm_poisson_freq_selected_features,
                            direction = "backward",
                            k = 2,
                            scope = list(upper = glm_poisson_freq_selected_features, lower = glm_poisson_none))

summary(poisson_aic_back)
#AIC Forward


#BIC Backward


#BIC Forward


### regular poisson not good? probably not good fit

## 2. Quasi poisson -> ad fit according to plots
glm_quasipoisson_freq_selected_features <- glm(claim_nb ~  pet_is_switcher + 
                                            nb_address_type_adj + 
                                            nb_contribution_excess + pet_age_years + 
                                            nb_average_breed_size + nb_breed_trait  +
                                            is_multi_pet_plan + 
                                            age_breed_interaction + age_bucket + pet_gender_de_sexed + 
                                            log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction,
                                          data = combined_data, subset = train_indices,
                                          offset = log(Total_Earned), 
                                          family = quasipoisson(link = "log"))

summary(glm_quasipoisson_freq_selected_features)
anova(glm_quasipoisson_freq_selected_features, test = "Chisq")
plot(glm_quasipoisson_freq_selected_features)


qqnorm(qresiduals(glm_quasipoisson_freq_selected_features))
qqline(qresiduals(glm_quasipoisson_freq_selected_features), col = "red")

qqrplot(glm_quasipoisson_freq_selected_features, main = "Quassi Poisson")

####


## 3. Negative Binomial
glm_freq_negbin <- glm.nb(claim_nb ~  offset(log(Total_Earned)) + pet_is_switcher + 
                           nb_address_type_adj + 
                           nb_contribution_excess + pet_age_years + 
                           nb_average_breed_size + nb_breed_trait  +
                           is_multi_pet_plan + 
                           age_bucket + pet_gender_de_sexed + 
                           age_breed_bin + 
                           log(owner_pet_age_interaction),
                       data = combined_data, subset = train_indices, link = log)

summary(glm_freq_negbin) #7591 #7580
AIC(glm_freq_negbin)
BIC(glm_freq_negbin) #7925
anova(glm_freq_negbin)

plot(glm_freq_negbin)

ggplot(combined_data) +
  aes(y = claim_freq, x = log(owner_pet_age_interaction)) +
  geom_point(alpha = 0.5, colour = "blue")

## QQ residuals the only useful plot for count response
qqnorm(qresiduals(glm_freq_negbin))
qqline(qresiduals(glm_freq_negbin), col = "red")

qqrplot(glm_freq_negbin, main = "Neg Bin")
title(main = "NegBin")
rootogram(glm_freq_negbin, main = "NegBin", max = 50)

summary(qresiduals(glm_freq_negbin))

## 4. zero inflated poisson


simple_zeroinfl_poisson_freq <- zeroinfl(formula = claim_nb ~ pet_is_switcher + 
                                           nb_address_type_adj + 
                                           nb_contribution_excess + pet_age_years + 
                                           nb_average_breed_size + nb_breed_trait  +
                                           is_multi_pet_plan + 
                                           age_breed_interaction + age_bucket + pet_gender_de_sexed + 
                                           log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction | 1,
                                  dist = "poisson", offset = log(Total_Earned),
                                  data = combined_data, subset = train_indices)

summary(simple_zeroinfl_poisson_freq)
AIC(simple_zeroinfl_poisson_freq) #7946
BIC(simple_zeroinfl_poisson_freq) #8304
anova(simple_zeroinfl_poisson_freq)

rootogram(glm_poisson_freq_selected_features, main = "Simple ZIP", max = 50, ylim = c(-5,80))

qqrplot(simple_zeroinfl_poisson_freq, main = "Simple ZIP")

## don't put anything on link argument if want no error
zeroinfl_poisson_freq <- zeroinfl(formula = claim_nb ~ pet_is_switcher +
                                    nb_address_type_adj + 
                                    nb_contribution_excess + pet_age_years + 
                                    nb_average_breed_size + nb_breed_trait  +
                                    is_multi_pet_plan + 
                                    age_breed_interaction + age_bucket + pet_gender_de_sexed + 
                                    log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction,
                                  dist = "poisson", offset = log(Total_Earned),
                                  data = combined_data, subset = train_indices)



summary(zeroinfl_poisson_freq)
AIC(zeroinfl_poisson_freq)
BIC(zeroinfl_poisson_freq)

qqrplot(zeroinfl_poisson_freq)


### ZINB
simple_zeroinfl_negbin_freq <- zeroinfl(formula = claim_nb ~ pet_is_switcher + 
                                           nb_address_type_adj + 
                                           nb_contribution_excess + pet_age_years + 
                                           nb_average_breed_size + nb_breed_trait  +
                                           is_multi_pet_plan + 
                                           age_breed_interaction + age_bucket + pet_gender_de_sexed + 
                                           log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction | 1,
                                         dist = "negbin", offset = log(Total_Earned),
                                         data = combined_data, subset = train_indices)

summary(simple_zeroinfl_negbin_freq)
AIC(simple_zeroinfl_negbin_freq)
BIC(simple_zeroinfl_negbin_freq)

qqrplot(simple_zeroinfl_negbin_freq)


zeroinfl_negbin_freq <- zeroinfl(formula = claim_nb ~ pet_is_switcher + 
                                          nb_address_type_adj + 
                                          nb_contribution_excess + pet_age_years + 
                                          nb_average_breed_size + nb_breed_trait  +
                                          is_multi_pet_plan + 
                                          age_bucket + pet_gender_de_sexed + 
                                          age_breed_bin + log(owner_pet_age_interaction) | pet_age_years,
                                        dist = "negbin", offset = log(Total_Earned),
                                        data = combined_data, subset = train_indices)
#https://stackoverflow.com/questions/43075911/examining-residuals-and-visualizing-zero-inflated-poission-r
#big coeff value for binom part of zero-infl probably means ZINB not suitable model
summary(zeroinfl_negbin_freq)
AIC(zeroinfl_negbin_freq)
BIC(zeroinfl_negbin_freq)

qqrplot(simple_zeroinfl_negbin_freq)



### Hurdle model
simple_hnb <- hurdle(claim_nb ~ pet_is_switcher + 
                       nb_address_type_adj + 
                       nb_contribution_excess + pet_age_years + 
                       nb_average_breed_size + nb_breed_trait  +
                       is_multi_pet_plan + 
                       age_bucket + pet_gender_de_sexed + 
                       age_breed_bin + 
                       log(owner_pet_age_interaction) | 1,
                     dist = "negbin", offset = log(Total_Earned),
                     data = combined_data, subset = train_indices)

summary(simple_hnb)
AIC(simple_hnb)
BIC(simple_hnb)

qqrplot(simple_hnb)
rootogram(simple_hnb, breaks = 50)

hnb <- hurdle(claim_nb ~ pet_is_switcher + 
                nb_address_type_adj + 
                nb_contribution_excess + pet_age_years + 
                nb_average_breed_size + nb_breed_trait  +
                is_multi_pet_plan + 
                age_bucket + pet_gender_de_sexed + 
                age_breed_bin + 
                log(owner_pet_age_interaction),
              dist = "negbin", offset = log(Total_Earned),
              data = combined_data, subset = train_indices)
summary(hnb)
AIC(hnb)
BIC(hnb)

qqrplot(hnb, main = "HNB")
rootogram(hnb)


######## Investigating features related to breed (to get more understanding and 
## feature selection)
## investigating nb_breed_trait
View(
  combined_data %>% group_by(nb_breed_trait) %>%
  summarise(n = n(),
            total_claim_nb = sum(claim_nb),
            total_exposure = sum(Total_Earned),
            avg_claim_amount = mean(Total_claim_amount),
            avg_claim_paid = mean(Total_claim_paid)) %>%
  mutate(claim_freq = total_claim_nb/total_exposure)
)

combined_data %>% filter(nb_breed_trait %in% c("cross", "", "unknown")) %>%
ggplot(data = .) +
  aes(x = nb_breed_trait, y = claim_freq) +
  geom_violin() + ylim(0,0.15)

ggplot(data = combined_data) %>%
  aes(x = nb_breed_type, y = claim_freq) +
  geom_violin()





###### investigating nb_breed_trait and other breed related var
View(combined_data_full %>% select(-exposure_id) %>% filter(nb_breed_trait == "") %>% count(nb_breed_name_unique))
View(combined_data %>% filter(nb_breed_type == "unnamed cross"))

View(combined_data %>% filter(nb_breed_type == "cross"))

View(combined_data_full %>% filter(nb_breed_type == "designerbreed"))
View(sum((combined_data_full %>% filter(nb_breed_trait == "water dog"))$nb_breed_name_unique == "spoodle"))

View(combined_data %>% count(nb_breed_type, nb_breed_trait))
View(combined_data %>% count(nb_breed_name_unique, nb_number_of_breeds))

View(combined_data  %>% count(pet_age_years))






##### Tree models 
## to find important variables
library(rattle)

fitcontrol <- trainControl(method = "cv",
                           number = 5)

### Decision Tree
set.seed(1)
tree <- train(claim_freq ~  pet_is_switcher + 
                pet_age_months + nb_address_type_adj + 
                nb_state + pet_age_years + 
                nb_average_breed_size + nb_breed_type + nb_breed_trait + 
                is_multi_pet_plan + quote_time_group + 
                age_breed_interaction + age_bucket + pet_gender_de_sexed + 
                log_age_breed_interaction + age_breed_bin + owner_pet_age_interaction,
              data = train_data,
              method = "rpart",
              metric = "RMSE",
              trControl = fitcontrol,
              tuneLength = 10)
print(tree)


plot(tree)


fancyRpartPlot(tree$finalModel, sub="", palettes ="RdPu")

tree$pred

tree_pred_train = predict(tree, newdata = train_data)
tree_rmse_train = sqrt(mean((train_data$claim_freq - tree_pred_train)^2))



tree_pred_test = predict(tree, newdata = test_data)
tree_rmse_test = sqrt(mean((test_data$claim_freq - tree_pred_test)^2))





## findings: 
# 1. nb_breed_trait all "" are cross in nb_breed_type
# 2. cannot use nb_number_of_breeds because some cross breed has one unique name (ie.e labradoodle)
# 3. the breed related vars is probably not double checked - e.g., waterdog is mostly cross but nb unique only one type (i.e., labradoodle instead of labrador and doodle)
# 4. only nb_breed_type and nb_breed_trait could be reliable
# 5. all water dogs are poodle (or cross with poodle) dogs

