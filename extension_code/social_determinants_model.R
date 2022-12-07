library(dplyr)
library(pROC) # for computing AUC

setwd("/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/extension_code")
source("utils.R")

data_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/"
save_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/"
roc_save_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))
raw_health_ins <- read_csv(paste(data_path, "health_insurance.csv", sep=""))
raw_food_insec <- read_csv(paste(data_path, "food_insecurity.csv", sep=""))


regression_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  inner_join(raw_health_ins, by = c("seqn")) %>%
  inner_join(raw_food_insec, by = c("seqn")) %>%
  filter(ridageyr >= 18) %>% # Taken from Supplement
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  filter(bmxbmi >= 18.5, # Taken from Supplement
         bmxbmi <= 50) %>%
  rename(race = ridreth3) %>%
  mutate(# Making the race variable more readable
    race = case_when(race == 1 | race == 2 ~ "Hispanic American",
                     race == 3 ~ "White American",
                     race == 4 ~ "Black American",
                     race == 6 ~ "Asian American"),
    race = factor(race),
    # Re-leveling the race factor, so that White is base level (as in paper)
    race = relevel(race, ref = "White American")) %>%
  # Recoding the education codes to interpretable values
  mutate(dmdeduc3_adj = case_when(dmdeduc3 %in% c(0,1,2,3,4,5,6,7,8,55,66) ~ "Less than 9th grade",
                                  dmdeduc3 %in% c(9,10,11,12) ~ "9-11th grade (Includes 12th grade with no diploma)",
                                  dmdeduc3 %in% c(13,14) ~ "High school graduate/GED or equivalent",
                                  dmdeduc3 == 15 ~ "More than high school",
                                  dmdeduc3 %in% c(66,77,".") ~ as.character(NA))
         ) %>%
  mutate(dmdeduc2_adj = case_when(dmdeduc2 == 1 ~ "Less than 9th grade",
                                  dmdeduc2 == 2 ~ "9-11th grade (Includes 12th grade with no diploma)",
                                  dmdeduc2 == 3 ~ "High school graduate/GED or equivalent",
                                  dmdeduc2 == 4 ~ "Some college or AA degree",
                                  dmdeduc2 == 5 ~ "College graduate or above",
                                  dmdeduc2 %in% c(7,8,".") ~ as.character(NA))
         ) %>%
  # Combining the education covariate for both age groups
  mutate(educ = case_when(is.na(dmdeduc3_adj) ~ dmdeduc2_adj,
                          is.na(dmdeduc2_adj) ~ dmdeduc3_adj)) %>%
  # Recoding the income codes to interpretable values
  mutate(income = case_when(indhhin2 == 1 ~ "$0 to $4,999",
                            indhhin2 == 2 ~ "$5,000 to $9,999",
                            indhhin2 == 3 ~ "$10,000 to $14,999",
                            indhhin2 == 4 ~ "$15,000 to $19,999",
                            indhhin2 == 5 ~ "$20,000 to $24,999",
                            indhhin2 == 6 ~ "$25,000 to $34,999",
                            indhhin2 == 7 ~ "$35,000 to $44,999",
                            indhhin2 == 8 ~ "$45,000 to $54,999",
                            indhhin2 == 9 ~ "$55,000 to $64,999",
                            indhhin2 == 10 ~ "$65,000 to $74,999",
                            indhhin2 == 12 ~ "$20,000 and Over",
                            indhhin2 == 13 ~ "Under $20,000",
                            indhhin2 == 14 ~ "$75,000 to $99,999",
                            indhhin2 == 15 ~ "$100,000 and Over",
                            indhhin2 %in% c(77,99,".") ~ as.character(NA))
         ) %>%
  # Recoding the health insurance codes to interpretable values
  mutate(health_insurance = case_when(hiq011 == 1 ~ "Yes",
                                      hiq011 == 2 ~ "No",
                                      hiq011 %in% c(7, 9, ".") ~ as.character(NA))
         ) %>%
  # Recoding the food security codes to interpretable values
  mutate(food_security = case_when(fsd032a == 1 ~ "Often worried",
                                      fsd032a == 2 ~ "Sometimes worried",
                                      fsd032a == 3 ~ "Never worried",
                                      fsd032a %in% c(7, 9, ".") ~ as.character(NA))
  ) %>%
  # Computing new weight for 4 survey cycles
  mutate(wtmec8yr = wtmec2yr/4) %>%
  # Making diabetes labels as described in the paper and replication code
  mutate(lbxgh = as.numeric(as.character((lbxgh))),
         diq010 = as.numeric(as.character((diq010))),
         a1c = cut(lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE),
         diabetes_diagnosis = case_when(diq010 %in% 1 ~ 1, 
                                        diq010 %in% c(2,3,9) ~ 0,
                                        diq010 %in% 7 ~ as.numeric(NA)),
         diabetes = diabetes_diagnosis,
         diabetes = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), 1, diabetes),
         diabetes = as.integer(diabetes),
         diabetes = if_else(diabetes == 1, TRUE, FALSE)
  ) %>%
  mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))

# Social characteristics -----
# INDHHIN2	Total household income (reported as a range value in dollars) (in demo)
# HIQ011	{Are you/Is SP} covered by health insurance or some other kind of health care plan? (in hiq)
# CBD765	Which of the following best describe your highest education level? (?)
# use DMDEDUC3 and DMDEDUC2 instead (in demo)
#   
# Food insecurity -----
# FSD032A	In the last 12 months, {I/we} worried whether {my/our} food would run out before {I/we} got money to buy more. (in fiq)

social_determinants_model_formula <- diabetes ~ ridageyr + bmxbmi + income + health_insurance + educ + food_security

social_determinants_model <- glm(social_determinants_model_formula, 
                         data = regression_data, 
                         family = "binomial",
                         weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

summary(social_determinants_model)  

# Save the model
saveRDS(social_determinants_model, file = paste(save_path, "social_determinants_model.rda", sep = ""))

# Model evaluation
predictions <- round(predict(social_determinants_model, newdata = regression_data, type = "response") * 100, 2)

compute_auc(age_and_bmi_model, regression_data)
compute_auc_by_race(age_and_bmi_model, regression_data)

# ROCR 
data_for_roc <-
  regression_data %>%
  mutate(predictions = predictions) %>%
  filter(!is.na(predictions)) # Need this step to drop NA predictions from the ROC

roc_data <- make_roc_data(data_for_roc$diabetes, data_for_roc$predictions)

write_csv(roc_data, paste(roc_save_path, "social_determinants_roc.csv", sep = ""))
