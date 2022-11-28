library(dplyr)
library(pROC) # for computing AUC
library(MASS) # for stepwise regression

data_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/"
save_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/"
roc_save_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))
raw_physical_act <- read_csv(paste(data_path, "physical_activity.csv", sep=""))
raw_medical_cond <- read_csv(paste(data_path, "medical_conditions.csv", sep=""))
raw_mental_health <- read_csv(paste(data_path, "mental_health.csv", sep=""))
raw_weight_hist <- read_csv(paste(data_path, "weight_history.csv", sep=""))

regression_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  inner_join(raw_physical_act, by = c("seqn")) %>%
  inner_join(raw_medical_cond, by = c("seqn")) %>%
  inner_join(raw_mental_health, by = c("seqn")) %>%
  inner_join(raw_weight_hist, by = c("seqn")) %>%
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
  # Recoding the gender covariate
  mutate(gender = case_when(riagendr == 2 ~ "Woman",
                            riagendr == 1 ~ "Man",
                            riagendr == "." ~ as.character(NA))) %>%
  # Recoding the increased diabetes risk covariate
  mutate(increased_diabetes_risk = case_when(diq170 == 1 ~ "Yes",
                            diq170 == 2 ~ "No",
                            diq170 %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the blood relatives had diabetes covariate
  mutate(relatives_had_diabetes = case_when(mcq300c == 1 ~ "Yes",
                                            mcq300c == 2 ~ "No",
                                            mcq300c %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the felt depressed covariate
  mutate(felt_depressed = case_when(dpq020 == 0 ~ "Not at all",
                                    dpq020 == 1 ~ "Several days",
                                    dpq020 == 2 ~ "More than half the days",
                                    dpq020 == 3 ~ "Nearly every day",
                                    dpq020 %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the feels like they're at risk for diabetes or prediabetes covariate
  mutate(feels_at_risk_diabetes = case_when(diq172 == 1 ~ "Yes",
                                            diq172 == 2 ~ "No",
                                            diq172 %in% c(7, 9, ".") ~ as.character(NA))) %>%
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

regression_data_no_na <- # need to drop NA's for the stepwise regression function
  regression_data %>%
  filter(!is.na(diabetes),
         !is.na(ridageyr),
         !is.na(bmxbmi),
         !is.na(gender),
         !is.na(pad660),
         !is.na(pad615),
         !is.na(whd140),
         !is.na(bmxwt),
         !is.na(bmxht),
         !is.na(bmxwaist),
         !is.na(increased_diabetes_risk),
         !is.na(relatives_had_diabetes),
         !is.na(felt_depressed),
         !is.na(feels_at_risk_diabetes))

biological_determinants_formula <- diabetes ~ ridageyr + bmxbmi + gender +
  pad660 + pad615 + whd140 + bmxwt + bmxht + bmxwaist + 
  increased_diabetes_risk + relatives_had_diabetes + felt_depressed + feels_at_risk_diabetes

biological_determinants_model <- glm(biological_determinants_formula, 
                         data = regression_data_no_na, 
                         family = "binomial",
                         weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

summary(biological_determinants_model)

saveRDS(biological_determinants_model, file = paste(save_path, "biological_determinants_model.rda", sep = ""))

# Biological characteristics -----
# RIAGENDR gender (in demo)
# PAD660 - Minutes vigorous recreational activities (in paq)
# PAD615 - Minutes vigorous-intensity work (in paq)
# DIQ170	ever been told by a doctor or other health professional that you're at increased risk for diabetes? (in survey data)
# MCQ300c	blood relatives ever told by a health professional that they had diabetes? (in MCQ)
# DPQ020	Over the last 2 weeks, how often have you been bothered by feeling down, depressed, or hopeless? (in DPQ)
# DIQ172	{Do you/Does SP} feel {you/he/she} could be at risk for diabetes or prediabetes? (in survey data)
# WHD140	Up to the present time, what is the most {you have/SP has} ever weighed? (in whq)
# BMXWT	Weight (kg) (in bmx)
# BMXHT	Standing Height (cm) (in bmx)
# BMXWAIST	Waist Circumference (cm) (in bmxw)



# Model evaluation
predictions <- round(predict(biological_determinants_model, newdata = regression_data_no_na, type = "response") * 100, 2)
auc(regression_data_no_na$diabetes, predictions)

# ROCR 
data_for_roc <-
  regression_data_no_na %>%
  mutate(predictions = predictions) %>%
  filter(!is.na(predictions)) # Need this step to drop NA predictions from the ROC

# NOTE: We should move this to a util file eventually
make_roc_data <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), scores[order(scores, decreasing=TRUE)], labels)
}

roc_data <- make_roc_data(data_for_roc$diabetes, data_for_roc$predictions)

write_csv(roc_data, paste(roc_save_path, "biological_determinants_model_roc.csv", sep = ""))

# ------------------- Stepwise version! -----------------------------
step_biological_determinants_model <- stepAIC(biological_determinants_model, 
                                              trace = FALSE)

summary(step_biological_determinants_model)

# Model evaluation
predictions <- round(predict(step_biological_determinants_model, newdata = regression_data_no_na, type = "response") * 100, 2)
auc(regression_data_no_na$diabetes, predictions)

# ROCR 
data_for_roc <-
  regression_data_no_na %>%
  mutate(predictions = predictions) %>%
  filter(!is.na(predictions)) # Need this step to drop NA predictions from the ROC

roc_data <- make_roc_data(data_for_roc$diabetes, data_for_roc$predictions)

write_csv(roc_data, paste(roc_save_path, "stepwise_biological_determinants_model_roc.csv", sep = ""))

