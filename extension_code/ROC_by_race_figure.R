library(ggplot2)
library(tidyverse)
library(kableExtra)

# Will eventually want to change this to just read from the R data object containing the regression data

setwd("/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/extension_code")
source("utils.R")

data_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/"
model_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))
raw_physical_act <- read_csv(paste(data_path, "physical_activity.csv", sep=""))
raw_medical_cond <- read_csv(paste(data_path, "medical_conditions.csv", sep=""))
raw_mental_health <- read_csv(paste(data_path, "mental_health.csv", sep=""))
raw_weight_hist <- read_csv(paste(data_path, "weight_history.csv", sep=""))
raw_health_ins <- read_csv(paste(data_path, "health_insurance.csv", sep=""))
raw_food_insec <- read_csv(paste(data_path, "food_insecurity.csv", sep=""))


social_determinants_model <- readRDS(paste(model_path, "social_determinants_model.rda", sep = ""))
biological_determinants_model <- readRDS(paste(model_path, "biological_determinants_model.rda", sep = ""))
race_only_model <- readRDS(paste(model_path, "race_only_model.rda", sep = ""))
full_paper_model <- readRDS(paste(model_path, "final_paper_model.rda", sep = ""))
age_and_bmi_model <- readRDS(paste(model_path, "age_and_bmi_model.rda", sep = ""))

regression_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  inner_join(raw_health_ins, by = c("seqn")) %>%
  inner_join(raw_food_insec, by = c("seqn")) %>%
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

# social_determinants_model_aucs <- compute_auc_by_race(social_determinants_model, regression_data)
# biological_determinants_model_aucs <- compute_auc_by_race(biological_determinants_model, regression_data)
# race_only_model_aucs <- compute_auc_by_race(race_only_model, regression_data)
# full_paper_model_aucs <- compute_auc_by_race(full_paper_model, regression_data)
age_and_bmi_rocs <- make_roc_by_race_data(age_and_bmi_model, regression_data) %>%
  mutate(model = "Age and BMI model")
biological_determinants_model_rocs <- make_roc_by_race_data(biological_determinants_model, regression_data) %>%
  mutate(model = "Biological determinants model")
social_determinants_rocs <- make_roc_by_race_data(social_determinants_model, regression_data) %>%
  mutate(model = "Social determinants model")
full_paper_model_rocs <- make_roc_by_race_data(full_paper_model, regression_data) %>%
  mutate(model = "Aggarwal et al. model")
race_only_rocs <- make_roc_by_race_data(race_only_model, regression_data) %>%
  mutate(model = "Race only model")

rbind(age_and_bmi_rocs,
      social_determinants_rocs,
      biological_determinants_model_rocs,
      full_paper_model_rocs,
      race_only_rocs) %>%
  ggplot() +
  geom_abline(slope = 1, linetype = "dashed", ) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "White American"), aes(x = FPR, y = TPR, color = "White American")) +
  geom_line(data = social_determinants_rocs %>%
              filter(race == "White American"), aes(x = FPR, y = TPR, color = "White American")) +
  geom_line(data = biological_determinants_model_rocs %>%
              filter(race == "White American"), aes(x = FPR, y = TPR, color = "White American")) +
  geom_line(data = full_paper_model_rocs %>%
              filter(race == "White American"), aes(x = FPR, y = TPR, color = "White American")) +
  geom_line(data = race_only_rocs %>%
              filter(race == "White American"), aes(x = FPR, y = TPR, color = "White American")) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "Black American"), aes(x = FPR, y = TPR, color = "Black American")) +
  geom_line(data = social_determinants_rocs %>%
              filter(race == "Black American"), aes(x = FPR, y = TPR, color = "Black American")) +
  geom_line(data = biological_determinants_model_rocs %>%
              filter(race == "Black American"), aes(x = FPR, y = TPR, color = "Black American")) +
  geom_line(data = full_paper_model_rocs %>%
              filter(race == "Black American"), aes(x = FPR, y = TPR, color = "Black American")) +
  geom_line(data = race_only_rocs %>%
              filter(race == "Black American"), aes(x = FPR, y = TPR, color = "Black American")) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "Asian American"), aes(x = FPR, y = TPR, color = "Asian American")) +
  geom_line(data = social_determinants_rocs %>%
              filter(race == "Asian American"), aes(x = FPR, y = TPR, color = "Asian American")) +
  geom_line(data = biological_determinants_model_rocs %>%
              filter(race == "Asian American"), aes(x = FPR, y = TPR, color = "Asian American")) +
  geom_line(data = full_paper_model_rocs %>%
              filter(race == "Asian American"), aes(x = FPR, y = TPR, color = "Asian American")) +
  geom_line(data = race_only_rocs %>%
              filter(race == "Asian American"), aes(x = FPR, y = TPR, color = "Asian American")) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "Hispanic American"), aes(x = FPR, y = TPR, color = "Hispanic American")) +
  geom_line(data = social_determinants_rocs %>%
              filter(race == "Hispanic American"), aes(x = FPR, y = TPR, color = "Hispanic American")) +
  geom_line(data = biological_determinants_model_rocs %>%
              filter(race == "Hispanic American"), aes(x = FPR, y = TPR, color = "Hispanic American")) +
  geom_line(data = full_paper_model_rocs %>%
              filter(race == "Hispanic American"), aes(x = FPR, y = TPR, color = "Hispanic American")) +
  geom_line(data = race_only_rocs %>%
              filter(race == "Hispanic American"), aes(x = FPR, y = TPR, color = "Hispanic American")) +
  facet_wrap(.~model) +
  scale_color_discrete(name="") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.25))

  

ggplot() +
  geom_abline(slope = 1, linetype = "dashed", ) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "White American"), aes(x = FPR, y = TPR, color = "White American")) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "Asian American"), aes(x = FPR, y = TPR, color = "Asian American")) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "Black American"), aes(x = FPR, y = TPR, color = "Black American")) +
  geom_line(data = age_and_bmi_rocs %>%
              filter(race == "Hispanic American"), aes(x = FPR, y = TPR, color = "Hispanic American")) +
  scale_color_discrete(name="") +
  theme_minimal()





