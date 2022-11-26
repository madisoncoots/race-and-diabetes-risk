library(dplyr)

data_path <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"
# model_path <- "/home/mcoots/harvard/research/race-in-healthcare/models/final_paper_model.rda"
model_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/final_paper_model.rda"

# Read in regression data -------------------------------------------------------------------

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))

regression_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
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

# Predict using the model -------------------------------------------------------------------

model <- readRDS(model_path)

predictions <- round(predict(model, newdata = regression_data, type = "response") * 100, 2)

# Change me 
threshold <- 1.4

regression_data %>%
  select(seqn, race, diabetes, ridageyr, bmxbmi) %>%
  mutate(predicted_risk = predictions,
         screening_decision = predicted_risk >= threshold) %>%
  filter(!is.na(screening_decision)) %>% # some rows were missing race
  count(diabetes, screening_decision)

# Implementing the race-based thresholds on BMI
race_based_screening_decisions <- 
  regression_data %>%
  filter(ridageyr >= 35) %>% 
  select(seqn, race, diabetes, ridageyr, bmxbmi) %>%
  mutate(screen = case_when(
    race == "Hispanic American" & bmxbmi >= 18.5 ~ TRUE,
    race == "Black American" & bmxbmi >= 18.5 ~ TRUE,
    race == "Asian American" & bmxbmi >= 20 ~ TRUE,
    race == "White American" & bmxbmi >= 25 ~ TRUE,
    TRUE ~ FALSE)
    )

# Model evaluation
auc(regression_data$diabetes, predictions)

# Race-based screening rule evaluation
auc(race_based_screening_decisions$diabetes, race_based_screening_decisions$screen)
