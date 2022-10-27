library(dplyr)

data_path <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))

raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))

raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))

raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))

# Ignoring weights for the first stab at the regression replication
cleaned_demographics_data <- raw_demographics_data %>%
  select(seqn,
         ridageyr,
         ridreth3)

cleaned_survey_responses_data <- raw_survey_responses %>%
  select(seqn,
         diq010)

cleaned_body_measurements <- raw_body_measurements %>%
  select(seqn,
         bmxbmi)

cleaned_glycohemoglobin <- raw_glycohemoglobin %>%
  mutate(high_gh = lbxgh >= 6.5)

regression_data <- cleaned_demographics_data %>%
  inner_join(cleaned_survey_responses_data, by = c("seqn")) %>%
  inner_join(cleaned_body_measurements, by = c("seqn")) %>%
  inner_join(cleaned_glycohemoglobin, by = c("seqn")) %>%
  # don't want to drop ppl with NA for high_gh if dr. told them they had diabetes
  # From the paper: Individuals with missing BMI data (1.2% of our study cohort) 
  # or hemoglobin A1c values (4.7% of our study cohort) were excluded.
  # so we might need to undo this depending on what the authors' code looks like 
  mutate(high_gh_no_NA = if_else(is.na(high_gh) & diq010, 
                                     TRUE,
                                 high_gh),
         has_diabetes = (diq010 == 1) | high_gh,
         # Making the race variable more readable
         race = case_when(ridreth3 == 1 | ridreth3 == 2 ~ "Hispanic",
                          ridreth3 == 3 ~ "White",
                          ridreth3 == 4 ~ "Black",
                          ridreth3 == 6 ~ "Asian",
                          ridreth3 == 7 ~ "Other"),
         # Converting race to factor
         race = factor(race),
         # Re-leveling the race factor, so that White is base level (as in paper)
         race = relevel(race, ref = "White")) %>%
  # From the paper: In the model, we excluded adults with extreme BMIs-that is, 
  # less than 15 kg/m2 or greater than 50 kg/m2 (n = 310) %>%
  filter(bmxbmi > 15,
         bmxbmi < 50) %>%
  select(-lbxgh,
         -high_gh_no_NA,
         -high_gh,
         -ridreth3,
         -diq010) %>%
  # Making column names more readable
  rename(id = seqn,
         age = ridageyr,
         bmi = bmxbmi)

# Eventually need to revisit (first, locate) the Supplement so that we can see the *exact* model they ended up using
model <- glm(has_diabetes ~ age + race + bmi, data = regression_data, family = "binomial")

summary(model)

test_data <- data.frame(race = c("White", "Asian", "Black", "Hispanic"), 
                        age = rep(35, 4), 
                        bmi = rep(25,4))

model_pred <- predict(model, newdata = test_data, type = "response")

paper_outputs <- c(0.014, 0.038, 0.035, 0.03)

model_comparison <- test_data %>%
  mutate(model_pred = model_pred,
         paper_pred = paper_outputs)
