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
         ridreth3,
         ridexprg) %>%
  rename(age = ridageyr, 
         race = ridreth3,
         pregnant = ridexprg) %>%
  filter(!is.na(age)) %>% # line 39 of author code
  filter(age >= 18) %>% # lines 10, 31 of author code
  filter(age <= 70) %>% # lines 10, 31 of author code
  mutate(# Making the race variable more readable
    race = case_when(race == 1 | race == 2 ~ "Hispanic",
                     race == 3 ~ "White",
                     race == 4 ~ "Black",
                     race == 6 ~ "Asian",
                     race == 7 ~ "Other"), # NOTE TO SELF: MIGHT NEED TO REMOVE THIS CATEG. BEFORE TRAINING!
    # Converting race to factor
    race = factor(race),
    # Re-leveling the race factor, so that White is base level (as in paper)
    race = relevel(race, ref = "White")) %>%
  filter(pregnant != 1) %>% # line 32 of author code
  mutate(age2 = age^2, # line 33 of author code
         age3 = age^3)
  
cleaned_survey_responses_data <- raw_survey_responses %>%
  select(seqn,
         diq010) %>%
  filter(!is.na(diq010)) %>% # line 23 of author code
  mutate(diabetes_diagnosis = (diq010 == 1)) %>%
  select(-diq010)
  
cleaned_body_measurements <- raw_body_measurements %>%
  select(seqn,
         bmxbmi) %>%
  rename(bmi = bmxbmi) %>%
  filter(!is.na(bmi)) %>% # line 10 of author code
  filter(bmi >= 18.5) %>% # line 31 of author code (differs from what is said in paper on p. 767)
  filter(bmi <= 50) %>% # line 31 of author code
  mutate(bmi2 = bmi^2, # line 34 of author code
         bmi3 = bmi^3)

cleaned_glycohemoglobin <- raw_glycohemoglobin %>%
  filter(!is.na(lbxgh)) %>% # from paper
  mutate(high_gh = lbxgh >= 6.5) # this logic taken from paper, not reflected in code

regression_data <- cleaned_demographics_data %>%
  inner_join(cleaned_survey_responses_data, by = c("seqn")) %>%
  inner_join(cleaned_body_measurements, by = c("seqn")) %>%
  inner_join(cleaned_glycohemoglobin, by = c("seqn")) %>%
  mutate(has_diabetes = diabetes_diagnosis | high_gh)

# Model formula
formula_1 <- has_diabetes ~ race + 
  age + age2 + age3 + 
  bmi + bmi2 + bmi3 +
  race:age + race:age2 + race:age3 +
  race:bmi + race:bmi2 + race:bmi3 +
  age:bmi


# Eventually need to revisit (first, locate) the Supplement so that we can see the *exact* model they ended up using
model_1 <- glm(formula_1, data = regression_data, family = "binomial")

summary(model_1)

test_data <- data.frame(race = c("White", "Asian", "Black", "Hispanic"), 
                        age = rep(35, 4),
                        bmi = rep(25,4)) %>%
  mutate(age2 = age^2,
         age3 = age^3,
         bmi2 = bmi^2,
         bmi3 = bmi^3)

model_1_pred <- predict(model_1, newdata = test_data, type = "response")

paper_outputs <- c(0.014, 0.038, 0.035, 0.03)


# Model formula
formula_2 <- has_diabetes ~ race + 
  age + bmi + bmi2 +
  race:age + race:bmi + race:bmi2 +
  age:bmi + race:age:bmi


# Eventually need to revisit (first, locate) the Supplement so that we can see the *exact* model they ended up using
model_2 <- glm(formula_2, data = regression_data, family = "binomial")

summary(model_2)

model_2_pred <- predict(model_2, newdata = test_data, type = "response")

model_comparison <- test_data %>%
  mutate(model_1_pred = model_1_pred,
         model_2_pred = model_2_pred,
         paper_pred = paper_outputs)
