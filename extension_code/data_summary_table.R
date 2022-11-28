library(kableExtra)
library(readr)
library(dplyr)
library(magrittr) # for transposing table 

data_path <- "/Volumes/GoogleDrive/My Drive/2022 fall classes/GOV 2001/Replication/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))
raw_health_ins <- read_csv(paste(data_path, "health_insurance.csv", sep=""))
raw_food_insec <- read_csv(paste(data_path, "food_insecurity.csv", sep=""))
raw_physical_act <- read_csv(paste(data_path, "physical_activity.csv", sep=""))
raw_medical_cond <- read_csv(paste(data_path, "medical_conditions.csv", sep=""))
raw_mental_health <- read_csv(paste(data_path, "mental_health.csv", sep=""))
raw_weight_hist <- read_csv(paste(data_path, "weight_history.csv", sep=""))

table_data <- raw_demographics_data %>%
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

# Top Row (Row 1) : Sample counts by race 
sample_cts_by_race <- table_data %>% 
  count(race)

# Row 2: Projected U.S. adults by race (dplyr only)
projected_cts_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  summarize(freq = sum(wtmec8yr))

# Row 3 : Diabetes prevalence % by race (dplyr only)
diabetes_prev_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  summarize(diabetes_prev = sum(diabetes * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 4 : Mean age by race (dplyr Only!) 
mean_age_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(age = as.numeric(as.character((ridageyr)))) %>%
  group_by(race) %>%
  summarize(mean_age = sum(age * wtmec8yr) / sum(wtmec8yr))

# Row 5 : Mean BMI by race (dplyr only!)
mean_bmi_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(bmxbmi)) %>% # NEED TO FILTER OUT NA BMI
  mutate(bmi = as.numeric(as.character((bmxbmi)))) %>%
  group_by(race) %>%
  summarize(mean_bmi = sum(bmi * wtmec8yr/sum(wtmec8yr)))

# Row 6 : % Income by race (dplyr only!)
pct_income_lt20000_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(income_lt20000 = (indhhin2 == 1) | 
           (indhhin2 == 2) |
           (indhhin2 == 3) |
           (indhhin2 == 4) |
           (indhhin2 == 13)) %>%
  group_by(race) %>%
  filter(!is.na(income_lt20000)) %>%
  summarize(pct_income_lt20000 = sum(income_lt20000 * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 7 : % Health insurance by race (dplyr only!)
pct_has_health_insurance_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(has_health_insurance = hiq011 == 1) %>%
  group_by(race) %>%
  summarize(pct_has_health_insurance = sum(has_health_insurance * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 8 : % College by race (dplyr only!)
pct_college_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(college = dmdeduc2 == 5) %>%
  group_by(race) %>%
  filter(!is.na(college)) %>%
  summarize(pct_college = sum(college * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 9 : % Food insecurity by race (dplyr only!)
pct_food_secure_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(food_secure = fsd032a == 3) %>%
  group_by(race) %>%
  filter(!is.na(food_secure)) %>%
  summarize(pct_food_secure = sum(food_secure * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 10 : % Woman by race (dplyr only!)
pct_woman_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(is_woman = riagendr == 2) %>%
  group_by(race) %>%
  summarize(pct_woman = sum(is_woman * wtmec8yr) / sum(wtmec8yr) * 100)

#To do: Move NA filter statements into table_data chunk
# Row 11 : Minutes vigorous recreational activities
mean_rec_activity_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  filter(!is.na(pad660)) %>%
  summarize(mean_rec_activity = sum(pad660 * wtmec8yr) / sum(wtmec8yr))

# Row 12 : Minutes vigorous-intensity work
mean_work_activity_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  filter(!is.na(pad615)) %>%
  summarize(mean_work_activity = sum(pad615 * wtmec8yr) / sum(wtmec8yr))

# Row 13 : % Increased diabetes risk
pct_increased_diabetes_risk_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(is_increased_diabetes_risk = diq170 == 1) %>%
  group_by(race) %>%
  filter(!is.na(diq170)) %>%
  summarize(pct_increased_diabetes_risk = sum(is_increased_diabetes_risk * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 14 : % Relatives had diabetes
pct_relatives_had_diabetes_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(is_relatives_had_diabetes = mcq300c == 1) %>%
  group_by(race) %>%
  filter(!is.na(mcq300c)) %>%
  summarize(pct_relatives_had_diabetes = sum(is_relatives_had_diabetes * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 15 : % Felt depressed
pct_depressed_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(depressed = (dpq020 == 1) | 
           (dpq020 == 2) |
           (dpq020 == 3)) %>%
  group_by(race) %>%
  filter(!is.na(dpq020)) %>%
  summarize(pct_depressed = sum(depressed * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 16 : % Feels at risk diabetes
pct_feels_at_risk_diabetes_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(is_feels_at_risk_diabetes = diq172 == 1) %>%
  group_by(race) %>%
  filter(!is.na(diq172)) %>%
  summarize(pct_feels_at_risk_diabetes = sum(is_feels_at_risk_diabetes * wtmec8yr) / sum(wtmec8yr) * 100)

# Row 17 : Self-reported greatest weight (pounds)
mean_greatest_weight_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  filter(!is.na(whd140)) %>%
  summarize(mean_greatest_weight = sum(whd140 * wtmec8yr) / sum(wtmec8yr))

# Row 18 : Weight (kg)
mean_weight_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  filter(!is.na(bmxwt)) %>%
  summarize(mean_weight = sum(bmxwt * wtmec8yr) / sum(wtmec8yr))

# Row 19 : Standing Height (cm)
mean_height_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  filter(!is.na(bmxht)) %>%
  summarize(mean_height = sum(bmxht * wtmec8yr) / sum(wtmec8yr))

# Row 20 : Waist Circumference (cm)
mean_waist_circumference_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  filter(!is.na(bmxwaist)) %>%
  summarize(mean_waist_circumference = sum(bmxwaist * wtmec8yr) / sum(wtmec8yr))

# Something interesting to note:
# This other package automatically threw out NA values
# For a lot of the values in the table. I wonder if that is also
# what was happening for the regression

# Making table
sample_cts_by_race %>%
  rename("Number in sample" = n) %>%
  left_join(projected_cts_by_race, by = "race") %>%
  mutate(freq = format(freq, nsmall = 0)) %>%
  rename("Projected adults, n" = freq) %>%
  left_join(diabetes_prev_by_race, by = c("race")) %>%
  mutate(diabetes_prev = format(diabetes_prev, digits = 3)) %>%
  rename("Diabetes, %" = diabetes_prev) %>%
  left_join(mean_age_by_race, by = c("race")) %>%
  mutate(mean_age = format(mean_age, digits = 3)) %>%
  rename("Mean age, y" = mean_age) %>%
  left_join(mean_bmi_by_race, by = c("race")) %>%
  mutate(mean_bmi = format(mean_bmi, digits = 3)) %>%
  rename("Mean BMI, kg/m^2" = mean_bmi) %>%
  left_join(pct_income_lt20000_by_race, by = c("race")) %>%
  mutate(pct_income_lt20000 = format(pct_income_lt20000, digits = 3)) %>%
  rename("Income <$20,000, %" = pct_income_lt20000) %>%
  left_join(pct_has_health_insurance_by_race, by = c("race")) %>%
  mutate(pct_has_health_insurance = format(pct_has_health_insurance, digits = 3)) %>%
  rename("Health insurance, %" = pct_has_health_insurance) %>%
  left_join(pct_college_by_race, by = c("race")) %>%
  mutate(pct_college = format(pct_college, digits = 3)) %>%
  rename("College gradaute, %" = pct_college) %>%
  left_join(pct_food_secure_by_race, by = c("race")) %>%
  mutate(pct_food_secure = format(pct_food_secure, digits = 3)) %>%
  rename("Food secure, %" = pct_food_secure) %>%
  left_join(pct_woman_by_race, by = c("race")) %>%
  mutate(pct_woman = format(pct_woman, digits = 3)) %>%
  rename("Women, %" = pct_woman) %>%
  left_join(mean_rec_activity_by_race, by = c("race")) %>%
  mutate(mean_rec_activity = format(mean_rec_activity, digits = 3)) %>%
  rename("Mean daily recreational exercise, min" = mean_rec_activity) %>%
  left_join(mean_work_activity_by_race, by = c("race")) %>%
  mutate(mean_work_activity = format(mean_work_activity, digits = 3)) %>%
  rename("Mean daily work exercise, min" = mean_work_activity) %>%  
  left_join(pct_increased_diabetes_risk_by_race, by = c("race")) %>%
  mutate(pct_increased_diabetes_risk = format(pct_increased_diabetes_risk, digits = 3)) %>%
  rename("Ever told have health risk for diabetes, %" = pct_increased_diabetes_risk) %>%
  left_join(pct_relatives_had_diabetes_by_race, by = c("race")) %>%
  mutate(pct_relatives_had_diabetes = format(pct_relatives_had_diabetes, digits = 3)) %>%
  rename("Close relative who had diabetes, %" = pct_relatives_had_diabetes) %>%
  left_join(pct_depressed_by_race, by = c("race")) %>%
  mutate(pct_depressed = format(pct_depressed, digits = 3)) %>%
  rename("Depressed at least several days over past 2 weeks, %" = pct_depressed) %>%
  left_join(pct_feels_at_risk_diabetes_by_race, by = c("race")) %>%
  mutate(pct_feels_at_risk_diabetes = format(pct_feels_at_risk_diabetes, digits = 3)) %>%
  rename("Feels at risk of diabetes, %" = pct_feels_at_risk_diabetes) %>%
  left_join(mean_greatest_weight_by_race, by = c("race")) %>%
  mutate(mean_greatest_weight = format(mean_greatest_weight, digits = 3)) %>%
  rename("Mean greatest weight, pounds" = mean_greatest_weight) %>%
  left_join(mean_weight_by_race, by = c("race")) %>%
  mutate(mean_weight = format(mean_weight, digits = 3)) %>%
  rename("Mean weight, kg" = mean_weight) %>%
  left_join(mean_height_by_race, by = c("race")) %>%
  mutate(mean_height = format(mean_height, digits = 3)) %>%
  rename("Mean height, cm" = mean_height) %>%
  left_join(mean_waist_circumference_by_race, by = c("race")) %>%
  mutate(mean_waist_circumference = format(mean_waist_circumference, digits = 3)) %>%
  rename("Mean waist circumference, cm" = mean_waist_circumference) %>%
  ###
  data.table::transpose(make.names = 'race', keep.names = 'race') %>%
  format(scientific = F) %>%
  rename(characteristic = race) %>% # fixing column name weirdness from transpose
  select(characteristic, 
         "White American",
         "Asian American",
         "Black American",
         "Hispanic American") %>%
  `rownames<-`(c()) %>%
  kable("latex",
        col.names = c("Characteristic", "White American", "Asian American", 
                      "Black American", "Hispanic American"),
        vline = "",
        booktabs = T,
        linesep = c("")) %>%
  row_spec(0,bold=TRUE) %>%
  row_spec(c(1:20), hline_after = T) %>%
  column_spec(1, width = "10em") %>%
  kable_styling(latex_options = c("scale_down")) 