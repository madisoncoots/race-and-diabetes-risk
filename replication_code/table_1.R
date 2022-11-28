library(dplyr)
library(magrittr) # for transposing table 
library(kableExtra)
library(readr)

data_path <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))
raw_blood_pressure <- read_csv(paste(data_path, "blood_pressure.csv", sep=""))
raw_cholesterol <- read_csv(paste(data_path, "cholesterol.csv", sep=""))
raw_fasting_glucose <- read_csv(paste(data_path, "fasting_glucose.csv", sep=""))

table_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  inner_join(raw_blood_pressure, by = c("seqn")) %>%
  inner_join(raw_cholesterol, by = c("seqn")) %>%
  filter(ridageyr >= 18) %>%
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  rename(race = ridreth3) %>%
  mutate(# Making the race variable more readable
    race = case_when(race == 1 ~ "Mexican American",
                     race == 2 ~ "Other Hispanic American",
                     race == 3 ~ "White American",
                     race == 4 ~ "Black American",
                     race == 6 ~ "Asian American",
                     race == 7 ~ "Other")) %>%
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
  )

# Top Row (Row 1) : Sample counts by race 
sample_cts_by_race <- table_data %>% 
  count(race) %>% 
  mutate(paper_cts = c(2658, 4597, 2884, 763, 2114, 6319)) 


# Row 2: Projected U.S. adults by race (dplyr only)
projected_cts_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  summarize(freq = sum(wtmec8yr)) %>%
  mutate(paper_cts = c(12019291, 24880636, 20060495, 14210314, 131061575))
  

# Row 3 : % Woman by race (dplyr only!)
pct_woman_by_race <- table_data %>%
  filter(race != "Other") %>%
  filter(ridageyr >= 35) %>% # This is a mistake in the author code!!!
  mutate(is_woman = riagendr == 2) %>%
  group_by(race) %>%
  summarize(pct_woman = sum(is_woman * wtmec8yr) / sum(wtmec8yr) * 100) %>%
  mutate(paper_pct = c(54.9, 55.3, 48.8, 52.4, 51.3)) 


# Row 4 : Mean age by race (dplyr Only!) 
mean_age_by_race <- table_data %>%
  filter(race != "Other") %>%
  mutate(age = as.numeric(as.character((ridageyr)))) %>%
  group_by(race) %>%
  summarize(mean_age = sum(age * wtmec8yr) / sum(wtmec8yr)) %>%
  mutate(paper_mean = c(41.8, 41.7, 38.4, 40.1, 44.9)) 


# Row 5 : Mean BMI by race (dplyr only!)
mean_bmi_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(bmxbmi)) %>% # NEED TO FILTER OUT NA BMI
  mutate(bmi = as.numeric(as.character((bmxbmi)))) %>%
  group_by(race) %>%
  summarize(mean_bmi = sum(bmi * wtmec8yr/sum(wtmec8yr))) %>%
  mutate(paper_mean = c(25.1, 30.9, 30.5, 29.2, 29.1)) 


# Row 6 : Diabetes prevalence % by race (dplyr only)
diabetes_prev_by_race <- table_data %>%
  filter(race != "Other") %>%
  group_by(race) %>%
  summarize(diabetes_prev = sum(diabetes * wtmec8yr) / sum(wtmec8yr) * 100) %>%
  mutate(paper_pct = c(11.9, 14.3, 12.8, 10.7, 9.1)) 


# Row 7 : Mean AC by race (dplyr only!)
mean_ac_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(lbxgh)) %>% # NEED TO FILTER OUT NA LBXGH
  group_by(race) %>%
  summarize(mean_ac = sum(lbxgh * wtmec8yr /sum(wtmec8yr))) %>%
  mutate(mean_ac = round(mean_ac, 1)) %>%
  mutate(paper_mean = c(5.7, 5.8, 5.7, 5.7, 5.5)) 


# Row 8 : Mean systolic blood pressure by race (dplyr only!)
mean_syst_bp_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(bpxsy1)) %>% # NEED TO FILTER OUT NA BPXSY1
  mutate(bpxsy1 = as.numeric(as.character((bpxsy1)))) %>%
  group_by(race) %>%
  summarize(mean_syst_bp = sum(bpxsy1 * wtmec8yr)/sum(wtmec8yr)) %>%
  mutate(mean_syst_bp = round(mean_syst_bp, 1)) %>%
  mutate(paper_mean = c(118.9, 125.1, 119.5, 119.6, 120.2)) 


# Row 9 : Mean diastolic blood pressure by race (dplyr only!)
mean_diast_bp_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(bpxsy1)) %>% # NEED TO FILTER OUT NA BPXSY1
  mutate(bpxdi1 = as.numeric(as.character((bpxdi1)))) %>%
  group_by(race) %>%
  summarize(mean_diast_bp = sum(bpxdi1 * wtmec8yr)/sum(wtmec8yr)) %>%
  mutate(paper_mean = c(72.4, 72.2, 70.3, 70.2, 71.7))
  

# Row 10 : Mean waist circumference by race (no dplyr)
mean_waist_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(bmxwaist)) %>% # NEED TO FILTER OUT NA BMXWAIST
  mutate(bmxwaist = as.numeric(as.character((bmxwaist)))) %>%
  group_by(race) %>%
  summarize(mean_wc = sum(bmxwaist * wtmec8yr)/sum(wtmec8yr)) %>%
  mutate(paper_mean = c(88.3, 100.2, 100.5, 97.3, 99.8))


# Row 11 : Mean total cholesterol (mmol/L) by race (dplyr only)
mean_cholesterol_1_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(lbdtcsi)) %>% # NEED TO FILTER OUT NA LBDTCSI
  mutate(lbdtcsi = as.numeric(as.character((lbdtcsi)))) %>%
  group_by(race) %>%
  dplyr::summarize(mean_lbdtcsi = sum(lbdtcsi * wtmec8yr)/sum(wtmec8yr)) %>%
  mutate(paper_mean = c(4.95, 4.78, 4.90, 4.92, 4.97))


# Row 12 : Mean total cholesterol (mg/dL) by race (no dplyr!)
mean_cholesterol_2_by_race <- table_data %>%
  filter(race != "Other",
         !is.na(lbxtc)) %>% # NEED TO FILTER OUT NA LBXTC
  mutate(lbxtc = as.numeric(as.character((lbxtc)))) %>%
  group_by(race) %>%
  summarize(mean_lbxtc = sum(lbxtc * wtmec8yr)/sum(wtmec8yr)) %>%
  mutate(paper_mean = c(191.4, 184.8, 189.4, 190.2, 192.3))
  
  
# Something interesting to note:
# This other package automatically threw out NA values
# For a lot of the values in the table. I wonder if that is also
# what was happening for the regression

# Making table
sample_cts_by_race %>% # row 1
  select(-paper_cts) %>%
  rename("Number in sample" = n) %>%
  filter(race != "Other") %>%
  left_join(projected_cts_by_race, by = "race") %>% # row 2
  select(-paper_cts) %>%
  mutate(freq = format(freq, nsmall = 0)) %>%
  rename("Projected U.S. adults, n" = freq) %>%
  left_join(pct_woman_by_race, by = c("race")) %>% # row 3
  select(-paper_pct) %>%
  mutate(pct_woman = format(pct_woman, digits = 3)) %>%
  rename("Women, %" = pct_woman) %>%
  left_join(mean_age_by_race, by = c("race")) %>% # row 4
  select(-paper_mean) %>%
  mutate(mean_age = format(mean_age, digits = 3)) %>%
  rename("Mean age, y" = mean_age) %>%
  left_join(mean_bmi_by_race, by = c("race")) %>% # row 5
  select(-paper_mean) %>%
  mutate(mean_bmi = format(mean_bmi, digits = 3)) %>%
  rename("Mean BMI, kg/m^2" = mean_bmi) %>%
  left_join(diabetes_prev_by_race, by = c("race")) %>% # row 6
  select(-paper_pct) %>%
  mutate(diabetes_prev = format(diabetes_prev, digits = 2)) %>%
  rename("Diabetes prevalence, %" = diabetes_prev) %>%
  left_join(mean_ac_by_race, by = c("race")) %>% # row 7
  select(-paper_mean) %>%
  mutate(mean_ac = format(mean_ac, digits = 2)) %>%
  rename("Mean hemoglobin A1c level, %" = mean_ac) %>%
  left_join(mean_syst_bp_by_race, by = c("race")) %>% # row 8
  select(-paper_mean) %>%
  mutate(mean_syst_bp = format(mean_syst_bp, digits = 4)) %>%
  rename("Mean systolic blood pressure, %" = mean_syst_bp) %>%
  left_join(mean_diast_bp_by_race, by = c("race")) %>% # row 9
  select(-paper_mean) %>%
  mutate(mean_diast_bp = format(mean_diast_bp, digits = 3)) %>%
  rename("Mean diastolic blood pressure, %" = mean_diast_bp) %>%
  left_join(mean_waist_by_race, by = c("race")) %>% # row 10
  select(-paper_mean) %>%
  mutate(mean_wc = format(mean_wc, digits = 3)) %>%
  rename("Mean waist circumference, cm" = mean_wc) %>%
  left_join(mean_cholesterol_1_by_race, by = c("race")) %>% # row 11
  select(-paper_mean) %>%
  mutate(mean_lbdtcsi = format(mean_lbdtcsi, digits = 3)) %>%
  rename("Mean total cholesterol level, mmol/L" = mean_lbdtcsi) %>%
  left_join(mean_cholesterol_2_by_race, by = c("race")) %>% # row 12
  select(-paper_mean) %>%
  mutate(mean_lbxtc = format(mean_lbxtc, digits = 4)) %>%
  rename("Mean total cholesterol level, mg/dL" = mean_lbxtc) %>%
  ###
  data.table::transpose(make.names = 'race', keep.names = 'race') %>%
  format(scientific = F) %>%
  rename(characteristic = race) %>% # fixing column name weirdness from transpose
  select(characteristic, 
         "White American",
         "Asian American",
         "Black American",
         "Mexican American",
         "Other Hispanic American") %>%
  `rownames<-`(c()) %>%
  kable("latex",
        col.names = c("Characteristic", "White American", "Asian American", 
                      "Black American", "Mexican American", "Other Hispanic American"),
        vline = "",
        booktabs = T,
        linesep = c("")) %>%
  row_spec(0,bold=TRUE) %>%
  row_spec(c(1:12), hline_after = T) %>%
  column_spec(1, width = "10em") %>%
  kable_styling(latex_options = c("scale_down")) 



