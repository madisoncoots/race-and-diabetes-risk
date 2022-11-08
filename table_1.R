library(dplyr)
library(survey)

data_path <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))
raw_blood_pressure <- read_csv(paste(data_path, "blood_pressure.csv", sep=""))
raw_cholesterol <- read_csv(paste(data_path, "cholesterol.csv", sep=""))

table_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  inner_join(raw_blood_pressure, by = c("seqn")) %>%
  inner_join(raw_cholesterol, by = c("seqn")) %>%
  filter(ridageyr >= 18) %>%
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  rename(age = ridageyr, 
         race = ridreth3) %>%
  mutate(# Making the race variable more readable
    race = case_when(race == 1 ~ "Mexican American",
                     race == 2 ~ "Other Hispanic American",
                     race == 3 ~ "White American",
                     race == 4 ~ "Black American",
                     race == 6 ~ "Asian American",
                     race == 7 ~ "Other")) %>%
  mutate(wtmec8yr = wtmec2yr/4) %>%
  mutate(gender = case_when(riagendr == 1 ~ "Man",
                            riagendr == 2 ~ "Woman",
                            TRUE ~ "Missing"))
  

NHANES_design <- svydesign(
  data = table_data %>% filter(race != "Other"), 
  strata = ~sdmvstra, 
  ids = ~sdmvpsu, 
  nest = TRUE, 
  weights = ~wtmec8yr
)

# Top Row (Row 1) : Sample counts by race 
table_data %>% count(race) %>% 
  View()

# Row 2 :  Projected U.S. adults by race
svytable(~race, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(prop = Freq / sum(Freq) * 100) %>%
  arrange(desc(prop)) %>%
  select(-prop) %>% 
  View()

# Row 3 : % Woman by race
svytable(~race + gender, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(prop = Freq / sum(Freq) * 100) %>%
  arrange(desc(prop)) %>%
  data.frame() %>%
  group_by(race) %>%
  mutate(in_group_pct = prop/sum(prop) * 100) %>%
  filter(gender == "Woman") %>%
  select(-Freq, -prop) %>%
  rename(pct = in_group_pct) %>% 
  View()

# Row 4 : Mean age by race
svytable(~race + age, design = NHANES_design) %>%
  as.data.frame() %>%
  group_by(race) %>%
  mutate(age = as.numeric(as.character((age)))) %>%
  summarize(mean_age = sum(age*Freq/sum(Freq))) %>% 
  View()
 
# Row 5 : Mean BMI by race  
svytable(~race + bmxbmi, design = NHANES_design) %>%
  as.data.frame() %>%
  group_by(race) %>%
  mutate(bmi = as.numeric(as.character((bmxbmi)))) %>%
  summarize(mean_bmi = sum(bmi*Freq/sum(Freq))) %>% 
  View()

# Row 6 : Diabetes prevalence % by race NOT DONE!!!!!!
# svytable(~race + diq010 + lbxgh, design = NHANES_design) %>%
#   as.data.frame() %>%
#   mutate(lbxgh = as.numeric(as.character((lbxgh))),
#          a1c = cut(lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE)) %>%
#   mutate(diabetes_diagnosis = if_else(diq010 == 1, 1, 0),
#          # high_gh = a1c == "[6.5,1e+03)",
#          diabetes = diabetes_diagnosis,
#          diabetes = if_else(a1c == "[6.5,1e+03)" & !is.na(a1c), 1, diabetes)) %>%
#          # diabetes = diabetes_diagnosis | high_gh) %>%
#   group_by(race) %>%
#   summarize(diabetes_pct = sum(diabetes*Freq/sum(Freq))) %>%
#   mutate(diabetes_pct = round(diabetes_pct, 3) * 100) %>% 
#   View() 

svytable(~race + diq010 + lbxgh, design = NHANES_design) %>%
  as.data.frame() %>%
  # filter(diq010 == 1 | diq010 == 2) %>%
  mutate(lbxgh = as.numeric(as.character((lbxgh))),
         diq010 = as.numeric(as.character((diq010))),
         high_gh = lbxgh >= 6.5,
         diabetes_diagnosis = if_else(diq010 == 1, 1, 0)) %>%
  mutate(diabetes = diabetes_diagnosis | high_gh) %>%
  group_by(race) %>%
  dplyr::summarize(diabetes_pct = sum(diabetes*Freq/sum(Freq)) * 100) 

# Row 7 : Mean AC by race 
svytable(~race + lbxgh, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(lbxgh = as.numeric(as.character((lbxgh)))) %>%
  group_by(race) %>%
  summarize(mean_ac = sum(lbxgh*Freq/sum(Freq))) %>%
  mutate(mean_ac = round(mean_ac, 1)) %>% 
  View()

# Row 8 : Mean systolic blood pressure by race
svytable(~race + bpxsy1, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(bpxsy1 = as.numeric(as.character((bpxsy1)))) %>%
  group_by(race) %>%
  summarize(mean_bp = sum(bpxsy1*Freq)/sum(Freq)) %>% 
  View()

# Row 9 : Mean diastolic blood pressure by race 
svytable(~race + bpxdi1, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(bpxdi1 = as.numeric(as.character((bpxdi1)))) %>%
  group_by(race) %>%
  summarize(mean_bp = sum(bpxdi1*Freq)/sum(Freq)) %>% 
  View()

# Row 10 : Mean waist circumference by race
svytable(~race + bmxwaist, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(bmxwaist = as.numeric(as.character((bmxwaist)))) %>%
  group_by(race) %>%
  summarize(mean_wc = sum(bmxwaist*Freq)/sum(Freq)) %>% 
  View()

# Row 11 : Mean waist circumference (mmol/L) by race
svytable(~race + lbdtcsi, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(lbdtcsi = as.numeric(as.character((lbdtcsi)))) %>%
  group_by(race) %>%
  summarize(mean_lbdtcsi = sum(lbdtcsi*Freq)/sum(Freq)) %>% 
  View()

# Row 12 : Mean total cholesterol (mg/dL) by race
svytable(~race + lbxtc, design = NHANES_design) %>%
  as.data.frame() %>%
  mutate(lbxtc = as.numeric(as.character((lbxtc)))) %>%
  group_by(race) %>%
  summarize(mean_lbxtc = sum(lbxtc*Freq)/sum(Freq)) %>% 
  View()
