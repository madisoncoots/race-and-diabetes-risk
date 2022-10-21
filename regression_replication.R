library(haven)
library(janitor)

raw_demographics_data <- read_xpt("/home/mcoots/harvard/research/race-in-healthcare/data/DEMO_J.XPT") %>%
  clean_names()

raw_survey_responses <- read_xpt("/home/mcoots/harvard/research/race-in-healthcare/data/DIQ_J.XPT") %>% 
  clean_names()

raw_body_measurements <- read_xpt("/home/mcoots/harvard/research/race-in-healthcare/data/BMX_J.XPT") %>%
  clean_names()

raw_glycohemoglobin <- read_xpt("/home/mcoots/harvard/research/race-in-healthcare/data/GHB_J.XPT") %>%
  clean_names() 

library(tidyverse)

# Ignoring weights for the first stab at the regression replication

cleaned_demographics_data <- raw_demographics_data %>%
  select(seqn,
         ridageyr,
         riagendr,
         ridreth1,
         ridreth3)

cleaned_survey_responses_data <- raw_survey_responses %>%
  select(seqn,
         diq010)

cleaned_body_measurements <- raw_body_measurements %>%
  select(seqn,
         bmxbmi)

cleaned_glycohemoglobin <- raw_glycohemoglobin %>%
  mutate(above_6.5 = lbxgh >= 6.5)

regression_data <- cleaned_demographics_data %>%
  inner_join(cleaned_survey_responses_data, by = c("seqn")) %>%
  inner_join(cleaned_body_measurements, by = c("seqn")) %>%
  inner_join(cleaned_glycohemoglobin, by = c("seqn")) %>% 
  mutate(has_diabetes = (diq010 == 1) | above_6.5) # need to be careful--dont want to drop folks with NA for above_6.5 if dr. told them they had diabetes
