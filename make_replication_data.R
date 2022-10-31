library(haven)
library(janitor)
library(dplyr)

path_string <- "/home/mcoots/harvard/research/race-in-healthcare/data/raw/"

# 2011-2012
raw_demographics_11_12 <- read_xpt(paste(path_string, "2011-2012/DEMO_G.XPT", sep = "")) %>%
  clean_names()
raw_survey_responses_11_12 <- read_xpt(paste(path_string, "2011-2012/DIQ_G.XPT", sep = "")) %>%
  clean_names()
raw_body_measurements_11_12 <- read_xpt(paste(path_string, "2011-2012/BMX_G.XPT", sep = "")) %>%
  clean_names()
raw_glycohemoglobin_11_12 <- read_xpt(paste(path_string, "2011-2012/GHB_G.XPT", sep = "")) %>%
  clean_names()
raw_reproductive_health_11_12 <- read_xpt(paste(path_string, "2011-2012/RHQ_G.XPT", sep = "")) %>%
  clean_names()

# 2013-2014
raw_demographics_13_14 <- read_xpt(paste(path_string, "2013-2014/DEMO_H.XPT", sep = "")) %>%
  clean_names()
raw_survey_responses_13_14 <- read_xpt(paste(path_string, "2013-2014/DIQ_H.XPT", sep = "")) %>%
  clean_names()
raw_body_measurements_13_14 <- read_xpt(paste(path_string, "2013-2014/BMX_H.XPT", sep = "")) %>%
  clean_names()
raw_glycohemoglobin_13_14 <- read_xpt(paste(path_string, "2013-2014/GHB_H.XPT", sep = "")) %>%
  clean_names()
raw_reproductive_health_13_14 <- read_xpt(paste(path_string, "2013-2014/RHQ_H.XPT", sep = "")) %>%
  clean_names()

# 2015-2016
raw_demographics_15_16 <- read_xpt(paste(path_string, "2015-2016/DEMO_I.XPT", sep = "")) %>%
  clean_names()
raw_survey_responses_15_16 <- read_xpt(paste(path_string, "2015-2016/DIQ_I.XPT", sep = "")) %>%
  clean_names()
raw_body_measurements_15_16 <- read_xpt(paste(path_string, "2015-2016/BMX_I.XPT", sep = "")) %>%
  clean_names()
raw_glycohemoglobin_15_16 <- read_xpt(paste(path_string, "2015-2016/GHB_I.XPT", sep = "")) %>%
  clean_names()
raw_reproductive_health_15_16 <- read_xpt(paste(path_string, "2015-2016/RHQ_I.XPT", sep = "")) %>%
  clean_names()

# 2017-2018
raw_demographics_17_18 <- read_xpt(paste(path_string, "2017-2018/DEMO_J.XPT", sep = "")) %>%
  clean_names()
raw_survey_responses_17_18 <- read_xpt(paste(path_string, "2017-2018/DIQ_J.XPT", sep = "")) %>%
  clean_names()
raw_body_measurements_17_18 <- read_xpt(paste(path_string, "2017-2018/BMX_J.XPT", sep = "")) %>%
  clean_names()
raw_glycohemoglobin_17_18 <- read_xpt(paste(path_string, "2017-2018/GHB_J.XPT", sep = "")) %>%
  clean_names()
raw_reproductive_health_17_18 <- read_xpt(paste(path_string, "2017-2018/RHQ_J.XPT", sep = "")) %>%
  clean_names()

# ------------------- Combine -------------------

# Demographics data
raw_demographics_all <- bind_rows(
  raw_demographics_11_12,
  raw_demographics_13_14,
  raw_demographics_15_16,
  raw_demographics_17_18
  )

# Survey data
raw_survey_responses_all <- bind_rows(
  raw_survey_responses_11_12,
  raw_survey_responses_13_14,
  raw_survey_responses_15_16,
  raw_survey_responses_17_18
  )

# Body measurements data
raw_body_measurements_all <- bind_rows(
  raw_body_measurements_11_12,
  raw_body_measurements_13_14,
  raw_body_measurements_15_16,
  raw_body_measurements_17_18
  )

# Glycohemoglobin data
raw_glycohemoglobin_all <- bind_rows(
  raw_glycohemoglobin_11_12,
  raw_glycohemoglobin_13_14,
  raw_glycohemoglobin_15_16,
  raw_glycohemoglobin_17_18
)

# Reproductive health data
raw_reproductive_health_all <- bind_rows(
  raw_reproductive_health_11_12,
  raw_reproductive_health_13_14,
  raw_reproductive_health_15_16,
  raw_reproductive_health_17_18
)

output_path_string <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"

write_csv(raw_demographics_all, paste(output_path_string, "demographics.csv", sep=""))
write_csv(raw_survey_responses_all, paste(output_path_string, "survey_responses.csv", sep=""))
write_csv(raw_body_measurements_all, paste(output_path_string, "body_measurements.csv", sep=""))
write_csv(raw_glycohemoglobin_all, paste(output_path_string, "glycohemoglobin.csv", sep=""))
write_csv(raw_reproductive_health_all, paste(output_path_string, "reproductive_health.csv", sep=""))
