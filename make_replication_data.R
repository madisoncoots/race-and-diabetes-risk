library(haven)
library(dplyr)
library(janitor)
library(readr)

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DIQ_G.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BMX_G.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GHB_G.XPT", ghb <- tempfile(), mode="wb")
raw_demographics_11_12 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_11_12 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_11_12 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_11_12 <- foreign::read.xport(ghb) %>% 
  clean_names()

# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DIQ_H.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BMX_H.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GHB_H.XPT", ghb <- tempfile(), mode="wb")
raw_demographics_13_14 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_13_14 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_13_14 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_13_14 <- foreign::read.xport(ghb) %>% 
  clean_names()

# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DIQ_I.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BMX_I.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GHB_I.XPT", ghb <- tempfile(), mode="wb")
raw_demographics_15_16 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_15_16 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_15_16 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_15_16 <- foreign::read.xport(ghb) %>% 
  clean_names()

# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DIQ_J.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BMX_J.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GHB_J.XPT", ghb <- tempfile(), mode="wb")
raw_demographics_17_18 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_17_18 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_17_18 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_17_18 <- foreign::read.xport(ghb) %>% 
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

output_path_string <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/data"

write_csv(raw_demographics_all, paste(output_path_string, "demographics.csv", sep=""))
write_csv(raw_survey_responses_all, paste(output_path_string, "survey_responses.csv", sep=""))
write_csv(raw_body_measurements_all, paste(output_path_string, "body_measurements.csv", sep=""))
write_csv(raw_glycohemoglobin_all, paste(output_path_string, "glycohemoglobin.csv", sep=""))
