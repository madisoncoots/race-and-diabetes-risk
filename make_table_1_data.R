library(haven)
library(dplyr)
library(janitor)
library(readr)

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BPX_G.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/TCHOL_G.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GLU_G.XPT", glu <- tempfile(), mode="wb")
raw_blood_pressure_11_12 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_cholesterol_11_12 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_fasting_glucose_11_12 <- foreign::read.xport(glu) %>%
  clean_names()

# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPX_H.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/TCHOL_H.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GLU_H.XPT", glu <- tempfile(), mode="wb")
raw_blood_pressure_13_14 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_cholesterol_13_14 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_fasting_glucose_13_14 <- foreign::read.xport(glu) %>%
  clean_names()

# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BPX_I.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/TCHOL_I.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GLU_I.XPT", glu <- tempfile(), mode="wb")
raw_blood_pressure_15_16 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_cholesterol_15_16 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_fasting_glucose_15_16 <- foreign::read.xport(glu) %>%
  clean_names()

# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BPX_J.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/TCHOL_J.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GLU_J.XPT", glu <- tempfile(), mode="wb")
raw_blood_pressure_17_18 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_cholesterol_17_18 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_fasting_glucose_17_18 <- foreign::read.xport(glu) %>% 
  clean_names()

# ------------------- Combine -------------------

# Blood pressure data
raw_blood_pressure_all <- bind_rows(
  raw_blood_pressure_11_12,
  raw_blood_pressure_13_14,
  raw_blood_pressure_15_16,
  raw_blood_pressure_17_18
)

# Cholesterol data
raw_cholesterol_all <- bind_rows(
  raw_cholesterol_11_12,
  raw_cholesterol_13_14,
  raw_cholesterol_15_16,
  raw_cholesterol_17_18
)

# Fasting glucose data
raw_fasting_glucose_all <- bind_rows(
  raw_fasting_glucose_11_12,
  raw_fasting_glucose_13_14,
  raw_fasting_glucose_15_16,
  raw_fasting_glucose_17_18
)

output_path_string <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"

write_csv(raw_blood_pressure_all, paste(output_path_string, "blood_pressure.csv", sep=""))
write_csv(raw_cholesterol_all, paste(output_path_string, "cholesterol.csv", sep=""))
write_csv(raw_fasting_glucose_all, paste(output_path_string, "fasting_glucose.csv", sep=""))
