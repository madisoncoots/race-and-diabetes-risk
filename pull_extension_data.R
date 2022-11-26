library(haven)
library(dplyr)
library(janitor)
library(readr)

# Need to pull PAQ, MCQ, DPQ, WHQ

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/HIQ_G.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/FSQ_G.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/PAQ_G.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/MCQ_G.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DPQ_G.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/WHQ_G.XPT", whq <- tempfile(), mode="wb")
raw_health_ins_11_12 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_11_12 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_11_12 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_11_12 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_11_12 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_11_12 <- foreign::read.xport(whq) %>% 
  clean_names()

# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/HIQ_H.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/FSQ_H.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/PAQ_H.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/MCQ_H.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DPQ_H.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/WHQ_H.XPT", whq <- tempfile(), mode="wb")
raw_health_ins_13_14 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_13_14 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_13_14 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_13_14 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_13_14 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_13_14 <- foreign::read.xport(whq) %>% 
  clean_names()

# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/HIQ_I.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/FSQ_I.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/PAQ_I.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/MCQ_I.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DPQ_I.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/WHQ_I.XPT", whq <- tempfile(), mode="wb")
raw_health_ins_15_16 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_15_16 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_15_16 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_15_16 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_15_16 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_15_16 <- foreign::read.xport(whq) %>% 
  clean_names()


# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/HIQ_J.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/FSQ_J.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/PAQ_J.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/MCQ_J.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DPQ_J.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/WHQ_J.XPT", whq <- tempfile(), mode="wb")
raw_health_ins_17_18 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_17_18 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_17_18 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_17_18 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_17_18 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_17_18 <- foreign::read.xport(whq) %>% 
  clean_names()

# ------------------- Combine -------------------

# Health insurance data
raw_health_ins_all <- bind_rows(
  raw_health_ins_11_12,
  raw_health_ins_13_14,
  raw_health_ins_15_16,
  raw_health_ins_17_18
)

# Food insecurity data
raw_food_insec_all <- bind_rows(
  raw_food_insec_11_12,
  raw_food_insec_13_14,
  raw_food_insec_15_16,
  raw_food_insec_17_18
)

# Physical activity data
raw_physical_act_all <- bind_rows(
  raw_physical_act_11_12,
  raw_physical_act_13_14,
  raw_physical_act_15_16,
  raw_physical_act_17_18
)

# Medical conditions data
raw_medical_cond_all <- bind_rows(
  raw_medical_cond_11_12,
  raw_medical_cond_13_14,
  raw_medical_cond_15_16,
  raw_medical_cond_17_18
)

# Mental health data
raw_mental_health_all <- bind_rows(
  raw_mental_health_11_12,
  raw_mental_health_13_14,
  raw_mental_health_15_16,
  raw_mental_health_17_18
)

# Weight history data
raw_weight_hist_all <- bind_rows(
  raw_weight_hist_11_12,
  raw_weight_hist_13_14,
  raw_weight_hist_15_16,
  raw_weight_hist_17_18
)




output_path_string <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/"

write_csv(raw_health_ins_all, paste(output_path_string, "health_insurance.csv", sep=""))
write_csv(raw_food_insec_all, paste(output_path_string, "food_insecurity.csv", sep=""))
write_csv(raw_physical_act_all, paste(output_path_string, "physical_activity.csv", sep=""))
write_csv(raw_medical_cond_all, paste(output_path_string, "medical_conditions.csv", sep=""))
write_csv(raw_mental_health_all, paste(output_path_string, "mental_health.csv", sep=""))
write_csv(raw_weight_hist_all, paste(output_path_string, "weight_history.csv", sep=""))

