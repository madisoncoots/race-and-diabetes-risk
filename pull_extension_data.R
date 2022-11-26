library(haven)
library(dplyr)
library(janitor)
library(readr)

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/HIQ_G.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/FSQ_G.XPT", fsq <- tempfile(), mode="wb")
raw_health_ins_11_12 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_11_12 <- foreign::read.xport(fsq) %>% 
  clean_names()

# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/HIQ_H.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/FSQ_H.XPT", fsq <- tempfile(), mode="wb")
raw_health_ins_13_14 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_13_14 <- foreign::read.xport(fsq) %>% 
  clean_names()

# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/HIQ_I.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/FSQ_I.XPT", fsq <- tempfile(), mode="wb")
raw_health_ins_15_16 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_15_16 <- foreign::read.xport(fsq) %>% 
  clean_names()


# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/HIQ_J.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/FSQ_J.XPT", fsq <- tempfile(), mode="wb")
raw_health_ins_17_18 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_17_18 <- foreign::read.xport(fsq) %>% 
  clean_names()

# ------------------- Combine -------------------

# Health insurance data
raw_health_ins_all <- bind_rows(
  raw_health_ins_11_12,
  raw_health_ins_13_14,
  raw_health_ins_15_16,
  raw_health_ins_17_18
)

raw_food_insec_all <- bind_rows(
  raw_food_insec_11_12,
  raw_food_insec_13_14,
  raw_food_insec_15_16,
  raw_food_insec_17_18
)



output_path_string <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/data"

write_csv(raw_health_ins_all, paste(output_path_string, "health_insurance.csv", sep=""))
write_csv(raw_food_insec_all, paste(output_path_string, "food_insecurity.csv", sep=""))

