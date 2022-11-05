library(dplyr)
library(ggplot2)
library(funModeling) # for Tukey exclusion

data_path <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))

figure_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  filter(ridageyr >= 35) %>%
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  filter(!is.na(diq010)) %>% # line 23 of author code
  rename(age = ridageyr, 
         race = ridreth3) %>%
  mutate(# Making the race variable more readable
    race = case_when(race == 1 ~ "Mexican American",
                     race == 2 ~ "Other Hispanic American",
                     race == 3 ~ "White American",
                     race == 4 ~ "Black American",
                     race == 6 ~ "Asian American",
                     race == 7 ~ "Other")) %>%
  mutate(diabetes_diagnosis = (diq010 == 1)) %>%
  # filter(!is.na(lbxgh)) %>% # from paper
  mutate(high_gh = lbxgh >= 6.5) %>% # this logic taken from paper, not reflected in code
  mutate(has_diabetes = diabetes_diagnosis | high_gh) %>%
  mutate(wtmec8yr = wtmec2yr/4)

NHANES_design <- svydesign(
  data = figure_data, 
  strata = ~sdmvstra, 
  ids = ~sdmvpsu, 
  nest = TRUE, 
  weights = ~wtmec8yr
)

svyquantile(x = ~race + bmxbmi, 
            design = NHANES_design, 
            na.rm = TRUE, 
            quantiles = c(.50))
svyby(formula = ~bmxbmi, by = ~race, FUN = svymean, 
      design = NHANES_design, na.rm = TRUE, keep.names = FALSE) %>%
  ggplot(aes(x = race, y = bmxbmi)) + geom_boxplot()


tukey_thresholds <- tukey_outlier(figure_data$bmxbmi)

figure_data %>%
  filter(race != "Other") %>%
  filter(!is.na(bmxbmi)) %>%
  filter(has_diabetes) %>%
  # filter(bmxbmi >= tukey_thresholds['bottom_threshold'],
  #        bmxbmi <= tukey_thresholds['top_threshold']) %>%
  group_by(race) %>%
  dplyr::summarize(median_bmi = median(bmxbmi))

figure_data %>%
  filter(race != "Other") %>%
  filter(!is.na(bmxbmi)) %>%
  filter(diabetes_diagnosis) %>%
  # filter(bmxbmi > tukey_thresholds['bottom_threshold'],
  #        bmxbmi < tukey_thresholds['top_threshold']) %>%
  group_by(race) %>%
  ggplot(aes(x=race, y=bmxbmi, weight = wtmec8yr)) + 
  geom_boxplot()




svyboxplot(~bmxbmi~ridreth3,
           subset(df_survey_final,ridageyr>=35 & ridageyr<=70  & diabetes==1)
           ,ylim=c(0,60),xlab="Race/Ethnicity",ylab="Body Mass Index",
           col=c("slateblue"),
           xaxt="n",xlim=c(0.5,5.3),range=0,yaxt="n",cex.lab=1.5,cex.main=2,
           main="Body Mass Index of Individuals with Diabetes"
)

axis(1,at=1:6,font=2,
     labels=c(" White \n American","Mexican \n American","Hispanic \n American",
              "Black \n American","Asian \n American","Other"),cex.axis=1.1
     ,tick=FALSE)
axis(2,at=seq(0,60,10),font=2,
     las=2,labels=c("0","10","20","30","40","50","60"),cex.axis=1.2)

# These match
df %>% count(ridreth3)
figure_data %>% count(ridreth3)

df %>% group_by(ridreth3) %>%
  filter(!ridexprg==1 |is.na(ridexprg)) %>%
  filter(!is.na(bmxbmi)) %>%
  filter(ridageyr>=35) %>%
  filter(ridageyr<= 70) %>%
  filter(diabetes == 1) %>%
  dplyr::summarize(med_bmi = median(bmxbmi))

df_survey$variables$bmxbmi
df_survey$variables$ridreth3
data.frame(race = df_survey$variables$ridreth3, 
           bmi = df_survey$variables$bmxbmi,
           age = ) %>%
  group_by(race) %>%
  filter(!is.na(bmi)) %>%
  dplyr::summarise(med_bmi = median(bmi))
