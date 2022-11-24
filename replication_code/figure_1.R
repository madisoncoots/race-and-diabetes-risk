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
                            TRUE ~ "Missing")) %>%
  mutate(lbxgh = as.numeric(as.character((lbxgh))),
         diq010 = as.numeric(as.character((diq010))),
         a1c = cut(lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE),
         diabetes_diagnosis = case_when(diq010 %in% 1 ~ 1, 
                                        diq010 %in% c(2,3,9) ~ 0,
                                        diq010 %in% 7 ~ as.numeric(NA)),
         diabetes = diabetes_diagnosis,
         diabetes = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), 1, diabetes)
  )

# NHANES_design <- svydesign(
#   data = figure_data, 
#   strata = ~sdmvstra, 
#   ids = ~sdmvpsu, 
#   nest = TRUE, 
#   weights = ~wtmec8yr
# )

# svyquantile(x = ~race + bmxbmi, 
#             design = NHANES_design, 
#             na.rm = TRUE, 
#             quantiles = c(.50))
# svyby(formula = ~bmxbmi, by = ~race, FUN = svymean, 
#       design = NHANES_design, na.rm = TRUE, keep.names = FALSE) %>%
#   ggplot(aes(x = race, y = bmxbmi)) + geom_boxplot()


tukey_thresholds <- tukey_outlier(figure_data$bmxbmi)

# For later sanity checks....
# figure_data %>%
#   filter(race != "Other") %>%
#   filter(!is.na(bmxbmi)) %>%
#   filter(diabetes == 1) %>%
#   filter(bmxbmi >= tukey_thresholds['bottom_threshold'],
#          bmxbmi <= tukey_thresholds['top_threshold']) %>%
#   group_by(race) %>%
#   dplyr::summarize(median_bmi = median(bmxbmi))

figure_data %>%
  filter(race != "Other") %>%
  filter(!is.na(bmxbmi)) %>%
  filter(diabetes == 1) %>%
  filter(bmxbmi > tukey_thresholds['bottom_threshold'],
         bmxbmi < tukey_thresholds['top_threshold']) %>%
  group_by(race) %>%
  mutate(race = factor(race, c("White American", 
                               "Asian American",
                               "Black American",
                               "Mexican American",
                               "Other Hispanic American"))) %>%
  ggplot(aes(x=race, y=bmxbmi, weight = wtmec8yr)) + 
  geom_boxplot(fill="darkgray", outlier.shape = NA) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(0,60)) +
  labs(x = "Race/Ethnicity",
       y = "BMI, kg/m^2",
       title = "BMI among U.S. adults with diabetes,\nstratified by race/ethnicity.") +
  scale_x_discrete(labels=c("White American" = "White\nAmerican",
                            "Asian American" = "Asian\nAmerican",
                            "Black American" = "Black\nAmerican",
                            "Mexican American" = "Mexican\nAmerican",
                            "Other Hispanic American" = "Other\nHispanic\nAmerican")) +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_text(face="bold", size=12))

save_path <- "/home/mcoots/harvard/research/race-in-healthcare/replication_figs/"
ggsave(paste(save_path, "bmi_boxplot.png"), height = 8, width = 6)

# For later sanity checks...
# df %>% filter(!is.na(bmxbmi)) %>%
#   filter(diabetes == 1) %>%
#   filter(bmxbmi > tukey_thresholds['bottom_threshold'],
#          bmxbmi < tukey_thresholds['top_threshold']) %>%
#   group_by(ridreth3) %>%
#   ggplot(aes(x=ridreth3, y=bmxbmi, weight = wtmec8yr)) + 
#   geom_boxplot(outlier.shape = NA) +
#   labs(x = "Race")