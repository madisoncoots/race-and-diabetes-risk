library(dplyr)
library(kableExtra)

data_path <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"
save_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/"
roc_save_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))


regression_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  filter(ridageyr >= 35) %>%
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  rename(race = ridreth3) %>%
  mutate(# Making the race variable more readable
    race = case_when(race == 1 ~ "Mexican American",
                     race == 2 ~ "Other Hispanic American",
                     race == 3 ~ "White American",
                     race == 4 ~ "Black American",
                     race == 6 ~ "Asian American",
                     race == 7 ~ "Other"),
    race = factor(race),
    # Re-leveling the race factor, so that White is base level (as in paper)
    race = relevel(race, ref = "White American")) %>%
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
  ) %>%
  mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))

# Test models --------------------------

glm(diabetes ~ race, 
    data = regression_data, 
    family = "binomial",
    weights = wtmec8yr/1000)

glm(diabetes ~ race, 
    data = regression_data, 
    family = "quasibinomial", # glm complains when weights aren't ints
    weights = wtmec8yr/1000)

glm(diabetes ~ race, 
    data = regression_data, 
    family = "binomial",
    weights = normalized_weights * nrow(regression_data))

#  --------------------------

race_only_model <- glm(diabetes ~ race, 
    data = regression_data, 
    family = "binomial",
    weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

saveRDS(race_only_model, file = paste(save_path, "race_only_model.rda", sep = ""))

names <- names(coef(race_only_model))
coef_vals <- coef(race_only_model)
se <- sqrt(diag(vcovHC(race_only_model, type = "HC0")))

# For paper 

data.frame(Parameter = names,
           Estimate = coef_vals,
           CI_Low = coef_vals - 1.96 * se,
           CI_High = coef_vals + 1.96 * se) %>%
  `rownames<-`(c()) %>%
  kable("latex",
        col.names = c("Parameter", "Estimate", "95% Conf. Lower Limit", "95% Conf. Upper Limit"),
        digits = 3,
        vline = "",
        booktabs = T,
        linesep = c("", "", "", "", "", "", "\\addlinespace")) %>%
  row_spec(0,bold=TRUE)

# This is just non-sensical output

# regression_design <- svydesign(id = ~sdmvpsu, 
#                                strata = ~sdmvstra, 
#                                nest=TRUE,
#                     weights = ~wtmec8yr, 
#                     data=regression_data)
# 
# svyglm(diabetes ~ race, regression_design)


# Model evaluation
predictions <- round(predict(race_only_model, newdata = regression_data, type = "response") * 100, 2)
auc(regression_data$diabetes, predictions)


# ROCR 
data_for_roc <-
  regression_data %>%
  mutate(predictions = predictions) %>%
  filter(!is.na(predictions)) # Need this step to drop NA predictions from the ROC

# NOTE: We should move this to a util file eventually
make_roc_data <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), scores[order(scores, decreasing=TRUE)], labels)
}

roc_data <- make_roc_data(data_for_roc$diabetes, data_for_roc$predictions)

write_csv(roc_data, paste(roc_save_path, "race_only_roc.csv", sep = ""))

