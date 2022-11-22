library(dplyr)
library(kableExtra)

data_path <- "/home/mcoots/harvard/research/race-in-healthcare/data/parsed/"
save_path <- "/home/mcoots/harvard/research/race-in-healthcare/models/"

raw_demographics_data <- read_csv(paste(data_path, "demographics.csv", sep=""))
raw_survey_responses <- read_csv(paste(data_path, "survey_responses.csv", sep=""))
raw_body_measurements <- read_csv(paste(data_path, "body_measurements.csv", sep=""))
raw_glycohemoglobin <- read_csv(paste(data_path, "glycohemoglobin.csv", sep=""))


regression_data <- raw_demographics_data %>%
  inner_join(raw_survey_responses, by = c("seqn")) %>%
  inner_join(raw_body_measurements, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin, by = c("seqn")) %>%
  filter(ridageyr >= 18) %>% # Taken from Supplement
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  filter(bmxbmi >= 18.5, # Taken from Supplement
         bmxbmi <= 50) %>%
  rename(race = ridreth3) %>%
  mutate(# Making the race variable more readable
    race = case_when(race == 1 | race == 2 ~ "Hispanic American",
                     race == 3 ~ "White American",
                     race == 4 ~ "Black American",
                     race == 6 ~ "Asian American"),
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

final_model_formula <- diabetes ~ race + ridageyr + bmxbmi +
  I(bmxbmi^2) + race:ridageyr + race:bmxbmi +
  race:I(bmxbmi^2) + ridageyr:bmxbmi + race:ridageyr:bmxbmi

final_paper_model <- glm(final_model_formula, 
                       data = regression_data, 
                       family = "binomial",
                       weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

# Save this blessed model
saveRDS(final_paper_model, file = paste(save_path, "final_paper_model.rda", sep = ""))

names <- names(coef(final_paper_model))
coef_vals <- coef(final_paper_model)
se <- sqrt(diag(vcovHC(final_paper_model, type = "HC0")))

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
        linesep = c("", "", "", "", "", "\\addlinespace", "")) %>%
  row_spec(0,bold=TRUE)

