library(tidyverse)

model_path <- "/home/mcoots/harvard/research/race-in-healthcare/models/final_paper_model.rda"

final_model <- readRDS(model_path)

bmi_vector <- c(c(25:19), 18.5)
age_vector <- rep(35, length(bmi_vector))
white_vector <- rep("White American", length(bmi_vector))
asian_vector <- rep("Asian American", length(bmi_vector))
black_vector <- rep("Black American", length(bmi_vector))
hispanic_vector <- rep("Hispanic American", length(bmi_vector))

table_2_data <- data.frame(race = c(white_vector, asian_vector, black_vector, hispanic_vector),
                           ridageyr = rep(age_vector, 4),
                           bmxbmi = rep(bmi_vector, 4))

table_2_pred <- round(predict(final_model, newdata = table_2_data, type = "response") * 100, 2)

white_pred <- table_2_pred[1:8]
asian_pred <- table_2_pred[9:16]
black_pred <- table_2_pred[17:24]
hispanic_pred <- table_2_pred[25:32]


table_2_replicated <- data.frame(bmi_vector,
                                 White_Americans = white_pred,
                                 Asian_Americans = asian_pred,
                                 Black_Americans = black_pred,
                                 Hispanic_Americans = hispanic_pred)

table_2_replicated %>%
  `rownames<-`(c()) %>%
  kable("latex",
        col.names = c("BMI (kg/m^2)", "White American", "Asian American", "Black American", "Hispanic American"),
        digits = 2,
        vline = "",
        booktabs = T,
        linesep = c("")) %>%
  row_spec(0,bold=TRUE) %>%
  add_header_above(c(" " = 1, "Predicted Prevalence (%)" = 4))


