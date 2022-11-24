library(tidyverse)
library(kableExtra)

model_path <- "/home/mcoots/harvard/research/race-in-healthcare/models/final_paper_model.rda"

final_model <- readRDS(model_path)

age_vector <- c(40:20)
bmi_vector <- rep(25, length(age_vector))
white_vector <- rep("White American", length(age_vector))
asian_vector <- rep("Asian American", length(age_vector))
black_vector <- rep("Black American", length(age_vector))
hispanic_vector <- rep("Hispanic American", length(age_vector))

table_3_data <- data.frame(race = c(white_vector, asian_vector, black_vector, hispanic_vector),
                           ridageyr = rep(age_vector, 4),
                           bmxbmi = rep(bmi_vector, 4))

table_3_pred <- round(predict(final_model, newdata = table_3_data, type = "response") * 100, 2)

white_pred <- table_3_pred[1:21]
asian_pred <- table_3_pred[22:42]
black_pred <- table_3_pred[43:63]
hispanic_pred <- table_3_pred[64:84]

table_3_replicated <- data.frame(age_vector,
                                 White_Americans = white_pred,
                                 Asian_Americans = asian_pred,
                                 Black_Americans = black_pred,
                                 Hispanic_Americans = hispanic_pred)

table_3_replicated %>%
  `rownames<-`(c()) %>%
  kable("latex",
        col.names = c("Age (years)", "White American", "Asian American", "Black American", "Hispanic American"),
        digits = 2,
        vline = "",
        booktabs = T,
        linesep = c("")) %>%
  add_header_above(c(" " = 1, "Predicted Prevalence (%)" = 4)) %>%
  row_spec(0,bold=TRUE)
  

