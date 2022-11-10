library(survey)
library(tidyverse)

model_path <- "/home/mcoots/harvard/research/race-in-healthcare/models/paper_final_model.rda"

final_model <- readRDS(model_path)

bmi_vector <- c(c(25:19), 18.5)
age_vector <- rep(35, length(age_vector))
white_vector <- rep("White American", length(age_vector))
asian_vector <- rep("Asian American", length(age_vector))
black_vector <- rep("Black American", length(age_vector))
hispanic_vector <- rep("Hispanic American", length(age_vector))

table_2_data <- data.frame(race = c(white_vector, asian_vector, black_vector, hispanic_vector),
                           ridageyr = rep(age_vector, 4),
                           bmxbmi = rep(bmi_vector, 4))

table_2_pred <- predict(final_model, newdata = table_2_data, type = "response") * 100

white_pred <- table_2_pred[1:8]
asian_pred <- table_2_pred[9:16]
black_pred <- table_2_pred[17:24]
hispanic_pred <- table_2_pred[25:32]

table_2_replicated <- data.frame(White_Americans = white_pred,
                                 Asian_Americans = asian_pred,
                                 Black_Americans = black_pred,
                                 Hispanic_Americans = hispanic_pred) %>%
  `rownames<-`(bmi_vector)

table_2_replicated

