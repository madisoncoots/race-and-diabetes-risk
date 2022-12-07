library(ggplot2)

roc_read_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/"

social_determinants_path <- paste(roc_read_path, "social_determinants_roc.csv", sep = "")
biological_determinants_path <- paste(roc_read_path, "biological_determinants_model_roc.csv", sep = "")
race_only_path <- paste(roc_read_path, "race_only_roc.csv", sep = "")
full_paper_path <- paste(roc_read_path, "full_paper_model_roc.csv", sep = "")
age_and_bmi_path <- paste(roc_read_path, "age_and_bmi_model_roc.csv", sep = "")
social_determinants_with_race_path <- paste(roc_read_path, "social_determinants_with_race_roc.csv", sep = "")
stepwise_bio_determinants_path <- paste(roc_read_path, "stepwise_biological_determinants_model_roc.csv", sep = "")

social_determinants_roc <- read_csv(social_determinants_path)
biological_determinants_roc <- read_csv(biological_determinants_path)
race_only_roc <- read_csv(race_only_path)
full_paper_model_roc <- read_csv(full_paper_path)
age_and_bmi_roc <- read_csv(age_and_bmi_path)
social_determinants_with_race_roc <- read_csv(social_determinants_with_race_path)
stepwise_bio_determinants_roc <- read_csv(stepwise_bio_determinants_path)

ggplot() +
  geom_abline(slope = 1, linetype = "dashed", ) +
  geom_line(data = social_determinants_roc, aes(x = FPR, y = TPR, color = "Social determinants model")) +
  geom_line(data = race_only_roc, aes(x = FPR, y = TPR, color = "Race only model")) +
  geom_line(data = full_paper_model_roc, aes(x = FPR, y = TPR, color = "Aggarwal et al. model")) +
  geom_line(data = biological_determinants_roc, aes(x = FPR, y = TPR, color = "Biological determinants model")) +
  geom_line(data = age_and_bmi_roc, aes(x = FPR, y = TPR, color = "Age and BMI model")) +
  # geom_line(data = social_determinants_with_race_roc, aes(x = FPR, y = TPR, color = "Social determinants with race model")) +
  # geom_line(data = stepwise_bio_determinants_roc, aes(x = FPR, y = TPR, color = "Stepwise biological determinants model")) +
  theme_minimal() +
  xlab("False positive rate") +
  ylab("True positive rate") + 
  scale_color_discrete(name="") +
  theme(legend.position = c(0.70, 0.25))
