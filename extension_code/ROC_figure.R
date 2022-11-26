library(ggplot2)

social_determinants_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/social_determinants_roc.csv"
biological_determinants_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/biological_determinants_model_roc.csv"
race_only_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/race_only_roc.csv"
full_paper_path <- "/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/model_roc_data/full_paper_model_roc.csv"

social_determinants_roc <- read_csv(social_determinants_path)
biological_determinants_roc <- read_csv(biological_determinants_path)
race_only_roc <- read_csv(race_only_path)
full_paper_model_roc <- read_csv(full_paper_path)

ggplot() +
  geom_abline(slope = 1, linetype = "dashed", ) +
  geom_line(data = social_determinants_roc, aes(x = FPR, y = TPR, color = "Social determinants model")) +
  geom_line(data = race_only_roc, aes(x = FPR, y = TPR, color = "Race only model")) +
  geom_line(data = full_paper_model_roc, aes(x = FPR, y = TPR, color = "Full paper model")) +
  geom_line(data = biological_determinants_roc, aes(x = FPR, y = TPR, color = "Biological determinants model")) +
  theme_minimal()
