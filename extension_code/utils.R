library(pROC) # for computing AUC


compute_auc <- function(model, data) {
  predictions <- round(predict(model, newdata = data, type = "response") * 100, 2)
  auc <- auc(data$diabetes, predictions)
  return(auc)
}

compute_auc_by_race <- function(model, data) {
  white <- data %>% filter(race == "White American")
  black <- data %>% filter(race == "Black American")
  asian <- data %>% filter(race == "Asian American")
  hispanic <- data %>% filter(race == "Hispanic American")
  white_pred <- round(predict(model, newdata = white, type = "response") * 100, 2)
  black_pred <- round(predict(model, newdata = black, type = "response") * 100, 2)
  asian_pred <- round(predict(model, newdata = asian, type = "response") * 100, 2)
  hispanic_pred <- round(predict(model, newdata = hispanic, type = "response") * 100, 2)
  white_auc <- error_robust_auc(white$diabetes, white_pred)
  black_auc <- error_robust_auc(black$diabetes, black_pred)
  asian_auc <- error_robust_auc(asian$diabetes, asian_pred)
  hispanic_auc <- error_robust_auc(hispanic$diabetes, hispanic_pred)
  auc_by_race <- data.frame(
    white = white_auc,
    black = black_auc,
    asian = asian_auc,
    hispanic = hispanic_auc
  )
  return(auc_by_race)
}

# Need this function to proactively catch errors from the auc function.
# Errors can occur if, for a given race, there are only data points of one label.
# This can happen if the model is large and many columns must be free of missing 
# values in order to produce a prediction.
error_robust_auc <- function(labels, predictions) {
  out <- tryCatch(auc(labels, predictions), error = function(e) NA)
  return(out)
}


make_roc_data <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), scores[order(scores, decreasing=TRUE)], labels)
}

