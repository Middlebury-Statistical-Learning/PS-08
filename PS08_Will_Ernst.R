library(tidyverse)
library(broom)
import_train <- read_csv("train.csv")
import_test <- read_csv("test.csv")


#clean data
train <- import_train %>%
  select(label, numberOfLinks, commonlinkratio_1, commonlinkratio_2, commonlinkratio_3, commonlinkratio_4)

#generate model
model_formula <- as.formula(label~numberOfLinks+commonlinkratio_1+commonlinkratio_2+commonlinkratio_3+commonlinkratio_4)
model_logistic <- glm(model_formula, data=train, family="binomial")

#calculate phat
predictions <- model_logistic %>%
  augment() %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  )

#calculate p*
p_star_vector <- seq(0, 1, by = 0.01)
cost_vector <- rep(0, length(p_star_vector))

for(i in 1:length(p_star_vector)){
  cost_per_person <- predictions %>%
    mutate(
      y_hat = ifelse(p_hat > p_star_vector[i], 1, 0),
      FP = ifelse(label == 0 & y_hat == 1, 1, 0),
      FN = ifelse(label == 1 & y_hat == 0, 1, 0),
      cost = FP + FN
    ) %>%
    select(label, y_hat, cost)
  
  # Be sure to view this!
  # View(cost_per_person)
  
  cost_vector[i] <- sum(cost_per_person$cost)
}

results <- data_frame(
  p_star = p_star_vector,
  cost = cost_vector
) %>%
  mutate(avg_cost = cost/nrow(train))


results %>%
  arrange(cost) %>%
  slice(1:10)

# The optimal p* is here
optima <- results %>%
  arrange(cost) %>%
  slice(1)

#clean test data
test <- import_test %>%
  select(urlid, numberOfLinks, commonlinkratio_1, commonlinkratio_2, commonlinkratio_3, commonlinkratio_4)

#get test predictions
test_predictions <- as.vector(model_logistic %>%
  predict(newdata=test))

#calculate predicted label
test_results <- test %>%
  mutate(
    p_hat = 1/(1+exp(-test_predictions)),
    p_hat = round(p_hat, 3),
    label = ifelse(p_hat>optima$p_star, as.integer(1), as.integer(0))
  )

#generate submission
submission <- test_results %>%
  select(urlid, label) %>%
  write_csv("Stumble_Upon_submission.csv")

