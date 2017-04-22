# load in libraries
library(tidyverse)
library(broom)
library(splines)
library(plotly)
library(glmnet)
library(ROCR)
library(okcupiddata)

# load in data, rename predictors, remove missing data
train <- read.csv(url("https://kaggle2.blob.core.windows.net/competitions-data/kaggle/2551/cs-training.csv?sv=2015-12-11&sr=b&sig=A7SEwjZAHboKS0WRhc3gNmMtwGq%2BVivV6U3yGNopvkM%3D&se=2017-04-24T17%3A11%3A32Z&sp=r")) %>%
  dplyr::rename(
    Dlq = `SeriousDlqin2yrs`,
    UnsecuredLines = `RevolvingUtilizationOfUnsecuredLines`,
    MonthPastDue = `NumberOfTime30.59DaysPastDueNotWorse`,
    OpenCreditLines = `NumberOfOpenCreditLinesAndLoans`,
    ThreeMonthsLate = `NumberOfTimes90DaysLate`,
    RELoans = `NumberRealEstateLoansOrLines`,
    TwoMonthsPastDue = `NumberOfTime60.89DaysPastDueNotWorse`,
    Dependents = `NumberOfDependents`
  )
train <- na.omit(train)

# create a model
model <- as.formula(Dlq ~ UnsecuredLines + age + MonthPastDue + DebtRatio + MonthlyIncome + OpenCreditLines + ThreeMonthsLate + RELoans + TwoMonthsPastDue + Dependents)

# check shrinkage to see if there are irrelevant predictors
n_folds <- 10
lambda_values <- 10^seq(from = 1, to = 30, length = 1000)

X <- model.matrix(model, data = train)[, -1]
y <- train$Dlq

cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda = lambda_values, nfolds = n_folds)
cv_ridge_tidy <- cv_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda = log(lambda)) 

# obtain the optimal lambda
lambda_star_ridge <- cv_ridge$lambda.min 

# compute coefficients for each value of lambda
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)
coefficients <- model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda = log(lambda)) %>%
  filter(term != "(Intercept)")

# plot coefficients for each value of lambda, with optimal lambda marked
ggplot(coefficients, aes(x = log_lambda, y = estimate, col = term)) +
  geom_line() +
  geom_vline(xintercept = log(lambda_star_ridge)) +
  labs(x = "log(lambda)", y = "Coefficient Estimate")

# compare ridge() to lm()
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

results_all_folds <- NULL

for(i in 1:n_folds) {
  # create pseudo train / test sets
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  
  pseudo_train_X <- model.matrix(model, data = pseudo_train)[, -1]
  pseudo_train_y <- pseudo_train$Dlq
  pseudo_test_X <- model.matrix(model, data = pseudo_test)[, -1]
  
  # fit lm(), loess(), and ridge models
  model_lm <- lm(model, data = pseudo_train)
  model_ridge <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 0, lambda = lambda_star_ridge)
  
  # predict using lm() and ridge models
  lm_predictions <- model_lm %>% 
    predict(newdata = pseudo_test)
  ridge_predictions <- model_ridge %>% 
    predict(newx = pseudo_test_X, s = lambda_star_ridge)
  
  results_all_folds <- data_frame(
      Dlq = pseudo_test$Dlq,
      lm = as.vector(lm_predictions),
      ridge = as.vector(ridge_predictions)
    ) %>% 
    tidyr::gather(method, yhat, -Dlq) %>% 
    group_by(method) %>% 
    summarise(MSE = mean((Dlq - yhat)^2)) %>% 
    mutate(fold = i) %>% 
    bind_rows(results_all_folds)
}

# show results
results_all_folds %>% 
  group_by(method) %>% 
  summarise(MSE_CV = mean(MSE)) %>% 
  arrange(desc(MSE_CV))

# convert prediction to (0, 1)
predictions_lm <- model_ridge %>%
  augment() %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  ) %>%
  select(Dlq, p_hat)

pred <- prediction(predictions_lm$p_hat, labels = predictions_lm$Dlq)
perf <- performance(pred, "tpr","fpr")

auc <- as.numeric(performance(pred,"auc")@y.values)
  
p_star_vector <- seq(0, 1, by = 0.01)
cost_vector <- rep(0, length(p_star_vector))

for(i in 1:length(p_star_vector)){
  cost_per_person <- predictions_lm %>%
    mutate(
      y_hat = ifelse(p_hat > p_star_vector[i], 1, 0),
      FP = ifelse(Dlq == 0 & y_hat == 1, 1, 0),
      FN = ifelse(Dlq == 1 & y_hat == 0, 1, 0),
      cost = FP + FN
    ) %>%
    select(Dlq, y_hat, cost)
  
  cost_vector[i] <- sum(cost_per_person$cost)
}

# obtain p*
results <- data_frame(
  p_star = p_star_vector,
  cost = cost_vector)

results %>%
  arrange(cost) 

optima <- results %>%
  arrange(cost) %>%
  slice(1)

# generate predictions on test data
sample_submission <- read.csv(url("https://kaggle2.blob.core.windows.net/competitions-data/kaggle/2551/sampleEntry.csv?sv=2015-12-11&sr=b&sig=z3X87wYfRAZfBgir8CfahB50EztE5Jb9nGL8gkK13DY%3D&se=2017-04-25T00%3A02%3A27Z&sp=r"))

test <- read.csv(url("https://kaggle2.blob.core.windows.net/competitions-data/kaggle/2551/cs-test.csv?sv=2015-12-11&sr=b&sig=sUTZVHUkwljmfCqQ6D27JahSaddbaF%2BJz%2FxTT4alsZw%3D&se=2017-04-24T17%3A10%3A50Z&sp=r")) %>%
  dplyr::rename(
    Dlq = `SeriousDlqin2yrs`,
    UnsecuredLines = `RevolvingUtilizationOfUnsecuredLines`,
    MonthPastDue = `NumberOfTime30.59DaysPastDueNotWorse`,
    OpenCreditLines = `NumberOfOpenCreditLinesAndLoans`,
    ThreeMonthsLate = `NumberOfTimes90DaysLate`,
    RELoans = `NumberRealEstateLoansOrLines`,
    TwoMonthsPastDue = `NumberOfTime60.89DaysPastDueNotWorse`,
    Dependents = `NumberOfDependents`
  )

predictions <- predict(model_lm, newdata = test) %>% 
  as.vector()
predictions[is.na(predictions)] <- 0
predictions[predictions < 0] <- 0

sample_submission %>% 
  mutate(Probability = as.vector(predictions)) %>% 
  write_csv("~/Desktop/PS06_Jewel_Chen_submission.csv")
