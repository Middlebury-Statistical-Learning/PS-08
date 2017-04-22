library(tidyverse)
library(broom)
library(ROCR)
library(plotly)
library(glmnet)
library(stringr)


cs_training <- cs_training %>%
  tbl_df() %>%
  na.omit() %>%
  mutate(p_hat = 1)
View(cs_training)

# Fit model:
model_formula <- as.formula(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+age+DebtRatio+MonthlyIncome+NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+NumberOfDependents)
model_logistic <- glm(model_formula, data=cs_training, family="binomial")

# Get predictions:
predictions <- model_logistic %>%
  augment() %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  ) %>%
  select(SeriousDlqin2yrs,RevolvingUtilizationOfUnsecuredLines,age,DebtRatio,MonthlyIncome,NumberOfOpenCreditLinesAndLoans,NumberOfTimes90DaysLate,NumberOfDependents,p_hat)

View(predictions)


pred <- prediction(predictions = predictions$p_hat, labels = cs_training$SeriousDlqin2yrs)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)), col="red", lwd=2)
abline(c(0, 1), lty=2)

#ROC: .649


test_p_hat_predictions <- predict(model_logistic, cs_test, type="response")

test_p_hat_predictions <- test_p_hat_predictions %>%
  tbl_df() %>%
  mutate(ID = 1:n()) 

test_p_hat_predictions_clean <- test_p_hat_predictions %>%
  na.omit()

View(test_p_hat_predictions)

test_p_hat_predictions

mean(test_p_hat_predictions_clean$value)

test_p_hat_predictions %>% 
  write_csv("~/Desktop/PS08_Alfred_Hurley_submission.csv")








