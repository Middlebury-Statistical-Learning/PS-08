library(tidyverse)
library(broom)
library(ROCR)

#This competition requires participants to improve on the state of the art in credit scoring,
#by predicting the probability that somebody will experience financial distress in the next two years.
#so have to see which vars effect the outcome the most to recommend model that predicts credit default the best

#purpose of assignment is to get largest ROC area. 
setwd('/Users/BiancaGonzalez/Desktop/RudeBoyMachineLearning/hw08')

train <- read.csv("cs-training.csv")
test <- read.csv("cs-test.csv")
sampleentry<- read.csv("sampleEntry.csv")

# formula on olny some components

# Training Logistic Model (models the probability that Y belongs to a particular category, ie Yes/no) 
  #predicting default on as formula 
credit_train <- train 
credit_test <- test


# Fit Models --------------------------------------------------------------
# Let's apply the same model formula for a couple of models. We can create a
# "formula" object for repeated use.
# MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberRealEstateLoansOrLines
model_formula <- 
  "SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + NumberOfTimes90DaysLate + NumberOfOpenCreditLinesAndLoans + NumberRealEstateLoansOrLines" %>%
  as.formula()

# Train logistic regression. glm command for logistic
model_logistic <- glm(model_formula, data=credit_train, family="binomial")

predict(model_logistic, newdata = credit_test, type="response")[1:10] %>% round(3)


# ROC Curve --------------------------------------------------------------------
# Let's use the ROCR package to create our ROC curves. The data is unfortunately
# not in tidy format, so we can't plot using ggplot() or use tidyverse tools:

credit_train <- credit_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(X, SeriousDlqin2yrs, p_hat)


# This bit of code computes the ROC curve
pred <- prediction(predictions = credit_train$p_hat, labels = credit_train$SeriousDlqin2yrs)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

# Could probably fit a ridge or lasso but we will leave it here for now. 
