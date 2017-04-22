# Packages
library(dplyr)
library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(ggplot2)
library(ROCR)

# READ CSV'S FROM KAGGLE
train <- read_csv("cs-training.csv")
test <- read_csv("cs-test.csv")

# REPLACE NA VALUES WITH AVG OF COLUMN
train <- train %>% 
  mutate(MonthlyIncome = ifelse(is.na(MonthlyIncome), mean(MonthlyIncome, na.rm=TRUE), MonthlyIncome)) %>% 
  mutate(NumberOfDependents = ifelse(is.na(NumberOfDependents), as.integer(mean(NumberOfDependents, na.rm=TRUE)), NumberOfDependents))

# CREATE MODEL FORMULA
model_formula <- as.formula(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age + `NumberOfTime30-59DaysPastDueNotWorse` + DebtRatio + MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + NumberRealEstateLoansOrLines + `NumberOfTime60-89DaysPastDueNotWorse` + NumberOfDependents)

# CREATE MODEL BASED ON TRAIN DATA
model_logistic <- glm(model_formula, data=train, family="binomial")

# PREDICT FOR TRAIN (TO CHECK FOR OPTIMAL P*)
train <- train %>% 
  mutate(p_delq_guess = round(predict(model_logistic, type="response"), 3))

# ROC GRAPH FOR TRAIN SET
pred <- prediction(predictions = train$p_delq_guess, labels = train$SeriousDlqin2yrs)
perf <- performance(pred, "tpr","fpr")
auc <- as.numeric(performance(pred,"auc")@y.values)
auc
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)





##### Finding p* ##### - THIS WASN'T ACTUALLY NECESSARY,
# SUBMISSION ONLY REQUIRES PROBABILITIES, NOT BINARY PREDICTIONS

# set min error as highest possible so comparatively the actual min error will be
# lower
min_error <- Inf

# list of p* values to test
p_star_vals <-  seq(0.001, 0.999, 0.001)

# set optimal p_star as 0 (a value that will isn't even hit)
p_star <- 0

# test all possible p* values
for (i in p_star_vals){
  # mutate using probabilities > p*
  train <- train %>% 
    mutate(delq=ifelse(p_delq_guess > i, 1, 0))
  
  # create contingency table
  table <- train %>% 
    select(SeriousDlqin2yrs, delq) %>% 
    group_by(SeriousDlqin2yrs, delq) %>% 
    summarise(num = n())
  
  # calculate false neg/false pos
  false_neg <- table$num[3]/(table$num[3] + table$num[4])
  false_pos <- table$num[2]/(table$num[1] + table$num[2])
  
  if((false_neg < false_pos) & ((table$num[2] + table$num[3])/sum(table$num)) < min_error){
    min_error = false_neg + false_pos
    p_star = i
  }
}

# find optimal p*
p_star

##### FOR FINAL SUBMISSION #####
test <- test %>% 
  # REMOVE NA
  mutate(MonthlyIncome = ifelse(is.na(MonthlyIncome), mean(MonthlyIncome, na.rm=TRUE), MonthlyIncome)) %>% 
  mutate(NumberOfDependents = ifelse(is.na(NumberOfDependents), as.integer(mean(NumberOfDependents, na.rm=TRUE)), NumberOfDependents)) %>% 
  # PREDICT PROBABILITY
  mutate(Probability = round(predict(model_logistic, test, type="response"), 3)) %>% 
  rename(Id = X1) %>% 
  # SELECT APPROPRIATE COLUMNS
  select(Id, Probability)


# WRITE CSV
write.csv(test, "submission.csv",quote = FALSE, row.names=FALSE)

# KAGGLE SCORE -
# My score on Kaggle was 0.703163
