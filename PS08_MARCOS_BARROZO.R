library(tidyverse)
library(broom)
library(ROCR)

setwd("~/PS08")
# Read train and test data
train <- read_csv("cs-training.csv")
test <- read_csv("cs-test.csv")
#Change some names and drop na's
train <- train %>%
  na.omit() %>%
dplyr::rename(
  seriousdlq = `SeriousDlqin2yrs`,
 numberoftimeonemonth = `NumberOfTime30-59DaysPastDueNotWorse`,
  numberoftimelate = `NumberOfTimes90DaysLate`,
  numberoftimetwomonth = `NumberOfTime60-89DaysPastDueNotWorse`
)

test <- test %>%
  dplyr::rename(
    numberoftimeonemonth = `NumberOfTime30-59DaysPastDueNotWorse`,
    numberoftimelate = `NumberOfTimes90DaysLate`,
    numberoftimetwomonth = `NumberOfTime60-89DaysPastDueNotWorse`
  )

# Fit model:
model_formula <- as.formula(seriousdlq~RevolvingUtilizationOfUnsecuredLines+age +
                            numberoftimeonemonth + DebtRatio +
                              MonthlyIncome + NumberOfOpenCreditLinesAndLoans +
                              numberoftimelate + NumberRealEstateLoansOrLines +
                             numberoftimetwomonth + NumberOfDependents)

model_logistic <- glm(model_formula, data=train, family="binomial")

# Get predictions:
predictions <- model_logistic %>%
  augment() %>%
  tbl_df() %>%
  mutate(
    # Convert from log-odds space (-infty, infty) to probability space (0, 1):
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  )


############################
#COMPUTING AN ROC CURVE
############################
# This bit of code computes the ROC curve
pred <- prediction(predictions = predictions$p_hat, labels = predictions$seriousdlq)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)), col="red", lwd=2)
abline(c(0, 1), lty=2)


####################################################
####### MAKING PREDICTIONS WITH A P* OF 0.5 ########
####################################################

test_predictions <- model_logistic %>% 
  predict(newdata=test)
test_predictions <- predict(model_logistic,test, type="response")


# If p less than .5 or it is NA, give it a 0. I could deal with NAs later but I'm in a thesis time crunch.
test <- test %>% 
  mutate(p_hat = test_predictions) %>%
  mutate(SeriousDlqin2yrs = ifelse(p_hat < .5 | is.na(p_hat) == TRUE,0,1))


# Make kaggle submission
test <- test %>%
  mutate(p_hat = ifelse(is.na(p_hat)== TRUE, 0,p_hat)) %>%
  select(X1, p_hat) %>%
  rename(Id = `X1`, probability = `p_hat`) %>%
  write.csv(file = "barrozo_submission.csv")
