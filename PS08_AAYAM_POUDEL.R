library(tidyverse)
library(broom)
library(ROCR)
library(okcupiddata)

#Set as working directory
setwd("~/Desktop/Junior Spring/Statistical Learning/Homework/HW8")


train <- read_csv("cs-training.csv")
test <- read_csv("cs-test.csv")
sample_submission <- read_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW8/sampleEntry.csv")



# Data cleaning -----------------------------------------------------------
train <- train %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=SeriousDlqin2yrs, ID = X1) %>%
  select(y,ID, RevolvingUtilizationOfUnsecuredLines, age, DebtRatio, MonthlyIncome, NumberOfOpenCreditLinesAndLoans,NumberOfTimes90DaysLate, NumberRealEstateLoansOrLines, NumberOfDependents) %>%
  # Remove all rows with NA missing values:
  na.omit()


# Data cleaning -----------------------------------------------------------
test <- test %>%
  mutate(ID = X1) %>% 
  select(ID, RevolvingUtilizationOfUnsecuredLines, age, DebtRatio, MonthlyIncome, NumberOfOpenCreditLinesAndLoans,NumberOfTimes90DaysLate, NumberRealEstateLoansOrLines, NumberOfDependents)


# Fit model using subset selection:
model_formula <- as.formula(y~RevolvingUtilizationOfUnsecuredLines+age+DebtRatio+MonthlyIncome+NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfDependents)
model_logistic <- glm(model_formula, data=train, family="binomial")

# Get predictions:
predictions <- model_logistic %>%
  augment(newdata = test) %>%
  tbl_df() %>%
  mutate(
    # Convert from log-odds space (-infty, infty) to probability space (0, 1):
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3),
    p_hat = ifelse(is.na(p_hat),0.5,p_hat)
  ) %>%
  mutate(Probability = p_hat, Id = ID) %>%
  select(Id, Probability )

predictions %>% readr::write_csv("submission.csv")
