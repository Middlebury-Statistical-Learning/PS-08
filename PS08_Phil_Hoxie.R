setwd("~/Machine Learning/HW_08")

library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(stringr)
library(readr)
library(class)

## Data  -----------------------------------------------

test <- read_csv("~/Machine Learning/HW_08/cs-test.csv") 
test <- test %>%   
  rename(ID = X1,
         dlqin2 = SeriousDlqin2yrs,
         revolve_lines = RevolvingUtilizationOfUnsecuredLines,
         past_due_59 = `NumberOfTime30-59DaysPastDueNotWorse`,
         credit_lines = NumberOfOpenCreditLinesAndLoans,
         days_late_90 = NumberOfTimes90DaysLate,
         real_estate = NumberRealEstateLoansOrLines,
         past_due_89 = `NumberOfTime60-89DaysPastDueNotWorse`,
         dependents = NumberOfDependents,
         debt_ratio = DebtRatio,
         income = MonthlyIncome)

train <- read_csv("~/Machine Learning/HW_08/cs-training.csv") 

train <- train %>% 
  rename(ID = X1,
         dlqin2 = SeriousDlqin2yrs,
         revolve_lines = RevolvingUtilizationOfUnsecuredLines,
         past_due_59 = `NumberOfTime30-59DaysPastDueNotWorse`,
         credit_lines = NumberOfOpenCreditLinesAndLoans,
         days_late_90 = NumberOfTimes90DaysLate,
         real_estate = NumberRealEstateLoansOrLines,
         past_due_89 = `NumberOfTime60-89DaysPastDueNotWorse`,
         dependents = NumberOfDependents,
         debt_ratio = DebtRatio,
         income = MonthlyIncome)

sample_entry <- read_csv("~/Machine Learning/HW_08/sampleEntry.csv")

# Let's run a dumb model and then make some charts to scope things out

names(train)

dumb_model <- glm(data = train, dlqin2 ~ 
                    revolve_lines +
                    age + 
                    past_due_59 + 
                    debt_ratio + 
                    income + 
                    credit_lines + 
                    days_late_90 + 
                    real_estate + 
                    past_due_89 + 
                    dependents)
summary(dumb_model)

summary(glm(data = train, dlqin2 ~ revolve_lines))
summary(glm(data = train, dlqin2 ~ debt_ratio))

## Debt ratio is significant on its own, revolving lines isn't

## Try KNN for multiple dims (Should work in theory)

neighbors <- 50

## I should use CV to find best K, but nah

classifications <- train$dlqin2

train_input <- train %>% 
  select(age, past_due_59, income, credit_lines, 
         days_late_90, real_estate, past_due_89,
         dependents)
test_input <- test %>% 
  select(age, past_due_59, income, credit_lines, 
         days_late_90, real_estate, past_due_89,
         dependents)

dim(test_input)
dim(train_input)

## Fix: Test: Income, dependents
##      Train: income, dependents

# Replace income with average income,
# replace dependents with 0

train_input <- train_input %>% 
  mutate(income = ifelse(is.na(income), mean(income, na.rm = TRUE), income),
         dependents = ifelse(is.na(dependents), 0, dependents))

test_input <- test_input %>% 
  mutate(income = ifelse(is.na(income), mean(income, na.rm = TRUE), income),
         dependents = ifelse(is.na(dependents), 0, dependents))

summary(test_input$income)
summary(test_input$dependents)
summary(train_input$income)
summary(train_input$dependents)

## KNN

model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = neighbors, prob=TRUE)

# Add predictions and probabilities to test set
entry <- test %>%
  mutate(
    Species = model_knn,
    Probability = attr(model_knn, "prob")
  )

## Make submission

entry <- entry %>% 
  rename(Id = ID)

entry <- entry %>% 
  select(Id, Probability)

summary(entry$Probability)
##note: may need to delete extra column
write.csv(entry, file = "PS08_Phil_Hoxie_Submissions.csv")
