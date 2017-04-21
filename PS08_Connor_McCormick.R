#Credit card kaggle competition


library(tidyverse)
library(broom)
library(plotly)
library(glmnet)

train <- read_csv("cs-training.csv")
test <- read_csv("cs-test.csv")
sample <- read_csv("sampleEntry.csv")

train <- train %>%
  tbl_df() %>%
  # Add ID column:
  rename(
    ID = X1,
    veryverypastdue = `NumberOfTimes90DaysLate`,
    pastdue = `NumberOfTime30-59DaysPastDueNotWorse`,
    verypastdue = `NumberOfTime60-89DaysPastDueNotWorse`,
    superlate = `SeriousDlqin2yrs`)

#filling in the N/As
training <- train %>%
  mutate(MonthlyIncome = ifelse(is.na(MonthlyIncome), 0, MonthlyIncome)) %>%
  mutate(NumberOfDependents = ifelse(is.na(NumberOfDependents), 0, NumberOfDependents)) 

#makeing the formulat
model_formula <- training %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("ID", "fold", "superlate")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("superlate ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

#running an lm

model_lm <- lm(model_formula, data=training)



#testing

testing <- test %>%
  tbl_df() %>%
  # Add ID column:
  rename(
    ID = X1,
    veryverypastdue = `NumberOfTimes90DaysLate`,
    pastdue = `NumberOfTime30-59DaysPastDueNotWorse`,
    verypastdue = `NumberOfTime60-89DaysPastDueNotWorse`,
    superlate = `SeriousDlqin2yrs`)

#filling in the N/As
tested <- testing %>%
  mutate(MonthlyIncome = ifelse(is.na(MonthlyIncome), 0, MonthlyIncome)) %>%
  mutate(NumberOfDependents = ifelse(is.na(NumberOfDependents), 0, NumberOfDependents)) 

#using test data
predictions <- model_lm %>% 
  predict(newdata=tested)

#submission
sample <- sample%>% 
  mutate(Probability = as.vector(predictions)) %>% 
  mutate(Probability = ifelse(Probability < 0, 0, Probability)) %>%
  mutate(Probability = ifelse(Probability > 1, 1, Probability)) %>%
  write_csv("PS08_Connor_McCormick_submission.csv")

#my score was 0.694739, which was in the top 50%



