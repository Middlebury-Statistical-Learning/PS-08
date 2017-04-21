library(tidyverse)
library(broom)
library(ROCR) 
library(readr)

train <- read_csv("~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/cs-training.csv") %>% 
  dplyr::rename(
    NumberOfTime30_59DaysPastDueNotWorse = `NumberOfTime30-59DaysPastDueNotWorse`,
    NumberOfTime60_89DaysPastDueNotWorse = `NumberOfTime60-89DaysPastDueNotWorse`
  ) %>% 
  na.omit()

model_formula<-train%>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("X1", "SeriousDlqin2yrs")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("SeriousDlqin2yrs ~ ", .)

model_formula <- as.formula(model_formula)
model_logistic <- glm(model_formula, data=train, family="binomial")

train_log <- train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(SeriousDlqin2yrs, p_hat)

pred_log <- prediction(predictions = train_log$p_hat, labels = train_log$SeriousDlqin2yrs)
perf_log <- performance(pred_log, "tpr","fpr")


auc_log <- as.numeric(performance(pred_log,"auc")@y.values)
auc_log


plot(perf_log, main=paste("Area Under the Curve =", round(auc_log, 3)))
abline(c(0, 1), lty=2)

test <- read_csv("~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/cs-test.csv") %>% 
  dplyr::rename(
    NumberOfTime30_59DaysPastDueNotWorse = `NumberOfTime30-59DaysPastDueNotWorse`,
    NumberOfTime60_89DaysPastDueNotWorse = `NumberOfTime60-89DaysPastDueNotWorse`
  ) %>% 
  select(X1, RevolvingUtilizationOfUnsecuredLines:NumberOfDependents)

predictions<-model_logistic %>%
  augment(newdata=test) %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3))

predictions<-predictions %>% 
  rename(Id=X1, Probability=p_hat) %>% 
  select(Id,Probability) %>% 
  mutate(Probability=ifelse(is.na(Probability)==TRUE,mean(Probability, na.rm=TRUE),Probability)) %>% 
  write_csv("PS08_predictions.csv")




