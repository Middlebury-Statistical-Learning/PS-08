library(tidyverse)
library(broom)
library(tidyr)
library(dplyr)

train <- readr::read_csv("/Users/Tina/Desktop/MATH218/cs-training.csv") %>% 
  tbl_df() %>%
  mutate(Id = X1)
test <- readr::read_csv("/Users/Tina/Desktop/MATH218/cs-test.csv")
submission <- readr::read_csv("/Users/Tina/Desktop/MATH218/sampleEntry.csv")

credit_train <- train %>%
  sample_frac(101503/150000)

credit_test <- train %>%
  anti_join(credit_train, by="Id")

creditlogit <- glm(SeriousDlqin2yrs ~ 
                              RevolvingUtilizationOfUnsecuredLines +
                              age +
                              `NumberOfTime30-59DaysPastDueNotWorse` +
                              DebtRatio +                   
                              NumberOfOpenCreditLinesAndLoans +
                              NumberOfTimes90DaysLate +
                              #MonthlyIncome+ not fitted p
                              NumberRealEstateLoansOrLines +
                              `NumberOfTime60-89DaysPastDueNotWorse` +
                              NumberOfDependents 
                     , data = credit_train, family = "binomial")

predict(creditlogit, newdata = credit_test)[1:101503] %>% round(3) %>% 
  tbl_df()

p_hat <- 1/(1+exp(-predict(creditlogit, newdata = credit_test)[1:101503]))
p_hat <- round(p_hat, 3)
p_hat

creditlogit %>%
  augment(newdata=credit_test) %>%
  tbl_df()

# Again we can convert the fitted values to probabilities p_hat by using the
# inverse logit function:
creditlogit %>%
  augment(newdata=credit_test) %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  )

submission <- credit_train %>% 
  mutate(
    Probability = p_hat) %>% 
  select(Probability)

submission$Id = rownames(submission)
submission <- submission[c("Id", "Probability")]

View(submission)

write.csv(submission, file = "submission.csv",row.names=FALSE, na = "0")

#Your submission scored 0.504334