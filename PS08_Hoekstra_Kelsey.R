# Setup -------------------------------------------------------------------
library(tidyverse)
library(broom)


credit<- read_csv("~/Google Drive/Math/homework/cs-training.csv") 

 
model_formula <-
  "SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age  + DebtRatio + MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + NumberRealEstateLoansOrLines " %>%
  as.formula()

model_logistic <- glm(model_formula, data=credit, family="binomial")

credit_test<- read_csv("~/Google Drive/Math/homework/cs-test.csv")

#predict for credit_test
model_logistic %>%
  augment(newdata=credit_test) %>%
  tbl_df()

# Again we can convert the fitted values to probabilities p_hat by using the
# inverse logit function:
credit_predict<-model_logistic %>%
  augment(newdata=credit_test) %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3),
    #there's probably a better way to handle NAs but i didn't have time 
    p_hat = ifelse(is.na(p_hat), 0.5, p_hat )
  )




credit_predict %>% 
  mutate(Probability = as.vector(p_hat)) %>% 
  rename(ID = X1) %>%
  select(ID, Probability) %>%
  write_csv("~/Desktop/PS08_Kelsey_Hoekstra_submission.csv")
#i scored 0.58 