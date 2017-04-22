library(tidyverse)
library(broom)
library(ROCR)

#Loading Data:
loan_train <- read.csv2("/Users/dvalentin/Documents/Code/CS_Homework/MATH218_Stat_Learning/PS_08/cs-training.csv", sep=",", header = TRUE)

loan_test <- read.csv2("/Users/dvalentin/Documents/Code/CS_Homework/MATH218_Stat_Learning/PS_08/cs-test.csv", sep=",", header = TRUE)

# Data cleaning -----------------------------------------------------------
loan_train <- loan_train %>% 
  na.omit() %>% 
  tbl_df()

loan_test <- loan_test %>% 
  na.omit() %>% 
  tbl_df()
  
# p_hats for Random and Perfect Guesses -------------------------------------
loan_train <- loan_train %>%
  mutate(
    # runif() generates n uniform random values betweeen (min, max):
    p_hat_perfect = SeriousDlqin2yrs
  )

#run the logistic regression on these variables
model_logit <- glm(formula = SeriousDlqin2yrs ~ DebtRatio + MonthlyIncome + NumberOfTime60.89DaysPastDueNotWorse, family = binomial, data=loan_train)

# ROC for Random Guesses ----------------------------------------------------
# This bit of code computes the ROC curve
pred <- predict(am.glm, newdata, type="response") 
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)), col="red", lwd=2)
abline(c(0, 1), lty=2)



# ROC for Perfect Guesses ----------------------------------------------------
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles$p_hat_perfect, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)), col="red", lwd=2)
abline(c(0, 1), lty=2)
