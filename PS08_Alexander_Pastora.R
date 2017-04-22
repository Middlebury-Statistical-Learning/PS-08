library(tidyverse)
library(ROCR)

training <- read.csv("cs-training.csv")
test <- read.csv("cs-test.csv")

# EDA
# Age is a really shitty variable
ggplot(training,aes(x=training$age,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Good variable
ggplot(training,aes(x=training$RevolvingUtilizationOfUnsecuredLines,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Very Good Variable
ggplot(training,aes(x=training$NumberOfTime30.59DaysPastDueNotWorse,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Good Variable
ggplot(training,aes(x=training$DebtRatio,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Looks Good, but when you take the log of Monthly Income, there is a lot of overlap.
ggplot(training,aes(x=log(training$MonthlyIncome),y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Awful Variable!
ggplot(training,aes(x=training$NumberOfOpenCreditLinesAndLoans,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Very Good
ggplot(training,aes(x=training$NumberOfTimes90DaysLate,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Nope, pretty sucky!
ggplot(training,aes(x=training$NumberRealEstateLoansOrLines,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Very Good, No Overlap
ggplot(training,aes(x=training$NumberOfTime60.89DaysPastDueNotWorse,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Nope
ggplot(training,aes(x=training$NumberOfDependents,y=training$SeriousDlqin2yrs, group=training$SeriousDlqin2yrs))+ 
  geom_boxplot()

# Training the data
model_logistic <- glm(SeriousDlqin2yrs~NumberOfTime30.59DaysPastDueNotWorse + 
                        NumberOfTimes90DaysLate +
                        NumberOfTime60.89DaysPastDueNotWorse+
                        RevolvingUtilizationOfUnsecuredLines+
                        DebtRatio, data=training, family="binomial")

training <- training %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) 

# This bit of code computes the ROC curve
pred <- prediction(predictions = training$p_hat, labels = training$SeriousDlqin2yrs)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

# Make Predictions
test <- test %>% 
  mutate(Probability= predict(model_logistic, newdata = test, type="response"))

predictions <- test %>% 
  select(X, Probability) %>% 
  rename(Id=X)

write.csv(predictions, "predictions.csv")







