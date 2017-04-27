library(tidyverse)
library(broom)
# We'll predict the sex (binary) of 60K OkCupid users using only their height
# https://github.com/rudeboybert/okcupiddata
library(okcupiddata)
# For ridge regression & LASSO
library(glmnet)
# For interactive visualizations:
library(plotly)

#My idea is to conbine Lasso and Ridge with logistic regression. The problem I encountered is that 
#whenever the log(lambda) value for ridge is less than0, the program stops running. I don't know why 
#it is the case. Also, when I appled lasso model, all of the other variable except the intercept just disappeared
#I don't know why that happened
cs_training <- na.omit(cs_training) %>%  
  dplyr::rename(
  NumberOfTime30to59DaysPastDueNotWorse = `NumberOfTime30-59DaysPastDueNotWorse`,
  NumberOfTime60to89DaysPastDueNotWorse =`NumberOfTime60-89DaysPastDueNotWorse`
  )

model_formula <- cs_training %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("X1", "fold", "SeriousDlqin2yrs")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("SeriousDlqin2yrs ~ ", .)
model_formula
model_formula <- as.formula(model_formula)
model_formula <- as.formula("SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age")


model_glm <- glm(model_formula, family="binomial", data=cs_training)
tidy(model_glm)

X <- model.matrix(model_formula, data = cs_training)[, -1]
y <- cs_training$SeriousDlqin2yrs
n_folds <- 10

# use cross validation to find the best lambda for lasso
# This is what you had originally
# lambda_values <- 10^seq(from = 0, to = 10 , length = 100)
lambda_values <- 10^seq(from = -3, to = 1.5 , length = 10)

# cv_lasso <- cv.glmnet(X, y, alpha = 1, lambda = lambda_values, nfolds = n_folds,family="binomial")
# model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values, family="binomial")

cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = n_folds,family="binomial")
model_lasso <- glmnet(X, y, alpha = 1, family="binomial")

cv_lasso_tidy <- cv_lasso %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda)) 

# Extract optimal lambda using broom::glance()
lambda_star_lasso <- cv_lasso %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_lasso

ggplot(cv_lasso_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="Lasso Regression Cross-Validation") + 
  geom_vline(xintercept = log(lambda_star_lasso), col="red")

model_lasso %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_lasso)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Lasso Regression Coefficients")

# find the optimal lambda for ridge 
lambda_values_ridge <- 10^seq(from = -20, to = 2, length = 100)
#the reason I use ridge is that I think most preditors in the dataset are relavent to the result
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values_ridge, nfolds = n_folds,family="binomial")
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values_ridge, family="binomial")

cv_ridge_tidy <- cv_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda)) 

# Extract optimal lambda using broom::glance()
lambda_star_ridge <- cv_ridge %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_ridge


ggplot(cv_ridge_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="Ridge Regression Cross-Validation") + 
  geom_vline(xintercept = log(lambda_star_ridge), col="red")

model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_ridge)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")


#compare ridge and lasso using cross validation
cs_training <- cs_training %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

results_all_folds <- NULL

for(i in 1:n_folds){

  pseudo_train <- cs_training %>%
    filter(fold != i)
  pseudo_test <- cs_training %>%
    filter(fold == i)

  pseudo_train_X <- model.matrix(model_formula, data = pseudo_train)[, -1]
  pseudo_train_y <- pseudo_train$SeriousDlqin2yrs
  pseudo_test_X <- model.matrix(model_formula, data = pseudo_test)[, -1]

  model_RIDGE <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 0, lambda = lambda_star_ridge, family="binomial")
  model_LASSO <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 1, lambda = lambda_star_lasso, family="binomial")
  

  ridge_predictions <- model_RIDGE %>% 
    predict(newx=pseudo_test_X, s=lambda_star_ridge)
  LASSO_predictions <- model_LASSO %>% 
    predict(newx=pseudo_test_X, s=lambda_star_lasso)
  
  p_hat_ridge <- 1/(1+exp(-predict(model_RIDGE, newx=pseudo_test_X, s=lambda_star_ridge)))
  p_hat_lasso <- 1/(1+exp(-predict(model_LASSO, newx=pseudo_test_X, s=lambda_star_lasso)))
  
  results_all_folds <- 
    data_frame(
      SeriousDlqin2yrs = pseudo_test$SeriousDlqin2yrs,
      ridge = as.vector(p_hat_ridge),
      LASSO = as.vector(p_hat_lasso)
    ) %>% 
    tidyr::gather(method, yhat, -SeriousDlqin2yrs) %>% 
    group_by(method) %>% 
    summarise(MSE = mean((SeriousDlqin2yrs-yhat)^2)) %>% 
    mutate(fold=i) %>% 
    bind_rows(results_all_folds)
  print(i)
}

results_all_folds %>% 
  group_by(method) %>% 
  summarise(MSE_CV = mean(MSE)) %>% 
  arrange(desc(MSE_CV))


# It seems like ridge model wins. Next step -- make prediction using ridge model
cs_test <- cs_test %>%  
  dplyr::rename(
    NumberOfTime30to59DaysPastDueNotWorse = `NumberOfTime30-59DaysPastDueNotWorse`,
    NumberOfTime60to89DaysPastDueNotWorse =`NumberOfTime60-89DaysPastDueNotWorse`
  ) 

cs_test$SeriousDlqin2yrs <- rep(1, nrow(cs_test))


for(i in 3:ncol(cs_test)){
  cs_test[[i]][is.na(cs_test[[i]])]<- round(mean(cs_test[[i]], na.rm = TRUE))
}

final_test_X <- model.matrix(model_formula, data = cs_test)[, -1]

predictions <- model_RIDGE %>% 
  predict(newx=final_test_X, s=lambda_star_ridge) %>% 
  as.vector()

p_hat_ridge <- 1/(1+exp(-predict(model_RIDGE, newx=final_test_X, s=lambda_star_ridge))) %>% 
  as.vector()

cs_test$SeriousDlqin2yrs <- p_hat_ridge

cs_test <- cs_test %>%
  dplyr::rename(
    Id =`X1`,
    Probability = `SeriousDlqin2yrs`
  ) %>% 
  select(Id,Probability)

write.csv(cs_test, file = "~/Desktop/PS08_Xiaoli_Jin_submission.csv" )

