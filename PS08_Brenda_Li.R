# Brenda Li PS08
# StumbleUpon Evergreen Classification Challenge

# Loading all appropriate libraries
library(tidyverse)
library(broom)
library(ROCR)
library(glmnet)


# PART 1: Exploring model options and looking at AUC

# Loading in training data and selecting the variables that have the most complete data for simplicity
train<-read.table(file = 'train.tsv', sep = '\t', header = TRUE) %>% 
  na.omit() %>% 
  select(-url,-boilerplate,-alchemy_category,-alchemy_category_score,-is_news,-news_front_page)

# Making a logistic model with all the variables
model_logistic <- glm(label~., data=train, family="binomial")

# Saving predictions into train
train <- train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) 

# Calculating AUC with this model
pred0 <- prediction(predictions = train$p_hat, labels = train$label)
perf0 <- performance(pred0, "tpr","fpr")
auc0 <- as.numeric(performance(pred0,"auc")@y.values)
auc0 # AUC = 0.6702287



# Using CV to find an appropriate lambda and seeing if LASSO will help improve AUC
X <- model.matrix(label~., data = train)[, -1]
y <- train$label

lambda_values <- 10^seq(10, -2, length = 100) # Trying different lambda values

cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Finding the optimal lambda
cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star #lambda_star=0.01


# Making the model with this optimal lambda
optimal_model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_star)
predictions <- predict(optimal_model_LASSO,newx = X)

# Saving predictions as p_hat1
train<-train %>% 
  mutate(p_hat1 = as.vector(predictions)) 

# Calculating AUC
pred1 <- prediction(predictions = train$p_hat1, labels = train$label)
perf1 <- performance(pred1, "tpr","fpr")

auc1 <- as.numeric(performance(pred1,"auc")@y.values)
auc1 # AUC with LASSO is also 0.6702287, so no difference



# PART 2: Using CV to find optimal p_start to use (not sure if this is the way to go)

# Shuffling and setting up folds
train <-train[sample(nrow(train)),]

n_folds <- 5
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# Trying different thresholds of p
p_thresholds<-seq(0, 1, length = 1000)

# Vector that will store the optimal p_star of each fold
p_stars<-rep(0,n_folds)


# C.V.
for (i in 1:n_folds){
  
  # Making pseudo_train and pseudo_test
  pseudo_train<-train %>% filter(fold!=i)
  pseudo_test<-train %>% filter(fold==i)
  
  # Setting up variables for LASSO (even though technically it didn't perform better than regular logistic regression)
  X_CV <- model.matrix(label~., data = pseudo_train)[, -1]
  y_CV <- pseudo_train$label
  X_Test_CV<-model.matrix(label~.,data=pseudo_test)[,-1]
  
  # Training the model and saving the predictions
  optimal_model_LASSO_CV <- glmnet(X_CV, y_CV, alpha = 1, lambda = lambda_star)
  predictions_CV <- predict(optimal_model_LASSO_CV,newx = X_Test_CV)
  best_score<-0
  
  # Storing which p-star generates the highest proportion correct
  for(j in 1:length(p_thresholds)){
    pseudo_test<-pseudo_test %>% 
      mutate(p_hat_cv = as.vector(predictions_CV)) %>% 
      mutate(label_hat=ifelse(p_hat_cv>p_thresholds[j],1,0)) %>% 
      mutate(right=ifelse(label==label_hat,1,0))
    if (mean(pseudo_test$right)>best_score){
      p_stars[i]<-j
      best_score<-mean(pseudo_test$right)
    }
  }
}

p_star_optimal<-p_thresholds[mean(p_stars)] #optimal p_star is 0.5095
  

# PART 3: Predicting on test data

# reading in test dataset
test<-read.table(file = 'test.tsv', sep = '\t', header = TRUE) %>% 
  na.omit() %>% 
  mutate(label=1)%>% 
  select(-url,-boilerplate,-alchemy_category,-alchemy_category_score,-is_news,-news_front_page)


# Taking out previous predictions from train so the dimensions work later on for predict function
train<-select(train,-p_hat,-p_hat1,-fold)

# Training model to train data
X <- model.matrix(label~., data = train)[, -1]
y <- train$label

optimal_model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_star)


# Making Predictions on test data
X_test <- model.matrix(label~., data = test)[, -1]

predictions <- predict(optimal_model_LASSO,newx = X_test)

test<-test %>% 
  mutate(p_hat = as.vector(predictions))

test<-test %>% 
  mutate(label_hat=ifelse(p_hat>p_star_optimal,1,0)) 

# Making submission file
test %>% 
  select(urlid,label_hat) %>% 
  rename(label=label_hat) %>% 
  write_csv("PS08_Brenda_Li_submission.csv")  # final model still did pretty poorly on Kaggle...
