# Problem Set 8
# Kaggle: Evergreen classification
# https://www.kaggle.com/c/stumbleupon


# Set up and loading data ---------------
library(tidyverse)
library(broom)
library(ROCR)
library(glmnet)

setwd("~/Documents/Statistical Learning/PS08 Evergreens")

train <- read_tsv('train.tsv') 
test <- read_tsv('test.tsv')

# Data cleanup
# These are often parsed incorrectly
str(train)
train <- train %>% 
  mutate(
    alchemy_category_score = as.numeric(alchemy_category_score),
    is_news = as.integer(is_news), news_front_page = as.integer(news_front_page)
         )

train[train=="?"] <- NA 


# Exploratory Data Analysis-----

# For all numerical or binary variables, I am calculating percent difference
# for evergreens/not

eda <- train %>%
  group_by(label) %>%
  summarise(mean(alchemy_category_score), mean(avglinksize),
            mean(commonlinkratio_1), mean(commonlinkratio_2),
            mean(commonlinkratio_3), mean(commonlinkratio_4),
            mean(compression_ratio), mean(embed_ratio),
            mean(framebased), mean(frameTagRatio), mean(hasDomainLink),
            mean(html_ratio), mean(image_ratio), mean(is_news),
            mean(lengthyLinkDomain), mean(linkwordscore), mean(news_front_page),
            mean(non_markup_alphanum_characters),
            mean(spelling_errors_ratio), mean(parametrizedLinkRatio), 
            mean(numwords_in_url), mean(numberOfLinks))

eda_long <- eda %>% gather(key = predictor, value = value, -label)

percent_diffs <- eda_long %>%
  group_by(predictor) %>%
  mutate(percent_difference = abs(((lag(value)- value)/lag(value)))) %>%
  filter(label == 1) 

percent_diffs %>%
  arrange(desc(percent_difference)) %>%
  head(10)


# Model model model ---------------

model_formula_all <- train %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  # I don't want my moedel formula to have these variables.
  # Must remove "is_news" because it 
  setdiff(c("label", "urlid", "boilerplate", "url")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("label ~ ", .)



# I'm having trouble getting the model for all variables to work
# so I can't do shrinkage method with all variables
model_formula_all <- as.formula(model_formula_all)
#model_logistic_all <- glm(model_formula_all, data = train, family = "binomial")


# Model 1 (Top 10 in percent difference) ----------------
model_formula <- as.formula(label~non_markup_alphanum_characters+commonlinkratio_4+
                              commonlinkratio_3+commonlinkratio_2+compression_ratio+
                              frameTagRatio+image_ratio+linkwordscore+embed_ratio+
                              numberOfLinks+alchemy_category)

model_logistic <- glm(model_formula, data=train, family="binomial")

# get predictions
model1_train <- train %>%
  mutate(
    p_hat = predict(model_logistic, type="response")
  ) %>%
  select(label, p_hat)
View(model1_train)

# Find area under the curve
pred_model1 <- prediction(predictions = model1_train$p_hat, labels = model1_train$label)
perf_model1 <- performance(pred_model1, "tpr","fpr")

auc1 <- as.numeric(performance(pred_model1,"auc")@y.values)
auc1

plot(perf_model1, main=paste("Area Under the Curve =", round(auc1, 3)))
abline(c(0, 1), lty=2)

# Model 2 (Top 6 percent differences) -----------------
percent_diffs %>%
  arrange(desc(percent_difference)) %>%
  head(6)

top6_formula <- as.formula(label~non_markup_alphanum_characters+
                             commonlinkratio_4+compression_ratio+
                             commonlinkratio_3+
                             frameTagRatio+image_ratio)

model_top6 <- glm(top6_formula, data=train, family="binomial")

top6_train <- train %>%
  mutate(
    p_hat = predict(model_top6, type="response")
  ) %>%
  select(label, p_hat)
View(top6_train)

pred_top6 <- prediction(predictions = top6_train$p_hat, labels = top6_train$label)
perf_top6 <- performance(pred_top6, "tpr","fpr")

auc_top6 <- as.numeric(performance(pred_top6,"auc")@y.values)
auc_top6
# slightly lower than top 10

# Using shrinkage ------------------------------------
# RIDGE----
model_formula_18 <- as.formula(label~non_markup_alphanum_characters+commonlinkratio_4+
                              commonlinkratio_3+commonlinkratio_2+compression_ratio+
                              frameTagRatio+image_ratio+linkwordscore+embed_ratio+
                              numberOfLinks+html_ratio+parametrizedLinkRatio+
                                numwords_in_url+avglinksize+lengthyLinkDomain+
                                hasDomainLink+commonlinkratio_1+spelling_errors_ratio+
                                commonlinkratio_2)

X <- model.matrix(model_formula_18, data = train)[, -1]
y <- train$label

n_folds <- 5
cv_ridge <- cv.glmnet(X, y, alpha = 0, nfolds = n_folds)

lambda_star_ridge <- cv_ridge %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_ridge

model_ridge <- glmnet(X, y, alpha = 0)

ridge_train <- train %>%
  mutate(
    p_hat = predict(model_ridge, newx=X, s=lambda_star_ridge)
  ) %>%
  select(label, p_hat)

pred_ridge <- prediction(predictions = ridge_train$p_hat, labels = ridge_train$label)
perf_ridge <- performance(pred_ridge, "tpr","fpr")

auc_ridge <- as.numeric(performance(pred_ridge,"auc")@y.values)
auc_ridge
# auc of .6691 is best so far

# LASSO-----
cv_LASSO <- cv.glmnet(X, y, alpha = 1, nfolds = n_folds)

lambda_star_LASSO <- cv_LASSO %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_LASSO

model_LASSO <- glmnet(X, y, alpha = 1)

LASSO_train <- train %>%
  mutate(
    p_hat = predict(model_LASSO, newx=X, s=lambda_star_LASSO)
  ) %>%
  select(label, p_hat)

pred_LASSO <- prediction(predictions = LASSO_train$p_hat, labels = LASSO_train$label)
perf_LASSO <- performance(pred_LASSO, "tpr","fpr")

auc_LASSO <- as.numeric(performance(pred_LASSO,"auc")@y.values)
auc_LASSO
# auc of .6685 slightly worse than ridge

# Submission ----------
# Ridge regression was best.

best_model <- model_ridge
p_star <- 0.5

# Run this code if best model doesn't use shrinkage:
# submission <- test %>%
#   mutate(p_hat =
#            predict(best_model, newdata = test, type="response")) %>%
#   mutate(label = ifelse(p_hat > p_star, 1, 0)) %>%
#   select(urlid, label)

# To make glmnet work:
test <- test %>% mutate(label=1)
test_X <- model.matrix(model_formula_18, data = test)[, -1]

predictions <- model_ridge %>% 
  predict(newx=test_X, s=lambda_star_ridge) %>% 
  as.vector()

# Submission data frame for ridge
submission <- test %>%
  mutate(p_hat =
           predictions) %>%
  mutate(label = ifelse(p_hat > p_star, 1, 0)) %>%
  select(urlid, label)

write_csv(submission, 'submission_evergreens.csv')

# I scored 0.62303
