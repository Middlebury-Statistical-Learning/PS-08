# Emily Miller
# Problem Set 8
# Evergreen Kaggle 

library(tidyverse)
library(broom)
setwd("C:/Users/Emily Miller/Documents/Stat_learning")


train <- read.delim("CSVs/evergreen_train.tsv")
View(train)
attach(train)

model_formula <- as.formula(label~urlid + alchemy_category+avglinksize+commonlinkratio_1+commonlinkratio_2 +
                              commonlinkratio_3 + commonlinkratio_4 + compression_ratio + embed_ratio
                            + framebased + frameTagRatio + hasDomainLink + html_ratio + image_ratio + is_news +
                              lengthyLinkDomain + linkwordscore + news_front_page + non_markup_alphanum_characters +
                              numberOfLinks + numwords_in_url + parametrizedLinkRatio + spelling_errors_ratio)
model_logistic <- glm(model_formula, data=train, family="binomial")

predictions <- model_logistic %>%
  augment() %>%
  tbl_df() %>%
  mutate(
    # Convert from log-odds space (-infty, infty) to probability space (0, 1):
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3))

p_star_vector <- seq(0, 1, by = 0.01)
cost_vector <- rep(0, length(p_star_vector))

for(i in 1:length(p_star_vector)){
  cost_per_person <- predictions %>%
    mutate(
      y_hat = ifelse(p_hat > p_star_vector[i], 1, 0),
      FP = ifelse(label == 0 & y_hat == 1, 1, 0),
      FN = ifelse(label == 1 & y_hat == 0, 1, 0),
      cost = FP + FN
    ) %>%
    select(label, y_hat, cost)

  cost_vector[i] <- sum(cost_per_person$cost)
}

# Explore Outcome ---------------------------------------------------------
results <- data_frame(
  p_star = p_star_vector,
  cost = cost_vector) %>%
  mutate(avg_cost = cost/nrow(train))

# Let's sort in ascending order of cost:
results %>%
  arrange(cost) %>%
  slice(1:10)

# The optimal p* is here
optima <- results %>%
  arrange(cost) %>%
  slice(1)

# optima p-star = 0.53

# Cross-Validation
detach(train)
predictions <-  select(predictions, label, p_hat) 
predictions <- mutate(predictions, y_hat = ifelse(p_hat > 0.53, 1, 0))
attach(predictions)
Percent_wrong <- sum(abs(y_hat - label))/nrow(predictions)
Percent_wrong
detach(predictions)

# Make predictions for test data set.
test <- read.delim("CSVs/evergreen_test.tsv")
model_logistic <- glm(model_formula, data=test, family="binomial")

test_predictions <- model_logistic %>%
  augment() %>%
  tbl_df() %>%
  mutate(
    # Convert from log-odds space (-infty, infty) to probability space (0, 1):
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)) %>%
  mutate(y_hat = ifelse(p_hat > 0.54, 1, 0)) %>%
  select(urlid, y_hat)
  
  




