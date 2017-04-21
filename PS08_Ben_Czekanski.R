library(tidyverse)
library(ROCR)
setwd("~/Documents/Math218")

train <- read_tsv("train.tsv") %>%
  mutate(alchemy_category_score = as.numeric(alchemy_category_score),
       is_news = as.numeric(is_news),
       news_front_page = as.numeric(news_front_page)) %>%
  select(
    urlid, 
    # alchemy_category_score, 
    avglinksize, commonlinkratio_1, commonlinkratio_2, commonlinkratio_3,
    commonlinkratio_4, compression_ratio, embed_ratio, framebased, frameTagRatio, hasDomainLink, 
    html_ratio, image_ratio, 
    # is_news, 
    lengthyLinkDomain, 
    # news_front_page, non_markup_alphanum_characters,
    numberOfLinks, numwords_in_url, parametrizedLinkRatio, spelling_errors_ratio, label
  )
test <- read_tsv("test.tsv") %>%
  mutate(alchemy_category_score = as.numeric(alchemy_category_score))

model_lm <- lm(label ~ ., train)


train2 <- train %>%
  mutate(pred = predict(model_lm))

pred = prediction(train2$pred, train2$label)
perf <- performance(pred, "tpr","fpr")

auc <- as.numeric(performance(pred,"auc")@y.values)

plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

submission1 <- test %>%
  mutate(label = predict(model_lm, test)) %>%
  select(urlid, label) %>%
  mutate(label = ifelse(is.na(label == TRUE), 0, label))

write_csv(submission1, path = "trees_s1")


# And Logistic Regression

model_log <- glm(label ~ ., train, family = "binomial")

train3 <- train %>%
  mutate(pred = predict(model_log))

pred2 <-  prediction(train3$pred, train3$label)
perf2 <- performance(pred2, "tpr","fpr")

auc2 <- as.numeric(performance(pred2, "auc")@y.values)

plot(perf2, main=paste("Area Under the Curve =", round(auc2, 3)))
abline(c(0, 1), lty=2)

submission2 <- test %>%
  mutate(label = predict(model_log, test)) %>%
  select(urlid, label) %>%
  mutate(label = ifelse(is.na(label == TRUE), 0, label))

write_csv(submission2, path = "trees_s2")


