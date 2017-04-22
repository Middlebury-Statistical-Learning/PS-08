library(tidyverse)
library(broom)
library(ROCR)

setwd("~/Desktop/Middlebury/Stat Learning/PS08")

#Credit 
cs_train<-read_csv("~/Desktop/Middlebury/Stat Learning/PS08/cs_training.csv")
cs_test<-read_csv("~/Desktop/Middlebury/Stat Learning/PS08/cs_test.csv")

##Evergreen 
eg_train<-read_tsv("~/Desktop/Middlebury/Stat Learning/PS08/eg_train.tsv")
eg_test<-read_tsv("~/Desktop/Middlebury/Stat Learning/PS08/eg_test.tsv")

model_formula <- as.formula(label~`avglinksize`+
                              #`commmonlinkratio_1`+
                              #`commmonlinkratio_2`+
                              #`commmonlinkratio_3`+
                              #`commmonlinkratio_4`+
                              #`commpression_ratio`+
                              `embed_ratio`+
                              #`framebase`+
                              `frameTagRatio`+
                              `hasDomainLink`+
                              `html_ratio`+
                              `image_ratio`+
                              `lengthyLinkDomain`+
                              `linkwordscore`+
                              `non_markup_alphanum_characters`+
                              `numberOfLinks`+
                              `numwords_in_url`+
                              `parametrizedLinkRatio`+
                              `spelling_errors_ratio`)
model_logistic <- glm(model_formula, data=eg_train, family="binomial")

p_star<-0.6 

eg_test <- eg_test %>% 
  mutate(p_hat=predict(model_logistic, newdata = eg_test, type="response")) %>% 
  mutate(label=if_else(p_hat>p_star,1,0)) 

eg_submission <- eg_test %>% 
  select(urlid, label)

write_csv(eg_submission, "~/Desktop/Middlebury/Stat Learning/PS08/PS08_Kyra_Gray_submission.csv")
