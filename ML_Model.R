library(tidyverse)
library(quanteda.textmodels)
library(MLmetrics)
library(htmltools)
library(quanteda)
library(quanteda.corpora)
library(caret)


#Data preperation for tolerance = tolerant
progressive_tweets <- read.csv("progressive-tweet-sentiment.csv")

tolerant_tweets <- progressive_tweets %>% 
  filter(target == "Legalization of Abortion" | target == "Feminist Movement"
         | target == "Atheism") %>%
  mutate(tolerance = "tolerant", text = tweet) %>% 
  select(tolerance, text) 



#Data Preperation for tolerance = neutral (slur _language.csv indlucds neutral topics)
slur_reddits<- read.csv("slur_language.csv")

neutral_reddits <- slur_reddits %>% 
  filter(annotation_Primary == "Neutral" & meta_text != "" ) %>% 
  mutate(tolerance = " neutral",text = meta_text) %>% 
  select(tolerance, text)

#Data Preperation tolerant = intoleran
intolerant_reddits <- slur_reddits %>% 
  filter(annotation_Primary == "Slur" |
           annotation_Primary =="PersonDirectedAbuse" |
           annotation_Primary == "IdentityDirectedAbuse"|
           annotation_Primary == "AffiliationDirectedAbuse" &
           meta_text != "" ) %>% 
  mutate(tolerance = " intolerant",text = meta_text) %>% 
  select(tolerance, text)


#Glueing the three categories togheter
tolerance <- rbind(tolerant_tweets, neutral_reddits, intolerant_reddits)
######################################################

#ML-Model
corpus <- corpus(tolerance)
docvars(corpus) %>% as.data.frame() %>% group_by(tolerance) %>% summarize(n=n())


dfm <- dfm(corpus, stem = T, remove = stopwords("en"), remove_punct=T,
           remove_symbols = T, remove_numbers = T)

total_sample_size = nrow(tolerance)
set.seed(300) #Makes our results replicable
id_train <- sample(1:total_sample_size, total_sample_size * 0.8,
                   replace = FALSE)

dfm$id_numeric <- 1:ndoc(dfm)

#Creating training and test set
dfm_train <- dfm_subset(dfm, id_numeric %in% id_train) 
dfm_test <- dfm_subset(dfm, !id_numeric %in% id_train)

#Ratio test and train set
ndoc(dfm_train)
ndoc(dfm_test)

#Training Naive Bayes
review_model <- textmodel_nb(dfm_train, dfm_train$tolerance)
summary(review_model)

prediction <- predict(review_model,dfm_test)
dfm_correct <- dfm_match(dfm_test, features = featnames(dfm_train))
correct <- dfm_correct$tolerance
tab_class <- table(correct, prediction)
tab_class
confusionMatrix(tab_class, mode = "prec_recall")















