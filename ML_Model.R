library(tidyverse)
library(quanteda.textmodels)
library(MLmetrics)
library(htmltools)
library(quanteda)
library(quanteda.corpora)
library(caret)


#Data preperation for tolerance = tolerant
progressive_tweet_sentiment <- read.csv("data/progressive-tweet-sentiment.csv")
progressive_tweet_sentiment <- progressive_tweet_sentiment %>% 
  filter(target == "Legalization of Abortion" | target == "Feminist Movement"
         | target == "Atheism") %>%
  mutate(tolerance = "tolerant", text = tweet) %>% 
  select(tolerance, text) 


reddit_posts_progressive_small <- read.csv("data/reddit_posts_progressive_small.csv")
reddit_posts_progressive_small <- reddit_posts_progressive_small %>% 
  mutate(text = Text, tolerance = "tolerant") %>% 
  select(text, tolerance)


#Data Preperation for tolerance = neutral (slur _language.csv indlucds neutral topics)
slur_languagexlsx<- read.csv("data/slur_languagexlsx.csv")

neutral_reddits <- slur_languagexlsx %>% 
  filter(annotation_Primary == "Neutral" & meta_text != "" ) %>% 
  mutate(tolerance = " neutral",text = meta_text) %>% 
  select(tolerance, text)

#Data Preperation tolerant = intoleran
intolerant_reddits <- slur_languagexlsx %>% 
  filter(annotation_Primary == "Slur" |
           annotation_Primary =="PersonDirectedAbuse" |
           annotation_Primary == "IdentityDirectedAbuse"|
           annotation_Primary == "AffiliationDirectedAbuse" &
           meta_text != "" ) %>% 
  mutate(tolerance = "intolerant",text = meta_text) %>% 
  select(tolerance, text)

hate_speech <- read.csv("data/hatespeach.csv")
hate_speech <- hate_speech %>% 
  mutate(tolerance = "intolerant", text = tweet) %>% 
  select(tolerance, text)


#Glueing the three categories togheter
tolerance <- rbind(progressive_tweet_sentiment, neutral_reddits,
                   intolerant_reddits, reddit_posts_progressive_small,
                   hate_speech)

###########################################################################

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













