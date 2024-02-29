library(tidyverse)
library(quanteda.textmodels)
library(quanteda.textstats)
library(MLmetrics)
library(htmltools)
library(quanteda)
library(quanteda.corpora)
library(caret)
library(plotly)
library(Rcpp)


#Data preperation for tolerance = tolerant
progressive_tweet_sentiment <- read.csv("data/progressive-tweet-sentiment.csv")
progressive_tweet_sentiment <- progressive_tweet_sentiment %>% 
  filter(target == "Legalization of Abortion" | target == "Feminist Movement"
         | target == "Atheism") %>%
  mutate(tolerance = "tolerant", text = tweet) %>% 
  select(tolerance, text) 


r.askFeminist = read.csv("data/reddit_askFeminism.csv")
r.climate = read.csv("data/reddit_climate.csv")
r.climateChange = read.csv("data/reddit_climateOffensive.csv")
r.disability = read.csv("data/reddit_disability.csv")
r.ecology = read.csv("data/reddit_ecology.csv")
r.feminism = read.csv("data/reddit_feminism.csv")
r.concatenated = rbind(r.askFeminist, r.climate, r.climateChange, r.disability, r.ecology, r.feminism)
r.concatenated = r.concatenated %>% mutate(text = Text, tolerance = "tolerant") %>% select(text, tolerance)


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
                   intolerant_reddits, r.concatenated,
                   hate_speech)

# Define the function to balance the data by tolerance category
balance_data <- function(data) {
  categories <- unique(data$tolerance)
  category_sizes <- table(data$tolerance)
  min_rows <- min(category_sizes)
  
  balanced_data <- data.frame()
  
  for (category in categories) {
    category_data <- subset(data, tolerance == category)
    sampled_rows <- category_data[sample(1:nrow(category_data), min_rows), ]
    balanced_data <- rbind(balanced_data, sampled_rows)
  }
  
  return(balanced_data)
}

# Balance the data by tolerance category
tolerance <- balance_data(tolerance)

# Shuffle the rows to ensure randomness
tolerance <- tolerance_balanced[sample(nrow(tolerance_balanced)), ]


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

#make predictions on the test set
prediction <- predict(review_model,dfm_test)
dfm_correct <- dfm_match(dfm_test, features = featnames(dfm_train))
correct <- dfm_correct$tolerance
tab_class <- table(correct, prediction)
tab_class
confusionMatrix(tab_class, mode = "prec_recall")

########################################################################
#use model on the movie dataset

#preparing the movie dataset
labeled_movie <- read.csv("data/movie_conversation_with_labels.csv")
unlabeled_movie <- read.csv("data/movie_conversation_without_labels.csv")

labeled_movie <- labeled_movie %>% 
  #rename(text = Text, tolerance = Tolerance) %>% 
  select(tolerance, text)

unlabeled_movie <- unlabeled_movie %>% 
  rename(text = Text)

prediction_movie_test <- predict(review_model, labeled_movie)
dfm_correct_movie <- dfm_match(labeled_movie, features = featnames(dfm_train))
correct_movie <- dfm_correct_movie$tolerance
tabl_class_movie <- table(movie_correct, prediction_movie_test)
tabl_class_movie
confusionMatrix(tabl_class_movie, mode = "prec_recall")



















