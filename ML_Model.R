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
  mutate(tolerance = "neutral",text = meta_text) %>% 
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
                   intolerant_reddits,
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

tolerance %>%
  group_by(tolerance) %>%
  summarise(count = n())

# Shuffle the rows to ensure randomness @Jeremy there is an error on this line and I dont know what its for
#tolerance <- tolerance[sample(nrow(tolerance_balanced)), ]


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
#use model on the labeled movie dataset

#preparing the movie dataset
labeled_movie <- read.csv("data/movie_conversation_with_labels.csv" )%>% rename(text = Text, tolerance = X.1) %>% select(-X)

#Preparing df for labeled movies
corpus_labeled_movie <- corpus(labeled_movie)
docvars(corpus_labeled_movie) %>% as.data.frame() %>% group_by(tolerance) %>% summarize(n=n())

dfm_labeled_movie <- dfm(corpus_labeled_movie, stem = T, remove = stopwords("en"), remove_punct=T,
          remove_symbols = T, remove_numbers = T) %>% 
          dfm_match(featnames(dfm_train))


#make predictions on the labeled movie data set
prediction_movie_test <- predict(review_model, dfm_labeled_movie)

dfm_correct_movie <- dfm_match(dfm_labeled_movie, features = featnames(dfm_train))
correct_movie <- dfm_correct_movie$tolerance

tabl_class_movie <- table(correct_movie, prediction_movie_test)
tabl_class_movie

confusionMatrix(tabl_class_movie, mode = "prec_recall")

##################################################################
#Use Model on the unlabeled movie dataset

unlabeled_movie <- read.csv("data/movie_conversation_without_labels.csv") %>% rename(text = Text) %>% select(-X, -X.1, -X.2, -X.3)

#Preparing df for unlabeled movies
corpus_unlabeled_movie <- corpus(unlabeled_movie)

dfm_unlabeled_movie <- dfm(corpus_unlabeled_movie, stem = T, remove = stopwords("en"), remove_punct=T,
                        remove_symbols = T, remove_numbers = T) %>% 
                        dfm_match(featnames(dfm_train))

predictions_unlabeled_movie <- predict(review_model, newdata = dfm_unlabeled_movie)

unlabeled_movie$predicted_label <- predictions_unlabeled_movie

# Tidying up the df
unlabeled_movie$Release.Year = str_replace(unlabeled_movie$Release.Year, "/.*", "")
unlabeled_movie %>% select(Release.Year) %>% unique()
unlabeled_movie <- unlabeled_movie %>% mutate(Genre = str_replace_all(Genre, "\\[|\\]", ""))
# Function to transform genre strings into vectors
string_to_vector <- function(genre_string) {
  # Remove leading and trailing single quotes
  genre_string <- gsub("^'|'$", "", genre_string)
  # Split the string by comma followed by a space
  unlist(strsplit(genre_string, "', '"))
}
# Apply the function to the Genre column
unlabeled_movie <- unlabeled_movie %>%
  mutate(Genre = lapply(Genre, string_to_vector))
# Unnest the Genre column
unlabeled_movie <- unlabeled_movie %>%
  tidyr::unnest(Genre)

# Analysis
movies_summary <- unlabeled_movie %>%
  group_by(Movie.ID, Movie.Title, Release.Year, Genre) %>%
  summarise(num_convs = n(),
            num_tolerant = sum(predicted_label == "tolerant"),
            num_neutral = sum(predicted_label == "neutral"),
            num_intolerant = sum(predicted_label == "intolerant"),
            score = ((num_tolerant + 1) / (num_intolerant + 1)) / num_convs) %>%
  filter(num_convs >= 20)
  
# Extract the decade from the year
movies_summary$Decade <- as.numeric(substr(movies_summary$Release.Year, 1, 3)) * 10

# Group movies into 10-year intervals
movies_summary$Decade_Group <- paste(movies_summary$Decade, "-", movies_summary$Decade + 9)

year_summary = movies_summary %>% group_by(Release.Year) %>% 
  summarise(num_movies = n(), avg_score = sum(score, na.rm = T) / num_movies)

decade_summary = movies_summary %>% group_by(Decade_Group) %>% 
  summarise(num_movies = n(), avg_score = sum(score, na.rm = T) / num_movies)

year_summary %>% ggplot(aes(x = Release.Year, y = avg_score)) +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
decade_summary %>% ggplot(aes(x = Decade_Group, y = avg_score)) +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

genre_summary <- movies_summary %>%
  group_by(Release.Year, Genre) %>%
  summarise(
    num_movies = n(),
    avg_score = sum(score, na.rm = TRUE) / num_movies
  )

genre_totals <- genre_summary %>%
  group_by(Release.Year) %>%
  summarise(total_movies = sum(num_movies))

genre_summary <- genre_summary %>%
  left_join(genre_totals, by = "Release.Year") %>%
  mutate(perc_of_total = (num_movies / total_movies) * 100)

genres = c("drama", "crime")
genres <- unique(genre_summary$Genre)

genre_summary %>%
  ggplot(aes(x = Release.Year, y = perc_of_total, color = Genre)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Percentage of Drama and Crime Movies Released Each Year",
       x = "Release Year", y = "Percentage of Total Movies")
