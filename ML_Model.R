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
library(caret)

#Data preperation
movies.labeled = read.csv("data/movie_formality_labled.csv") %>% select(-X) %>% rename(formality = Formal.Informal, text = Text)
movies = read.csv("data/movie_formality_unlabled.csv") %>% select(-X, -X.1) %>% rename(formality = Formal.Informal, text = Text)

df = movies.labeled %>% select(formality, text)

# Spliting labeled set into training and testing set
#train_proportion <- 0.8  # 80% for training, 20% for testing
#set.seed(123)

# Create a stratified random split based on the 'formality' label
#train_indices <- createDataPartition(df$formality, p = train_proportion, list = FALSE)

# Split the data into training and testing sets using the indices
#train_set <- df[train_indices, ]
#test_set <- df[-train_indices, ]

# Check the class distribution in both sets
#table(train_set$formality)
#table(test_set$formality)

# Shuffle both sets
#train_set <- train_set[sample(nrow(train_set)), ]
#test_set <- test_set[sample(nrow(test_set)), ]

###########################################################################
#ML-Model Training
corpus.training <- corpus(df)
docvars(corpus) %>% as.data.frame() %>% group_by(formality) %>% summarize(n=n())

dfm.training <- dfm(corpus.training, stem = T, remove_punct=T, remove_symbols = T, remove_numbers = T)

total_sample_size = nrow(df)
set.seed(300) #Makes our results replicable
id_train <- sample(1:total_sample_size, total_sample_size * 0.8, replace = FALSE)

dfm$id_numeric <- 1:ndoc(dfm.training)

#Creating training and test set
dfm_train <- dfm_subset(dfm.training, id_numeric %in% id_train) 
dfm_test <- dfm_subset(dfm.training, !id_numeric %in% id_train)

#Ratio test and train set
ndoc(dfm_train)
ndoc(dfm_test)

#Training Naive Bayes
review_model <- textmodel_nb(dfm_train, dfm_train$formality)
summary(review_model)

#make predictions on the test set
prediction.training <- predict(review_model,dfm_test)
dfm_correct.training <- dfm_match(dfm_test, features = featnames(dfm_train))
correct.training <- dfm_correct$formality
tab_class <- table(correct, prediction)
tab_class
confusionMatrix(tab_class, mode = "prec_recall")

##################################################################
#Use Model on the unlabeled movie dataset
corpus.movies <- corpus(movies)

dfm.movies <- dfm(corpus.movies, stem = T, remove_punct=T,
                        remove_symbols = T, remove_numbers = T) %>% 
                        dfm_match(featnames(dfm_train))

predictions_movies <- predict(review_model, newdata = dfm.movies)

movies$predicted_label <- predictions_movies

# Tidying up the df
movies$Release.Year = str_replace(movies$Release.Year, "/.*", "")
movies %>% select(Release.Year) %>% unique()
movies <- movies %>% mutate(Genre = str_replace_all(Genre, "\\[|\\]", ""))
# Function to transform genre strings into vectors
string_to_vector <- function(genre_string) {
  # Remove leading and trailing single quotes
  genre_string <- gsub("^'|'$", "", genre_string)
  # Split the string by comma followed by a space
  unlist(strsplit(genre_string, "', '"))
}
# Apply the function to the Genre column
movies <- movies %>%
  mutate(Genre = lapply(Genre, string_to_vector))
# Unnest the Genre column
movies <- movies %>%
  tidyr::unnest(Genre)

# Analysis
movies_summary <- movies %>%
  group_by(Movie.ID, Movie.Title, Release.Year, Genre) %>%
  summarise(num_convs = n(),
            num_formal = sum(predicted_label == "formal"),
            num_informal = sum(predicted_label == "informal"),
            score = ((num_formal + 1) / (num_informal + 1)) / num_convs) %>%
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

genres = c("war", "sport")
genres <- unique(genre_summary$Genre)

genre_summary %>%
  filter(Genre %in% genres) %>%
  ggplot(aes(x = Release.Year, y = perc_of_total, color = Genre, group = Genre)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Percentage of Drama and Crime Movies Released Each Year",
       x = "Release Year", y = "Percentage of Total Movies")

year_genre_summary %>% ggplot(aes(x = Release.Year, y = avg_score)) +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
decade_genre_summary %>% ggplot(aes(x = Decade_Group, y = avg_score)) +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
