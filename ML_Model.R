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
docvars(corpus.training) %>% as.data.frame() %>% group_by(formality) %>% summarize(n=n())

dfm.training <- dfm(corpus.training, stem = T, remove_punct=T, remove_symbols = T, remove_numbers = T)

total_sample_size = nrow(df)
set.seed(300) #Makes our results replicable
id_train <- sample(1:total_sample_size, total_sample_size * 0.8, replace = FALSE)

dfm.training$id_numeric <- 1:ndoc(dfm.training)

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
correct.training <- dfm_correct.training$formality
tab_class <- table(correct.training, prediction.training)
tab_class
confusionMatrix(tab_class, mode = "prec_recall")

##################################################################
#Use Model on the unlabeled movie dataset
corpus.movies <- corpus(movies)

dfm.movies <- dfm(corpus.movies, stem = T, remove_punct=T,
                        remove_symbols = T, remove_numbers = T) %>% 
                        dfm_match(featnames(dfm_train))

predictions_movies <- predict(review_model, newdata = dfm.movies)

movies$formality <- predictions_movies

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

##################################################################
# Analysis
movies_summary <- movies %>%
  group_by(Movie.ID, Movie.Title, Release.Year, Genre) %>%
  summarise(num_convs = n(),
            num_formal = sum(formality == "formal"),
            num_informal = sum(formality == "informal"),
            score = ((num_formal + 1) / (num_informal + 1))) %>%
  filter(num_convs >= 20)
  
# Extract the decade from the year
movies_summary$Decade <- as.numeric(substr(movies_summary$Release.Year, 1, 3)) * 10

# Group movies into 10-year intervals
movies_summary$Decade_Group <- paste(movies_summary$Decade, "-", movies_summary$Decade + 9)

year_summary = movies_summary %>% group_by(Release.Year) %>% 
  summarise(num_movies = n(), median_score = median(score, na.rm = T))

decade_summary = movies_summary %>% group_by(Decade_Group) %>% 
  summarise(num_movies = n(), median_score = median(score, na.rm = T))

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
    median_score = median(score, na.rm = TRUE)
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Combine genres into broader categories
movies_summary_combined <- movies_summary %>%
  mutate(Genre_Combined = case_when(
    Genre %in% c("action", "adventure") ~ "Action/Adventure",
    Genre %in% c("drama", "romance") ~ "Drama/Romance",
    Genre %in% c("sci-fi", "fantasy") ~ "Sci-Fi/Fantasy",
    Genre %in% c("crime", "mystery", "thriller") ~ "Crime/Mystery/Thriller",
    Genre == "horror" ~ "Horror/Thriller",
    Genre %in% c("musical", "music") ~ "Musical/Music",
    Genre %in% c("biography", "documentary") ~ "Biography/Documentary",
    Genre %in% c("animation", "family") ~ "Animation/Family",
    Genre %in% c("history", "war") ~ "History/War",
    TRUE ~ as.character(Genre)
  ))

# Check unique values in the combined genre category
unique(movies_summary_combined$Genre_Combined)


# Calculate average score for each combined genre in each year
genre_summary_avg_combined <- movies_summary_combined %>%
  group_by(Release.Year, Genre_Combined) %>%
  summarise(
    avg_score = median(score, na.rm = TRUE)
  )

# Plot the average score for each combined genre over the years
genre_summary_avg_combined %>%
  ggplot(aes(x = Release.Year, y = avg_score, color = Genre_Combined, group = Genre_Combined)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Average Score for Each Combined Genre Over the Years",
       x = "Release Year", y = "Average Score")

# Calculate average score for each combined genre in each decade
genre_summary_avg_combined_decade <- movies_summary_combined %>%
  group_by(Decade_Group, Genre_Combined) %>%
  summarise(
    avg_score = mean(score, na.rm = TRUE)
  )

# Plot the average score for each combined genre decade-wise
genre_summary_avg_combined_decade %>%
  ggplot(aes(x = Decade_Group, y = avg_score, color = Genre_Combined, group = Genre_Combined)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Average Score for Each Combined Genre Decade-wise",
       x = "Decade", y = "Average Score")


# Calculate the number of movies released for each combined genre in each year
genre_movie_counts <- movies_summary_combined %>%
  group_by(Release.Year, Genre_Combined) %>%
  summarise(
    num_movies = n()
  )

# Filter out years where less than 4 movies were released for a genre
genre_movie_counts_filtered <- genre_movie_counts %>%
  group_by(Genre_Combined) %>%
  filter(num_movies >= 4)

# Join with the original data to get the scores for the filtered years
genre_summary_avg_combined_filtered <- genre_movie_counts_filtered %>%
  left_join(movies_summary_combined, by = c("Release.Year", "Genre_Combined")) %>%
  group_by(Release.Year, Genre_Combined) %>%
  summarise(
    avg_score = mean(score, na.rm = TRUE)
  )

# Plot the average score for each combined genre decade-wise
genre_summary_avg_combined_filtered %>%
  ggplot(aes(x = Release.Year, y = avg_score, color = Genre_Combined, group = Genre_Combined)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Average Score for Each Combined Genre (Years with >= 4 movies)",
       x = "Release Year", y = "Average Score")

