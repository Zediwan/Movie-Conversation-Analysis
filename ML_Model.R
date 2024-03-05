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

#Data preparation
movies.labeled = read.csv("data/movie_formality_labled.csv") %>% select(-X) %>% rename(formality = Formal.Informal, text = Text)
movies = read.csv("data/movie_formality_unlabled.csv") %>% select(-X, -X.1) %>% rename(formality = Formal.Informal, text = Text)
load("data/hansard.RData") 
hansard = hansard %>% select(text = speech)
hansard$formality = "formal"
reddit = read.csv("data/reddit_ask.csv") %>% select(Text) %>% mutate(formality = "informal") %>% rename(text = Text)

smallest_df = min(nrow(hansard), nrow(reddit))
reddit = reddit %>% head(smallest_df)
hansard = hansard %>% head(smallest_df)

df.training = movies.labeled %>% select(formality, text)
df.training = rbind(df.training, hansard, reddit)

###########################################################################
#ML-Model Training
corpus.training <- corpus(df.training)
docvars(corpus.training) %>% as.data.frame() %>% group_by(formality) %>% summarize(n=n())

dfm.training <- dfm(corpus.training, stem = T, remove_punct=T, remove_symbols = T, remove_numbers = T)

total_sample_size = nrow(df.training)
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
movies.unnested <- movies %>%
  tidyr::unnest(Genre)

##################################################################
# Analysis
movies_summary <- movies %>%
  group_by(Movie.ID, Movie.Title, Release.Year, Genre) %>%
  summarise(num_convs = n(),
            num_formal = sum(formality == "formal"),
            num_informal = sum(formality == "informal"),
            score = num_formal / num_convs) %>%
  filter(num_convs >= 20)

#Analyzing the distribution of the formality score
score_distribution <- ggplot(movies_summary, aes(x = score)) +
                      geom_histogram(color = "black") +
                      ggtitle("Formaility Score Distribution") +
                      xlab("Formality Score") +
                      ylab("count")
score_distribution

print(mean(movies_summary$score))
print(median(movies_summary$score))

movies_summary.unnested <- movies.unnested %>%
  group_by(Movie.ID, Movie.Title, Release.Year, Genre) %>%
  summarise(num_convs = n(),
            num_formal = sum(formality == "formal"),
            num_informal = sum(formality == "informal"),
            score = num_formal / num_convs) %>%
  filter(num_convs >= 20)

# Extract the decade from the year
movies_summary$Decade <- as.numeric(substr(movies_summary$Release.Year, 1, 3)) * 10
# Group movies into 10-year intervals
movies_summary$Decade_Group <- paste(movies_summary$Decade, "-", movies_summary$Decade + 9)
min_num_movies_per_year = 7
min_num_movies_per_decade = 20

#Plotting Median Score of all Movies Over the Years
movies_summary %>% 
  group_by(Release.Year) %>% 
  summarise(num_movies = n(), median_score = median(score, na.rm = TRUE)) %>%
  filter(num_movies >= min_num_movies_per_year) %>%
  ggplot(aes(x = Release.Year, y = median_score)) +
  geom_line(group = 1) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "red") +
  scale_x_discrete(breaks = seq(1955, 2010, by = 5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Median Score of all Movies Over the Years", x = "Release Year", y = "Median Score")

#Plotting Median Score of all Movies Decade-wise
movies_summary %>% group_by(Decade_Group) %>% 
  summarise(num_movies = n(), median_score = median(score, na.rm = T)) %>%
  filter(num_movies > min_num_movies_per_decade) %>% 
  ggplot(aes(x = Decade_Group, y = median_score)) +
  geom_line(group = 1) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Median Score of all Movies Decade-wise", x = "Release Year", y = "Median Score")

# Combine genres into broader categories
movies_summary_combined <- movies_summary.unnested %>%
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
  )) %>%
  filter(!Genre_Combined %in% c("film-noir", "short", "adult", "sport", "western", "comedy"))


# Check unique values in the combined genre category
unique(movies_summary_combined$Genre_Combined)

min_num_movies_per_year_per_category = 5
# Calculate average score for each combined genre in each year
genre_summary_avg_combined <- movies_summary_combined %>%
  group_by(Release.Year, Genre_Combined) %>%
  filter(n() >= min_num_movies_per_year_per_category) %>%
  summarise(
    median_score = median(score, na.rm = TRUE)
  )

# Plot the average score for each combined genre over the years
genre_summary_avg_combined %>%
  ggplot(aes(x = Release.Year, y = median_score, color = Genre_Combined, group = Genre_Combined)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(limits = c(0, 0.15)) +
  scale_x_discrete(breaks = seq(1955, 2010, by = 5)) +
  labs(title = "Median Score for Each Combined Genre Over the Years \nwith at least 5 movies per category per year",
       x = "Release Year", y = "Median Score")
