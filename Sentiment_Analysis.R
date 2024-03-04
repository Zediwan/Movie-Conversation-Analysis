library(quanteda)
library(quanteda.textstats)
library(quanteda.corpora)
library(tidyverse)

#Importing sentiment dataset
lsd.dict <- data_dictionary_LSD2015 

#Creating corups and dfm
courpus_sentiment <- corpus(movies$text)
dfm_sentiment <- dfm(courpus_sentiment, remove = stopwords("en"),
           remove_punct = TRUE, stem = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)

#Apply sentiment dictionary
sent.analysis <- dfm_sentiment %>% 
  dfm_lookup(dictionary = lsd.dict[1:2])
head(sent.analysis)

#Convert to dataframe
sent.analysis <- sent.analysis %>% 
  convert(to = "data.frame")

#Creating sentiment score
sent.analysis <- sent.analysis %>% 
  mutate(length = ntoken(dfm_sentiment),
         sentiment.score = (positive - negative)/length)

#Creating a df with all the observations and the sentiment score
movie_sentiment <- convert(courpus_sentiment, to = "data.frame")
movie_sentiment <-left_join(movie_sentiment, sent.analysis, by = "doc_id") %>% 
  distinct(text, .keep_all = TRUE)

movie_sentiment <- left_join(movies, movie_sentiment, by = "text")

#filter out observations with less than 4 movies per decade and genre
movie_sentiment_filtred <- movie_sentiment %>% 
  group_by(Release.Year, Genre) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  filter(count > 4) 

movie_sentiment_filtred <- movie_sentiment %>% 
  semi_join(movie_sentiment_filtred, by = (c("Release.Year", "Genre")))


#
genre_year_average_sentiment <- movie_sentiment_filtred %>%
  mutate(Genre = case_when(
    Genre %in% c("action", "adventure") ~ "Action/Adventure",
    Genre %in% c("drama", "romance") ~ "Drama/Romance",
    Genre %in% c("sci-fi", "fantasy") ~ "Sci-Fi/Fantasy",
    Genre %in% c("crime", "mystery", "thriller") ~ "Crime/Mystery/Thriller",
    Genre == "horror" ~ "Horror/Thriller",
    Genre %in% c("musical", "music") ~ "Musical/Music",
    Genre %in% c("biography", "documentary") ~ "Biography/Documentary",
    Genre %in% c("animation", "family") ~ "Animation/Family",
    Genre %in% c("history", "war") ~ "History/War",
    TRUE ~ as.character(Genre))) %>% 
  mutate(decade =  as.numeric(substr(Release.Year, 1, 3)) * 10) %>% 
  group_by(decade, Genre) %>% 
  summarise(
    mean_score = mean(sentiment.score, na.rm = TRUE)
  )


genre_year_average_sentiment %>%
  ggplot(aes(x = decade, y = mean_score, color = Genre, group = Genre)) +
  geom_line() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Average Score for Each Combined Genre Decade-wise",
       x = "Decade", y = "Average Score")



  
