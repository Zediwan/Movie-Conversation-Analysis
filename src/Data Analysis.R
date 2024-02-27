library(tidyverse)
library(quanteda)

df.movie_conversation = read.csv("data/movie_conversation.csv")

threshold <- 50  # Min length of conv

number_of_movies = df.movie_conversation %>% select(Movie.Title) %>% unique() %>% nrow()

corpus <- corpus(df.movie_conversation$Text)
text_lengths <- nchar(corpus)
df.movie_conversation <- df.movie_conversation[text_lengths >= threshold, ] # Filter out rows with text length below the threshold
#write.csv(df.movie_conversation, "data/movie_conversation.csv")

df.movie_conversation.releases_per_year = df.movie_conversation %>% group_by(Release.Year) %>% summarise(num_releases = length(unique(Movie.Title)))
df.movie_conversation.releases_per_year %>% ggplot(aes(x=Release.Year, y = num_releases)) + geom_line()                                           