library(tidyverse)


df = read.csv("movie_conversation.csv")

df = df %>% rename(ID = X)

number_of_movies = df %>% select(Movie.Title) %>% unique() %>% nrow()

df.releases_per_year = df %>% group_by(Release.Year) %>% summarise(num_releases = length(unique(Movie.Title)))
df.releases_per_year %>% ggplot(aes(x=Release.Year, y = num_releases)) + geom()                                             
