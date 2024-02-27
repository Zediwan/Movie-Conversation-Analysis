library(tidyverse)
library(quanteda)
library(dplyr)

subreddit = "feminism"
link = str_c("data/big_reddit_", subreddit)
filename = str_c (link, ".csv")

df = read.csv(filename)
df = df %>% select(-X)

# Remove deleted columns
df <- df %>% mutate(Text = gsub("\\[deleted\\], ", "", Text))
df = df %>% filter(Text != "")
df = df %>% filter(Text != ", ")

# Remove short convs
threshold <- 20  # Min length of conv
corpus <- corpus(df$Text)
text_lengths <- nchar(corpus)
df <- df[text_lengths >= threshold, ] # Filter out rows with text length below the threshold

# Shrink in size to not have to large files for github
df = df %>% head(30000)

link = str_c("data/reddit_", subreddit)
filename = str_c (link, ".csv")
write.csv(df, filename)
