library(tidyverse)
library(dplyr)

filename = "feminism.csv"
link = str_c("data/reddit_", filename)

df = read.csv(link)
df = df %>% select(-X)

# Remove deleted columns
df <- df %>% mutate(Text = gsub("\\[deleted\\], ", "", Text))
df = df %>% filter(Text != "")
df = df %>% filter(Text != ", ")

write.csv(df, filename)