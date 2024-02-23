library(tidyverse)

filename = "reddit_posts_progressive.csv"
#link = str_c("data/", filename)
df = read.csv(filename)
df = df %>% rename(ID = X)
write.csv(df, filename)
