df = read.csv("reddit_posts_progressive.csv")
df
df = df %>% filter(Text == ",")