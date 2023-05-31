
# Exercise Subset 
UK23<-UK %>%
  filter(str_detect(date, "2023"))


#Plot the 2023 posts:
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(UK23, aes(x = date, y =Sentiment, colour=Sentiment)) +
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/UK VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  geom_hline(yintercept=0, linetype='dotted')