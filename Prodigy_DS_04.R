#install.packages(c("tidyverse", "tidytext", "tm", "wordcloud", "syuzhet"))
#install.packages("reshape")
library(tidyverse)    # For data manipulation and visualization
library(tidytext)     # For text mining
library(syuzhet)      # For sentiment analysis
library(ggplot2)      # For creating visualizations
library(wordcloud)    # For creating word clouds
library(reshape2)     # For reshaping data
library(readr)
twitter_data<- read_csv("twitter_training.csv")
View(head(twitter_data))

# Display the structure and summary of the dataset
glimpse(twitter_data)
summary(twitter_data)

# Basic Data Overview
head(twitter_data)
summary(twitter_data)

# Data Preprocessing: Remove unnecessary columns and NA values
data_clean <- twitter_data %>%
  #select(-X1) %>%  # Remove index column
  filter(!is.na(Sentiments))

# Sentiment Analysis using NRC lexicon via syuzhet package
# Clean the text data
twitter_data$Cleaned_Tweet <- iconv(twitter_data$Tweet, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
twitter_data$Cleaned_Tweet <- gsub("[^[:alnum:][:space:]]", "", twitter_data$Cleaned_Tweet)

# Now apply get_nrc_sentiment
sentiments <- get_nrc_sentiment(twitter_data$Cleaned_Tweet)

# Combine the sentiment scores with the original data
twitter_data <- cbind(twitter_data, sentiments)

# Summarize sentiment by Topic
sentiment_summary <- twitter_data %>%
  group_by(Topic) %>%
  summarise(positive = mean(positive),
            negative = mean(negative),
            anger = mean(anger),
            anticipation = mean(anticipation),
            disgust = mean(disgust),
            fear = mean(fear),
            joy = mean(joy),
            sadness = mean(sadness),
            surprise = mean(surprise),
            trust = mean(trust))

# Reshape data for visualization
sentiment_summary_melt <- melt(sentiment_summary, id.vars = "Topic")

# Visualize sentiment distribution
ggplot(twitter_data, aes(x = Sentiments, fill = Sentiments)) +
  geom_bar() +
  facet_wrap(~ Topic, scales = "free_y") +
  theme_minimal() +
  labs(title = "Sentiment Distribution Across Topics",
       x = "Sentiments",
       y = "Count")

# Visualize overall sentiment distribution
ggplot(twitter_data, aes(x = Sentiments, fill = Sentiments)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Overall Sentiment Distribution",
       x = "Sentiments",
       y = "Count")

# Create a word cloud for positive and negative words
positive_words <- twitter_data %>%
  filter(Sentiments == "Positive") %>%
  unnest_tokens(word, Tweet) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(50)

negative_words <- twitter_data %>%
  filter(Sentiments == "Negative") %>%
  unnest_tokens(word, Tweet) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(50)

# Word Cloud for Positive Words
wordcloud(positive_words$word, positive_words$n, max.words = 100, colors = brewer.pal(8, "Blues"))

# Word Cloud for Negative Words
wordcloud(negative_words$word, negative_words$n, max.words = 100, colors = brewer.pal(8, "Reds"))
