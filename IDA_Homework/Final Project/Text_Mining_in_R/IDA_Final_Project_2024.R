# Load necessary libraries
library(tidyverse)
library(tidytext)
library(tm)
library(ggplot2)
library(caTools)
library(caret)
library(lubridate)

# Load Dataset
tweets <- read_csv("covid19_tweets.csv")

# Data Structure and Summary
str(tweets)
glimpse(tweets)
summary(tweets)

# Function to summarize missing values
missing_summary <- function(data) {
  data %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count")
}

# Visualize Missing Data
plot_missing_data <- function(missing_data) {
  ggplot(missing_data, aes(x = reorder(column, missing_count), y = missing_count)) +
    geom_bar(stat = "identity", fill = "lightcoral") +
    coord_flip() +
    labs(title = "Missing Values per Column", x = "Column", y = "Number of Missing Values")
}


# Check for Invalid Dates, Empty Text, and Duplicates
check_data_quality <- function(data) {
  list(
    invalid_dates = data %>% filter(!is.Date(as.Date(date))) %>% nrow(),
    empty_text_count = data %>% filter(text == "" | is.na(text)) %>% nrow(),
    duplicate_rows = data %>% duplicated() %>% sum(),
    duplicate_ids = if ("id" %in% colnames(data)) data %>% count(id) %>% filter(n > 1) %>% nrow() else NA
  )
}

# Create Data Quality Summary Table
create_summary_table <- function(data, missing_data, quality_checks) {
  data.frame(
    Metric = c("Total Rows", "Missing Values", "Duplicate Rows", "Duplicate IDs",
               "Invalid Dates", "Empty Text Entries"),
    Count = c(nrow(data), sum(missing_data$missing_count), quality_checks$duplicate_rows,
              quality_checks$duplicate_ids, quality_checks$invalid_dates, quality_checks$empty_text_count)
  )
}

# Clean Text Function
clean_text <- function(text) {
  text %>%
    tolower() %>%
    str_replace_all("http\\S+|www\\S+", "") %>%
    str_replace_all("@\\w+", "") %>%
    str_replace_all("#\\w+", "") %>%
    str_replace_all("[^a-zA-Z\\s]", "")
}

# Apply Cleaning and Add ID
tweets <- tweets %>%
  mutate(cleaned_text = map_chr(text, clean_text), id = row_number())

# Tokenization and Stopword Removal
data("stop_words")
tweets_tokenized <- tweets %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words, by = "word")

# Sentiment Scoring Functions
calculate_bing_sentiment <- function(data) {
  bing_sentiments <- get_sentiments("bing")
  data %>%
    inner_join(bing_sentiments, by = "word") %>%
    count(id, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(bing_sentiment_score = positive - negative) %>%
    select(id, bing_sentiment_score)
}

calculate_afinn_sentiment <- function(data) {
  afinn_sentiments <- get_sentiments("afinn")
  data %>%
    inner_join(afinn_sentiments, by = "word") %>%
    group_by(id) %>%
    summarise(afinn_sentiment_score = sum(value))
}

calculate_nrc_sentiment <- function(data) {
  nrc_sentiments <- get_sentiments("nrc")
  data %>%
    inner_join(nrc_sentiments, by = "word") %>%
    count(id, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(nrc_positive_score = positive, nrc_negative_score = negative) %>%
    select(id, nrc_positive_score, nrc_negative_score)
}

# Merge Sentiment Scores
bing_scores <- calculate_bing_sentiment(tweets_tokenized)
afinn_scores <- calculate_afinn_sentiment(tweets_tokenized)
nrc_scores <- calculate_nrc_sentiment(tweets_tokenized)

tweets <- tweets %>%
  left_join(bing_scores, by = "id") %>%
  left_join(afinn_scores, by = "id") %>%
  left_join(nrc_scores, by = "id") %>%
  mutate(
    combined_sentiment_score = (bing_sentiment_score + afinn_sentiment_score +
                                  nrc_positive_score - nrc_negative_score) / 3,
    net_sentiment = nrc_positive_score - nrc_negative_score
  )

# Visualize Net Sentiment Score Distribution
ggplot(tweets, aes(x = net_sentiment)) +
  geom_histogram(binwidth = 1, fill = "cyan", color = "black") +
  theme_minimal() +
  labs(title = "Net Sentiment Score Distribution", x = "Net Sentiment Score", y = "Count")

# Top Positive and Negative Words
plot_top_words <- function(data, sentiment) {
  data %>%
    filter(sentiment == sentiment) %>%
    count(word, sort = TRUE) %>%
    top_n(20) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_bar(stat = "identity", fill = ifelse(sentiment == "positive", "lightblue", "pink")) +
    coord_flip() +
    labs(title = paste("Top", sentiment, "Words"), x = "Word", y = "Frequency")
}

positive_words_plot <- plot_top_words(bing_sentiments, "positive")
negative_words_plot <- plot_top_words(bing_sentiments, "negative")

# Define Sentiment Categories
tweets <- tweets %>%
  mutate(sentiment_category = case_when(
    combined_sentiment_score > 0 ~ "Positive",
    combined_sentiment_score < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Plot Sentiment Distribution by Category
sentiment_counts <- tweets %>% count(sentiment_category)
ggplot(sentiment_counts, aes(x = sentiment_category, y = n, fill = sentiment_category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Tweets by Sentiment Category", x = "Sentiment Category", y = "Number of Tweets") +
  scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red", "Neutral" = "gray"))

# Topic-Based Sentiment Analysis
topics <- c("vaccine", "mask", "quarantine")
topic_sentiments <- map_df(topics, function(topic) {
  tweets %>%
    filter(str_detect(text, topic)) %>%
    summarise(
      topic = topic,
      avg_bing = mean(bing_sentiment_score, na.rm = TRUE),
      avg_afinn = mean(afinn_sentiment_score, na.rm = TRUE),
      avg_nrc_positive = mean(nrc_positive_score, na.rm = TRUE),
      avg_nrc_negative = mean(nrc_negative_score, na.rm = TRUE)
    )
})

# Plot Topic-Based Sentiment Scores
topic_sentiments_long <- topic_sentiments %>%
  pivot_longer(cols = starts_with("avg"), names_to = "lexicon", values_to = "average_score")

ggplot(topic_sentiments_long, aes(x = topic, y = average_score, fill = lexicon)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Average Sentiment Score by Topic and Lexicon", x = "Topic", y = "Average Sentiment Score")

# Sentiment Trends Over Time
tweets <- tweets %>% mutate(date = as.Date(date))
daily_sentiment <- tweets %>%
  group_by(date) %>%
  summarise(
    avg_bing = mean(bing_sentiment_score, na.rm = TRUE),
    avg_afinn = mean(afinn_sentiment_score, na.rm = TRUE),
    avg_nrc_positive = mean(nrc_positive_score, na.rm = TRUE),
    avg_nrc_negative = mean(nrc_negative_score, na.rm = TRUE)
  )

ggplot(daily_sentiment, aes(x = date)) +
  geom_line(aes(y = avg_bing, color = "Bing")) +
  geom_line(aes(y = avg_afinn, color = "AFINN")) +
  geom_line(aes(y = avg_nrc_positive, color = "NRC Positive")) +
  geom_line(aes(y = avg_nrc_negative, color = "NRC Negative")) +
  theme_minimal() +
  labs(title = "Sentiment Scores Over Time", x = "Date", y = "Average Sentiment Score") +
  scale_color_manual(name = "Lexicons", values = c("Bing" = "blue", "AFINN" = "orange", "NRC Positive" = "green", "NRC Negative" = "red"))


library(wordcloud)
plot_bing_wordcloud <- function(tweets_df) {
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text)) %>%
    unnest_tokens(word, cleaned_text) %>%
    inner_join(get_sentiments("bing"), by = "word")
  
  # Separate positive and negative words
  positive_words <- tweets_processed %>%
    filter(sentiment == "positive") %>%
    count(word, sort = TRUE)
  
  negative_words <- tweets_processed %>%
    filter(sentiment == "negative") %>%
    count(word, sort = TRUE)
  
  # Plot positive word cloud
  wordcloud(words = positive_words$word, freq = positive_words$n, min.freq = 1,
            colors = brewer.pal(8, "Blues"), scale = c(3, 0.5), max.words = 100)
  title("Bing Lexicon - Positive Words")
  
  # Plot negative word cloud
  wordcloud(words = negative_words$word, freq = negative_words$n, min.freq = 1,
            colors = brewer.pal(8, "Reds"), scale = c(3, 0.5), max.words = 100)
  title("Bing Lexicon - Negative Words")
}

# Usage
plot_bing_wordcloud(tweets)



