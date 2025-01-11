# Load required libraries
library(tidyverse)
library(tidytext)
library(tm)
library(ggplot2)

###########
# Text cleaning
###########


tweets <- read_csv("covid19_tweets.csv")

clean_text <- function(text) {
  text %>%
    tolower() %>%
    str_replace_all("http\\S+|www\\S+", "") %>%  # Remove URLs
    str_replace_all("@\\w+", "") %>%             # Remove mentions
    str_replace_all("#\\w+", "") %>%             # Remove hashtags
    str_replace_all("[^a-zA-Z\\s]", "")          # Remove special characters
}

###########
# Tokenization, Stopword Removal and sentiment calculation - BING 
###########

analyze_bing <- function(tweets_df) {
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text), id = row_number())
  
  data("stop_words")
  tweets_tokenized <- tweets_processed %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word")
  
  bing_scores <- tweets_tokenized %>%
    inner_join(get_sentiments("bing"), by = "word",relationship = "many-to-many") %>%
    count(id, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(bing_sentiment_score = positive - negative)
  
  return(bing_scores)
}

###########
# Tokenization, Stopword Removal and sentiment calculation  - AFINN 
###########

analyze_afinn <- function(tweets_df) {
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text), id = row_number())
  
  tweets_tokenized <- tweets_processed %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word")
  
  afinn_scores <- tweets_tokenized %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarise(afinn_sentiment_score = sum(value))
  
  return(afinn_scores)
}

###########
# Tokenization, Stopword Removal and sentiment calculation - NRC 
###########

analyze_nrc <- function(tweets_df) {
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text), id = row_number())
  
  tweets_tokenized <- tweets_processed %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word")
  
  nrc_scores <- tweets_tokenized %>%
    inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%
    count(id, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(
      nrc_positive_score = positive,
      nrc_negative_score = negative
    )
  
  return(nrc_scores)
}




# Run individual analyses
bing_results <- analyze_bing(tweets)
afinn_results <- analyze_afinn(tweets)
nrc_results <- analyze_nrc(tweets)

# View results
print(bing_results)
print(afinn_results)
print(nrc_results)

###########
#  Bing Lexicon Sentiment Score Distribution
###########

plot_bing_sentiment <- function(bing_results) {
  ggplot(bing_results, aes(x = bing_sentiment_score)) +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(
      title = "Bing Lexicon Sentiment Score Distribution",
      x = "Bing Sentiment Score",
      y = "Count"
    )
}

# Plotting Bing sentiment scores
plot_bing_sentiment(bing_results)

###########
#  AFINN Lexicon Sentiment Score Distribution
###########

plot_afinn_sentiment <- function(afinn_results) {
  ggplot(afinn_results, aes(x = afinn_sentiment_score)) +
    geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
    theme_minimal() +
    labs(
      title = "AFINN Lexicon Sentiment Score Distribution",
      x = "AFINN Sentiment Score",
      y = "Count"
    )
}

# Plotting AFINN sentiment scores
plot_afinn_sentiment(afinn_results)

###########
#  NRC Lexicon Sentiment Score Distribution
###########


plot_nrc_sentiment <- function(nrc_results) {
  nrc_long <- nrc_results %>%
    pivot_longer(cols = c(nrc_positive_score, nrc_negative_score),
                 names_to = "sentiment_type", values_to = "count")
  
  print(nrc_long)
  
  # Summarize the counts by sentiment_type
  nrc_summary <- nrc_long %>%
    group_by(sentiment_type) %>%
    summarise(total_count = sum(count, na.rm = TRUE))
  
  print(nrc_summary)
  
  # Plotting the summed counts
  ggplot(nrc_summary, aes(x = sentiment_type, y = total_count, fill = sentiment_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(
      title = "NRC Lexicon Positive and Negative Counts (Summed)",
      x = "Sentiment Type",
      y = "Total Count"
    ) +
    scale_fill_manual(values = c("nrc_positive_score" = "cadetblue2", "nrc_negative_score" = "lightcoral"))
}

# Plotting NRC sentiment scores
plot_nrc_sentiment(nrc_results)

###########
#  Positive and Negative words - BING
###########

# For Bing, count the positive and negative words directly.
bing_pos_neg_counts <- function(bing_results) {
  bing_counts <- bing_results %>%
    pivot_longer(cols = c(positive, negative), names_to = "sentiment", values_to = "count") %>%
    group_by(sentiment) %>%
    summarise(total = sum(count))
  
  return(bing_counts)
}

bing_counts <- bing_pos_neg_counts(bing_results)
bing_counts

###########
#  Positive and Negative words - AFINN
###########

afinn_pos_neg_counts <- function(tweets_df) {
  tweets_tokenized <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text)) %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word") %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    mutate(sentiment = if_else(value > 0, "positive", "negative"))
  
  afinn_counts <- tweets_tokenized %>%
    count(sentiment) %>%
    rename(total = n)
  
  return(afinn_counts)
}

afinn_counts <- afinn_pos_neg_counts(tweets)
afinn_counts

###########
#  Positive and Negative words - NRC
###########

nrc_pos_neg_counts <- function(nrc_results) {
  nrc_counts <- nrc_results %>%
    select(nrc_positive_score, nrc_negative_score) %>%
    summarise(
      positive = sum(nrc_positive_score),
      negative = sum(nrc_negative_score)
    ) %>%
    pivot_longer(cols = everything(), names_to = "sentiment", values_to = "total")
  
  return(nrc_counts)
}

nrc_counts <- nrc_pos_neg_counts(nrc_results)
nrc_counts

plot_pos_neg_counts <- function(bing_counts, afinn_counts, nrc_counts) {
  combined_counts <- bind_rows(
    bing_counts %>% mutate(lexicon = "Bing"),
    afinn_counts %>% mutate(lexicon = "AFINN"),
    nrc_counts %>% mutate(lexicon = "NRC")
  )
  
  ggplot(combined_counts, aes(x = lexicon, y = total, fill = sentiment)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(
      title = "Positive and Negative Word Counts by Lexicon",
      x = "Lexicon",
      y = "Count"
    ) +
    scale_fill_manual(values = c("positive" = "cadetblue2", "negative" = "lightcoral"))
}

# Plotting positive and negative word counts for each lexicon
plot_pos_neg_counts(bing_counts, afinn_counts, nrc_counts)



###########
# Most Common Positive and Negative Words - BING Lexicon
###########

plot_bing_common_words <- function(tweets_df) {
  # Process tweets with Bing lexicon
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text))
  
  tweets_tokenized <- tweets_processed %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word") %>%
    inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many")
  
  print(tweets_tokenized)
  
  # Count most common positive and negative words
  common_words <- tweets_tokenized %>%
    count(word, sentiment, sort = TRUE) %>%
    group_by(sentiment) %>%
    slice_max(n = 10, order_by = n) %>%
    ungroup()
  
  print(common_words)
  
  # Plot
  ggplot(common_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Most Common Positive and Negative Words - Bing Lexicon",
      x = "Word",
      y = "Count"
    ) +
    scale_fill_manual(values = c("positive" = "cadetblue2", "negative" = "lightcoral"))
}

# Usage
plot_bing_common_words(tweets)

###########
# Most Common Positive and Negative Words - AFINN Lexicon
###########

plot_afinn_common_words <- function(tweets_df) {
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text))
  
  tweets_tokenized <- tweets_processed %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word") %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    mutate(sentiment = if_else(value > 0, "positive", "negative"))
  
  # Count most common positive and negative words
  common_words <- tweets_tokenized %>%
    count(word, sentiment, sort = TRUE) %>%
    group_by(sentiment) %>%
    slice_max(n = 10, order_by = n) %>%
    ungroup()
  
  # Plot
  ggplot(common_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Most Common Positive and Negative Words - AFINN Lexicon",
      x = "Word",
      y = "Count"
    ) +
    scale_fill_manual(values = c("positive" = "cadetblue2", "negative" = "lightcoral"))
}

# Usage
plot_afinn_common_words(tweets)

###########
# Most Common Positive and Negative Words - NRC Lexicon
###########

plot_nrc_common_words <- function(tweets_df) {
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text))
  
  tweets_tokenized <- tweets_processed %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word") %>%
    inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%
    filter(sentiment %in% c("positive", "negative"))
  
  # Count most common positive and negative words
  common_words <- tweets_tokenized %>%
    count(word, sentiment, sort = TRUE) %>%
    group_by(sentiment) %>%
    slice_max(n = 10, order_by = n) %>%
    ungroup()
  
  # Plot
  ggplot(common_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Most Common Positive and Negative Words - NRC Lexicon",
      x = "Word",
      y = "Count"
    ) +
    scale_fill_manual(values = c("positive" = "cadetblue2", "negative" = "lightcoral"))
}

# Usage
plot_nrc_common_words(tweets)

##############
# wordcloud
#############
tweets_processed <- tweets %>%
  mutate(cleaned_text = map_chr(text, clean_text), id = row_number())

tweets_tokenized <- tweets_processed %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words, by = "word")

##############
# wordcloud - BING
#############

# Load required library for word cloud
library(wordcloud)

# Define word cloud functions for each lexicon
generate_bing_wordcloud <- function(tweets_tokenized) {
  bing_words <- tweets_tokenized %>%
    inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
    count(word, sentiment, sort = TRUE)
  
  # Separate positive and negative words for Bing
  positive_words <- bing_words %>% filter(sentiment == "positive")
  negative_words <- bing_words %>% filter(sentiment == "negative")
  
  # Plot positive word cloud for Bing
  wordcloud(words = positive_words$word, freq = positive_words$n, max.words = 100,
            colors = brewer.pal(8, "Blues"), random.order = FALSE)
  
  # Plot negative word cloud for Bing
  wordcloud(words = negative_words$word, freq = negative_words$n, max.words = 100,
            colors = brewer.pal(8, "Reds"), random.order = FALSE)
}

##############
# wordcloud - AFINN
#############

generate_afinn_wordcloud <- function(tweets_tokenized) {
  afinn_words <- tweets_tokenized %>%
    inner_join(get_sentiments("afinn"), by = "word", relationship = "many-to-many") %>%
    mutate(sentiment = if_else(value > 0, "positive", "negative")) %>%
    count(word, sentiment, sort = TRUE)
  
  # Separate positive and negative words for AFINN
  positive_words <- afinn_words %>% filter(sentiment == "positive")
  negative_words <- afinn_words %>% filter(sentiment == "negative")
  
  # Plot positive word cloud for AFINN
  wordcloud(words = positive_words$word, freq = positive_words$n, max.words = 100,
            colors = brewer.pal(8, "Greens"), random.order = FALSE)
  
  # Plot negative word cloud for AFINN
  wordcloud(words = negative_words$word, freq = negative_words$n, max.words = 100,
            colors = brewer.pal(8, "Oranges"), random.order = FALSE)
}

##############
# wordcloud - NRC
#############

generate_nrc_wordcloud <- function(tweets_tokenized) {
  nrc_words <- tweets_tokenized %>%
    inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count(word, sentiment, sort = TRUE)
  
  # Separate positive and negative words for NRC
  positive_words <- nrc_words %>% filter(sentiment == "positive")
  negative_words <- nrc_words %>% filter(sentiment == "negative")
  
  # Plot positive word cloud for NRC
  wordcloud(words = positive_words$word, freq = positive_words$n, max.words = 100,
            colors = brewer.pal(8, "Purples"), random.order = FALSE)
  
  # Plot negative word cloud for NRC
  wordcloud(words = negative_words$word, freq = negative_words$n, max.words = 100,
            colors = brewer.pal(8, "Greys"), random.order = FALSE)
}

# Generate word clouds for each lexicon
generate_bing_wordcloud(tweets_tokenized)
generate_afinn_wordcloud(tweets_tokenized)
generate_nrc_wordcloud(tweets_tokenized)


################
# TF-IDF Implementation
###############

head(tweets)

# Define a function to compute TF-IDF with id column added
compute_tf_idf <- function(tweets_df) {
  # Add an id column if it doesn't already exist
  tweets_df <- tweets_df %>%
    mutate(id = row_number())  # Add a row number as a unique document ID
  
  # Process tweets and tokenize
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text)) %>%
    unnest_tokens(word, cleaned_text)
  
  # Calculate term frequency (TF)
  tf <- tweets_processed %>%
    count(id, word, sort = TRUE) %>%
    group_by(id) %>%
    mutate(tf = n / sum(n)) %>%
    ungroup()
  
  # Calculate inverse document frequency (IDF)
  idf <- tweets_processed %>%
    count(word) %>%
    mutate(idf = log(nrow(tweets_df) / n))
  
  # Calculate TF-IDF by joining TF and IDF
  tf_idf <- tf %>%
    left_join(idf, by = "word") %>%
    mutate(tf_idf = tf * idf)
  
  return(tf_idf)
}

# Run TF-IDF calculation
tf_idf_results <- compute_tf_idf(tweets)

# View the results
head(tf_idf_results)


# Visualizing the top TF-IDF words
plot_tf_idf <- function(tf_idf_results) {
  top_tf_idf <- tf_idf_results %>%
    arrange(desc(tf_idf)) %>%
    slice_head(n = 20) %>%
    mutate(word = reorder(word, tf_idf))
  
  ggplot(top_tf_idf, aes(x = word, y = tf_idf, fill = tf_idf)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Top 20 Words by TF-IDF",
      x = "Word",
      y = "TF-IDF Score"
    ) +
    scale_fill_viridis_c()
}

# Plot the top 20 words by TF-IDF
plot_tf_idf(tf_idf_results)

# Calculate Document Frequency (DF) for each word
compute_document_frequency <- function(tweets_df) {
  tweets_processed <- tweets_df %>%
    mutate(cleaned_text = map_chr(text, clean_text)) %>%
    unnest_tokens(word, cleaned_text)
  
  # Calculate document frequency (DF) by counting number of documents each word appears in
  df <- tweets_processed %>%
    distinct(id, word) %>%
    count(word) %>%
    rename(df = n)
  
  return(df)
}

# Run document frequency calculation
df_results <- compute_document_frequency(tweets)

# Visualizing the document frequency of words
plot_document_frequency <- function(df_results) {
  top_df <- df_results %>%
    arrange(desc(df)) %>%
    slice_head(n = 20) %>%
    mutate(word = reorder(word, df))
  
  ggplot(top_df, aes(x = word, y = df, fill = df)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Top 20 Words by Document Frequency",
      x = "Word",
      y = "Document Frequency"
    ) +
    scale_fill_viridis_c()
}

# Plot the top 20 words by document frequency
plot_document_frequency(df_results)

