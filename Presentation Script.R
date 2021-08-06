## Example for presentation
## Using https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/
## https://www.tidytextmining.com/dtm.html#cast-dtm
## https://www.tidytextmining.com/sentiment.html

library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)

## Find some climate tweets
## You will need a twitter account for this
climate_tweets <- search_tweets(q = "#climatechange", n = 10000,
                                lang = "en",
                                include_rts = FALSE)

## Add Document number
climate_tweets <- tibble::rowid_to_column(climate_tweets, "document")

## Clean https
climate_tweets <- climate_tweets %>% mutate(stripped_text = gsub("http.*","",  text))
climate_tweets <- climate_tweets %>% mutate(stripped_text = gsub("https.*","", stripped_text))

## cast to tidy
climate_tweets_clean <- climate_tweets %>%
  select(document, stripped_text) %>%
  unnest_tokens(word, stripped_text)

## remove stopwords
climate_tweets_clean <- climate_tweets_clean %>% anti_join(stop_words)

## plot top 15 words
climate_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Tagged with #climatechange") +
  theme_bw()

## Sentiment analysis
## by tweet
Tweet_sentiment <- climate_tweets_clean %>% inner_join(get_sentiments("nrc")) %>% 
  count(document, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

summary(Tweet_sentiment$sentiment)

## by Word
Word_sentiment <- climate_tweets_clean %>% inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()

Word_sentiment %>% filter(sentiment %in% c("positive","negative")) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) + 
  theme_bw()

rm(climate_tweets, Tweet_sentiment, Word_sentiment)
## Topic Modeling
## create word count
Tidy_word_count <- climate_tweets_clean %>% count(document, word)

## Create document term matrix
Tweets_dtm <- Tidy_word_count %>% cast_dtm(document, word, n)

## Time for LDA
library(topicmodels)
tweet_lda <- LDA(Tweets_dtm, k = 2, control = list(seed = 8675309))
tweet_lda

Tweet_topics <- tidy(tweet_lda, matrix = "beta")
Tweet_topics

## Let's find the terms that most contribute to topics
Top_terms <- Tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

## Graph it!
Top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + 
  theme_bw()


beta_spread <- Tweet_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL) + 
  theme_bw()

