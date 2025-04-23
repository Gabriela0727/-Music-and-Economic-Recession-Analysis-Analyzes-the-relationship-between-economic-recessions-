# -Music-and-Economic-Recession-Analysis-Analyzes-the-relationship-between-economic-recessions-
# Analyzes the relationship between economic recessions and music features # Comparing 2007-2009 Great Recession with 2023-2024 data

# Load required libraries
install.packages("RColorBrewer")
install.packages("ggplot2")
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(tidytext)  # For text analysis
library(wordcloud)  # For word clouds
library(textdata)   # For sentiment lexicons
library(igraph)     # For network analysis
library(ggraph)     # For network visualization
library(spotifyr)
library(geniusr)
library(quantmod)

# Set Spotify credentials 
Sys.setenv(SPOTIFY_CLIENT_ID = "EOuTWJduZHq2T_ZsLrSGVRqyuoFPMCqkjHq1Iw7JiXFS4Fke1b7X31Qbt0KTV6")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "ElOv1ajmxvob6OuiJG_iFFxPPAmRBW17ntHGUgpd70m38vAlJk3uW7orOe-PIB9ZfCGi_7JgPaTyYTRSPi7KDdg")

# Define time periods for analysis
pre_recession_2007 <- c("2007-01-01", "2007-11-30")  # Before recession
recession_2008_2009 <- c("2007-12-01", "2009-06-30") # During recession
post_recession_2009 <- c("2009-07-01", "2010-01-31") # After recession
current_period <- c("2023-01-01", "2024-10-31")      # Current period

# 1. Prepare Billboard data
# Combine billboard_24years_lyrics_spotify with charts to get complete date information
billboard_data <- billboard_24years_lyrics_spotify %>%
  left_join(charts, by = c("song", "band_singer" = "artist"), relationship = "many-to-many") %>%
  # Convert date to proper format
  mutate(date = as.Date(date)) %>%
  # Filter for relevant time periods
  filter((date >= as.Date(pre_recession_2007[1]) & date <= as.Date(post_recession_2009[2])) |
           (date >= as.Date(current_period[1]) & date <= as.Date(current_period[2]))) %>%
  # Add period classification
  mutate(period = case_when(
    date >= as.Date(pre_recession_2007[1]) & date <= as.Date(pre_recession_2007[2]) ~ "pre_recession_2007",
    date >= as.Date(recession_2008_2009[1]) & date <= as.Date(recession_2008_2009[2]) ~ "recession_2008_2009",
    date >= as.Date(post_recession_2009[1]) & date <= as.Date(post_recession_2009[2]) ~ "post_recession_2009",
    date >= as.Date(current_period[1]) & date <= as.Date(current_period[2]) ~ "current_period"
  )) %>%
  # Remove duplicates after joining
  distinct(song, band_singer, date, .keep_all = TRUE)

# 2. For each month, keep only top 20 songs
top_20_songs <- billboard_data %>%
  # Group by year-month and rank
  mutate(year_month = floor_date(date, "month")) %>%
  group_by(year_month) %>%
  # Get top 20 by rank
  slice_min(order_by = rank, n = 20) %>%
  ungroup()

# 3. Process top_10000_1950.now dataset with better date handling
top_songs_filtered <- top_10000_1950.now %>%
  # First, clean up the dates before conversion
  mutate(
    # Create a clean date column for conversion
    clean_date = case_when(
      # For full dates (YYYY-MM-DD)
      grepl("^\\d{4}-\\d{2}-\\d{2}$", Album.Release.Date) ~ Album.Release.Date,
      # For year-month format (YYYY-MM)
      grepl("^\\d{4}-\\d{2}$", Album.Release.Date) ~ paste0(Album.Release.Date, "-01"),
      # For year only (YYYY)
      grepl("^\\d{4}$", Album.Release.Date) ~ paste0(Album.Release.Date, "-01-01"),
      # Default to NA for any other format
      TRUE ~ NA_character_
    )
  ) %>%
  # Now convert to date safely
  mutate(
    release_date = as.Date(clean_date)
  ) %>%
  # Remove the temporary column
  select(-clean_date) %>%
  # Continue with filtering
  filter(!is.na(release_date)) %>%
  filter((release_date >= as.Date(pre_recession_2007[1]) & release_date <= as.Date(post_recession_2009[2])) |
           (release_date >= as.Date(current_period[1]) & release_date <= as.Date(current_period[2]))) %>%
  mutate(period = case_when(
    release_date >= as.Date(pre_recession_2007[1]) & release_date <= as.Date(pre_recession_2007[2]) ~ "pre_recession_2007",
    release_date >= as.Date(recession_2008_2009[1]) & release_date <= as.Date(recession_2008_2009[2]) ~ "recession_2008_2009",
    release_date >= as.Date(post_recession_2009[1]) & release_date <= as.Date(post_recession_2009[2]) ~ "post_recession_2009",
    release_date >= as.Date(current_period[1]) & release_date <= as.Date(current_period[2]) ~ "current_period"
  ))

# 4. Prepare monthly aggregates from Billboard data
monthly_music_features_billboard <- top_20_songs %>%
  mutate(
    # Ensure all features are numeric
    danceability = as.numeric(danceability),
    energy = as.numeric(energy),
    valence = as.numeric(valence),
    tempo = as.numeric(tempo),
    mode = as.numeric(mode),
    year_month = floor_date(date, "month")
  ) %>%
  group_by(year_month, period) %>%
  summarize(
    avg_valence_billboard = mean(valence, na.rm = TRUE),
    avg_danceability_billboard = mean(danceability, na.rm = TRUE),
    avg_energy_billboard = mean(energy, na.rm = TRUE),
    avg_tempo_billboard = mean(tempo, na.rm = TRUE),
    major_key_pct_billboard = mean(mode, na.rm = TRUE) * 100,
    n_songs_billboard = n(),
    .groups = "drop"
  )

# 5. Prepare monthly aggregates from top_10000
monthly_music_features_top <- top_songs_filtered %>%
  mutate(
    year_month = floor_date(release_date, "month"),
    # Ensuring all features are numeric
    Danceability = as.numeric(Danceability),
    Energy = as.numeric(Energy),
    Valence = as.numeric(Valence),
    Tempo = as.numeric(Tempo),
    Mode = as.numeric(Mode)
  ) %>%
  group_by(year_month, period) %>%
  summarize(
    avg_valence_top = mean(Valence, na.rm = TRUE),
    avg_danceability_top = mean(Danceability, na.rm = TRUE),
    avg_energy_top = mean(Energy, na.rm = TRUE),
    avg_tempo_top = mean(Tempo, na.rm = TRUE),
    major_key_pct_top = mean(Mode, na.rm = TRUE) * 100,
    n_songs_top = n(),
    .groups = "drop"
  )

# 6. Join the datasets by year_month and period
combined_music_features <- full_join(
  monthly_music_features_billboard,
  monthly_music_features_top,
  by = c("year_month", "period")
)

# 7. Load and prepare economic data
library(quantmod)
install.packages("quantmod")
# Using quantmod to fetch data from FRED (Federal Reserve Economic Data)

# Define the timeframes you're analyzing
dates <- list(
  pre_recession_2007 = c(as.Date("2006-01-01"), as.Date("2007-11-30")),
  recession_2008_2009 = c(as.Date("2007-12-01"), as.Date("2009-06-30")),
  post_recession_2009 = c(as.Date("2009-07-01"), as.Date("2010-12-31")),
  current_period = c(as.Date("2022-01-01"), as.Date("2024-03-16"))
)

# Define economic indicators to fetch from FRED
indicators <- c(
  "UNRATE",        # Unemployment Rate
  "UMCSENT",       # University of Michigan Consumer Sentiment
  "GDPC1",         # Real GDP
  "CPIAUCSL",      # Consumer Price Index
  "FEDFUNDS",      # Federal Funds Rate
  "HOUST",         # Housing Starts
  "RSAFS",         # Retail Sales
  "T10Y2Y",        # 10-Year Treasury Yield minus 2-Year Treasury Yield (yield curve)
  "DCOILWTICO"     # Crude Oil Prices
)

# Set date range for FRED data
start_date <- min(unlist(lapply(dates, function(x) x[1])))
end_date <- max(unlist(lapply(dates, function(x) x[2])))

# Download economic data
getSymbols(indicators, src = "FRED", from = start_date, to = end_date)

# Function to convert xts objects to tibbles
xts_to_tibble <- function(xts_obj, indicator_name) {
  tibble(
    date = index(xts_obj),
    value = as.numeric(coredata(xts_obj)),
    indicator = indicator_name
  )
}
# Define the function to convert xts objects to tibbles
xts_to_tibble <- function(xts_obj, indicator_name) {
  tibble(
    date = index(xts_obj),
    value = as.numeric(coredata(xts_obj)),
    indicator = indicator_name
  )
}

# Create an empty list to store our data
economic_data_list <- list()

# Convert all indicators to tibbles and combine
library(tibble)

economic_data_list <- list()
for (ind in indicators) {
  if(exists(ind)) {
    economic_data_list[[ind]] <- xts_to_tibble(get(ind), ind)
  }
}

# Combine all indicators into one dataset
library(dplyr)
install.packages("dplyr")
economic_data_long <- bind_rows(economic_data_list)

# Using base R reshape instead of pivot_wider
economic_data <- reshape(economic_data_long, 
                         idvar = "date",
                         timevar = "indicator", 
                         direction = "wide")

# Rename columns to remove "value." prefix
names(economic_data) <- gsub("value.", "", names(economic_data))

# Create year_month column for monthly aggregation
economic_data$year_month <- as.Date(paste0(format(economic_data$date, "%Y-%m"), "-01"))

# Group by month and calculate averages
economic_data_monthly <- aggregate(
  economic_data[, !colnames(economic_data) %in% c("date", "year_month")],
  by = list(year_month = economic_data$year_month),
  FUN = mean,
  na.rm = TRUE
)

# Add period classification
economic_data_monthly$period <- NA
economic_data_monthly$period[economic_data_monthly$year_month >= as.Date("2006-01-01") & 
                               economic_data_monthly$year_month <= as.Date("2007-11-30")] <- "pre_recession_2007"
economic_data_monthly$period[economic_data_monthly$year_month >= as.Date("2007-12-01") & 
                               economic_data_monthly$year_month <= as.Date("2009-06-30")] <- "recession_2008_2009"
economic_data_monthly$period[economic_data_monthly$year_month >= as.Date("2009-07-01") & 
                               economic_data_monthly$year_month <= as.Date("2010-12-31")] <- "post_recession_2009"
economic_data_monthly$period[economic_data_monthly$year_month >= as.Date("2022-01-01") & 
                               economic_data_monthly$year_month <= as.Date("2024-03-16")] <- "current_period"

# Filter only for relevant periods
economic_data_monthly <- economic_data_monthly[!is.na(economic_data_monthly$period), ]

# Convert to monthly data for consistency
install.packages("lubridate")
library(lubridate)

economic_data %>%
  mutate(
    year_month = floor_date(date, unit = "month")
  ) %>%
  

 group_by(year_month) %>%
  summarize(
    UNRATE = mean(UNRATE, na.rm = TRUE),
    UMCSENT = mean(UMCSENT, na.rm = TRUE),
    GDPC1 = mean(GDPC1, na.rm = TRUE),
    CPIAUCSL = mean(CPIAUCSL, na.rm = TRUE),
    FEDFUNDS = mean(FEDFUNDS, na.rm = TRUE),
    HOUST = mean(HOUST, na.rm = TRUE),
    RSAFS = mean(RSAFS, na.rm = TRUE),
    T10Y2Y = mean(T10Y2Y, na.rm = TRUE),
    DCOILWTICO = mean(DCOILWTICO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  #Add economic period labels
  mutate(period = case_when(
    year_month >= dates$pre_recession_2007[1] & year_month <= dates$pre_recession_2007[2] ~ "pre_recession_2007",
    year_month >= dates$recession_2008_2009[1] & year_month <= dates$recession_2008_2009[2] ~ "recession_2008_2009",
    year_month >= dates$post_recession_2009[1] & year_month <= dates$post_recession_2009[2] ~ "post_recession_2009",
    year_month >= dates$current_period[1] & year_month <= dates$current_period[2] ~ "current_period",
    TRUE ~ NA_character_
  )) %>%
  # Filter only for relevant periods
  filter(!is.na(period))

Here: 
# Create a simple dataframe with just the columns you need
temp_df <- economic_data %>%
  select(year_month, UNRATE) %>%
  arrange(year_month)

# Check for NA values
sum(is.na(temp_df$UNRATE))

# Try the lag operation on this smaller dataframe
temp_df <- temp_df %>%
  mutate(unemployment_change = UNRATE - lag(UNRATE))

# Check the result
head(temp_df)


# Fill missing values with reasonable approximations
economic_data <- economic_data %>%
  mutate(across(where(is.numeric), ~ na.approx(.x, na.rm = FALSE)))

# Create dummy variables for periods
economic_data <- economic_data %>%
  mutate(
    is_recession = period == "recession_2008_2009" | (period == "current_period" & year_month >= as.Date("2022-07-01")),
    months_from_period_start = (year(year_month) - year(min(year_month[period == period]))) * 12 + 
      (month(year_month) - month(min(year_month[period == period])))
  )

# 8. Join music data with economic indicators
# Assuming you have 'combined_music_features' with year_month field
combined_data <- combined_music_features %>%
  left_join(economic_data, by = c("year_month", "period"))

# Add recession sentiment analysis
combined_data <- combined_data %>%
  mutate(
    # Music valence compared to economic conditions
    valence_vs_economy = avg_valence_billboard - scale(economic_stress),
    
    # Categorize relationship between music and economy
    music_economy_relation = case_when(
      valence_vs_economy > 0.5 ~ "Strong Escapism",
      valence_vs_economy > 0 ~ "Mild Escapism",
      valence_vs_economy > -0.5 ~ "Mild Reflection",
      TRUE ~ "Strong Reflection"
    )
  )

# Create aggregated monthly summaries
monthly_summary <- combined_data %>%
  group_by(year_month, period) %>%
  summarize(
    avg_valence = mean(avg_valence_billboard, na.rm = TRUE),
    avg_danceability = mean(avg_danceability_billboard, na.rm = TRUE),
    avg_energy = mean(avg_energy_billboard, na.rm = TRUE),
    UNRATE = first(UNRATE),
    UMCSENT = first(UMCSENT),
    economic_stress = first(economic_stress),
    songs_count = n(),
    .groups = "drop"
  )

# 9. Save the prepared data for analysis
write_csv(combined_data, "combined_music_economic_data.csv")
write_csv(monthly_summary, "monthly_music_economic_summary.csv")
write_csv(economic_data, "economic_indicators.csv")

# 10. Create visualizations for audio features vs economic indicators

# Plot 1: Valence vs Unemployment Over Time
ggplot(monthly_summary, aes(x = year_month)) +
  geom_line(aes(y = avg_valence, color = "Music Valence")) +
  geom_line(aes(y = UNRATE/10, color = "Unemployment Rate (scaled)")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Music Valence vs. Unemployment Rate",
       x = "Date",
       y = "Value") +
  scale_y_continuous(
    name = "Music Valence",
    sec.axis = sec_axis(~.*10, name = "Unemployment Rate (%)")
  ) +
  scale_color_manual(values = c("Music Valence" = "blue", 
                                "Unemployment Rate (scaled)" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Music Features vs Consumer Sentiment
ggplot(monthly_summary, aes(x = year_month)) +
  geom_line(aes(y = avg_valence, color = "Valence")) +
  geom_line(aes(y = avg_energy, color = "Energy")) +
  geom_line(aes(y = UMCSENT/100, color = "Consumer Sentiment (scaled)")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Music Features vs. Consumer Sentiment",
       x = "Date",
       y = "Value") +
  scale_y_continuous(
    name = "Music Feature Value",
    sec.axis = sec_axis(~.*100, name = "Consumer Sentiment Index")
  ) +
  scale_color_manual(values = c("Valence" = "blue", 
                                "Energy" = "green",
                                "Consumer Sentiment (scaled)" = "orange")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Relationship between Economic Stress and Music Mood
ggplot(combined_data, aes(x = economic_stress, y = avg_valence_billboard)) +
  geom_point(alpha = 0.3, aes(color = period)) +
  geom_smooth(method = "lm", color = "black") +
  geom_smooth(aes(color = period), method = "lm") +
  labs(title = "Economic Stress vs. Music Valence",
       subtitle = "Relationship between economic hardship and music mood",
       x = "Economic Stress Index",
       y = "Music Valence",
       color = "Period") +
  theme_minimal()

# Plot 4: Music Features Distribution by Period
combined_data_long <- combined_data %>%
  select(period, avg_valence_billboard, avg_danceability_billboard, avg_energy_billboard) %>%
  pivot_longer(
    cols = c(avg_valence_billboard, avg_danceability_billboard, avg_energy_billboard),
    names_to = "feature",
    values_to = "value"
  ) %>%
  mutate(feature = case_when(
    feature == "avg_valence_billboard" ~ "Valence",
    feature == "avg_danceability_billboard" ~ "Danceability",
    feature == "avg_energy_billboard" ~ "Energy"
  ))

ggplot(combined_data_long, aes(x = period, y = value, fill = feature)) +
  geom_boxplot() +
  labs(title = "Distribution of Music Features by Economic Period",
       x = "Economic Period",
       y = "Feature Value",
       fill = "Music Feature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 10.1 Compare valence (positivity) over time periods
ggplot(combined_music_features, aes(x = year_month)) +
  geom_line(aes(y = avg_valence_billboard, color = "Billboard Data")) +
  geom_line(aes(y = avg_valence_top, color = "Top 10000 Data")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Music Valence (Positivity) Over Time",
       subtitle = "Comparing Billboard Top 20 and Top 10000 datasets",
       y = "Average Valence", 
       x = "Date",
       color = "Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 10.2 Compare danceability over time periods
ggplot(combined_music_features, aes(x = year_month)) +
  geom_line(aes(y = avg_danceability_billboard, color = "Billboard Data")) +
  geom_line(aes(y = avg_danceability_top, color = "Top 10000 Data")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Music Danceability Over Time",
       subtitle = "Comparing Billboard Top 20 and Top 10000 datasets",
       y = "Average Danceability", 
       x = "Date",
       color = "Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 10.3 Compare energy over time periods
ggplot(combined_music_features, aes(x = year_month)) +
  geom_line(aes(y = avg_energy_billboard, color = "Billboard Data")) +
  geom_line(aes(y = avg_energy_top, color = "Top 10000 Data")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Music Energy Over Time",
       subtitle = "Comparing Billboard Top 20 and Top 10000 datasets",
       y = "Average Energy", 
       x = "Date",
       color = "Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 10.4 Compare major key percentage over time periods
ggplot(combined_music_features, aes(x = year_month)) +
  geom_line(aes(y = major_key_pct_billboard, color = "Billboard Data")) +
  geom_line(aes(y = major_key_pct_top, color = "Top 10000 Data")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Percentage of Songs in Major Key Over Time",
       subtitle = "Comparing Billboard Top 20 and Top 10000 datasets",
       y = "Percentage in Major Key (%)", 
       x = "Date",
       color = "Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 11. LYRICS ANALYSIS
# Ensure lyrics data is properly processed
lyrics_data <- top_20_songs %>%
  filter(!is.na(lyrics)) %>%
  mutate(song_id = row_number())  # Add unique identifier

# 11.1 Tokenize lyrics into words
lyrics_words <- lyrics_data %>%
  unnest_tokens(word, lyrics) %>%
  # Remove stop words (common words like "the", "and", etc.)
  anti_join(stop_words) %>%
  # Remove numbers and special characters
  filter(str_detect(word, "[a-z]")) %>%
  # Remove URLs
  filter(!str_detect(word, "http"))

# 11.2 Count words by period
lyrics_word_counts <- lyrics_words %>%
  count(period, word, sort = TRUE) %>%
  group_by(period) %>%
  # Get top 100 words per period
  slice_max(n, n = 100) %>%
  ungroup()

# 11.3 Calculate TF-IDF to find distinctive words for each period
lyrics_tfidf <- lyrics_words %>%
  count(period, word) %>%
  bind_tf_idf(word, period, n) %>%
  arrange(desc(tf_idf))

# 11.4 Get top distinctive words for each period
top_tfidf_words <- lyrics_tfidf %>%
  group_by(period) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  arrange(period, desc(tf_idf))

# 11.5 Sentiment analysis of lyrics
# Get sentiment lexicons
nrc_lexicon <- get_sentiments("nrc")  # Emotions like joy, anger, fear, etc.
bing_lexicon <- get_sentiments("bing")  # Positive/negative classification

# 11.6 Analyze sentiment by period using NRC lexicon (emotions)
lyrics_emotions <- lyrics_words %>%
  inner_join(nrc_lexicon, by = "word") %>%
  # Count occurrences of each emotion by period
  count(period, sentiment) %>%
  # Calculate proportion of each emotion within each period
  group_by(period) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

# 11.7 Analyze positive/negative sentiment by period using Bing lexicon
lyrics_sentiment <- lyrics_words %>%
  inner_join(bing_lexicon, by = "word") %>%
  # Count positive and negative words by period
  count(period, sentiment) %>%
  # Calculate net sentiment (positive - negative)
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(
    total_words = positive + negative,
    net_sentiment = positive - negative,
    sentiment_ratio = positive / negative,
    sentiment_score = (positive - negative) / (positive + negative)
  )

# 11.8 Analyze sentiment by month to track changes over time
lyrics_sentiment_by_month <- lyrics_words %>%
  # Join with song data to get the date
  left_join(lyrics_data %>% select(song_id, date, period), by = "song_id") %>%
  # Create month column
  mutate(year_month = floor_date(date, "month")) %>%
  # Join with sentiment lexicon
  inner_join(bing_lexicon, by = "word") %>%
  # Count by month and sentiment
  count(year_month, period, sentiment) %>%
  # Convert to wide format
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  # Calculate sentiment score
  mutate(
    total_words = positive + negative,
    sentiment_score = (positive - negative) / (positive + negative)
  )

# 11.9 Create visualization for word frequencies by period
ggplot(lyrics_word_counts %>% 
         group_by(period) %>% 
         slice_max(n, n = 15) %>%
         ungroup() %>%
         mutate(word = reorder_within(word, n, period))) +
  geom_col(aes(n, word, fill = period), show.legend = FALSE) +
  facet_wrap(~period, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Most Common Words in Lyrics by Period",
       subtitle = "Comparing different economic periods",
       x = "Word Count",
       y = NULL) +
  theme_minimal()

# 11.10 Create visualization for distinctive words by period (TF-IDF)
ggplot(top_tfidf_words %>%
         mutate(word = reorder_within(word, tf_idf, period))) +
  geom_col(aes(tf_idf, word, fill = period), show.legend = FALSE) +
  facet_wrap(~period, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Most Distinctive Words in Lyrics by Period",
       subtitle = "Based on TF-IDF scores",
       x = "TF-IDF Score",
       y = NULL) +
  theme_minimal()
# 11.11 Create visualization for emotions across periods
ggplot(lyrics_emotions, aes(x = reorder(sentiment, -proportion), y = proportion, fill = period)) +
  geom_col(position = "dodge") +
  labs(title = "Emotional Content in Lyrics Across Economic Periods",
       subtitle = "Based on NRC sentiment lexicon",
       x = NULL,
       y = "Proportion",
       fill = "Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 11.12 Create visualization for sentiment score over time
ggplot(lyrics_sentiment_by_month, aes(x = year_month, y = sentiment_score, color = period)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Lyrical Sentiment Score Over Time",
       subtitle = "Positive vs. Negative Language (normalized to -1 to 1 scale)",
       x = "Date",
       y = "Sentiment Score",
       color = "Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 11.13 Analyze bi-grams (word pairs) to understand context better
lyrics_bigrams <- lyrics_data %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2) %>%
  # Remove bigrams with stop words
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  # Remove non-alphabetic words and URLs
  filter(str_detect(word1, "[a-z]") & str_detect(word2, "[a-z]")) %>%
  filter(!str_detect(word1, "http") & !str_detect(word2, "http")) %>%
  # Recombine
  unite(bigram, word1, word2, sep = " ")

# Count bigrams by period
bigram_counts <- lyrics_bigrams %>%
  count(period, bigram, sort = TRUE) %>%
  group_by(period) %>%
  # Get top bigrams per period
  slice_max(n, n = 30) %>%
  ungroup()

# 11.14 Visualize top bigrams by period
ggplot(bigram_counts %>%
         group_by(period) %>%
         slice_max(n, n = 10) %>%
         ungroup() %>%
         mutate(bigram = reorder_within(bigram, n, period))) +
  geom_col(aes(n, bigram, fill = period), show.legend = FALSE) +
  facet_wrap(~period, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Most Common Word Pairs in Lyrics by Period",
       x = "Count",
       y = NULL) +
  theme_minimal()

# 11.15 Create network graph of word co-occurrences for each period
# This helps visualize thematic clusters in the lyrics

# Function to create a word network for a specific period
create_word_network <- function(period_name) {
  # Filter data for the specific period
  period_words <- lyrics_words %>%
    left_join(lyrics_data %>% select(song_id, period), by = "song_id") %>%
    filter(period == period_name)
  
  # Get top words for this period
  top_words <- period_words %>%
    count(word, sort = TRUE) %>%
    slice_max(n, n = 100) %>%
    pull(word)
  
  # Filter to only include those top words
  filtered_words <- period_words %>%
    filter(word %in% top_words)
  
  # Create word pairs within songs
  word_pairs <- filtered_words %>%
    pairwise_count(word, song_id, sort = TRUE, upper = FALSE) %>%
    filter(n > 2)  # Only keep pairs that occur together more than twice
  
  # Create graph
  set.seed(1234)  # For reproducibility
  word_graph <- word_pairs %>%
    filter(n > 3) %>%
    graph_from_data_frame(directed = FALSE)
  
  # Plot the graph
  ggraph(word_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "lightgray") +
    geom_node_point(color = "darkblue", size = 3) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    labs(title = paste("Word Co-occurrence Network for", period_name),
         subtitle = "Words that frequently appear together in the same songs") +
    theme_void()
}

# Create networks for each period
pre_recession_network <- create_word_network("pre_recession_2007")
recession_network <- create_word_network("recession_2008_2009")
post_recession_network <- create_word_network("post_recession_2009")
current_network <- create_word_network("current_period")

# 11.16 Topic modeling to discover latent themes
# Install if needed: install.packages("topicmodels")
library(topicmodels)

# Create document-term matrix
lyrics_dtm <- lyrics_words %>%
  count(song_id, word) %>%
  cast_dtm(song_id, word, n)

# Set number of topics
k <- 10

# Run LDA topic model
lyrics_lda <- LDA(lyrics_dtm, k = k, control = list(seed = 1234))

# Extract topics
lyrics_topics <- tidy(lyrics_lda, matrix = "beta")

# Get top terms per topic
top_terms <- lyrics_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Create visualization of topics
ggplot(top_terms, aes(beta, reorder_within(term, beta, topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top Words in Each Topic",
       x = expression(beta),
       y = NULL) +
  theme_minimal()

# Connect topics to time periods
# Get topic distributions for each song
song_topics <- tidy(lyrics_lda, matrix = "gamma")

# Rename columns
song_topics <- song_topics %>%
  rename(song_id = document) %>%
  mutate(song_id = as.numeric(song_id))

# Join with song data to get period information
topic_by_period <- song_topics %>%
  left_join(lyrics_data %>% select(song_id, period), by = "song_id") %>%
  group_by(period, topic) %>%
  summarize(avg_gamma = mean(gamma), .groups = "drop") %>%
  # Convert to relative importance within each period
  group_by(period) %>%
  mutate(relative_importance = avg_gamma / sum(avg_gamma)) %>%
  ungroup()

# Visualize topic distribution by period
ggplot(topic_by_period, aes(x = factor(topic), y = relative_importance, fill = period)) +
  geom_col(position = "dodge") +
  labs(title = "Topic Distribution by Economic Period",
       x = "Topic",
       y = "Relative Importance",
       fill = "Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0))

# 11.17 Compare lyrical features with economic indicators
# Once you have economic data, you can add this analysis
# Placeholder example:

# Create a function to generate economic comparison charts
generate_economic_comparison <- function(economic_data) {
  # Join sentiment data with economic data
  sentiment_econ <- lyrics_sentiment_by_month %>%
    left_join(economic_data, by = "year_month") %>%
    filter(!is.na(unemployment))  # Only keep months with economic data
  
  # Plot sentiment vs unemployment
  ggplot(sentiment_econ, aes(x = year_month)) +
    geom_line(aes(y = sentiment_score, color = "Lyrical Positivity")) +
    geom_line(aes(y = unemployment/20, color = "Unemployment Rate (scaled)")) +
    facet_wrap(~period, scales = "free_x") +
    labs(title = "Lyrical Sentiment vs. Unemployment Rate",
         y = "Value", 
         x = "Date",
         color = "Measure") +
    scale_y_continuous(
      name = "Sentiment Score",
      sec.axis = sec_axis(~.*20, name = "Unemployment Rate (%)")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# 11.18 Combine lyrical and audio features for comprehensive analysis
combined_features_by_month <- lyrics_sentiment_by_month %>%
  left_join(monthly_music_features_billboard, by = c("year_month", "period"))

# Create correlation heatmap between lyrical sentiment and audio features
# Install if needed: install.packages("corrplot")
library(corrplot)

# Calculate correlations
cor_data <- combined_features_by_month %>%
  select(sentiment_score, avg_valence_billboard, avg_energy_billboard, 
         avg_danceability_billboard, major_key_pct_billboard) %>%
  cor(use = "complete.obs")

# Visualize correlations
corrplot(cor_data, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Correlation between Lyrical and Audio Features")

# 11.19 Save the processed lyrics data for further analysis
write_csv(lyrics_words, "processed_lyrics_words.csv")
write_csv(lyrics_sentiment_by_month, "lyrics_sentiment_by_month.csv")
write_csv(lyrics_emotions, "lyrics_emotions_by_period.csv")

# 11.20 Create summary dataframe of all features by period
period_summary <- lyrics_sentiment %>%
  select(period, sentiment_score) %>%
  left_join(
    lyrics_emotions %>%
      pivot_wider(names_from = sentiment, values_from = proportion),
    by = "period"
  ) %>%
  left_join(
    combined_music_features %>%
      group_by(period) %>%
      summarize(
        avg_valence = mean(avg_valence_billboard, na.rm = TRUE),
        avg_danceability = mean(avg_danceability_billboard, na.rm = TRUE),
        avg_energy = mean(avg_energy_billboard, na.rm = TRUE),
        avg_major_key_pct = mean(major_key_pct_billboard, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "period"
  )

# 11.21 Create final summary visualization
# Convert data to long format for easier plotting
period_summary_long <- period_summary %>%
  pivot_longer(cols = -period, 
               names_to = "feature", 
               values_to = "value")

# Plot key features across periods
ggplot(period_summary_long %>% 
         filter(feature %in% c("avg_valence", "avg_energy", "avg_danceability", 
                               "joy", "sadness", "fear", "anger", "sentiment_score")), 
       aes(x = period, y = value, fill = period)) +
  geom_col() +
  facet_wrap(~feature, scales = "free_y") +
  labs(title = "Key Music and Lyrical Features Across Economic Periods",
       x = NULL,
       y = "Value",
       fill = "Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# 12. When you have economic data, add these visualizations:
# Example structure:
# music_econ_data_combined <- combined_music_features %>%
#   left_join(
#     unemployment %>% 
#       mutate(year_month = floor_date(date, "month")) %>%
#       select(year_month, unemployment = value),
#     by = "year_month"
#   ) %>%
#   left_join(
#     sentiment %>% 
#       mutate(year_month = floor_date(date, "month")) %>%
#       select(year_month, consumer_sentiment = value),
#     by = "year_month"
#   )

# 12.1 Create a summary of research findings
# This part would typically be written after data analysis is complete

# You can use the following template in comments as a guide
# Findings summary:
# 1. Did music valence (positivity) increase during the recession period?
# 2. How did lyrical sentiment compare to audio features like valence?
# 3. Were there specific topics or themes that became more prevalent during economic downturns?
# 4. Is the current period (2023-2024) showing similar patterns to the pre-recession period of 2007?
# 5. What emotion categories showed the biggest changes during economic transitions?

# NEXT STEPS FOR MUSIC AND ECONOMIC RECESSION ANALYSIS
# This script continues from the previous analysis by adding:
# 1. Economic data integration
# 2. Advanced temporal analysis
# 3. Hypothesis testing
# 4. Visualization of key findings

# Load additional packages for advanced analysis
library(forecast)  # For time series analysis
library(tseries)   # For statistical tests
library(ccf)       # For cross-correlation analysis
library(broom)     # For tidy model outputs
library(knitr)     # For table creation
library(gridExtra) # For arranging multiple plots

# -------------------------------------------------------------------------
# 1. ECONOMIC DATA INTEGRATION
# -------------------------------------------------------------------------

# 1.1 Download Economic Data from FRED
# If you don't have the data downloaded yet, use the quantmod package
# install.packages("quantmod")
library(quantmod)

# Define the economic indicators we want to analyze
fred_codes <- c(
  "UNRATE",        # Unemployment Rate
  "UMCSENT",       # University of Michigan Consumer Sentiment
  "GDPC1",         # Real GDP
  "CSCICP03USM665S", # Consumer Confidence Index
  "HOUST",         # Housing Starts
  "MORTGAGE30US"   # 30-Year Fixed Rate Mortgage Average
)

# Set date range
date_range <- paste(
  format(as.Date(pre_recession_2007[1]), "%Y-%m-%d"), 
  format(Sys.Date(), "%Y-%m-%d"), 
  sep = "/"
)

# Download data from FRED
getSymbols(fred_codes, src = "FRED", from = pre_recession_2007[1], to = Sys.Date())

# Convert FRED data to data frames and combine
economic_data <- data.frame(
  date = index(UNRATE),
  unemployment = as.numeric(UNRATE),
  consumer_sentiment = as.numeric(UMCSENT),
  gdp = as.numeric(GDPC1),
  consumer_confidence = as.numeric(CSCICP03USM665S),
  housing_starts = as.numeric(HOUST),
  mortgage_rate = as.numeric(MORTGAGE30US)
) %>%
  # Convert to date format and create year-month
  mutate(
    date = as.Date(date),
    year_month = floor_date(date, "month")
  ) %>%
  # Handle potential NA values with forward-fill interpolation
  fill(unemployment, consumer_sentiment, gdp, consumer_confidence, 
       housing_starts, mortgage_rate, .direction = "down")

# Or alternatively, load from CSV files if you already have them
# economic_data <- read_csv("economic_indicators.csv") %>%
#   mutate(date = as.Date(date),
#          year_month = floor_date(date, "month"))

# 1.2 Add period classification to economic data
economic_data <- economic_data %>%
  mutate(period = case_when(
    date >= as.Date(pre_recession_2007[1]) & date <= as.Date(pre_recession_2007[2]) ~ "pre_recession_2007",
    date >= as.Date(recession_2008_2009[1]) & date <= as.Date(recession_2008_2009[2]) ~ "recession_2008_2009",
    date >= as.Date(post_recession_2009[1]) & date <= as.Date(post_recession_2009[2]) ~ "post_recession_2009",
    date >= as.Date(current_period[1]) & date <= as.Date(current_period[2]) ~ "current_period",
    TRUE ~ NA_character_
  ))

# 1.3 Calculate month-over-month changes for economic indicators
economic_data <- economic_data %>%
  arrange(date) %>%
  group_by(1) %>%  # Group by a constant to create one group
  mutate(
    unemployment_change = unemployment - lag(unemployment),
    sentiment_change = consumer_sentiment - lag(consumer_sentiment),
    gdp_growth = (gdp - lag(gdp)) / lag(gdp) * 100,
    mortgage_change = mortgage_rate - lag(mortgage_rate)
  ) %>%
  ungroup()

# 1.4 Combine economic and music data
# Merge with monthly music features
full_analysis_data <- combined_music_features %>%
  left_join(economic_data, by = c("year_month", "period"))

# Merge with lyrics sentiment data
lyrics_econ_data <- lyrics_sentiment_by_month %>%
  left_join(economic_data, by = c("year_month", "period"))

# -------------------------------------------------------------------------
# 2. TEMPORAL ANALYSIS
# -------------------------------------------------------------------------

# 2.1 Create time series objects for key variables
# For music features
valence_ts <- ts(full_analysis_data$avg_valence_billboard, 
                 frequency = 12, 
                 start = c(year(min(full_analysis_data$year_month, na.rm = TRUE)), 
                           month(min(full_analysis_data$year_month, na.rm = TRUE))))

# For economic indicators
unemployment_ts <- ts(economic_data$unemployment, 
                      frequency = 12, 
                      start = c(year(min(economic_data$date)), 
                                month(min(economic_data$date))))

# 2.2 Cross-correlation analysis to detect lead/lag relationships
# This helps determine if music features change before or after economic indicators

# Function to perform and plot cross-correlation
analyze_cross_correlation <- function(series1, series2, name1, name2, max_lag = 12) {
  # Calculate cross-correlation
  ccf_result <- ccf(series1, series2, lag.max = max_lag, plot = FALSE)
  
  # Convert to data frame for ggplot
  ccf_df <- data.frame(
    lag = seq(-max_lag, max_lag),
    correlation = ccf_result$acf
  )
  
  # Find maximum correlation and its lag
  max_cor <- ccf_df[which.max(abs(ccf_df$correlation)),]
  
  # Create plot
  p <- ggplot(ccf_df, aes(x = lag, y = correlation)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 1.96/sqrt(length(series1)), linetype = "dotted", color = "red") +
    geom_hline(yintercept = -1.96/sqrt(length(series1)), linetype = "dotted", color = "red") +
    labs(title = paste("Cross-correlation between", name1, "and", name2),
         subtitle = paste("Maximum correlation:", round(max_cor$correlation, 3), "at lag", max_cor$lag),
         x = paste("Lag (Negative = ", name1, "leads", name2, ")"),
         y = "Correlation") +
    theme_minimal()
  
  # Return both the plot and the data
  return(list(plot = p, data = ccf_df, max = max_cor))
}

# Create cross-correlations for key relationships
valence_unemployment_ccf <- analyze_cross_correlation(
  na.omit(full_analysis_data$avg_valence_billboard),
  na.omit(full_analysis_data$unemployment),
  "Music Valence",
  "Unemployment Rate"
)

danceability_sentiment_ccf <- analyze_cross_correlation(
  na.omit(full_analysis_data$avg_danceability_billboard),
  na.omit(full_analysis_data$consumer_sentiment),
  "Music Danceability",
  "Consumer Sentiment"
)

lyrics_unemployment_ccf <- analyze_cross_correlation(
  na.omit(lyrics_econ_data$sentiment_score),
  na.omit(lyrics_econ_data$unemployment),
  "Lyrical Positivity",
  "Unemployment Rate"
)

# Display the cross-correlation plots
valence_unemployment_ccf$plot
danceability_sentiment_ccf$plot
lyrics_unemployment_ccf$plot

# 2.3 Create lagged variables to capture temporal relationships
full_analysis_data <- full_analysis_data %>%
  mutate(
    # Music features lagged by 3 months
    valence_lag3 = lag(avg_valence_billboard, 3),
    danceability_lag3 = lag(avg_danceability_billboard, 3),
    energy_lag3 = lag(avg_energy_billboard, 3),
    
    # Economic indicators lagged by 3 months
    unemployment_lag3 = lag(unemployment, 3),
    sentiment_lag3 = lag(consumer_sentiment, 3)
  )

# -------------------------------------------------------------------------
# 3. HYPOTHESIS TESTING
# -------------------------------------------------------------------------

# 3.1 Test the main hypothesis: Music becomes more positive during economic hardship

# Compare mean valence across periods
valence_anova <- aov(avg_valence_billboard ~ period, data = full_analysis_data)
valence_anova_summary <- summary(valence_anova)

# Run post-hoc tests
valence_tukey <- TukeyHSD(valence_anova)

# Create tidy output
valence_anova_tidy <- tidy(valence_anova)
valence_tukey_tidy <- tidy(valence_tukey)

# 3.2 Test correlation between economic indicators and music features
# Run multiple regression models

# Model 1: Does unemployment predict music valence?
model_valence <- lm(avg_valence_billboard ~ unemployment + period, data = full_analysis_data)
model_valence_summary <- summary(model_valence)

# Model 2: Does consumer sentiment predict music danceability?
model_dance <- lm(avg_danceability_billboard ~ consumer_sentiment + period, data = full_analysis_data)
model_dance_summary <- summary(model_dance)

# Model 3: Are lagged economic variables better predictors?
model_lagged <- lm(avg_valence_billboard ~ unemployment_lag3 + period, data = full_analysis_data)
model_lagged_summary <- summary(model_lagged)

# Create tidy model outputs
model_valence_tidy <- tidy(model_valence)
model_dance_tidy <- tidy(model_dance)
model_lagged_tidy <- tidy(model_lagged)

# 3.3 Compare recession patterns: 2008 vs. 2023-2024
# Filter data for specific periods
recession_2008 <- full_analysis_data %>%
  filter(period == "recession_2008_2009") %>%
  arrange(year_month)

current_period_data <- full_analysis_data %>%
  filter(period == "current_period") %>%
  arrange(year_month)

# Create standardized variables for comparison
recession_comparison <- bind_rows(
  recession_2008 %>% 
    mutate(months_from_start = row_number()) %>%
    select(months_from_start, period, avg_valence_billboard, avg_danceability_billboard, unemployment),
  current_period_data %>% 
    mutate(months_from_start = row_number()) %>%
    select(months_from_start, period, avg_valence_billboard, avg_danceability_billboard, unemployment)
)

# Create comparison visualization
ggplot(recession_comparison, aes(x = months_from_start, color = period)) +
  geom_line(aes(y = avg_valence_billboard, linetype = "Music Valence")) +
  geom_line(aes(y = unemployment/10, linetype = "Unemployment Rate (scaled)")) +
  labs(title = "Comparing 2008 Recession to Current Period Patterns",
       x = "Months from Period Start",
       y = "Value",
       color = "Period",
       linetype = "Measure") +
  scale_y_continuous(
    name = "Music Valence",
    sec.axis = sec_axis(~.*10, name = "Unemployment Rate (%)")
  ) +
  theme_minimal()

# -------------------------------------------------------------------------
# 4. RESULTS VISUALIZATION
# -------------------------------------------------------------------------

# 4.1 Create comprehensive dashboard plots

# Plot 1: Music features vs. Unemployment over time
p1 <- ggplot(full_analysis_data, aes(x = year_month)) +
  geom_line(aes(y = avg_valence_billboard, color = "Valence")) +
  geom_line(aes(y = avg_danceability_billboard, color = "Danceability")) +
  geom_line(aes(y = unemployment/10, color = "Unemployment Rate (scaled)")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Music Features vs. Unemployment Rate",
       x = "Date",
       y = "Value",
       color = "Measure") +
  scale_y_continuous(
    name = "Music Feature Value",
    sec.axis = sec_axis(~.*10, name = "Unemployment Rate (%)")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Music features vs. Consumer Sentiment over time
p2 <- ggplot(full_analysis_data, aes(x = year_month)) +
  geom_line(aes(y = avg_valence_billboard, color = "Valence")) +
  geom_line(aes(y = avg_energy_billboard, color = "Energy")) +
  geom_line(aes(y = consumer_sentiment/100, color = "Consumer Sentiment (scaled)")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Music Features vs. Consumer Sentiment",
       x = "Date",
       y = "Value",
       color = "Measure") +
  scale_y_continuous(
    name = "Music Feature Value",
    sec.axis = sec_axis(~.*100, name = "Consumer Sentiment")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Lyrical sentiment vs. Economic indicators
p3 <- ggplot(lyrics_econ_data, aes(x = year_month)) +
  geom_line(aes(y = sentiment_score, color = "Lyrical Sentiment")) +
  geom_line(aes(y = unemployment/20, color = "Unemployment Rate (scaled)")) +
  geom_line(aes(y = consumer_sentiment/200, color = "Consumer Sentiment (scaled)")) +
  facet_wrap(~period, scales = "free_x") +
  labs(title = "Lyrical Sentiment vs. Economic Indicators",
       x = "Date",
       y = "Value",
       color = "Measure") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4.2 Create summary tables

# Table 1: Summary statistics by period
period_summary_table <- full_analysis_data %>%
  group_by(period) %>%
  summarize(
    avg_valence = mean(avg_valence_billboard, na.rm = TRUE),
    avg_danceability = mean(avg_danceability_billboard, na.rm = TRUE),
    avg_energy = mean(avg_energy_billboard, na.rm = TRUE),
    avg_unemployment = mean(unemployment, na.rm = TRUE),
    avg_sentiment = mean(consumer_sentiment, na.rm = TRUE),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  kable(digits = 3, caption = "Summary Statistics by Economic Period")

# Table 2: Correlation matrix
correlation_table <- full_analysis_data %>%
  select(avg_valence_billboard, avg_danceability_billboard, 
         avg_energy_billboard, unemployment, consumer_sentiment) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  kable(digits = 3, caption = "Correlation Between Music Features and Economic Indicators")

# Visualization of correlation matrix
correlation_plot <- ggcorrplot(
  correlation_table[-1], 
  hc.order = TRUE, 
  type = "lower", 
  lab = TRUE, 
  lab_size = 3,
  title = "Correlation Matrix: Music Features vs. Economic Indicators"
)
# -------------------------------------------------------------------------
# 5. ADVANCED ANALYSIS & MODELS
# -------------------------------------------------------------------------

# 5.1 Time Series Decomposition
# Decompose valence to understand seasonal patterns
valence_decomp <- stl(valence_ts, s.window = "periodic")
plot(valence_decomp)

# Decompose unemployment for comparison
unemployment_decomp <- stl(unemployment_ts, s.window = "periodic")
plot(unemployment_decomp)

# 5.2 Granger Causality Tests
# Test if economic indicators "cause" music features
unemployment_to_valence <- grangertest(avg_valence_billboard ~ unemployment, 
                                       order = 3, 
                                       data = full_analysis_data)

sentiment_to_dance <- grangertest(avg_danceability_billboard ~ consumer_sentiment, 
                                  order = 3, 
                                  data = full_analysis_data)

# 5.3 Create a combined recession index
full_analysis_data <- full_analysis_data %>%
  mutate(
    # Simple economic hardship index (higher = worse conditions)
    econ_hardship_index = scale(unemployment) - scale(consumer_sentiment) - scale(gdp_growth),
    
    # Categorize periods by economic hardship
    hardship_category = case_when(
      econ_hardship_index <= quantile(econ_hardship_index, 0.25, na.rm = TRUE) ~ "Low Hardship",
      econ_hardship_index <= quantile(econ_hardship_index, 0.75, na.rm = TRUE) ~ "Moderate Hardship",
      TRUE ~ "High Hardship"
    )
  )

# Plot music features by hardship level
hardship_plot <- ggplot(full_analysis_data, aes(x = hardship_category)) +
  geom_boxplot(aes(y = avg_valence_billboard, fill = "Valence")) +
  geom_boxplot(aes(y = avg_danceability_billboard, fill = "Danceability")) +
  geom_boxplot(aes(y = avg_energy_billboard, fill = "Energy")) +
  labs(title = "Music Features by Economic Hardship Level",
       x = "Economic Hardship Level",
       y = "Music Feature Value",
       fill = "Feature") +
  theme_minimal() +
  theme(legend.position = "bottom")

# -------------------------------------------------------------------------
# 6. PREDICTIVE MODELING
# -------------------------------------------------------------------------

# 6.1 Can we predict music mood from economic indicators?
predict_model <- lm(avg_valence_billboard ~ unemployment + consumer_sentiment + gdp_growth + 
                      mortgage_rate + period, data = full_analysis_data)
predict_summary <- summary(predict_model)

# Calculate VIF to check for multicollinearity
model_vif <- vif(predict_model)

# 6.2 Create a predictive model for current music trends
# Train on pre-2020 data
training_data <- full_analysis_data %>%
  filter(year_month < as.Date("2020-01-01"))

test_data <- full_analysis_data %>%
  filter(year_month >= as.Date("2020-01-01"))

# Train model
predict_valence_model <- lm(avg_valence_billboard ~ unemployment + consumer_sentiment + 
                              gdp_growth + mortgage_rate, data = training_data)

# Make predictions
test_data$predicted_valence <- predict(predict_valence_model, newdata = test_data)

# Plot actual vs predicted
prediction_plot <- ggplot(test_data, aes(x = year_month)) +
  geom_line(aes(y = avg_valence_billboard, color = "Actual Valence")) +
  geom_line(aes(y = predicted_valence, color = "Predicted Valence")) +
  labs(title = "Predicted vs Actual Music Valence Based on Economic Indicators",
       x = "Date",
       y = "Valence",
       color = "Type") +
  theme_minimal()

# -------------------------------------------------------------------------
# 7. FINAL RESULTS & REPORT
# -------------------------------------------------------------------------

# 7.1 Create a grid of key visualizations for the final report
final_plot_grid <- grid.arrange(
  p1, p2, valence_unemployment_ccf$plot, hardship_plot,
  ncol = 2
)

# 7.2 Prepare summary tables for final report
# Key findings table
key_findings <- data.frame(
  finding = c(
    "Correlation between valence and unemployment",
    "Lag relationship (months)",
    "Recession vs. post-recession valence difference",
    "Predictive model R-squared",
    "Most significant economic predictor"
  ),
  value = c(
    round(cor(full_analysis_data$avg_valence_billboard, full_analysis_data$unemployment, 
              use = "pairwise.complete.obs"), 3),
    valence_unemployment_ccf$max$lag,
    round(diff(tapply(full_analysis_data$avg_valence_billboard, full_analysis_data$period, 
                      mean, na.rm = TRUE)[c("recession_2008_2009", "post_recession_2009")]), 3),
    round(summary(predict_model)$r.squared, 3),
    rownames(coef(summary(predict_model))[-1,])[which.min(coef(summary(predict_model))[-1,"Pr(>|t|)"])]
  )
) %>%
  kable(caption = "Key Findings Summary")

# 7.3 Export key results
# Save plots
ggsave("music_econ_dashboard.png", final_plot_grid, width = 12, height = 10)

# Write key tables to CSV
write.csv(period_summary_table, "period_summary_stats.csv", row.names = FALSE)
write.csv(as.data.frame(correlation_table), "feature_correlations.csv", row.names = FALSE)
write.csv(as.data.frame(key_findings), "key_findings.csv", row.names = FALSE)

# Export model results
export_model <- function(model, filename) {
  sink(filename)
  print(summary(model))
  sink()
}
export_model(predict_model, "predictive_model_results.txt")
export_model(model_valence, "valence_model_results.txt")

