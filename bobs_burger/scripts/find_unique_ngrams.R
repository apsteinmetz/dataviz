# scripts/find_unique_ngrams.R
# Find most unique n-gram per episode using tf-idf

library(tidyverse)
library(tidytext)

#' Find the most unique n-gram for each episode using tf-idf
#'
#' @param n Integer specifying the n-gram size (1 = unigram, 2 = bigram, etc.)
#' @param rdata_path Path to the RData file containing transcript data
#' @param min_occurrences Minimum times an n-gram must appear in an episode
#' @return A tibble with columns: season, episode, title, and most_unique_n_gram
find_unique_ngrams <- function(
  ng = 1,
  rdata_path = "www/transcript_data.RData",
  min_occurrences = 2
) {
  # Load the transcript data
  if (!file.exists(rdata_path)) {
    stop("Data file not found: ", rdata_path)
  }

  load(rdata_path)

  # Define stopwords to exclude (articles and common words)
  stopwords <- c(
    "a",
    "an",
    "the",
    "any",
    "some",
    "to",
    "and",
    "or",
    "of",
    "but",
    "if",
    "is",
    "are",
    "was",
    "were",
    "in",
    "on",
    "at",
    "by",
    "for",
    "with"
  )

  # Create episode identifier
  episode_text <- transcript_data |>
    filter(!is.na(dialogue)) |>
    mutate(episode_id = paste(season, episode, sep = "_")) |>
    group_by(season, episode, title, episode_id) |>
    summarize(text = paste(dialogue, collapse = " "), .groups = "drop")

  # Tokenize into n-grams
  ngrams <- episode_text |>
    unnest_tokens(ngram, text, token = "ngrams", n = ng) |>
    filter(!is.na(ngram))

  # Remove n-grams that contain stopwords

  if (ng == 1) {
    # For unigrams, just filter out the stopwords directly
    ngrams <- ngrams |>
      filter(!ngram %in% stopwords)
  } else {
    # For n-grams, remove any that start or end with stopwords
    stopwords_pattern <- paste0(
      "^(",
      paste(stopwords, collapse = "|"),
      ")\\b|\\b(",
      paste(stopwords, collapse = "|"),
      ")$"
    )
    ngrams <- ngrams |>
      filter(!str_detect(ngram, stopwords_pattern))
  }

  # Count n-grams per episode
  ngram_counts <- ngrams |>
    count(episode_id, season, episode, title, ngram, name = "n") |>
    filter(n >= min_occurrences)

  # Calculate tf-idf
  # tf = term frequency within episode

  # idf = inverse document frequency across all episodes
  ngram_tfidf <- ngram_counts |>
    bind_tf_idf(ngram, episode_id, n)

  # Find the most unique n-gram per episode (highest tf-idf)
  result <- ngram_tfidf |>
    group_by(season, episode, title) |>
    slice_max(tf_idf, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(season, episode, title, ngram, n, tf_idf) |>
    arrange(season, episode)

  # Rename columns to reflect n
  result <- result |> rename(occurrences = n) |> mutate(n_gram = ng)

  result
}

# Main execution
# Get unigrams
result_1 <- find_unique_ngrams(ng = 1)

# Get bigrams and trigrams
result_2 <- find_unique_ngrams(ng = 2)
result_3 <- find_unique_ngrams(ng = 3)

# Define stopwords for checking bigram endings
stopwords <- c(
  "a",
  "an",
  "the",
  "any",
  "some",
  "to",
  "and",
  "or",
  "but",
  "if",
  "is",
  "are",
  "was",
  "were",
  "in",
  "on",
  "at",
  "by",
  "for",
  "with"
)

# Check which bigrams end with a stopword
result_2 <- result_2 |>
  mutate(
    last_word = str_extract(ngram, "\\w+$"),
    ends_with_stopword = last_word %in% stopwords
  )

# For episodes where bigram ends with stopword, use trigram instead
episodes_needing_trigram <- result_2 |>
  filter(ends_with_stopword) |>
  select(season, episode)

# Replace those episodes with trigrams
result_2_final <- result_2 |>
  filter(!ends_with_stopword) |>
  select(-last_word, -ends_with_stopword)

result_3_replacements <- result_3 |>
  semi_join(episodes_needing_trigram, by = c("season", "episode"))

# Combine: bigrams (where valid) + trigrams (where bigram ended with stopword)
result_phrases <- bind_rows(result_2_final, result_3_replacements) |>
  arrange(season, episode)

# Combine unigrams and phrases
result <- bind_rows(result_1, result_phrases)

# Save to CSV
output_file <- paste0("www/most_unique_words.csv")
write_csv(result, output_file)

message("Saved results to: ", output_file)
