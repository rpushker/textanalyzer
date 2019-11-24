#' analyzeNGrams
#' @title Analyze NGrams
#' @description Analyze text with ngram among 1, 2 or 3.
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param n a numeric_vector of length 1. Ngram = 1, 2 or 3.
#' @param top_rows a numeric vector of length 1. Number of top rows to be returned.
#' @examples
#' \dontrun{
#' analyzeTokens(text=c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))
#' }
#' @export
analyzeNGrams <- function(in_text, n=1, top_rows=25) {
  if (!(is.character(in_text) || is.factor(in_text))) {
    stop("The input text should be either character of factor.")
  }

  in_text <- data.frame(text=in_text, stringsAsFactors = FALSE)
  if (n ==1) {
    analyzeTokens(in_text, top_rows)
  } else if (n == 2) {
    analyzeBigrams(in_text, top_rows)
  } else if (n == 3) {
    analyzeTrigrams(in_text, top_rows)
  } else {
    stop("Functionality for ngram=", ngram, " is still not available.")
  }
}

#' analyzeTokens
#' @title Analyze Tokens
#' @description Analyze text with ngram=1
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param top_rows a numeric vector of length 1. Number of top rows to be returned.
#' @examples
#' \dontrun{
#' analyzeTokens(data.frame(text=c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), stringAsFactors=FALSE))
#' }
#' @import tidytext
#' @import dplyr
#' @export
analyzeTokens <- function(in_text, top_rows=25) {
  in_text$text <- as.character(in_text$text)

  token_df <- in_text %>%
    unnest_tokens(word, text)

  # Remove stop words like the, in, a, of etc.
  data(stop_words)

  token_df <- token_df %>% anti_join(stop_words, by = "word")

  token_count <- token_df %>% count(word, sort = TRUE)

  token_count <- token_count %>%
    top_n(n=top_rows, wt=n) %>%
    mutate(word = reorder(word, n))

  return(token_count)
}

#' analyzeBigrams
#' @title Analyze Bigrams
#' @description Analyze text with ngram=2 (bigrams).
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param top_rows a numeric vector of length 1. Number of top rows to be returned.
#' @examples
#' \dontrun{
#' analyzeTokens(data.frame(text=c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), stringAsFactors=FALSE))
#' }
#' @import tidytext
#' @import tidyr
#' @import dplyr
#' @export
analyzeBigrams <- function(in_text, top_rows=25) {
  in_text$text <- as.character(in_text$text)

  bigrams <- in_text %>%
    unnest_tokens(bigram, text, token="ngrams", n=2)

  bigram_counts <- bigrams %>% count(bigram, sort = T)

  bigrams_separated <- bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

  data(stop_words)

  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

  # new bigram counts:
  bigram_counts <- bigrams_filtered %>%
    count(word1, word2, sort = TRUE)

  bigrams_united <- bigram_counts %>%
    unite(bigram, word1, word2, sep = " ") %>%
    top_n(n=top_rows, wt=n) %>%
    mutate(word = reorder(bigram, n))

  return(bigrams_united)
}

#' analyzeTrigrams
#' @title Analyze Trigrams
#' @description Analyze text with ngram=3 (trigrams).
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param top_rows a numeric vector of length 1. Number of top rows to be returned.
#' @examples
#' \dontrun{
#' analyzeTokens(data.frame(text=c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), stringAsFactors=FALSE))
#' }
#' @import tidytext
#' @import tidyr
#' @import dplyr
#' @export
analyzeTrigrams <- function(in_text, top_rows=25) {
  in_text$text <- as.character(in_text$text)
  trigrams <- in_text %>%
    unnest_tokens(trigram, text, token="ngrams", n=3)

  trigram_counts <- trigrams %>% count(trigram, sort = T)

  trigrams_separated <- trigrams %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ")

  data(stop_words)

  trigrams_filtered <- trigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word)

  # new trigram counts:
  trigram_counts <- trigrams_filtered %>%
    count(word1, word2, word3, sort = TRUE)

  trigrams_united <- trigram_counts %>%
    unite(trigram, word1, word2, word3, sep = " ") %>%
    top_n(n=top_rows, wt = n) %>%
    mutate(word = reorder(trigram, n))

  return(trigrams_united)
}

#' plotNGrams
#' @title Plot Ngrams
#' @description Plot ngrams - Word(s) vs. Count.
#' @author Ravindra Pushker
#' @param ngrams_data a data.frame containing word and n columns.
#' @param top_rows a numeric vector of length 1. Number of top rows to be returned.
#' @param plot_nrows a numeric vector of length 1. Number of rows to be plotted.
#' @examples
#' \dontrun{
#' plotNGrams(data.frame(word=c("test1", "test2"), n=c(25, 30)))
#' }
#' @import ggplot2
#' @export
plotNGrams <- function(ngrams_data, top_rows=25, plot_nrows=25) {
  ngrams_data %>%
    top_n(n=top_rows, wt=n) %>%
    head(plot_nrows) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    ylab('Count') +
    xlab('Word') +
    coord_flip()
}
