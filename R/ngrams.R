#' analyze_ngrams
#' @title Analyze NGrams
#' @description Analyze text with ngram among 1, 2 or 3.
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param ngram a numeric_vector of length 1. Ngram = 1, 2 or 3.
#' @param top_rows a numeric vector of length 1. Number of top rows to be
#' returned.
#' @examples
#' analyze_ngrams(in_text=c("How much wood would a woodchuck chuck if a
#' woodchuck could chuck wood?"))
#' @export
analyze_ngrams <- function(in_text, ngram = 1, top_rows = 25) {
  if (!(is.character(in_text) || is.factor(in_text))) {
    stop("The input text should be either character or factor.")
  }

  if (ngram == 1) {
    analyze_tokens(in_text, top_rows)
  } else if (ngram == 2) {
    analyze_bigrams(in_text, top_rows)
  } else if (ngram == 3) {
    analyze_trigrams(in_text, top_rows)
  } else {
    stop("Functionality for ngram=", n, " is still not available.")
  }
}

#' analyze_tokens
#' @title Analyze Tokens
#' @description Analyze text with ngram=1
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param top_rows a numeric vector of length 1. Number of top rows to be
#' returned.
#' @examples
#' analyze_tokens(in_text=c("How much wood would a woodchuck chuck if a
#' woodchuck could chuck wood?"))
#' @import tidytext
#' @import dplyr
#' @export
analyze_tokens <- function(in_text, top_rows = 25) {
  word <- NULL

  in_text <- data.frame(text = in_text, stringsAsFactors = FALSE)
  in_text$text <- as.character(in_text$text)

  token_df <- tidytext::unnest_tokens(tbl = in_text,
                                      output = "word",
                                      input = "text")

  # Remove stop words like the, in, a, of etc.
  stop_words <- tidytext::stop_words

  token_df <- token_df %>% dplyr::anti_join(stop_words, by = "word")

  token_count <- token_df %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::top_n(n = top_rows, wt = n) %>%
    dplyr::mutate(word = stats::reorder(word, n))

  token_count
}

#' analyze_bigrams
#' @title Analyze Bigrams
#' @description Analyze text with ngram=2 (bigrams).
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param top_rows a numeric vector of length 1. Number of top rows to be
#' returned.
#' @examples
#' analyze_bigrams(in_text=c("How much wood would a woodchuck chuck if a
#' woodchuck could chuck wood?"))
#' @import tidytext
#' @import tidyr
#' @import dplyr
#' @export
analyze_bigrams <- function(in_text, top_rows = 25) {
  bigram <- word1 <- word2 <- NULL

  in_text <- data.frame(text = in_text, stringsAsFactors = FALSE)
  in_text$text <- as.character(in_text$text)

  bigrams <- tidytext::unnest_tokens(tbl = in_text, output = "bigram",
                                     input = "text", token = "ngrams", n = 2)

  bigram_counts <- bigrams %>%
    dplyr::count(bigram, sort = TRUE)

  bigrams_separated <- bigrams %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ")

  stop_words <- tidytext::stop_words

  bigrams_filtered <- bigrams_separated %>%
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word)

  # new bigram counts:
  bigram_counts <- bigrams_filtered %>%
    dplyr::count(word1, word2, sort = TRUE)

  bigrams_united <- bigram_counts %>%
    tidyr::unite(bigram, word1, word2, sep = " ") %>%
    dplyr::top_n(n = top_rows, wt = n) %>%
    dplyr::mutate(word = stats::reorder(bigram, n))

  bigrams_united
}

#' analyze_trigrams
#' @title Analyze Trigrams
#' @description Analyze text with ngram=3 (trigrams).
#' @author Ravindra Pushker
#' @param in_text a character vector. Text to be analyzed as a character vector.
#' @param top_rows a numeric vector of length 1. Number of top rows to be
#' returned.
#' @import tidytext
#' @import tidyr
#' @import dplyr
#' @examples
#' analyze_trigrams(in_text=c("How much wood would a woodchuck chuck if a
#' woodchuck could chuck wood?"))
#' @export
analyze_trigrams <- function(in_text, top_rows = 25) {
  trigram <- word1 <- word2 <- word3 <- NULL

  in_text <- data.frame(text = in_text, stringsAsFactors = FALSE)

  in_text$text <- as.character(in_text$text)

  trigrams <- tidytext::unnest_tokens(tbl = in_text, output = "trigram",
                                      input = "text", token = "ngrams", n = 3)

  trigram_counts <- trigrams %>% dplyr::count(trigram, sort = TRUE)

  trigrams_separated <- trigrams %>%
    tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ")

  stop_words <- tidytext::stop_words

  trigrams_filtered <- trigrams_separated %>%
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word) %>%
    dplyr::filter(!word3 %in% stop_words$word)

  # new trigram counts:
  trigram_counts <- trigrams_filtered %>%
    dplyr::count(word1, word2, word3, sort = TRUE)

  trigrams_united <- trigram_counts %>%
    tidyr::unite(trigram, word1, word2, word3, sep = " ") %>%
    dplyr::top_n(n = top_rows, wt = n) %>%
    dplyr::mutate(word = stats::reorder(trigram, n))

  trigrams_united
}

#' plot_ngrams
#' @title Plot Ngrams
#' @description Plot ngrams - Word(s) vs. Count.
#' @author Ravindra Pushker
#' @param ngrams_data a data.frame containing word and n columns.
#' @param top_rows a numeric vector of length 1. Number of top rows to be
#' returned.
#' @param plot_nrows a numeric vector of length 1. Number of rows to be plotted.
#' @examples
#' plot_ngrams(data.frame(word=c("test1", "test2"), n=c(25, 30)))
#' @import ggplot2
#' @import utils
#' @export
plot_ngrams <- function(ngrams_data, top_rows = 25, plot_nrows = 25) {
  word <- NULL

  ngrams_data %>%
    dplyr::top_n(n = top_rows, wt = n) %>%
    utils::head(plot_nrows) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    ylab("Count") +
    xlab("Word") +
    coord_flip()
}
