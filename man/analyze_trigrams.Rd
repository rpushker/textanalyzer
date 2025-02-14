% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ngrams.R
\name{analyze_trigrams}
\alias{analyze_trigrams}
\title{Analyze Trigrams}
\usage{
analyze_trigrams(in_text, top_rows = 25)
}
\arguments{
\item{in_text}{a character vector. Text to be analyzed as a character vector.}

\item{top_rows}{a numeric vector of length 1. Number of top rows to be
returned.}
}
\value{
A data.frame with two columns - trigram (character vector) and count
(numeric vector).
}
\description{
Analyze text with ngram=3 (trigrams).
}
\details{
analyze_trigrams
}
\examples{
analyze_trigrams(in_text=c("The quick brown fox jumps over the lazy dog."))
}
\author{
Ravindra Pushker
}
