% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_score}
\alias{normalize_score}
\title{Normalize raw scores}
\usage{
normalize_score(x, table, what)
}
\arguments{
\item{x}{vector of raw scores to normalize}

\item{table}{\code{FrequencyTable} or \code{ScoreTable} object}

\item{what}{the values to get. One of either:
\itemize{
\item \code{quan} - the quantile of x in the raw score distribution
\item \code{Z} - normalized Z score for the x raw score
\item name of the scale calculated in \code{ScoreTable} provided to \code{table} argument
}}
}
\value{
Numeric vector with values specified in \code{what} argument
}
\description{
Use computed \code{FrequencyTable} or \code{ScoreTable} to normalize the
provided raw scores.
}
\examples{
# normalize with FrequencyTable
suppressMessages(
  ft <- FrequencyTable(HEXACO_60$HEX_H)
)

normalize_score(HEXACO_60$HEX_H[1:5], ft, what = "Z")

# normalize with ScoreTable
st <- ScoreTable(ft, list(STEN, STANINE))

normalize_score(HEXACO_60$HEX_H[1:5], st, what = "sten")
normalize_score(HEXACO_60$HEX_H[1:5], st, what = "stanine")
}
\seealso{
Other score-normalization functions: 
\code{\link{normalize_scores_df}()},
\code{\link{normalize_scores_grouped}()},
\code{\link{normalize_scores_scoring}()}
}
\concept{score-normalization functions}
