% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useful_functions.R
\name{summary,MetaNLP-method}
\alias{summary,MetaNLP-method}
\title{Summary of MetaNLP-objects}
\usage{
\S4method{summary}{MetaNLP}(object, n = 5, stop_words = FALSE, ...)
}
\arguments{
\item{object}{An object of class MetaNLP.}

\item{n}{Number of most frequent words to be displayed.}

\item{stop_words}{Boolean to decide whether stop words shall be included in
the summary. \code{stop_words = TRUE} means, that stop words are included.}

\item{...}{Additional parameters for \code{delete_stop_words} (e.g. language
of the stop words).}
}
\value{
A list of most frequent words.
}
\description{
Returns a quick overview over the \eqn{n} most frequent word stems structured
into included and excluded papers.
}
\examples{
path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
obj <- MetaNLP(path)
summary(obj, n = 8)

}
