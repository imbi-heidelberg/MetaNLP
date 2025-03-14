% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete_functions.R
\name{replace_special_characters}
\alias{replace_special_characters}
\alias{replace_special_characters,MetaNLP-method}
\title{Replace special characters in column names}
\usage{
replace_special_characters(object)

\S4method{replace_special_characters}{MetaNLP}(object)
}
\arguments{
\item{object}{An object of class MetaNLP.}
}
\value{
An object of class MetaNLP, where the column names do not have
special characters anymore.
}
\description{
When using non-english languages, the column names of the document-term matrix
can contain special characters. These might lead to encoding problems, when
this matrix is used to train a machine learning model. This functions
automatically replaces all special characters by the nearest equivalent
character, e.g. "é" would be replaced by "e".
}
\examples{
path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
obj <- MetaNLP(path, language = "french")
obj <- replace_special_characters(obj)

}
