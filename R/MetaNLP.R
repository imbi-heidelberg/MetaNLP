#' Natrual Language Processing for Meta Analysis
#'
#' The \pkg{MetaNLP} package provides methods to quickly transform a
#' CSV-file with titles and abstracts to an R data frame that can be
#' used for automatic title-abstract screening using machine learning.
#'
#'
#' @import methods
#' @name MetaNLP
"_PACKAGE"


#' Create a data frame with word counts
#'
#' A \code{MetaNLP} object is the base class of the \pkg{MetaNLP}.
#' It is initialized by passing the path to a CSV file and constructs
#' a data frame which column names are the words that occur in the titles
#' and abstracts and which cells contain the word counts for each
#' paper.
#'
#' @rdname MetaNLP
setClass("MetaNLP", representation(data_frame = "data.frame",
                                   label = "character"))

#' @param path path to the CSV file
#' @return an object of class \code{MetaNLP}
#'
#' @details
#' An object of class \code{MetaNLP} contains a slot data_frame where
#' the word count data frame is stored.
#'
#' @rdname MetaNLP
#' @export
MetaNLP <- function(path) {
  file = utils::read.csv(path, header = TRUE, sep = ";")
  # TODO: modify here
  return(new("MetaNLP", data_frame = file))
}
