#' Natural Language Processing for Meta Analysis
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
#' A \code{MetaNLP} object is the base class of the package \pkg{MetaNLP}.
#' It is initialized by passing the path to a CSV file and constructs
#' a data frame which column names are the words that occur in the titles
#' and abstracts and which cells contain the word counts for each
#' paper.
#'
#' @rdname MetaNLP
setClass("MetaNLP", representation(data_frame = "data.frame"))

#' @param path Path to the CSV file
#' @param bounds An integer vector of length 2. The first value specifies
#' the minimum number of appearances of a word to become a column of the word
#' count matrix, the second value specifies the maximum number.
#' Defaults to \code{c(2, Inf)}.
#' @param word_length An integer vector of length 2. The first value specifies
#' the minimum number of characters of a word to become a column of the word
#' count matrix, the second value specifies the maximum number.
#' Defaults to \code{c(3, Inf)}.
#' @param ... Additional arguments passed on to \code{read.csv2}.
#' See \link[utils]{read.table}.
#' @return An object of class \code{MetaNLP}
#'
#' @details
#' An object of class \code{MetaNLP} contains a slot data_frame where
#' the word count data frame is stored.
#' The CSV file must have a column \code{ID} to identify each paper, a column
#' \code{title} with the belonging titles of the papers and a column
#' \code{abstract} which contains the abstracts. Furthermore, to store the
#' decision for each paper, a column \code{decision} should exist, where the
#' values are either "yes" and "no" or "include" and "exclude". The value "maybe"
#' is handled as a "yes"/"include".
#'
#' @rdname MetaNLP
#' @export
MetaNLP <- function(path, bounds = c(2, Inf), word_length = c(3, Inf), ...) {
  title <- NULL
  abstract <- NULL

  # load file
  file <- utils::read.csv2(path, header = TRUE)

  # make column names lower case
  names(file) <- tolower(names(file))

  # only select rows without na values
  file <-  subset(file, !(is.na(file$abstract) | is.na(file$title)))

  suppressWarnings({file |>
    # select the columns "abstract" and "title"
    (`[`)(c("title", "abstract")) |>
    # add new column x where Title and Abstract are pasted
    within(x <- paste(title, abstract)) |>
    (`[[`)(c("x")) |>
    # lower case
    tolower() |>
    # lemmatization of the words
    textstem::lemmatize_strings() |>
    tm::VectorSource() |>
    # create corpus object
    tm::Corpus() |>
    # remove special characters
    tm::tm_map(tm::content_transformer(replaceSpecialChars)) |>
    # strip white space
    tm::tm_map(tm::stripWhitespace) |>
    # only use word stems
    tm::tm_map(tm::stemDocument) |>
    # create matrix
    tm::TermDocumentMatrix(control = list(wordLengths = word_length)) |>
    as.matrix() |>
    t() |>
    as.data.frame() -> temp
  })

  # only choose word stems that appear at least a pre-specified number of times
  temp <- temp[, colSums(temp) >= bounds[1] & colSums(temp) <= bounds[2]]

  # order by column name
  index_vec <- order(names(temp))
  temp |>
    subset(select = index_vec) -> temp

  # allow for "maybe" as decision
  decision <- ifelse(file$decision %in% c("include", "maybe", "yes"), "yes", "no")

  # add columns containing the ids of the papers and the belonging decisions
  res <- cbind(id = file$id, decision, temp)

  return(new("MetaNLP", data_frame = res))
}
