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
setClass("MetaNLP", representation(data_frame = "data.frame",
                                   label = "character"))

#' @param path path to the CSV file
#' @param min_appear minimum number of appearances of a word to become a column
#' of the word count matrix
#' @return an object of class \code{MetaNLP}
#'
#' @details
#' An object of class \code{MetaNLP} contains a slot data_frame where
#' the word count data frame is stored.
#' The CSV file must have a column \code{ID} to identify each paper, a column
#' \code{Title} with the belonging titles of the papers and a column
#' \code{Abstract} which contains the abstracts.
#'
#' @rdname MetaNLP
#' @export
MetaNLP <- function(path, min_appear = 2) {
  # directly remove rows with na values
  file = subset(y <- utils::read.csv(path, header = TRUE, sep = ";"),
                !(is.na(y$Abstract) | is.na(y$Title)))

  suppressWarnings({file |>
    # select the columns "Abstract" and "Title"
    _[c("Title", "Abstract")] |>
    # add new column x where Title and Abstract are pasted
    within(x <- paste(Title, Abstract)) |>
    _$x |>
    # lower case
    tolower()|>
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
    tm::TermDocumentMatrix(control = list(minWordLength = 1)) |>
    as.matrix() |>
    t() -> temp
  })

  # only choose word stems that appear at least a pre-specified number of times
  temp[, colSums(temp) >= min_appear] |>
    as.data.frame() -> temp

  # order by column name
  index_vec <- order(names(temp))
  temp |>
    subset(select = index_vec) -> temp

  # allow for "maybe" as decision
  decision <- ifelse(file$Decision %in% c("include", "maybe"), "yes", "no")

  res <- cbind(file$ID, decision, temp)

  return(new("MetaNLP", data_frame = res))
}
