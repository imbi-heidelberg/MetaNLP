#' Delete list of words
#'
#' There can words that do not offer additional information
#' in the classification whether a paper should be included or excluded
#' from a meta-analysis. Thus, such words should not be part of the word count
#' matrix. This function allows the user to make a list of words which should
#' not be part of the word count matrix.
#'
#' @param object A MetaNLP object, whose data frame is to be modified
#' @param delete_list A character vector containing the words to be deleted
#' @return An object of class \code{MetaNLP}
#'
#' @details
#' The words in \code{delete_list} can be given like they appear in the
#' text. They are lemmatized and stemmed by \code{delete_words} to match the
#' columns of the word count matrix.
#'
#' @export
setGeneric("delete_words", function(object, delete_list) {
  standardGeneric("delete_words")
})



#' @examples
#' obj<- MetaNLP("data/test_data.csv")
#' del_words <- c("beautiful", "considering", "found")
#' obj <- delete_words(obj, del_words)
#'
#' @rdname delete_words
#' @export
setMethod("delete_words", signature("MetaNLP", "character"),
          function(object, delete_list) {
            # lemmatize and stem delete words
            delete_list |>
              textstem::lemmatize_strings() |>
              tm::stemDocument() -> lem_list

            # create vector of words which remain in word count matrix
            col_names <- names(object@data_frame)
            index <- col_names[!(col_names %in% lem_list)]

            object@data_frame <- object@data_frame[index]
            object
          })




#' Delete stop words
#'
#' Usually, stop words do not offer useful information in the classification
#' whether a paper should be included or excluded
#' from a meta-analysis. Thus, such words should not be part of the word count
#' matrix. This function allows the user to automatically delete stop words.
#'
#' @param object A MetaNLP object, whose data frame is to be modified.
#' @param ... Language of the stop words. Defaults to "english".
#' @return An object of class \code{MetaNLP}.
#'
#' @details
#' This function allows to delete stop words from different languages. Supported
#' languages are \code{catalan}, \code{danish}, \code{dutch}, \code{english},
#' \code{finnish}, \code{french}, \code{german}, \code{hungarian}, \code{italian},
#' \code{norwegian}, \code{portugese}, \code{romanian},
#' \code{russian}, \code{spanish} and \code{swedish}. Language names are case
#' sensitive.
#'
#'
#' @export
setGeneric("delete_stop_words", function(object, ...) {
  standardGeneric("delete_stop_words")
})

#' @examples
#' obj <- MetaNLP("data/test_data.csv")
#' obj <- delete_stop_words(obj, "english")
#'
#' @rdname delete_stop_words
#' @export
setMethod("delete_stop_words", signature("MetaNLP"),
          function(object, ...) {

            language <- list(...)
            # define stop words by language
            delete_list <- tm::stopwords(kind = language)

            # delete these words from word count matrix
            object@data_frame <- delete_words(object, delete_list)@data_frame
            object
          })
