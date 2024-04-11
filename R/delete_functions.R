#' Delete list of words
#'
#' There can words that do not offer additional information
#' in the classification process whether a paper should be included or excluded
#' from a meta-analysis. Thus, such words should not be part of the word count
#' matrix. This function allows the user to make a list of words which should
#' not be part of the word count matrix.
#'
#' @param object A MetaNLP-object, whose data frame is to be modified
#' @param delete_list A character vector containing the words to be deleted
#'
#' @details
#' The words in \code{delete_list} can be given like they appear in the
#' text. They are lemmatized and stemmed by \code{delete_words} to match the
#' columns of the word count matrix.
#'
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
