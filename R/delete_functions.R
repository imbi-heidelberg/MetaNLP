#' Delete list of words
#'
#' There can be words that do not offer additional information
#' in the classification whether a paper should be included or excluded
#' from a meta-analysis. Thus, such words should not be part of the word count
#' matrix. This function allows the user to remove these columns of the word
#' count matrix by specifying a vector of words to delete.
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
#' path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' obj <- MetaNLP(path)
#' del_words <- c("beautiful", "considering", "found")
#' obj <- delete_words(obj, del_words)
#'
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
#' languages are \code{english}, \code{french}, \code{german}, \code{russian} and
#' \code{spanish}. Language names are case sensitive.
#'
#'
#' @export
setGeneric("delete_stop_words", function(object, ...) {
  standardGeneric("delete_stop_words")
})

#' @examples
#' path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' obj <- MetaNLP(path)
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



#' Replace special characters in column names
#'
#' When using non-english languages, the column names of the word count matrix
#' can contain special characters. These might lead to encoding problems, when
#' this matrix is used to train a machine learning model. This functions
#' automatically replaces all special characters by the nearest equivalent
#' character, e.g. "é" would be replaced by "e".
#'
#' @param object An object of class MetaNLP.
#' @return An object of class MetaNLP, where the column names do not have
#' special characters anymore.
#'
#' @examples
#' path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' obj <- MetaNLP(path, language = "french")
#' obj <- replace_special_characters(obj)
#'
#' @rdname replace_special_characters
#' @export
setGeneric("replace_special_characters", function(object) {
  standardGeneric("replace_special_characters")
})

#' @rdname replace_special_characters
#' @export
setMethod("replace_special_characters", signature("MetaNLP"),
          function(object) {

            # get column names
            names <- colnames(object@data_frame[-c(1, 2)])
            data <- object@data_frame[-c(1, 2)]

            # replace all special characters
            names <- gsub("[\u00E4\u00E0\u00E1\u00E2\u00E3\u00E5\u0103]", "a", names)
            names <- gsub("[\u00EB\u00E8\u00E9\u00EA]", "e", names)
            names <- gsub("[\u00EE\u00EF\u00ED]", "i", names)
            names <- gsub("[\u00F6\u00F4\u00F3\u00F5]", "o", names)
            names <- gsub("[\u00FC\u00F9\u00FA\u00FB]", "u", names)
            names <- gsub("\u00DF", "ss", names)
            names <- gsub("\u00E7", "c", names)
            names <- gsub("\u00F1", "n", names)

            colnames(data) <- names

            # add columns that could now have the same column names
            data <- as.data.frame(t(rowsum(t(data), group = colnames(data))))

            names <- c("id_", "decision_", colnames(data))
            colnames(object@data_frame) <- names
            object
          })
