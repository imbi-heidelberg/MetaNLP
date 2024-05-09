#' Summary of MetaNLP-objects
#'
#' Returns a quick overview over the \eqn{n} most frequent word stems structured
#' into included and excluded papers.
#'
#' @param object An object of class MetaNLP.
#' @param n Number of most frequent words to be displayed.
#' @param stop_words Boolean to decide whether stop words shall be included in
#' the summary. \code{stop_words = TRUE} means, that stop words are included.
#' @param ... Additional parameters for \code{delete_stop_words} (e.g. language
#' of the stop words).
#'
#' @return A list of most frequent words.
#'
#' @examples
#' \dontrun{
#' obj <- MetaNLP("test_data.csv")
#' summary(obj, n = 8)
#' }
#'
#' @rdname summary
#' @export
setMethod("summary", signature("MetaNLP"),
          function(object, n = 5, stop_words = FALSE, ...) {
            decision_ <- NULL

            # delete stop words
            if(!stop_words) {
              wcm <- delete_stop_words(object, ...)@data_frame
            } else {
              wcm <- object@data_frame
            }

            # get n most frequent words
            wcm |>
              (`[`)(-c(1, 2)) |>
              colSums() |>
              sort(decreasing = TRUE) |>
              (`[`)(1:n) -> total

            # get n most frequent words from "exclude"
            wcm |>
              subset(decision_ == "exclude") |>
              (`[`)(-c(1, 2)) |>
              colSums() |>
              sort(decreasing = TRUE) |>
              (`[`)(1:n) -> exclude

            # get n most frequent words from "include"
            wcm |>
              subset(decision_ == "include") |>
              (`[`)(-c(1, 2)) |>
              colSums() |>
              sort(decreasing = TRUE) |>
              (`[`)(1:n) -> include

            # relative frequency of words
            denom_total <- sum(colSums(wcm[-c(1, 2)]))
            denom_ex <- sum(colSums(subset(wcm, decision_ == "exclude")[-c(1, 2)]))
            denom_in <- sum(colSums(subset(wcm, decision_ == "include")[-c(1, 2)]))

            rel_total <- paste(round(total / denom_total * 100,
                               digits = 2), "%")
            rel_exclude <- paste(round(exclude /  denom_ex * 100,
                                       digits = 2), "%")
            rel_include <- paste(round(include / denom_in * 100,
                                       digits = 2), "%")

            # return list with all information
            list("Total" = noquote(rbind("Absolute" = total,
                                         "Relative" = rel_total)),
                 "Exclude" = noquote(rbind("Absolute" = exclude,
                                           "Relative" = rel_exclude)),
                 "Include" = noquote(rbind("Absolute" = include,
                                           "Relative" = rel_include)))
          })


#' @rdname write_csv
#' @export
setGeneric("write_csv", function(object, ...) {
  standardGeneric("write_csv")
})


#' Save the word count matrix
#'
#' This function can be used to save the word count matrix of a MetaNLP object
#' as a csv-file.
#'
#' @param object An object of class MetaNLP.
#' @param path Path where to save the csv. If no path is set, the csv is saved
#' in the current working directory
#' @param type Specifies if the word count matrix should be saved as
#' "train_wcm.csv" or "test_wcm.csv". If the user wants to use another file name,
#' the whole path including the file name should be given as the \code{path}
#' argument
#' @param ... Additional arguments for \link[utils]{write.table}, e.g. encoding
#' as \code{UTF-8}.
#'
#' @details
#' Overall, there are three options to specify the path. By
#' default, no path is set, so the csv is saved as "train_wcm.csv" or
#' "test_wcm.csv" in the current working directory. If a path to a specific
#' folder is given (but the path name does not end with ".csv"), the file is
#' saved in this folder as "train_wcm.csv" or "test_wcm.csv".
#' By providing a path ending with ".csv", the user can override the default
#' naming convention and the file is saved according to this path.
#'
#' @examples
#' \dontrun{
#' obj <- MetaNLP("test_data.csv")
#' obj2 <- delete_stop_words(obj)
#' write_csv(obj2)
#' write_csv(obj2, path = "foo.csv")
#' }
#'
#'
#' @rdname write_csv
#' @export
setMethod("write_csv", signature("MetaNLP"),
          function(object, path = "", type = c("train", "test"), ...) {
            lastchar <- 0

            # extract data
            data <- object@data_frame

            # extract last 3 characters from path
            if(nchar(path) > 3) {
              lastchar <- substr(path, nchar(path) - 3, nchar(path))
            }

            # create file path
            if(lastchar == ".csv") {
              path_to_save <- path
            } else if(path == ""){
              type <- match.arg(type)
              path_to_save <- paste0(type, "_wcm.csv")
            } else {
              type <- match.arg(type)
              path_to_save <- file.path(path, paste0(type, "_wcm.csv"))

            }

            utils::write.csv2(data, file = path_to_save, row.names = FALSE, ...)
          })


#' Read and adapt test data
#'
#' This function takes a MetaNLP object (the training data) and the
#' test data csv. The function creates the word count matrix from the test data
#' and matches the columns of the given training MetaNLP object with the columns
#' of the test word count matrix. This means that columns, which do appear
#' in the test word count matrix but not in the training word count matrix are
#' removed; columns that appear in the training word count matrix but not in the
#' test word count matrix are added as a column consisting of zeros.
#'
#' @param object The MetaNLP object created from the training data.
#' @param file Either the path to the test data csv, the data frame containing
#' the papers or a MetaNLP object
#' @param ... Further arguments to \code{MetaNLP}.
#'
#' @examples
#' \dontrun{
#' obj <- MetaNLP("test_data.csv")
#' to_test_obj <- read_test_data(obj, "path/to_test.csv")
#' }
#'
#' @rdname read_test_data
#' @export
setGeneric("read_test_data", function(object, ...) {
  standardGeneric("read_test_data")
})

#' @rdname read_test_data
#' @export
setMethod("read_test_data", signature("MetaNLP"),
          function(object, file, ...) {

            # read test data
            if(is(file, "MetaNLP")) test_obj <- file
            else test_obj <- MetaNLP(file, ...)

            # delete columns id and decision
            test_data <- test_obj@data_frame[-1]
            names_obj  <- colnames(object@data_frame[-c(1, 2)])
            names_test <- colnames(test_data)

            # get columns that are in test csv but not in training csv
            excess_index <- names_test %in% names_obj
            test_data <- test_data[, excess_index]

            # get column names that are in train csv but not in test csv
            miss_index <- !(names_obj %in% names_test)
            miss_names <- names_obj[miss_index]

            # add these columns as columns only consisting of zeros
            zeros <- rep(0, length(miss_names) * nrow(test_data))
            df <- data.frame(matrix(zeros, ncol = length(miss_names),
                                    nrow = nrow(test_data)))

            colnames(df) <- miss_names

            test_data <- cbind(test_data, df)

            # order by column name
            index_vec <- order(names(test_data))
            test_data |>
              subset(select = index_vec) -> test_data

            # add the column id
            test_data <- cbind("id_" = test_obj@data_frame$id_, test_data)
            test_obj@data_frame <- test_data
            test_obj
          })
