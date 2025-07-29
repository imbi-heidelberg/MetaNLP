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
#' path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' obj <- MetaNLP(path)
#' summary(obj, n = 8)
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


#' Save the document-term matrix
#'
#' This function can be used to save the document-term matrix of a MetaNLP object
#' as a csv-file.
#'
#' @param object An object of class MetaNLP.
#' @param path Path where to save the csv.
#' @param type Specifies if the document-term matrix should be saved as
#' "train_wcm.csv" or "test_wcm.csv". If the user wants to use another file name,
#' the whole path including the file name should be given as the \code{path}
#' argument
#' @param ... Additional arguments for \link[utils]{write.table}, e.g. encoding
#' as \code{UTF-8}.
#'
#' @details
#' If a path to a specific folder is given (but the path name does not end with
#' ".csv"), the file is saved in this folder as "train_wcm.csv" or "test_wcm.csv".
#' By providing a path ending with ".csv", the user can override the default
#' naming convention and the file is saved according to this path.
#'
#' @examples
#' path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' obj <- MetaNLP(path)
#' obj2 <- delete_stop_words(obj)
#' write_path <- tempdir()
#' write_csv(obj2, path = write_path)
#' file.remove(file.path(write_path, "train_wcm.csv"))
#'
#' @return nothing
#'
#' @rdname write_csv
#' @export
setMethod("write_csv", signature("MetaNLP"),
          function(object, path, type = c("train", "test"), ...) {
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
            } else {
              type <- match.arg(type)
              path_to_save <- file.path(path, paste0(type, "_wcm.csv"))
            }

            utils::write.csv2(data, file = path_to_save, row.names = FALSE, ...)
          })


#' Read and adapt test data
#'
#' This function takes a MetaNLP object (the training data) and the
#' test data. The function creates the document-term matrix from the test data
#' and matches the columns of the given training MetaNLP object with the columns
#' of the test document-term matrix. This means that columns, which do appear
#' in the test document-term matrix but not in the training document-term matrix are
#' removed; columns that appear in the training document-term matrix but not in the
#' test document-term matrix are added as a column consisting of zeros.
#'
#' @param object The MetaNLP object created from the training data.
#' @param file Either the path to the test data csv, the data frame containing
#' the papers or a MetaNLP object
#' @param ... Further arguments to \code{MetaNLP}.
#'
#' @examples
#' path_train <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' path_test <- system.file("extdata", "test_data_changed.csv", package = "MetaNLP", mustWork = TRUE)
#' obj_train <- MetaNLP(path_train)
#' obj_test <- MetaNLP(path_test)
#' to_test_obj <- read_test_data(obj_train, obj_test)
#'
#' @return An object of class MetaNLP
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



#' Create word cloud from MetaNLP-object
#'
#' This method creates a word cloud from a MetaNLP object. The word size
#' indicates the frequency of the words.
#'
#' @param object A MetaNLP object to plot
#' @param max.words Maximum number of words in the word cloud
#' @param colors Character vector with the colors in
#' @param decision Stratify word cloud by decision. Default is no stratification.
#' @param stop_words Boolean to decide whether stop words shall be included in
#' @param ... Additional parameters for \link[wordcloud]{wordcloud}
#'
#' @examples
#' path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' obj <- MetaNLP(path)
#' plt <- plot(obj)
#'
#' @return nothing
#' @rdname wordcloud
#' @export

setGeneric("wordcloud", function(object, ...) {
  standardGeneric("wordcloud")
})


#' @rdname wordcloud
#' @export
setMethod("wordcloud", signature("MetaNLP"),
          function(object, max.words = 70,
                   colors = c("snow4", "darkgoldenrod1", "turquoise4", "tomato"),
                   decision = c("total", "include", "exclude"),
                   stop_words = FALSE,
                   ...) {

            decision_ <- NULL

            # delete stop words
            if(!stop_words) {
              data <- delete_stop_words(object)@data_frame
            } else {
              data <- object@data_frame
            }

            dec <- match.arg(decision)
            # check whether decision column exists and filter data
            if(dec != "total") {
              if(is.null(object@data_frame$decision_)) {
                warning("Column decision_ does not exist. Word cloud is created by using the whole document-term matrix.")
              }
              else {
                data <- data[data$decision_ == dec, ]
              }
            }

            data$id_ <- NULL
            data$decision_ <- NULL

            # create word cloud
            words <- names(data)
            freqs <- colSums(data)

            wordcloud::wordcloud(words, freqs, max.words = max.words,
                                 random.order = FALSE,
                                 color = colors, ...)

          })
