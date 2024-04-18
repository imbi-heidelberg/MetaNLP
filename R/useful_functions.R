#' Summary of MetaNLP-objects
#'
#' Returns a quick overview over the \eqn{n} most frequent word stems structured
#' into included and excluded papers.
#'
#' @param object An object of class MetaNLP.
#' @param n Number of most frequent words to be displayed.
#' @param stop_words Boolean to decide wether stop words shall be included in
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
            decision <- NULL

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
              subset(decision == "no") |>
              (`[`)(-c(1, 2)) |>
              colSums() |>
              sort(decreasing = TRUE) |>
              (`[`)(1:n) -> exclude

            # get n most frequent words from "include"
            wcm |>
              subset(decision == "yes") |>
              (`[`)(-c(1, 2)) |>
              colSums() |>
              sort(decreasing = TRUE) |>
              (`[`)(1:n) -> include

            # relative frequency of words
            denom_total <- sum(colSums(wcm[-c(1, 2)]))
            denom_ex <- sum(colSums(subset(wcm, decision == "no")[-c(1, 2)]))
            denom_in <- sum(colSums(subset(wcm, decision == "yes")[-c(1, 2)]))

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
