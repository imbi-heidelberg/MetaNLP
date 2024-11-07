#' Select features via elasticnet regularization
#'
#' As the document-term matrix quickly grows with an increasing number of abstracts,
#' it can easily reach several thousand columns. Thus, it can be important to
#' extract the columns that carry most of the information in the decision making
#' process. This function uses a generalized linear model combined with
#' elasticnet regularization to extract these features. In contrast to a usual
#' regression model or a L2 penalty (ridge regression), elasticnet (and LASSO)
#' sets some regression parameters to 0. Thus, the selected features are exactly
#' the features with a non-zero entry.
#'
#'
#' @return An object of class \code{MetaNLP}, where the columns were selected
#' via elastic net.
#' @details The computational aspects are executed by the \code{\link{glmnet}}
#' package. At first, a model is fitted via \link[glmnet]{glmnet}. The
#' elastic net parameter \eqn{\alpha} can be specified by the user. The
#' parameter \eqn{\lambda}, which determines the weight of the penalty, can
#' either be chosen via cross validation (using \link[glmnet]{cv.glmnet} or by
#' giving a numeric value.
#'
#' @importFrom glmnet glmnet
#'
#' @rdname select_features
#' @export
setGeneric("select_features", function(object, ...) {
  standardGeneric("select_features")
})


#' @param object An object of class \code{MetaNLP}
#' @param alpha The elastic net mixing parameter, with \eqn{0\leq \alpha \leq 1}.
#' \code{alpha = 1} then equals the lasso penalty, \code{alpha = 0} is the ridge
#' penalty.
#' @param lambda The weight parameter of the penalty. The possible values are
#' \code{"avg", "min", "1se"} or a numeric value which directly determines
#' \eqn{\lambda}. When choosing \code{"avg", "min"} or \code{"1se"}, cross
#' validation is executed to determine \eqn{\lambda}.
#' Note that cross validation uses random folds, so the results are not necessarily
#' replicable.
#' "avg" calls \code{select_features} 10 times, computes the \eqn{\lambda} which
#' minimizes the loss for each iteration and then uses the median of these
#' values as the final value, for which the objective function is
#' minimized. \code{"min"} and \code{"1se"} carry out the cross validation just
#' once and \eqn{\lambda} is either the value, for which the cross-validated
#' error is minimized (option \code{"min"}) or the value, that gives
#' the most regularized model such that the cross-validated error is within
#' one standar error of the minimum (option \code{"1se"}).
#' @param seed A numeric value which is used as a local seed for this function.
#' Default is \code{seed = NULL}, so no seed is set.
#' Setting a seed leads to replicable results of
#' the cross validation, such that each call of \code{select_features} selects
#' the same columns. If a seed is set, the option \code{lambda = "avg"}
#' yields the same results as \code{lambda = "min"}.
#' @param ... Additional arguments for \link[glmnet]{cv.glmnet}. An important
#' option might be \code{type.measure} to specify which loss is used when
#' the cross validation is executed.
#'
#' @note
#' By using a fix value for \code{lambda}, the number of features which should
#' be selected can easily be adjusted by the parameter \code{alpha}. The smaller
#' one chooses \code{alpha}, the more columns will still be present in the
#' resulting data frame, the higher one chooses \code{alpha}, the less
#' columns will be chosen.
#'
#' @examples
#' path <- system.file("extdata", "test_data.csv", package = "MetaNLP", mustWork = TRUE)
#' obj <- MetaNLP(path)
#' obj2 <- select_features(obj, alpha = 0.7, lambda = "min")
#'
#'
#' @rdname select_features
#' @export
setMethod("select_features", signature("MetaNLP"),
          function(object, alpha = 0.8, lambda = "avg", seed = NULL, ...) {

            # set seed (but only locally within this function)
            if(!is.null(seed)) {
              old <- .Random.seed
              set.seed(seed)
              on.exit( {.Random.seed <<- old})
            }

            # extract feature and response vector
            y <- as.factor(object@data_frame$decision_)
            x <- as.matrix(object@data_frame[-c(1, 2)])

            # fit model
            model <- glmnet::glmnet(x, y,
                                    family = "binomial",
                                    alpha = alpha)

            # lambda either "avg", "min" or "1se"
            if(is.character(lambda)) {

              # case 1: lambda either "min" or "1se"
              if(lambda %in% c("min", "1se")) {
                cvmodel <- glmnet::cv.glmnet(x, y,
                                             family = "binomial",
                                             alpha = alpha, ...)

                if(lambda == "min") lambda <- cvmodel$lambda.min
                if(lambda == "1se") lambda <- cvmodel$lambda.1se
              }

              # case 2: lambda = "avg"
              # average only makes sense if seed is not fix value
              if(lambda == "avg" & is.null(seed)) {
                l_avg <- rep(0, 10)
                for(i in 1:10) {
                  cvmodel <- glmnet::cv.glmnet(x, y,
                                               family = "binomial",
                                               alpha = alpha, ...)
                  l_avg[i] <- cvmodel$lambda.min
                }

                lambda <- stats::median(l_avg)
              }
              # if seed is fix value, lambda = "avg" is equal to lambda = "min"
              else if(lambda == "avg" & !is.null(seed)) {
                cvmodel <- glmnet::cv.glmnet(x, y,
                                     family = "binomial",
                                     alpha = alpha, ...)
                lambda <- cvmodel$lambda.min
              }
            }

            # get non-zero coefficients
            res <- stats::coef(model, s = lambda)

            # select columns with non-zero coefficients
            interim <- object@data_frame[-c(1, 2)][res@i[-1]]
            object@data_frame <- cbind(object@data_frame[c(1, 2)], interim)
            object
          })
