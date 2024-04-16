#' Select features via elasticnet regularization
#'
#' As the word count matrix quickly grows with an increasing number of abstracts,
#' it can easily reach several thousand columns. Thus, it can be important to
#' extract the columns that carry most of the information in the decision making
#' process. This function uses a generalized linear model combined with
#' elasticnet regularization to extract these features.
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
#' For further documentation and background information on \code{\link{glmnet}},
#' have a look at the
#' \href{https://glmnet.stanford.edu/articles/glmnet.html#logistic-regression-family-binomial}{online documentation}.
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
#' @param random A boolean indicating whether a local seed for this function
#' should be set or not. Default is \code{TRUE}, so no seed is set.
#' Setting the seed (meaning \code{random = FALSE}) leads to replicable results of
#' the cross validation, such that each call of \code{select_features} selects
#' the same columns. If \code{random = FALSE}, the option \code{lambda = "avg"}
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
#' \dontrun{
#' obj <- MetaNLP("test_data.csv")
#'
#' obj <- select_features(obj, alpha = 0.7)
#' obj <- select_features(obj, lambda = "1se", type.measure = "auc")
#' }
#'
#' @rdname select_features
#' @export
setMethod("select_features", signature("MetaNLP"),
          function(object, ..., alpha = 0.8, lambda = "avg", random = TRUE) {

            # set seed (but only locally within this function)
            if(!random) {
              old <- .Random.seed
              set.seed(42)
              on.exit( {.Random.seed <<- old})
            }

            # extract feature and response vector
            y <- as.factor(object@data_frame$decision)
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
              if(lambda == "avg" & random) {
                l_avg <- rep(0, 10)
                for(i in 1:10) {
                  cvmodel <- glmnet::cv.glmnet(x, y,
                                               family = "binomial",
                                               alpha = alpha, ...)
                  l_avg[i] <- cvmodel$lambda.min
                }

                lambda <- median(l_avg)
              }
              # if seed is fix value, lambda = "avg" is equal to lambda = "min"
              else if(lambda == "avg" & !random) {
                cvmodel <- cv.glmnet(x, y,
                                     family = "binomial",
                                     alpha = alpha, ...)
                lambda <- cvmodel$lambda.min
              }
            }

            # get non-zero coefficients
            res <- coef(model, s = lambda)

            # select columns with non-zero coefficients
            object@data_frame <- object@data_frame[-c(1, 2)][res@i[-1]]
            object
          })
