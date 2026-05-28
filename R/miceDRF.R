#' Imputation with Distributional Random Forests for 'mice'
#'
#' Imputes missing values using distributional random forests within the
#' multiple imputation by chained equations framework implemented in the
#' \pkg{mice} package.
#'
#' This function is called internally by \code{mice} when the imputation
#' method is set to \code{"DRF"}. For each variable with missing values, a
#' distributional random forest is fitted to the observed values using the
#' remaining variables as predictors. Missing values are then imputed by
#' sampling observed responses according to the forest weights.
#'
#' @importFrom drf drf
#' @importFrom stats predict
#'
#' @param y Vector to be imputed.
#' @param ry Logical vector indicating which elements of \code{y} are observed
#' and used to fit the imputation model.
#' @param x Numeric design matrix with \code{length(y)} rows, containing
#' predictors for \code{y}. The matrix should not contain missing values.
#' @param wy Logical vector indicating elements of \code{y} for which
#' imputations are generated. If \code{NULL}, defaults to \code{!ry}.
#' @param min.node.size Target minimum number of observations in each tree leaf
#' in the distributional random forest. Default is \code{1}.
#' @param num.features Number of random features to sample at each split.
#' Default is \code{10}.
#' @param num.trees Number of trees in the distributional random forest.
#' Default is \code{10}.
#' @param ... Additional arguments passed by \code{mice} for compatibility with
#' the \code{mice.impute} interface. Currently ignored.
#'
#' @return A numeric vector of imputed values for the entries of \code{y}
#' indicated by \code{wy}. The vector has length \code{sum(wy)} and is
#' returned to \code{mice} to replace the missing values in the current
#' variable.
#'
#' @references
#' Näf, J., Scornet, E., and Josse, J. (2024).
#' "What is a good imputation under MAR missingness?"
#' \url{https://arxiv.org/abs/2403.19196}.
#'
#' Cevid, D., Michel, L., Näf, J., Meinshausen, N., and Buehlmann, P. (2022).
#' "Distributional random forests: Heterogeneity adjustment and multivariate
#' distributional regression."
#' Journal of Machine Learning Research, 23(333), 1--79.
#'
#' @examples
#' library(mice)
#'
#' set.seed(123)
#' X <- matrix(rnorm(1000), nrow = 100)
#' X[runif(length(X)) < 0.3] <- NA
#'
#' imp <- mice(X, method = "DRF", m = 1, maxit = 1, printFlag = FALSE)
#' complete(imp)
#'
#' @export


mice.impute.DRF <- function(y, ry, x, wy = NULL, min.node.size = 1,
                            num.features = 10, num.trees = 10, ...) {
  if (is.null(wy)) {
    wy <- !ry
  }

  if (ncol(x) == 0) {
    x <- cbind(x, 1)
    dimnames(x) <- list(NULL, "int")
  }

  xobs <- x[ry, , drop = FALSE]
  xmis <- x[wy, , drop = FALSE]
  yobs <- as.matrix(y[ry])

  fit <- drf(
    Y = yobs,
    X = xobs,
    num.trees = num.trees,
    num.features = num.features,
    compute.oob.predictions = FALSE,
    min.node.size = min.node.size,
    ci.group.size = 1
  )

  drf_weights <- predict(fit, newdata = xmis)$weights

  vapply(seq_len(nrow(xmis)), function(s) {
    yobs[
      sample(seq_len(nrow(yobs)), size = 1, replace = TRUE,
             prob = drf_weights[s, ]),
      1
    ]
  }, numeric(1))
}


