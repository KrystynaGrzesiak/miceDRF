
#' mice DRF (Distributional Random Forest)
#'
#' This function performs imputation using MICE and Distributional Random Forest
#'
#' @param missdf incomplete dataset with missing values denoted as NA's
#' @param printFlag logical, indicating whether silent computations should be
#' performed. Default to FALSE.
#' @param m number of imputed datasets to generate
#' @param ... used for compatibility with mice package.
#'
#' @return completed dataset
#'
#' @examples
#' X <- matrix(rnorm(1000), nrow = 100)
#' X[c(runif(700), rep(1, 300)) < 0.3] <- NA
#' impute_mice_drf(X, printFlag = TRUE)
#'
#' @references
#' This method is described in detail in:
#'
#' Näf, J., Scornet, E., & Josse, J. (2024).
#' What is a good imputation under MAR missingness?. arXiv.
#' \url{https://arxiv.org/abs/2403.19196}
#'
#' It's based on:
#'
#' Cevid, D., Michel, L., Näf, J., Meinshausen, N., and B¨ uhlmann, P. (2022).
#' Distributional random forests: Heterogeneity adjustment and multivariate
#' distributional regression. Journal of Machine Learning Research, 23(333):1–79.
#'
#' @export

impute_mice_drf <- function (missdf, printFlag = FALSE, m = 1, ...) {

  args <- list(...)
  args <- args[setdiff(names(args), c("data", "method", "printFlag"))]

  factor_vars <- as.vector(sapply(missdf, is.factor))

  if(any(factor_vars)){
    warning("Changing factor to numeric.")
    missdf[] <- lapply(missdf, function(col) {
      if (is.factor(col)) as.numeric(levels(col))[col] else col
    })
  }

  args <- c(list(data = missdf, method = "DRF", printFlag = printFlag, m = m),
            args)

  imputed <- do.call(mice::mice, args)

  if(m > 1) {
    res <- lapply(1:m, function(i) mice::complete(imputed, i))
    names(res) <- paste0("imp", 1:m)
    return(res)
  }

  mice::complete(imputed)
}


#' mice DRF (Distributional Random Forest)
#'
#' This function performs imputation using MICE and Distributional Random Forest
#'
#' @importFrom drf drf
#' @importFrom stats predict
#'
#' @param y Vector to be imputed.
#' @param ry Logical vector indicating which elements of `y` are used to fit the
#' imputation model (TRUE = observed).
#' @param x Numeric design matrix with `length(y)` rows, containing predictors
#' for `y` and no missing values.
#' @param wy Logical vector indicating elements of `y` for which imputations are
#' generated.
#' @param min.node.size target minimum number of observations in each tree leaf
#' in DRF. The default value is 5.
#' @param num.features the number of random features to sample.
#' @param num.trees number of trees in DRF. Default to 10.
#' @param ... used for compatibility with \code{mice} package.
#'
#' @references
#' This method is described in detail in:
#'
#' Näf, J., Scornet, E., & Josse, J. (2024).
#' What is a good imputation under MAR missingness?. arXiv.
#' \url{https://arxiv.org/abs/2403.19196}
#'
#' It's based on:
#'
#' Cevid, D., Michel, L., Näf, J., Meinshausen, N., and B¨ uhlmann, P. (2022).
#' Distributional random forests: Heterogeneity adjustment and multivariate
#' distributional regression. Journal of Machine Learning Research, 23(333):1–79.
#'
#' @export


mice.impute.DRF <- function (y, ry, x, wy = NULL, min.node.size = 1,
                            num.features = 10, num.trees = 10, ...) {

  if (is.null(wy)) wy <- !ry

  if (dim(x)[2] == 0) {
    x <- cbind(x, 1)
    dimnames(x) <- list(NULL, "int")
  }

  xobs <- x[ry, , drop = FALSE]
  xmis <- x[wy, , drop = FALSE]
  yobs <- as.matrix(y[ry])

  m <- 1

  fit <- drf(Y = yobs,
             X = xobs,
             num.trees = num.trees,
             num.features = num.features,
             compute.oob.predictions = F,
             min.node.size = min.node.size,
             ci.group.size=1) #ci.group.size=num.trees, is also possible, but appears to be slower

  DRFw <- predict(fit, newdata = xmis)$weights # These are the nodes now

  imputed <- vapply(1:nrow(xmis), function(s) {
    yobs[sample(1:nrow(yobs), size = 1, replace = T, prob = DRFw[s, ]), ]
  }, numeric(m))  # sample one observation per xmis

  imputed

}
