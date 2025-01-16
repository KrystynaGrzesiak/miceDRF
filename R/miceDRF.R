
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

  factor_vars <- sapply(missdf, is.factor)

  if(any(factor_vars)){
    warning("Changing factor to numeric.")
    column_names <- colnames(missdf)
    missdf[, factor_vars] <- apply(data.frame(missdf[, factor_vars]), 2, as.numeric)
    colnames(missdf) <- column_names
  }

  args <- c(list(data = missdf, method = "DRF", printFlag = printFlag, m = m), args)

  imputed <- do.call(mice::mice, args)

  mice::complete(imputed)
}


#' mice DRF (Distributional Random Forest)
#'
#' This function performs imputation using MICE and Distributional Random Forest
#'
#' @importFrom drf drf
#'
#' @param y words, words...
#' @param ry words, words...
#' @param x words, words...
#' @param wy words, words...
#' @param min.node.size words, words...
#' @param num.features words, words...
#' @param num.trees words, words...
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
             min.node.size = min.node.size)

  DRFw <- predict(fit, newdata = xmis)$weights # These are the nodes now

  impute <- vapply(1:nrow(xmis), function(s) {
    yobs[sample(1:nrow(yobs), size = 1, replace = T, prob = DRFw[s, ]), ]
  }, numeric(m))  # sample one observation per xmis

  impute

}
