
#' Very internal function for getting mice methods
#'
#' @examples
#' methods <- c("pmm", "cart", "sample", "norm.nob", "DRF")
#' imputation_funcs <- create_mice_imputations(methods)
#'
#' @keywords internal
#'

create_mice_imputations <- function(methods, maxit = 5) {
  imputation_funcs <- lapply(methods, function(ith_method) {
    function(X, m, maxit = 5) {
      imp_dat <- mice(X, m = m, method = ith_method, printFlag = FALSE,
                      visitSequence = "arabic", maxit = maxit)
      mice::complete(imp_dat, action = "all")
    }
  })
  names(imputation_funcs) <- methods
  imputation_funcs
}


#' IScore
#'
#' This function... TODO: details
#'
#' @param X data containing missing values denoted with NAs
#'
#' @param N a numeric value. Number of samples from imputation distribution H.
#' Default to 50.
#'
#' @param imputation_funcs A list of functions, whereby each
#' `imputation_funcs[[method]]` is a function that takes the arguments \code{X}
#' and \code{m} and imputes \code{X} \code{m} times using method:
#' \code{imputations = imputation_funcs[[method]](X,m)}. Default to NULL meaning
#' ... TODO::
#'
#' @param imputations Either \code{NULL} or a list of imputations for the
#' methods considered, each imputed \code{X} saved as
#' \code{imputations[[method]]}, whereby method is a string. Default to
#' \code{NULL}.
#'
#' @param max_length Maximum number of variables \eqn{X_j} to consider, can
#' speed up the code
#'
#' @return a list of scores calculated for every imputation function from
#' \code{imputation_funcs}. Each result consists of two elements:
#'
#' - a weighted score,
#' - a table with scores calculated for particular columns.
#'
#' @examples
#' X <- matrix(rnorm(1000), nrow = 100)
#' X[c(runif(700), rep(1, 300)) < 0.3] <- NA
#' methods <- c("pmm", "cart", "sample", "norm.nob")
#' imputation_funcs <- create_mice_imputations(methods)
#' Iscores_new(X, N = 50, imputation_funcs)
#'
#' @export
#'


Iscores_new <- function(X, N = 50, imputation_funcs = NULL, imputations = NULL,
                        max_length = NULL){

  methods_names <- names(imputation_funcs)

  lapply(methods_names, function(ith_method) {
    print(paste0("Evaluating method ", ith_method))

    X_imp <- imputations[[ith_method]][[1]]

    data.frame(method = ith_method,
               Iscores_new_perimp(X, X_imp = X_imp, N = N,
                                  imputation_func = imputation_funcs[[ith_method]],
                                  max_length = max_length))
  }) |>
    do.call(rbind, args = _)
}


#' @title Calculates score for a single imputation function
#'
#' @importFrom scoringRules crps_sample
#'
#' @keywords internal
#'


Iscores_new_perimp <- function(X, X_imp, N = 50, imputation_func, max_length = NULL){

  if (is.null(X_imp))
    X_imp <- imputation_func(X = X, m = 1)[[1]]

  X <- as.matrix(X)
  X_imp <- as.matrix(X_imp)

  n <- nrow(X)

  ## Reoder the data according to the number of missing values (least missing first)
  missings_per_col <- colSums(is.na(X))

  ## Miss pattern
  M <- is.na(X)

  dim_with_NA <- missings_per_col[missings_per_col > 0]
  dim_with_NA_ord <- order(missings_per_col[missings_per_col > 0], decreasing = T)

  if (is.null(max_length)) max_length <- length(dim_with_NA)

  if (length(dim_with_NA) < max_length){
    warning("max_length is larger than the total number of columns with missing values!")
    max_length <- sum(dim_with_NA)
  }

  scores_dat <- lapply(dim_with_NA_ord, function(j) {

    weight <- (dim_with_NA[j] / n) * ((n - dim_with_NA[j]) / n)

    if(dim_with_NA[j] < 10) {
      warning('Sample size of missing and nonmissing too small for nonparametric distributional regression, setting to NA')
      return(data.frame(column_id = j, weight = weight, score = NA, weighted_score = NA)) # return score = NA
    }

    # Fully observed columns except j
    Oj <- colSums(is.na(X[!M[, j], ][, -j])) == 0

    # Only take those that are fully observed
    # H for all observed values of X_j
    X_imp_0 <- X_imp[!M[, j], ]
    X_test <- X_imp_0[, -j][, Oj]
    Y_test <- X_imp_0[, j]

    # Only take those that are fully observed
    # H for all missing values of X_j
    X_imp_1 <- X_imp[M[, j], ]
    X_train <- X_imp_1[, -j][, Oj]
    Y_train <- X_imp_1[, j]

    if(!any(Oj)){
      warning("Oj was empty. There was no complete columns for training.")
      return(data.frame(column_id = j, weight = weight, score = NA, weighted_score = NA)) # return score = NA
    }

    # Train DRF on imputed data
    X_artificial <- rbind(cbind(NA, X_test), cbind(Y_train, X_train))
    imputation_list <- imputation_func(X = X_artificial , m = N)
    Y_matrix <- do.call(cbind, lapply(imputation_list, function(x)  x[1:length(Y_test), 1]))

    score_j <- -mean(scoringRules::crps_sample(y = Y_test, dat = Y_matrix))

    data.frame(column_id = j,
               weight = weight,
               score = score_j)
  }) |>
    do.call(rbind, args = _)

  weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
                          (sum(scores_dat[["weight"]], na.rm = T)), na.rm = T)

  data.frame(scores_dat,
             weighted_score = weighted_score)
}
