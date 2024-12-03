
#' Very internal function for getting mice methods
#'
#' @importFrom mice mice
#' @importFrom mice complete
#'
#' @examples
#' methods <- "pmm"
#' imputation_funcs <- create_mice_imputations(methods)
#'
#' @keywords internal
#'

create_mice_imputation <- function(method) {
  function(X) {
    imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE,
                    visitSequence = "arabic")
    mice::complete(imp_dat, action = "all")[[1]]
  }
}



#' @title Calculates score for a single imputation function
#'
#' @importFrom scoringRules crps_sample
#'
#' @inheritParams Iscores
#'
#' @examples
#' X <- matrix(rnorm(1000), nrow = 100)
#' X[c(runif(700), rep(1, 300)) < 0.3] <- NA
#' imputation_func <- miceDRF:::create_mice_imputation("pmm")
#' Iscore(X, N = 50, imputation_func = imputation_func)
#'
#' @export
#'


Iscore <- function(X, X_imp = NULL, N = 50, imputation_func, max_length = NULL){

  if (is.null(X_imp))
    X_imp <- imputation_func(X)

  X <- as.matrix(X)
  X_imp <- as.matrix(X_imp)

  n <- nrow(X)

  ## Reoder the data according to the number of missing values (least missing first)
  missings_per_col <- colSums(is.na(X))

  ## Miss pattern
  M <- is.na(X)

  dim_with_NA <- missings_per_col[missings_per_col > 0]

  if (is.null(max_length)) max_length <- length(dim_with_NA)

  if (length(dim_with_NA) < max_length){
    warning("max_length is larger than the total number of columns with missing values!")
    max_length <- sum(dim_with_NA)
  }

  scores_dat <- lapply(order(dim_with_NA, decreasing = TRUE), function(j) {

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

    imputation_list <- lapply(1:N, function(ith_imputation) {
      imputation_func(X_artificial)
    })

    Y_matrix <- do.call(cbind, lapply(imputation_list, function(x)  x[1:length(Y_test), 1]))

    score_j <- -mean(scoringRules::crps_sample(y = Y_test, dat = Y_matrix))

    data.frame(column_id = j,
               weight = weight,
               score = score_j)
  }) |>
    do.call(rbind, args = _)

  weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
                          (sum(scores_dat[["weight"]], na.rm = T)), na.rm = T)

  attr(weighted_score, "dat") <- scores_dat
  weighted_score
}
