#' @title Calculates score for a single imputation function
#'
#' @importFrom scoringRules crps_sample
#'
#' @param X data containing missing values denoted with NA's
#' @param X_imp imputed dataset.
#' @param imputation_func a function that imputes data
#' @param N a numeric value. Number of samples from imputation distribution H.
#' Default to 50.
#' @param max_length Maximum number of variables \eqn{X_j} to consider, can
#' speed up the code. Default to \code{NULL} meaning that all the columns will
#' be taken under consideration.
#' @param multiple a logical indicating whether provided imputation method is a
#' multiple imputation approach (i.e. it generates different values to impute
#' for each call). Default to TRUE. Note that if multiple equals to FALSE, N is
#' automatically set to 1.
#' @param skip_if_needed logical, indicating whether some observations should be
#' skipped to obtain complete columns for scoring. If FALSE, NA will be returned
#' for column with no observed variable for training.
#'
#' @return a numerical value denoting weighted Imputation Score obtained for
#' provided imputation function and a table with scores and weights calculated
#' for particular columns.
#'
#' @examples
#' set.seed(111)
#' X <- matrix(rnorm(1000), nrow = 100)
#' X[runif(1000) < 0.4] <- NA
#' imputation_func <- miceDRF:::create_mice_imputation("pmm")
#' X_imp <- imputation_func(X)
#'
#' miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func)
#'
#' imputation_func <- miceDRF:::create_mice_imputation("mean")
#' X_imp <- imputation_func(X)
#'
#' miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func)
#'
#' miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func, multiple = FALSE)
#'
#' # zero imputation
#' X <- matrix(rnorm(1000), nrow = 100)
#' X[c(runif(1000) < 0.3)] <- NA
#' imputation_func <- function(X) {X[is.na(X)] <- 0; X}
#' X_imp <- imputation_func(X)
#'
#' Iscore(X, X_imp, N = 50, imputation_func = imputation_func, multiple = FALSE)
#'
#' @export
#'


Iscore_fast <- function(X, X_imp, multiple = TRUE, N = 50, imputation_func,
                        max_length = NULL, skip_if_needed = TRUE) {

  N <- ifelse(multiple, N, 1)

  X <- as.data.frame(X, check.names = FALSE)
  X_imp <- as.data.frame(X_imp, check.names = FALSE)

  n <- nrow(X)
  missings_per_col <- colSums(is.na(X))

  M <- is.na(X)
  dim_with_NA <- which(missings_per_col > 0)

  if (is.null(max_length)) max_length <- length(dim_with_NA)

  if (length(dim_with_NA) < max_length) {
    warning("max_length is larger than the total number of columns with missing values!")
    max_length <- length(dim_with_NA)
  }

  scores_list <- future_lapply(order(missings_per_col[dim_with_NA], decreasing = TRUE), function(idx) {
    j <- dim_with_NA[idx]
    weight <- (missings_per_col[j] / n) * ((n - missings_per_col[j]) / n)

    if (missings_per_col[j] < 10) {
      warning("Sample size of missing and nonmissing too small, setting to NA")
      return(data.frame(column_id = j, weight = weight, score = NA))
    }

    observed_j_for_train <- !M[, j]
    Oj <- colSums(!M[observed_j_for_train, -j, drop = FALSE]) == sum(observed_j_for_train)

    if (!any(Oj)) {
      if (skip_if_needed) {
        Oj_candidates <- !M[, -j, drop = FALSE]
        max_obs_Ojs <- colSums(Oj_candidates[observed_j_for_train, , drop = FALSE])
        best_Oj_idx <- which.max(max_obs_Ojs)
        observed_j_for_train <- observed_j_for_train & Oj_candidates[, best_Oj_idx]
        message(sprintf("No complete variables for training column %d. Skipping some observations.", j))
        Oj <- colSums(!M[observed_j_for_train, -j, drop = FALSE]) == sum(observed_j_for_train)
      } else {
        warning(sprintf("No complete columns for training column %d.", j))
        return(data.frame(column_id = j, weight = weight, score = NA))
      }
    }

    Oj_idx <- which(Oj)
    X_imp_0 <- X_imp[observed_j_for_train, ]
    X_test <- X_imp_0[, -j, drop = FALSE][, Oj_idx, drop = FALSE]
    Y_test <- X_imp_0[, j]

    X_imp_1 <- X_imp[!observed_j_for_train, ]
    X_train <- X_imp_1[, -j, drop = FALSE][, Oj_idx, drop = FALSE]
    Y_train <- X_imp_1[, j]

    X_artificial <- rbind(cbind(y = NA, X_test), cbind(y = Y_train, X_train))

    imputation_list <- future_lapply(1:N, function(ith) {
      imputed <- tryCatch(imputation_func(X_artificial), error = function(e) NA)
      if (is.null(imputed) || any(is.na(imputed))) return(NA)
      imputed[1:length(Y_test), 1]
    })

    if (any(sapply(imputation_list, is.na))) {
      warning("Unsuccessful imputation. Returning NA.")
      return(data.frame(column_id = j, weight = weight, score = NA))
    }

    Y_matrix <- do.call(cbind, imputation_list)
    score_j <- mean(crps_sample(y = Y_test, dat = Y_matrix), na.rm = TRUE)

    data.frame(column_id = j, weight = weight, score = score_j, n_columns_used = length(Oj_idx))
  }, future.seed = TRUE)

  scores_dat <- do.call(rbind, scores_list)

  weighted_score <- sum(scores_dat$score * scores_dat$weight, na.rm = TRUE) /
    sum(scores_dat$weight, na.rm = TRUE)

  if (all(is.na(scores_dat$score))) weighted_score <- NA

  attr(weighted_score, "dat") <- scores_dat
  weighted_score
}
