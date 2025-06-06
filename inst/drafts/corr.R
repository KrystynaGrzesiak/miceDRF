


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

Iscore_corr <- function(X, X_imp, multiple = TRUE, N = 50, imputation_func,
                        max_length = NULL){

  N <- ifelse(multiple, N, 1)

  X <- as.data.frame(X, check.names = FALSE)
  X_imp <- as.data.frame(X_imp, check.names = FALSE)

  n <- nrow(X)

  missings_per_col <- colSums(is.na(X))

  ## Missings pattern
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
      return(data.frame(column_id = j, weight = weight, score = NA)) # return score = NA
    }

    observed_j_for_train <- !M[, j]

    # Fully observed columns except j
    Oj <- colSums(is.na(X[observed_j_for_train, ][, -j])) == 0

    if(!any(Oj)) {

      Oj_candidates <- M[, -j]

      max_obs_Ojs <- colSums(!Oj_candidates[observed_j_for_train, ])

      max_train_size <- sum(observed_j_for_train)

      all_combinations <- unlist(lapply((1:(ncol(M) - 1)), function(i) {
        apply(t(combn((1:ncol(M))[-j], i)), 1, paste0, collapse = ".")
      }))

      cand <- sapply(1:nrow(Oj_candidates[observed_j_for_train, ]), function(ith_obs) {
        paste0(which(!M[observed_j_for_train, ][ith_obs, ]), collapse = ".")
      })

      train_dat <- data.frame(combinations = all_combinations,
                              nrow = unlist(lapply(all_combinations, function(ith) {
                                sum(grepl(ith, cand))})),
                              ncol = lengths(strsplit(all_combinations, ".", fixed = TRUE)))

      train_dat[["skipped"]] <- max_train_size - train_dat[["nrow"]]

      train_dat <- train_dat[rev(order(train_dat[["nrow"]])), ]

      train_dat_single <- train_dat[train_dat[["ncol"]] == 1, ]

      cormat <- cor(X, use = "pairwise.complete.obs")

      train_dat_single[["corr"]] <- sapply(as.numeric(train_dat_single[["combinations"]]),
                                           function(i) {abs(cormat[i, j])})

      train_dat_single[["score"]] <- train_dat_single[["corr"]] * train_dat_single[["nrow"]]
      train_dat_single <- train_dat_single[rev(order(train_dat_single[["score"]])), ]
      cols_to_train <- as.numeric(train_dat_single[["combinations"]])

      observed_j_for_train <- !M[, cols_to_train[which.max(train_dat_single[["score"]])]] & !M[, j]

      message(paste0("No complete variables for training column ", j,
                     ". Skipping some observations."))

    }

    Oj <- colSums(is.na(X[observed_j_for_train, ][, -j])) == 0

    if(!any(Oj)){
      warning("Oj was empty. There was no complete column for training.")
      return(data.frame(column_id = j, weight = weight, score = NA)) # return score = NA
    }

    # Only take those that are fully observed H for all observed values of X_j
    X_imp_0 <- X_imp[observed_j_for_train, ]
    X_test <- X_imp_0[, -j][, Oj]
    Y_test <- X_imp_0[, j]

    # Only take those that are fully observed H for all missing values of X_j
    X_imp_1 <- X_imp[!observed_j_for_train, ]
    X_train <- X_imp_1[, -j][, Oj]
    Y_train <- X_imp_1[, j]

    # Train DRF on imputed data
    X_artificial <- rbind(cbind(y = NA, X_test), cbind(y = Y_train, X_train))

    imputation_list <- lapply(1:N, function(ith_imputation) {

      imputed <- try({imputation_func(X_artificial)})

      if(inherits(imputed, "try-error"))
        return(NA)

      imputed[1:length(Y_test), 1]
    })

    if(length(imputation_list) < N) {
      warning("Unsuccessful imputation! Imputation function is unstable! Returning NA!")
      return(data.frame(column_id = j, weight = weight, score = NA)) # return score = NA
    }

    Y_matrix <- do.call(cbind, imputation_list)
    score_j <- mean(crps_sample(y = Y_test, dat = Y_matrix))

    data.frame(column_id = j,
               weight = weight,
               score = score_j)

  }) |>
    do.call(rbind, args = _)

  weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
                          (sum(scores_dat[["weight"]], na.rm = TRUE)),
                        na.rm = TRUE)

  weighted_score <- ifelse(all(is.na(scores_dat[["score"]])), NA, weighted_score)

  attr(weighted_score, "dat") <- scores_dat
  weighted_score
}
