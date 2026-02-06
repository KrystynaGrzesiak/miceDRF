
#' Internal function for changing factors to numerical
#'
#' A supplementarty function for data management
#'
#' @param factor_col a factor column
#'

factor_to_numeric <- function(factor_col) {
  as.numeric(levels(factor_col))[factor_col]
}

#' One hot encoding
#'
#' A supplementarty function for one-hot encoding
#'
#' @param dat a data containinig some factor but numeric columns.
#'
factor_to_onehot <- function(dat) {

  dat <- data.frame(dat)
  factor_columns <- which(sapply(as.data.frame(dat), is.factor))

  if(length(factor_columns) == 0) return(dat)

  n_levels <- c()

  for(ith_factor in factor_columns){
    n_levels <- c(n_levels, length(levels(dat[, ith_factor])))
    dat <- cbind(dat, do_one_hot(dat[, ith_factor]))
  }

  dat <- dat[, -factor_columns]
  attr(dat, "mask") <- rep(c(0, factor_columns),
                           times = c(ncol(dat) - sum(n_levels),  n_levels))
  attr(dat, "column_names") <- names(factor_columns)

  dat
}

#' One hot encoding
#'
#' A supplementarty function for one-hot encoding
#'
#' @param onehot_dat a data coded with \code{factor_to_onehot} function.
#'
onehot_to_factor <- function(onehot_dat) {

  mask <- attr(onehot_dat, "mask")
  column_names <- attr(onehot_dat, "column_names")

  factor_dat <- onehot_dat
  factor_dat[, mask != 0] <- NULL

  for(ith_var in setdiff(unique(mask), 0)) {

    col_id <- which(setdiff(unique(mask), 0) == ith_var)

    onehot_part <- onehot_dat[, mask == ith_var]

    categories <- colnames(onehot_dat)[mask == ith_var]
    categories <- substr(categories, start = 7, stop = nchar(categories))
    categories <- sub("\\..*", "", categories)

    cat_column <- factor(apply(onehot_part, 1, function(ith_row) {
      category <- categories[which(as.logical(ith_row))]
      ifelse(length(category) == 0, NA, category)
    }), levels = as.numeric(categories))

    if(ith_var > ncol(factor_dat)) {
      factor_dat <- cbind(factor_dat, dummy_col_123_unique = cat_column)
    } else {
      factor_dat <- cbind(
        factor_dat[, 1:(ith_var - 1)],
        dummy_col_123_unique = cat_column,
        factor_dat[, (ith_var + 1):(ncol(factor_dat) + length(unique(mask)) - 1)]
      )
    }

    colnames(factor_dat)[ith_var] <- column_names[col_id]
  }


}


#' One hot encoding
#'
#' A supplementarty function for one-hot encoding
#'
#' @param vec a factor vector to be encoded
#'

do_one_hot <- function(vec) {

  NA_mat <- matrix(NA, nrow = length(vec), ncol = length(levels(vec)))

  if(ncol(NA_mat) == 1) {
    NA_mat[, 1] <- vec
  } else {
    mm <- cbind(model.matrix(~vec), 0)
    mm[, 1] <-  mm[, 1] - rowSums(data.frame(mm[, -1]))
    NA_mat[as.numeric(rownames(mm)), ] <- mm[, - ncol(mm)]
  }
  colnames(NA_mat) <- paste0("level_", sort(levels(vec)))

  NA_mat
}




#' energy-I-Score for imputation of categorical (mixed) data
#'
#' @importFrom scoringRules crps_sample
#' @importFrom pbapply pblapply
#' @inheritParams Iscore
#'
#' @param factor_vars a logical value indicating whether imputation should be
#' performed on factors. If \code{FALSE}, all the variables that are factors
#' will be converted to numeric values.
#'
#' @return a numerical value denoting weighted Imputation Score obtained for
#' provided imputation function and a table with scores and weights calculated
#' for particular columns.
#'
#' @details
#' The categorical variables should be stored as factors. If you need additional
#' conversion of the data (for example one-hot encoding) for imputation, please,
#' implement everything within \code{imputation_func} parameter. You can use
#' \code{miceDRF:::onehot_to_factor} and \code{miceDRF:::factor_to_onehot}
#' functions.
#'
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(500), nrow = 100)
#' X <- cbind(X, factor(sample(1:5, 100, replace = TRUE), levels = 1:5))
#' X[runif(600) < 0.2] <- NA
#' X <- cbind(X, factor(sample(1:2, 100, replace = TRUE), levels = 1:2))
#' X <- as.data.frame(X)
#' X[["V6"]] <- factor(X[["V6"]], levels = 1:5)
#' X[["V7"]] <- factor(X[["V7"]], levels = 1:2)
#' X[["V8"]] <- rnorm(100)
#' imputation_func <- miceDRF:::create_mice_imputation("cart")
#' X_imp <- imputation_func(X)
#'
#' Iscore_cat(X, X_imp, imputation_func, factor_vars = FALSE)
#'
#' @export
#'

Iscore_cat <- function(X, X_imp, imputation_func, factor_vars = TRUE,
                       multiple = TRUE, N = 50, max_length = NULL,
                       skip_if_needed = TRUE){

  N <- ifelse(multiple, N, 1)

  X <- as.data.frame(X, check.names = FALSE)
  X_imp <- as.data.frame(X_imp, check.names = FALSE)
  n <- nrow(X)
  missings_per_col <- colSums(is.na(X))

  ## Missings pattern
  M <- is.na(X)

  dim_with_NA <- missings_per_col > 0

  if (is.null(max_length)) max_length <- sum(dim_with_NA)

  if (sum(dim_with_NA) < max_length){
    warning("max_length is larger than the total number of columns with missing values!")
    max_length <- sum(dim_with_NA)
  }

  factor_columns <- which(sapply(X, is.factor))

  cols_to_iterate <- intersect(order(missings_per_col, decreasing = TRUE),
                               which(dim_with_NA))[1:max_length]

  scores_dat <- pbapply::pblapply(cols_to_iterate, function(j) {

    weight <- (missings_per_col[j] / n) * ((n - missings_per_col[j]) / n)

    if(missings_per_col[j] < 10) {
      warning('Sample size of missing and nonmissing too small for nonparametric distributional regression, setting to NA')
      return(data.frame(column_id = j, weight = weight, score = NA,
                        n_columns_used = NA)) # return score = NA
    }

    observed_j_for_train <- !M[, j]

    # Fully observed columns except j
    Oj <- colSums(is.na(X[observed_j_for_train, ][, -j])) == 0

    if(!any(Oj)) {

      if(skip_if_needed) {

        Oj_candidates <- M[, -j]
        max_obs_Ojs <- colSums(!Oj_candidates[observed_j_for_train, ])
        observed_j_for_train <- !Oj_candidates[, which.max(max_obs_Ojs)] & !M[, j]
        message(paste0("No complete variables for training column ", j,
                       ". Skipping some observations."))

        Oj <- colSums(is.na(X[observed_j_for_train, ][, -j])) == 0

      } else {

        warning("Oj was empty. There was no complete column for training.")
        return(data.frame(column_id = j, weight = weight, score = NA,
                          n_columns_used = sum(Oj))) # return score = NA
      }
    }

    # Only take those that are fully observed H for all observed values of X_j
    X_imp_0 <- X_imp[observed_j_for_train, ]
    X_test <- X_imp_0[, -j][, Oj]
    Y_test <- X_imp_0[, j]

    # Only take those that are fully observed H for all missing values of X_j
    X_imp_1 <- X_imp[!observed_j_for_train, ]
    X_train <- X_imp_1[, -j][, Oj]
    Y_train <- X_imp_1[, j]

    if(sum(Oj) > 2) {
      names(X_test) <- paste0("1234", 1:ncol(X_test))
      names(X_train) <- names(X_test)
    }

    if(j %in% factor_columns) {
      if(factor_vars) {
        X_artificial <- rbind(data.frame(y = as.factor(NA), X = X_test),
                              data.frame(y = Y_train, X = X_train))
      } else {
        X_artificial <- rbind(data.frame(y = NA_real_, X = X_test),
                              data.frame(y = as.numeric(Y_train), X = X_train))
      }
      Y_test <- factor_to_onehot(Y_test)
    } else {
      X_artificial <- rbind(data.frame(y = NA_real_,
                                       X = X_test),
                            data.frame(y = as.numeric(Y_train), X = X_train))
    }

    if(!factor_vars) {
      factor_cols_X_art <- which(sapply(X_artificial, is.factor))
      X_artificial[, factor_cols_X_art] <-
        sapply(X_artificial[, factor_cols_X_art], factor_to_numeric)
    }

    # if(onehot) {
    #   y_encoded <- factor_to_onehot(X_artificial[, 1])
    #   x_encoded <- factor_to_onehot(X_artificial[, -1])
    #   mask <- c(attr(y_encoded, "mask") + 1, attr(x_encoded, "mask"))
    #   X_artificial <- data.frame(y_encoded, x_encoded)
    # }

    imputation_list <- lapply(1:N, function(ith_imputation) {

      imputed <- try({imputation_func(X_artificial)})

      if(inherits(imputed, "try-error") | any(is.na(imputed)))
        return(NA)

      if(j %in% factor_columns) {
        res <- imputed[1:nrow(Y_test), 1]

        if(!factor_vars) {
          res <- as.factor(res)
        }
        res <- factor_to_onehot(res)

      } else {
        res <- imputed[1:length(Y_test), 1]
      }

      res

    })

    if(length(imputation_list[!sapply(imputation_list, function(x) all(is.na(x)))]) < N) {
      warning("Unsuccessful imputation! Imputation function is unstable!
              Returning NA!")
      return(data.frame(column_id = j, weight = weight, score = NA,
                        n_columns_used = sum(Oj))) # return score = NA
    }

    if(j %in% factor_columns) {
      Y_test <- factor_to_onehot(Y_test)
      Y_matrix <- do.call(cbind, imputation_list)

      score_j <- mean(sapply(1:nrow(Y_test), function(ith_obs) {
        scoringRules::es_sample(as.numeric(unlist(Y_test[ith_obs, ])),
                                t(matrix(as.numeric(Y_matrix[ith_obs, ]),
                                         ncol = ncol(Y_test), byrow = TRUE)))
      }))

    } else {
      Y_matrix <- do.call(cbind, imputation_list)
      score_j <- mean(scoringRules::crps_sample(y = Y_test, dat = Y_matrix))
    }

    data.frame(column_id = j,
               weight = weight,
               score = score_j,
               n_columns_used = sum(Oj))

  }) |>
    do.call(rbind, args = _)

  weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
                          (sum(scores_dat[["weight"]], na.rm = TRUE)),
                        na.rm = TRUE)

  weighted_score <- ifelse(all(is.na(scores_dat[["score"]])), NA, weighted_score)

  attr(weighted_score, "dat") <- scores_dat
  weighted_score
}





