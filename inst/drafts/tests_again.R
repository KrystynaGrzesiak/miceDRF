
impute_missmda_famd_em <- function (missdf, ...) {
  imputed <- missMDA::imputeFAMD(X = missdf, method = "EM")
  data.frame(imputed[["completeObs"]])
}



missdf <- airquality
imputed <- impute_missmda_famd_em(airquality)

library(miceDRF)

# Iscore(airquality, imputed, imputation_func = impute_missmda_famd_em)

X = airquality
X_imp = imputed
multiple = TRUE
N = 50
imputation_func = impute_missmda_famd_em

max_length = NULL

# imputation_func = impute_mice_drf
# imputed <- impute_mice_drf(airquality)



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
    return(data.frame(column_id = j, weight = weight, score = NA,
                      n_columns_used = sum(Oj))) # return score = NA
  }

  Y_matrix <- do.call(cbind, imputation_list)
  score_j <- mean(crps_sample(y = Y_test, dat = Y_matrix))


  data.frame(column_id = j,
             weight = weight,
             score = score_j,
             n_columns_used = sum(Oj))

}) |>
  do.call(rbind, args = _)

weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
                        (sum(scores_dat[["weight"]][!is.na(scores_dat[["score"]])], na.rm = TRUE)),
                      na.rm = TRUE)

weighted_score <- ifelse(all(is.na(scores_dat[["score"]])), NA, weighted_score)

attr(weighted_score, "dat") <- scores_dat
weighted_score








