
#' @title Calculates score for one imputation function
#'
#' @importFrom scoringRules crps_sample
#' @importFrom pbapply pblapply
#'
#' @inheritParams IScore
#'
#' @param n_patterns numerical indicating how many patterns should be sampled
#' to calculate the score for j'th column. \code{n_patterns} can be NULL meaning
#' that all the patterns will be used (it might be slow). Default to 10.
#'
#' @return a numerical value denoting weighted Imputation Score obtained for
#' provided imputation function and a table with scores and weights calculated
#' for particular columns.
#'
#' @examples
#' set.seed(111)
#' X <- matrix(rnorm(8000), nrow = 1000)
#' X[runif(8000) < 0.4] <- NA
#' X_imp <- impute_mice_drf(X)
#'
#' miceDRF::Iscore_beta_v2(X, X_imp, N = 10, imputation_func = impute_mice_drf)
#'
#' @export
#'


Iscore_beta_v2 <- function(X, X_imp, multiple = TRUE, N = 50, imputation_func,
                        max_length = NULL, skip_if_needed = TRUE, n_patterns = 10,
                        scale = FALSE){

  N <- ifelse(multiple, N, 1)

  X <- as.data.frame(X, check.names = FALSE)
  ##Delete
  X_imp <- as.data.frame(X_imp, check.names = FALSE)
  ###This is not a 100% correct, X_imp should have been done only for the training set!

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

    observed_j_for_train <- which(!M[, j])

    X_0 <- X[observed_j_for_train, ] #X[M_j==1,]
    # Only take those that are fully observed H for all missing values of X_j


    ## Randomly choose the test set \mathcal{T} from X_0 (M_j=1)
    testsample0<-sample(1:nrow(X_0), size=round(1*nrow(X_0)/4))
    testsample<-observed_j_for_train[testsample0]

    ### Important: X_0[testsample0,]==X[testsample,] ######

    ## Take the imputation for the training set \mathcal{T}^c
    X_0train<-X_imp[-testsample,] ## X_imp now!
    X_0test<-X_0[testsample0,]

    #trainsample1<-sample(1:nrow(X_1), size=round(3*nrow(X_1)/4))
    #X_1train<-X_1[trainsample1,]
    #X_1test<-X_1[-trainsample1,]

    #Xtrain<-rbind(X_0train,X_1train)

    # Impute Training data, but only take M_j=0
    #(ideally imputation function should not change the order of the data)
    #X_imp<-imputation_func(Xtrain)[1:nrow(X_0train),]


    X_test <- X_0test[, -j]
    Y_test <- X_0test[, j]

    #X_train <- X_1[, -j]
    #Y_train <- X_imp[!observed_j_for_train, ][, j]

    X_train <- X_0train[, -j]
    Y_train <- X_0train[, j]

    M_test <- unique(M[testsample, -j])

    if(!is.null(n_patterns)) {
      n_patterns <- min(nrow(M_test), n_patterns)
      M_test <- M_test[sample(1:nrow(M_test), replace = FALSE, size = n_patterns), ]
    }

    score_j <- mean((apply(M_test, 1, function(m){

      ##Important: For the final version we want an expectation, that is, either random sampling
      ## or weighting according to the number of times pattern m appears in M_test.

      # Train DRF on imputed data
      X_artificial <- rbind(cbind(y = NA, X_test[, !m]), cbind(y = Y_train, X_train[, !m]))

      imputation_list <- lapply(1:N, function(ith_imputation) {

        imputed <- try({imputation_func(X_artificial)})

        if(inherits(imputed, "try-error") | any(is.na(imputed)))
          return(NA)

        imputed[1:length(Y_test), 1]
      })

      if(length(imputation_list) < N) {
        warning("Unsuccessful imputation! Imputation function is unstable! Returning NA!")
        return(data.frame(column_id = j, weight = weight, score = NA,
                          n_columns_used = ncol(X))) # return score = NA
      }

      Y_matrix <- do.call(cbind, imputation_list)

      if(scale) {
        Y_test <- (Y_test - mean(Y_test)) / sd(Y_test)
        Y_matrix <- (Y_matrix - mean(Y_test)) / sd(Y_test)
      }

      mean(scoringRules::crps_sample(y = Y_test, dat = Y_matrix))



      } )))

    data.frame(column_id = j,
               weight = weight,
               score = score_j,
               n_columns_used = ncol(X))

  }) |>
    do.call(rbind, args = _)

  weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
                          (sum(scores_dat[["weight"]], na.rm = TRUE)),
                        na.rm = TRUE)

  weighted_score <- ifelse(all(is.na(scores_dat[["score"]])), NA, weighted_score)

  attr(weighted_score, "dat") <- scores_dat
  weighted_score
}
