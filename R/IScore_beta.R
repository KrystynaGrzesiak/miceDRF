
#' @title Calculates score for one imputation function
#'
#' @importFrom scoringRules crps_sample
#' @importFrom pbapply pblapply
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
#' miceDRF::Iscore_beta(X, X_imp, N = 50, imputation_func = imputation_func)
#'
#' @export
#'


Iscore_beta <- function(X, X_imp, multiple = TRUE, N = 50, imputation_func,
                        max_length = NULL, skip_if_needed = TRUE){
  
  N <- ifelse(multiple, N, 1)
  
  X <- as.data.frame(X, check.names = FALSE)
  ##Delete
  X_imp <- as.data.frame(X_imp, check.names = FALSE)
  #### X_imp is not actually used!!
  
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
    
    X_0 <- X[observed_j_for_train, ]
    # Only take those that are fully observed H for all missing values of X_j
    #X_1 <- X[!observed_j_for_train, ]
    
    
    ## Divide into training and test set
    trainsample0<-sample(1:nrow(X_0), size=round(3*nrow(X_0)/4))
    X_0train<-X_0[trainsample0,]
    X_0test<-X_0[-trainsample0,]
    
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
    
    X_train<-X_0train[, -j]
    Y_train<-X_0train[,j]
    
    
    
    
    
    # Train DRF on imputed data
    X_artificial <- rbind(cbind(y = NA, X_test), cbind(y = Y_train, X_train))
    
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
    score_j <- mean(crps_sample(y = Y_test, dat = Y_matrix))
    
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


# Iscore_beta <- function(X, X_imp, multiple = TRUE, N = 50, imputation_func,
#                          max_length = NULL, skip_if_needed = TRUE){
#   
#   N <- ifelse(multiple, N, 1)
#   
#   X <- as.data.frame(X, check.names = FALSE)
#   ##Delete
#   X_imp <- as.data.frame(X_imp, check.names = FALSE)
#   #### X_imp is not actually used!!
#   
#   n <- nrow(X)
#   
#   missings_per_col <- colSums(is.na(X))
#   
#   ## Missings pattern
#   M <- is.na(X)
#   
#   dim_with_NA <- missings_per_col > 0
#   
#   if (is.null(max_length)) max_length <- sum(dim_with_NA)
#   
#   if (sum(dim_with_NA) < max_length){
#     warning("max_length is larger than the total number of columns with missing values!")
#     max_length <- sum(dim_with_NA)
#   }
#   
#   cols_to_iterate <- intersect(order(missings_per_col, decreasing = TRUE),
#                                which(dim_with_NA))[1:max_length]
#   
#   
#   scores_dat <- pbapply::pblapply(cols_to_iterate, function(j) {
#     
#     weight <- (missings_per_col[j] / n) * ((n - missings_per_col[j]) / n)
#     
#     if(missings_per_col[j] < 10) {
#       warning('Sample size of missing and nonmissing too small for nonparametric distributional regression, setting to NA')
#       return(data.frame(column_id = j, weight = weight, score = NA,
#                         n_columns_used = NA)) # return score = NA
#     }
#     
#     observed_j_for_train <- !M[, j]
#     
#     X_0 <- X[observed_j_for_train, ]
#     # Only take those that are fully observed H for all missing values of X_j
#     X_1 <- X[!observed_j_for_train, ]
#     
#     
#     ## Divide into training and test set
#     trainsample0<-sample(1:nrow(X_0), size=round(3*nrow(X_0)/4))
#     X_0train<-X_0[trainsample0,]
#     X_0test<-X_0[-trainsample0,]
#     
#     trainsample1<-sample(1:nrow(X_1), size=round(3*nrow(X_1)/4))
#     X_1train<-X_1[trainsample1,]
#     X_1test<-X_1[-trainsample1,]
#     
#     Xtrain<-rbind(X_0train,X_1train)
#     
#     # Impute Training data, but only take M_j=0
#     #(ideally imputation function should not change the order of the data)
#     X_imp<-imputation_func(Xtrain)[1:nrow(X_0train),]
#     
#     
#     X_test <- X_0test[, -j]
#     Y_test <- X_0test[, j]
#     
#     #X_train <- X_1[, -j]
#     #Y_train <- X_imp[!observed_j_for_train, ][, j]
#     
#     X_train<-X_imp[, -j]
#     Y_train<-X_imp[,j]
#     
#     
#     
#     
#     
#     # Train DRF on imputed data
#     X_artificial <- rbind(cbind(y = NA, X_test), cbind(y = Y_train, X_train))
#     
#     imputation_list <- lapply(1:N, function(ith_imputation) {
#       
#       imputed <- try({imputation_func(X_artificial)})
#       
#       if(inherits(imputed, "try-error") | any(is.na(imputed)))
#         return(NA)
#       
#       imputed[1:length(Y_test), 1]
#     })
#     
#     if(length(imputation_list) < N) {
#       warning("Unsuccessful imputation! Imputation function is unstable! Returning NA!")
#       return(data.frame(column_id = j, weight = weight, score = NA,
#                         n_columns_used = ncol(X))) # return score = NA
#     }
#     
#     Y_matrix <- do.call(cbind, imputation_list)
#     score_j <- mean(crps_sample(y = Y_test, dat = Y_matrix))
#     
#     data.frame(column_id = j,
#                weight = weight,
#                score = score_j,
#                n_columns_used = ncol(X))
#     
#   }) |>
#     do.call(rbind, args = _)
#   
#   weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
#                           (sum(scores_dat[["weight"]], na.rm = TRUE)),
#                         na.rm = TRUE)
#   
#   weighted_score <- ifelse(all(is.na(scores_dat[["score"]])), NA, weighted_score)
#   
#   attr(weighted_score, "dat") <- scores_dat
#   weighted_score
# }






# Iscore_beta <- function(X, X_imp, multiple = TRUE, N = 50, imputation_func,
#                         max_length = NULL, skip_if_needed = TRUE){
#   
#   N <- ifelse(multiple, N, 1)
#   
#   X <- as.data.frame(X, check.names = FALSE)
#   X_imp <- as.data.frame(X_imp, check.names = FALSE)
#   
#   n <- nrow(X)
#   
#   missings_per_col <- colSums(is.na(X))
#   
#   ## Missings pattern
#   M <- is.na(X)
#   
#   dim_with_NA <- missings_per_col > 0
#   
#   if (is.null(max_length)) max_length <- sum(dim_with_NA)
#   
#   if (sum(dim_with_NA) < max_length){
#     warning("max_length is larger than the total number of columns with missing values!")
#     max_length <- sum(dim_with_NA)
#   }
#   
#   cols_to_iterate <- intersect(order(missings_per_col, decreasing = TRUE),
#                                which(dim_with_NA))[1:max_length]
#   
#   
#   scores_dat <- pbapply::pblapply(cols_to_iterate, function(j) {
#     
#     weight <- (missings_per_col[j] / n) * ((n - missings_per_col[j]) / n)
#     
#     if(missings_per_col[j] < 10) {
#       warning('Sample size of missing and nonmissing too small for nonparametric distributional regression, setting to NA')
#       return(data.frame(column_id = j, weight = weight, score = NA,
#                         n_columns_used = NA)) # return score = NA
#     }
#     
#     observed_j_for_train <- !M[, j]
#     
#     # # Fully observed columns except j
#     # Oj <- colSums(is.na(X[observed_j_for_train, ][, -j])) == 0
#     #
#     # if(!any(Oj)) {
#     #
#     #   if(skip_if_needed) {
#     #
#     #     Oj_candidates <- M[, -j]
#     #     max_obs_Ojs <- colSums(!Oj_candidates[observed_j_for_train, ])
#     #     observed_j_for_train <- !Oj_candidates[, which.max(max_obs_Ojs)] & !M[, j]
#     #     message(paste0("No complete variables for training column ", j,
#     #                    ". Skipping some observations."))
#     #
#     #     Oj <- colSums(is.na(X[observed_j_for_train, ][, -j])) == 0
#     #
#     #   } else {
#     #
#     #     warning("Oj was empty. There was no complete column for training.")
#     #     return(data.frame(column_id = j, weight = weight, score = NA,
#     #                       n_columns_used = sum(Oj))) # return score = NA
#     #   }
#     # }
#     #
#     # Only take those that are fully observed H for all observed values of X_j
#     X_0 <- X[observed_j_for_train, ]
#     X_test <- X_0[, -j]
#     Y_test <- X_0[, j]
#     
#     # Only take those that are fully observed H for all missing values of X_j
#     X_1 <- X[!observed_j_for_train, ]
#     #X_imp_1 <- X[!observed_j_for_train, ]
#     X_train <- X_1[, -j]
#     Y_train <- X_imp[!observed_j_for_train, ][, j]
#     
#     # Train DRF on imputed data
#     X_artificial <- rbind(cbind(y = NA, X_test), cbind(y = Y_train, X_train))
#     
#     imputation_list <- lapply(1:N, function(ith_imputation) {
#       
#       imputed <- try({imputation_func(X_artificial)})
#       
#       if(inherits(imputed, "try-error") | any(is.na(imputed)))
#         return(NA)
#       
#       imputed[1:length(Y_test), 1]
#     })
#     
#     if(length(imputation_list) < N) {
#       warning("Unsuccessful imputation! Imputation function is unstable! Returning NA!")
#       return(data.frame(column_id = j, weight = weight, score = NA,
#                         n_columns_used = ncol(X))) # return score = NA
#     }
#     
#     Y_matrix <- do.call(cbind, imputation_list)
#     score_j <- mean(crps_sample(y = Y_test, dat = Y_matrix))
#     
#     data.frame(column_id = j,
#                weight = weight,
#                score = score_j,
#                n_columns_used = ncol(X))
#     
#   }) |>
#     do.call(rbind, args = _)
#   
#   weighted_score <- sum(scores_dat[["score"]] * scores_dat[["weight"]] /
#                           (sum(scores_dat[["weight"]], na.rm = TRUE)),
#                         na.rm = TRUE)
#   
#   weighted_score <- ifelse(all(is.na(scores_dat[["score"]])), NA, weighted_score)
#   
#   attr(weighted_score, "dat") <- scores_dat
#   weighted_score
# }












