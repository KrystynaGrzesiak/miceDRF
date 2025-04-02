
#' A function creating list of mice imputation functions
#'
#' @importFrom mice mice
#' @importFrom mice complete
#'
#' @param methods a character vector of names of mice methods
#'
#' @examples
#' methods <- c("pmm", "cart", "sample", "norm.nob", "DRF")
#' reate_mice_imputations(methods)
#'
#' @export
#'
create_mice_imputations <- function(methods) {
  imp_list <- lapply(methods, function(method) {
    miceDRF:::create_mice_imputation(method)
  })
  names(imp_list) <- methods
  imp_list
}



#' @title Calculates IScores for multiple imputation functions
#'
#' @inheritParams IScore
#'
#' @param imputation_list a list of imputation functions
#' @param methods a character vector of names of methods in
#' \code{imputation_list}. It can be \code{NULL}, then the function will attempt
#' to get names from \code{imputation_list} object.
#'
#' @return a vector of IScores for provided methods
#'
#' @examples
#' set.seed(111)
#' X <- matrix(rnorm(1000), nrow = 100)
#' X[runif(1000) < 0.4] <- NA
#'
#' methods <- c("pmm", "cart", "sample", "norm.nob", "DRF")
#' imputation_list <- create_mice_imputations(methods)
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

Iscores_compare <- function(X, imputation_list, methods = NULL, N = 50,
                            max_length = NULL, skip_if_needed = TRUE) {
  if(is.null(methods)) {
    methods <- names(methods)
  } else {
    if(length(methods) != length(imputation_list))
      stop("Provided methods doesn't match imputation_list!")
  }

  iscores <- sapply(seq_along(methods), function(ith_method) {

    print(paste0("Calculating score for method: ", methods[ith_method]))

    X_imp <- imputation_list[[ith_method]](X)
    as.numeric(Iscore(X, X_imp, N = N, max_length = max_length,
                      imputation_func = imputation_list[[ith_method]],
                      skip_if_needed = skip_if_needed))
  })

  names(iscores) <- methods
  sort(iscores)
}





