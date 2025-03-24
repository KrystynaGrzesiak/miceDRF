set.seed(111)
X <- matrix(rnorm(1000), nrow = 100)
X[runif(1000) < 0.4] <- NA
imputation_func <- miceDRF:::create_mice_imputation("pmm")
X_imp <- imputation_func(X)

miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func)

imputation_func <- miceDRF:::create_mice_imputation("mean")
X_imp <- imputation_func(X)

miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func)

miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func, multiple = FALSE)

# zero imputation
X <- matrix(rnorm(1000), nrow = 100)
X[c(runif(1000) < 0.3)] <- NA
imputation_func <- function(X) {X[is.na(X)] <- 0; X}
X_imp <- imputation_func(X)

Iscore(X, X_imp, N = 50, imputation_func = imputation_func, multiple = FALSE)


X <- airquality
X_imp <- imputation_func(X)
Iscore(X, X_imp, N = 50, imputation_func = imputation_func, multiple = FALSE)


set.seed(111)

get_data <- function() {
  d <- 5
  n <- 500
  X <- matrix(runif(n = d * n), nrow = n, ncol = d)
  vectors <- matrix(c(
    rep(0, d),
    0, 1, rep(0, d - 2),
    1, rep(0, d - 1)
  ), nrow = 3, byrow = TRUE)

  M <- vectors[apply(X,1, function(x) sample(1:3, size = 1, prob=c(x[1]/3, 2/3-x[1]/3, 1/3), replace = TRUE)), ]

  X_miss <- X
  X_miss[M == 1] <- NA

  X_miss <- as.data.frame(X_miss)
}


X_miss <- get_data()


X_imp <- mice::complete(mice::mice(X_miss, m = 1, method = "cart"))

imputation_func <- function(X_miss) mice::complete(mice::mice(X_miss, m = 1, method = "cart",
                                                              maxit = 5))

Iscore(X_miss, X_imp, N = 50, imputation_func = imputation_func, multiple = TRUE)





