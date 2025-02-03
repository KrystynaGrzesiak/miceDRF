

library(mice)
library(miceDRF)

library(Iscores)



densityRatioScore(X, X.NA, x = c(NA, 1))




n <- 100
X <- cbind(rnorm(n),rnorm(n))
X.NA <- X
X.NA[,1] <- ifelse(stats::runif(n)<=0.2, NA, X[,1])

imputations <- list()

imputations[[1]] <- lapply(1:5, function(i) {
  X.loc <- X.NA
  X.loc[is.na(X.NA[,1]),1] <- mean(X.NA[,1],na.rm=TRUE)
  return(X.loc)
})

imputations[[2]] <- lapply(1:5, function(i) {
  X.loc <- X.NA
  X.loc[is.na(X.NA[,1]),1] <- sample(X.NA[!is.na(X.NA[,1]),1],
                                     size = sum(is.na(X.NA[,1])), replace = TRUE)
  return(X.loc)
})

methods <- c("mean","sample")

Iscores(imputations,
        methods,
        X.NA,
        num.proj=5
)


X <- matrix(rnorm(1000), nrow = 100)
X[c(runif(700), rep(1, 300)) < 0.3] <- NA
imputation_func <- miceDRF:::create_mice_imputation("pmm")
X_imp <- imputation_func(X)

NA.pat <- X
NA.pat[!is.na(NA.pat)] <- 1

NA.pat.unique <- unique(NA.pat)



NA.pat <- X.NA
NA.pat[!is.na(NA.pat)] <- 1
NA.pat.unique <- unique(NA.pat)


Iscores:::densityRatioScore(X, X_imp, x = NA.pat.unique[1, ])

res <- Iscores(list(X_imp),
        "pemm",
        X,
        num.proj=1
)

############# more drafts

set.seed(111)
X <- matrix(rnorm(1000), nrow = 100)
X_miss <- X
X_miss[runif(1000) < 0.4] <- NA
imputation_func <- miceDRF:::create_mice_imputation("pmm")
X_imp <- imputation_func(X_miss)

miceDRF::Iscore(X_miss, X_imp, N = 50, imputation_func = imputation_func)

X_miss <- X
X_miss[runif(1000) < 0.1] <- NA
imputation_func <- miceDRF:::create_mice_imputation("pmm")
X_imp <- imputation_func(X_miss)

miceDRF::Iscore(X_miss, X_imp, N = 50, imputation_func = imputation_func)


