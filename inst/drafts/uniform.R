
library(miceDRF)
library(mice)
library(dplyr)
library(ggplot2)
library(patchwork)

set.seed(100)

nrmse <- function(X, X_imp, X_miss) {
  observed <- X[is.na(X_miss)]
  imputed <- X_imp[is.na(X_miss)]

  sqrt(mean((observed - imputed)^2) / var(observed))
}

n = 500
d <- 5

# independent uniform
X<-matrix(runif(n=d*n), nrow=n, ncol=d)
# uniform with Gaussian copula
#X <- gaussian_copula_uniform_sim(n = n, d = d)$uniform_data

vectors <- matrix(c(
  rep(0, d),
  0, 1, rep(0,d-2),
  1, rep(0,d-1)
), nrow = 3, byrow = TRUE)

# Generate random draws
# sample() will generate indices, which we use to select rows from the matrix
M <- vectors[apply(X,1, function(x) sample(1:3, size = 1, prob=c(x[1]/3, 2/3-x[1]/3, 1/3), replace = TRUE)), ]

patterns <- apply(X,1, function(x) sample(1:3, size = 1, prob=c(x[1]/3, 2/3-x[1]/3, 1/3), replace = TRUE))


methods <- c("DRF", "cart", "norm.predict", "norm.nob", "sample")


U<-runif(n=1000)
imp<-sqrt(2*U)
hist(imp)

obs_pattern <- apply(X.NA, 1, function(ith_row) all(!is.na(ith_row)))

X %>%  data.frame() %>%
  mutate(obs_pattern = patterns == 1) %>%
  filter(obs_pattern) %>%
  ggplot() +
  geom_histogram(aes(x = X1))



res <- lapply(1:10, function(i) {
  print(i)

  # Generate random draws
  # sample() will generate indices, which we use to select rows from the matrix
  M <- vectors[apply(X,1, function(x) sample(1:3, size = 1, prob=c(x[1]/3, 2/3-x[1]/3, 1/3), replace = TRUE)), ]

  X_miss <- X
  X_miss[M==1]<-NA

  imp_list <- lapply(methods, function(ith_method) {
    imp_fun <- miceDRF:::create_mice_imputation(ith_method)
    X_imp <- imp_fun(X_miss)
    list(X_imp = X_imp, imp_fun = imp_fun)
  })

  runif_imp <- function(X_miss){
    X_miss[is.na(X_miss)] <- runif(sum(is.na(X_miss)))
    X_miss
  }

  imp_list[[6]] <- list(X_imp = runif_imp(X_miss), imp_fun = runif_imp)


  scores <- lapply(imp_list, function(ith) {
    data.frame(iscore_improved1 = Iscore(X = X_miss, X_imp = ith[["X_imp"]], imputation_func = ith[["imp_fun"]], N = 30),
               iscore = Iscore_old(X = X_miss, X_imp = ith[["X_imp"]], imputation_func = ith[["imp_fun"]], N = 30),
               iscore_improved2 = Iscore_corr(X = X_miss, X_imp = ith[["X_imp"]], imputation_func = ith[["imp_fun"]], N = 30),
               nrmse = nrmse(X = X, X_imp = ith[["X_imp"]], X_miss = X_miss),
               energy = as.vector(energy_dist(X, X_imp = ith[["X_imp"]])))
  }) %>%  bind_rows()


  scores %>%
    mutate(method = c(methods, "runif"))

}) %>%
  bind_rows()



res %>%
  rename(iscore_highest_n = "iscore_improved1") %>%
  rename(iscore_corr = "iscore_improved2") %>%
  tidyr::gather("score", "value", -method) %>%
  mutate(value = -value) %>%
  ggplot() +
  geom_boxplot(aes(x = score, y = value, fill = score)) +
  facet_grid(~method) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10))
