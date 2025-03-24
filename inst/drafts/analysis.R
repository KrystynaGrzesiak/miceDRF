
library(miceDRF)
library(mice)
library(dplyr)
library(ggplot2)
library(patchwork)

insert_MAR <- function(dat, ratio = 0.3, shift = "right", shift_size = 0.5) {

  # shift_size is a number > 0

  if(shift_size == 0)
    stop("shift is wrong")

  shift <- match.arg(shift, c("right", "left"))

  min_val <- 0.1
  max_val <- 0.5

  min_val <- min_val + shift_size
  max_val <-  max_val + shift_size

  if(shift == "left") {
    min_tmp <- min_val
    min_val <- - max_val
    max_val <- - min_tmp
  }

  n <- nrow(dat)
  p <- ncol(dat)
  total_missing <- round(n * p * ratio, 0)

  if((p - 1) * n <= p * n * ratio) stop("Not enough data to ampute")

  if(p < 2) {
    stop(paste0("The data should contain at least two columns!",
                "Your data contains ", p, "."))
  }

  tmp_missing_per_column <- rmultinom(1, total_missing, rep(1/p, p - 1))[, 1]
  # random_complete_col <- sample(1:p, size = 1)

  random_complete_col <- 1

  missing_per_column <- numeric(p)
  missing_per_column[-random_complete_col] <- tmp_missing_per_column

  ids_0 <- which(missing_per_column == 0)
  ids_non_0 <- which(missing_per_column != 0)

  for(i in ids_non_0) {

    n_cols_to_sample <- sample(1:length(ids_0), size = 1)
    sampled_cols_ind <- sample(ids_0, n_cols_to_sample)

    sampled_cols <- dat[, sampled_cols_ind]
    sampled_scales <- runif(n_cols_to_sample, min = min_val, max = max_val)
    scaled_sum <- as.matrix(sampled_cols) %*% sampled_scales

    probs <- 1/(1 + exp(-scaled_sum))

    ids <- which(as.logical(sapply(probs, function(p) rbinom(1, 1, p))))

    missing_per_column[i] <- min(length(ids), missing_per_column[i])

    dat[sample(ids, missing_per_column[i]), i] <- NA
  }

  dat
}

nrmse <- function(X, X_imp, X_miss) {
  observed <- X[is.na(X_miss)]
  imputed <- X_imp[is.na(X_miss)]

  sqrt(mean((observed - imputed)^2) / var(observed))
}

######################   super simple gaussian example


set.seed(10)

methods <- c("DRF", "cart", "norm.predict", "norm.nob", "sample")

n <- 300
d <- 3

X <- MASS::mvrnorm(n = n,
                   mu = rep(0, d),
                   Sigma = diag(0.3,d,d)+matrix(0.7,d,d))

X_miss <- insert_MAR(X, shift_size = 2)


#### shift vis

X %>%
  as.data.frame() %>%
  mutate(x2_missing = is.na(X_miss[, 2])) %>%
  ggplot(aes(x = V2, fill = x2_missing, col = x2_missing)) +
  geom_density(alpha = 0.5)


#########################################################


res <- lapply(1:10, function(i) {
  print(i)

  X_miss <- mice::ampute(X)$amp

  imp_list <- lapply(methods, function(ith_method) {
    imp_fun <- miceDRF:::create_mice_imputation(ith_method)
    X_imp <- imp_fun(X_miss)
    list(X_imp = X_imp, imp_fun = imp_fun)
  })

  scores <- lapply(imp_list, function(ith) {
    data.frame(iscore_improved1 = Iscore(X = X_miss, X_imp = ith[["X_imp"]], imputation_func = ith[["imp_fun"]], N = 30),
               # iscore = Iscore_old(X = X_miss, X_imp = ith[["X_imp"]], imputation_func = ith[["imp_fun"]], N = 30),
               iscore_improved2 = Iscore_corr(X = X_miss, X_imp = ith[["X_imp"]], imputation_func = ith[["imp_fun"]], N = 30),
               nrmse = nrmse(X = X, X_imp = ith[["X_imp"]], X_miss = X_miss),
               energy = as.vector(energy_dist(X, X_imp = ith[["X_imp"]])))
  }) %>%  bind_rows()


  scores %>%
    mutate(method = methods)

}) %>%
  bind_rows()


res %>%
  rename(iscore_highest_n = "iscore_improved1") %>%
  rename(iscore_corr = "iscore_improved2") %>%
  tidyr::gather("score", "value", -method) %>%
  mutate(value = -value) %>%
  filter(method != "sample") %>%
  ggplot() +
  geom_boxplot(aes(x = score, y = value, fill = score)) +
  facet_grid(~method) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10))


plt_list <- lapply(c("iscore", "iscore_highest_n", "iscore_corr", "energy", "nrmse"), function(i) {
  res %>%
    rename(iscore_highest_n = "iscore_improved1") %>%
    rename(iscore_corr = "iscore_improved2") %>%
    tidyr::gather("score", "value", -method) %>%
    group_by(score, method) %>%
    mutate(run_id = 1:n()) %>%
    ungroup() %>%
    group_by(run_id, score) %>%
    arrange(value) %>%
    mutate(rank = n():1) %>%
    group_by(method, score) %>%
    reframe(mean_rank = mean(rank)) %>%
    filter(score == i) %>%
    arrange(mean_rank) %>%
    mutate(place = n():1) %>%
    ggplot() +
    geom_col(aes(x = reorder(method, mean_rank), y = mean_rank), alpha = 0.8) +
    theme_minimal() +
    geom_label(aes(x = reorder(method, mean_rank), y = mean_rank, label = reorder(method, mean_rank)), size = 3) +
    geom_text(aes(x = method, y = mean_rank, label = place), position = position_stack(vjust = .5), size = 6) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank()) +
    ggtitle(i)
})



patchwork::wrap_plots(plt_list, nrow = 1)



