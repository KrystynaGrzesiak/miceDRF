library(testthat)
library(miceDRF)

set.seed(123)

missdf <- matrix(rnorm(20), ncol = 2)
missdf[runif(length(missdf)) < 0.2] <- NA
missdf <- as.data.frame(missdf)

missdf_factor <- data.frame(
  missdf,
  fctr = factor(sample(1:3, nrow(missdf), replace = TRUE))
)

test_that("impute_mice_drf returns a completed data frame", {
  set.seed(123)

  imputed <- impute_mice_drf(missdf, printFlag = FALSE, maxit = 2)

  expect_s3_class(imputed, "data.frame")
  expect_equal(dim(imputed), dim(missdf))
  expect_false(anyNA(imputed))
})

test_that("impute_mice_drf warns and converts factor columns to numeric", {
  set.seed(123)

  expect_warning(
    imputed <- impute_mice_drf(missdf_factor, printFlag = FALSE, maxit = 2),
    "Changing factor to numeric\\."
  )

  expect_s3_class(imputed, "data.frame")
  expect_true(is.numeric(imputed[["fctr"]]))
  expect_false(anyNA(imputed))
})

test_that("impute_mice_drf respects printFlag = TRUE", {
  set.seed(123)

  output <- capture.output(
    impute_mice_drf(missdf, printFlag = TRUE, m = 1, maxit = 2)
  )

  expect_true(length(output) > 0)
  expect_true(any(grepl("iter\\s+imp\\s+variable", output)))
})

test_that("impute_mice_drf returns multiple imputations when m > 1", {
  set.seed(123)

  m <- 2
  imputed <- impute_mice_drf(missdf, m = m, printFlag = FALSE, maxit = 2)

  expect_type(imputed, "list")
  expect_length(imputed, m)
  expect_named(imputed, paste0("imp", seq_len(m)))

  expect_true(all(vapply(imputed, inherits, logical(1), what = "data.frame")))
  expect_true(all(vapply(imputed, function(x) identical(dim(x), dim(missdf)), logical(1))))
  expect_true(all(vapply(imputed, function(x) !anyNA(x), logical(1))))
})
