library(testthat)
library(mice)
library(miceDRF)

test_that("mice.impute.DRF imputes missing values through mice", {
  set.seed(123)

  missdf <- matrix(rnorm(40), ncol = 2)
  missdf[runif(length(missdf)) < 0.2] <- NA
  missdf <- as.data.frame(missdf)

  imp <- mice(
    missdf,
    method = "DRF",
    m = 1,
    maxit = 1,
    printFlag = FALSE
  )

  completed <- complete(imp)

  expect_s3_class(completed, "data.frame")
  expect_equal(dim(completed), dim(missdf))
  expect_false(anyNA(completed))
})

test_that("mice.impute.DRF returns one imputed value per missing entry", {
  set.seed(123)

  y <- c(1, 2, NA, 4, NA, 6)
  ry <- !is.na(y)
  x <- matrix(seq_along(y), ncol = 1)
  wy <- is.na(y)

  imputed <- mice.impute.DRF(
    y = y,
    ry = ry,
    x = x,
    wy = wy,
    num.trees = 5,
    num.features = 1
  )

  expect_type(imputed, "double")
  expect_length(imputed, sum(wy))
  expect_false(anyNA(imputed))
})

test_that("mice.impute.DRF uses !ry when wy is NULL", {
  set.seed(123)

  y <- c(1, 2, NA, 4, NA, 6)
  ry <- !is.na(y)
  x <- matrix(seq_along(y), ncol = 1)

  imputed <- mice.impute.DRF(
    y = y,
    ry = ry,
    x = x,
    wy = NULL,
    num.trees = 5,
    num.features = 1
  )

  expect_length(imputed, sum(!ry))
  expect_false(anyNA(imputed))
})


test_that("mice.impute.DRF works with multiple imputations", {
  set.seed(123)

  missdf <- matrix(rnorm(40), ncol = 2)
  missdf[runif(length(missdf)) < 0.2] <- NA
  missdf <- as.data.frame(missdf)

  imp <- mice(
    missdf,
    method = "DRF",
    m = 2,
    maxit = 1,
    printFlag = FALSE
  )

  completed_1 <- complete(imp, 1)
  completed_2 <- complete(imp, 2)

  expect_s3_class(completed_1, "data.frame")
  expect_s3_class(completed_2, "data.frame")
  expect_equal(dim(completed_1), dim(missdf))
  expect_equal(dim(completed_2), dim(missdf))
  expect_false(anyNA(completed_1))
  expect_false(anyNA(completed_2))
})



