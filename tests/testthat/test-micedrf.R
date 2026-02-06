
library(testthat)

set.seed(123)
missdf <- rnorm(20)
missdf[runif(20) < 0.2] <- NA
missdf <- matrix(missdf, ncol = 2)

missdf_factor <- missdf
missdf_factor <- data.frame(missdf_factor,
                            fctr = as.factor(sample(1:3, 10, replace = TRUE)))


test_that("impute_mice_drf imputes correctly", {

  imputed <- impute_mice_drf(missdf, printFlag = TRUE)

  out <- structure(
    list(V1 = c(0.070508391424576, -0.23017748948328, 1.55870831414912,
                0.070508391424576, -0.686852851893526, -0.23017748948328,
                0.460916205989202, -1.26506123460653, -0.686852851893526,
                -0.445661970099958),
         V2 = c(1.78691313680308, 0.359813827057364, 0.400771450594052,
                -0.555841134754075, -0.555841134754075, 1.78691313680308,
                -0.472791407727934, -1.96661715662964, 0.701355901563686,
                -0.472791407727934)), row.names = c(NA, -10L),
    class = "data.frame")

  expect_identical(round(imputed, 10), round(out, 10))

})


test_that("impute_mice_drf warns and converts factor columns to numeric", {

  expect_warning(imputed <- impute_mice_drf(missdf_factor),
                 "Changing factor to numeric\\.")

  expect_true(is.numeric(imputed[["fctr"]]))

})


test_that("impute_mice_drf prints progress when printFlag = TRUE", {

  output <- capture.output(
    imputed <- impute_mice_drf(missdf, printFlag = TRUE, m = 1, maxit = 5)
  )

  # Check that something was printed
  expect_true(length(output) > 0)

  # Check for characteristic mice progress header
  expect_true(any(grepl("iter\\s+imp\\s+variable", output)))
})


test_that("impute_mice_drf imputed multiple times for m > 1", {

  m <- 2

  imputed <- impute_mice_drf(missdf, m = m, maxit = 5)

  expect_true(length(imputed) == 2)

  expect_true(all(names(imputed) == paste0("imp", 1:m)))

  expect_true(all(sapply(imputed, dim)[ , 1] == dim(missdf)))
})

