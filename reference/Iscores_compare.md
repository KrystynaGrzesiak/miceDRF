# Calculates IScores for multiple imputation functions

Calculates IScores for multiple imputation functions

## Usage

``` r
Iscores_compare(
  X,
  imputation_list,
  methods = NULL,
  N = 50,
  max_length = NULL,
  skip_if_needed = TRUE
)
```

## Arguments

- imputation_list:

  a list of imputation functions

- methods:

  a character vector of names of methods in `imputation_list`. It can be
  `NULL`, then the function will attempt to get names from
  `imputation_list` object.

## Value

a vector of IScores for provided methods

## Examples

``` r
set.seed(111)
X <- matrix(rnorm(1000), nrow = 100)
X[runif(1000) < 0.4] <- NA

methods <- c("pmm", "cart", "sample", "norm.nob", "DRF")
imputation_list <- create_mice_imputations(methods)

Iscores_compare(X, imputation_list)
#> [1] "Calculating score for method: pmm"
#> No complete variables for training column 1. Skipping some observations.
#> No complete variables for training column 3. Skipping some observations.
#> No complete variables for training column 8. Skipping some observations.
#> No complete variables for training column 10. Skipping some observations.
#> No complete variables for training column 5. Skipping some observations.
#> No complete variables for training column 6. Skipping some observations.
#> No complete variables for training column 7. Skipping some observations.
#> No complete variables for training column 4. Skipping some observations.
#> No complete variables for training column 9. Skipping some observations.
#> No complete variables for training column 2. Skipping some observations.
#> [1] "Calculating score for method: cart"
#> No complete variables for training column 1. Skipping some observations.
#> No complete variables for training column 3. Skipping some observations.
#> No complete variables for training column 8. Skipping some observations.
#> No complete variables for training column 10. Skipping some observations.
#> No complete variables for training column 5. Skipping some observations.
#> No complete variables for training column 6. Skipping some observations.
#> No complete variables for training column 7. Skipping some observations.
#> No complete variables for training column 4. Skipping some observations.
#> No complete variables for training column 9. Skipping some observations.
#> No complete variables for training column 2. Skipping some observations.
#> [1] "Calculating score for method: sample"
#> No complete variables for training column 1. Skipping some observations.
#> No complete variables for training column 3. Skipping some observations.
#> No complete variables for training column 8. Skipping some observations.
#> No complete variables for training column 10. Skipping some observations.
#> No complete variables for training column 5. Skipping some observations.
#> No complete variables for training column 6. Skipping some observations.
#> No complete variables for training column 7. Skipping some observations.
#> No complete variables for training column 4. Skipping some observations.
#> No complete variables for training column 9. Skipping some observations.
#> No complete variables for training column 2. Skipping some observations.
#> [1] "Calculating score for method: norm.nob"
#> No complete variables for training column 1. Skipping some observations.
#> No complete variables for training column 3. Skipping some observations.
#> No complete variables for training column 8. Skipping some observations.
#> No complete variables for training column 10. Skipping some observations.
#> No complete variables for training column 5. Skipping some observations.
#> No complete variables for training column 6. Skipping some observations.
#> No complete variables for training column 7. Skipping some observations.
#> No complete variables for training column 4. Skipping some observations.
#> No complete variables for training column 9. Skipping some observations.
#> No complete variables for training column 2. Skipping some observations.
#> [1] "Calculating score for method: DRF"
#> No complete variables for training column 1. Skipping some observations.
#> No complete variables for training column 3. Skipping some observations.
#> No complete variables for training column 8. Skipping some observations.
#> No complete variables for training column 10. Skipping some observations.
#> No complete variables for training column 5. Skipping some observations.
#> No complete variables for training column 6. Skipping some observations.
#> No complete variables for training column 7. Skipping some observations.
#> No complete variables for training column 4. Skipping some observations.
#> No complete variables for training column 9. Skipping some observations.
#> No complete variables for training column 2. Skipping some observations.
#>    sample  norm.nob       pmm      cart       DRF 
#> 0.5485229 0.5488967 0.5583584 0.5797292 0.5963409 
```
