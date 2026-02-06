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

- X:

  data containing missing values denoted with NA's

- imputation_list:

  a list of imputation functions

- methods:

  a character vector of names of methods in `imputation_list`. It can be
  `NULL`, then the function will attempt to get names from
  `imputation_list` object.

- N:

  a numeric value. Number of samples from imputation distribution H.
  Default to 50.

- max_length:

  Maximum number of variables \\X_j\\ to consider, can speed up the
  code. Default to `NULL` meaning that all the columns will be taken
  under consideration.

- skip_if_needed:

  logical, indicating whether some observations should be skipped to
  obtain complete columns for scoring. If FALSE, NA will be returned for
  column with no observed variable for training.

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
#>    sample  norm.nob       pmm       DRF      cart 
#> 0.5485229 0.5488967 0.5580149 0.5587692 0.5797292 
```
