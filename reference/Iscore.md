# Calculates score for one imputation function

Calculates score for one imputation function

## Usage

``` r
Iscore(
  X,
  X_imp,
  multiple = TRUE,
  N = 50,
  imputation_func,
  max_length = NULL,
  skip_if_needed = TRUE,
  scale = FALSE
)
```

## Arguments

- X:

  data containing missing values denoted with NA's

- X_imp:

  imputed dataset.

- multiple:

  a logical indicating whether provided imputation method is a multiple
  imputation approach (i.e. it generates different values to impute for
  each call). Default to TRUE. Note that if multiple equals to FALSE, N
  is automatically set to 1.

- N:

  a numeric value. Number of samples from imputation distribution H.
  Default to 50.

- imputation_func:

  a function that imputes data

- max_length:

  Maximum number of variables \\X_j\\ to consider, can speed up the
  code. Default to `NULL` meaning that all the columns will be taken
  under consideration.

- skip_if_needed:

  logical, indicating whether some observations should be skipped to
  obtain complete columns for scoring. If FALSE, NA will be returned for
  column with no observed variable for training.

- scale:

  a logical value. If TRUE, each variable is scaled in the score.

## Value

a numerical value denoting weighted Imputation Score obtained for
provided imputation function and a table with scores and weights
calculated for particular columns.

## Examples

``` r
set.seed(111)
X <- matrix(rnorm(1000), nrow = 100)
X[runif(1000) < 0.4] <- NA
imputation_func <- miceDRF:::create_mice_imputation("pmm")
X_imp <- imputation_func(X)

miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func)
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
#> [1] 0.5580149
#> attr(,"dat")
#>     column_id weight     score n_columns_used
#> V1          1 0.2499 0.5616408              1
#> V3          3 0.2491 0.4399447              1
#> V8          8 0.2475 0.5055475              1
#> V10        10 0.2475 0.5871200              1
#> V5          5 0.2464 0.6850586              1
#> V6          6 0.2436 0.5006411              1
#> V7          7 0.2379 0.6925815              1
#> V4          4 0.2331 0.5289814              1
#> V9          9 0.2331 0.5369845              1
#> V2          2 0.2244 0.5429367              1

imputation_func <- miceDRF:::create_mice_imputation("mean")
X_imp <- imputation_func(X)

miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func)
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
#> [1] 0.7652515
#> attr(,"dat")
#>     column_id weight     score n_columns_used
#> V1          1 0.2499 0.7787504              1
#> V3          3 0.2491 0.6142445              1
#> V8          8 0.2475 0.7015580              1
#> V10        10 0.2475 0.7621273              1
#> V5          5 0.2464 0.9875052              1
#> V6          6 0.2436 0.6425266              1
#> V7          7 0.2379 0.9592499              1
#> V4          4 0.2331 0.7665391              1
#> V9          9 0.2331 0.7250410              1
#> V2          2 0.2244 0.7154879              1

miceDRF::Iscore(X, X_imp, N = 50, imputation_func = imputation_func, multiple = FALSE)
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
#> [1] 0.7652515
#> attr(,"dat")
#>     column_id weight     score n_columns_used
#> V1          1 0.2499 0.7787504              1
#> V3          3 0.2491 0.6142445              1
#> V8          8 0.2475 0.7015580              1
#> V10        10 0.2475 0.7621273              1
#> V5          5 0.2464 0.9875052              1
#> V6          6 0.2436 0.6425266              1
#> V7          7 0.2379 0.9592499              1
#> V4          4 0.2331 0.7665391              1
#> V9          9 0.2331 0.7250410              1
#> V2          2 0.2244 0.7154879              1

# zero imputation
X <- matrix(rnorm(1000), nrow = 100)
X[c(runif(1000) < 0.3)] <- NA
imputation_func <- function(X) {X[is.na(X)] <- 0; X}
X_imp <- imputation_func(X)

Iscore(X, X_imp, N = 50, imputation_func = imputation_func, multiple = FALSE)
#> No complete variables for training column 5. Skipping some observations.
#> No complete variables for training column 7. Skipping some observations.
#> No complete variables for training column 2. Skipping some observations.
#> No complete variables for training column 4. Skipping some observations.
#> No complete variables for training column 8. Skipping some observations.
#> No complete variables for training column 6. Skipping some observations.
#> No complete variables for training column 1. Skipping some observations.
#> No complete variables for training column 10. Skipping some observations.
#> No complete variables for training column 3. Skipping some observations.
#> No complete variables for training column 9. Skipping some observations.
#> [1] 0.8265311
#> attr(,"dat")
#>     column_id weight     score n_columns_used
#> V5          5 0.2400 0.7023632              1
#> V7          7 0.2275 0.8767438              1
#> V2          2 0.2211 0.8043624              1
#> V4          4 0.2100 0.9224779              1
#> V8          8 0.2100 0.8895373              1
#> V6          6 0.2059 0.7644872              1
#> V1          1 0.1971 0.7896345              1
#> V10        10 0.1971 0.8515953              1
#> V3          3 0.1924 0.8454487              1
#> V9          9 0.1659 0.8347622              1
```
