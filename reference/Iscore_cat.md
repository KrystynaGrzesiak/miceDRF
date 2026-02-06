# energy-I-Score for imputation of categorical (mixed) data

energy-I-Score for imputation of categorical (mixed) data

## Usage

``` r
Iscore_cat(
  X,
  X_imp,
  imputation_func,
  factor_vars = TRUE,
  multiple = TRUE,
  N = 50,
  max_length = NULL,
  skip_if_needed = TRUE
)
```

## Arguments

- X:

  data containing missing values denoted with NA's

- X_imp:

  imputed dataset.

- imputation_func:

  a function that imputes data

- factor_vars:

  a logical value indicating whether imputation should be performed on
  factors. If `FALSE`, all the variables that are factors will be
  converted to numeric values.

- multiple:

  a logical indicating whether provided imputation method is a multiple
  imputation approach (i.e. it generates different values to impute for
  each call). Default to TRUE. Note that if multiple equals to FALSE, N
  is automatically set to 1.

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

a numerical value denoting weighted Imputation Score obtained for
provided imputation function and a table with scores and weights
calculated for particular columns.

## Details

The categorical variables should be stored as factors. If you need
additional conversion of the data (for example one-hot encoding) for
imputation, please, implement everything within `imputation_func`
parameter. You can use `miceDRF:::onehot_to_factor` and
`miceDRF:::factor_to_onehot` functions.

## References

This method is described in detail in:

Näf, J., Grzesiak, K., and Scornet, E. (2025a). How to rank imputation
methods? arXiv preprint arXiv:2507.11297
(<https://doi.org/10.48550/arXiv.2507.11297>).

## Examples

``` r
set.seed(123)
X <- matrix(rnorm(500), nrow = 100)
X <- cbind(X, factor(sample(1:5, 100, replace = TRUE), levels = 1:5))
X[runif(600) < 0.2] <- NA
X <- cbind(X, factor(sample(1:2, 100, replace = TRUE), levels = 1:2))
X <- as.data.frame(X)
X[["V6"]] <- factor(X[["V6"]], levels = 1:5)
X[["V7"]] <- factor(X[["V7"]], levels = 1:2)
X[["V8"]] <- rnorm(100)
imputation_func <- miceDRF:::create_mice_imputation("cart")
X_imp <- imputation_func(X)

Iscore_cat(X, X_imp, imputation_func, factor_vars = FALSE)
#> [1] 0.6935389
#> attr(,"dat")
#>    column_id weight     score n_columns_used
#> V1         1 0.1971 0.6819397              2
#> V5         5 0.1924 0.6718325              2
#> V3         3 0.1600 0.7061233              2
#> V6         6 0.1539 0.6258925              2
#> V2         2 0.1476 0.7203252              2
#> V4         4 0.1275 0.7790775              2
```
