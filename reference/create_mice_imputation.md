# Very internal function for getting mice methods

Very internal function for getting mice methods

## Usage

``` r
create_mice_imputation(method)
```

## Arguments

- method:

  a character name of imputation method from `mice` package. For more
  details see [mice](https://amices.org/mice/reference/mice.html).

## Examples

``` r
methods <- "pmm"
imputation_funcs <- create_mice_imputations(methods)
```
