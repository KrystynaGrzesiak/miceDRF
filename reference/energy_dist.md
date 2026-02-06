# Energy distance

This function is a wrapper for energy distance calculating. For more
details see
[eqdist.e](https://rdrr.io/pkg/energy/man/eqdist.etest.html).

## Usage

``` r
energy_dist(X, X_imp)
```

## Arguments

- X:

  a complete original dataset

- X_imp:

  an imputed dataset

## Examples

``` r
X <- matrix(rnorm(1000), nrow = 100)
X_imp <- matrix(rnorm(1000), nrow = 100)
energy_dist(X, X_imp)
#> E-statistic 
#>    2.856983 
```
