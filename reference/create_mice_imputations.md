# A function creating list of mice imputation functions

A function creating list of mice imputation functions

## Usage

``` r
create_mice_imputations(methods)
```

## Arguments

- methods:

  a character vector of names of mice methods

## Value

a named list of wrappers for mice imputation according to provided
methods.

## Examples

``` r
methods <- c("pmm", "cart", "sample", "norm.nob", "DRF")
create_mice_imputations(methods)
#> $pmm
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5589afd04aa0>
#> <environment: 0x5589b1560358>
#> 
#> $cart
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5589afd04aa0>
#> <environment: 0x5589b15600b8>
#> 
#> $sample
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5589afd04aa0>
#> <environment: 0x5589b155fe18>
#> 
#> $norm.nob
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5589afd04aa0>
#> <environment: 0x5589b155fb78>
#> 
#> $DRF
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5589afd04aa0>
#> <environment: 0x5589b155f8d8>
#> 
```
