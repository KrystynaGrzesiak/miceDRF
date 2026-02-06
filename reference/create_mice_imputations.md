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
#> <bytecode: 0x5568c64c3b98>
#> <environment: 0x5568c9e888d8>
#> 
#> $cart
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5568c64c3b98>
#> <environment: 0x5568c9e88638>
#> 
#> $sample
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5568c64c3b98>
#> <environment: 0x5568c9e88398>
#> 
#> $norm.nob
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5568c64c3b98>
#> <environment: 0x5568c9e880f8>
#> 
#> $DRF
#> function (X) 
#> {
#>     imp_dat <- mice(X, m = 1, method = method, printFlag = FALSE, 
#>         visitSequence = "arabic")
#>     mice::complete(imp_dat, action = "all")[[1]]
#> }
#> <bytecode: 0x5568c64c3b98>
#> <environment: 0x5568c9e87e58>
#> 
```
