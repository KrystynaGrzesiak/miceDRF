# Energy-I-Score: First Steps

### Introduction

This vignette provides a basic introduction to using the `miceDRF`
package.  
To install the latest development version directly from GitHub, run the
following code in your R console:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("KrystynaGrzesiak/miceDRF")
```

After installation, load the package and set a random seed to ensure
reproducibility:

``` r
library(miceDRF)

set.seed(17)
```

We will begin by loading the example dataset `windspeed` from the `mice`
package:

``` r
library(mice)
#> 
#> Attaching package: 'mice'
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following objects are masked from 'package:base':
#> 
#>     cbind, rbind

head(windspeed)
#>   RochePt Rosslare Shannon Dublin Clones MalinHead
#> 1    4.92     7.29    3.67   3.71   2.71      7.83
#> 2   22.50    19.41   16.13  16.08  16.58     19.67
#> 3    7.54     9.29   11.00   1.71   9.71     15.37
#> 4    6.29     6.75    8.25   8.46  10.29     15.46
#> 5   10.34    11.29    9.38   8.71   8.42     11.12
#> 6   10.63    11.38    5.71   6.54   5.17      6.38
```

Next, we will introduce some missing values using the
[`ampute()`](https://amices.org/mice/reference/ampute.html) function:

``` r
windspeed_miss <- ampute(windspeed, 0.2, mech  = "MAR")$amp
```

Finally, let’s check the proportion of missing values in each column:

``` r
colMeans(is.na(windspeed_miss))
#>    RochePt   Rosslare    Shannon     Dublin     Clones  MalinHead 
#> 0.05080831 0.03695150 0.03233256 0.03002309 0.03002309 0.02309469
```

### Imputation Function

Before we can compute the *energy-I-Score*, we need to define the
**imputation method** that will be applied to the dataset with missing
values. The `miceDRF` package is flexible — it allows you to use **any
imputation strategy**, as long as your function follows a few simple
rules.

Your imputation function:

1.  **Must accept** a dataset with missing values as its only required
    argument.  
2.  **Must return** a dataset of the same dimensions, with missing
    values filled in.  
3.  **Should not require any additional non-default arguments** — the
    function will be called internally by the scoring procedure.
4.  Can be:
    - a simple custom function (e.g. mean imputation),
    - a wrapper around a more complex method (e.g. random forests,
      Bayesian models, `mice`, `missForest`),
    - or any other approach that returns a fully imputed dataset.

For example, let’s define *zero imputation* below

``` r
impute_zero <- function(X) { X[is.na(X)] <- 0; X }
```

Additionally, the `miceDRF` package provides a convenient way to quickly
implement imputation methods from the `mice` package that are fully
compatible with the energy-I-Score. You can simply use
[`create_mice_imputation()`](https://krystynagrzesiak.github.io/miceDRF/reference/create_mice_imputation.md)
with the name of the method (for the list of methods see
[`?mice::mice`](https://amices.org/mice/reference/mice.html)) as below
*random forest imputation*:

``` r
library(ranger)                                  # for random forest imputation
impute_rf <- create_mice_imputation("rf")
```

### Energy-I-Score

To calculate Energy-I-Score we need to provide imputation method along
with incomplete and imputed datasets. Let’s see an example for random
forest imputation:

``` r
sc <- Iscore(windspeed_miss, impute_rf(windspeed_miss), imputation_func = impute_rf)

sc
#> [1] 2.719471
#> attr(,"dat")
#>           column_id     weight    score n_columns_used
#> RochePt           1 0.04822683 2.865981              1
#> Rosslare          2 0.03558609 3.239500              1
#> Shannon           3 0.03128717 2.765237              1
#> Dublin            4 0.02912171 2.181387              1
#> Clones            5 0.02912171 1.986160              1
#> MalinHead         6 0.02256132 3.163669              1
```

The result is a single score summarizing the performance across all
columns that required imputation. In addition, a table with scores
calculated for each column separately is also returned as an attribute
of the result. The table says what were score, weight and number of
columns used for the training part for each column. To access this
table, simply use the [`attr()`](https://rdrr.io/r/base/attr.html)
function.

``` r
attr(sc, "dat")
#>           column_id     weight    score n_columns_used
#> RochePt           1 0.04822683 2.865981              1
#> Rosslare          2 0.03558609 3.239500              1
#> Shannon           3 0.03128717 2.765237              1
#> Dublin            4 0.02912171 2.181387              1
#> Clones            5 0.02912171 1.986160              1
#> MalinHead         6 0.02256132 3.163669              1
```

The
[`Iscore()`](https://krystynagrzesiak.github.io/miceDRF/reference/Iscore.md)
function exposes several parameters that control how the Energy-I-Score
is computed.  
Below we illustrate the most important ones with concise examples.

The parameter `N` controls how many times the missing part is re-imputed
to estimate the score (only relevant if your method is multiple). Let us
note that for deterministic methods, `N` is effectively ignored (when
`multiple = FALSE`).

``` r
Iscore(windspeed_miss, impute_rf(windspeed_miss), imputation_func = impute_rf, N = 5)
#> [1] 3.520822
#> attr(,"dat")
#>           column_id     weight    score n_columns_used
#> RochePt           1 0.04822683 3.771838              1
#> Rosslare          2 0.03558609 3.616765              1
#> Shannon           3 0.03128717 3.867273              1
#> Dublin            4 0.02912171 3.082888              1
#> Clones            5 0.02912171 3.136965              1
#> MalinHead         6 0.02256132 3.413230              1
```

When working with datasets that contain many columns with missing
values, calculating the energy-I-Score for all of them can be
time-consuming. To make the computation faster, you can **limit the
number of columns used for scoring** by setting the `max_length`
parameter. This parameter defines how many variables (columns) will be
used to compute the score. The function automatically selects the
variables **with the largest number of missing values first**. By
default, `max_length = NULL`, which means that **all columns** with
missing data are included in the calculation.

``` r
Iscore(windspeed_miss, impute_rf(windspeed_miss), imputation_func = impute_rf, max_length = 2)
#> [1] 3.458355
#> attr(,"dat")
#>          column_id     weight    score n_columns_used
#> RochePt          1 0.04822683 3.332980              1
#> Rosslare         2 0.03558609 3.628264              1
```

Some variables may lack fully observed predictors among rows needed for
training the conditional imputations. Thus, we can specify

- `skip_if_needed = TRUE` (default): try to skip minimal rows to obtain
  a workable design; proceed with scoring.

- `skip_if_needed = FALSE`: if no complete predictors can be formed, the
  score for that variable is returned as NA.

Additionally, we can adopt some scaling by `scale = TRUE` to ensure each
variable is standardized internally before computing distances. This
prevents variables with large numeric ranges from dominating the score.

#### Summary of best practices

- Use `multiple = TRUE` with a genuinely multiple imputers and determine
  `N` for stable estimates.

- Consider `scale = TRUE` when mixing variables on different scales.

- Use `max_length` for quick experiments; remove it for final runs.

- Keep `skip_if_needed = TRUE` unless you explicitly want to flag
  unscorable columns with NA.

#### Comparisons of energy-I-Scores

We also provide functionality for **quick benchmarking** of different
imputation methods. To run Energy-I-Scores for more than one imputation
function at once, use the
[`Iscores_compare()`](https://krystynagrzesiak.github.io/miceDRF/reference/Iscores_compare.md)
function. You simply need to provide an incomplete dataset and a **list
of imputation functions**, as shown below:

``` r
imputation_list <- list(rf = impute_rf, zero = impute_zero)

Iscores_compare(windspeed_miss, imputation_list, N = 10)
#> [1] "Calculating score for method: rf"
#> [1] "Calculating score for method: zero"
#>        rf      zero 
#>  3.083034 11.063610
```

In the example above, we compare random forest imputation with a simple
zero imputation method. According to the results, the random forest
method yields a lower Energy-I-Score, indicating better imputation
quality compared to the zero imputation baseline.

### Energy-I-Score for mixed datasets

When the data is mixed, i.e. it contains both categorical and numerical
variables, you can use `IScore_cat()` function which is able to
calculate scores on categorical columns. Before using it make sure that
all the variables that contain categorical values are stored as
**factors**.

### Energy score

If you have access to the original dataset before imputation, you can
also use the energy distance as an additional evaluation metric. Our
package provides an easy-to-use wrapper
[`energy_dist()`](https://krystynagrzesiak.github.io/miceDRF/reference/energy_dist.md)
around the
[`energy::eqdist.e`](https://rdrr.io/pkg/energy/man/eqdist.etest.html)
function from the energy package. You can use it providing complete and
imputed datasets:

``` r
energy_dist(windspeed, impute_rf(windspeed_miss))
#> E-statistic 
#>    0.763335

energy_dist(windspeed, impute_zero(windspeed_miss))
#> E-statistic 
#>    29.18786
```

## References

This approach follows the methodology proposed by Näf, Grzesiak, and
Scornet (2025) in “How to rank imputation methods?” (arXiv:2507.11297).
