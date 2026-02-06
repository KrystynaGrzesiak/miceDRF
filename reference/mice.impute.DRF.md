# mice DRF (Distributional Random Forest)

This function performs imputation using MICE and Distributional Random
Forest

## Usage

``` r
mice.impute.DRF(
  y,
  ry,
  x,
  wy = NULL,
  min.node.size = 1,
  num.features = 10,
  num.trees = 10,
  ...
)
```

## Arguments

- y:

  Vector to be imputed.

- ry:

  Logical vector indicating which elements of \`y\` are used to fit the
  imputation model (TRUE = observed).

- x:

  Numeric design matrix with \`length(y)\` rows, containing predictors
  for \`y\` and no missing values.

- wy:

  Logical vector indicating elements of \`y\` for which imputations are
  generated.

- min.node.size:

  target minimum number of observations in each tree leaf in DRF. The
  default value is 5.

- num.features:

  the number of random features to sample.

- num.trees:

  number of trees in DRF. Default to 10.

- ...:

  used for compatibility with `mice` package.

## References

This method is described in detail in:

Näf, J., Scornet, E., & Josse, J. (2024). What is a good imputation
under MAR missingness?. arXiv. <https://arxiv.org/abs/2403.19196>

It's based on:

Cevid, D., Michel, L., Näf, J., Meinshausen, N., and B¨ uhlmann, P.
(2022). Distributional random forests: Heterogeneity adjustment and
multivariate distributional regression. Journal of Machine Learning
Research, 23(333):1–79.
