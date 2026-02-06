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

  words, words...

- ry:

  words, words...

- x:

  words, words...

- wy:

  words, words...

- min.node.size:

  words, words...

- num.features:

  words, words...

- num.trees:

  words, words...

## References

This method is described in detail in:

Näf, J., Scornet, E., & Josse, J. (2024). What is a good imputation
under MAR missingness?. arXiv. <https://arxiv.org/abs/2403.19196>

It's based on:

Cevid, D., Michel, L., Näf, J., Meinshausen, N., and B¨ uhlmann, P.
(2022). Distributional random forests: Heterogeneity adjustment and
multivariate distributional regression. Journal of Machine Learning
Research, 23(333):1–79.
