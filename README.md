# miceDRF: Imputation with Distributional Random Forests in MICE

<!-- badges: start -->
[![R-CMD-check](https://github.com/KrystynaGrzesiak/miceDRF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KrystynaGrzesiak/miceDRF/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/miceDRF)](https://CRAN.R-project.org/package=miceDRF)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/miceDRF)](https://CRAN.R-project.org/package=miceDRF)
[![pkgdown](https://img.shields.io/badge/pkgdown-reference-blue)](https://krystynagrzesiak.github.io/miceDRF/)
<!-- badges: end -->

`miceDRF` provides an imputation method for the
[`mice`](https://github.com/amices/mice) framework based on
distributional random forests (DRF).

The package extends multiple imputation by chained equations (MICE) with
a nonparametric approach that models conditional distributions rather
than only conditional means. This allows flexible imputation of complex
data structures, nonlinear effects, and heterogeneous conditional
distributions.

The method can be used directly within the standard `mice` workflow via:

```r
method = "DRF"
```

## Installation

The `miceDRF` package can be intstalled from CRAN:

```r
install.packages("miceDRF")
```

To install the development version run:

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("KrystynaGrzesiak/miceDRF")
```

## Example

```r
library(mice)
library(miceDRF)

set.seed(123)

# Generate data
n <- 200
d <- 5

X <- matrix(runif(n * d), nrow = n, ncol = d)

# Introduce missing values
pmiss <- 0.2

X.NA <- apply(X, 2, function(x) {
  U <- runif(length(x))
  ifelse(U <= pmiss, NA, x)
})

# Imputation with DRF
imp <- mice(X.NA, m = 1, method = "DRF")

Ximp <- complete(imp)
```

## References

Näf, J., Scornet, E., & Josse, J. (2024).
*What is a good imputation under MAR missingness?*
arXiv preprint.
https://arxiv.org/abs/2403.19196

Cevid, D., Michel, L., Näf, J., Meinshausen, N., and Buehlmann, P. (2022).
*Distributional random forests: Heterogeneity adjustment and multivariate distributional regression.*
Journal of Machine Learning Research, 23(333), 1–79.

## Citation

If you use `miceDRF` in your research, please cite:

Jeffrey Näf, Erwan Scornet, Julie Josse (2026).
*What Is a Good Imputation Under MAR Missingness?*
arXiv preprint arXiv:2403.19196.
https://arxiv.org/abs/2403.19196
