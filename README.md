# miceDRF: imputation with mice DRF and I-Score


## Installation

To install the latest development version directly from GitHub, run the following code in your R console:

```R
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("KrystynaGrzesiak/miceDRF")

```

## Example usage

```R

library(miceDRF)
library(mice)

# Generate a random dataset
n <- 200
d <- 5
X <- matrix(runif(n * d), nrow = n, ncol = d)

# Introduce missing values
pmiss <- 0.2
X.NA <- apply(X, 2, function(x) {
  U <- runif(length(x))
  ifelse(U <= pmiss, NA, x)
})

# Perform imputation with DRF method
imp <- mice(X.NA, m = 1, method = "DRF")
Ximp <- complete(imp)

```

## Citation

If you use miceDRF in your research, please cite the following work:

https://doi.org/10.48550/arXiv.2403.19196

