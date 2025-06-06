% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_IScores.R
\name{Iscores_compare}
\alias{Iscores_compare}
\title{Calculates IScores for multiple imputation functions}
\usage{
Iscores_compare(
  X,
  imputation_list,
  methods = NULL,
  N = 50,
  max_length = NULL,
  skip_if_needed = TRUE
)
}
\arguments{
\item{imputation_list}{a list of imputation functions}

\item{methods}{a character vector of names of methods in
\code{imputation_list}. It can be \code{NULL}, then the function will attempt
to get names from \code{imputation_list} object.}
}
\value{
a vector of IScores for provided methods
}
\description{
Calculates IScores for multiple imputation functions
}
\examples{
set.seed(111)
X <- matrix(rnorm(1000), nrow = 100)
X[runif(1000) < 0.4] <- NA

methods <- c("pmm", "cart", "sample", "norm.nob", "DRF")
imputation_list <- create_mice_imputations(methods)

Iscores_compare(X, imputation_list)

}
