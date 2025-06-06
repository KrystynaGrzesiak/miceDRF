% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IScore_1col.R
\name{Iscore_1col}
\alias{Iscore_1col}
\title{Calculates score for one imputation function}
\usage{
Iscore_1col(
  X,
  X_imp,
  multiple = TRUE,
  N = 50,
  imputation_func,
  max_length = NULL,
  skip_if_needed = TRUE,
  scale = FALSE,
  col_id = 1
)
}
\arguments{
\item{X}{data containing missing values denoted with NA's}

\item{X_imp}{imputed dataset.}

\item{multiple}{a logical indicating whether provided imputation method is a
multiple imputation approach (i.e. it generates different values to impute
for each call). Default to TRUE. Note that if multiple equals to FALSE, N is
automatically set to 1.}

\item{N}{a numeric value. Number of samples from imputation distribution H.
Default to 50.}

\item{imputation_func}{a function that imputes data}

\item{max_length}{Maximum number of variables \eqn{X_j} to consider, can
speed up the code. Default to \code{NULL} meaning that all the columns will
be taken under consideration.}

\item{skip_if_needed}{logical, indicating whether some observations should be
skipped to obtain complete columns for scoring. If FALSE, NA will be returned
for column with no observed variable for training.}

\item{scale}{logica. If TRUE, each variable is scaled in the score.}
}
\value{
a numerical value denoting weighted Imputation Score obtained for
provided imputation function and a table with scores and weights calculated
for particular columns.
}
\description{
Calculates score for one imputation function
}
\examples{
set.seed(111)
X <- matrix(rnorm(1000), nrow = 100)
X[runif(1000) < 0.4] <- NA
imputation_func <- miceDRF:::create_mice_imputation("pmm")
X_imp <- imputation_func(X)

miceDRF::Iscore_1col(X, X_imp, N = 50, imputation_func = imputation_func)

}
