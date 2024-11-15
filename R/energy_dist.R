
#' Energy distance
#'
#' This function is a wrapper for energy distance calculating. For more details
#' see \link[energy]{eqdist.e}.
#'
#' @importFrom energy eqdist.e
#'
#' @param X a complete original dataset
#' @param X_imp an imputed dataset
#'
#' @examples
#' X <- matrix(rnorm(1000), nrow = 100)
#' X_imp <- matrix(rnorm(1000), nrow = 100)
#' energy_dist(X, X_imp)
#'
#' @export
#'


energy_dist <- function(X, X_imp)
  energy::eqdist.e(rbind(X, X_imp), c(nrow(X), nrow(X_imp)))


