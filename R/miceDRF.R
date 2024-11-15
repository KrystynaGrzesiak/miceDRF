
#' mice DRF (Distributional Random Forest)
#'
#' This function performs imputation using MICE and Distributional Random Forest
#'
#' @importFrom drf drf
#'
#' @param y words, words...
#' @param ry words, words...
#' @param x words, words...
#' @param wy words, words...
#' @param min.node.size words, words...
#' @param num.features words, words...
#' @param num.trees words, words...
#'
#' @return
#'
#' @examples
#'
#' @references
#' This method is described in detaik in Näf, J., Scornet, E., & Josse, J. (2024).
#' What is a good imputation under MAR missingness?. arXiv.
#' \url{https://arxiv.org/abs/2403.19196}
#'
#' It's based on:
#'
#' Cevid, D., Michel, L., N¨af, J., Meinshausen, N., and B¨ uhlmann, P. (2022).
#' Distributional random forests: Heterogeneity adjustment and multivariate
#' distributional regression. Journal of Machine Learning Research, 23(333):1–79.
#'
#' @export
#'


mice.impute.DRF <- function (y, ry, x, wy = NULL, min.node.size = 1,
                            num.features = 10, num.trees = 10) {

  if (is.null(wy)) wy <- !ry

  if (dim(x)[2] == 0) {
    x <- cbind(x, 1)
    dimnames(x) <- list(NULL, "int")
  }

  xobs <- x[ry, , drop = FALSE]
  xmis <- x[wy, , drop = FALSE]
  yobs <- as.matrix(y[ry])

  # args<-list(...)
  # if ("m" %in% names(args)){
  #   m<-args$m
  # }else{
  #   m=1
  # }

  m <- 1

  fit <- drf(Y = yobs,
             X = xobs,
             num.trees = num.trees,
             num.features = num.features,
             compute.oob.predictions = F,
             min.node.size = min.node.size)

  DRFw <- predict(fit, newdata = xmis)$weights # These are the nodes now

  impute <- vapply(1:nrow(xmis), function(s) {
    yobs[sample(1:nrow(yobs), size = 1, replace = T, prob = DRFw[s, ]), ]
  }, numeric(m))  # sample one observation per xmis

  impute

}
