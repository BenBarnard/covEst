#' Haff Shrinkage Covariance Estimator
#'
#' @param x data matrix
#' @param ... other options
#'
#' @return Haff shrinkage covariance estimator
#' @export
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom stats cov
#'
#' @examples Haff_shrinkage(as.matrix(iris[-5]))
Haff_shrinkage <- function(x, ...){
  cov <- cov(x)
  invCov <- solve(cov)
  n <- nrow(x)
  p <- ncol(x)
  tu <- tu(cov, n, p)
  (1 - tu) *
    (n - p - 2) *
    invCov + ((tu * (n * p - p - 2)) / tr(cov)) *
    diag(1, p)
}
