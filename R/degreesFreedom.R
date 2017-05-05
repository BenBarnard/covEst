#' Title
#'
#' @param x covariance class matrix
#'
#' @return Degrees of freedom of the covariance matrix
#' @export
#' @importFrom lazyeval f_eval
#'
#' @examples degreesFreedom(cov(iris[,1:4]))
degreesFreedom <- function(x){
  f_eval(attributes(x)$df)
}
