#' Degrees of Freedom of a Covariance Matrix
#'
#' @param x a matrix with class \code{covariance}
#'
#' @return The degrees of freedom of the covariance matrix, as a \code{numeric}
#'    object
#'
#' @details This function uses the \code{f_eval} function from the \code{lazyeval}
#'    package to return the degrees of freedom stored within the \code{df}
#'    attribute of a \code{matrix} object having the additional class
#'    \code{covariance}. This evaluation is necessary because the \code{df}
#'    attribute of a matrix with class \code{covariance} is stored as a
#'    \code{formula} object.
#'
#' @export
#' @importFrom lazyeval f_eval
#'
#' @examples
#' degreesFreedom(cov(iris[,1:4]))
#' lapply(cov(iris, group = "Species"), degreesFreedom)
degreesFreedom <- function(x){
  f_eval(attributes(x)$df)
}
