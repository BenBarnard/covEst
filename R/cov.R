source("R/helper.R")
#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
cov <- function(x, ...){
  UseMethod("cov")
}

#' @export
#' @keywords internal
cov.data.frame <- helper(cov)

#' @export
#' @keywords internal
cov.resample <- helper(cov)

#' @export
#' @keywords internal
cov.grouped_df <- helper(cov)



#' Cov helper function
#'
#' @param x data
#' @param ... other options passed to method
#' @param method method of cov/precision estimation
#'
#' @export
#' @keywords internal
#'
#' @importFrom stats cov
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#'
#' @examples cov(iris[,1:4])
cov.matrix <- function(x, ..., covEst = stats::cov){
  dots <- lazy_dots(...)
  do.call(covEst, c(x = list(x), lazy_eval(dots)))
}
