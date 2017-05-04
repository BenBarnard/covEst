source("R/helper.R")
#' Pooled Covariance
#'
#' @param x data
#' @param ... other options
#'
#' @return
#' @export
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#'
#' @examples pooled_cov(iris, group = Species)
pooled_cov <- function(x, ...){
  UseMethod("pooled_cov")
}

#' @export
#' @keywords internal
pooled_cov.data.frame <- helper(pooled_cov)

#' @export
#' @keywords internal
pooled_cov.resample <- helper(pooled_cov)

#' @export
#' @keywords internal
pooled_cov.grouped_df <- helper(pooled_cov)

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
pooled_cov.matrix <- function(x, ...){
  browser()
  dots <- lazy_dots(...)
  covs <- do.call(cov, c(x = list(x), lazy_eval(dots)))

}
