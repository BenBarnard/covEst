#' Covariance Matrix Calculation for (Potentially Grouped) Data
#'
#' @param x data you would like the covariance matrix of. The parameter \code{x}
#'    can be a \code{data.frame}, \code{grouped_df}, \code{resample}, or
#'    \code{matrix}.
#' @param ... other options passed to covarince estimation method
#' @param covEst covariance estimation method, as a function. Defaults to
#'    \code{stats::cov}.
#'
#' @return list of covariance class matries by group
#'
#' @details This function has the capability to calculate total and group-level
#'    covariance matrices estimated from the given data. Specify group membership
#'    with the \code{group = "COLNAME"} syntax. Additionally, this function
#'    returns objects with an additional class: when this function returns group
#'    -level lists of matrices, each covariance matrix with have the \code{R}
#'    class \code{matrix} \emph{and} \code{covariance}; likewise, total covariance
#'    matrices are also returned with the classes \code{matrix} and
#'    \code{covariance}. Moreover, these matrices have an additional named
#'    attribute: \code{df}, for \emph{degrees of freedom}. The computational
#'    functionality of the \code{covariance} class will be explored in future
#'    updates.
#'
#' @export
#'
#' @examples
#' cov(iris[,1:4])
#' cov(iris, group = "Species")
cov <- function(x, ..., covEst = stats::cov){
  UseMethod("cov")
}

#' @export
#' @keywords internal
#' @importFrom dplyr as_data_frame
#' @importFrom stats cov
#' @importFrom stats setNames
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval f_unwrap
cov.data.frame <- function(x, ..., covEst = stats::cov){
  dots <- lazy_dots(...)
  if("group" %in% names(dots)){
    groupname <- names(unique(x[paste(dots$group$expr)]))
    group <- as.character(unique(x[[paste(dots$group$expr)]]))
    dots <- dots[!("group" %in% names(dots))]
    x <- setNames(lapply(group, function(y){
      as.matrix(x[x[groupname] == y,][names(x) != groupname])
    }), group)
    lapply(x, function(y){
      mat <- do.call(covEst, c(x = list(y), lazy_eval(dots)))
      df <- nrow(y) - 1
      atr <- attributes(mat)
      attributes(mat) <- c(atr, df = f_unwrap(~ df))
      class(mat) <- c("covariance", "matrix")
      mat
    })
  }else{
    mat <- do.call(covEst, c(x = list(x), lazy_eval(dots)))
    df <- nrow(x) - 1
    atr <- attributes(mat)
    attributes(mat) <- c(atr, df = f_unwrap(~ df))
    class(mat) <- c("covariance", "matrix")
    mat
  }
}

#' @export
#' @keywords internal
#' @importFrom dplyr as_data_frame
#' @importFrom stats cov
#' @importFrom stats setNames
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval f_unwrap
cov.grouped_df <- function(x, ..., covEst = stats::cov){
  groups <- attributes(x)$labels
  x <- as_data_frame(x)
  group <- as.character(groups[,1])
  groupname <- names(groups)
  ls <- setNames(lapply(group, function(y){
    as.matrix(x[x[groupname] == y,][names(x) != groupname])
  }), group)
  dots <- lazy_dots(...)
  lapply(ls, function(x){
    mat <- do.call(covEst, c(x = list(x), lazy_eval(dots)))
    df <- nrow(x) - 1
    atr <- attributes(mat)
    attributes(mat) <- c(atr, df = f_unwrap(~ df))
    class(mat) <- c("covariance", "matrix")
    mat
  })
}

#' @export
#' @keywords internal
#' @importFrom dplyr as_data_frame
#' @importFrom stats cov
#' @importFrom stats setNames
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval f_unwrap
cov.resample <- function(x, ..., covEst = stats::cov){
  x <- as_data_frame(x)
  groups <- attributes(x)$labels
  group <- as.character(groups[,1])
  groupname <- names(groups)
  ls <- setNames(lapply(group, function(y){
    as.matrix(x[x[groupname] == y,][names(x) != groupname])
  }), group)
  dots <- lazy_dots(...)
  lapply(ls, function(x){
    mat <- do.call(covEst, c(x = list(x), lazy_eval(dots)))
    df <- nrow(x) - 1
    atr <- attributes(mat)
    attributes(mat) <- c(atr, df = f_unwrap(~ df))
    class(mat) <- c("covariance", "matrix")
    mat
  })
}

#' @export
#' @keywords internal
#'
#' @importFrom stats cov
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval f_unwrap
cov.matrix <- function(x, ..., covEst = stats::cov){
  dots <- lazy_dots(...)
  mat <- do.call(covEst, c(x = list(x), lazy_eval(dots)))
  df <- nrow(x) - 1
  atr <- attributes(mat)
  attributes(mat) <- c(atr, df = f_unwrap(~ df))
  class(mat) <- c("covariance", "matrix")
  mat
}
