#' Pooled Covariance Estimator
#'
#' @param x data as a \code{data.frame}, \code{grouped_df}, or \code{resample}
#'    objects
#' @param ... other options passed to estimation method
#' @param covEst covariance or precision matrix estimation method
#'
#' @return The pooled covariance or precision matrix with class \code{covariance}
#'    and total degrees of freedom attribute \code{df} as a formula.
#'
#' @details This function returns the weighted average of a collection of group-
#'    or class-specific covariance or precision matrices. The weights are
#'    proportional to the degrees of freedom of each matrix.  This matrix has
#'    the total degrees of freedom stored within the \code{df} attribute as a
#'    formula for simple evaluation by the \code{\link{degreesFreedom}} function
#'
#' @export
#'
#' @examples pooled_cov(iris, group = Species)
pooled_cov <- function(x, ...,  covEst = stats::cov){
  UseMethod("pooled_cov")
}

#' @export
#' @keywords internal
#' @importFrom dplyr as_data_frame
#' @importFrom stats cov
#' @importFrom stats setNames
#' @importFrom stats as.formula
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval f_unwrap
pooled_cov.data.frame <- function(x, ..., covEst = stats::cov){
  dots <- lazy_dots(...)
  if("group" %in% names(dots)){
    groupname <- names(unique(x[paste(dots$group$expr)]))
    group <- as.character(unique(x[[paste(dots$group$expr)]]))
    dots <- dots[!("group" %in% names(dots))]
    x <- setNames(lapply(group, function(y){
      as.matrix(x[x[groupname] == y,][names(x) != groupname])
    }), group)
    mat <- lapply(x, function(y){
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

  dfs <- lapply(mat, function(x){
    degreesFreedom(x)
  })

  pool <- Reduce(`+`, lapply(mat, function(x){
    x *  degreesFreedom(x)
  })) / Reduce(`+`, dfs)
  attributes(pool)$df <- as.formula(
    paste("~",
          do.call(paste,
                  args = c(dfs, sep = " + "))))
  pool
}

#' @export
#' @keywords internal
#' @importFrom dplyr as_data_frame
#' @importFrom stats cov
#' @importFrom stats setNames
#' @importFrom stats as.formula
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval f_unwrap
pooled_cov.grouped_df <- function(x, ..., covEst = stats::cov){
  groups <- attributes(x)$labels
  x <- as_data_frame(x)
  group <- as.character(groups[,1])
  groupname <- names(groups)
  ls <- setNames(lapply(group, function(y){
    as.matrix(x[x[groupname] == y,][names(x) != groupname])
  }), group)
  dots <- lazy_dots(...)
  mat <- lapply(ls, function(x){
    mat <- do.call(covEst, c(x = list(x), lazy_eval(dots)))
    df <- nrow(x) - 1
    atr <- attributes(mat)
    attributes(mat) <- c(atr, df = f_unwrap(~ df))
    class(mat) <- c("covariance", "matrix")
    mat
  })
  dfs <- lapply(mat, function(x){
    degreesFreedom(x)
  })

  pool <- Reduce(`+`, lapply(mat, function(x){
    x *  degreesFreedom(x)
  })) / Reduce(`+`, dfs)
  attributes(pool)$df <- as.formula(
    paste("~",
          do.call(paste,
                  args = c(dfs, sep = " + "))))
  pool
}

#' @export
#' @keywords internal
#' @importFrom dplyr as_data_frame
#' @importFrom stats cov
#' @importFrom stats setNames
#' @importFrom stats as.formula
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval f_unwrap
pooled_cov.resample <- function(x, ..., covEst = stats::cov){
  x <- as_data_frame(x)
  groups <- attributes(x)$labels
  group <- as.character(groups[,1])
  groupname <- names(groups)
  ls <- setNames(lapply(group, function(y){
    as.matrix(x[x[groupname] == y,][names(x) != groupname])
  }), group)
  dots <- lazy_dots(...)
  mat <- lapply(ls, function(x){
    mat <- do.call(covEst, c(x = list(x), lazy_eval(dots)))
    df <- nrow(x) - 1
    atr <- attributes(mat)
    attributes(mat) <- c(atr, df = f_unwrap(~ df))
    class(mat) <- c("covariance", "matrix")
    mat
  })
  dfs <- lapply(mat, function(x){
    degreesFreedom(x)
  })

  pool <- Reduce(`+`, lapply(mat, function(x){
    x *  degreesFreedom(x)
  })) / Reduce(`+`, dfs)
  attributes(pool)$df <- as.formula(
    paste("~",
          do.call(paste,
                  args = c(dfs, sep = " + "))))
  pool
}

