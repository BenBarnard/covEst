#' Pooled Covariance
#'
#' @param x data
#' @param ... other options passed to estimation method
#' @param covEst covariance matrix estimation method
#'
#' @return pooled covariance class matrix
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
cov.resample <- function(x, ..., covEst = stats::cov){
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

