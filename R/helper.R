#' Use Method Creater for Tests
#' @keywords internal
#' @export
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom stats setNames
#' @importFrom dplyr as_data_frame
#'
helper <- function(meth){
  func <- function(x, ...){
    if(is.null(attributes(x)$labels)){
      x <- dplyr::as_data_frame(x)
      groups <- attributes(x)$labels
    }else{
      groups <- attributes(x)$labels
      x <- dplyr::as_data_frame(x)
    }
    dots <- lazyeval::lazy_dots(...)
    if(is.null(groups)){
      groupname <- names(unique(x[paste(dots$group$expr)]))
      if(!(identical(groupname, character(0)))){
        group <- as.character(unique(x[[paste(dots$group$expr)]]))
      }else{
        group <- NULL
      }
    }else{
      group <- as.character(groups[,1])
      groupname <- names(groups)
    }
    dots <- dots[!(names(dots) == "group")]
    if(!(is.null(group))){
      ls <- stats::setNames(lapply(group, function(y){
        as.matrix(x[x[groupname] == y,][names(x) != groupname])
      }), group)

      a <- lapply(ls, function(x){
        do.call(what = meth,
                args = c(x = list(x), lazyeval::lazy_eval(dots)))
      })

    }else{
      ls <- list(as.matrix(x))
      a <- lapply(ls, function(x){
        do.call(what = meth,
                args = c(x = list(x), lazyeval::lazy_eval(dots)))})[[1]]
    }
    a

  }
}
