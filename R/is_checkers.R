#' Checkers for stenR S3 classes
#' @name is_stenR_methods
#' @rdname is_stenR_methods
#' @param x any \R object
#' @description 
#' Various functions to check if given \R object is of given class
#' @aliases is.ScoringTable
NULL

#' @rdname is_stenR_methods
#' @export
is.ScoringTable <- function(x) {
  
  inherits(x, "ScoringTable")
  
}