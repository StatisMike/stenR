#' Specify standard scale
#' 
#' @description `StandardScale` objects are used with \code{\link{ScoreTable}}
#' objects to recalculate \code{\link{FrequencyTable}} into some standardized
#' scale score. 
#' 
#' There are some `StandardScale` defaults available. Check out the
#' \code{\link{default_scales}} help page for more information.
#' 
#' Plot method requires `ggplot2` package to be installed.
#' 
#' @param name Name of the scale
#' @param M Mean of the scale
#' @param SD Standard deviation of the scale
#' @param min Minimal value the scale takes
#' @param max Maximal value the scale takes
#' @return StandardScale object
#' 
#' @export
#' 
StandardScale <- function(
  name, M, SD, min, max
) {
  
  if (!is.character(name) || length(name)) {
    "Argument provided to 'name' should be of type 'character'"
  }
  if (!is.numeric(M) || length(M)) {
    "Argument provided to 'M' should be of type 'numeric'"
  }
  if (!is.numeric(SD) || length(SD)) {
    "Argument provided to 'SD' should be of type 'numeric'"
  }
  if (!is.numeric(min) || length(min)) {
    "Argument provided to 'min' should be of type 'numeric'"
  }
  if (!is.numeric(max) || length(max)) {
    "Argument provided to 'max' should be of type 'numeric'"
  }
  
  obj <- list(name = name,
              M = M,
              SD = SD,
              min = min,
              max = max)
  
  class(obj) <- "StandardScale"
  
  return(obj)
  
}

#' @param object an object for which a summary is desired.
#' @rdname StandardScale
#' @export
print.StandardScale <- function(object) {
  
  cat("<StandardScale>: '", object$name, "'\n", sep = "")
  cat("( M: ", object$M, "; SD: ", object$SD, "; min: ", object$min, "; max: ", object$max, " )", sep = "")
  cat("\n")
  
}

#' Default Standard Scales
#' 
#' @description Few `StandardScale` objects pre-defined for usage. To create
#' any other, use [StandardScale()] function.
#' 
#' - **STEN**: M: 5.5, SD: 2, min: 1, max: 10
#' - **STANINE**: M: 5, SD: 2, min: 1, max: 9
#' - **TANINE**: M: 50, SD: 10, min: 1, max: 100
#' - **TETRONIC**: M: 10, SD: 4, min: 0, max: 20
#' - **WECHSLER_IQ**: M: 100, SD: 15, min: 40, max: 160
#' @name default_scales
#' @rdname default_scales
#' @aliases STEN STANINE TANINE TETRONIC WECHSLER_IQ
NULL

#' @export
STEN <- StandardScale(name = "sten", M = 5.5, SD = 2, min = 1, max = 10)

#' @export
STANINE <- StandardScale(name = "stanine", M = 5, SD = 2, min = 1, max = 9)

#' @export
TANINE <- StandardScale(name = "tanine", M = 50, SD = 10, min = 1, max = 100)

#' @export
TETRONIC <- StandardScale(name = "tetronic", M = 10, SD = 4, min = 0, max = 20)

#' @export
WECHSLER_IQ <- StandardScale(name = "wechslerIQ", M = 100, SD = 15, min = 40, max = 160)

#' @param scale StandardScale object
#' @param n Number of points the plot generates. The higher the number, the more
#' detailed are the plots. Default to 1000 for nicely detailed plot.
#' @rdname StandardScale
#' @export
plot.StandardScale <- function(scale, n = 1000) {
  
  if (!requireNamespace("ggplot2", quietly = T))
    stop("Generic plotting of 'StandardScore' requires 'ggplot2' package installed")
  
  data_points = data.frame(score = seq(from = scale$min, to = scale$max, by = 1))
  
  SD1 <- c(scale$M-scale$SD, scale$M+scale$SD)
  SD2 <- c(scale$M-2*scale$SD, scale$M+2*scale$SD)
  
  func1SD <- function(x) {
    y <- dnorm(x, scale$M, scale$SD)
    y[x < SD1[1] | x > SD1[2]] <- NA
    return(y)
  }
  func2SD <- function(x){
    y <- dnorm(x, scale$M, scale$SD)
    y[(x > SD1[1] & x < SD1[2]) | x < SD2[1] | x > SD2[2]] <- NA
    return(y)
  }
  func3SD <- function(x){
    y <- dnorm(x, scale$M, scale$SD)
    y[x > SD2[1] & x < SD2[2]] <- NA
    return(y)
  }
  
  ggplot2::ggplot(data_points, 
                  ggplot2::aes(x = score)) + 
    ggplot2::stat_function(fun = dnorm, args = c(scale$M, scale$SD), n = n) +
    ggplot2::stat_function(fun = func1SD, geom = "area", fill = "green", alpha = 0.3, n = n) +
    ggplot2::stat_function(fun = func2SD, geom = "area", fill = "blue", alpha = 0.3, n = n) +
    ggplot2::stat_function(fun = func3SD, geom = "area", fill = "red", alpha = 0.3, n = n) +
    ggplot2::scale_x_continuous(breaks = c(scale$min, SD2[1], SD1[1], scale$M, SD1[2], SD2[2], scale$max)) +
    ggplot2::geom_vline(xintercept = scale$M) +
    ggplot2::geom_vline(xintercept = SD1, color = "green") +
    ggplot2::geom_vline(xintercept = SD2, color = "blue") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = NULL)
  
}
  
.default_scales <- list(
  StandardScale(name = "sten", M = 5.5, SD = 2, min = 1, max = 10),
  StandardScale(name = "stanine", M = 5, SD = 2, min = 1, max = 9),
  StandardScale(name = "tanine", M = 50, SD = 10, min = 1, max = 100),
  StandardScale(name = "tetronic", M = 10, SD = 4, min = 0, max = 20),
  StandardScale(name = "wechsler-iq", 100, SD = 15, min = 40, max = 160)
)
