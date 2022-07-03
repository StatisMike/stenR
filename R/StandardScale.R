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

#' @param x a `StandardScale` object.
#' @param ... further arguments passed to or from other methods.
#' @rdname StandardScale
#' @export
print.StandardScale <- function(x, ...) {
  
  cat("<StandardScale>: '", x$name, "'\n", sep = "")
  cat("( M: ", x$M, "; SD: ", x$SD, "; min: ", x$min, "; max: ", x$max, " )", sep = "")
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

#' @param x a `StandardScale` object
#' @param n Number of points the plot generates. The higher the number, the more
#' detailed are the plots. Default to 1000 for nicely detailed plot.
#' @param ... further arguments passed to or from other methods.
#' @rdname StandardScale
#' @export
plot.StandardScale <- function(x, n = 1000, ...) {
  
  if (!requireNamespace("ggplot2", quietly = T))
    stop("Generic plotting of 'StandardScore' requires 'ggplot2' package installed")
  
  data_points = data.frame(score = seq(from = x$min, to = x$max, by = 1))
  
  SD1 <- c(x$M-x$SD, x$M+x$SD)
  SD2 <- c(x$M-2*x$SD, x$M+2*x$SD)
  
  func1SD <- function(x) {
    y <- stats::dnorm(x, x$M, x$SD)
    y[x < SD1[1] | x > SD1[2]] <- NA
    return(y)
  }
  func2SD <- function(x) {
    y <- stats::dnorm(x, x$M, x$SD)
    y[(x > SD1[1] & x < SD1[2]) | x < SD2[1] | x > SD2[2]] <- NA
    return(y)
  }
  func3SD <- function(x) {
    y <- stats::dnorm(x, x$M, x$SD)
    y[x > SD2[1] & x < SD2[2]] <- NA
    return(y)
  }
  
  ggplot2::ggplot(data_points, 
                  ggplot2::aes(x = score)) + 
    ggplot2::stat_function(fun = stats::dnorm, args = c(x$M, x$SD), n = n) +
    ggplot2::stat_function(fun = func1SD, geom = "area", ggplot2::aes(fill = factor("<1SD", levels = c("<1SD", "1SD-2SD", ">2SD"))), alpha = 0.3, n = n) +
    ggplot2::stat_function(fun = func2SD, geom = "area", ggplot2::aes(fill = factor("1SD-2SD", levels = c("<1SD", "1SD-2SD", ">2SD"))), alpha = 0.3, n = n) +
    ggplot2::stat_function(fun = func3SD, geom = "area", ggplot2::aes(fill = factor(">2SD", levels = c("<1SD", "1SD-2SD", ">2SD"))), alpha = 0.3, n = n) +
    ggplot2::scale_x_continuous(breaks = c(x$min, SD2[1], SD1[1], x$M, SD1[2], SD2[2], x$max)) +
    ggplot2::geom_vline(xintercept = x$M) +
    ggplot2::geom_vline(xintercept = SD1, color = "green") +
    ggplot2::geom_vline(xintercept = SD2, color = "blue") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::scale_fill_manual("Distance from\nthe mean",
                               values = c("<1SD" = "green", "1SD-2SD" = "blue", ">2SD" = "red")) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 0.15)))
  
}
  
.default_scales <- list(
  StandardScale(name = "sten", M = 5.5, SD = 2, min = 1, max = 10),
  StandardScale(name = "stanine", M = 5, SD = 2, min = 1, max = 9),
  StandardScale(name = "tanine", M = 50, SD = 10, min = 1, max = 100),
  StandardScale(name = "tetronic", M = 10, SD = 4, min = 0, max = 20),
  StandardScale(name = "wechsler-iq", 100, SD = 15, min = 40, max = 160)
)
