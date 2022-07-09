#' Specify standard scale
#' 
#' @description `StandardScale` objects are used with [ScoreTable()] or
#' [GroupedScoreTable()] objects to recalculate [FrequencyTable()] or 
#' [GroupedFrequencyTable()] into some standardized scale score. 
#' 
#' There are few `StandardScale` defaults available. 
#'  
#' Plot method requires `ggplot2` package to be installed.
#' 
#' @param name Name of the scale
#' @param M Mean of the scale
#' @param SD Standard deviation of the scale
#' @param min Minimal value the scale takes
#' @param max Maximal value the scale takes
#' @importFrom cli cli_abort
#' @return StandardScale object
#' 
#' @export
#' 
StandardScale <- function(
  name, M, SD, min, max
) {
  
  if (!is.character(name) || length(name) > 1) {
    cli_abort("Argument provided to {.var name} should be of type {.cls character} and length 1.",
              class = "TypeError")
  }
  if (!is.numeric(M) || length(M) > 1) {
    cli_abort("Argument provided to {.var M} should be of type {.cls numeric} and length 1.",
              class = "TypeError")
  }
  if (!is.numeric(SD) || length(SD) > 1) {
    cli_abort("Argument provided to {.var SD} should be of type {.cls numeric} and length 1.",
              class = "TypeError")
  }
  if (!is.numeric(min) || length(min) > 1) {
    cli_abort("Argument provided to {.var min} should be of type {.cls numeric} and length 1.",
              class = "TypeError")
  }
  if (!is.numeric(max) || length(max) > 1) {
    cli_abort("Argument provided to {.var max} should be of type {.cls numeric} and length 1.",
              class = "TypeError")
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
#' @importFrom cli cli_inform
#' @export
print.StandardScale <- function(x, ...) {
  
  cli_inform("{.cls StandardScale}: {.emph {x$name}}")
  cli_inform("{.var M}: {x$M} {.var SD}: {x$SD} {.var min} {x$min}: {.var max}: {x$max}")
  
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
  
  rlang::check_installed("ggplot2")
  
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
