#' Create a ScoreTable
#'
#' @description Creates a table to calculate scores in specified standardized 
#' scale for each discrete raw score. Uses normalization provided by 
#' \code{\link{FrequencyTable}} and scale definition created with 
#' \code{\link{StandardScale}}.
#'
#' After creation it can be used to normalize and standardize raw scores with
#' \code{\link{normalize_score}}.
#' 
#' `plot.ScoreTable` method requires `ggplot2` package to be installed.
#' 
#' @param ft a `FrequencyTable` object
#' @param scale a `StandardScale` object or list of multiple `StandardScale` objects
#' @example examples/ScoreTable.R
#' @return object of class `StandardScale`. Consists of:
#' 
#' - table: data.frame containing for each point in the raw score: 
#'     - number of observations (`n`), 
#'     - frequency in sample (`freq`),
#'     - quantile (`quan`), 
#'     - normalized Z-score (`Z`), 
#'     - score transformed to every of provided `StandardScales`
#' - status: list with status description of the table
#'     - range: either 'complete' or 'incomplete', if there were any missing
#'     raw scores between minimum and maximum
#'     - n: complete number or observations for which the `ScoreTable` is computed
#' - scale: named list of all attached `StandardScale` objects 
#' @export

ScoreTable <- function(ft,
                       scale) {
  
  if (class(ft) != "FrequencyTable")
    stop("Object of class 'FrequencyTable' needs to be provided to 'ft' argument")
  if (!class(scale) %in% c("StandardScale", "list"))
    stop("Object of class 'StandardScale' or list of such objects needs to be provided to 'scale' argument")
  if (class(scale) == "StandardScale") {
    scales <- list(scale)
  } else if (class(scale) == "list") {
    areScales <- all(sapply(scale, \(x) class(x) == "StandardScale"))
    if (!isTRUE(areScales)) 
      stop("List provided to 'scale' argument should contain only StandardScale objects")
    scales <- scale
  }
  
  scales_ls <- list()
    
  for (scale in scales) {
    val <- round(ft$table$Z * scale$SD + scale$M)
    ft$table[[scale$name]] <- 
      ifelse(val < scale$min, scale$min,
             ifelse(val > scale$max, scale$max, val))
    scales_ls[[scale$name]] <- scale
  }

  
  output <- list(table = ft$table,
                 status = ft$status,
                 scale = scales_ls)
  
  class(output) <- c("ScoreTable")
  return(output)
  
  
}

#' @rdname ScoreTable
attach_scales <- function(st, scale) {
  
  if (class(st) != "ScoreTable")
    stop("Object of class 'ScoreTable' needs to be provided to 'ft' argument")
  if (!class(scale) %in% c("StandardScale", "list"))
    stop("Object of class 'StandardScale' or list of such objects needs to be provided to 'scale' argument")
  if (class(scale) == "StandardScale") {
    scales <- list(scale)
  } else if (class(scale) == "list") {
    areScales <- all(sapply(scale, \(x) class(x) == "StandardScale"))
    if (!isTRUE(areScales)) 
      stop("List provided to 'scale' argument should contain only StandardScale objects")
    scales <- scale
  }
  
  for (scale in scales) {
    val <- round(st$table$Z * scale$SD + scale$M)
    st$table[[scale$name]] <- 
      ifelse(val < scale$min, scale$min,
             ifelse(val > scale$max, scale$max, val))
    st$scale[[scale$name]] <- scale
  }
  
  output <- list(table = st$table,
                 status = st$status,
                 scale = st$scale)
  
  class(output) <- c("ScoreTable")
  return(output)
  
}

#' @param st A `ScoreTable` object
#' @param max numeric or NULL, specifying the maximal number of entries to be 
#' printed. By default, when NULL, \code{\link{getOption}("max.print")} used.
#' @rdname ScoreTable
#' @export
print.ScoreTable <- function(st, max = NULL) {
  
  cat(sep = "", "<ScoreTable> computed on: ", st$status$n, " observations\n")
  cat("\n")
  print(st$table, max = max, row.names = F)
  cat("\n\n")
  cat(sep = "", "Used StandardScales:\n") 
  invisible(lapply(st$scale, print))
  cat("\n")
  
  invisible(st)
}

#' @param st a `ScoreTable` object
#' @param scale_name if scores for multiple scales available, provide the name
#' of the scale for plotting.
#' @rdname ScoreTable
#' @export
plot.ScoreTable <- function(st, scale_name = NULL) {

  sum_of_n <- sum(st$table$n)
  
  if (length(st$scale) == 1) {
    scale <- st$scale[[1]]
  } else {
    if (is.null(scale_name) || length(scale_name) != 1 || 
        !scale_name %in% names(st$scale))
      stop("Provide one of the computed scale names to 'scale_name' argument.")
    scale <- st$scale[sapply(st$scale, \(x) x$name == scale_name)][[1]]
  }
  
  plot_data <- data.frame(
    x = unique(st$table[[scale$name]]))
  
  plot_data$prop <- sapply(
      plot_data$x,
      \(x) sum(as.numeric(st$table[st$table[[scale$name]] == x, "n"]))/sum_of_n
    )
  
  SD1 <- c(scale$M-scale$SD, scale$M+scale$SD)
  SD2 <- c(scale$M-2*scale$SD, scale$M+2*scale$SD)
  
  plot_data$SD <- ifelse(
    plot_data$x < SD2[1] | plot_data$x > SD2[2], ">SD2",
    ifelse(plot_data$x < SD1[1] | plot_data$x > SD1[2], "SD1-SD2", "<SD1")
  )
  
  plot_data$SD <- factor(plot_data$SD, levels = c("<SD1", "SD1-SD2", ">SD2"))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = prop)) + 
    ggplot2::geom_col(ggplot2::aes(fill = SD), alpha = 0.3, color = "black") + 
    ggplot2::stat_function(fun = dnorm, args = c(scale$M, scale$SD), color = "black") +
    ggplot2::scale_fill_manual(name = "Distance from\nthe mean", values = c("green", "blue", "red")) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(
      name = scale$name, 
      breaks = c(scale$min, SD2[1], SD1[1], 
                 scale$M, SD1[2], SD2[2], scale$max))

}
