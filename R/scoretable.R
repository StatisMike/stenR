#' Create a ScoreTable using `FrequencyTable` and `StandardScale` objects 
#'
#' @description Creates a table to recalculate raw scores into specified
#' standardized scale. Uses normalization provided by `FrequencyTable`.
#' 
#' `plot.ScoreTable` method requires `ggplot2` package to be installed.
#' 
#' @param ft `FrequencyTable` object
#' @param scale `StandardScale` object
#' @export

ScoreTable <- function(ft,
                       scale) {
  
  if (!"FrequencyTable" %in% class(ft))
    stop("Object of class 'FrequencyTable' needs to be provided to 'ft' argument")
  if (!"StandardScale" %in% class(scale))
    stop("Object of class 'StandardScale' or list of such objects needs to be provided to 'scale' argument")
    
  val <- round(ft$table$Z * scale$SD + scale$M)
  ft$table[[scale$name]] <- 
    ifelse(val < scale$min, scale$min,
           ifelse(val > scale$max, scale$max, val))
  
  output <- list(table = ft$table,
                 status = ft$status,
                 scale = scale)
  
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
  cat(sep = "", "Used StandardScale: ", st$scale$name, "\n")
  
  print(st$table, max = max, row.names = F)
  
  invisible(st)
}

#' @param st a `ScoreTable` object
#' @rdname ScoreTable
#' @export
plot.ScoreTable <- function(st) {

  sum_of_n <- sum(st$table$n)
  
  plot_data <- data.frame(
    x = unique(st$table[[st$scale$name]]))
  
  plot_data$prop <- sapply(
      plot_data$x,
      \(x) sum(as.numeric(st$table[st$table[[st$scale$name]] == x, "n"]))/sum_of_n
    )
  
  SD1 <- c(st$scale$M-st$scale$SD, st$scale$M+st$scale$SD)
  SD2 <- c(st$scale$M-2*st$scale$SD, st$scale$M+2*st$scale$SD)
  
  plot_data$SD <- ifelse(
    plot_data$x < SD2[1] | plot_data$x > SD2[2], ">SD2",
    ifelse(plot_data$x < SD1[1] | plot_data$x > SD1[2], "SD1-SD2", "<SD1")
  )
  
  plot_data$SD <- factor(plot_data$SD, levels = c("<SD1", "SD1-SD2", ">SD2"))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = prop)) + 
    ggplot2::geom_col(ggplot2::aes(fill = SD), alpha = 0.3, color = "black") + 
    ggplot2::stat_function(fun = dnorm, args = c(st$scale$M, st$scale$SD), color = "black") +
    ggplot2::scale_fill_manual(name = "Distance from\nthe mean", values = c("green", "blue", "red")) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(
      name = st$scale$name, 
      breaks = c(st$scale$min, SD2[1], SD1[1], 
                 st$scale$M, SD1[2], SD2[2], st$scale$max))

}
