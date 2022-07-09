#' Create a ScoreTable
#'
#' @description Creates a table to calculate scores in specified standardized 
#' scale for each discrete raw score. Uses normalization provided by 
#' [FrequencyTable()] and scale definition created with 
#' [StandardScale()].
#'
#' After creation it can be used to normalize and standardize raw scores with
#' [normalize_score()] or [normalize_scores_df()].
#' 
#' [plot.ScoreTable()] method requires `ggplot2` package to be installed.
#' 
#' @param ft a `FrequencyTable` object
#' @param scale a `StandardScale` object or list of multiple `StandardScale` objects
#' @example man/examples/ScoreTable.R
#' @return object of class `ScoreTable`. Consists of:
#' 
#' - table: data.frame containing for each point in the raw score: 
#'     - number of observations (`n`), 
#'     - frequency in sample (`freq`),
#'     - quantile (`quan`), 
#'     - normalized Z-score (`Z`), 
#'     - score transformed to every of provided `StandardScales`
#' - status: list containing the total number of simulated observations (`n`) 
#' and information about raw scores range completion (`range`): complete or incomplete 
#' - scale: named list of all attached `StandardScale` objects 
#' @export

ScoreTable <- function(ft,
                       scale) {
  
  if (!"FrequencyTable" %in% class(ft))
    stop("Object of class 'FrequencyTable' needs to be provided to 'ft' argument")
  if (!class(scale) %in% c("StandardScale", "list"))
    stop("Object of class 'StandardScale' or list of such objects needs to be provided to 'scale' argument")
  if (is.StandardScale(scale)) {
    scales <- list(scale)
  } else {
    areScales <- all(sapply(scale, is.StandardScale))
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
  
  class(output) <- c("ScoreTable", if("Simulated" %in% class(ft)) "Simulated")
  return(output)
  
  
}

#' @param x A `ScoreTable` object
#' @param max numeric or NULL, specifying the maximal number of entries to be 
#' printed. By default, when NULL, \code{\link{getOption}("max.print")} used.
#' @param ... further arguments passed to or from other methods
#' @rdname ScoreTable
#' @export
print.ScoreTable <- function(x, max = NULL, ...) {
  
  cat(sep = "", "<ScoreTable> computed on: ", x$status$n, " observations\n")
  cat("\n")
  print(x$table, max = max, row.names = F)
  cat("\n\n")
  cat(sep = "", "Used StandardScales:\n") 
  invisible(lapply(x$scale, print))
  cat("\n")
  
  invisible(x)
}

#' @param x a `ScoreTable` object
#' @param scale_name if scores for multiple scales available, provide the name
#' of the scale for plotting.
#' @param ... further arguments passed to or from other methods
#' @rdname ScoreTable
#' @export
plot.ScoreTable <- function(x, scale_name = NULL, ...) {

  sum_of_n <- sum(x$table$n)
  
  if (length(x$scale) == 1) {
    scale <- x$scale[[1]]
  } else {
    if (is.null(scale_name) || length(scale_name) != 1 || 
        !scale_name %in% names(x$scale))
      stop("Provide one of the computed scale names to 'scale_name' argument.")
    scale <- x$scale[sapply(x$scale, \(y) y$name == scale_name)][[1]]
  }
  
  plot_data <- data.frame(
    x = unique(x$table[[scale$name]]))
  
  plot_data$prop <- sapply(
      plot_data$x,
      \(y) sum(as.numeric(x$table[x$table[[scale$name]] == y, "n"]))/sum_of_n
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
    ggplot2::stat_function(fun = stats::dnorm, args = c(scale$M, scale$SD), color = "black") +
    ggplot2::scale_fill_manual(name = "Distance from\nthe mean", values = c("green", "blue", "red")) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(
      name = scale$name, 
      breaks = c(scale$min, SD2[1], SD1[1], 
                 scale$M, SD1[2], SD2[2], scale$max))

}

#' Revert the ScoreTable back to FrequencyTable object.
#' @param x a *ScoreTable* object
#' @example man/examples/strip_ScoreTable.R
#' @export
strip_ScoreTable <- function(x) {
  
  if (!"ScoreTable" %in% class(x))
    stop("Object of class 'ScoreTable' needs to be provided to 'x' argument")
  
  ft <- list(
    table = x$table[, c("score", "n", "freq", "quan", "Z")],
    status = x$status)
  
  class(ft) <- c("FrequencyTable",
                 class(x)[!class(x) %in% "ScoreTable"])
  
  return(ft)
  
}

#' Attach additional StandardScale to already created ScoreTable
#' @param x A *ScoreTable* object
#' @param scale a *StandardScale* object or list of multiple *StandardScale* objects
#' @example man/examples/attach_scales.R
#' @export
attach_scales <- function(x, scale) {
  
  if (!"ScoreTable" %in% class(x))
    stop("Object of class 'ScoreTable' needs to be provided to 'x' argument")
  if (!class(scale) %in% c("StandardScale", "list"))
    stop("Object of class 'StandardScale' or list of such objects needs to be provided to 'scale' argument")
  if (is.StandardScale(scale)) {
    scales <- list(scale)
  } else {
    areScales <- all(sapply(scale, is.StandardScale))
    if (!isTRUE(areScales)) 
      stop("List provided to 'scale' argument should contain only StandardScale objects")
    scales <- scale
  }
  
  for (scale in scales) {
    val <- round(x$table$Z * scale$SD + scale$M)
    x$table[[scale$name]] <- 
      ifelse(val < scale$min, scale$min,
             ifelse(val > scale$max, scale$max, val))
    x$scale[[scale$name]] <- scale
  }
  
  output <- list(table = x$table,
                 status = x$status,
                 scale = x$scale)
  
  class(output) <- c("ScoreTable", if("Simulated" %in% class(x)) "Simulated")
  return(output)
  
}


#' @title Create GroupedScoreTable
#' @param table `GroupedFrequencyTable` object
#' @param scale a `StandardScale` object or list of multiple `StandardScale` objects
#' @seealso plot.GroupedScoreTable
#' @return `GroupedScoreTable` object, which consists of named `list` of 
#' `ScoreTable` objects and `GroupConditions` object used for grouping
#' @export

GroupedScoreTable <- function(table,
                              scale) {
  
  STs <- list()
  
  for (i in seq_along(table)) {
    
    STs[[names(table)[i]]] <-
      ScoreTable(table[[i]], scale)
  
  }
  
  attr(STs, "conditions") <- attr(table, "conditions")
  attr(STs, "scales") <- STs[[1]]$scale
  class(STs) <- "GroupedScoreTable"
  
  return(STs)
}

#' @title Gerenic plot of the GroupedScoreTable
#' @description Generic plot using `ggplot2`. It plots ScoreTables for all 
#' groups by default, or only chosen ones using when `group_names` argument is specified. 
#' @param x A `GroupedScoreTable` object
#' @param scale_name if scores for multiple scales available, provide the name
#' of the scale for plotting.
#' @param group_names *character* vector specifying which groups should appear in the plots
#' @param strict_names *boolean* If `TRUE`, then intersected groups are filtered
#' using *strict* strategy: `group_names` need to be provided in form: `"group1:group2"`. If
#' `FALSE`, then intersected groups will be taken into regard separately, so 
#' eg. when `"group1"` is provided to `group_names`, all of: `"group1:group2"`, 
#' `"group1:group3"`, `"group1:groupN"`  will be plotted. Defaults to `TRUE`
#' @param ... named list of additional arguments passed to either [ggplot2::facet_wrap()] 
#' when plotting `GroupedScoreTable` created on basis of one `GroupConditions` 
#' or [ggplot2::facet_grid()] when it was created with two such objects. 
#' @export
plot.GroupedScoreTable <- function(
    x, 
    scale_name = NULL,
    group_names = NULL,
    strict_names = TRUE,
    ...
) {
  
  if (!requireNamespace("ggplot2", quietly = T))
    stop("Generic plotting of 'GroupedScoreTable' requires 'ggplot2' package installed")
  
  if (!is.null(group_names)) {
    if (isTRUE(strict_names)){
      if (!any(group_names %in% names(x)))
        stop("Not all names specified in 'group_names' specify group names")
    } else {
      all_names <- unique(unlist(strsplit(names(x), split = ":")))
      if (!any(group_names %in% all_names))
        stop("Not all names specified in 'group_names' specify group names")
    }
  }
  
  scale_data <- list()
  
  plot_data <- lapply(seq_along(x), \(i) {
    
    if (!is.null(group_names)) {
      if (isTRUE(strict_names)) {
        name_check <- names(x)[i]
        if (!name_check %in% group_names)
          return(NULL)
      } else {
        name_check <- strsplit(names(x)[i], split = ":")[[1]]
        if (!any(name_check %in% group_names))
          return(NULL)
      } 
    }
    
    name <- strsplit(names(x)[i], split = ":")[[1]]
    
    st <- x[[i]]
    
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
      \(y) sum(as.numeric(st$table[st$table[[scale$name]] == y, "n"]))/sum_of_n
    )
    
    # during first group computations scale data is extracted
    if (length(scale_data) == 0) {
      scale_data[["min"]] <<- scale$min
      scale_data[["max"]] <<- scale$max
      scale_data[["SD1"]] <<- c(scale$M-scale$SD, scale$M+scale$SD) 
      scale_data[["SD2"]] <<- c(scale$M-2*scale$SD, scale$M+2*scale$SD)
    }
    
    plot_data$SD <- ifelse(
      plot_data$x < scale_data$SD2[1] | plot_data$x > scale_data$SD2[2], ">SD2",
      ifelse(plot_data$x < scale_data$SD1[1] | plot_data$x > scale_data$SD1[2], "SD1-SD2", "<SD1")
    )
    
    plot_data$SD <- factor(plot_data$SD, levels = c("<SD1", "SD1-SD2", ">SD2"))
    
    if (length(name) == 1) {
      plot_data$group1 <- name
    } else if (length(name) == 2) {
      plot_data$group1 <- name[1]
      plot_data$group2 <- name[2]
    }
    
    return(plot_data)
    
  })
  
  plot_data <- data.table::rbindlist(plot_data)
  
  if ("group2" %in% names(plot_data)) {
    
    plot_data$group1 <- factor(plot_data$group1, levels = c(".all1", attr(attr(x, "conditions")[[1]], "groups")))
    plot_data$group2 <- factor(plot_data$group2, levels = c(".all2", attr(attr(x, "conditions")[[2]], "groups")))
    
    grp1_row <- length(unique(plot_data$group2)) < length(unique(plot_data$group1))
    
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = prop)) + 
      ggplot2::geom_col(ggplot2::aes(fill = SD), alpha = 0.3, color = "black") + 
      ggplot2::scale_fill_manual(name = "Distance from\nthe mean", values = c("green", "blue", "red")) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(
        name = scale_name, 
        breaks = c(scale_data$min, scale_data$SD2[1], scale_data$SD1[1], 
                   scale_data$M, scale_data$SD1[2], scale_data$SD2[2], scale_data$max))
    
    plot_args <- list(
      rows = if (grp1_row) ggplot2::vars(group1) else ggplot2::vars(group2),
      cols = if (grp1_row) ggplot2::vars(group2) else ggplot2::vars(group1)
    )
    
    add_args <- list(...)
    
    plot <- plot +
      do.call(ggplot2::facet_grid,
              args = c(plot_args[!names(plot_args) %in% names(add_args)],
                       add_args))
    
    return(plot)
    
  } else {
    
    plot_data$group1 <- factor(plot_data$group1, levels = c(".all", attr(attr(x, "conditions")[[1]], "groups")))
    
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = prop)) + 
      ggplot2::geom_col(ggplot2::aes(fill = SD), alpha = 0.3, color = "black") + 
      ggplot2::scale_fill_manual(name = "Distance from\nthe mean", values = c("green", "blue", "red")) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(
        name = scale_name, 
        breaks = c(scale_data$min, scale_data$SD2[1], scale_data$SD1[1], 
                   scale_data$M, scale_data$SD1[2], scale_data$SD2[2], scale_data$max))
    
    plot_args <- list(
      facets = ggplot2::vars(group1)
    )
    
    add_args <- list(...)
    
    plot <- plot +
      do.call(ggplot2::facet_wrap, args = c(plot_args[!names(plot_args) %in% names(add_args)],
                                            add_args))
    
    return(plot)
    
  }
}
