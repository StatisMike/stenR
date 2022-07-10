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
#' - scale: named list of all attached `StandardScale` objects \
#' @importFrom cli cli_abort
#' @export

ScoreTable <- function(ft,
                       scale) {
  
  if (!is.FrequencyTable(ft))
    cli_abort("Object of class {.cls FrequencyTable} needs to be provided to the {.var ft} argument.",
              cli_class$error$Class)
  if (!is.StandardScale(scale) && !is.list(scale))
    cli_abort("Object of class {.cls StandardScale} or list of such objects need to be provided to the {.var scale} argument.",
             cli_class$error$Class)
  if (is.StandardScale(scale)) {
    scales <- list(scale)
  } else {
    areScales <- all(sapply(scale, is.StandardScale))
    if (!isTRUE(areScales)) 
      cli_abort("List provided to {.var scale} argument should contain only objects of class {.cls StandardScale}",
                cli_class$error$Class)
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
  
  class(output) <- c("ScoreTable", if(is.Simulated(ft)) "Simulated")
  return(output)
  
  
}

#' @param x A `ScoreTable` object
#' @rdname ScoreTable
#' @importFrom cli cli_text cli_ul cli_li cli_end
#' @export
print.ScoreTable <- function(x, ...) {
  
  cli_text("{.cls ScoreTable}") 
  if (is.Simulated(x))
    cli_text("Distribution is {.strong Simulated}")
  else
    cli_text("computed on {.val {x$status$n}} observations")
  cli_text("Attached {.cls StandardScale} object(s):")
  ul <- cli_ul()
  for (scale in x$scale)
    cli_li("{.strong {scale$name}}: {.var M}: {.val {scale$M}}; {.var SD}: {.val {scale$SD}}; {.var min}: {.val {scale$min}}; {.var max}: {.val {scale$max}}")
  cli_end(ul)

}

#' @param x a `ScoreTable` object
#' @param scale_name if scores for multiple scales available, provide the name
#' of the scale for plotting.
#' @param ... further arguments passed to or from other methods
#' @rdname ScoreTable
#' @importFrom cli cli_abort
#' @export
plot.ScoreTable <- function(x, scale_name = NULL, ...) {
  
  rlang::check_installed("ggplot2")

  sum_of_n <- sum(x$table$n)
  
  if (length(x$scale) == 1) {
    scale <- x$scale[[1]]
  } else {
    if (is.null(scale_name) || length(scale_name) != 1 || 
        !scale_name %in% names(x$scale))
      cli_abort("Provide one of the computed scale names ({.val {names(x$scale)}}) to the {.var scale_name} argument.",
                class = cli_class$error$NoScale)
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
#' @importFrom cli cli_abort
#' @export
strip_ScoreTable <- function(x) {
  
  if (!is.ScoreTable(x))
    cli_abort("Object of class {.cls ScoreTable} needs to be provided to {.var x} argument.",
              class = cli_class$error$Class)

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
#' @importFrom cli cli_abort
#' @export
attach_scales <- function(x, scale) {
  
  if (!is.ScoreTable(x))
    cli_abort("Object of class {.cls ScoreTable} needs to be provided to {.var x} argument.",
              class = cli_class$error$Class)
  if (!is.StandardScale(scale) && !is.list(scale))
    cli_abort("Object of class {.cls StandardScale} or list of such objects needs to be provided to {.var scale} argument.",
              class = cli_class$error$Class)
  if (is.StandardScale(scale)) {
    scales <- list(scale)
  } else {
    areScales <- all(sapply(scale, is.StandardScale))
    if (!isTRUE(areScales)) 
      cli_abort("List provided to {.var scale} argument should contain only {.cls StandardScale} objects.",
                class = cli_class$error$Class)
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
  
  class(output) <- c("ScoreTable", if(is.Simulated(x)) "Simulated")
  return(output)
  
}


#' @title Create GroupedScoreTable
#' @param table `GroupedFrequencyTable` object
#' @param scale a `StandardScale` object or list of multiple `StandardScale` objects
#' @seealso plot.GroupedScoreTable
#' @return `GroupedScoreTable` object, which consists of named `list` of 
#' `ScoreTable` objects and `GroupConditions` object used for grouping
#' @importFrom cli cli_abort
#' @export

GroupedScoreTable <- function(table,
                              scale) {
  
  if (!is.GroupedFrequencyTable(table))
    cli_abort("Object of class {.cls GroupedFrequencyTable} needs to be provided to {.var table} argument",
              class = cli_class$error$Class)
  
  if (!is.StandardScale(scale) && !is.list(scale))
    cli_abort("Object of class {.cls StandardScale} or list of such objects needs to be provided to {.var scale} argument.",
              class = cli_class$error$Class)
  if (is.StandardScale(scale)) {
    scales <- list(scale)
  } else {
    areScales <- all(sapply(scale, is.StandardScale))
    if (!isTRUE(areScales)) 
      cli_abort("List provided to {.var scale} argument should contain only {.cls StandardScale} objects.",
                class = cli_class$error$Class)
    scales <- scale
  }
  
  STs <- list()
  
  for (i in seq_along(table)) {
    
    STs[[names(table)[i]]] <-
      ScoreTable(table[[i]], scale)
  
  }
  
  attr(STs, "conditions") <- attr(table, "conditions")
  attr(STs, "scales") <- STs[[1]]$scale
  attr(STs, "all") <- isTRUE(attr(table, "all"))
  
  class(STs) <- c("GroupedScoreTable", if(is.intersected(table)) "Intersect")
  
  return(STs)
}

#' @param x A `GroupedScoreTable` object
#' @param ... further arguments passed to or from other methods.
#' @rdname GroupedScoreTable
#' @importFrom cli cli_text cli_ol cli_ul cli_li cli_end
#' @export

print.GroupedScoreTable <- function(x, ...) {
  
  cli_text("{.cls GroupedScoreTable}")
  cli_text("{.field No. groups}: {.val {length(x)}}")
  cli_text("{.field GroupConditions}: {.val {length(attr(x, 'conditions'))}}")
  ol <- cli_ol()
  for (cond in attr(x, "conditions")) {
    cli_li("{.strong Category}: {attr(cond, 'cond_category')}")
    ul <- cli_ul()
    cli_li("{.field Tested vars}: {.val {attr(cond, 'formula_vars')}}")
    cli_li("{.field No. groups:}: {.val {length(attr(cond, 'groups'))}}")
    cli_end(ul)
  }
  cli_end(ol)
  cli_text("{.field {.bold .all} groups} included: {.val {isTRUE(attr(x, 'all'))}}")
  cli_text("Attached {.cls StandardScale} object(s):")
  ul <- cli_ul()
  for (scale in attr(x, "scales"))
    cli_li("{.strong {scale$name}}: {.var M}: {.val {scale$M}}; {.var SD}: {.val {scale$SD}}; {.var min}: {.val {scale$min}}; {.var max}: {.val {scale$max}}")
  cli_end(ul)
  
  
}

#' @title Gerenic plot of the GroupedScoreTable
#' @description Generic plot using `ggplot2`. It plots ScoreTables for all 
#' groups by default, or only chosen ones using when `group_names` argument is specified. 
#' @param x A `GroupedScoreTable` object
#' @param scale_name if scores for multiple scales available, provide the name
#' of the scale for plotting.
#' @param group_names names specifying which groups should appear in the plots
#' @param strict_names If `TRUE`, then intersected groups are filtered
#' using *strict* strategy: `group_names` need to be provided in form: `"group1:group2"`. If
#' `FALSE`, then intersected groups will be taken into regard separately, so 
#' eg. when `"group1"` is provided to `group_names`, all of: `"group1:group2"`, 
#' `"group1:group3"`, `"group1:groupN"`  will be plotted. Defaults to `TRUE`
#' @param plot_grid boolean indicating if the [ggplot2::facet_grid()] should be used.
#' If `FALSE`, then [ggplot2::facet_wrap()] is used. If groups are not intersected,
#' then it will be ignored and `facet_wrap` will be used.
#' @param ... named list of additional arguments passed to `facet` function.
#' @importFrom cli cli_abort
#' @export
plot.GroupedScoreTable <- function(
    x, 
    scale_name = NULL,
    group_names = NULL,
    strict_names = TRUE,
    plot_grid = is.intersected(x),
    ...
) {
  
  rlang::check_installed("ggplot2")
  
  if (!is.null(group_names)) {
    if (isTRUE(strict_names)){
      if (!any(group_names %in% names(x)))
        cli_abort("Not all names specified in {.var group_names} specify group names.",
                  class = cli_class$error$WrongGroup)
    } else {
      all_names <- unique(unlist(strsplit(names(x), split = ":")))
      if (!any(group_names %in% all_names))
        cli_abort("Not all names specified in {.var group_names} specify group names.",
                  class = cli_class$error$WrongGroup)
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
        cli_abort("Provide one of the computed scale names ({.val {names(x$scale)}}) to the {.var scale_name} argument.",
                  class = cli_class$error$NoScale)
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
  
  if ("group2" %in% names(plot_data) && isTRUE(plot_grid)) {
    
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
    
    if (is.intersected(x)) {
      
      plot_data$group1 <- factor(plot_data$group1, 
                                 levels = c(if (attr(x, "all")) ".all1", attr(attr(x, "conditions")[[1]], "groups")))
      plot_data$group2 <- factor(plot_data$group2, 
                                 levels = c(if (attr(x, "all")) ".all2", attr(attr(x, "conditions")[[2]], "groups")))
      plot_data$group1 <- paste(plot_data$group1, plot_data$group2, sep = ":")
      
    } else {
      
      plot_data$group1 <- factor(plot_data$group1, 
                                 levels = c(if (attr(x, "all")) ".all", attr(attr(x, "conditions")[[1]], "groups")))
    }
    
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
