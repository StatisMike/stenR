#' Internal items summing for one ScaleSpec
#' @param ScaleSpec object of ScaleSpec or CombScaleSpec class
#' @param data Data for all operations
#' @param warn_env Environment for warnings
#' @return data.frame of items reversed, NA inputted and summed to scale
#' @keywords internal
#' @noRd

items_summing <- function(spec, data, warn_env) {
  
 if (is.CombScaleSpec(spec)) {
    
    comb_scale <- lapply(spec$ScaleSpecs, \(single_spec) {
      
      single_scale <- items_summing(single_spec, data, warn_env)
      
      if (single_spec$name %in% spec$reverse && is.ScaleSpec(single_spec)) {
        single_scale <- 
          (single_spec$min * length(single_spec$item_names) + 
             single_spec$max * length(single_spec$item_names)) - single_scale
      }
      
      if (single_spec$name %in% spec$reverse && is.CombScaleSpec(single_spec)) {
        single_scale <- single_spec$min + single_spec$max - single_scale
      }
      
      return(single_scale)
      
    })
    summed_scale <- data.frame(item = as.integer(rowSums(dplyr::bind_cols(comb_scale))))
    names(summed_scale) <- spec$name
    return(summed_scale)
  }
  
  # if any of the item names is not available in data, skip the scale
  if (any(!spec$item_names %in% names(data))) {
    warn_env[["not_summed"]] <- c(warn_env[["not_summed"]], spec$name)
    return(NULL)
  }
  
  scale_items <- data[, spec$item_names, drop = F]
  
  # regular reversing of items
  if (!is.null(spec$reverse)) {
    scale_items[, spec$reverse] <-
      spec$max + spec$min - data[, spec$reverse]
  }
  
  # handling NAs with functional strategies
  if (spec$na_strategy != "asis") {
    
    values_by_row <- lapply(1:nrow(scale_items), \(i) {
      
      items_to_process <- scale_items[i, ]
      
      items_with_NAs <- mapply(is.na, items_to_process)
      
      if (sum(!items_with_NAs) <= 2)
        warn_env[["not_enough"]] <- warn_env[["not_enough"]] + 1
      
      else {
        
        if (spec$na_strategy == "mode") {
          freq <- table(as.numeric(items_to_process[, !items_with_NAs]))
          freq <- freq[freq == max(freq)]
          if (length(freq) != 1) {
            warn_env[["mode"]] <- warn_env[["mode"]] + 1
            na_val <- NA
          } else 
            na_val <- as.numeric(freq)
        }
        
        if (spec$na_strategy == "mean") 
          na_val <- round(mean(as.numeric(items_to_process[, !items_with_NAs])))
        
        if (spec$na_strategy == "median")
          na_val <- round(stats::median(as.numeric(items_to_process[, !items_with_NAs])))
        
        # input NA value in chosen strategy
        items_to_process[, items_with_NAs] <- na_val
        
      }
      
      return(items_to_process)
      
    })
    
    scale_items <- dplyr::bind_rows(values_by_row)
    
  } 
  
  # handling NAs in literal strategy
  # custom NA values if any are available
  if (!is.null(spec$na_value_custom)) {
    na_values_custom <- lapply(names(spec$na_value_custom), \(v) {
      
      vals <- scale_items[, v, drop = F]
      vals[is.na(vals)] <- 
        spec$na_value_custom[v]
      return(vals)
      
    })
    scale_items_custom <- dplyr::bind_cols(na_values_custom)
  }
  
  # default NA values
  scale_items <- scale_items[, !names(scale_items) %in% names(spec$na_value_custom), drop = F] 
  scale_items[is.na(scale_items)] <- spec$na_value
  
  if (!is.null(spec$na_value_custom)) {
    scale_items <- dplyr::bind_cols(scale_items, scale_items_custom)
  }
  
  # sum items to scale
  scale_summed <- data.frame(item = as.integer(rowSums(scale_items)))
  names(scale_summed) <- spec$name
  return(scale_summed)
  
}

#' @title Scale Specification object
#' @description Object containing scale or factor specification data. It describes
#' the scale or factor, with regard to which items from the source data are part
#' of it, which need to be summed with reverse scoring, and how to handle `NA`s.
#' To be used with [sum_items_to_scale()] function to preprocess item data.
#' @param name character with name of the scale/factor
#' @param item_names character vector containing names of the items that the
#' scale/factor consists of. 
#' @param min,max integer containing the default minimal/maximal value that the
#' answer to the item can be scored as. 
#' @param reverse character vector containing names of the items that need to be
#' reversed during scale/factor summing. Reversed using the default `"min"` and
#' `"max"` values.
#' @param na_strategy character vector specifying which strategy should be taken
#' during filling of `NA`. Defaults to `"asis"` and, other options are `"mean"`, 
#' `"median"` and `"mode"`. Strategies are explained in the details section. 
#' @param na_value integer value to be input in missing values as default.
#' Defaults to `as.integer(NA)`.
#' @param na_value_custom if there are any need for specific questions be gives
#' specific values in place of `NA`s, provide a named integer vector there. Names
#' should be the names of the questons.
#' @details 
#' 
#' ## NA imputation
#' it specifies how `NA` values should be treated during [sum_items_to_scale()]
#' function run.
#' **asis** strategy is literal: the values specified in `na_value` or `na_value_custom` 
#' will be used without any changes.
#' **mean**, **median** and **mode** are functional strategies. They work on a 
#' rowwise basis, so the appropriate value for every observation will be used.
#' If there are no values provided to check for the *mean*, *median* or *mode*,
#' the value provided in `na_value` or `na_value_custom` will be used. The 
#' values of *mean* and *median* will be rounded before imputation. 
#' 
#' ## Order of operations
#' - item reversion
#' - functional `NA`s imputation
#' - literal `NA`s imputation  
#' 
#' @return object of `ScaleSpec` class
#' @example man/examples/ScaleSpec.R
#' @family item preprocessing functions
#' @rdname ScaleSpec
#' @importFrom cli cli_abort
#' @export
#' 
ScaleSpec <- function(
    name,
    item_names,
    min,
    max,
    reverse = character(0),
    na_strategy = c("asis", "mean", "median", "mode"),
    na_value = as.integer(NA),
    na_value_custom) {
  
  na_strategy <- match.arg(na_strategy)
  
  if (min >= max)
    cli_abort("{.var min} needs to be smaller than {.var max}",
              class = cli_class$error$WrongMinMax)
  if (min < 0 || max < 0)
    cli_abort("Only non-negative {.var min} and {.var max} are supported.",
              class = cli_class$error$WrongMinMax)
  if (!is.character(reverse))
    cli_abort("For {.var reverse} argument a character vector should be specified.",
              class = cli_class$error$Type)
  
  out <- list(
    name = name,
    item_names = item_names,
    min = min,
    max = max,
    reverse = character(0),
    na_strategy = na_strategy,
    na_value = na_value)
  
  class(out) <- "ScaleSpec"
  
  if (length(reverse) > 0) {
    
    out[["reverse"]] <- reverse
    
    rev_missing <- out[["reverse"]][!out[["reverse"]] %in% out[["item_names"]]]
    
    if (length(rev_missing) > 0)
      cli_abort("There are some item names specified in {.var reverse} that are missing from {.var item_names}: {.val {rev_missing}}.",
                class = cli_class$error$NoValidVars)
  }
  
  if (!missing(na_value_custom)) {
    if (any(sapply(names(na_value_custom), \(x) is.null(x))) || !is.numeric(na_value_custom))
      cli_abort("Character vector provided to {.var na_value_custom} needs to be named.",
                 class = cli_class$error$Class)
    
    na_value_names_missing <- names(na_value_custom)
    na_value_names_missing <- na_value_names_missing[!na_value_names_missing %in% item_names]
    
    if (length(na_value_names_missing) > 0)
      cli_abort("There are some item names specified in {.var na_value_custom} that are missing from {.var item_names}: {.val {na_value_names_missing}}.",
                class = cli_class$error$NoValidVars)
    
    out[["na_value_custom"]] <- na_value_custom
  }
  
  return(out)
  
}

#' @rdname ScaleSpec
#' @param x a `ScaleSpec` object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli cli_text
#' @export
print.ScaleSpec <- function(x, ...) {
  
  cli({
    cli_text("{.cls ScaleSpec}: {.strong {x$name}}")
    cli_text(paste0("{.field No. items}: {.val {length(x$item_names)}}"),
             if (length(x$reverse) > 0) " [{.val {length(x$reverse)}} reversed]")
  })
  
}

#' @rdname ScaleSpec
#' @param object a `ScaleSpec` object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_text cli_ol cli_li cli_end 
#' @return data.frame of item names, if they are reversed, and custom NA value if available, invisibly
#' @export
summary.ScaleSpec <- function(object, ...) {
  
  items <- data.frame(
    item_name = object$item_names,
    reversed = sapply(object$item_names, \(name) name %in% object$reverse),
    custom_na = sapply(object$item_names, \(name) if (is.null(object$custom_na[[name]])) NA 
                                                else object$custom_na[[name]])
  )
  
  row.names(items) <- NULL
  
  cli_text("{.cls ScaleSpec}: {.strong {object$name}}")
  cli_text("{.strong min}: {.val {object$min}}; {.strong max}: {.val {object$max}}")
  cli_text("{.field NA imputation method}: {.val {object$na_strategy}}")
  cli_text("{.field NA literal value}: {.val {object$na_value}}")
  ol <- cli_ol()
  for (item_i in seq_along(items$item_name))
    cli_li(
      paste0(
        "{.emph {items$item_name[item_i]}}",
        ifelse(items$reversed[item_i], " {.bold reversed}", ""),
        ifelse(!is.na(items$custom_na[item_i]), " {.field custom NA}: {.val items$custom_na}", "")
      )
    )
  cli_end(ol)
  
  return(invisible(items))
}

#' @title Combined Scale Specification
#' @description
#' Combine multiple `ScaleSpec` objects into one in regards of [sum_items_to_scale()]
#' function. Useful when one scale of factor contains items of different possible
#' values or if there is hierarchy of scale or factors.
#' 
#' Also allows combining `CombScaleSpec` object if the factor structure have deeper
#' hierarchy.
#' 
#' @param name Name of the combined scale or factor
#' @param ... `ScaleSpec` or other `CombScaleSpec` objects
#' @param reverse character vector containing names of the underlying subscales
#' or factors that need to be reversed
#' @family item preprocessing functions
#' @return `CombScaleSpec` object
#' @example man/examples/CombScaleSpec.R
#' @rdname CombScaleSpec
#' @importFrom cli cli_abort
#' @export
CombScaleSpec <- function(name, ..., reverse = character(0)) {
  
  out <- list(name = name,
              ScaleSpecs = list(...),
              reverse = reverse)
  
  class(out) <- "CombScaleSpec"
  
  if (any(sapply(out$ScaleSpecs, \(x) !is.ScaleSpec(x) && !is.CombScaleSpec(x))))
    cli_abort("Objects of class {.cls ScaleSpec} or {.cls CombScaleSpec} need to be provided in {.var ...} argument.",
              class = cli_class$error$Class)
  if (!is.character(reverse))
    cli_abort("For {.var reverse} argument a character vector should be specified.",
              class = cli_class$error$Type)
  else if (length(reverse) > 0 && any(!reverse %in% sapply(out$ScaleSpecs, \(spec) spec$name)))
    cli_abort("Not all names provided in {.val reverse} argument are refering to the names of provided objects.",
              class = cli_class$error$NoScale)

  out[["item_names"]] <- lapply(out$ScaleSpecs, \(spec) {
    spec$item_names
  })
  
  out[["item_names"]] <- unlist(out[["item_names"]])
  
  out[["min"]] <- sapply(out$ScaleSpecs, \(spec) {
    if (is.ScaleSpec(spec))
      spec$min * length(spec$item_names)
    else if (is.CombScaleSpec(spec))
      spec$min
  })
  
  out[["min"]] <- sum(out[["min"]])
  
  out[["max"]] <- sapply(out$ScaleSpecs, \(spec) {
    if (is.ScaleSpec(spec))
      spec$max * length(spec$item_names)
    else if (is.CombScaleSpec(spec))
      spec$max
  })
  
  out[["max"]] <- sum(out[["max"]])
  
  return(out)
  
} 

#' @rdname CombScaleSpec
#' @param x a *CombScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_text cli_ol cli_end
#' @export
print.CombScaleSpec <- function(x, ...) {
  
    cli_text("{.cls CombScaleSpec}: {.strong x$name}")
    cli_text("{.field Total items}: {.val {length(x$item_names)}}")
    cli_text("{.strong Underlying objects}:")
    ol <- cli_ol()
    for (spec in x$ScaleSpecs)
      cli_li("{.cls {class(spec)}} {.strong {spec$name}} [{.field No.items}: {.val {length(spec$item_names)}}]")
    cli_end(ol)

}

#' @rdname CombScaleSpec
#' @param object a *CombScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_text cli_ol cli_li cli_ul cli_end  
#' @export
summary.CombScaleSpec <- function(object, ...) {
  
  suppressMessages(
    items_ls <- lapply(object$ScaleSpec, summary)
  )
  
  items_ls <- list()
  
  cli_text("{.cls CombScaleSpec}: {.strong {object$name}}")
  cli_text("{.strong Underlying objects}:")
  
  ol <- cli_ol()
  for (spec in object$ScaleSpecs) {
    cli_li("{.strong {spec$name}}")
    ul <- cli_ul()
    cli_li("{.field Class}: {.cls {class(spec)}}")
    cli_li("{.field Items}: {.val {spec$item_names}}")
    if (length(spec$reverse) > 0)
      cli_li("{.field Reversed}: {.val {spec$reverse}}")
    cli_end(ul)
  }
  cli_end(ol)
  
  names(items_ls) <- sapply(object$ScaleSpecs, \(spec) spec$name)
  items_df <- dplyr::bind_rows(items_ls, .id = "scale")
  
  return(invisible(items_df))
  
}

#' @title Sum up discrete raw data
#' @description Helper function to sum-up and - if needed - automatically 
#' reverse discrete raw item values to scale or factor that they
#' are measuring.
#' @details All summing up of the raw discrete values into scale or factor
#' score is done according to provided specifications utilizing [ScaleSpec()]
#' objects. For more information refer to their constructor help page.
#' @param data `data.frame` object containing numerical values of items data
#' @param ... objects of class `ScaleSpec` or `CombScaleSpec`. If all item names
#' are found in `data`, summed items will be available in returned data.frame
#' as column named as their `name` value.
#' @param retain either `boolean`: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none, or character vector with names of columns to be retained
#' @param .dots `ScaleSpec` or `CombScaleSpec` objects provided as a list, instead 
#' of individually in `...`. 
#' @return object of class `data.frame`
#' @example man/examples/sum_items_to_scale.R
#' @family item preprocessing functions
#' @importFrom cli cli_abort cli_warn
#' @export
sum_items_to_scale <- function(
    data,
    ...,
    retain = FALSE,
    .dots = list()) {
  
  if (length(.dots) != 0) 
    ScaleSpecs <- .dots
  else
    ScaleSpecs <- list(...)
  
  if (any(sapply(ScaleSpecs, \(x) !is.ScaleSpec(x) && !is.CombScaleSpec(x))))
    cli_abort("Objects of class {.cls ScaleSpec} or {.cls CombScaleSpec} need to be provided in {.var ...} argument.",
              class = cli_class$error$Class)
  
  if (length(ScaleSpecs) == 0)
    cli_abort("There should be at least one object provided in {.var ...} or {.var .dots} arguments.",
              class = cli_class$error$Class)
  
  if (!is.logical(retain) && !is.character(retain)) 
    cli_abort("{.var retain} argument should be a {.emph character} vector or {.emph boolean} value.",
              class = cli_class$error$Type)

  if (is.character(retain)) {
    retain_missing <- retain[!retain %in% names(data)]
    
    if (length(retain_missing) > 0)
      cli_abort("There are some variables specified in {.var retain} that are not available in the {.var data}: {.val {retain_missing}}",
                class = cli_class$error$NoValidVars)
  }
  
  warn_env <- new.env()
  warn_env[["not_summed"]] <- c()
  warn_env[["not_enough"]] <- 0
  warn_env[["mode"]] <- 0
  
  summed_scales <- lapply(ScaleSpecs, items_summing, data = data, warn_env = warn_env)
  
  if (length(warn_env$not_summed) > 0)
    cli_warn("Some of the scales were not summed: not all specified items were available in the data: {.val {warn_env$not_summed}}",
             class = cli_class$warning$NotSummed)
  
  if (warn_env$not_enough > 0)
    cli_warn("Functional {.emph NA imputations} weren't done for {.val {warn_env$not_enough}} observations, because less than 2 {.emph non-NA} values were available.",
             class = cli_class$warning$NoInputNA)
  
  if (warn_env$mode > 0)
    cli_warn("Function {.strong mode} {.emph NA imputations} weren't done for {.val {warn_env$mode}} observations, because of polimodals.",
             class = cli_class$warning$NoInputNA)

  if (isTRUE(retain)) {
    out <- dplyr::bind_cols(data,
                            summed_scales)
  } else if (isFALSE(retain)) {
    out <- dplyr::bind_cols(summed_scales)
  } else {
    out <- dplyr::bind_cols(data[, retain, drop = F],
                            summed_scales)
  }
  
  return(out)
  
}
