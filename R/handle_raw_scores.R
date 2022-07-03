#' Internal items summing for one ScaleSpec
#' @param ScaleSpec object of ScaleSpec or CombScaleSpec class
#' @param data Data for all operations
#' @param warn_env Environment for warnings
#' @return data.frame of items reversed, NA inputted and summed to scale
#' @keywords internal
#' @noRd

items_summing <- function(spec, data, warn_env) {
  
 if (class(spec) == "CombScaleSpec") {
    
    comb_scale <- lapply(spec$ScaleSpecs, \(single_spec) {
      
      single_scale <- items_summing(single_spec, data, warn_env)
      
      if (single_spec$name %in% spec$reverse && class(single_spec) == "ScaleSpec") {
        single_scale <- 
          (single_spec$min * length(single_spec$item_names) + 
             single_spec$max * length(single_spec$item_names)) - single_scale
      }
      
      if (single_spec$name %in% spec$reverse && class(single_spec) == "CombScaleSpec") {
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
#' reversed during scale/factor summing. Reversed using the default `min` and
#' `max` values.
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
#' @return object of *ScaleSpec* class
#' @example examples/ScaleSpec.R
#' @family item preprocessing functions
#' @rdname ScaleSpec
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
  
  if (!na_strategy[1] %in% c("asis", "mean", "median", "mode"))
    stop("The 'na_strategy' needs to be one of 'asis', 'mean', 'median' or 'mode'")
  
  if (min >= max)
    stop("'min' needs to be lesser than 'max'")
  if (min < 0 || max < 0)
    stop("Only non-negative 'min' and 'max' values are supported.")
  if (!is.character(reverse))
    stop("Character vector should be specified for 'reverse' argument.")
  
  out <- list(
    name = name,
    item_names = item_names,
    min = min,
    max = max,
    reverse = character(0),
    na_strategy = na_strategy[1],
    na_value = na_value)
  
  class(out) <- "ScaleSpec"
  
  if (length(reverse) > 0) {
    
    out[["reverse"]] <- reverse
    
    rev_missing <- out[["reverse"]][!out[["reverse"]] %in% out[["item_names"]]]
    
    if (length(rev_missing) > 0)
      stop(paste0("There are some item names specified in `reverse` that are missing from the `item_names`:\n",
                  "'", paste(rev_missing, sep = "', '"), "'."))
    
  }
  
  if (!missing(na_value_custom)) {
    if (any(sapply(names(na_value_custom), \(x) is.null(x))) || !is.numeric(na_value_custom))
      stop("Integer vector assigned to the `na_value_custom` should be named.")
    
    na_value_names_missing <- names(na_value_custom)
    na_value_names_missing <- na_value_names_missing[!na_value_names_missing %in% item_names]
    
    if (length(na_value_names_missing) > 0)
      stop(paste0("There are some item names specified in `na_value_custom` that are missing from the `item_names`:\n",
                  "'", paste(na_value_names_missing, sep = "', '"), "'."))
    
    out[["na_value_custom"]] <- na_value_custom
  }
  
  return(out)
  
}

#' @rdname ScaleSpec
#' @param x a *ScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @export
print.ScaleSpec <- function(x, ...) {
  
  cat(sep = "", "<ScaleSpec>: '", x$name, "'\n")
  cat(sep = "", "No. items: ", length(x$item_names))
  if (length(x$reverse) > 0) 
    cat(sep = "", " (", length(x$reverse), " reversed)")
  cat("\nNA imputation method:", x$na_strategy, "\n")
  cat("NA literal value:", x$na_value, "\n")
  
}

#' @rdname ScaleSpec
#' @param object a *ScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @export
summary.ScaleSpec <- function(object, ...) {
  
  cat(sep = "", "<ScaleSpec>: '", object$name, "'\n")
  
  cat(sep = "", "min: ", object$min, "; ", "max: ", object$max, "\n")
  
  cat("NA imputation method:", object$na_strategy, "\n")
  cat("NA literal value:", object$na_value, "\n\n")
  
  cat("Items:\n")
  invisible(lapply(object$item_names, \(item) {
    
    cat(item)
    if (item %in% object$reverse) cat(" <reversed>")
    if (item %in% names(object$custom_na)) cat(" <custom NA>: ", spec$custom_na[item])
    cat("\n")
    
  }))
  
  return(invisible(NULL))
  
}

#' @title Combined Scale Specification
#' @description
#' Combine multiple *ScaleSpec* objects into one in regards of [sum_items_to_scale()]
#' function. Useful when one scale of factor contains items of different possible
#' values or if there is hierarchy of scale or factors.
#' 
#' Also allows combining *CombScaleSpec* object, if the factor structure have deeper
#' hierarchy.
#' 
#' @param name Name of the combined scale or factor
#' @param ... *ScaleSpec* or other *CombScaleSpec* objects
#' @param reverse character vector containing names of the underlying subscales
#' of factors that need to be reversed
#' @family item preprocessing functions
#' @return *CombScaleSpec* object
#' @example examples/CombScaleSpec.R
#' @rdname CombScaleSpec
#' @export
CombScaleSpec <- function(name, ..., reverse = character(0)) {
  
  out <- list(name = name,
              ScaleSpecs = list(...),
              reverse = reverse)
  
  class(out) <- "CombScaleSpec"
  
  if (any(sapply(out$ScaleSpecs, \(x) !class(x) %in% c("ScaleSpec", "CombScaleSpec"))))
    stop("Objects of class 'ScaleSpec' or 'CombScaleSpec' need to be provided in '...' argument.")
  if (!is.character(reverse))
    stop("Character vector need to be provided to 'reverse' argument.")
  else if (length(reverse) > 0 && any(!reverse %in% sapply(out$ScaleSpecs, \(spec) spec$name)))
    stop("Some names provided in 'reverse' argument are reffering to the names of provided scales.")
  
  out[["item_names"]] <- lapply(out$ScaleSpecs, \(spec) {
    spec$item_names
  })
  
  out[["item_names"]] <- unlist(out[["item_names"]])
  
  out[["min"]] <- sapply(out$ScaleSpecs, \(spec) {
    if (class(spec) == "ScaleSpec")
      spec$min * length(spec$item_names)
    else if (class(spec) == "CombScaleSpec")
      spec$min
  })
  
  out[["min"]] <- sum(out[["min"]])
  
  out[["max"]] <- sapply(out$ScaleSpecs, \(spec) {
    if (class(spec) == "ScaleSpec")
      spec$max * length(spec$item_names)
    else if (class(spec) == "CombScaleSpec")
      spec$max
  })
  
  out[["max"]] <- sum(out[["max"]])
  
  return(out)
  
} 

#' @rdname CombScaleSpec
#' @param x a *CombScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @export
print.CombScaleSpec <- function(x, ...) {
  
  cat(sep = "", "<CombScaleSpec>: '", x$name, "'\n")
  cat(sep = "", "No. items total: ", length(x$item_names), "\n\n")
  cat("Underlying objects:\n")
  invisible(lapply(x$ScaleSpecs, \(y) {
    cat(sep = "", "<", class(y), ">: ", x$name)
    if (y$name %in% x$reverse) cat(" <reversed>")
    cat("\n")
  }))
  
}

#' @rdname CombScaleSpec
#' @param object a *CombScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @export
summary.CombScaleSpec <- function(object, ...) {
  
  cat(sep = "", "<CombScaleSpec>: '", object$name, "'\n")
  cat(sep = "", "No. items total: ", length(object$item_names), "\n\n")
  cat("Underlying objects:\n")
  invisible(lapply(object$ScaleSpecs, \(x) {
    cat("- ")
    print(x)
    cat("\n")
  }))
  
}

#' @title Sum up discrete raw data
#' @description Helper function to sum-up and - if needed - automatically 
#' reverse discrete raw item values to scale or factor that they
#' are measuring.
#' @details All summing up of the raw discrete values into scale or factor
#' score is done according to provided specifications utilizing [ScaleSpec()]
#' objects. For more information refer to their constructor help page.
#' @param data `data.frame` object containing numerical values of items data
#' @param ... objects of class *ScaleSpec*. If all item names for *ScaleSpec*
#' are found in `data`, summed items will be available in returned data.frame
#' as column named as the *ScaleSpec* `name` value.
#' @param retain either *boolean*: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none, or character vector with names of columns to be retained
#' @param .dots *ScaleSpec* objects provided as a list, instead of individually
#' in `...`. 
#' @return object of class *data.frame*
#' @example examples/sum_items_to_scale.R
#' @family item preprocessing functions
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
  
  if (any(sapply(ScaleSpecs, \(x) !class(x) %in% c("ScaleSpec", "CombScaleSpec"))))
    stop ("Objects of class `ScaleSpec` or 'CombScaleSpec' need to be provided in `...` argument")
  
  if (length(ScaleSpecs) == 0)
    stop ("There should be at least one `ScaleSpec` or 'CombScaleSpec' object provided in `...` argument")
  
  if (!is.logical(retain) && !is.character(retain)) 
    stop ("`retain` argument need to be either a character vector with column names to retain or boolean.")
  
  if (is.character(retain)) {
    retain_missing <- retain[!retain %in% names(data)]
    
    if (length(retain_missing) > 0)
      stop(paste0("There are some colnames specified in `retain` that are not in the data:\n",
                  "'", paste(retain_missing, sep = "', '"), "'."))
  }
  
  warn_env <- new.env()
  warn_env[["not_summed"]] <- c()
  warn_env[["not_enough"]] <- 0
  warn_env[["mode"]] <- 0
  
  summed_scales <- lapply(ScaleSpecs, items_summing, data = data, warn_env = warn_env)
  
  if (length(warn_env$not_summed) > 0) 
    warning(paste0("Some of the scales were not summed: not all specified items were available in data:\n",
                  paste(warn_env$not_summed, collapse = ", ")))
  
  if (warn_env$not_enough > 0)
    warning(paste0("Functional NA imputations weren't done for ",
                   warn_env$not_enough, 
                   " observations, because less than 2 non-NA values were available."))
  
  if (warn_env$mode > 0)
    warning(paste0("Functional 'mode' NA imputations weren't done for ",
                   warn_env$mode,
                   " observations, because polimodals were detected for observations."))
  
  
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
