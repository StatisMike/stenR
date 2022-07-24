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