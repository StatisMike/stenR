#' @title Scale Specification object
#' @description Object containing scale or factor specification data. It describes
#' the scale or factor, with regard to which items from the source data are part
#' of it, which need to be summed with reverse scoring, and how to handle `NA`s.
#' To be used with [sum_items_to_scale()] function to preprocess item data.
#' @param name character with name of the scale/factor
#' @param item_names character vector containing names of the items that the
#' scale/factor consists of. 
#' @param reverse character vector containing names of the items that need to be
#' reversed during scale/factor summing. Reversed using the default `min` and
#' `max` values.
#' @param min,max integer containing the default minimal/maximal value that the
#' answer to the item can be scored as. Needed only if there are any names
#' presented in `reverse`.
#' @param na_value integer value to be input in missing values as default.
#' Defaults to `as.integer(NA)`.
#' @param reverse_custom if there are any differently-scored items in need of
#' reverse scoring, their specification need to be given there as a *data.frame*
#' containing columns: `item_names`, `min`, `max` and, optionally, `na_value`.
#' @return object of *ScaleSpec* class
#' @example examples/handle_raw_scores.R
#' @family item preprocessing functions
#' @export
#' 
ScaleSpec <- function(
    name,
    item_names,
    reverse,
    min,
    max,
    na_value = as.integer(NA),
    reverse_custom) {
  
  out <- list(
    name = name,
    item_names = item_names)
  
  class(out) <- "ScaleSpec"
  
  if (!missing(reverse)) {
    
    if (missing(min) || missing(max))
      stop("If `reverse` contains any item names, the `min` and `max` arguments need to be specified")
    
    out[["reverse"]] <- reverse
    
    rev_missing <- out[["reverse"]][!out[["reverse"]] %in% out[["item_names"]]]
    
    if (length(rev_missing) > 0)
      stop(paste0("There are some item names specified in `reverse` that are missing from the `item_names`:\n",
                  "'", paste(rev_missing, sep = "', '"), "'."))
    
    out[["min"]] <- min
    out[["max"]] <- max
    
  }
  
  out[["na_value"]] <- na_value
  
  if (!missing(reverse_custom)) {
    
    if (!"data.frame" %in% class(reverse_custom))
      stop("Object of class 'data.frame' need to be provided in 'reverse_custom' argument.")
    
    if (!all(c("item_names", "min", "max") %in% names(reverse_custom)))
      stop("Columns 'item_names', 'min' and 'max' need to be specified in object provided in 'reverse_custom' argument.")
    
    rev_cus_missing <- reverse_custom$item_names[!reverse_custom$item_names %in% out[["item_names"]]]
    
    if (length(rev_cus_missing) > 0)
      stop(paste0("There are some item names specified in `reverse_custom` that are missing from the `item_names`:\n",
                  "'", paste(rev_cus_missing, sep = "', '"), "'."))
    
    out[["custom_reverse"]] <-
      data.frame(item_names = reverse_custom$item_name,
                 min = reverse_custom$min,
                 max = reverse_custom$max,
                 na_value = if (is.null(reverse_custom$na_value)) as.integer(NA) 
                          else reverse_custom$na_value
      )
    
  }
  
  return(out)
  
}
# #' @rdname ScaleSpec
# #' @param ss *ScaleSpec* object
# summary.ScaleSpec <- function(ss) {
#   
#   
#   
# }

#' @title Sum up discrete raw data
#' @description Helper function to sum-up and - if needed - automatically 
#' reverse discrete raw data. 
#' @param data `data.frame` object containing numerical values of items data
#' @param ... objects of class *ScaleSpec*. If all item names for *ScaleSpec*
#' are found in `data`, summed items will be available in returned data.frame
#' as column named as the *ScaleSpec* `name` value.
#' @param id_col character describing which column should be retained as
#' observation id. If kept as default `NULL`, then no column will be retained.
#' @param .dots *ScaleSpec* objects provided as a list, instead of individually
#' in `...`. 
#' @return object of class *data.frame*
#' @example examples/handle_raw_scores.R
#' @family item preprocessing functions
#' @export
sum_items_to_scale <- function(
    data,
    ...,
    id_col = NULL,
    .dots) {
  
  if (!missing(.dots)) 
    ScaleSpacs <- .dots
  else
    ScaleSpecs <- list(...)
  
  if (any(sapply(ScaleSpecs, \(x) class(x) != "ScaleSpec")))
    stop ("Objects of class `ScaleSpec` need to be provided in `...` argument")
  
  if (length(ScaleSpecs) == 0)
    stop ("There should be at least one `ScaleSpec` object provided in `...` argument")
  
  if (!is.null(id_col)) {
    id_obs <- data.frame(id = data[[id_col]])
    names(id_obs) <- id_col
  } else 
    id_obs <- NULL
  
  # sum scales
  summed_scales <- lapply(ScaleSpecs, \(spec) {
    
    # if any of the item names is not available in data, skip the scale
    if (any(!spec$item_names %in% names(data)))
      return(NULL)
    
    # every item need to be handled separately, depending of the item specification
    scale_items <- lapply(spec$item_names, \(item) {
      
      # if item is supposed to be reversed customly
      if (item %in% spec$custom_reverse$item_names) {
        
        custom_spec <- spec$custom_reverse[spec$custom_reverse$item_names == item, ]
        item_vals <- custom_spec$max + custom_spec$min - data[, item, drop = F]
        item_vals[is.na(item_vals)] <- custom_spec$na_value
        return(item_vals)
        
        # if item is to be reversed as default
      } else if (item %in% spec$reverse) {
        
        item_vals <- spec$max + spec$min - data[, item, drop = F]
        item_vals[is.na(item_vals)] <- spec$na_value
        return (item_vals)
        
        # if no reverse will be made
      } else {
        
        item_vals <- data[, item, drop = F]
        item_vals[is.na(item_vals)] <- spec$na_value
        return(item_vals)
        
      }
    })
    
    scale_items <- dplyr::bind_cols(scale_items)
    scale_summed <- data.frame(item = rowSums(scale_items))
    names(scale_summed) <- spec$name
    return(scale_summed)
    
  })
  
  return(
    dplyr::bind_cols(list(id_obs),
                     summed_scales)
  )
  
}