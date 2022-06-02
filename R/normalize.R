#' Normalize raw scores
#' 
#' @description Use computed `FrequencyTable` or `ScoreTable` to normalize the
#' provided raw scores.
#' 
#' @param x vector of raw scores to normalize
#' @param table `FrequencyTable` or `ScoreTable` object 
#' @param what the values to get. One of either:
#' 
#' - `quan` - the quantile of x in the raw score distribution
#' - `Z` - normalized Z score for the x raw score
#' - name of the scale calculated in `StandardScale` provided to `x` argument 
#' 
#' @example examples/normalize_score.R
#' @return Numeric vector with values specified in `what` argument
#' @seealso [normalize_scores_df()]
#' 
#' @export

normalize_score <- function(
  x, 
  table,
  what) {
  
  switch(
    class(table)[1],
    "FrequencyTable" = {
      if (!what %in% c("quan", "Z")) 
        stop("Provide either 'quan' or 'Z' to the 'what' argument.")},
    
    "ScoreTable" = {
      if (!what %in% c("quan", "Z", names(table$scale)))
        stop("Provide either 'quan', 'Z' or name of the computed scale to the 'what' argument.")},
    
    stop("Object of class 'FrequencyTable' or 'ScoreTable' needs to be provided to the 'table' argument.") )
  
  sapply(as.integer(x), \(x) {
    
    if (is.na(x)) {
      score <- NA
    } else if (x > max(table$table$score)){
      score <- max(table$table[[what]])
    } else if (x < min(table$table$score)){
      score <- min(table$table[[what]])
    } else {
      score <- table$table[table$table$score == x, ][[what]]
    }
    return(score)
  })
  
}

#' Normalize raw scores for multiple variables
#' @description Wrapper for [normalize_score] that works on data frame
#' and multiple variables
#' @param data *data.frame* object containing raw scores
#' @param vars *character vector* with names of columns to normalize. Length of vars
#' need to be the same as number of tables provided to either `...` or `.dots`
#' @param ... *ScoreTable* or *FrequencyTable* objects to be used for normalization
#' @param what the values to get. One of either:
#' 
#' - `quan` - the quantile of x in the raw score distribution
#' - `Z` - normalized Z score for the x raw score
#' - name of the scale calculated in `StandardScale`s provided to `...` or
#' `.dots` argument
#' 
#' @param retain either *boolean*: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none, or *character vector* with names of columns to be retained
#' @param .dots *ScoreTable* or *FrequencyTable* objects provided as a list, 
#' instead of individually in `...`. 
#' @example examples/normalize_scores_df.R
#' @export
#' @seealso [normalize_score()]
#' @return *data.frame* with normalized scores
#' 

normalize_scores_df <- function(
    data,
    vars,
    ...,
    what,
    retain = FALSE,
    .dots = list()) {
  
  if (!"data.frame" %in% class(data))
    stop("'data.frame' need to be provided to 'data' argument.")
  if (!is.character(vars))
    stop("Character vector need to be provided to 'vars' argument.")
  if (any(!vars %in% names(data)))
    stop("All 'vars' need to be available in the 'data'.")
  if (!is.logical(retain) && !(is.character(retain) && all(retain %in% names(data))))
    stop("Bool value or character vector containing column names available in 'data' need to be provided to 'retain' argument.")
  
  tables <- list(...)
  if (length(tables) == 0 && length(.dots) > 0)
    tables <- .dots
  
  if (!all(sapply(tables, \(x) class(x) %in% c("FrequencyTable", "ScoreTable"))))
    stop("All objects provided to '...' or '.dots' need to be of class 'FrequencyTable' or 'ScoreTable'")
  
  if (length(vars) != length(tables))
    stop("Number of provided tables and 'vars' to normalize need to be the same.")
  
  if (all(sapply(tables, \(x) class(x) == "ScoreTable")) && !what %in% c("quan", "Z")) {
    if (!all(sapply(tables, \(x) what %in% names(x$scale))))
      stop("Scale of the name provided in 'what' need to be available in all provided 'ScoreTable' objects.")
  } else if (!what %in% c("quan", "Z"))
    stop("'what' argument can be one of: 'quan', 'Z' or name of the scale in provided 'ScoreTable' objects.")
  
  normalized <- lapply(1:length(vars), \(i) {
    
    res <- data.frame(var = normalize_score(x = data[, vars[i]],
                                            table = tables[[i]],
                                            what = what))
    names(res) <- vars[i]
    return(res)
  })
  
  normalized <- dplyr::bind_cols(normalized)
  
  if (isTRUE(retain)) {
    data[, vars] <- normalized
    out <- data
  } else if (isFALSE(retain))
    out <- normalized
  else {
    out <- dplyr::bind_cols(data[, retain],
                            normalized)
  }
  
  return(out)
  
}