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
#' 
#' @example examples/normalize_score.R
#' @return Numeric vector with values specified in `what` argument
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