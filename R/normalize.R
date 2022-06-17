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
#' - name of the scale calculated in *ScoreTable* provided to `table` argument 
#' 
#' @example examples/normalize_score.R
#' @return Numeric vector with values specified in `what` argument
#' @family score-normalization functions
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
#' - name of the scale calculated in *ScoreTables* provided to `...` or
#' `.dots` argument
#' 
#' @param retain either *boolean*: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none, or *character vector* with names of columns to be retained
#' @param .dots *ScoreTable* or *FrequencyTable* objects provided as a list, 
#' instead of individually in `...`. 
#' @example examples/normalize_scores_df.R
#' @export
#' @family score-normalization functions
#' @return *data.frame* with normalized scores
#' 

normalize_scores_df <- function(
    data,
    vars,
    ...,
    what,
    retain = FALSE,
    .dots = list()) {
  
  if (!is.data.frame(data))
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
    out <- dplyr::bind_cols(data[, retain, drop = F],
                            normalized)
  }
  
  return(out)
  
}

#' @title Normalize scores using GroupedFrequencyTables or GroupedScoreTables
#' @description Normalize scores using either *GroupedFrequencyTable* or
#' *GroupedScoreTable* for one or more variables. Given *data.frame* should also
#' contain columns used in *GroupingConditions* attached to the table
#' @param data *data.frame* object containing raw scores
#' @param vars *character vector* with names of columns to normalize. Length of vars
#' need to be the same as number of tables provided to either `...` or `.dots`
#' @param ... *GroupedFrequencyTable* or *GroupedScoreTable* objects to be used 
#' for normalization. They should be provided in the same order as `vars`
#' @param what the values to get. One of either:
#' 
#' - `quan` - the quantile of x in the raw score distribution
#' - `Z` - normalized Z score for the x raw score
#' - name of the scale calculated in *GroupedScoreTables* provided to `...` or
#' `.dots` argument
#' 
#' @param retain either *boolean*: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none, or *character vector* with names of columns to be retained
#' @param group_col *character* name of the column for name of the group each
#' observation was qualified into. If left as default `NULL`, they won't be returned.
#' @param .dots *GroupedFrequencyTable* or *GroupedScoreTable* objects provided 
#' as a list, instead of individually in `...`. 
#' @export
#' @example /examples/normalize_scores_grouped.R
#' @family score-normalization functions
#' @return *data.frame* with normalized scores

normalize_scores_grouped <- function(
    data,
    vars,
    ...,
    what,
    retain = FALSE,
    group_col = NULL,
    .dots = list()) {
  
  if (!is.data.frame(data))
    stop("'data.frame' need to be provided to 'data' argument.")
  if (!is.character(vars))
    stop("Character vector need to be provided to 'vars' argument.")
  if (any(!vars %in% names(data)))
    stop("All 'vars' need to be available in the 'data'.")
  if (!is.logical(retain) && !(is.character(retain) && all(retain %in% names(data))))
    stop("Bool value or character vector containing column names available in 'data' need to be provided to 'retain' argument.")
  if (!is.null(group_col) && (!is.character(group_col) || length(group_col) != 1))
    stop("One character value need to be passed to 'group_col'")
  
  tables <- list(...)
  if (length(tables) == 0 && length(.dots) > 0)
    tables <- .dots
  
  if (!all(sapply(tables, \(x) class(x) %in% c("GroupedFrequencyTable", "GroupedScoreTable"))))
    stop("All objects provided to '...' or '.dots' need to be of class 'GroupedFrequencyTable' or 'GroupedScoreTable'")
  
  if (length(vars) != length(tables))
    stop("Number of provided tables and 'vars' to normalize need to be the same.")
  
  if (all(sapply(tables, \(x) class(x) == "GroupedScoreTable")) && !what %in% c("quan", "Z")) {
    if (!all(sapply(tables, \(x) what %in% names(attr(x, "scales")))))
      stop("Scale of the name provided in 'what' need to be available in all provided 'GroupedScoreTable' objects.")
  } else if (!what %in% c("quan", "Z"))
    stop("'what' argument can be one of: 'quan', 'Z' or name of the scale in provided 'GroupedScoreTable' objects.")
  
  if (".temp_GroupAssignment_index" %in% names(data))
    stop("Column name '.temp_GroupAssignment_index'")
  
  # check if all conditions are the same
  conditions <- lapply(tables, attr, which = "conditions")
  equal_comb <- all(sapply(conditions[-1], \(cond) identical(conditions[[1]], cond)))
  if (!isTRUE(equal_comb))
    stop("All ", class(tables[[1]]), " objects need to be created on the basis of the same 'GroupConditions'.")
  # keep only one conditions
  conditions <- conditions[[1]]
  
  # add temporary index to return the data in correct order
  data[[".temp_GroupAssignment_index"]] <- paste(1:nrow(data), "index", sep = "_")
  
  # qualify observations to correct group
  # handle conditions to be intersected
  if (length(conditions) == 2) {
    group_indices <- intersect_GroupAssignment(
      GA1 = GroupAssignment(data = data,
                            conditions = conditions[[1]],
                            force_disjoint = T,
                            force_exhaustive = T,
                            id = ".temp_GroupAssignment_index",
                            na_as_all = T),
      GA2 = GroupAssignment(data = data,
                            conditions = conditions[[2]],
                            force_disjoint = T,
                            force_exhaustive = T,
                            id = ".temp_GroupAssignment_index",
                            na_as_all = T))
    # handle one condition only
  } else {
    group_indices <- GroupAssignment(
      data = data,
      conditions = conditions[[1]],
      force_disjoint = T,
      force_exhaustive = T,
      id = ".temp_GroupAssignment_index",
      na_as_all = T)
  }
  
  groups <- extract_observations(
    data,
    groups = group_indices,
    id = ".temp_GroupAssignment_index"
  )
  
  normalized_all_groups <- lapply(seq_along(groups), \(i) {
    
    if (nrow(groups[[i]]) == 0)
      return(NULL)
    
    group_name <- names(groups)[i]
    
    group_tables <- lapply(tables, \(tbl) {
      tbl[[which(names(tbl) == group_name)]]
    })
    
    normalized_ingroup <- normalize_scores_df(
      data = groups[[i]],
      vars = vars,
      what = what,
      retain = if (isTRUE(retain)) TRUE
          else if (isFALSE(retain)) ".temp_GroupAssignment_index"
             else c(".temp_GroupAssignment_index", retain),
      .dots = group_tables
    )
    
    return(normalized_ingroup)
  })
  
  names(normalized_all_groups) <- names(groups)
  
  normalized_all_groups <- data.table::rbindlist(normalized_all_groups,
                                                 use.names = T,
                                                 idcol = group_col[1])
  
  normalized_all_groups <- normalized_all_groups[
    order(normalized_all_groups[[".temp_GroupAssignment_index"]]), 
    -which(names(normalized_all_groups) == ".temp_GroupAssignment_index")
  ]
  
  return(normalized_all_groups)
  
}