#### EXPORTED ####

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
#' - name of the scale calculated in `ScoreTable` provided to `table` argument 
#' 
#' @example man/examples/normalize_score.R
#' @importFrom cli cli_abort
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
        cli_abort("Provide either {.val quan} or {.val Z} to the {.var what} argument.",
                  class = cli_class$error$NoValidWhat)
    },
    
    "ScoreTable" = {
      if (!what %in% c("quan", "Z", names(table$scale)))
        cli_abort("Provide either {.val quan}, {.val Z} or name of the computed scale ({.val {names(table$scale)}}) to the {.var what} argument.",
                  class = cli_class$error$NoValidWhat)
    },
    
    cli_abort("{.cls FrequencyTable} or {.cls ScoreTable} object needs to be provided to the {.var table} argument.",
              clss = cli_class$error$Class))
  
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
#' @description Wrapper for [normalize_score()] that works on data frame
#' and multiple variables
#' @param data `data.frame` containing raw scores
#' @param vars names of columns to normalize. Length of `vars`
#' need to be the same as number of tables provided to either `...` or `.dots`
#' @param ... `ScoreTable` or `FrequencyTable` objects to be used for normalization
#' @param what the values to get. One of either:
#' 
#' - `quan` - the quantile of x in the raw score distribution
#' - `Z` - normalized Z score for the x raw score
#' - name of the scale calculated in `ScoreTables` provided to `...` or
#' `.dots` argument
#' 
#' @param retain either boolean: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none; or character vector with names of columns to be retained
#' @param .dots `ScoreTable` or `FrequencyTable` objects provided as a list, 
#' instead of individually in `...`. 
#' @example man/examples/normalize_scores_df.R
#' @importFrom cli cli_abort
#' @export
#' @family score-normalization functions
#' @return `data.frame` with normalized scores
#' 

normalize_scores_df <- function(
    data,
    vars,
    ...,
    what,
    retain = FALSE,
    .dots = list()) {
  
  if (!is.data.frame(data))
    cli_abort("{.cls data.frame} need to be provided to {.var data}.",
              class = cli_class$error$Class)
  if (!is.character(vars))
    cli_abort("{.emph character vector} needs to be provided to {.var vars} argument",
              class = cli_class$error$Type)
  if (any(!vars %in% names(data)))
    cli_abort("All {.var vars} need to be available in the {.var data}.",
              class = cli_class$error$NoValidVars)
  if (!is.logical(retain) && !(is.character(retain) && all(retain %in% names(data))))
    cli_abort("{.emph Boolean value} or {.emph character vector} containing column names available in {.var data} need to be provided in {.var retain} argument.",
              class = cli_class$error$NoValidRetain)

  tables <- list(...)
  if (length(tables) == 0 && length(.dots) > 0)
    tables <- .dots
  
  if (any(sapply(tables, \(x) !is.FrequencyTable(x) && !is.ScoreTable(x))))
    cli_abort("All object provided to {.var ...} or {.var .dots} need to be a {.cls FrequencyTable} or {.cls ScoreTable}.",
              class = cli_class$error$Class)
  
  if (length(vars) != length(tables))
    cli_abort("Number of provided tables ({.val {length(tables}}) and {.var vars} to normalize ({.val {length(vars}}) need to be equal.",
              class = cli_class$error$WrongLength)
  
  if (all(sapply(tables, is.ScoreTable)) && !what %in% c("quan", "Z")) {
    if (!all(sapply(tables, \(x) what %in% names(x$scale))))
      cli_abort("Scale of the name provided in {.var what} need to be available in all provided {.cls ScoreTable} objects.",
                class = cli_class$error$NoValidWhat)
  } else if (!what %in% c("quan", "Z"))
    cli_abort("{.var what} argument can be one of {.val quan} or {.val Z}, or name of the scale if {.cls ScoreTable} objects are provided.",
              class = cli_class$error$NoValidWhat)
  
  normalized <- lapply(1:length(vars), \(i) {
    
    res <- data.frame(var = normalize_score(x = data[, vars[i]],
                                            table = tables[[i]],
                                            what = what))
    names(res) <- vars[i]
    return(res)
  })
  
  normalized <- dplyr::bind_cols(normalized)
  
  out <- handle_retain(data = data,
                       output = normalized,
                       retain = retain)
  
  return(out)
  
}

#' @title Normalize scores using GroupedFrequencyTables or GroupedScoreTables
#' @description Normalize scores using either `GroupedFrequencyTable` or
#' `GroupedScoreTable` for one or more variables. Given data.frame should also
#' contain columns used in `GroupingConditions` attached to the table
#' @param data data.frame object containing raw scores
#' @param vars names of columns to normalize. Length of vars
#' need to be the same as number of tables provided to either `...` or `.dots`
#' @param ... `GroupedFrequencyTable` or `GroupedScoreTable` objects to be used 
#' for normalization. They should be provided in the same order as `vars`
#' @param what the values to get. One of either:
#' 
#' - `quan` - the quantile of x in the raw score distribution
#' - `Z` - normalized Z score for the x raw score
#' - name of the scale calculated in `GroupedScoreTables` provided to `...` or
#' `.dots` argument
#' 
#' @param retain either boolean: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none; or character vector with names of columns to be retained
#' @param group_col name of the column for name of the group each
#' observation was qualified into. If left as default `NULL`, they won't be returned.
#' @param .dots `GroupedFrequencyTable` or `GroupedScoreTable` objects provided 
#' as a list, instead of individually in `...`. 
#' @export
#' @importFrom cli cli_abort
#' @example man/examples/normalize_scores_grouped.R
#' @family score-normalization functions
#' @return data.frame with normalized scores

normalize_scores_grouped <- function(
    data,
    vars,
    ...,
    what,
    retain = FALSE,
    group_col = NULL,
    .dots = list()) {
  
  if (!is.data.frame(data))
    cli_abort("{.cls data.frame} need to be provided to {.var data}.",
              class = cli_class$error$Class)
  if (!is.character(vars))
    cli_abort("{.emph character vector} needs to be provided to {.var vars} argument",
              class = cli_class$error$Type)
  if (any(!vars %in% names(data)))
    cli_abort("All {.var vars} need to be available in the {.var data}.",
              class = cli_class$error$NoValidVars)
  if (!is.logical(retain) && !(is.character(retain) && all(retain %in% names(data))))
    cli_abort("{.emph Boolean value} or {.emph character vector} containing column names available in {.var data} need to be provided in {.var retain} argument.",
              class = cli_class$error$NoValidRetain)
  if (!is.null(group_col) && (!is.character(group_col) || length(group_col) != 1))
    cli_abort("One {.emph character value} can be passed to {.var group_col}",
              class = cli_class$error$Type)
  
  tables <- list(...)
  if (length(tables) == 0 && length(.dots) > 0)
    tables <- .dots
  
  if (any(sapply(tables, \(x) !is.GroupedFrequencyTable(x) && !is.GroupedScoreTable(x))))
    cli_abort("All object provided to {.var ...} or {.var .dots} need to be a {.cls FrequencyTable} or {.cls ScoreTable}.",
              class = cli_class$error$Class)
  
  if (length(vars) != length(tables))
    cli_abort("Number of provided tables ({.val {length(tables}}) and {.var vars} to normalize ({.val {length(vars}}) need to be equal.",
              class = cli_class$error$WrongLength)
  
  if (all(sapply(tables, is.GroupedScoreTable)) && !what %in% c("quan", "Z")) {
    if (!all(sapply(tables, \(x) what %in% names(attr(x, "scales")))))
      cli_abort("Scale of the name provided in {.var what} need to be available in all provided {.cls GroupedScoreTable} objects.",
                class = cli_class$error$NoValidWhat)
  } else if (!what %in% c("quan", "Z"))
    cli_abort("{.var what} argument can be one of {.val quan} or {.val Z}, or name of the scale if {.cls GroupedScoreTable} objects are provided.",
              class = cli_class$error$NoValidWhat)
  
  if (".temp_GroupAssignment_index" %in% names(data))
    cli_abort("Column name: {.val .temp_GroupAssignment_index} is reserved for internal operations.",
              class = cli_class$error$BadName)
  
  # check if all conditions are the same
  conditions <- lapply(tables, attr, which = "conditions")
  equal_comb <- all(sapply(conditions[-1], \(cond) identical(conditions[[1]], cond)))
  if (!isTRUE(equal_comb))
    cli_abort("All grouped tables need to be created on the basis of the same {.cls GroupConditions}",
              class = cli_class$error$NoCompatibleAssignements)
  # keep only one conditions
  conditions <- conditions[[1]]
  
  # assing observations to groups
  groups <- qualify_to_groups(
    data = data,
    conditions = conditions
  )
  
  normalized_all_groups <- lapply(seq_along(groups), \(i) {
    
    if (nrow(groups[[i]]) == 0)
      return(NULL)
    
    group_name <- names(groups)[i]
    
    group_tables <- lapply(tables, \(tbl) {
      tbl_n <- which(names(tbl) == group_name)
      # safety measure if there are groups without their table
      if (length(tbl_n) == 0)
        mockNAtable(tbl[[1]])
      else
        tbl[[tbl_n]]
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
  
  out <- normalized_all_groups[
    order(normalized_all_groups[[".temp_GroupAssignment_index"]]), 
    -which(names(normalized_all_groups) == ".temp_GroupAssignment_index"),
    drop = FALSE
  ]
  
  return(out)
  
}

#' @title Normalize scores using ScoringTables
#' @description Normalize scores using either `ScoringTable` objects for one or 
#' more variables. Given data.frame should also contain columns used in 
#' `GroupingConditions` attached to the table (if any)
#' @param data data.frame containing raw scores
#' @param vars names of columns to normalize. Length of vars
#' need to be the same as number of tables provided to either `...` or `.dots`
#' @param ... `ScoringTable` objects to be used for normalization. They should 
#' be provided in the same order as `vars`
#' @param retain either boolean: `TRUE` if all columns in the `data` are to be
#' retained, `FALSE` if none; or names of columns to be retained
#' @param group_col name of the column for name of the group each
#' observation was qualified into. If left as default `NULL`, they won't be returned.
#' Ignored if no conditions are available
#' @param .dots `ScoringTable` objects provided as a list, instead of individually in `...`. 
#' @export
#' @importFrom cli cli_abort
#' @family score-normalization functions
#' @return data.frame with normalized scores

normalize_scores_scoring <- function(
    data,
    vars,
    ...,
    retain = FALSE,
    group_col = NULL,
    .dots = list()) {
  
  if (!is.data.frame(data))
    cli_abort("{.cls data.frame} need to be provided to {.var data}.",
              class = cli_class$error$Class)
  if (!is.character(vars))
    cli_abort("{.emph character vector} needs to be provided to {.var vars} argument",
              class = cli_class$error$Type)
  if (any(!vars %in% names(data)))
    cli_abort("All {.var vars} need to be available in the {.var data}.",
              class = cli_class$error$NoValidVars)
  if (!is.logical(retain) && !(is.character(retain) && all(retain %in% names(data))))
    cli_abort("{.emph Boolean value} or {.emph character vector} containing column names available in {.var data} need to be provided in {.var retain} argument.",
              class = cli_class$error$NoValidRetain)
  if (!is.null(group_col) && (!is.character(group_col) || length(group_col) != 1))
    cli_abort("One {.emph character value} can be passed to {.var group_col}",
              class = cli_class$error$Type)
  
  tables <- list(...)
  if (length(.dots) > 0)
    tables <- .dots
  
  if (!all(sapply(tables, is.ScoringTable)))
    cli_abort("All object provided to {.var ...} or {.var .dots} need to be a {.cls ScoringTable}.",
              class = cli_class$error$Class)
  
  if (length(vars) != length(tables))
    cli_abort("Number of provided tables ({.val {length(tables}}) and {.var vars} to normalize ({.val {length(vars}}) need to be equal.",
              class = cli_class$error$WrongLength)
  
  if (".temp_GroupAssignment_index" %in% names(data))
    cli_abort("Column name: {.val .temp_GroupAssignment_index} is reserved for internal operations.",
              class = cli_class$error$BadName)
  
  # check if all conditions are the same
  conditions <- lapply(tables, attr, which = "conditions")
  
  if (!all(sapply(lapply(tables, attr, which = "conditions"), \(x) is.null(x)))) {
    equal_comb <- all(sapply(conditions[-1], \(cond) identical(conditions[[1]], cond)))
    if (!isTRUE(equal_comb))
      cli_abort("All {.cls ScoringTable} objects need to be created on the basis of the same {.cls GroupConditions}",
                class = cli_class$error$NoCompatibleAssignements)
    # keep only one conditions
    conditions <- conditions[[1]]
  } else {
    conditions <- NULL
  }
  
  if (is.null(conditions)) {
    
    normalized_variables <- lapply(seq_along(vars), \(i_v) {
      val_name = vars[i_v]

        out_var <- 
        sapply(data[[val_name]], \(x) 
               check_score_between(
                 x = x,
                 col_raw = tables[[i_v]][[2]],
                 col_score = tables[[i_v]][[1]]))
      
      out <- data.frame(var = out_var)
      names(out) <- val_name
      return(out)
    })
    
    out_data <- dplyr::bind_cols(normalized_variables)

  } else {
    
    groups <- qualify_to_groups(
      data = data,
      conditions = conditions
    )
    
    normalized_all_groups <- lapply(seq_along(groups), \(i_g) {
      
      if (nrow(groups[[i_g]]) == 0)
        return(NULL)
      
      group_name <- names(groups)[i_g]
      
      group_variables <- lapply(seq_along(vars), \(i_v) {
        
        val_name = vars[i_v]
        col_n <- which(names(tables[[i_v]]) == group_name)
        # safety measure if there are groups without their table
        if (length(col_n) == 0)
          out_var <- rep(as.numeric(NA))
        else
          out_var <- 
          sapply(groups[[i_g]][[val_name]], \(x) 
                 check_score_between(
                   x = x,
                   col_raw = tables[[i_v]][[col_n]],
                   col_score = tables[[i_v]][[1]]))
        
        out <- data.frame(var = out_var)
        names(out) <- val_name
        return(out)
      })
      
      binded <- 
        dplyr::bind_cols(
          c(list(groups[[i_g]][, ".temp_GroupAssignment_index", drop = FALSE]),
            group_variables)
        )
      
      return(binded)
    })
    
    names(normalized_all_groups) <- names(groups)
    
    out_data <- data.table::rbindlist(normalized_all_groups,
                                      use.names = T,
                                      idcol = group_col[1]) |>
      as.data.frame()
    
    out_data <- out_data[order(out_data[[".temp_GroupAssignment_index"]]), 
                         -which(names(out_data) == ".temp_GroupAssignment_index"),
                         drop = FALSE]
    
  }
  
  out_data <- handle_retain(data = data,
                            output = out_data,
                            retain = retain)
  
  rownames(out_data) <- NULL
  
  return(out_data)

}

#### INTERNAL ####

#' @title Qualify observations to groups for normalization
#' @description Function used internally in [normalize_scores_grouped()] and
#' [normalize_scores_scoring()]
#' @param data Data with observations to group by
#' @param conditions GroupConditions passed to normalization function
#' @keywords internal

qualify_to_groups <- function(data,
                              conditions) {
  # add temporary index to return the data in correct order
  data[[".temp_GroupAssignment_index"]] <- 1:nrow(data)
  
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
  
  return(groups)
  
}

#' @title Pick up standarized value from ScoringTable 
#' @description Internal function for picking up the standardized value from 
#' ScoringTable for single observation. [normalize_scores_scoring()]
#' @param x raw score
#' @param col_raw Column of raw scores from ScoringTable
#' @param col_score COlumn of StandardScale score from ScoringTable
#' @importFrom cli cli_abort
#' @keywords internal

check_score_between <- function(x, col_raw, col_score) {
  
  if (length(x) != 1)
    cli_abort("{.var x} need to be {.strong one raw score value}",
              class = cli_class$error$Type)

  if (is.na(x))
    return(as.numeric(NA))
  
  x <- as.numeric(x)
  
  splitscore <- strsplit(col_raw, split = "-")
  splitscore <- lapply(splitscore, as.numeric)
  score_n <- which(sapply(splitscore, \(score) x %in% score[1]:score[2]))
  
  if (length(score_n) < 1) {
    if (x < min(unlist(splitscore)))
      score_n <- 1
    else  if (x > max(unlist(splitscore)))
      score_n <- length(splitscore)
  }
  
  return(col_score[score_n])
  
}

#' Handle retain columns
#' @description Internal function that handles *retain* columns in non-base
#' normalize_scores functions
#' @param data input data
#' @param output output data.frame
#' @param retain retain value
#' @keywords internal

handle_retain <- function(data, output, retain) {
  
  if (isFALSE(retain))
    out <- output
  else if (isTRUE(retain)) {
    data[, names(output)] <- output
    out <- data
  } else {
    data[, names(output)] <- output
    out <- data[, c(retain, names(output))]
  }
     
  return(out)

}

