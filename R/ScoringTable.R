#### INTERNAL, UNUSED ####

#' @title Pivot wide ScoringTable csv into longer
#' @description Pivot wide ScoringTable into longer table with raw scores 
#' as the first column to the left
#' @param scores numeric vector of standardized scores
#' @param raws character vector of numeric raw scores separated by `split`.
#' @param name character name of the scores names
#' @noRd  

scores_to_long <- function(scores, raws, name, split = ":") {
  
  if (length(scores) != length(raws)) {
    stop("Uneven scores and raws lengths")
  }
  
  sc_tbl <- lapply(seq_along(scores), \(i) {
    
    raw_sc <- as.numeric(unlist(strsplit(raws[i], split = split)))
    data.frame(raw = min(raw_sc):max(raw_sc), 
               score = scores[i])
  })
  
  sc_tbl <- dplyr::bind_rows(sc_tbl)
  names(sc_tbl)[2] <- name
  
  return(sc_tbl)
  
}

#' @title Create ScoringTable from shortened table
#' @param short_st Short ScoringTable to make longer
#' @param x_group Horizontal groups (colnames)
#' @param y_group Vertical groups (column values)
#' @param standard_scores name of the column containing the standard scores
#' @noRd

create_st <- function(short_st,
                      x_group = "Group_1",
                      # y_group = "Group_2",
                      standard_scores = "sten") {
  
  raw_table <- short_st
  
  # check the x_group if it is available
  if (!is.null(x_group)) {
    x_pattern_prefix <- paste0("^", x_group, ":")
    x_pattern <- paste0(x_pattern_prefix, "\\w{1,}")
    # check if there are any correctly formatted group_x:value names
    x_ind <- which(grepl(names(raw_table), pattern = x_pattern))
    if (length(x_ind) == 0)
      stop(paste0("There are no correctly formatted columns with values of '", x_group, "'.",
                  " Correctly formatted column names with 'x_group' should follow the pattern:\n",
                  "'x_group:val1', 'x_group:val2', 'x_group:valN'"))
    
    x_vals <- gsub(names(raw_table)[x_ind], pattern = x_pattern_prefix, replacement = "")
    if (any(nchar(x_vals) == 0))
      stop(paste0("There are some uncorrectly provided values to the 'x_group': '", x_group, "'.",
                  " Values of 'x_group' should consists of alphanumerics characters only.",
                  " Correctly formatted column names with 'x_group' should follow the pattern:\n",
                  "'x_group:val1', 'x_group:val2', 'x_group:valN'"))
    
  }
  
  ss_ind <- which(grepl(names(raw_table), pattern = paste0("^", standard_scores, "$")))
  
  if (length(ss_ind) != 1)
    stop(paste0("There is not exactly one column named '", standard_scores, "' (standard_scores)."))
  
  for (x_val_i in x_ind) {
    
    i_scores <- scores_to_long(
      scores = raw_table[[standard_scores]],
      raws = raw_table[[x_val_i]],
      name = names(raw_table)[x_val_i],
      split = "-"
    )
    
    if (x_val_i == min(x_ind))
      out_score <- i_scores
    else 
      out_score <- dplyr::left_join(
        out_score, i_scores, by = "raw"
      )
  }
  
  return(out_score)
}

#### EXPORTED ####

#' @title Create ScoringTable
#' @description ScoringTable is a simple version of *ScoreTable* or *GroupedScoreTable*,
#' that don't include the *FrequencyTable* internally. It can be easily
#' saved to `csv` or `json` using [export_ScoringTable()] and loaded from these
#' files using `import_ScoringTable`.
#' 
#' When using *GroupedScoreTable*, the columns will be named the same as the
#' name of group. If it was created using two *GroupCondition* object, the names
#' of columns will be names of the groups seperated by `:`
#' @param table *ScoreTable* or *GroupedScoreTable* object
#' @param ... additional arguments 
#' @return *ScoringTable*
#' @export

to_ScoringTable <- function(table, ...) {
  
  UseMethod("to_ScoringTable", table)
  
}

#' @rdname to_ScoringTable
#' @param scale *character* name of the scale attached in `table`. If only one
#' scale is attached, it can be left as default `NULL`
#' @param min_raw *numeric* absolute minimum score that can be received. If left
#' as default `NULL`, the minimum available in the data will be used.
#' @param max_raw *numeric* absolute maximum score that can be received. If left
#' as default `NULL`, the maximum available in the data will be used.
#' @param score_colname Name of the column containing the raw scores
#' @export

to_ScoringTable.ScoreTable <- function(
    table, scale = NULL, min_raw = NULL, max_raw = NULL, score_colname = "Score", ...) {
  
  if (is.null(scale) && length(table$scales) != 1)
    stop("If 'ScoreTable' have multiple scales attached, provide 'scale' argument.")
  else if (length(table$scales == 1)) 
    scale <- names(table$scales)
  
  score_values <- table$table[[scale]] |> unique()
  
  raws <- sapply(score_values, \(score_val) {
    
    min_val <- if(score_val == min(score_values) && !is.null(min_raw)) min_raw
    else min(table$table[table$table[[scale]] == score_val, "score"])
    max_val <- if(score_val == max(score_values) && !is.null(max_raw)) max_raw
    else max(table$table[table$table[[scale]] == score_val, "score"])
    
    paste(min_val, max_val, sep = "-")
    
  })
  
  out <- data.frame(ss = score_values,
                    Score = raws)
  
  names(out)[1] <- scale
  names(out)[2] <- score_colname
  attr(out, "grouped") <- FALSE
  class(out) <- c("ScoringTable", class(out))
  return(out)
  
}

#' @rdname to_ScoringTable
#' @export

to_ScoringTable.GroupedScoreTable <- function(
    table, scale = NULL, min_raw = NULL, max_raw = NULL, ...) {
  
  tables <- mapply(to_ScoringTable, 
                   table = table, 
                   score_colname = names(table), 
                   MoreArgs = list(min = min_raw, 
                                   max = max_raw, 
                                   scale = scale), 
                   SIMPLIFY = F)
  
  out <- Reduce(\(x, y) dplyr::full_join(x, y, by = scale), tables)
  out <- out[order(out[[1]]), ]
  
  attr(out, "grouped") <- TRUE
  class(out) <- c("ScoringTable", "data.frame")
  attr(out, "conditions") <- attr(table, "conditions")
  return(out)
  
}

#' @title Export ScoringTable
#' @description After creation of *ScoringTable* it can be handy to export it
#' into universally recognized and readable format. Two formats are currently
#' supported: `csv` and `json`. They can be imported back into *ScoringTable*
#' using `import_ScoreTable` function.
#' 
#' - `csv` format is more universally readable - it can be opened, edited
#' and altered (eg. before publication) in any spreadsheet manager. In case of 
#' *ScoringTable* created from *GroupedScoreTable*, *GroupConditions* can
#' be exported to another `csv` file, creating two different files.
#' - `json` format can be more obtuse, but it allows export of both 
#' *ScoringTable* itself and *GroupConditions* in the same `json` file.
#' @param table A *ScoringTable* object to export
#' @param out_file Output file
#' @param method Method for export, either `csv` or `json`
#' @param conditions_file Output file for *GroupConditions*. Necessary only
#' if `method = csv`.
#' @export

export_ScoringTable <- function(table,
                                out_file,
                                method = c("csv", "json"),
                                conditions_file) {
  
  if (!is.ScoringTable(table))
    stop("Object of class 'ScoringTable', need to be provided in 'table' argument")
  
  method <- match.arg(method)
  
  cond <- attr(table, "conditions")
  
  if (method == "csv" && !is.null(cond) && missing(conditions_file))
    warning("'GroupConditions' haven't been exported. To export them with 'csv' method, ",
            "please provide the 'conditions_file' argument.")
  
  if (method == "json" && !requireNamespace("jsonlite", quietly = T))
    stop("To use 'json' exporting method, package 'jsonlite' needs to be installed.")
  
  switch(method,
         csv = {
           utils::write.csv(table, file = out_file, row.names = F)
           if (!is.null(cond) && !missing(conditions_file)) {
             cond_ls <- lapply(cond, as.data.frame.GroupConditions)
             cond_df <- dplyr::bind_rows(cond_ls)
             utils::write.csv(cond_df, conditions_file, row.names = F)
           } else if (!is.null(attr(table, "conditions"))) 
             message("Exported ScoringTable based on GroupedScoreTable without exporting conditions.")
         },
         json = {
           out <- list(ScoringTable = table)
           if (!is.null(cond)) {
             
             out[["GroupConditions"]] <- 
               lapply(cond, \(x) 
                      stats::setNames(as.list(attr(x, "conditions")), 
                               nm = attr(x, "groups")))
             
             names(out[["GroupConditions"]]) <- sapply(cond, \(x) attr(x, "cond_category"))
           }
           
           jsonlite::write_json(out, out_file)
         })
}

#' @title Import ScoringTable
#' @description ScoringTable can be imported from `csv` or `json` file
#' into R object. Source file can be either an output of [export_ScoringTable()]
#' function, or created by hand - though it needs to be created following the
#' correct format.
#' @param source_file Path to the file to import the *ScoringTable* from
#' @param method Method for import, either `csv` or `json`
#' @param cond_file File to import the *GroupConditions* from, if using `csv` method
#' @param conditions If no conditions file is provided (when using `csv` method)
#' or the existing conditions (when using `json` method) are to be overwritten,
#' provide *GroupCondition* object or list of up to two of them here.
#' @return ScoringTable object
#' @export

import_ScoringTable <- function(
    source_file,
    method = c("csv", "json"),
    cond_file,
    conditions) {
  
  method <- match.arg(method)
  
  out <- switch(method,
                
                csv = {
                  st_read <- utils::read.table(source_file, sep = ",")
                  st_df <- st_read[-1, ]
                  names(st_df) <- as.character(st_read[1, ])
                  rownames(st_df) <- NULL
                  out <- list(st = st_df)
                  
                  if (ncol(st_df) > 2) {
                    
                    # priority: handle conditions provided
                    if (!missing(conditions)) {
                      if (is.GroupConditions(conditions))
                        conditions <- list(conditions)
                      gc_df <-  dplyr::bind_rows(
                        lapply(conditions, as.data.frame.GroupConditions))
                      
                    } else if (!missing(cond_file)) {
                      gc_df <- utils::read.csv(cond_file)
                    } else {
                      stop("When importing ScoringTable with groups, provide either 'cond_file' or 'conditions' arguments")
                    }
                    out[["gc"]] <- gc_df
                  }
                  
                  out
                },
                
                json = {
                  
                  st_read <- jsonlite::read_json(source_file, simplifyVector = T)
                  st_df <- st_read[["ScoringTable"]]
                  st_df <- as.data.frame(dplyr::bind_rows(st_df))
                  
                  
                  if (!is.null(st_read[["GroupConditions"]]) && ncol(st_read[["ScoringTable"]]) > 2) {
                    gc_df <- lapply(st_read[["GroupConditions"]], \(cond) {
                      data.frame(group = names(cond), condition = unlist(cond), row.names = NULL)
                    })
                    
                    out <- list(st = st_df,
                                gc = dplyr::bind_rows(gc_df, .id = "category"))
                    
                  } else if (ncol(st_read[["ScoringTable"]]) == 2){
                    
                    out <- list(st = st_df)
                    
                  }
                  
                  # if conditions are provided, overwrite them
                  if (!missing(conditions) && ncol(st_read[["ScoringTable"]]) > 2) {
                    if (is.GroupConditions(conditions))
                      conditions <- list(conditions)
                    out[["gc"]] <- dplyr::bind_rows(
                      lapply(conditions, as.data.frame.GroupConditions))
                    
                  }
                  
                  out
                })
  
  st_out <- out[["st"]]
  if (!is.null(out[["gc"]])) {
    attr(st_out, "grouped") <- TRUE
    attr(st_out, "conditions") <- verify_GC_for_ST(out[["st"]], out[["gc"]])
  } else
    attr(st_out, "grouped") <- FALSE
  
  class(st_out) <- c("ScoringTable", class(st_out))
  return(st_out)
  
}

#' Internal function to verify the provided conditions with conditions available
#' in imported *ScoringTable*
#' @param st_df data.frame form of ScoreTable
#' @param gc_df data.frame form of GroupConditions
#' @return list of *GroupConditions* objects or `NULL`
#' @keywords internal

verify_GC_for_ST <- function(st_df, gc_df) {
  
  group_cols <- names(st_df)[-1]
  
  if (length(group_cols) == 1)
    return(NULL)
  
  splitted <- strsplit(group_cols, ":")
  is_intersected <- all(sapply(splitted, \(x) length(x)) == 2)
  
  if (is_intersected && length(unique(gc_df$category)) != 2)
    stop("Imported ScoringTable seems to be based upon intersected GroupConditions, ",
         "but only one GroupCondition is provided")
  
  groups <- lapply(unique(gc_df$category), \(x) {
    out <- list(category = x,
                which = sapply(splitted, \(y) which(y %in% gc_df$group[gc_df$category == x])) |>
                  unique() |> unlist())
    
    out[["groups"]] <- unique(sapply(splitted, \(z) z[out$which]))
    out[["conditions"]] <- gc_df$condition[gc_df$category == x]
    return(out)
  })
  
  if (isTRUE(is_intersected)) {
    
    valid_groups <- isTRUE(
      all(group_cols %in% as.character(sapply(groups[[1]]$groups, \(x) paste(x, groups[[2]]$groups, sep = ":"))))
    )
    
  } else {
    
    if (length(unique(gc_df$category)) != 1)
      stop("Imported ScoringTable seems to be based upon one GroupConditions, ",
           "but two GroupConditions object are provided")
    
    valid_groups <- isTRUE(
      all(group_cols %in% groups[[1]]$groups)
    )
  }
  
  if (!isTRUE(valid_groups))
    stop("Provided GroupConditions are not compatible with imported ScoringTable")
  
  out <- lapply(groups, \(x) {
    
    conds <- paste0("'", x$groups[!grepl(x = x$groups, pattern = "^\\.")], 
                    "' ~ ", x$conditions)
    conds <- lapply(conds, stats::as.formula)
    
    GroupConditions(conditions_category = x$category,
                    .dots = conds)
    
  })
  
  return(out)
  
}