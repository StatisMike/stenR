#### INTERNAL, UNUSED ####
## nocov start

#' @title Pivot wide ScoringTable csv into longer
#' @description Pivot wide ScoringTable into longer table with raw scores 
#' as the first column to the left
#' @param scores numeric vector of standardized scores
#' @param raws character vector of numeric raw scores separated by `split`.
#' @param name character name of the scores names
#' @importFrom cli cli_abort
#' @noRd  

scores_to_long <- function(scores, raws, name, split = ":") {
  
  if (length(scores) != length(raws)) {
    cli_abort("Uneven {.var scores}: {.val {length(scores)}} and {.var raws}: {.val {length(raws)}}",
              "WrongLengthError")
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
    cli_abort("There is not exactly one column named {.val {standard_scores}}.")
  
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

## nocov end

#### EXPORTED ####

#' @title Create ScoringTable
#' @description ScoringTable is a simple version of [ScoreTable()] or [GroupedScoreTable()],
#' that don't include the `FrequencyTable` internally. It can be easily
#' saved to `csv` or `json` using [export_ScoringTable()] and loaded from these
#' files using [import_ScoringTable()].
#' 
#' When using `GroupedScoreTable`, the columns will be named the same as the
#' name of group. If it was created using two `GroupCondition` object, the names
#' of columns will be names of the groups seperated by `:`
#' @param table `ScoreTable` or `GroupedScoreTable` object
#' @param ... additional arguments 
#' @return `ScoringTable` object
#' @rdname ScoringTable
#' @aliases to_ScoringTable
#' @export

to_ScoringTable <- function(table, ...) {
  
  UseMethod("to_ScoringTable", table)
  
}

#' @rdname ScoringTable
#' @param scale name of the scale attached in `table`. If only one
#' scale is attached, it can be left as default `NULL`
#' @param min_raw,max_raw absolute minimum/maximum score that can be received. 
#' If left as default `NULL`, the minimum/maximum available in the data will be 
#' used.
#' @param score_colname Name of the column containing the raw scores
#' @importFrom cli cli_abort
#' @aliases to_ScoringTable
#' @example man/examples/ScoringTable_ungrouped.R
#' @export

to_ScoringTable.ScoreTable <- function(
    table, scale = NULL, min_raw = NULL, max_raw = NULL, score_colname = "Score", ...) {
  
  if (is.null(scale) && length(table$scale) != 1)
    cli_abort("{.cls ScoreTable} have multiple scales attached: {.val {names(table$scale)}}. Provide one of these names to {.var scale}.",
              class = "ScaleNonunequivocalError")
  else if (is.null(scale) && length(table$scale) == 1) 
    scale <- names(table$scale)
  
  else if (!scale %in% names(table$scale)) 
      cli_abort("{.var scale}: {.val {scale}} is not attached. Choose one of {.val {names(table$scale)}}",
                class = "WrongScaleError")
    
  
  
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

#' @rdname ScoringTable
#' @aliases to_ScoringTable
#' @example man/examples/ScoringTable_grouped.R
#' @export

to_ScoringTable.GroupedScoreTable <- function(
    table, scale = NULL, min_raw = NULL, max_raw = NULL, ...) {
  
  if (is.null(scale) && length(table[[1]]$scale) != 1)
    cli_abort("{.cls ScoreTable} have multiple scales attached: {.val {names(table[[1]]$scale)}}. Provide one of these names to {.var scale}.",
              class = "ScaleNonunequivocalError")
  else if (is.null(scale) && length(table[[1]]$scale) == 1) 
    scale <- names(table[[1]]$scale)
    
  else if (!scale %in% names(table[[1]]$scale)) {
      cli_abort("{.var scale}: {.val {scale}} is not attached. Choose one of {.val {names(table$scale)}}",
                class = "WrongScaleError")
    scale <- names(table$scale)
  }
  
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
  attr(out, "conditions") <- attr(table, "conditions")
  attr(out, "all") <- isTRUE(attr(table, "all"))
  class(out) <- c("ScoringTable", if (is.intersected(table)) "Intersect", "data.frame")
  return(out)
  
}

#' @title Export ScoringTable
#' @description After creation of `ScoringTable` it can be handy to export it
#' into universally recognized and readable format. Two formats are currently
#' supported: *csv* and *json*. They can be imported back into `ScoringTable`
#' using [import_ScoringTable()] function.
#' 
#' - *csv* format is universally readable - it can be opened, edited
#' and altered (eg. before publication) in any spreadsheet editor. In case of 
#' `ScoringTable` created from `GroupedScoreTable`, `GroupConditions` can
#' be exported to another *csv* file, creating two different files.
#' - *json* format can be more obtuse, but it allows export of both 
#' `ScoringTable` itself and `GroupConditions` in the same *json* file.
#' @param table A `ScoringTable` object to export
#' @param out_file Output file. Ignored if `method = "object"`
#' @param method Method for export, either `"csv"`, `"json"` or `"object"`
#' @param cond_file Output file for `GroupConditions`. Used only
#' if `method = csv` and `table` created with `GroupedScoreTable`.
#' @seealso import_ScoringTable
#' @importFrom cli cli_abort cli_warn
#' @example man/examples/import_export_ScoringTable.R
#' @return list containing `ScoringTable` as a `tibble` and `GroupConditions` 
#' if `method = "object"`. `NULL` for other methods
#' @family import/export functions
#' @export

export_ScoringTable <- function(table,
                                out_file,
                                method = c("csv", "json", "object"),
                                cond_file) {
  
  if (!is.ScoringTable(table))
    cli_abort("Object of class {.cls ScoringTable} need to be provided in {.var table}.")
  
  method <- match.arg(method)
  
  cond <- attr(table, "conditions")
  
  if (method == "json")
    rlang::check_installed("jsonlite")

  switch(method,
         csv = {
           utils::write.csv(table, file = out_file, row.names = F)
           if (!is.null(cond) && !missing(cond_file)) {
             cond_ls <- lapply(cond, as.data.frame.GroupConditions)
             cond_df <- dplyr::bind_rows(cond_ls)
             utils::write.csv(cond_df, cond_file, row.names = F)
           } else if (!is.null(attr(table, "conditions"))) 
             cli_warn("{.cls GroupConditions} haven't been exported. To export them with {.emph csv method}, please provide the {.var cond_file} argument",
                      class = "NonExportedConditionsWarning")
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
         },
         object = {
           out <- list(ScoringTable = dplyr::as_tibble(table))
           if (!is.null(cond)) {
             
             out[["GroupConditions"]] <- 
               lapply(cond, \(x) 
                      attr(x, "conditions"))
             
             names(out[["GroupConditions"]]) <- sapply(cond, \(x) attr(x, "cond_category"))
             
             return(out)
           }
         })
}

#' @title Import ScoringTable
#' @description `ScoringTable` can be imported from `csv`, `json` file or
#' `tibble`. Source file or object can be either an output of [export_ScoringTable()]
#' function, or created by hand - though it needs to be created following the
#' correct format.
#' @param source Path to the file to import the `ScoringTable` from (for *csv* and *json* methods)
#' or `ScoringTable` in form of `data.frame` (for *object* method)
#' @param method Method for import, either *csv*, *json* or *object*
#' @param cond_file File to import the `GroupConditions` from, if using *csv* method
#' @param conditions `GroupCondition` object or list of up to two of them. Mandatory
#' for *object* method and *csv* method if no `cond_file` is provided. If provided
#' while using *json* method, original `GroupConditions` will be ignored.  
#' @importFrom cli cli_abort
#' @seealso export_ScoringTable
#' @example man/examples/import_export_ScoringTable.R
#' @return `ScoringTable` object
#' @family import/export functions
#' @export

import_ScoringTable <- function(
    source,
    method = c("csv", "json", "object"),
    cond_file,
    conditions) {
  
  method <- match.arg(method)
  
  if (method %in% c("csv", "json") && (!is.character(source) || length(source) != 1 || !file.exists(source)))
    cli_abort("With {.code method = {.val {method}}} the {.var source} needs to be a path to the existing file.",
              class = cli_class$error$WrongSourceError)
  
  if (method == "object" && inherits(source, "tibble"))
    cli_abort("With {.code method = {.val object}} the {.var source} needs to be a {.cls tibble}.",
              class = cli_class$error$WrongSourceError)
  
  switch(method,
         
         csv = {
           st_read <- utils::read.table(source, sep = ",")
           st_df <- st_read[-1, ]
           names(st_df) <- as.character(st_read[1, ])
           st_df[[1]] <- as.numeric(st_df[[1]])
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
               cli_abort("When importing {.cls ScoringTable} with groups, provide either {.var cond_file} or {.var conditions} arguments",
                         class = cli_class$error$NoConditions)
             }
             out[["gc"]] <- gc_df
           }
         },
         
         json = {
           
           st_read <- jsonlite::read_json(source, simplifyVector = T)
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
         },
         
         object = {
           
           out <- list(st = source)
           
           if (ncol(out$st) > 2) {
             if (is.GroupConditions(conditions))
               conditions <- list(conditions)
             out[["gc"]] <- dplyr::bind_rows(
               lapply(conditions, as.data.frame.GroupConditions)
             )
           }
         })
  
  st_out <- out[["st"]]
  if (!is.null(out[["gc"]])) {
    attr(st_out, "grouped") <- TRUE
    attr(st_out, "conditions") <- verify_GC_for_ST(out[["st"]], out[["gc"]])
  } else
    attr(st_out, "grouped") <- FALSE
  
  if (attr(st_out, "grouped")) {
    attr(st_out, "all") <- isTRUE(any(grepl(names(st_out), pattern = "^.all")))
  }
    
  class(st_out) <- c("ScoringTable", 
                     if (isTRUE(all(grepl(names(st_out)[-1], pattern = ":")))) "Intersect", 
                     "data.frame")
  return(st_out)
  
}

#' Internal function to verify the provided conditions with conditions available
#' in imported *ScoringTable*
#' @param st_df data.frame form of ScoreTable
#' @param gc_df data.frame form of GroupConditions
#' @return list of *GroupConditions* objects or `NULL`
#' @importFrom cli cli_abort
#' @keywords internal

verify_GC_for_ST <- function(st_df, gc_df) {
  
  group_cols <- names(st_df)[-1]
  
  if (length(group_cols) == 1)
    return(NULL)
  
  splitted <- strsplit(group_cols, ":")
  is_intersected <- all(sapply(splitted, \(x) length(x)) == 2)
  
  if (is_intersected && length(unique(gc_df$category)) != 2)
    cli_abort("Imported {.cls ScoringTable} is based upon {.emph intersected} {.cls GroupConditions}, but only one {.cls GroupConditions} object is provided.",
              class = cli_class$error$ScoringImport)
  
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
      cli_abort("Imported {.cls ScoringTable} is based on one {.cls GroupConditions} object, but two are provided.",
                class = cli_class$error$ScoringImport)
    
    valid_groups <- isTRUE(
      all(group_cols %in% groups[[1]]$groups)
    )
  }
  
  # TODO: More informative message
  if (!isTRUE(valid_groups)) {
    # cli({
      cli_abort(c("Provided {.cls GroupConditions} are not compatible with imported {.cls ScoringTable}"),
                class = cli_class$error$ScoringImport)
    #   cli_h2("In {.cls ScoringTable}")
    #   for (n_st in seq_along(splitted[[1]])) {
    #     
    #     vals <- unique(sapply(splitted, \(x) x[n_st]))
    #     vals <- vals[!grepl(vals, pattern = "^\\.")]
    #     cli_inform(c("*" = "{.val {vals}}"))
    #   }
    #   
    #   cli_h2("In {.cls GroupConditions}")
    #   for (group in groups) {
    #     
    #     vals <- group$groups
    #     vals <- vals[!grepl(vals, pattern = "^\\.")]
    #     cli_inform(c("*" = "{.val {vals}}"))
    #   }
    #     
    # })
  }
    
  out <- lapply(groups, \(x) {
    
    conds <- paste0("'", x$groups[!grepl(x = x$groups, pattern = "^\\.")], 
                    "' ~ ", x$conditions)
    conds <- lapply(conds, stats::as.formula)
    
    GroupConditions(conditions_category = x$category,
                    .dots = conds)
    
  })
  
  return(out)
  
}

#' @rdname ScoringTable
#' @param object `ScoringTable` object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_text cli_ul cli_li cli_end 
#' @export

summary.ScoringTable <- function(object, ...) {
  
  cli_text("{.cls ScoringTable}")
  if (ncol(object) == 2)
    cli_text("{.field No. groups}: ungrouped")
  else
    cli_text("{.field No. groups}: {.val {ncol(object)}}")
  cli_text("{.strong Scale}: {.val {names(object)[1]}}; {.var min}: {.val {min(object[[1]])}}; {.var max}: {.val {max(object[[1]])}}")
  
  if (!is.null(attr(object, "conditions"))) {
    
    conds <- attr(object, "conditions")
    
    cli_text("{.field GroupConditions}: {.val {length(conds)}}")
    ol <- cli_ol()
    for (cond in conds) {
      cli_li("{.strong Category}: {attr(cond, 'cond_category')}")
      ul <- cli_ul()
      cli_li("{.field Tested vars}: {.val {attr(cond, 'formula_vars')}}")
      cli_li("{.field No. groups:}: {.val {length(attr(cond, 'groups'))}}")
      cli_end(ul)
    }
    cli_end(ol)
    cli_text("{.field {.bold .all} groups} included: {.val {isTRUE(attr(object, 'all'))}}")
    
  }
}