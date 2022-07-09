#' @docType class
#' @title R6 class for producing easily re-computable ScoreTable
#'
#' @description
#' Computable ScoreTable class. It can compute and store \code{\link{ScoreTable}s} 
#' for multiple variables containing raw score results.
#' 
#' After computation, it could be also used to compute new standardized scores 
#' for provided raw scores and integrate them into stored tables.
#' 
#' `summary()` function can be used to get general information about 
#' `CompScoreTable` object.
#'
#' @rdname CompScoreTable
#' @export
#' @import R6
#' 
CompScoreTable <- R6::R6Class(
  "CompScoreTable",
  
  public = list(
    
    #' @description Initialize a `CompScoreTable` object. You can attach one or many
    #' `StandardScale` and `FrequencyTable` objects
    #' @param tables Named list of `FrequencyTable` objects to be attached. Names
    #' will indicate the name of variable for which the table is calculated.
    #' Defaults to `NULL`, so no tables will be available at the beginning.
    #' @param scales `StandardScale` object or list of such objects to be attached. 
    #' They will be used for calculation of `ScoreTables.` Defaults to `NULL`, so no
    #' scales wil be available at the beginning.
    #' @details Both `FrequencyTable` and `StandardScale` objects can be attached
    #' with appropriate methods after object initialization.
    #' @return CompScoreTable object
    
    initialize = function(tables = NULL, scales = NULL) {
      
      if (!is.null(tables)) {
        if (!all(sapply(tables, is.FrequencyTable)) || any(is.null(names(tables))))
          cli::cli_abort("Object provided to {.var tables} argument should be a named list of {.cls FrequencyTable} objects.",
                         class = "ClassError")
        for (n_table in seq_along(tables))
        self$attach_FrequencyTable(
          ft = tables[[n_table]], var = names(tables)[n_table]
        )
      }
      
      if (!is.null(scales)) {
        if (!all(sapply(scales, is.StandardScale)) && !is.StandardScale(scales))
          cli::cli_abort("{.cls StandardScale} object or list of such objects needs to be provided to {.var scale} argument",
                         class = "ClassError")
        if (is.StandardScale(scales)) {
          scales <- list(scales)
        }
        for (scale in scales) {
          self$attach_StandardScale(scale = scale)
        }
      }
      
    },
    
    #' @description Attach new scale to the object. If there are any ScoreTables
    #' already computed, score for newly-attached scale will be computed automatically.
    #' @param scale `StandardScale` object defining a scale
    #' @param overwrite boolean indicating if the definition for a scale
    #' of the same name should be overwritten
    #' 
    
    attach_StandardScale = function(scale, overwrite = FALSE) {
      
      if (!is.StandardScale(scale))
        cli::cli_abort("Scale definition should be provided in form of the {.cls StandardScale} object",
                       class = "ClassError")
      
      if (!is.null(private$attached_scales[[scale$name]])) {
        if (isTRUE(overwrite)) {
          cli::cli_inform("Definition of scale {.val {scale$name}} was already attached. It is being overwritten. All calculated {.cls ScoreTables} are being recalculated.")
        } else {
          cli::cli_abort("Definition of scale {.val {scale$name}} was already attached. To overwrite the scale and recalculate all computed {.cls ScoreTables} specify {.code overwrite = TRUE}.",
                         class = "AttachedError")
        }
      }
      
      private$attached_scales[[scale$name]] <- scale
      
      private$calculate_st(scale = scale$name)
      
    },
    
    #' @description Attach previously generated `FrequencyTable` for a given 
    #' variable. `ScoreTable` containing every attached scale will be calulcated
    #' automatically based on every new `FrequencyTable`.
    #' 
    #' @param ft FrequencyTable to be attached
    #' @param var String with the name of the variable
    #' @param if_exists Action that should be taken if `FrequencyTable` for 
    #' given variable already exists in the object.
    #' 
    #' - `stop` DEFAULT: don't do anything
    #' - `append` recalculates existing table
    #' - `replace` replaces existing table
    #' 
    
    attach_FrequencyTable = function(ft, var, if_exists = c("stop", "append", "replace")) {
      
      if_exists <- match.arg(if_exists)
      
      if (!is.character(var) || length(var) != 1) 
        cli::cli_abort("Value provided to {.var val} should be a {.cls character}.",
                       class = "TypeError")
      if (!is.FrequencyTable(ft))
        cli::cli_abort("Object provided to {.var ft} should be a {.cls FreqencyTable}",
                       class = "ClassError")
      
      if (var %in% names(private$tables) && if_exists == "stop") {
        cli::cli_abort("Table for {.val {var}} already exists. To {.emph append} or {.emph replace} the frequencies specify {.var if_exists} argument accordingly.",
                       class = "TableConflictError")
        
      } else if (var %in% names(private$tables) && if_exists == "append") {
        private$merge_ft(data = rep(ft$table$score, ft$table$n),
                         var = var)
        
      } else if (!var %in% names(private$tables) || if_exists == "replace") {
        private$tables[[var]] <- ft
        
      }
      
      if (length(private$attached_scales) > 0 && "FrequencyTable" %in% class(private$tables[[var]]))
        private$tables[[var]] <- ScoreTable(private$tables[[var]], private$attached_scales)
    },
    
    #' @description Export list of `ScoreTables` from the object
    #' @param vars Names of the variables for which to get the tables.
    #' If left at `NULL` default - get all off them.
    #' @param strip logical indicating if the `ScoreTables` should be stripped
    #' down to `FrequencyTables` during export. Defaults to `FALSE`
    #' 
    #' @return list of `ScoreTable` or `FrequencyTable` object

    export_ScoreTable = function(vars = NULL, strip = FALSE) {
      
      if (length(private$tables) == 0 || 
          !all(sapply(private$tables, is.ScoreTable)))
        cli::cli_abort("No {.cls ScoreTable} objects are available to extract.",
                       class = "NoTableError")
      
      if (is.null(vars)) {
        out <- private$tables
      } else {
        if (any(!vars %in% names(private$tables))) 
          cli::cli_abort("Some of the provided {.var vars} aren't valid computed {.cls ScoreTable} names. Currently computed tables for {.val {names(private$table}}.",
                         class = "NoTableError")
        
        out <- private$tables[which(names(private$tables) %in% vars)]
      }
      
      if (isTRUE(strip)) {
        temp_names <- names(out)
        out <- lapply(out, strip_ScoreTable)
        names(out) <- temp_names
      }
      
      return(out)
    },
    
    #' @description Compute standardize scores for `data.frame` of raw scores.
    #' Additionally, the raw scores can be used to recalculate ScoreTables
    #' before computing (using `calc = T`).
    #' @param data data.frame containing raw scores.
    #' @param what the values to get. One of either:
    #' 
    #' - `quan` - the quantile of raw score in the distribution
    #' - `Z` - normalized Z score for the raw scores
    #' - name of the scale attached to the `CompScoreTable` object
    #' 
    #' @param vars vector of variable names which will taken into account 
    #' @param calc should the `ScoreTables` be computed (or recalculated, if
    #' some are already provided?). Default to `TRUE`
    #' @return `data.frame` with standardized values 
    #' 
    standardize = function(data, what, vars = names(data), calc = FALSE) {
      
      # TODO: cli messages
      
      if (length(private$attached_scales) == 0)
        cli::cli_abort("No {.cls StandardScale} have been attached yet. To standardize, please use {.var attach_StandardScale()} method.",
                       class = "NoAttachedScaleError")
      
      valid_whats <- c("quan", "Z", names(private$attached_scales))
      if (!what %in% valid_whats)
        cli::cli_abort("Provide one valid value to {.var what} argument. Valid values: {.val {valid_whats}}.",
                       class = "NoValidWhatError")
      if (!is.data.frame(data))
        cli::cli_abort("Object provided to {.var data} needs to be a {.cls data.frame}.",
                       class = "ClassError")
      if (!all(vars %in% names(data)))
        cli::cli_abort("All values provided to {.var vars} need to be variables in provided {.var data}.",
                       class = "NoValidVarsError")
      if (!all(sapply(vars, \(x) is.numeric(data[[x]]))))
        cli::cli_abort("All specified {.val vars} should be of type {.cls numeric}.",
                       class = "TypeError")
      
      data[, vars] <- sapply(data[, vars], as.integer)
      
      # calculate the ScoreTables
      if (isTRUE(calc)) {
        for (v in vars) {
          if (is.null(private$tables[[v]]))
            private$calculate_ft(data[[v]], v)
          else 
            private$merge_ft(data[[v]], v)
        }
      } else {
        if (length(private$tables) == 0)
          cli::cli_abort("No {.cls ScoreTable} to get the values from.",
                         class = "NoTableError")
      }
      
      # compute the values
      for (v in vars) {
        data[[v]] <- normalize_score(
          x = data[[v]],
          table = private$tables[[v]],
          what = what
        )
      }
      
      return(data)
      
    }
  ),
  
  private = list(
    # source data, if the argument 'keep_data' during initial data insertion is 
    # `TRUE` (default)
    source_data = NULL,
    # computed scoretables for provided variables
    tables = list(),
    # scales list of attached scales
    scales = NULL,
    # scales attached and ready for calculating ScoreTables
    attached_scales = list(),
    ## calculate ScoreTables for scales ##
    # `scale` the name of the scale to recalculate. If NULL, recalulate for all 
    # attached `StandardScales`
    # `var` the name of the variable to calculate for. If NULL, calc for all
    # possible variables
    calculate_st = function(scale = NULL, var = NULL) {
      
      if (!is.null(scale))
        scales <- private$attached_scales[which(names(private$attached_scales) == scale)]
      else scales <- private$attached_scales
      
      if (!is.null(var)) {
        private$tables[[var]] <- switch(
          class(private$tables[[var]])[1], 
          "FrequencyTable" = ScoreTable(private$tables[[var]], scales),
          "ScoreTable" = attach_scales(private$tables[[var]], scales))
      } else {
        for (v in names(private$tables)) {
          private$tables[[v]] <- switch(
            class(private$tables[[v]])[1],
            "FrequencyTable" = ScoreTable(private$tables[[v]], scales),
            "ScoreTable" = attach_scales(private$tables[[v]], scales)) 
        }   
      }
    },
    
    ## create FrequencyTable for variable ##
    # `data` vector of raw scores to calculate the frequency table for
    # `var` the name of the variable to recreate. If NULL, recreate all attached
    # `FrequencyTables`
    calculate_ft = function(data, var = NULL) {
      
      if (!is.null(var)) 
        suppressMessages(private$tables[[var]] <- FrequencyTable(data),
                         class = "IncompleteRangeMessage")
      else 
        for (v in names(private$tables))
          suppressMessages(private$tables[[v]] <- FrequencyTable(data),
                           class = "IncompleteRangeMessage")
      
      private$calculate_st(var = var)  
        
    },
    
    ## merge Frequency tables ##
    merge_ft = function(data, var) {
      
      if (is.Simulated(private$tables[[var]]))
        cli::cli_abort("You can't add new raw values to {.strong Simulated} {.cls FrequencyTable}.",
                       class = "SimulatedError")
      
      vals <- rep(private$tables[[var]]$table$score, 
                  private$tables[[var]]$table$n)
      
      suppressMessages(private$tables[[var]] <- FrequencyTable(c(data, vals)),
                       class = "IncompleteRangeMessage")
      private$calculate_st(var = var)
      
    }
  )
)

#' @export
summary.CompScoreTable <- function(object, ...) {
  
  cli::cli_inform("{.cls CompScoreTable}")
  
  summaries <- list()
  
  if (length(object$.__enclos_env__$private$tables) > 0) {
    
    table_class <- 
      unique(unlist(sapply(object$.__enclos_env__$private$tables, \(t) class(t))))
    
    table_class <-
      table_class[!is.Simulated(table_class)]
    
    cli::cli_inform("{.strong Attached {.cls {table_class}}}")
    summaries[["tables"]] <- 
      data.frame(
        variable = names(object$.__enclos_env__$private$tables),
        n = sapply(object$.__enclos_env__$private$tables, \(t) t$status$n),
        range = sapply(object$.__enclos_env__$private$tables, \(t) t$status$range))
    rownames(summaries[["tables"]]) <- NULL
    print(summaries[["tables"]], row.names = F)
  } else {
    cli::cli_inform("{.strong No tables attached}.")
  }
  
  if(length(summaries) == 1) cat("\n")
  
  if (length(object$.__enclos_env__$private$attached_scales) > 0){
    
    cli::cli_inform("{.strong Attached {.cls StandardScale}}")
    summaries[["scales"]] <-
      data.frame(
        name = names(object$.__enclos_env__$private$attached_scales),
        M = sapply(object$.__enclos_env__$private$attached_scales, \(s) s$M),
        SD = sapply(object$.__enclos_env__$private$attached_scales, \(s) s$SD),
        min = sapply(object$.__enclos_env__$private$attached_scales, \(s) s$min),
        max = sapply(object$.__enclos_env__$private$attached_scales, \(s) s$max)
      )
    rownames(summaries[["scales"]]) <- NULL
    print(summaries[["scales"]], row.names = F)
  } else {
    cli::cli_inform("{.strong No {.cls StandardScale} attached}.")
  }
  
  return(invisible(summaries))
  
}
                             