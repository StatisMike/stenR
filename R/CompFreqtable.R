#' @docType class
#' @title R6 class for producing easily re-computable ScoreTable
#'
#' @description
#' Computed ScoreTable class. It can compute and store \code{\link{ScoreTable}s} 
#' for multiple variables containing raw score results.
#' 
#' After computation, it could be also used to compute new standardized scores 
#' for provided raw scores and integrate them into stored tables.
#'
#' @export
#' @import R6
#' 
CompScoreTable <- R6::R6Class(
  "CompScoreTable",
  
  public = list(
    
    #' @description Initialize a CompScoreTable object. You can attach one or many
    #' StandardScale and FrequencyTable objects
    #' @param tables Named list of FrequencyTable objects to be attached. Names
    #' will indicate the name of variable for which the table is calculated.
    #' Defaults to NULL, so no tables will be available at the beginning.
    #' @param scales StandardScale object or list of such objects to be attached. 
    #' They will be used for calculation of ScoreTables. Default to NULL, so no
    #' scales wil be attached.
    #' @details Both `FrequencyTable` and `StandardScale` objects can be attached
    #' with appropriate methods after object initialization.
    
    initialize = function(tables = NULL, scales = NULL) {
      
      if (!is.null(tables)) {
        if (class(tables) != "list" ||
            !all(sapply(tables, \(x) "FrequencyTable" %in% class(x))) ||
            any(is.null(names(tables))))
          stop("Object provided to 'tables' argument should be a named list of 'FrequencyTable' objects.")
        for (n_table in seq_along(tables))
        self$attach_FrequencyTable(
          ft = tables[[n_table]], var = names(tables)[n_table]
        )
      }
      
      if (!is.null(scales)) {
        if (!class(scales) %in% c("StandardScale", "list"))
          stop("Object of class 'StandardScale' or list of such objects needs to be provided to 'scale' argument")
        if (class(scales) == "StandardScale") {
          scales <- list(scales)
        } else if (class(scales) == "list") {
          areScales <- all(sapply(scales, \(x) class(x) == "StandardScale"))
          if (!isTRUE(areScales)) 
            stop("List provided to 'scale' argument should contain only StandardScale objects")
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
      
      if (class(scale) != "StandardScale")
        stop("Scale definition should be provided in form of the `StandardScale` object")
      
      if (!is.null(private$attached_scales[[scale$name]])) {
        if (isTRUE(overwrite)) {
          warning(paste0("Definition of scale '", scale$name, 
                         "' was already attached. It is being overwritten. All calculated ScoreTables are being recalculated."))
        } else {
          stop(paste0("Definition of scale '", scale$name, 
                      "' was already attached. To overwrite the scale and recalculate all computed ScoreTables specify `overwrite == TRUE`."))
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
    #' - `append` recalculates existing table
    #' - `replace` replaces existing table
    #' 
    
    attach_FrequencyTable = function(ft, var, if_exists = "append") {
      
      if (!is.character(var) || length(var) != 1) 
        stop("Value provided to 'val' should be a string.")
      if (!"FrequencyTable" %in% class(ft))
        stop("Object provided to 'ft' should be a 'FreqencyTable'")
      if (!if_exists %in% c("append", "replace") || length(if_exists) != 1)
        stop("Value provided to 'if_exists' should be either 'append' or 'replace'.")
      
      if (var %in% names(private$tables) && if_exists == "append") {
        
        private$merge_ft(data = rep(ft$table$score, ft$table$n),
                         var = var)
        
      } else if (!var %in% names(private$tables) || if_exists == "replace") {
        private$tables[[var]] <- ft
        
      }
      
      if (length(private$attached_scales) > 0)
        private$tables[[var]] <- ScoreTable(private$tables[[var]], private$attached_scales)
    },
    
    #' @description Extract list of ScoreTables from the object
    #' @param vars Names of the variables for which to get the ScoreTables.
    #' If left at `NULL` default - get all ScoreTables

    get_ScoreTable = function(vars = NULL) {
      
      if (length(private$tables) == 0 || 
          !all(sapply(private$tables, \(x) "ScoreTable" %in% class(x))))
        stop("No 'ScoreTable's objects are available to extract.")
      
      if (is.null(vars)) {
        out <- private$tables
      } else {
        if (any(!vars %in% names(private$tables))) 
          stop(
            paste0("Some of the provided 'vars' aren't valid ScoreTable names. ",
                   "Currently computed tables for: '",
                   paste(names(private$tables), sep = "', '"), "'.")
            )
        out <- private$tables[which(names(private$tables) %in% vars)]
      }
      return(out)
    },
    
    #' @description Compute standardize scores for data.frame of raw scores.
    #' Additionally, the raw scores can be used to recalculate ScoreTables
    #' before computing.
    #' @param data data.frame containing raw scores.
    #' @param vars vector of variable names which will taken into account
    #' @param what the values to get. One of either:
    #' 
    #' - `quan` - the quantile of raw score in the distribution
    #' - `Z` - normalized Z score for the raw scores
    #' - name of the scale attached to the `CompScoreTable` object 
    #' 
    #' @param calc should the ScoreTables be computed (or recalculated, if
    #' some are already provided?)
    #' @return data.frame with values recalculated
    #' 
    standardize = function(data, vars = names(data), what, calc = TRUE) {
      
      if (length(private$attached_scales) == 0)
        stop("No 'StandardScales' are currently attached. Attach some of them first.")
      if (!what %in% c("quan", "Z", names(private$attached_scales)))
        stop("Provide either 'quan', 'Z' or name of the computed scale to the 'what' argument.")
      if (!"data.frame" %in% class(data))
        stop("Object provided to 'data' argument should be a 'data.frame'")
      if (!all(vars %in% names(data)))
        stop("All 'vars' should be pointing to columns in provided 'data.frame'")
      
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
          stop("No ScoreTables to get the values from.")
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
        private$tables[[var]] <- FrequencyTable(data)
      else 
        for (v in names(private$tables))
          private$tables[[v]] <- FrequencyTable(data)
      
      private$calculate_st(var = var)  
        
    },
    
    ## merge Frequency tables ##
    merge_ft = function(data, var) {
      
      if (any(c(class(private$tables[[var]]), class(ft)) == "Simulated"))
        stop("You can't merge Simulated FrequencyTable", call. = F)
      
      vals <- rep(private$tables[[var]]$table$score, 
                  private$tables[[var]]$table$n)
      
      private$tables[[var]] <- FrequencyTable(c(data, vals))
      private$calculate_st(var = var)
      
    }
  )
)
                             