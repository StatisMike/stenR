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
    
    #' @description Attach new scale to the object. If there are any ScoreTables
    #' already computed, score for newly-attached scale will be computed automatically.
    #' @param scale `StandardScale` object defining a scale
    #' @param overwrite boolean indicating if the definition for a scale
    #' of the same name should be overwritten
    #' 
    
    attach_scale = function(scale, overwrite = FALSE) {
      
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
      
      private$calculate_tables(
        scale = scale$name
      )
      
    },
    
    #' @description Attach previously generated `FrequencyTable` for a given 
    #' variable. `ScoreTable` containing every attached scale will be calulcated
    #' automatically based on every new `FrequencyTable`.
    #' 
    #' @param var String with the name of the variable
    #' @param ft FrequencyTable to be attached
    #' @param if_exists Action that should be taken if `FrequencyTable` for 
    #' given variable already exists in the object.
    #' 
    #' - `append` recalculates existing table
    #' - `replace` replaces existing table
    #' 
    
    attach_FrequencyTable = function(var, ft, if_exists = "append") {
      
      if (!is.character(var) || length(var) != 1) 
        stop("Value provided to 'val' should be a string.")
      if (!"FrequencyTable" %in% class(ft))
        stop("Object provided to 'ft' should be a 'FreqencyTable'")
      if (if_exists %in% c("append", "replace") || length(if_exists) != 1)
        stop("Value provided to 'if_exists' should be either 'append' or 'replace'.")
      
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
    attached_scales = list()
    ## calculate ScoreTables for scales ##
    # `scale` the name of the scale to recalculate. If NULL, recalulate for all attached
    # ``
    
  )
)
                             