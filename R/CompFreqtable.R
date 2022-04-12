#' @docType class
#' @title R6 class for producing re-computable FreqTable
#'
#' @description
#' Computed frequency table class. It can compute and store multiple 
#' \code{\link{FrequencyTable}s} and \code{\link{ScoreTable}s} for multiple
#' variables containing raw score results.
#' 
#' After computation, it could be also used to compute new standardized scores 
#' for provided raw scores and integrate them into stored tables.
#'
#' @export
#' @import R6
#' 
CompFreqtable <- R6::R6Class(
  "CompFreqtable",
  
  public = list(
    
    #' @description Attach new scale to the object
    #' @param scale `StandardScale` object defining a scale
    #' @param 
    #' 
    
    attach_scale = function(scale, overwrite = FALSE) {
      
      overwritten <- FALSE
      
      if (class(scale) != "StandardScale")
        stop("Scale definition should be provided in form of the `StandardScale` object")
      
      if (!is.null(private$attached_scales[[scale$name]])) {
        if (isTRUE(overwrite)) {
          overwritten <- TRUE
          warning(paste0("Definition of scale '", scale$name, 
                         "' was already attached. It is being overwritten. All calculated ScoreTables are being recalculated."))
        } else {
          stop(paste0("Definition of scale '", scale$name, 
                      "' was already attached. To overwrite the scale and recalculate all computed ScoreTables specify `overwrite == TRUE`."))
        }
      }
      
      private$attached_scales[[scale$name]] <- scale
      
      if (isTRUE(overwritten)) {
        
      }
      
    }
    
    
  ),
  
  private = list(
    # source data, if the argument 'keep_data' during initial data insertion is 
    # `TRUE` (default)
    source_data = NULL,
    # computed frequency tables for all variables specified by character vector 
    # 'vars'
    freq_tables = NULL,
    # frequency tables statuses
    freq_tables_status = NULL,
    # computed_scores Computed scores for all raw score values present in the 
    # source data. Computed in format of scale specified by argument 'score'
    computed_scores = NULL,
    # scales list of attached scales
    scales = NULL,
    # scales attached and ready for calculating ScoreTables
    attached_scales = list()
  )
)
                             