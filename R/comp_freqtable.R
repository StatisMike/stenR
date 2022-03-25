#' R6 class containing computed freqtable
#'
#' @description
#' Computed frequency table class. On basis of raw scores of some variables,
#' it creates a frequency table and allows to compute scores in some standard
#' scale. To initialize the object of this class the 'gen_freqtable' function
#' is recommended.
#' @export
#' @import R6

CompFreqtable <- R6::R6Class("CompFreqtable",

  private = list(

    # how many observation were used to create frequency tables
    n = NULL,

    # source data, if the argument 'keep_data' during initialization is TRUE (default)
    source_data = NULL,

    # computed frequency tables for all variables specified by character vector 'vars'
    freq_tables = NULL,

    # frequency tables statuses
    freq_tables_status = NULL,

    # computed_scores Computed scores for all raw score values present in the source data. Computed in format of scale specified by argument 'score'
    computed_scores = NULL
  ),

  public = list(

    #' @description Current information about object
    #' 
    #' @return List of metrics
    #' \itemize{
    #'   \item n: Number of observations for frequency table generation
    #'   \item kept_data: Is the data kept in the object?
    #'   \item frequency_tables: Are the frequency table complete or incomplete?
    #'   \item standardized_scores: Are the standardized scores computed - and if yes, then: for which scales?
    #' }

    get_status = function() {
      
      status <- list(
        n = private$n,
        data_kept = !is.null(private$source_data),
        frequency_tables = private$freq_tables_status,
        standardized_scores = if (is.null(private$computed_scores)) "not computed yet"
        else names(private$computed_scores)
      )
      
      return(status)
        
    },

    #' @description Information about kept data
    #'
    get_data_info = function(){
      if(is.null(private$source_data)){
        stop(.warnings$need_source_data)
      }

      list(
        variables = names(private$source_data)[-1],
        ids = private$source_data[,1]
      )
    },

    #' @description Initialization of the 'comp_freqtable' class
    #'
    #' @param data data to use for computing.
    #' @param vars character vector containing column names of numeric variables that you want to calculate frequency tables for.
    #' @param id character string containing column name for identification of the observations. Needed only if 'keep_data' is TRUE
    #' @param keep_data do you want to keep the table for getting scores for individual observations? Defaults to TRUE.

    initialize = function(data, vars, id, keep_data = T) {

      if (!is.data.frame(data)) {
        stop(.warnings$data.frame_required, call. = F)
      }
      if (is.null(id) & keep_data) {
        stop(.warnings$missing_id_for_keep, call. = F)
      }
      if (keep_data && !id %in% names(data)) {
        stop(.warnings$bad_id_name)
      }
      if (!all(vars %in% names(data))) {
        stop(.warnings$bad_var_name)
      }


      private$n <- nrow(data)

      if (keep_data) {
        private$source_data <- data[, c(id, vars)]
      }
      tables <- list()
      for (var in vars) {
        tables[[var]] <- .calc_freq_Z_table(data, var)
      }

      freqtables <- list()
      statuses <- list()

      for (n_freq in 1:length(tables)) {

        freqtables[[names(tables)[n_freq]]] <- tables[[n_freq]]$table
        statuses[[names(tables)[n_freq]]] <- tables[[n_freq]]$status

      }

      private$freq_tables <- freqtables
      private$freq_tables_status <- statuses

    },

    #' @description Get generated frequency tables.
    #' @return
    #' Two lists, containing actual cumulated frequency tables and their statuses
    #' \itemize{
    #'   \item freqtables: Cumulated frequency tables for each variable specified during initialization
    #'   \item statuses: Statuses for each frequency table. 'complete' if there was at least one observation for each raw score between actual minimum and maximum. 'incomplete' if there was at least one missing raw score and they were filled automatically
    #' }
    get_freqtables = function() {

      return(private$freq_tables)

    },

    #' @description Compute scores in scale of your choice. After computation, you can get them with 'get_scoretables' method.
    #' @param scale Either a character string indicating one of built-in scale, or list specifying the parameters of scale of your choice
    #' \itemize{
    #'   \item built-in scales: 'sten', 'stanine', 'tanine', 'tetronic', 'wechsler-iq'
    #'   \item custom scales: specify parameters within list: 'M', 'SD', 'min', 'max' and 'name' of the scale for storage within object
    #' }
    compute_scores = function(scale) {

      if (class(scale) == "character" & length(scale) == 1) { # default scoring scales
        if (scale == "sten") {
          params <- list(M = 5.5, SD = 2, min = 1, max = 10)
        } else if (scale == "stanine") {
          params <- list(M = 5, SD = 2, min = 1, max = 9)
        } else if (scale == "tanine") {
          params <- list(M = 50, SD = 10, min = 1, max = 100)
        } else if (scale == "tetronic") {
          params <- list(M = 10, SD = 4, min = 0, max = 20)
        } else if (scale == "wechsler-iq") {
          params <- list(M = 100, SD = 15, min = 40, max = 160)
        } else {
          stop(.warnings$bad_scale_specification)
        }
      } else if (class(scale) == "list") {
        if (all(c("M", "SD", "min", "max", "name") %in% names(scale))) {
          params <- list(M = scale$M, SD = scale$SD, min = scale$min, max = scale$max)
          scale <- scale$name
        } else {
          stop(.warnings$bad_scale_specification)
        }
      } else {
        stop(.warnings$bad_scale_specification)
      }

      output <- list()
      for(var in names(private$freq_tables)){
        output[[var]] <- .calc_score(name = var,
                                     table = private$freq_tables[[var]],
                                     params$M,
                                     params$SD,
                                     params$min,
                                     params$max)
      }

      private$computed_scores[[scale]] <- list(tables = output,
                                               params = params)

    },

    #' @description Get computed scoring tables for all variables
    #' @param scale character string indicating for which computed scale return values
    #' @return
    #' Two lists, containing the actual tables and parameters used for calculation
    #' \itemize{
    #' \item tables: tables with raw score and standardized score pairs
    #' \item params: values of 'M', 'SD', 'min' and 'max' used for calculations
    #' }
    #'
    get_scoretables = function(scale){

      if(is.null(scale) || !is.character(scale) || length(scale) > 1 || !scale %in% names(private$computed_scores)) {
        stop(.warnings$valid_scale_required)
      }

      private$computed_scores[[scale]]
    },

    #' @description Get computed scores for specified ids or all sourced observations. You can only use this method if you chose to keep source data.
    #' @param scale character string defining the computed scale. Mandatory argument.
    #' @param vars character vector declaring for which variables of the embedded data to get the standardized score. If not specified, returns values for all
    #' @param ids for the observations in kept data. If none is provided, then it is returned for all observations.
    #'
    get_computed_scores = function(scale,
                                   vars = NULL,
                                   ids = NULL){

      # check all arguments

      if(is.null(private$source_data)){
        stop(.warnings$need_source_data, call. = F)
      }
      if(is.null(scale) || !is.character(scale) || length(scale) > 1 || !scale %in% names(private$computed_scores)) {
        stop(.warnings$valid_scale_required, call. = F)
      }
      if(!is.null(ids) & (!is.character(ids) || !all(ids %in% private$source_data[[1]]))){
        stop(.warnings$valid_id_required, call. = F)
      }
      if(!is.null(vars) & (!is.character(vars) || !all(vars %in% names(private$computed_scores[[scale]]$tables)))){
        stop(.warnings$bad_var_name, call. = F)
      }

      # if ids not provided, get all ids
      if(is.null(ids)){
        results <- data.frame(id = private$source_data[[1]])
      } else {
        results <- data.frame(id = ids)
      }

      # if vars not provided, get all vars
      if(is.null(vars)){
        variables <- names(private$computed_scores[[scale]]$tables)
      } else {
        variables <- vars
      }

      # for every var create new column in results
      for (variable in variables) {
        results[[variable]] <- rep(as.numeric(NA), nrow(results))

            # for every id:
            for (id in results[[1]]) {
              # get index from results table and source data
              results_index <- which(results[[1]] == id)
              source_index <- which(private$source_data[[1]] == id)
              # get value from computed scores table and into the correct results row
              results[results_index, variable] <- .get_comp_score(
                raw_score = private$source_data[source_index, variable],
                comp_table = private$computed_scores[[scale]]$tables[[variable]]
              )
            }
        }
      return(results)
    },

    #' @description Get computed scores for external observations
    #' @param data data.frame containing raw scores to get standardized scores for
    #' @param scale character string defining the computed scale
    #' @param vars character vector declaring variables for which to get standardized score. If the variables in external data are named differently, the vector should be named accordingly.

    get_computed_scores_ext = function(data,
                                       scale,
                                       vars){

      if(is.null(scale) || !is.character(scale) || length(scale) > 1 || !scale %in% names(private$computed_scores)) {
        stop(.warnings$valid_scale_required, call. = F)
      }
      if(!is.character(vars)){ 
        stop(.warnings$bad_var_name, call. = F)
      }
      
      if(!is.null(names(vars))){
        source_names <- names(data)
        if(!all(names(vars) %in% names(private$computed_scores[[scale]]$tables))){
          stop(.warnings$bad_var_name, call. = F)
        }
      } else {
        if(!all(vars %in% names(private$computed_scores[[scale]]$tables))){
          stop(.warnings$bad_var_name, call. = F)
        }
      }

    # loop over all variables
      for (variable in vars) {

        # if the character vector is named, rename the data
        if(!is.null(names(vars))){
          name_index <- which(names(data) == variable)
          variable <- names(data)[name_index] <- names(vars)[name_index]
        }

      # get computed values for each row
        for (row in 1:nrow(data)) {
          data[row, variable] <- .get_comp_score(
            raw_score = as.character(data[row, variable]),
            comp_table = private$computed_scores[[scale]]$tables[[variable]]
          )
        }
      }

      # if original data were named differently, return renamed to original
      if(!is.null(names(vars))){
        names(data) <- source_names
      }

      return(data)
    }
  )
)

#' Compute frequency table for standardized scores computing
#' @name gen_freqtable
#'
#' @param data data for computing
#' @param vars character vector containing column names of numeric variables that you want to calculate frequency tables for
#' @param id character string containing column name for identification of the observations. Needed only if 'keep_data' is TRUE
#' @param keep_data do you want to keep the table for getting scores for individual observations? Defaults to TRUE
#' @export
#'

gen_freqtable <- function(data,
                          vars,
                          id = NULL,
                          keep_data = T
                          ) {
  
  CompFreqtable$new(data = data,
                     vars = vars,
                     id = id,
                     keep_data = keep_data)

}


#' Summary method for CompFreqtable object
#' @export

summary.CompFreqtable <- function(object) {
  
  status <- object$get_status()
  
  cat("Frequency tables have been computed on:", status$n, "observations.\n")
  cat("\nSource data is", if(status$data_kept) "kept within." else "not kept within.\n")
  cat("\nComputed frequency tables for:", 
      length(status$frequency_tables), "scales.\n")
  cat("\nFrequency table status:\n")
  for (i in seq_along(status$frequency_tables)) {
    cat(names(status$frequency_tables)[i], ":", status$frequency_tables[[i]], "\n")
  }
  cat("\nComputed standardized scores for scales:\n")
  if (status$standardized_scores[1] == "not computed yet") cat("None yet!")
  else { 
    for (scale in status$standardized_scores) {
      cat(scale, "\n")
    }
  }
  
}

