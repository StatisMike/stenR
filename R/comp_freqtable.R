#' #' R6 class containing computed freqtable
#' #'
#' #' @description
#' #' Computed frequency table class. On basis of raw scores of some variables,
#' #' it creates a frequency table and allows to compute scores in some standard
#' #' scale. To initialize the object of this class the 'gen_freqtable' function
#' #' is recommended.
#' #' @export
#' #' @import R6
#' 
#' Comp_Freqtable <- R6::R6Class("Comp_Freqtable",
#' 
#'   private = list(
#' 
#' 
#'     # source data, if the argument 'keep_data' during initialization is TRUE (default)
#'     source_data = NULL,
#' 
#'     # computed frequency tables for all variables specified by character vector 'vars'
#'     freq_tables = NULL,
#' 
#'     # frequency tables statuses
#'     freq_tables_status = NULL,
#' 
#'     # computed_scores Computed scores for all raw score values present in the source data. Computed in format of scale specified by argument 'score'
#'     computed_scores = NULL,
#'     
#'     # scales list of attached scales
#'     scales = NULL
#'     
#'   ),
#' 
#'   public = list(
#' 
#'     #' @description Current information about object
#'     #' 
#'     #' @return List of metrics
#'     #' \itemize{
#'     #'   \item n: Number of observations for frequency table generation
#'     #'   \item kept_data: Is the data kept in the object?
#'     #'   \item frequency_tables: Are the frequency table complete or incomplete?
#'     #'   \item standardized_scores: Are the standardized scores computed - and if yes, then: for which scales?
#'     #' }
#' 
#'     get_status = function() {
#'       
#'       status <- list(
#'         data_kept = !is.null(private$source_data),
#'         frequency_tables = private$freq_tables_status,
#'         standardized_scores = if (is.null(names(private$computed_scores))) "not computed yet"
#'         else private$scales[names(private$scales) %in% names(private$computed_scores)]
#'       )
#'       
#'       return(status)
#'         
#'     },
#' 
#'     #' @description Information about kept data
#'     #'
#'     get_data_info = function(){
#'       if(is.null(private$source_data)){
#'         stop(.warnings$need_source_data)
#'       }
#' 
#'       list(
#'         variables = names(private$source_data)[-1],
#'         ids = private$source_data[,1]
#'       )
#'     },
#' 
#'     #' @description Initialization of the 'comp_freqtable' class
#'     #'
#'     #' @param data data to use for computing.
#'     #' @param vars character vector containing column names of numeric variables that you want to calculate frequency tables for.
#'     #' @param id character string containing column name for identification of the observations. Needed only if 'keep_data' is TRUE
#'     #' @param keep_data do you want to keep the table for getting scores for individual observations? Defaults to TRUE.
#' 
#'     initialize = function(data, vars, id, keep_data = T) {
#' 
#'       if (!is.data.frame(data)) {
#'         stop(.warnings$data.frame_required, call. = F)
#'       }
#'       if (is.null(id) & keep_data) {
#'         stop(.warnings$missing_id_for_keep, call. = F)
#'       }
#'       if (keep_data && !id %in% names(data)) {
#'         stop(.warnings$bad_id_name)
#'       }
#'       if (!all(vars %in% names(data))) {
#'         stop(.warnings$bad_var_name)
#'       }
#' 
#'       if (keep_data) {
#'         private$source_data <- data[, c(id, vars)]
#'       }
#'       
#'       tables <- list()
#'       
#'       for (var in vars) {
#'         tables[[var]] <- .calc_freq_Z_table(data, var)
#'       }
#' 
#'       freqtables <- list()
#'       statuses <- list()
#' 
#'       for (n_freq in 1:length(tables)) {
#' 
#'         freqtables[[names(tables)[n_freq]]] <- tables[[n_freq]]$table
#'         statuses[[names(tables)[n_freq]]] <- tables[[n_freq]]$status
#' 
#'       }
#'       
#'       lapply(.default_scales, function(x) self$attach_scale(x))
#' 
#'       private$freq_tables <- freqtables
#'       private$freq_tables_status <- statuses
#' 
#' 
#'     },
#' 
#'     #' @description Get generated frequency tables.
#'     #' @return
#'     #' Two lists, containing actual cumulated frequency tables and their statuses
#'     #' \itemize{
#'     #'   \item freqtables: Cumulated frequency tables for each variable specified during initialization
#'     #'   \item statuses: Statuses for each frequency table. 'complete' if there was at least one observation for each raw score between actual minimum and maximum. 'incomplete' if there was at least one missing raw score and they were filled automatically
#'     #' }
#'     get_freqtables = function() {
#' 
#'       return(private$freq_tables)
#' 
#'     },
#'     
#'     #' @description Attach new scale to the object
#'     #' @param scale `FreqtableScale` object defining a scale
#'     #' 
#'     
#'     attach_scale = function(scale) {
#'       
#'       if (class(scale) != "FreqtableScale")
#'         stop("Scale definition should be provided in form of the `FreqtableScale` object")
#'       
#'       if (!is.null(private$scales[[scale$name]]))
#'         warning(paste0("Definition of scale '", scale$name, 
#'                        "' was already attached. It is being overwritten."))
#'       
#'       private$scales[[scale$name]] <- 
#'         list(M = scale$M, SD = scale$SD, min = scale$min, max = scale$max)
#'       
#'     },
#' 
#'     #' @description Compute scores in scale of your choice. After computation, 
#'     #' you can get them with 'get_scoretables' method.
#'     #' @param scale Either a character string indicating one of built-in scale, 
#'     #' or `FreqtableScale` object describing new scale.
#'     #' If new scale is declared, it will be automatically attached
#'     #' 
#'     compute_scores = function(scale) {
#'       
#'       if (class(scale) == "FreqtableScale") {
#'         
#'         self$attach_scale(scale)
#'         scale <- scale$name
#'         
#'       } else if (class(scale) == "character" & length(scale) == 1) {
#'         
#'         if (!scale %in% names(private$scales)) {
#'           stop(paste0("Scale of name '", scale, " were not attached before."))
#'         }
#'         
#'       } else {
#'         
#'         stop("'scale' argument should contain eiter character poining to attached scale or new `FreqtableScale` object with definition")
#'         
#'       }
#' 
#'       output <- list()
#'       
#'       for (var in names(private$freq_tables))
#'       
#'       if (is.null(private$computed_scores[[var]])) {
#'         
#'         private$computes_scores
#'         
#'         
#'       }
#'       
#'       for(var in names(private$freq_tables)){
#'         output[[var]] <- .calc_score(name = var,
#'                                      table = private$freq_tables[[var]],
#'                                      private$scales[[scale]]$M,
#'                                      private$scales[[scale]]$SD,
#'                                      private$scales[[scale]]$min,
#'                                      private$scales[[scale]]$max,
#'                                      scale)
#'       }
#' 
#'       private$computed_scores[[scale]] <- output
#' 
#'     },
#' 
#'     #' @description Get computed scoring tables for all variables
#'     #' @param scale character string indicating for which computed scale return values
#'     #' @return
#'     #' List containing the actual tables with raw score and standardized score pairs
#'     #'
#'     get_scoretables = function(scale){
#' 
#'       if(is.null(scale) || !is.character(scale) || length(scale) > 1 || !scale %in% names(private$computed_scores)) {
#'         stop(.warnings$valid_scale_required)
#'       }
#' 
#'       private$computed_scores[[scale]]
#'     },
#' 
#'     #' @description Get computed scores for specified ids or all sourced observations. You can only use this method if you chose to keep source data.
#'     #' @param scale character string defining the computed scale. Mandatory argument.
#'     #' @param vars character vector declaring for which variables of the embedded data to get the standardized score. If not specified, returns values for all
#'     #' @param ids for the observations in kept data. If none is provided, then it is returned for all observations.
#'     #'
#'     get_computed_scores = function(scale,
#'                                    vars = NULL,
#'                                    ids = NULL){
#' 
#'       # check all arguments
#' 
#'       if(is.null(private$source_data)){
#'         stop(.warnings$need_source_data, call. = F)
#'       }
#'       if(is.null(scale) || !is.character(scale) || length(scale) > 1 || !scale %in% names(private$computed_scores)) {
#'         stop(.warnings$valid_scale_required, call. = F)
#'       }
#'       if(!is.null(ids) & (!is.character(ids) || !all(ids %in% private$source_data[[1]]))){
#'         stop(.warnings$valid_id_required, call. = F)
#'       }
#'       if(!is.null(vars) & (!is.character(vars) || !all(vars %in% names(private$computed_scores[[scale]])))){
#'         stop(.warnings$bad_var_name, call. = F)
#'       }
#' 
#'       # if ids not provided, get all ids
#'       if(is.null(ids)){
#'         results <- data.frame(id = private$source_data[[1]])
#'       } else {
#'         results <- data.frame(id = ids)
#'       }
#' 
#'       # if vars not provided, get all vars
#'       if(is.null(vars)){
#'         variables <- names(private$computed_scores[[scale]])
#'       } else {
#'         variables <- vars
#'       }
#' 
#'       # for every var create new column in results
#'       for (variable in variables) {
#'         results[[variable]] <- rep(as.numeric(NA), nrow(results))
#' 
#'             # for every id:
#'             for (id in results[[1]]) {
#'               # get index from results table and source data
#'               results_index <- which(results[[1]] == id)
#'               source_index <- which(private$source_data[[1]] == id)
#'               # get value from computed scores table and into the correct results row
#'               results[results_index, variable] <- .get_comp_score(
#'                 raw_score = private$source_data[source_index, variable],
#'                 comp_table = private$computed_scores[[scale]][[variable]]
#'               )
#'             }
#'         }
#'       return(results)
#'     },
#' 
#'     #' @description Get computed scores for external observations
#'     #' @param data data.frame containing raw scores to get standardized scores for
#'     #' @param scale character string defining the computed scale
#'     #' @param vars character vector declaring variables for which to get 
#'     #' standardized score. If the variables in external data are named 
#'     #' differently, the vector should be named accordingly.
#'     #' @param attach boolean indicating if new data should be attached to the
#'     #' frequency table. Defaults to TRUE.
#' 
#'     get_computed_scores_ext = function(data,
#'                                        scale,
#'                                        vars,
#'                                        attach = TRUE){
#' 
#'       if(is.null(scale) || !is.character(scale) || length(scale) > 1 || !scale %in% names(private$computed_scores)) {
#'         stop(.warnings$valid_scale_required, call. = F)
#'       }
#'       if(!is.character(vars)){ 
#'         stop(.warnings$bad_var_name, call. = F)
#'       }
#'       
#'       if(!is.null(names(vars))){
#'         source_names <- names(data)
#'         if(!all(names(vars) %in% names(private$computed_scores[[scale]]))){
#'           stop(.warnings$bad_var_name, call. = F)
#'         }
#'       } else {
#'         if(!all(vars %in% names(private$computed_scores[[scale]]))){
#'           stop(.warnings$bad_var_name, call. = F)
#'         }
#'       }
#' 
#'     # loop over all variables
#'       for (variable in vars) {
#' 
#'         # if the character vector is named, rename the data
#'         if(!is.null(names(vars))){
#'           name_index <- which(names(data) == variable)
#'           variable <- names(data)[name_index] <- names(vars)[name_index]
#'         }
#' 
#'       # get computed values for each row
#'         for (row in 1:nrow(data)) {
#'           data[row, variable] <- .get_comp_score(
#'             raw_score = as.character(data[row, variable]),
#'             comp_table = private$computed_scores[[scale]][[variable]]
#'           )
#'         }
#'       }
#' 
#'       # if original data were named differently, return renamed to original
#'       if(!is.null(names(vars))){
#'         names(data) <- source_names
#'       }
#' 
#'       return(data)
#'     }
#'   )
#' )
#' 
#' #' Compute frequency table for standardized scores computing
#' #' @name gen_freqtable
#' #'
#' #' @param data data for computing
#' #' @param vars character vector containing column names of numeric variables that you want to calculate frequency tables for
#' #' @param id character string containing column name for identification of the observations. Needed only if 'keep_data' is TRUE
#' #' @param keep_data do you want to keep the table for getting scores for individual observations? Defaults to TRUE
#' #' @export
#' #'
#' 
#' gen_freqtable <- function(data,
#'                           vars,
#'                           id = NULL,
#'                           keep_data = T
#'                           ) {
#'   
#'   CompFreqtable$new(data = data,
#'                      vars = vars,
#'                      id = id,
#'                      keep_data = keep_data)
#' 
#' }
#' 
#' 
#' #' Summary method for CompFreqtable object
#' #' @param object an object for which a summary is desired.
#' #' @param ... additional arguments affecting the summary produced.
#' #' @export
#' 
#' summary.CompFreqtable <- function(object, ...) {
#'   
#'   status <- object$get_status()
#'   
#'   cat("Source data is", if(status$data_kept) "kept within." else "not kept within.\n")
#'   cat("\nComputed frequency tables for:", 
#'       length(status$frequency_tables), "scales.\n")
#'   cat("\nFrequency tables status:\n")
#'   for (i in seq_along(status$frequency_tables)) {
#'     cat(sep = "", names(status$frequency_tables)[i], ":\n", 
#'         "\trange: ", status$frequency_tables[[i]]$range, "\n",
#'         "\tno obs: ", status$frequency_tables[[i]]$n, "\n")
#'   }
#'   cat("\nComputed standardized scores for scales:\n")
#'   if (status$standardized_scores[1] == "not computed yet") cat("None yet!")
#'   else { 
#'     for (i in seq_along(status$standardized_scores)) {
#'       cat(names(status$standardized_scores)[i], ":\t( ", sep = "")
#'       for (i_p in seq_along(status$standardized_scores[[i]])) {
#'         cat(names(status$standardized_scores[[i]])[i_p], ": ", sep = "")
#'         cat(status$standardized_scores[[i]][[i_p]])
#'         if (i_p == 4) cat(" ") else cat("; ")
#'       }
#'       cat(")\n")
#'     }
#'   }
#'   
#' }
#' 
