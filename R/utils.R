#### GLOBAL VARIABLES ####

utils::globalVariables(c("SD", "group1", "group2", "n", "prop", "score", "sds", "spec"))

cli_class <- list(
  error = list(
    Class = "ClassError",
    Type = "TypeError",
    Attached = "AttachedError",
    TableConflict = "TableConflictError",
    NoScale = "NoScaleError",
    NoTable = "NoTableError",
    NoValidWhat = "NoValidWhatError",
    NoValidVars = "NoValidVarsError",
    NoValidRetain = "NoValidRetainError",
    NoConditionsVars = "NoConditionsVarsError",
    NoCompatibleAssignements = "NoCompatibleAssignementsError",
    WrongLength = "WrongLengthError",
    ScaleNonunequicoval = "ScaleNonunequivocalError",
    NoConditions = "NoConditionsError",
    WrongSource = "WrongSourceError",
    ScoringImport = "ScoringImportError",
    TooManyConditions = "TooManyConditionsError",
    WrongGroup = "WrongGroupError",
    WrongFormula = "WrongFormulaError",
    WrongIdVal = "WrongIdValError",
    WrongMinMax = "WrongMinMax",
    BadName = "BadNameError"
  ),
  warning = list(
    Type = "TypeWarning",
    NonExportedConditions = "NonExportedConditionsWarning",
    NonExhaustive = "NonExhaustiveWarning",
    NotSummed = "NotSummedWarning",
    NoInputNA = "NoInputNAWarning"
  ),
  message = list(
    IncompleteRange = "IncompleteRangeMessage"
  )
)


# all .warnings for functions

# .warnings <- list(
#   # stop message for not probiding data in data.frame object
#   data.frame_required = "Data should be provided in form of the 'data.frame' object.",
# 
#   # stop message for not providing the "scale" argument when it is needed
#   valid_scale_required = "Please provide valid 'scale' argument. You can only get scores for scale which was computed first by using 'compute_scores()' method",
# 
#   # stop message for when id isn't specified with keep_data set to TRUE
#   missing_id_for_keep = "While the 'keep_data' is set to TRUE, you also need to provide column name for 'id'",
# 
#   # warning for mussing values also returns for which variables the warnings are generated
#   missing_values = function(name){
#     paste0(
#       "For [", name,
#       "]: There are missing score values between minimum and maximum scores. They have been filled automatically, though have in mind that you should get more representative sample.")
#   },
# 
#   # stop message if there is not kept data and it is needed
#   need_source_data = "You tried to do something that requires your class to have kept the data.",
# 
#   # stop message if provided id is not valid
#   valid_id_required = "Please provide valid ids. All of the provided ids should be in the data.",
# 
#   # stop message if provided variable name is not valid
#   bad_var_name = "Please provide valid variable names.",
# 
#   # stop message if provided id name is not valid
#   bad_id_name = "Please provide valid column name containing ids."
# )

FQ_incomplete_message <- function(incompletes, total) {
  
  cli::cli_inform(
    message = c("i" = "There are missing raw score values between minimum and maximum raw scores. They have been filled automatically.",
                " " = "No. missing: {length(incompletes)}/{total} [{round(length(incompletes)/total*100, 2)}%]"),
    class = cli_class$message$IncompleteRange
  )  
}
    
  

GFQ_incomplete_message <- function(groups, incompletes, total) {
  incompletes <- sapply(incompletes, length)
  percentages <- sapply(seq_along(groups), \(i) paste0(round(incompletes[i]/total[i]*100, 2), "%"))
  
  cli::cli_inform(
    message = c(
      "i" = "There are missing raw score values between minimum and maximum raw scores for some groups. They have been filled automatically.",
      stats::setNames(nm = rep("*", length = length(groups)),
                      object = sapply(seq_along(groups), 
                                      \(i) paste0("{.field {groups[", i, "]}} No. missing: {incompletes[", i,"]}/{total[", i, "]}; {percentages[", i, "]}")))),
    class = cli_class$message$IncompleteRange
  )
}
  

GA_exhaustive_warning <- function()
  cli::cli_warn(c("!" = "Some observations were not assigned on provided condition. Set the {.code force_exhaustive = TRUE} to gather them in {.code .NA} group."),
                class = cli_class$warning$NonExhaustive)



#' @title Mockup NA table
#' @description Creates mockup table if none is available for given group. Used in
#' [normalize_scores_grouped()]. All results passed through it will have their values
#' "normalized" into `NA`
#' @param table Table to mockup
#' @keywords internal

mockNAtable <- function(table) {
  
  mockup <- data.frame(
    score = table$table[["score"]]
  )
  
  for(col in names(table$table)[-1:-2])
    mockup[[col]] <- NA
  
  out <- list(table = mockup)
  
  if ("scale" %in% names(table))
    out[["scale"]] <- table[["scale"]]
  
  class(out) <- class(table)
  return(out)
  
}