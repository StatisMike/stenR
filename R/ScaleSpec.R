#' @title Scale Specification object
#' @description Object containing scale or factor specification data. It describes
#' the scale or factor, with regard to which items from the source data are part
#' of it, which need to be summed with reverse scoring, and how to handle `NA`s.
#' To be used with [sum_items_to_scale()] function to preprocess item data.
#' @param name character with name of the scale/factor
#' @param item_names character vector containing names of the items that the
#' scale/factor consists of. 
#' @param min,max integer containing the default minimal/maximal value that the
#' answer to the item can be scored as. 
#' @param reverse character vector containing names of the items that need to be
#' reversed during scale/factor summing. Reversed using the default `"min"` and
#' `"max"` values.
#' @param na_strategy character vector specifying which strategy should be taken
#' during filling of `NA`. Defaults to `"asis"` and, other options are `"mean"`, 
#' `"median"` and `"mode"`. Strategies are explained in the details section. 
#' @param na_value integer value to be input in missing values as default.
#' Defaults to `as.integer(NA)`.
#' @param na_value_custom if there are any need for specific questions be gives
#' specific values in place of `NA`s, provide a named integer vector there. Names
#' should be the names of the questons.
#' @details 
#' 
#' ## NA imputation
#' it specifies how `NA` values should be treated during [sum_items_to_scale()]
#' function run.
#' **asis** strategy is literal: the values specified in `na_value` or `na_value_custom` 
#' will be used without any changes.
#' **mean**, **median** and **mode** are functional strategies. They work on a 
#' rowwise basis, so the appropriate value for every observation will be used.
#' If there are no values provided to check for the *mean*, *median* or *mode*,
#' the value provided in `na_value` or `na_value_custom` will be used. The 
#' values of *mean* and *median* will be rounded before imputation. 
#' 
#' ## Order of operations
#' - item reversion
#' - functional `NA`s imputation
#' - literal `NA`s imputation  
#' 
#' @return object of `ScaleSpec` class
#' @example man/examples/ScaleSpec.R
#' @family item preprocessing functions
#' @rdname ScaleSpec
#' @importFrom cli cli_abort
#' @export
#' 
ScaleSpec <- function(
    name,
    item_names,
    min,
    max,
    reverse = character(0),
    na_strategy = c("asis", "mean", "median", "mode"),
    na_value = as.integer(NA),
    na_value_custom) {
  
  na_strategy <- match.arg(na_strategy)
  
  if (min >= max)
    cli_abort("{.var min} needs to be smaller than {.var max}",
              class = cli_class$error$WrongMinMax)
  if (min < 0 || max < 0)
    cli_abort("Only non-negative {.var min} and {.var max} are supported.",
              class = cli_class$error$WrongMinMax)
  if (length(reverse) > 0 && !is.character(reverse))
    cli_abort("For {.var reverse} argument a character vector should be specified.",
              class = cli_class$error$Type)
  
  out <- list(
    name = name,
    item_names = item_names,
    min = min,
    max = max,
    reverse = character(0),
    na_strategy = na_strategy,
    na_value = as.numeric(na_value))
  
  class(out) <- "ScaleSpec"
  
  if (length(reverse) > 0) {
    
    out[["reverse"]] <- reverse
    
    rev_missing <- out[["reverse"]][!out[["reverse"]] %in% out[["item_names"]]]
    
    if (length(rev_missing) > 0)
      cli_abort("There are some item names specified in {.var reverse} that are missing from {.var item_names}: {.val {rev_missing}}.",
                class = cli_class$error$NoValidVars)
  }
  
  if (!missing(na_value_custom)) {
    if (any(sapply(names(na_value_custom), \(x) is.null(x))) || !is.numeric(na_value_custom))
      cli_abort("Character vector provided to {.var na_value_custom} needs to be named.",
                 class = cli_class$error$Class)
    
    na_value_names_missing <- names(na_value_custom)
    na_value_names_missing <- na_value_names_missing[!na_value_names_missing %in% item_names]
    
    if (length(na_value_names_missing) > 0)
      cli_abort("There are some item names specified in {.var na_value_custom} that are missing from {.var item_names}: {.val {na_value_names_missing}}.",
                class = cli_class$error$NoValidVars)
    
    out[["na_value_custom"]] <- na_value_custom
  }
  
  return(out)
  
}

#' @rdname ScaleSpec
#' @param x a `ScaleSpec` object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli cli_text
#' @export
print.ScaleSpec <- function(x, ...) {
  
  cli({
    cli_text("{.cls ScaleSpec}: {.strong {x$name}}")
    cli_text(paste0("{.field No. items}: {.val {length(x$item_names)}}"),
             if (length(x$reverse) > 0) " [{.val {length(x$reverse)}} reversed]")
  })
  
}

#' @rdname ScaleSpec
#' @param object a `ScaleSpec` object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_text cli_ol cli_li cli_end 
#' @return data.frame of item names, if they are reversed, and custom NA value if available, invisibly
#' @export
summary.ScaleSpec <- function(object, ...) {
  
  items <- data.frame(
    item_name = object$item_names,
    reversed = sapply(object$item_names, \(name) name %in% object$reverse),
    custom_na = sapply(object$item_names, \(name) if (is.null(object$custom_na[[name]])) NA 
                                                else object$custom_na[[name]])
  )
  
  row.names(items) <- NULL
  
  cli_text("{.cls ScaleSpec}: {.strong {object$name}}")
  cli_text("{.strong min}: {.val {object$min}}; {.strong max}: {.val {object$max}}")
  cli_text("{.field NA imputation method}: {.val {object$na_strategy}}")
  cli_text("{.field NA literal value}: {.val {object$na_value}}")
  ol <- cli_ol()
  for (item_i in seq_along(items$item_name))
    cli_li(
      paste0(
        "{.emph {items$item_name[item_i]}}",
        ifelse(items$reversed[item_i], " {.bold reversed}", ""),
        ifelse(!is.na(items$custom_na[item_i]), " {.field custom NA}: {.val items$custom_na}", "")
      )
    )
  cli_end(ol)
  
  return(invisible(items))
}

#' @title Combined Scale Specification
#' @description
#' Combine multiple `ScaleSpec` objects into one in regards of [sum_items_to_scale()]
#' function. Useful when one scale of factor contains items of different possible
#' values or if there is hierarchy of scale or factors.
#' 
#' Also allows combining `CombScaleSpec` object if the factor structure have deeper
#' hierarchy.
#' 
#' @param name Name of the combined scale or factor
#' @param ... `ScaleSpec` or other `CombScaleSpec` objects
#' @param reverse character vector containing names of the underlying subscales
#' or factors that need to be reversed
#' @family item preprocessing functions
#' @return `CombScaleSpec` object
#' @example man/examples/CombScaleSpec.R
#' @rdname CombScaleSpec
#' @importFrom cli cli_abort
#' @export
CombScaleSpec <- function(name, ..., reverse = character(0)) {
  
  out <- list(name = name,
              ScaleSpecs = list(...),
              reverse = reverse)
  
  class(out) <- "CombScaleSpec"
  
  if (any(sapply(out$ScaleSpecs, \(x) !is.ScaleSpec(x) && !is.CombScaleSpec(x))))
    cli_abort("Objects of class {.cls ScaleSpec} or {.cls CombScaleSpec} need to be provided in {.var ...} argument.",
              class = cli_class$error$Class)
  if (length(reverse) > 0 && !is.character(reverse))
    cli_abort("For {.var reverse} argument a character vector should be specified.",
              class = cli_class$error$Type)
  else if (length(reverse) > 0 && any(!reverse %in% sapply(out$ScaleSpecs, \(spec) spec$name)))
    cli_abort("Not all names provided in {.val reverse} argument are refering to the names of provided objects.",
              class = cli_class$error$NoScale)

  out[["item_names"]] <- lapply(out$ScaleSpecs, \(spec) {
    spec$item_names
  })
  
  out[["item_names"]] <- unlist(out[["item_names"]])
  
  out[["min"]] <- sapply(out$ScaleSpecs, \(spec) {
    if (is.ScaleSpec(spec))
      spec$min * length(spec$item_names)
    else if (is.CombScaleSpec(spec))
      spec$min
  })
  
  out[["min"]] <- sum(out[["min"]])
  
  out[["max"]] <- sapply(out$ScaleSpecs, \(spec) {
    if (is.ScaleSpec(spec))
      spec$max * length(spec$item_names)
    else if (is.CombScaleSpec(spec))
      spec$max
  })
  
  out[["max"]] <- sum(out[["max"]])
  
  return(out)
  
} 

#' @rdname CombScaleSpec
#' @param x a *CombScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_text cli_ol cli_end
#' @export
print.CombScaleSpec <- function(x, ...) {
  
    cli_text("{.cls CombScaleSpec}: {.strong {x$name}}")
    cli_text("{.field Total items}: {.val {length(x$item_names)}}")
    cli_text("{.strong Underlying objects}:")
    ol <- cli_ol()
    for (spec in x$ScaleSpecs)
      cli_li("{.cls {class(spec)}} {.strong {spec$name}} [{.field No.items}: {.val {length(spec$item_names)}}]")
    cli_end(ol)

}

#' @rdname CombScaleSpec
#' @param object a *CombScaleSpec* object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_text cli_ol cli_li cli_ul cli_end  
#' @export
summary.CombScaleSpec <- function(object, ...) {
  
  suppressMessages(
    items_ls <- lapply(object$ScaleSpec, summary)
  )
  
  items_ls <- list()
  
  cli_text("{.cls CombScaleSpec}: {.strong {object$name}}")
  cli_text("{.strong Underlying objects}:")
  
  ol <- cli_ol()
  for (spec in object$ScaleSpecs) {
    cli_li("{.strong {spec$name}}")
    ul <- cli_ul()
    cli_li("{.field Class}: {.cls {class(spec)}}")
    cli_li("{.field Items}: {.val {spec$item_names}}")
    if (length(spec$reverse) > 0)
      cli_li("{.field Reversed}: {.val {spec$reverse}}")
    cli_end(ul)
  }
  cli_end(ol)
  
  names(items_ls) <- sapply(object$ScaleSpecs, \(spec) spec$name)
  items_df <- dplyr::bind_rows(items_ls, .id = "scale")
  
  return(invisible(items_df))
  
}

listinize_spec <- function(spec, ...) {
  UseMethod("listinize_spec", spec)
}

listinize_spec.ScaleSpec <- function(spec, ...) {
  
  out <- lapply(names(spec), \(x) spec[[x]])
  names(out) <- names(spec)
  out[["class"]] <- "ScaleSpec"
  return(out)
  
}

listinize_spec.CombScaleSpec <- function(spec, ...) {
  
  field_names <- c("name", "reverse", "item_names", "min", "max")
  
  out <- lapply(field_names, \(x) spec[[x]])
  names(out) <- field_names
  out[["class"]] <- "CombScaleSpec"
  
  # for loop, because lapply gives an 'non applicable method' error
  for (i in seq_along(spec[["ScaleSpecs"]]))
    out[["ScaleSpecs"]][[i]] <- listinize_spec(spec[["ScaleSpecs"]][[i]])
  
  return(out)
  
}

objectifize_spec <- function(x) {
  
  out <- switch(
    x[["class"]],
    ScaleSpec = objectifize_ScaleSpec(x),
    CombScaleSpec = objectifize_CombScaleSpec(x)
  )
  
  return(out)
  
}

objectifize_ScaleSpec <- function(x) {
  
  spec <- do.call("ScaleSpec",
                  x[!grepl("class", names(x))])
  
  return(spec)
  
}

objectifize_CombScaleSpec <- function(x) {

  ScaleSpecs <- lapply(x[["ScaleSpecs"]], objectifize_spec)
  
  args <- list(name = x[["name"]],
               reverse = x[["reverse"]])
  
  args <- c(args, ScaleSpecs)
  
  spec <- do.call("CombScaleSpec",
                  args)
  
  return(spec)
  
}

#' @title Export scale specification
#' @description Function to export ScaleSpec or CombScaleSpec object into json file
#' which can be imported by [import_ScaleSpec()]
#' @param spec ScaleSpec or CombScaleSpec object to export
#' @param out_file path to output file
#' @family import/export functions
#' @example man/examples/import_export_ScaleSpec.R
#' @export
export_ScaleSpec <- function(spec, out_file) {
  
  rlang::check_installed("jsonlite")
  
  out <- listinize_spec(spec)
  jsonlite::write_json(out, out_file)
  
}

#' @title Import scale specification
#' @description Function to import ScaleSpec or CombScaleSpec object from json file
#' that havebeen exported with [export_ScaleSpec()]
#' @param source path to JSON file containing exported object
#' @family import/export functions
#' @example man/examples/import_export_ScaleSpec.R
#' @export
import_ScaleSpec <- function(source) {
  
  rlang::check_installed("jsonlite")
  
  out <- jsonlite::read_json(source, simplifyVector = T, simplifyDataFrame = F, simplifyMatrix = F)
  
  out <- objectifize_spec(out)
  
  return(out)
  
}
