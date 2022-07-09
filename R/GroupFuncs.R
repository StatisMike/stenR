#' checks for formula in `GroupGonditions` and returns the variable names used
#' in the call
#' @return *character* vector with variable names
#' @importFrom cli cli_abort
#' @noRd
formula_check <- function(group_formula) {
  
  lhs_val <- rlang::f_lhs(group_formula)
  
  if (isFALSE(is.character(lhs_val)) && length(lhs_val) != 1)
    cli_abort("{.strong LHS} of all grouping formulas need to be {.emph character}",
              class = cli_class$error$WrongFormula)
  
  if (strtrim(lhs_val, 1) == ".")
    cli_abort("User-defined group name can't begin with reserved symbol {.strong {.val .}}",
              class = cli_class$error$WrongFormula)
  
  if (grepl(lhs_val, pattern = ":"))
    cli_abort("User-defined group name can't constain reserved symbol {.strong {.val .}}",
              class = cli_class$error$WrongFormula)
  
  rhs_val <- rlang::f_rhs(group_formula)
  
  if (isFALSE(is.call(rhs_val)) && length(rhs_val) != 1)
    cli_abort("{.strong RHS of all grouping formulas need to be a call to check assignment.}",
              class = cli_class$error$WrongFormula)
  
  return(all.vars(rhs_val))
}

#' @title Conditions for observation grouping
#' @description With help of this function you can create GroupingConditions
#' object, holding the basis of observation grouping. Objects of this class
#' can be provided to complex functions to automatically group observations
#' accordingly.
#' @param conditions_category *chracter* value describing character of the group 
#' conditions. Mainly informative.
#' @param ... *formulas* that will be used to determine group
#' to which the observation should be assigned to. LHS should contain name of the
#' group, and RHS: condition which can return `TRUE` or `FALSE`
#' @param force_disjoint *boolean* indicating if the condition formulas by default
#' should be handled with `force_disjoint` strategy. By default `TRUE`. 
#' If `TRUE`, the first condition which will be met will indicate
#' the group the observation will be assigned to.
#' @param force_exhaustive *boolean* indicating if groups exhaustiveness should
#' be forced in case when there are observations that don't pass any of the provided
#' conditions. If `TRUE`, then they will be assigned to `.NA` group. Defaults
#' to `FALSE`
#' @param .dots *formulas* in form of a *list*
#' @return *GroupConditions* object
#' @example man/examples/GroupConditions.R
#' @importFrom cli cli_abort
#' @export

GroupConditions <- function(
    conditions_category, 
    ..., 
    force_disjoint = TRUE, 
    force_exhaustive = FALSE,
    .dots = list()) {
  
  if (length(.dots) > 0) 
    formulas <- .dots
  else
    formulas <- list(...)
  
  formula_vars <- unique(sapply(formulas, formula_check))
  
  if (length(formula_vars) == 0)
    cli_abort("No variables are tested with specified conditions.",
              class = cli_class$error$WrongFormula)
  
  class(formulas) <- "GroupConditions"
  attr(formulas, "cond_category") <- conditions_category
  attr(formulas, "formula_vars") <- formula_vars
  attr(formulas, "force_disjoint") <- isTRUE(force_disjoint)
  attr(formulas, "force_exhaustive") <- isTRUE(force_exhaustive)
  attr(formulas, "groups") <- sapply(formulas, rlang::f_lhs)
  attr(formulas, "conditions") <- as.character(sapply(formulas, rlang::f_rhs))
  
  return(formulas)
  
}

#' @rdname GroupConditions
#' @param x object
#' @param ... additional arguments to be passed to or from method
#' @importFrom cli cli cli_text cli_li
#' @export

print.GroupConditions <- function(x, ...) {
  cli::cli({
    cli_text("{.cls GroupConditions}")
    cli_text("Conditions category: {.strong {attr(x, 'cond_category')}}")
    cli_text("Tested variables: {.val {attr(x, 'formula_vars')}}")
    cli_text("{.strong {.val {length(unique(attr(x, 'groups')))}} Groups}:")
    cli_li(paste0("{.field ", attr(x, "groups"), "} IF: {.emph ", attr(x, "conditions"), "}"))
    cli_text("{.strong Forced disjointedness} by default: {.val {attr(x, 'force_disjoint')}}")
    cli_text("{.strong Forced exhaustiveness} by default: {.val {attr(x, 'force_exhaustive')}}")
    })
}

#' @rdname GroupConditions
#' @param x `GroupConditions` object
#' @param ... additional arguments to be passed to or from methods.
#' @export
#' 
as.data.frame.GroupConditions <- function(x, ...) {
  
  data.frame(
    category = attr(x, "cond_category"),
    group = attr(x, "groups"),
    conditions = attr(x, "conditions")
  )

} 

#' @title Assign to groups based on GroupConditions
#' @description Using *GroupConditions* object, assign observations to one
#' of the groups. It can export either indices of the observations, or their
#' unique **ID**: if column name is provided in `id` argument. Mostly used internally
#' by more complex functions and `R6 classes`, but could also be useful
#' on its own.
#' @param data data.frame containing observations
#' @param conditions *GroupConditions* object
#' @param id *character* name of the column containing unique **ID** of the
#' observations to assign to each group. If not provided, indices 
#' will be used instead.
#' @param force_disjoint *boolean* indicating if groups disjointedness should be 
#' forced in case when one observation would pass conditions for more than one
#' group. If `TRUE`, the first condition which will be met will indicate
#' the group the observation will be assigned to. If not provided, the default
#' from `conditions` will be used
#' @param force_exhaustive *boolean* indicating if groups exhausiveness should
#' be forced in case when there are observations that don't pass any of the provided
#' conditions. If `TRUE`, then they will be assigned to `.NA` group. If not provided, the default
#' from `conditions` will be used
#' @param skip_faulty *boolean* should the faulty `condition` be skipped? 
#' If `FALSE` as in default, error will be produced. Faultiness of seemingly correct
#' condition may be caused by variable names to not be present in the `data`.
#' @param .all *boolean*. If `TRUE`, then additional group named `.all`
#' will be created, which will contain all observations. Useful when object will be
#' used for creation of [GroupedFrequencyTable()]
#' @param ... Do not set - used internally
#' 
#' @family observation grouping functions
#' @example man/examples/GroupAssignment.R
#' @importFrom cli cli_abort
#' @export
#' @return *GroupAssignment* object

GroupAssignment <- function(data, 
                            conditions, 
                            id,
                            force_disjoint, 
                            force_exhaustive,
                            skip_faulty = FALSE,
                            .all = FALSE,
                            ...) {

  # checks
  if (!is.data.frame(data))
    cli_abort("Data provided should be a {.cls data.frame}.",
              class = cli_class$error$Class)
  if (!is.GroupConditions(conditions))
    cli_abort("{.cls GroupConditions} object should be provided to {.var conditions} argument.",
              class = cli_class$error$Class)
  if (!all(attr(conditions, "formula_vars") %in% names(data)))
    cli_abort("Not all variables tested in provided {.cls GroupConditions} are available in the {.val data}.",
              class = cli_class$error$NoConditionsVars)
  
  # mode 
  if (!missing(id)) {
    if (!is.character(id) || length(id) != 1 || !id %in% names(data))
      cli_abort("Value provided to {.var id} argument: {.val {id}} should be name of one column in the {.var data}",
                class = cli_class$error$WrongIdVal)
    if (nrow(data) != length(unique(data[[id]])))
      cli_abort("Values of {.code {data}[[{id}]]} are not unique.",
                class = cli_class$error$WrongIdVal)
    group_mode <- "id"
  } else {
    group_mode <- "index"
  }
  
  # disjointedness & exhaustiveness
  if (missing(force_disjoint))
    force_disjoint <- attr(conditions, "force_disjoint")
  if (missing(force_exhaustive))
    force_exhaustive <- attr(conditions, "force_exhaustive")
  
  # state observe
  assigned_i <- c()
  are_disjoint <- TRUE
  are_exhaustive <- TRUE
  
  # main extraction
  out <- list()
  
  for (form in conditions) {
    
    out_i <- list()
    out_i[["group"]] <- rlang::f_lhs(form)
    
    # catch every problem with conditions
    out_bool <- tryCatch(
      eval(rlang::f_rhs(form), 
           envir = data[, attr(conditions, "formula_vars"), drop = F]),
    error = function(e)  e$message )
    
    if (!is.logical(out_bool) && !isTRUE(skip_faulty))
      stop(out_bool)
    if (!is.logical(out_bool) && isTRUE(skip_faulty))
      return(numeric(0))
    
    if (group_mode == "index")
      out_i[["els"]] <- which(out_bool)
    else if (group_mode == "id")
      out_i[["els"]] <- data[[id]][out_bool]
    
    if (isTRUE(force_disjoint)) {
      out_i[["els"]] <- out_i[["els"]][!out_i[["els"]] %in% assigned_i]
    }
    
    if (isTRUE(are_disjoint) && any(out_i[["els"]] %in% assigned_i))
      are_disjoint <- FALSE
    
    assigned_i <- unique(c(assigned_i, out_i[["els"]]))
    
    # check if this group name is already present
    cur_group <- which(sapply(out, \(x) x$group) == out_i[["group"]])
    
    if (length(cur_group) == 1)
      out[[cur_group]][["els"]] <- unique(c(out[[cur_group]][["els"]], out_i[["els"]]))
    else
      out <- c(out, list(out_i))

  }
  
  add <- list(...)
  
  # if some were not assigned, create additional group
  if (length(unique(assigned_i)) != nrow(data)) {
    if (isTRUE(force_exhaustive) || isTRUE(add$na_as_all)) {
      
      out_i <- list()
      
      if (isTRUE(add$na_as_all))
        out_i[["group"]] <- ".all"
      else
        out_i[["group"]] = ".NA"
      
      if (group_mode == "index")
        out_i[["els"]] <- which(!1:nrow(data) %in% assigned_i)
      else if (group_mode == "id")
        out_i[["els"]] <- data[[id]][which(!data[[id]] %in% assigned_i)]
      assigned_i <- c(assigned_i, out_i[["els"]])
      
      out <- c(out, list(out_i))
    } else {
      exhaustive <- FALSE
      GA_exhaustive_warning()
    }
  }
  
  if (isTRUE(.all)) {
    
    out_i <- list(group = ".all")
    if (group_mode == "index")
      out_i[["els"]] <- 1:nrow(data)
    else if (group_mode == "id")
      out_i[["els"]] <- data[[id]]
    
    out <- c(out, list(out_i))
    
  }
  
  # finalize
  attr(out, "mode") <- group_mode
  if (group_mode == "id")
    attr(out, "id_col") <- id
  attr(out, "formula_vars") <- attr(conditions, "formula_vars")
  attr(out, "force_disjoint") <- isTRUE(force_disjoint)
  attr(out, "disjoint") <- are_disjoint
  attr(out, "force_exhaustive") <- isTRUE(force_exhaustive)
  attr(out, "exhaustive") <- are_exhaustive
  attr(out, "total") <- length(assigned_i)
  class(out) <- "GroupAssignment"
  return(out)
  
}

#' @rdname GroupAssignment
#' @param x object
#' @param ... additional arguments to be passed to or from method
#' @importFrom cli cli cli_text
#' @export

print.GroupAssignment <- function(x, ...) {
  
  groups <- sapply(x, \(y) paste(y$group, collapse = ":"))
  
  cli({
    if (is.intersected(x))
      cli_text("{.strong intersected} {.cls GroupAssignment}")
    else
      cli_text("{.cls GroupAssignment}")
    cli_text("Total assigned: {.val {attr(x, 'total')}}")
    cli_text("Mode: {.val {attr(x, 'mode')}}")
    cli_text("{.field Groups}:")
    cli_text("{.val {groups}}")
  })
}

#' @rdname GroupAssignment
#' @param object `GroupAssignment` object
#' @param ... additional arguments to be passed to or from method
#' @return list of summaries, invisibly
#' @importFrom cli cli cli_inform cli_li cli_text
#' @export

summary.GroupAssignment <- function(object, ...) {
  
  summaries <- list(
    mode = attr(object, "mode"),
    id_col = attr(object, "id_col"),
    total = attr(object, "total"),
    disjoint = attr(object, "disjoint"),
    forced_disjoint = attr(object, "force_disjoint"),
    exhaustive = attr(object, "exhaustive"),
    forced_exhaustive = attr(object, "force_exhaustive"),
    groups = stats::setNames(
      nm = sapply(object, \(x) paste(x$group, collapse=":")),
      object = sapply(object, \(x) length(x$els)))
  )
  
  cli({
    if (is.intersected(object))
      cli_text("{.strong intersected} {.cls GroupAssignment}")
    else
      cli_text("{.cls GroupAssignment}")
    cli_text("{.strong Status}")
    cli_li(paste0("{.field Mode}: {.strong {summaries$mode}}",
                    if (summaries$mode == "id") " [default ID: {.var {summaries$id_col}}]"))
    cli_li("{.field Total assigned}: {.val {summaries$total}}")
    cli_li("{.field Disjointedness}: {.val {summaries$disjoint}}; {.strong Forced}: {.val {summaries$forced_disjoint}}")
    cli_li("{.field Exhaustiveness}: {.val {summaries$exhaustive}}; {.strong Forced}: {.val {summaries$forced_exhaustive}}")
    cli_text("{.strong Assignment} [{.emph tested vars:} {.var {attr(object, 'formula_vars')}}]")
    for (i in seq_along(summaries$groups)) 
      cli_li("Group: {.strong {names(summaries$groups[i])}} [obs: {.val {summaries$groups[i]}}]")
  })

  return(invisible(summaries))
  
}

#' @title Intersect two GroupAssignment
#' @description You can intersect two GroupAssignment with this function.
#' @param GA1,GA2 *GroupAssignment* objects to intersect. No previously intersected
#' objects can be intersected again.
#' @param force_disjoint *boolean* indicating if groups disjointedness should be 
#' forced in case when one observation would end in multiple intersections. 
#' If `TRUE`, observation will remain only in the first intersection to which 
#' it will be assigned. Default to `TRUE`.
#' @param force_exhaustive *boolean* indicating if elements that are not assigned
#' to any of the intersecting groups should be gathered together in `.NA:.NA` group
#' 
#' @return *GroupAssignment* object with intersected groups.
#' @family observation grouping functions
#' @importFrom cli cli_abort
#' @example man/examples/intersect_GroupAssignment.R
#' @export

intersect_GroupAssignment <- function(
    GA1, 
    GA2, 
    force_disjoint = TRUE, 
    force_exhaustive = FALSE) {
  
  if(any(!is.GroupAssignment(GA1), !is.GroupAssignment(GA2)))
    cli_abort("Both {.var GA1} and {.var GA2} need to be {.cls GroupAssignment} objects.",
              class = cli_class$error$Class)
  
  if(is.intersected(GA1) || is.intersected(GA2))
    cli_abort("{.cls GroupAssignement} can't be intersected twice.",
              class = cli_class$error$Class)
  
  # make sure that the mode in both GroupAssignment is the same
  if (attr(GA1, "mode") != attr(GA2, "mode"))
    cli_abort("{.val mode} of both {.cls GroupAssignment} objects need to be the same.",
              class = cli_class$error$NoCompatibleAssignements)
  
  # if 'mode' is id, then check the defalt id col
  if (attr(GA1, "mode") == "id") {
    if (attr(GA1, "id_col") != attr(GA2, "id_col")) {
      cli_abort("Default of {.val id} in both {.cls GroupAssignment} objects need to be the same.",
                class = cli_class$error$NoCompatibleAssignements)
    }
  }
  
  # fix non-user defined group names
  for (i in seq_along(GA1)) {
    if (strtrim(GA1[[i]]$group, 1) == ".")
      GA1[[i]]$group <- paste0(GA1[[i]]$group, 1)
  }
  
  for (i in seq_along(GA2)) {
    if (strtrim(GA2[[i]]$group, 1) == ".")
      GA2[[i]]$group <- paste0(GA2[[i]]$group, 2)
  }
  
  # create intersected groups
  out_groups <- 
    lapply(
      sapply(GA1, \(x) x$group, simplify = F), 
      \(y) sapply(GA2, \(x) c(y, x$group), simplify = F)) 
  
  out_groups <- unlist(out_groups, recursive = F)
  
  # state observe
  assigned_i <- c()
  are_disjoint <- TRUE
  are_exhaustive <- TRUE
  
  # get elements from interected groups
  out <- c()
  
  for (group in out_groups) {
    
    out_i <- list()
    
    els1 <- GA1[sapply(GA1, \(x) x$group == group[1])][[1]]$els
    els2 <- GA2[sapply(GA2, \(x) x$group == group[2])][[1]]$els
    
    out_i[["group"]] <- group
    out_i[["els"]] <- unique(c(els1[els1 %in% els2], els2[els2 %in% els1]))
    
    if (isTRUE(force_disjoint)) {
      out_i[["els"]] <- out_i[["els"]][!out_i[["els"]] %in% assigned_i]
    }
    
    if (isTRUE(are_disjoint) && any(out_i[["els"]] %in% assigned_i)) {
      are_disjoint <- FALSE
    }
    
    assigned_i <- unique(c(assigned_i, out_i[["els"]]))
    
    out <- c(out, list(out_i))
    
  }
  
  # if some were not assigned, create additional group
  if (length(unique(assigned_i)) < max(c(attr(GA1, "total"), attr(GA2, "total")))) {
    if (isTRUE(force_exhaustive)) {
      
      out_i <- list(group = c(".NA", ".NA"))
      
      els1 <- unique(unlist(sapply(GA1, \(x) x$els)))
      els1 <- els1[!els1 %in% assigned_i]
      els2 <- unique(unlist(sapply(GA2, \(x) x$els)))
      els2 <- els2[!els2 %in% assigned_i]
      
      out_i[["els"]] <- unique(c(els1, els2))
      
      assigned_i <- c(assigned_i, out_i[["els"]])
      
      out <- c(out, list(out_i))
    } else {
      exhaustive <- FALSE
      GA_exhaustive_warning()
    }
  }
  
  # finalize
  attr(out, "mode") <- attr(GA1, "mode")
  if (attr(out, "mode") == "id")
    attr(out, "id_col") <- attr(GA1, "id_col")
  attr(out, "formula_vars") <- unique(c(attr(GA1, "formula_vars"), attr(GA2, "formula_vars")))
  attr(out, "force_disjoint") <- isTRUE(force_disjoint)
  attr(out, "disjoint") <- are_disjoint
  attr(out, "force_exhaustive") <- isTRUE(force_exhaustive)
  attr(out, "exhaustive") <- are_exhaustive
  attr(out, "total") <- length(assigned_i)
  class(out) <- c("GroupAssignment", "Intersect")
  return(out)
}

#' @title Extract observations from data
#' @description On basis of *GroupAssignment* extract one or many groups from
#' provided data.frame
#' @param data *data.frame* from which to extract data
#' @param groups *GroupAssignment* object on basis of which extract the data.
#' @param group_names *character* vector of group names which to extract. If kept
#' as default `NULL`, all groups are extracted.
#' @param extract_mode *character*: either `list` or `data.frame`. When kept as 
#' default: `list`, data is extracted as named list: where the name of list is 
#' name of the groups, and each one contains *data.frame* with observations. 
#' When `data.frame` is used, then assigned data is returned as one *data.frame* 
#' with new column named: `GroupAssignment`, declaring the group.
#' @param strict_names *boolean* If `TRUE`, then intersected groups are extracted
#' using *strict* strategy: `group_names` need to be provided in form: `"group1:group2"`. If
#' `FALSE`, then intersected groups will be taken into regard separately, so 
#' eg. when `"group1"` is provided to `group_names`, all of: `"group1:group2"`, 
#' `"group1:group3"`, `"group1:groupN"`  will be extracted. Defaults to `TRUE`
#' @param simplify *boolean* If `TRUE`, then when only one group is to be
#' returned, it returns as `data.frame` without taking into account value of
#' `group_name` argument. Defaults to `FALSE`
#' @param id If *GroupAssignment* mode is `id`, and you want to overwrite the
#' original `id_col`, provide a name of the column there. If none is provided,
#' then the default `id_col` will be used.
#' @return either:
#'   - *named list* of *data.frames* if `extract_mode = 'list'`
#'   - *data.frame* if `extract_mode = 'data.frame'` or if only one group is to be
#'   returned and `simplify = TRUE`
#' @family observation grouping functions 
#' @example man/examples/extract_observations.R
#' @importFrom cli cli_abort
#' @export

extract_observations <- function(
    data, 
    groups, 
    group_names = NULL,
    extract_mode = c("list", "data.frame"),
    strict_names = TRUE,
    simplify = FALSE,
    id) {
  
  extract_mode <- match.arg(extract_mode)
  
  # basic checks ####
  if (!is.data.frame(data))
    cli_abort("{.var data} needs to be a {.cls data.frame} object.",
              class = cli_class$error$Class)
  if (!is.null(group_names) && (!is.character(group_names) || length(group_names) == 0))
    cli_abort("{.var group_names} need to be a {.emph character} vector.",
              class = cli_class$error$Type)
  if (!is.GroupAssignment(groups))
    cli_abort("{.cls GroupAssignment} object need to be provided to {.var groups} argument.",
              class = cli_class$error$Class)

  # if no name is provided, extract all groups with strict name strategy
  if (is.null(group_names)) {
    group_names <- sapply(groups, \(x) paste(x$group, collapse = ":"))
    stric_names <- TRUE
  }
  
  # preparation for group extraction ####
  ## strict names strategy
  if (isTRUE(strict_names)) 
    groups_to_extract <- sapply(groups, \(x) paste(x$group, collapse = ":") %in% group_names)
  ## not strict names strategy
  else 
    groups_to_extract <- sapply(groups, \(x) any(x$group %in% group_names)) 
  ## additional check
  if (sum(groups_to_extract) == 0)
    cli_abort("No group matches the provided {.var group_names}: {.val {group_names}}.",
              class = cli_class$error$WrongGroup)

  if (attr(groups, "mode") == "id" && missing(id))
    id <- attr(groups, "id_col")
  
  # main extraction ####
  extracted_obs <- lapply(groups[groups_to_extract], \(grp) {
    
    ## if group mode is ID
    if (attr(groups, "mode") == "id") {
      if (!id %in% names(data))
        cli_abort("ID column: {.val {id}} is not present in the provided data.",
                  class = cli_class$error$WrongIdVal)
      
      grp_ind <- which(data[[attr(groups, "id_col")]] %in% grp$els)
    } else {
      grp_ind <- grp$els
    }
    
    data[grp_ind, ]
  })
  
  names(extracted_obs) <- sapply(groups[groups_to_extract], \(grp) paste(grp$group, collapse = ":"))
  
  # simplify if possible
  if (length(extracted_obs) == 1 && isTRUE(simplify))
    return(extracted_obs[[1]])
  
  # extract in chosen mode
  if (extract_mode == "list")
    return(extracted_obs)
  if (extract_mode == "data.frame") {
    extracted_obs <- data.table::rbindlist(extracted_obs, idcol = "GroupAssignment")
    return(as.data.frame(extracted_obs))
  }
}
