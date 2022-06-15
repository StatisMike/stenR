#' checks for formula in `GroupGonditions` and returns the variable names used
#' in the call
#' @return *character* vector with variable names
#' @noRd
formula_check <- function(group_formula) {
  
  lhs_val <- rlang::f_lhs(group_formula)
  
  if (isFALSE(is.character(lhs_val)) && length(lhs_val) != 1)
    stop("LHS of all grouping formulas need to be character string.")
  if (strtrim(lhs_val, 1) == ".")
    stop("User-defined group name can't begin with reserved symbol'.'")
  if (grepl(lhs_val, pattern = ":"))
    stop("User-defined group name can't contain reserveds symbol ':'")
  
  rhs_val <- rlang::f_rhs(group_formula)
  
  if (isFALSE(is.call(rhs_val)) && length(rhs_val) != 1)
    stop("RHS of all grouping formulas need to be a call to check assignment.")
  
  return(all.vars(rhs_val))
}

#' @title Conditions for observation grouping
#' @description With help of this function you can create GroupingConditions
#' object, holding the basis of observation grouping. Objects of this class
#' can be provided to complex functions to automatically group observations
#' accordingly.
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
#' @return *GroupConditions* object
#' @example examples/GroupConditions.R
#' @export

GroupConditions <- function(..., force_disjoint = TRUE, force_exhaustive = FALSE) {
  
  formulas <- list(...)
  
  formula_vars <- unique(sapply(formulas, formula_check))
  
  if (length(formula_vars) == 0)
    stop("No variables are tested with specified conditions.")
  
  class(formulas) <- "GroupConditions"
  attr(formulas, "formula_vars") <- formula_vars
  attr(formulas, "force_disjoint") <- isTRUE(force_disjoint)
  attr(formulas, "force_exhaustive") <- isTRUE(force_exhaustive)
  attr(formulas, "groups") <- sapply(formulas, rlang::f_lhs)
  attr(formulas, "conditions") <- as.character(sapply(formulas, rlang::f_rhs))
  
  return(formulas)
  
}

#' @rdname GroupConditions
#' @param x object
#' @export

is.GroupConditions <- function(x) {
  inherits(x, "GroupConditions")
}

#' @rdname GroupConditions
#' @param x object
#' @export

print.GroupConditions <- function(x) {
  cat("<GroupConditions>\n")
  cat("For", length(unique(attr(x, "groups"))), "unique groups\n\n")
  cat("Conditions ")
  cat("[Tested vars: ", paste(attr(x, "formula_vars"), collapse = ", "), "]:\n", sep = "")
  for (i in seq_along(attr(x, "groups"))) {
    cat("Group: [")
    cat(attr(x, "groups")[i])
    cat("] IF: [")
    cat(attr(x, "conditions")[i])
    cat("]\n")
  }
  cat("\nForced disjointedness by default: ")
  cat(attr(x, "force_disjoint"))
  cat("\n")
  cat("Forced exhaustiveness by default: ")
  cat(attr(x, "force_exhaustive"))
  cat("\n")
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
#' @example examples/GroupAssignment.R
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
  
  # browser()
  
  # checks
  if (!is.data.frame(data))
    stop("Data provided should be in `data.frame` form.")
  if (!is.GroupConditions(conditions))
    stop("Object of class `GroupConditions` should be provided to `conditions` argument")
  if (!all(attr(conditions, "formula_vars") %in% names(data)))
    stop("Not all variables tested in provided GroupCondition are available in the data.")
  
  # mode 
  if (!missing(id)) {
    if (!is.character(id) || length(id) != 1 || !id %in% names(data))
      stop ("`id` should be a name of one column in the `data`")
    if (nrow(data) != length(unique(data[[id]])))
      stop ("`id` values are not unique")
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
      stop(e)
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
      warning("Some observations were not assigned on provided condition.",
              " Set the `force_exhaustive` to `TRUE` to gather them in `.NA` group.")
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

is.GroupAssignment <- function(x) {
  inherits(x, "GroupAssignment")
}

#' @rdname GroupAssignment
#' @param x object
#' @export

print.GroupAssignment <- function(x) {
  cat("<GroupAssignment>\n")
  cat("Total assigned:", attr(x, "total"), "\n")
  cat("Mode:", attr(x, "mode"), "\n")
  cat("Groups: '", paste(sapply(x, \(y) paste(y$group, collapse = ":")), collapse = "', '"), "'\n", sep = "")
}

#' @rdname GroupAssignment
#' @param x object
#' @export

summary.GroupAssignment <- function(x) {
  
  summaries <- list(
    mode = attr(x, "mode"),
    id_col = attr(x, "id_col"),
    total = attr(x, "total"),
    disjoint = attr(x, "disjoint"),
    forced_disjoint = attr(x, "force_disjoint"),
    exhaustive = attr(x, "exhaustive"),
    forced_exhaustive = attr(x, "force_exhaustive"),
    groups = setNames(
      nm = sapply(x, \(y) paste(y$group, collapse=":")),
      object = sapply(x, \(y) length(y$els)))
  )
  cat("<GroupAssignment>\n")
  cat("Status:\n")
  cat("Mode: ", summaries$mode, sep = "",
      if (summaries$mode == "id") 
        paste("; default ID column:", summaries$id_col), "\n")
  cat("Total assigned:", summaries$total, "\n")
  cat("Disjointedness: ", summaries$disjoint, 
      "; Forced: ", summaries$forced_disjoint, "\n", sep = "")
  cat("Exhaustiveness: ", summaries$exhaustive, 
      "; Forced: ", summaries$forced_exhaustive, "\n\n", sep = "")
  
  cat("Assignment ")
  cat("[Tested vars: ", paste(attr(x, "formula_vars"), collapse = ", "), "]:\n", sep = "")
  for (i in seq_along(summaries$groups)) {
    cat("Group: [")
    cat(names(summaries$groups)[i])
    cat("] number of obs: [")
    cat(summaries$groups[i])
    cat("]\n")
  }
  
  
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
#' @example examples/intersect_GroupAssignment.R
#' @export

intersect_GroupAssignment <- function(
    GA1, 
    GA2, 
    force_disjoint = TRUE, 
    force_exhaustive = FALSE,
    .all = FALSE) {
  
  if(any(!is.GroupAssignment(GA1), !is.GroupAssignment(GA2)))
    stop("Both 'GA1' and 'GA2' need to be of the 'GroupAssignment' class.")
  
  if(any(inherits(GA1, "Intersect"), inherits(GA2, "Intersect")))
    stop("Only GroupAssignments that haven't been created by intersection can be intersected")
  
  # make sure that the mode in both GroupAssignment is the same
  if (attr(GA1, "mode") != attr(GA2, "mode"))
    stop("`mode` of both GroupAssignments need to be the same to intersect them.")
  
  # if 'mode' is id, then check the defalt id col
  if (attr(GA1, "mode") == "id") {
    if (attr(GA1, "id_col") != attr(GA2, "id_col")) {
      stop("Name of default id column in both GroupAssignments need to be the same!")
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
      warning("Some observations were not assigned on provided condition.",
              " Set the `force_exhaustive` to `TRUE` to gather them in `.NA` group.")
    }
  }
  
  # if (isTRUE(.all)) {
  #   
  #   out_i <- list(group = ".all:.all",
  #                 els = unique(c(unlist(sapply(GA1, \(x) x$els)),
  #                                unlist(sapply(GA2, \(x) x$els)))
  #                              )
  #                 )
  #   out <- c(out, list(out_i))
  #   
  # }
  
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
#' @export

extract_observations <- function(
    data, 
    groups, 
    group_names = NULL,
    extract_mode = "list",
    strict_names = TRUE,
    simplify = FALSE,
    id) {
  
  # basic checks ####
  if (!is.data.frame(data))
    stop("Data provided should be in `data.frame` form.")
  if (!is.character(group_names) || length(group_names) == 0)
    stop("At least one name in 'group_names' need to be provided")
  if (!extract_mode %in% c("list", "data.frame"))
    stop("Either 'list' or 'data.frame' need to be provided in 'extract_mode'.")
  if (!is.GroupAssignment(groups))
    stop("Object of class 'GroupAssignment' need to be provided to 'groups' argument")
  
  # preparation for group extraction ####
  ## strict names strategy
  if (isTRUE(strict_names)) 
    groups_to_extract <- sapply(groups, \(x) paste(x$group, collapse = ":") %in% group_names)
  ## not strict names strategy
  else 
    groups_to_extract <- sapply(groups, \(x) any(x$group %in% group_names)) 
  ## additional check
  if (sum(groups_to_extract) == 0)
    stop("No group matches the provided 'group_names'.")
  
  if (attr(groups, "mode") == "id" && missing(id))
    id <- attr(groups, "id_col")
  
  # main extraction ####
  extracted_obs <- lapply(groups[groups_to_extract], \(grp) {
    
    ## if group mode is ID
    if (attr(groups, "mode") == "id") {
      if (!id %in% names(data))
        stop ("ID column: '", id, "' is not present in the 'data' provided.")
      
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