#' Checkers for stenR S3 and R6 classes
#' @name is_stenR_classes
#' @rdname is_stenR_classes
#' @param x any \R object
#' @description 
#' Various functions to check if given \R object is of given class
#' @aliases is.GroupConditions is.GroupAssignment is.intersected_GroupAssignment 
#' is.ScaleSpec is.CombScaleSpec is.FrequencyTable is.GroupedFrequencyTable 
#' is.ScoreTable is.GroupedScoreTable is.ScoringTable is.StandardScale
NULL

#### GroupFuncs.R checkers ####

#' @rdname is_stenR_classes
#' @export

is.GroupConditions <- function(x) {
  inherits(x, "GroupConditions")
}

#' @rdname is_stenR_classes
#' @export
is.GroupAssignment <- function(x) {
  inherits(x, "GroupAssignment")
}

#' @rdname is_stenR_classes
#' @export
is.intersected <- function(x) {
  inherits(x, "Intersect")
}

#### handle_raw_scores.R ####

#' @rdname is_stenR_classes
#' @export
is.ScaleSpec <- function(x) {
  inherits(x, "ScaleSpec")
}

#' @rdname is_stenR_classes
#' @export
is.CombScaleSpec <- function(x) {
  inherits(x, "CombScaleSpec")
}

#### FrequencyTable.R checkers ####

#' @rdname is_stenR_classes
#' @export
is.FrequencyTable <- function(x) {
  inherits(x, "FrequencyTable")
}

#' @rdname is_stenR_classes
#' @export
is.GroupedFrequencyTable <- function(x) {
  inherits(x, "GroupedFrequencyTable")
}

#' @rdname is_stenR_classes
#' @export
is.Simulated <- function(x) {
  inherits(x, "Simulated")
}

#### ScoreTable.R checkers ####

#' @rdname is_stenR_classes
#' @export
is.ScoreTable <- function(x) {
  inherits(x, "ScoreTable")
}

#' @rdname is_stenR_classes
#' @export
is.GroupedScoreTable <- function(x) {
  inherits(x, "GroupedScoreTable")
}

#### ScoringTable.R checkers ####

#' @rdname is_stenR_classes
#' @export
is.ScoringTable <- function(x) {
  inherits(x, "ScoringTable")
}

#' @rdname is_stenR_classes
#' @export
is.StandardScale <- function(x) {
  inherits(x, "StandardScale")
}