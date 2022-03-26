#' FreqtableScale
#' 
#' An object with specification for scale to be used by `CompFreqtable` object
#' @param name Name of the scale
#' @param M Mean of the scale
#' @param SD Standard deviation of the scale
#' @param min Minimal value the scale takes
#' @param max Maximal value the scale takes
#' @export
#' 
FreqtableScale <- function(
  name, M, SD, min, max
) {
  
  if (!is.character(name) || length(name)) {
    "Argument provided to 'name' should be of type 'character'"
  }
  if (!is.numeric(M) || length(M)) {
    "Argument provided to 'M' should be of type 'numeric'"
  }
  if (!is.numeric(SD) || length(SD)) {
    "Argument provided to 'SD' should be of type 'numeric'"
  }
  if (!is.numeric(min) || length(min)) {
    "Argument provided to 'min' should be of type 'numeric'"
  }
  if (!is.numeric(max) || length(max)) {
    "Argument provided to 'max' should be of type 'numeric'"
  }
  
  obj <- list(name = name,
              M = M,
              SD = SD,
              min = min,
              max = max)
  
  class(obj) <- "FreqtableScale"
  
  return(obj)
  
}

#' Summary method for FreqtableScale object
#' @param object an object for which a summary is desired.
#' @param ... additional arguments affecting the summary produced.
#' @export
summary.FreqtableScale <- function(object, ...) {
  
  cat("<FreqtableScale> object: '", object$name, "' scale definition\n", sep = "")
  cat("( M: ", object$M, "; SD: ", object$SD, "; min: ", object$min, "; max: ", object$max, " )", sep = "")
  
}

.default_scales <- list(
  FreqtableScale(name = "sten", M = 5.5, SD = 2, min = 1, max = 10),
  FreqtableScale(name = "stanine", M = 5, SD = 2, min = 1, max = 9),
  FreqtableScale(name = "tanine", M = 50, SD = 10, min = 1, max = 100),
  FreqtableScale(name = "tetronic", M = 10, SD = 4, min = 0, max = 20),
  FreqtableScale(name = "wechsler-iq", 100, SD = 15, min = 40, max = 160)
)
