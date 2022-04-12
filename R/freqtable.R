#' Create a FrequencyTable from raw data
#' 
#' @param data vector of raw scores. Double values are coerced to integer
#' @return 
#' FrequencyTable object. Consists of:
#' 
#' - table: data.frame with number of observations (`n`), frequency in sample 
#' (`freq`), quantile (`quan`) and normalized Z-score (`Z`) for each point in 
#' raw score 
#' @export

FrequencyTable <- function(data) {
  
  if (!is.numeric(data)) stop("Vector of non-numeric values were provided")
  if (!is.integer(data)) {
    warning("Non-integer numeric values were coerced to integers")
    raw <- as.integer(data)
  }
  
  # calculate statistics for frequency table
  H <- table(data)
  #Hcum <- cumsum(H)
  h <- as.numeric(prop.table(H))
  hcum <- cumsum(h)
  
  # create whole frequency table
  comp <- data.frame(
    score = names(H),
    h = h,
    hcum = hcum
  )
  
  comp[["lag_hcum"]] <- c(0, comp[1:nrow(comp) - 1, "hcum"])
  comp[["props"]] <- comp[["lag_hcum"]] + comp[["h"]]/2
  comp[["Z_val"]] <- stats::qnorm(comp[["props"]])
  
  table <- data.frame(
    n = as.numeric(H),
    score = comp$score,
    freq = as.numeric(comp$h * 100),
    quan = as.numeric(comp$props * 100),
    Z = as.numeric(comp$Z_val)
  )
  
  # check if there are any scores between without values
  first_score <- as.numeric(table$score[1])
  last_score <- as.numeric(table$score[length(table$score)])
  
  # if there are any, there is a need to add missing values
  if(!(length(table$score) == last_score - first_score + 1)){
    
    #generate warning and update status correctly
    warning("There are missing score values between minimum and maximum scores. They have been filled automatically.")
    status <- list(range = "incomplete",
                   n = sum(table$n))
    
    # generate table with all score values
    complete_table <- data.frame(score = first_score:last_score)
    complete_table$score <- as.character(complete_table$score)
    complete_table <- dplyr::left_join(complete_table, table, by = "score")
    
    # if there is a score with missing values, get them from the row before
    for (row in 1:nrow(complete_table)) {
      if (is.na(complete_table[row, "n"])) {
        complete_table[row, "n"] <- 0
        complete_table[row, "freq"] <- 0
        complete_table[row, c("quan", "Z")] <- complete_table[row - 1, c("quan", "Z")]
      }
    }
    table <- complete_table
  } else {
    status <- list(range = "complete",
                   n = sum(table$n))
  }
  
  output <- list(table = table,
                 status = status)
  
  class(output) <- "FrequencyTable"
  
  return(output)
  
}

#' @param ft A `FrequencyTable` object
#' @param max numeric or NULL, specifying the maximal number of entries to be 
#' printed. By default, when NULL, \code{\link{getOption}("max.print")} used.
#' @rdname FrequencyTable
#' @export
print.FrequencyTable <- function(ft, max = NULL) {
  
  cat(sep = "", "<FrequencyTable> computed on: ", ft$status$n, " observations\n")
  cat(ft$status$range, "range", if(ft$status$range != "complete") "(missing raw score values between <min> and <max>)\n\n")
  
  print(ft$table, max = max, row.names = F)
  
  invisible(ft)
}

#' @param ft A `FrequencyTable` object
#' @rdname FrequencyTable
#' @export
plot.FrequencyTable <- function(ft) {
  
  if (!requireNamespace("ggplot2", quietly = T))
    stop("Generic plotting of 'FrequencyTable' requires 'ggplot2' package installed")
  
  sds <- factor(ifelse(ft$table$Z < -2 | ft$table$Z > 2, ">2SD",
                ifelse(ft$table$Z < -1 | ft$table$Z > 1, "1SD-2SD", "<1SD")),
                levels = c("<1SD", "1SD-2SD", ">2SD"))
  
  i <- which(abs(ft$table$Z) == min(abs(ft$table$Z)))
  
  Z_label <- paste("Z =", round(ft$table$Z[i], 2))
  
  ggplot2::ggplot(data = ft$table, ggplot2::aes(x = score, y = n)) + 
    ggplot2::geom_col(ggplot2::aes(fill = sds)) +
    ggplot2::scale_fill_discrete("Normalized\ndistribution") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = ft$table$score[i],
                                     color = Z_label)) +
    ggplot2::scale_color_manual("Closest to\ncenter", values = "#000000")

}
