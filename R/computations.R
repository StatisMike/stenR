#' helper function to calculate the regular freq table
#' @param data data.frame containing source data for calculations
#' @param var character string with column name of the variable
#' @param old_data existing data to append
#' @return data.frame containing raw scores and their quantile and Z equivalent
#' @importFrom dplyr left_join
#' @noRd

.calc_freq_Z_table <- function (data, var, old_data = NULL) {

  var_data <- data[[var]]
  
  if (!is.null(old_data)) {
    old_data <- as.numeric(rep(old_data[[var]]$score, times = old_data[[var]]$n))
    var_data <- c(var_data, old_data)
  }

  # calculate statistics for frequency table
  H <- table(var_data)
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
    warning(.warnings$missing_values(var), call. = F)
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

  return(
    list(table = table,
         status = status)
  )
}

#' helper function to calculate score from raw data
#' @param name of the variable for standardization
#' @param table frequency table for standardized variable
#' @param M mean of output scale
#' @param SD sd of output scale
#' @param min min of output scale
#' @param max max of output scale
#' @return tibble with normalized values for each raw score
#' @noRd

.calc_score <- function (name, table, M, SD, min, max) {

  Z_val <- table$Z
  val <- round(Z_val * SD + M)
  val_mut <- ifelse(val < min, min,
                    ifelse(val > max, max, val))
  output <- data.frame(score = table$score,
                       val = val_mut)
  names(output)[2] <- name

  return(output)

}

#' helper function to get computated scores
#' @param raw_score raw score to get data for
#' @param comp_table table with computated score
#' @noRd

.get_comp_score <- function(raw_score, comp_table){

  if (as.numeric(raw_score) > max(as.numeric(comp_table[["score"]]))){
    score <- as.numeric(comp_table[nrow(comp_table), 2])
  } else if (as.numeric(raw_score) < min(as.numeric(comp_table[["score"]]))){
    score <- as.numeric(comp_table[1, 2])
  } else {
    score <- as.numeric(comp_table[comp_table[["score"]] == as.character(raw_score), 2])
  }
  
  return(score)
}
