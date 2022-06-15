# file = "../stenR_scoringTable.csv"
# sep = ","
# x_group = "Group_1"
# y_group = "Group_2"
# scores = "sten"

# ScoringTable <- function(...)

#' @title Pivot scoringtable into raw score on left
#' @param scores numeric vector of standardized scores
#' @param raws character vector of numeric raw scores separated by `split`.
#' @param name character name of the scores names
#' @noRd  

scores_to_long <- function(scores, raws, name, split = "-") {
  
  if (length(scores) != length(raws)) {
    stop("Uneven scores and raws lengths")
  }
  
  sc_tbl <- lapply(seq_along(scores), \(i) {
    
    raw_sc <- as.numeric(unlist(strsplit(raws[i], split = split)))
    data.frame(raw = min(raw_sc):max(raw_sc), 
               score = scores[i])
  })
  
  sc_tbl <- dplyr::bind_rows(sc_tbl)
  names(sc_tbl)[2] <- name
  
  return(sc_tbl)
  
}

#' @title Create ScoringTable from shortened table
#' @param short_st Short ScoringTable to make longer
#' @param x_group Horizontal groups (colnames)
#' @param y_group Vertical groups (column values)
#' @param standard_scores name of the column containing the standard scores
#' @noRd

create_st <- function(short_st,
                      x_group = "Group_1",
                      # y_group = "Group_2",
                      standard_scores = "sten") {
  
  raw_table <- short_st
  
  # check the x_group if it is available
  if (!is.null(x_group)) {
    x_pattern_prefix <- paste0("^", x_group, ":")
    x_pattern <- paste0(x_pattern_prefix, "\\w{1,}")
    # check if there are any correctly formatted group_x:value names
    x_ind <- which(grepl(names(raw_table), pattern = x_pattern))
    if (length(x_ind) == 0)
      stop(paste0("There are no correctly formatted columns with values of '", x_group, "'.",
                  " Correctly formatted column names with 'x_group' should follow the pattern:\n",
                  "'x_group:val1', 'x_group:val2', 'x_group:valN'"))
    
    x_vals <- gsub(names(raw_table)[x_ind], pattern = x_pattern_prefix, replacement = "")
    if (any(nchar(x_vals) == 0))
      stop(paste0("There are some uncorrectly provided values to the 'x_group': '", x_group, "'.",
                  " Values of 'x_group' should consists of alphanumerics characters only.",
                  " Correctly formatted column names with 'x_group' should follow the pattern:\n",
                  "'x_group:val1', 'x_group:val2', 'x_group:valN'"))
    
  }
  
  ss_ind <- which(grepl(names(raw_table), pattern = paste0("^", standard_scores, "$")))
  
  if (length(ss_ind) != 1)
    stop(paste0("There is not exactly one column named '", standard_scores, "' (standard_scores)."))
  
  for (x_val_i in x_ind) {
    
    i_scores <- scores_to_long(
      scores = raw_table[[standard_scores]],
      raws = raw_table[[x_val_i]],
      name = names(raw_table)[x_val_i],
      split = "-"
    )
    
    if (x_val_i == min(x_ind))
      out_score <- i_scores
    else 
      out_score <- dplyr::left_join(
        out_score, i_scores, by = "raw"
      )
  }
  
  return(out_score)
}



# ScoringTable1 <- read.delim("../stenR_scoringTable.csv", sep = ",", check.names = F) |>
#   dplyr::filter(Group_2 == "M")
