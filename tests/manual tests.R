# testing the correct workflow

table <- stenR:::.calc_freq_Z_table(data, "HEX_X")

score <- stenR:::.calc_score(name = "HEX_X",
            table = table$table,
            M = 5.5,
            SD = 2,
            min = 1,
            max = 10)

test_freqtable_not_keep <- gen_freqtable(data = HEXACO_60,
                                vars = c("HEX_H", "HEX_E", "HEX_X", "HEX_A", "HEX_C", "HEX_O"),
                                keep_data = F)

test_freqtable <- gen_freqtable(data = HEXACO_60,
                                         vars = c("HEX_H", "HEX_E", "HEX_X", "HEX_A", "HEX_C", "HEX_O"),
                                         id = "user_id")

wip_freqtables <- test_freqtable$get_freqtables()

test_freqtable$compute_scores("sten")

wip_scoretables_sten <- test_freqtable$get_scoretables("sten")

custom_scale <- list(name = "custom one",
                     M = 20,
                     SD = 5,
                     min = 1,
                     max = 40)

test_freqtable$compute_scores(custom_scale)

wip_scoretables_custom <- test_freqtable$get_scoretables("custom one")

wip_scoretables_sten
wip_scoretables_custom

status <- test_freqtable$get_status()

stenR:::.get_comp_score(raw_score = 1,
                        comp_table = wip_scoretables_sten$tables$HEX_H)

test_freqtable$get_computed_scores("custom one")

private <- list(computed_scores = test_freqtable$get_raw_computed())

#### rebuilding from tibbles

data <- stenR::HEXACO_60
var <- "HEX_H"

var_data <- data[[var]]

# default status: complete raw scores from min to max
status <- "complete"

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
  score = comp$score,
  freq = as.numeric(round(comp$h, 4)*100),
  quan = as.numeric(round(comp$props, 4)*100),
  Z = as.numeric(comp$Z_val)
)

# check if there are any scores between without values
first_score <- as.numeric(table$score[1])
last_score <- as.numeric(table$score[length(table$score)])

# if there are any, there is a need to add missing values
if(!(length(table$score) == last_score - first_score + 1)){

  #generate warning and update status correctly
  warning(.warnings$missing_values(var), call. = F)
  status <- "incomplete"

  # generate table with all score values
  complete_table <- data.frame(score = first_score:last_score)
  complete_table$score <- as.character(complete_table$score)
  complete_table <- dplyr::left_join(complete_table, table, by = "score")

  # if there is a score with missing values, get them from the row before
  for (row in 1:nrow(complete_table)) {
    if (is.na(complete_table[row, 2])) {
      complete_table[row, 2] <- 0
      complete_table[row, 3:4] <- complete_table[row - 1, 3:4]
    }
  }
  table <- complete_table
}

#### working on named vectors

character_vector <- c("HEX_H", "HEX_O")

names(character_vector)

test_data <- data.frame("HEX_H" = c(20,45), HEX_O = c(40, 32))

changed_data <- test_data
which(names(changed_data) == "Honesty")
which(names(changed_data) == "Openness")

for (var in character_vector) {
  name_index <- which(names(changed_data) == var)
  names(changed_data)[name_index] <- names(character_vector)[name_index]
}

data <- test_data
scale <- "sten"
vars <- character_vector
private <- list(computed_scores = list("sten" = test_freqtable$get_scoretables("sten")))

