HEX_ST <- NULL
HEX_ST2 <- NULL

sum_test <- NULL
sum_test2 <- NULL

test_that("CompScoreTable initializes", {
  HEX_ST <<- CompScoreTable$new()
  expect_equal(class(HEX_ST)[1], "CompScoreTable")
})

test_that("CompScoreTable attaches scales", {
  HEX_ST$attach_StandardScale(STEN)
  suppressMessages(
    sum_test <- summary(HEX_ST)
  )
  expect_equal(sum_test$scales$name[1], "sten")
})

test_that("CompScoreTable normalizes data with attachement", {
  hexaco_vars <- 
    paste("HEX", c("H", "E", "X", "A", "C", "O"), sep = "_")
  
  standardized <- 
    HEX_ST$standardize(
      data = HEXACO_60,
      vars = hexaco_vars,
      what = "sten",
      calc = TRUE
    )
  
  suppressMessages(
    sum_test <<- summary(HEX_ST)
  )  
  
  # names need to be the same as in source data
  expect_equal(names(standardized), names(HEXACO_60))
  
  # normalized data should be between STEN min and max
  expect_equal(min(standardized[, hexaco_vars]), STEN$min)
  expect_equal(max(standardized[, hexaco_vars]), STEN$max)
  
  # object should have tables for all variables
  expect_equal(sum_test$tables$variable, hexaco_vars)
})

test_that("CompScoreTable initializes with scales and tables", {
  
  suppressMessages(
    freqtables <- lapply(
      paste("HEX", c("H", "E", "X", "A", "C", "O"), sep = "_"),
      \(x) FrequencyTable(HEXACO_60[[x]])
    ), class = stenR:::cli_class$message$IncompleteRange)
  
  names(freqtables) <- paste("HEX", c("H", "E", "X", "A", "C", "O"), sep = "_")
  
  # warnings for incomplete tables
  HEX_ST2 <<- CompScoreTable$new(
    tables = freqtables,
    scales = STEN)
  
  expect_equal(class(HEX_ST2)[1], "CompScoreTable")

})

test_that("CompScoreTables created in different ways are equal", {
  
  suppressMessages(
    sum_test2 <<- summary(HEX_ST2)
  )  

  expect_equal(sum_test$tables, sum_test2$tables)
  expect_equal(sum_test$scales, sum_test$scales)
  
})

test_that("Tables can be exported from CompScoreTables", {
  
  scoretables <- HEX_ST$export_ScoreTable()
  
  lapply(scoretables, \(x) expect_s3_class(x, "ScoreTable"))
  
  freqtables <- HEX_ST$export_ScoreTable(strip = T)
  
  lapply(freqtables, \(x) expect_s3_class(x, "FrequencyTable"))
  
})

test_that("New data can be added to existing CompScoreTables", {
  
  suppressWarnings(new_stand <- HEX_ST$standardize(
    HEXACO_60[1:20, paste("HEX", c("H", "E", "X", "A", "C", "O"), sep = "_")],
    what = "sten",
    calc = T
  ), class = stenR:::cli_class$warning$Type)
  
  suppressMessages(
    sum_test3 <- summary(HEX_ST)
  )

  expect_equal(
    sum_test3$tables$n[1], nrow(HEXACO_60) + 20
  )
  
})

test_that("No new data can be added when using simulated FreqTable", {
  
  HEX_ST$attach_FrequencyTable(
    SimFrequencyTable(min = 10, max = 50, M = 30, SD = 7),
    "HEX_H",
    if_exists = "replace"
  )
  
  expect_error(
    HEX_ST$standardize(
      data.frame(HEX_H = c(10, 25, 30)),
      what = "sten",
      calc = T)
  )

})