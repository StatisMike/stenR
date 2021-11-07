#### checking comp_freqtable object initialization ####

vars <- names(HEXACO_60)[-c(1:3)]

test_that("Errors with wrong id", {
  
  expect_error(
    gen_freqtable(
      data = HEXACO_60,
      vars = vars,
      id = "wrong_id_name"
  ),
  regexp = stenR:::.warnings$bad_id_name)
  
  expect_error(
    gen_freqtable(
      data = HEXACO_60,
      vars = vars
    ),
    regexp = stenR:::.warnings$missing_id_for_keep
  )
  
})

test_that("Errors with wrong varname", {
  
  expect_error(
    gen_freqtable(
      data = HEXACO_60,
      vars = c("HEX_H", "HEX_S"),
      id = "user_id"
    ),
    regexp = stenR:::.warnings$bad_var_name
  )
  
})

test_that("Errors with wrong data argument", {
  
  expect_error(
    gen_freqtable(
      data = "not_a_dataframe",
      vars = vars,
      id = "user_id"
    ),
    regexp = stenR:::.warnings$data.frame_required
  )
})

#### initialize comp_freqtable object for further tests

suppressWarnings({
  # with kept data: default
  freqtable <- gen_freqtable(
    data = HEXACO_60,
    vars = vars,
    id = "user_id"
  )
  # without kept data: 'light' version
  freqtable_l <- gen_freqtable(
    data = HEXACO_60,
    vars = vars,
    keep_data = F
  )
})

#### information on data

test_that("The kept data is valid", {
  expect_equal(freqtable$get_data_info()$variable, vars)
  expect_equal(length(freqtable$get_data_info()$ids), nrow(HEXACO_60))
})

test_that("There is no kept data with `keep_data` = F", {
  expect_error(freqtable_l$get_data_info())
})

#### comp_freqtable without computed scores

test_that("There is good output with get_status() method (not computed scores)", {
  expect_match(freqtable$get_status()$`standardized scores`, regexp = "not computed yet")
  expect_match(freqtable_l$get_status()$`standardized scores`, regexp = "not computed yet")
})

test_that("There is an error trying to get_scoretables() without computing", {
  expect_error(freqtable$get_scoretables("sten"))
  expect_error(freqtable_l$get_scoretables("sten"))
})

