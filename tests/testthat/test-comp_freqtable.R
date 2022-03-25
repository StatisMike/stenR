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
  expect_match(freqtable$get_status()$standardized_scores, regexp = "not computed yet")
  expect_match(freqtable_l$get_status()$standardized_scores, regexp = "not computed yet")
})

test_that("There is an error trying to get_scoretables() without computing", {
  expect_error(freqtable$get_scoretables("sten"))
  expect_error(freqtable_l$get_scoretables("sten"))
})

#### comp_freqtable score computation

test_that("Score computation checks works", {
  
  expect_error(freqtable$compute_scores("non-existent scale"))
  expect_error(freqtable$compute_scores(
    list(name = "Incomplete custom scale",
         M = 5,
         SD = 3))) 
  expect_error(freqtable$compute_scores(scale = 3))
  
  })

test_that("Score computation works for default scales", {
  
  for (scale in c("sten", "stanine", "tanine", "tetronic", "wechsler-iq")){
    
    # regular freqtable
      expect_silent(
        freqtable$compute_scores(scale)
      )
    
    # freqtable-light

      expect_silent(
        freqtable_l$compute_scores(scale)
      )
  }
})

test_that("Score computation works for custom scales", {
  
  custom_scales <- list(
    list(name = "Low-score scale",
         M = 2, SD = 0.5, min = 0, max = 4),
    list(name = "High-score scale",
         M = 1000, SD = 200, min = 0,max = 2000),
    list(name = "Medium-score scale",
         M = 20, SD = 5, min = 0, max = 40),
    list(name = "Negative-score scale",
         M = 0, SD = 5, min = -10, max = 10)
  )
  
  for (scale in custom_scales){
    # regular freqtable
    expect_silent(
      freqtable$compute_scores(scale)
    )
    
    # freqtable-light
    
    expect_silent(
      freqtable_l$compute_scores(scale)
    )
  }
})

#### get scoretables from regular comp_freqtable test

test_that("You can get computed scoretables from comp_freqtable", {
  
  for (scale in names(freqtable$get_status()$standardized_scores)){
    
    expect_silent(
      computed_score <- freqtable$get_scoretables(scale)
    )

    expect_equal(length(computed_score), length(vars))
    
    for (var in vars){
      
      expect_equal(nrow(computed_score[[var]]),
                   nrow(freqtable$get_freqtables()[[var]]))
      
    }
  }
})

#### get scoretables from light comp_freqtable test

test_that("You can get computed scoretables from comp_freqtable light", {
  
  for (scale in names(freqtable_l$get_status()$standardized_scores)){
    
    expect_silent(
      computed_score <- freqtable_l$get_scoretables(scale)
    )
    
    expect_equal(length(computed_score), length(vars))
    
    for (var in vars){
      
      expect_equal(nrow(computed_score[[var]]),
                   nrow(freqtable_l$get_freqtables()[[var]]))
      
    }
  }
})

#### get computed scores checks tests ####

test_that("You cannot get internal computed_scores from light freqtable", {
  
  expect_error(freqtable_l$get_computed_scores())
  
})

test_that("Prints error messages for invalid arguments are provided", {
  
  expect_error(freqtable$get_computed_scores(scale = "non-computed scale"))
  expect_error(freqtable$get_computed_scores(scale = c("multiple", "strings")))
  expect_error(freqtable$get_computed_scores(scale = "sten",
                                             ids = 1))
  expect_error(freqtable$get_computed_scores(ids = c("non-existing", "identifications"),
                                             scale = "sten"))
  expect_error(freqtable$get_computed_scores(vars = c("non-existing", "variables"),
                                             scale = "sten"))
  
})

#### get computed scores works as it should

test_that("Gets computed scores for all scales that are computed", {
  
  for (scale in names(freqtable$get_status()$standardized_scores)){
    
    test_scores <- freqtable$get_computed_scores(scale = scale)
    
    expect_equal(nrow(test_scores), nrow(HEXACO_60))
    expect_equal(ncol(test_scores), length(vars) + 1)
  }
})

test_that("Gets computed scores for all scales that are computed and only for specified ids and vars", {
  
  random_ids <- sample(HEXACO_60$user_id, 5)
  random_vars <- sample(vars, 3)
  
  for (scale in names(freqtable$get_status()$standardized_scores)){
      
      test_scores <- freqtable$get_computed_scores(scale = scale,
                                                   ids = random_ids,
                                                   vars = random_vars)
      
      expect_equal(nrow(test_scores), length(random_ids))
      expect_equal(ncol(test_scores), length(random_vars) + 1)
      
  }
  
})

#### get_computed_scores_ext checks

# external data

external_data_same_names <- data.frame(
  HEX_C = c(32, 49), HEX_E = c("40", "23")
)

external_data_differing_names <- data.frame(
  Extraversion = c(25, 42), Openness = c("40", "23")
)


test_that("Prints error messages for invalid arguments are provided", {
    
  expect_error(freqtable$get_computed_scores_ext(scale = "non-computed scale",
                                                 data = external_data_same_names))
  expect_error(freqtable$get_computed_scores_ext(scale = c("multiple", "strings"),
                                                 data = external_data_same_names))
  expect_error(freqtable$get_computed_scores_ext(vars = c("non-existing", "variables"),
                                                 scale = "sten",
                                                 data = external_data_same_names))
  expect_error(freqtable$get_computed_scores_ext(vars = 1,
                                                 scale = "sten",
                                                 data = external_data_same_names))
  expect_error(freqtable$get_computed_scores_ext(vars = c("HEX_NONE" = "non-existing"),
                                                 scale = "sten",
                                                 data = external_data_same_names))
})

test_that("Correctly gets computed scores from external data", {
  
  test_scores <- list()
  
  # for regular freqtable
  for (scale in names(freqtable$get_status()$standardized_scores)){
    # same names
    expect_silent(test_scores[["reg_same"]] <- freqtable$get_computed_scores_ext(
      scale = scale,
      vars = c("HEX_C", "HEX_E"),
      data = external_data_same_names))
    expect_equal(nrow(test_scores$reg_same), nrow(external_data_same_names))
    expect_equal(ncol(test_scores$reg_same), ncol(external_data_same_names))
    # different names
    expect_silent(test_scores[["reg_diff"]] <- freqtable$get_computed_scores_ext(
      scale = scale,
      vars = c("HEX_O" = "Openness", 
               "HEX_X" = "Extraversion"),
      data = external_data_differing_names))
    expect_equal(nrow(test_scores$reg_diff), nrow(external_data_differing_names))
    expect_equal(ncol(test_scores$reg_diff), ncol(external_data_differing_names))
  }
  
  # for light freqtable
  for (scale in names(freqtable$get_status()$standardized_scores)){
    # same names
    expect_silent(test_scores[["l_same"]] <- freqtable_l$get_computed_scores_ext(
      scale = scale,
      vars = c("HEX_C", "HEX_E"),
      data = external_data_same_names))
    expect_equal(nrow(test_scores$l_same), nrow(external_data_same_names))
    expect_equal(ncol(test_scores$l_same), ncol(external_data_same_names))
    # different names
    expect_silent(test_scores[["l_diff"]] <- freqtable_l$get_computed_scores_ext(
      scale = scale,
      vars = c("HEX_O" = "Openness", 
               "HEX_X" = "Extraversion"),
      data = external_data_differing_names))
    expect_equal(nrow(test_scores$l_diff), nrow(external_data_differing_names))
    expect_equal(ncol(test_scores$l_diff), ncol(external_data_differing_names))
  }
  
  expect_identical(test_scores[["reg_diff"]], test_scores[["l_diff"]])
  expect_identical(test_scores[["reg_same"]], test_scores[["l_same"]])
  
})