st <- NULL

test_that("ScoreTable is can be contructed", {
  
  # incomplete warning
  expect_message(
    st <<- ScoreTable(FrequencyTable(HEXACO_60$HEX_H), STEN),
    class = "IncompleteRangeMessage"
  )

  expect_s3_class(st, "ScoreTable")
  
})

test_that("ScoreTable prints", {
  
  expect_message(
    print(st)
  )
})

test_that("ScoreTable plots", {
  
  expect_s3_class(
    plot(st),
    c("gg", "ggplot")
  )
})

test_that("ScoreTable can be changed to FrequencyTable", {
  
  expect_s3_class(
    strip_ScoreTable(st),
    "FrequencyTable"
  )
})

test_that("New scale can be attached to ScoreTable", {
  
  first_scale <- length(st$scale)
  
  st <<- attach_scales(st, STANINE)
  
  second_scales <- length(st$scale)
  
  st <<- attach_scales(st, list(TANINE, WECHSLER_IQ))
  
  third_scales <- length(st$scale)
  
  expect_gt(second_scales, first_scale)
  expect_gt(third_scales, second_scales)
  
  expect_true(
    all(names(st$scale) %in% names(st$table))
  )
  
})

test_that("ScoreTable can be used to normalize raw scores", {
  
  for (st_scale in st$scale) {
    
    norm_vec <- normalize_score(
      HEXACO_60$HEX_H,
      st,
      st_scale$name
    )
    
    expect_true(all(norm_vec >= st_scale$min))
    expect_true(all(norm_vec <= st_scale$max))
    
    norm_df <- normalize_scores_df(HEXACO_60,
                                   "HEX_H",
                                   st,
                                   what = st_scale$name)
    
    expect_equal(norm_vec, norm_df$HEX_H)
    
  }
})
