ft <- NULL
ft_sim <- NULL

test_that("FrequencyTable can be constructed from raw values", {
  
  expect_message(
    ft <<- FrequencyTable(HEXACO_60$HEX_H),
    class = "IncompleteRangeMessage"
  )
  
  expect_s3_class(
    ft, "FrequencyTable"
  )
})

test_that("FrequencyTable can be simulated", {
  
  ft_sim <<- SimFrequencyTable(
    min = 10,
    max = 50,
    M = 30,
    SD = 7
  )
  
  expect_s3_class(
    ft_sim, c("FrequencyTable", "Simulated")
  )
  
})

test_that("FrequencyTable plots", {
  
  expect_s3_class(
    plot(ft), c("gg", "ggplot")
  )
  expect_s3_class(
    plot(ft_sim), c("gg", "ggplot")
  )
})

test_that("FrequencyTable generates summary", {
  
  expect_equal(
    names(summary(ft)),
    c("n", "min", "max", "mean", "median", "sd", "skewness", "kurtosis", "incomplete")
  )
  
  expect_equal(
    names(summary(ft_sim)),
    c("n", "min", "max", "mean", "median", "sd", "skewness", "kurtosis", "incomplete")
  )
})
