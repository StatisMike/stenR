# .calc_freq_Z warnings tests ####

test_that("no warning message if all consecutive scores are present", {
  expect_failure(expect_warning(
    stenR:::.calc_freq_Z_table(data = HEXACO_60,
                               var = "HEX_C")
  ))
})

test_that("warning message if consecutive scores are not present", {
  expect_warning(
    stenR:::.calc_freq_Z_table(data = HEXACO_60,
                               var = "HEX_X")
  )
})

# .calc_freq_Z computed for further testing ####

freq_table <- stenR:::.calc_freq_Z_table(
  data = HEXACO_60,
  var = "HEX_C"
)

# .calc_freq_Z output object is correct ####

test_that("status object is correct", {
  expect_equal(freq_table$status, "complete")
})

test_that("table object is correct", {
  expect_equal(names(freq_table$table), c("score", "freq", "quan", "Z"))
  expect_s3_class(freq_table$table, "data.frame")
  expect_equal(min(as.numeric(freq_table$table$score)), min(HEXACO_60$HEX_C))
  expect_equal(max(as.numeric(freq_table$table$score)), max(HEXACO_60$HEX_C))
})

# .calc_score get object for further test ####

score_table <- stenR:::.calc_score(
  name = "HEX_C", 
  table = freq_table$table,
  M = 5.5,
  SD = 2,
  min = 1,
  max = 10)

#### .calc_score tests ####

test_that(".calc_score output is correct", {
  expect_s3_class(score_table, "data.frame")
  expect_equal(names(score_table), c("score", "HEX_C"))
  expect_equal(min(as.numeric(score_table$score)), min(HEXACO_60$HEX_C))
  expect_equal(max(as.numeric(score_table$score)), max(HEXACO_60$HEX_C))
  expect_equal(min(score_table$HEX_C), 1)
  expect_equal(max(score_table$HEX_C), 10)
  })

#### .get_comp_score tests ####

test_that(".get_comp_score works correcty on raw scores available in scoring table", {
  for (raw_score in min(as.numeric(score_table$score)):max(as.numeric(score_table$score))){
    expect_type(
      stenR:::.get_comp_score(
        raw_score = ifelse(
          sample(c(T,F), 1), 
          as.numeric(raw_score),
          as.character(raw_score)),
        comp_table = score_table
      ), "double")
  }
})

test_that(".get_comp_score works correctly on raw scores lower or higher", {
  for (raw_score in list(1, "1", 55, "55")) {
    expect_type(
      stenR:::.get_comp_score(
        raw_score = raw_score,
        comp_table = score_table
      ), "double"
    )
  }
})