wide_scale <- NULL
short_scale <- NULL

wide_scale_args <- list(
  name = "wide scale",
  M = 10,
  SD = 4,
  min = 0,
  max = 20
)

short_scale_args <- list(
  name = "short scale",
  M = 3,
  SD = 1,
  min = 1,
  max = 5 
)

test_that("StandardScale can be constructed", {
  wide_scale <<- do.call(
    StandardScale,
    wide_scale_args
  )
  
  short_scale <<- do.call(
    StandardScale,
    short_scale_args
  )
  
  expect_s3_class(wide_scale, "StandardScale")
  expect_s3_class(short_scale, "StandardScale")
})

test_that("StandardScale plots correctly", {
  
  expect_s3_class(
    plot(wide_scale), c("gg", "ggplot")
  )
  expect_s3_class(
    plot(short_scale), c("gg", "ggplot")
  )
})