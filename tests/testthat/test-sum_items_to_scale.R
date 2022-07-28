# create mockup dataset

mockup_data <- data.frame(
  some_id = paste("user", 1:4, sep = "_"),
  item_1_reg = 2,
  item_2_reg_to_rev = 4,
  item_3_na = c(NA, 2, NA, 2),
  item_4_custom_na = c(NA, 2, NA, 2),
  item_5_wider_rev = 5
)

# create different scales specification
scale_reg <- ScaleSpec(
  name = "reg",
  item_names = c("item_1_reg", "item_2_reg_to_rev", "item_3_na", "item_4_custom_na", "item_5_wider_rev"),
  reverse = "item_2_reg_to_rev",
  min = 1,
  max = 5
)

scale_reg_NA <- ScaleSpec(
  name = "reg_NA",
  item_names = c("item_1_reg", "item_2_reg_to_rev", "item_3_na", "item_4_custom_na", "item_5_wider_rev"),
  reverse = "item_2_reg_to_rev",
  min = 1,
  max = 5,
  na_value = 3
)

scale_custom_NA <- ScaleSpec(
  name = "custom_NA",
  item_names = c("item_1_reg", "item_2_reg_to_rev", "item_3_na", "item_4_custom_na", "item_5_wider_rev"),
  reverse = "item_2_reg_to_rev",
  min = 1,
  max = 5,
  na_value = 3,
  na_value_custom = c(item_4_custom_na = 2)
)

scale_mode_func <- ScaleSpec(
  name = "mode",
  item_names = c("item_1_reg", "item_2_reg_to_rev", "item_3_na", "item_4_custom_na", "item_5_wider_rev"),
  reverse = "item_2_reg_to_rev",
  min = 1,
  max = 5,
  na_strategy = "mode"
)

scale_median_func <- ScaleSpec(
  name = "median",
  item_names = c("item_1_reg", "item_2_reg_to_rev", "item_3_na", "item_4_custom_na", "item_5_wider_rev"),
  reverse = "item_2_reg_to_rev",
  min = 1,
  max = 5,
  na_strategy = "median"
)

scale_mean_func <- ScaleSpec(
  name = "mean",
  item_names = c("item_1_reg", "item_2_reg_to_rev", "item_3_na", "item_4_custom_na", "item_5_wider_rev"),
  reverse = "item_2_reg_to_rev",
  min = 1,
  max = 5,
  na_strategy = "mean"
)

comb_asis <- CombScaleSpec(
  name = "comb_asis",
  scale_reg_NA,
  scale_custom_NA
)

comb_func <- CombScaleSpec(
  name = "comb_func",
  scale_mode_func,
  scale_median_func,
  scale_mean_func
)

rec_straight <- CombScaleSpec(
  name = "rec_straight",
  comb_asis,
  comb_func
)

rec_rev <- CombScaleSpec(
  name = "rec_rev",
  comb_asis,
  comb_func,
  reverse = "comb_func"
)

summed <- sum_items_to_scale(
  data = mockup_data,
  retain = "some_id",
  scale_reg,
  scale_reg_NA,
  scale_custom_NA,
  scale_mode_func,
  scale_median_func,
  scale_mean_func,
  comb_asis,
  comb_func,
  rec_straight,
  rec_rev
)

test_that("ScaleSpec prints and summaries", {
  
  expect_message(print(scale_reg), regexp = "ScaleSpec")
  expect_message(summary(scale_reg), regexp = "ScaleSpec")
  
})

test_that("CombScaleSpec prints and summaries", {
  
  expect_message(print(comb_asis), regexp = "CombScaleSpec")
  expect_message(test_sum <- summary(comb_asis), regexp = "CombScaleSpec")
  
  expect_s3_class(test_sum, "data.frame")
  expect_equal(nrow(test_sum), length(comb_asis$item_names))
  expect_equal(length(unique(test_sum$scale)), length(comb_asis$ScaleSpecs))
  
  
})

test_that("NAs without NA handling are retained", {
  expect_equal(sum(is.na(summed$reg)), 2)
})

test_that("NAs with regular and custom NA handling works correctly", {
  expect_gt(sum(summed$reg_NA, na.rm = T), sum(summed$custom_NA))
})

test_that("CombScaleSpec works as intended", {
  
  expect_equal(rowSums(summed[, c("reg_NA", "custom_NA")]),
               summed$comb_asis)
  
  expect_equal(rowSums(summed[, c("mode", "median", "mean")]),
               summed$comb_func)
  
})

test_that("CombScaleSpec recursion works", {
  
  expect_equal(rowSums(summed[, c("comb_asis", "comb_func")]),
               summed$rec_straight)
  
  with_revs <- cbind(
    summed[, "comb_asis", drop = F],
    comb_func$min + comb_func$max - summed[, "comb_func", drop = F]
  )
  
  expect_equal(rowSums(with_revs), summed$rec_rev)
  
  
})


test_that("Only retained colums are of original class", {
  expect_equal(sum(sapply(names(summed), \(x) is.character(summed[[x]]))),
               1)
})


test_that("All scales that are to be calculated are present in the final data", {
  expect_equal(sum(sapply(names(summed), \(x) is.numeric(summed[[x]]))),
               10)
})

