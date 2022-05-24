# simple scale specification

simple_scaleSpec <- ScaleSpec(
  name = "simple",
  # scale consists of 5 items
  item_names = c("item_1", "item_2", "item_3", "item_4", "item_5"),
  # item 2 and 5 need to be reversed
  reverse = c("item_2", "item_5"),
  # item scores can take range of values: 1-5
  min = 1,
  max = 5)



# complec scale specification

complex_scaleSpec <- ScaleSpec(
  name = "complex",
  item_names = c("item_1", "item_2", "item_3", "item_4", "item_5"),
  reverse = "item_2",
  min = 1,
  max = 5,
  # na values by default will be filled with `3`
  na_value = 3,
  # except for item_4, where they will be filled with `2`
  na_value_custom = c(item_4 = 2),
  # item 4 and 5 can take custom range of values, so the reversing
  # of score need to be handled customly
  reverse_custom = 
    data.frame(item_names = c("item_4", "item_5"),
               min = c(1, 1),
               max = c(3, 7))
)