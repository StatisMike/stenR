# simple scale specification

simple_scaleSpec <- ScaleSpec(
  name = "simple",
  # scale consists of 5 items
  item_names = c("item_1", "item_2", "item_3", "item_4", "item_5"),
  # item scores can take range of values: 1-5
  min = 1,
  max = 5,
  # item 2 and 5 need to be reversed
  reverse = c("item_2", "item_5"))

print(simple_scaleSpec)

# scale specification with literal NA imputation strategy 

asis_scaleSpec <- ScaleSpec(
  name = "w_asis",
  item_names = c("item_1", "item_2", "item_3", "item_4", "item_5"),
  min = 1,
  max = 5,
  reverse = "item_2",
  # na values by default will be filled with `3`
  na_value = 3,
  # except for item_4, where they will be filled with `2`
  na_value_custom = c(item_4 = 2)
)

print(asis_scaleSpec)

# scale specification with functional NA imputation strategy

func_scaleSpec <- ScaleSpec(
  name = "w_func",
  item_names = c("item_1", "item_2", "item_3", "item_4", "item_5"),
  min = 1,
  max = 5,
  reverse = "item_2",
  # strategies available are 'mean', 'median' and 'mode'
  na_strategy = "mean"
)

print(func_scaleSpec)