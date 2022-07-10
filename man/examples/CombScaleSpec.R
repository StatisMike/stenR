# ScaleSpec objects to Combine

first_scale <- ScaleSpec(
  name = "First Scale",
  item_names = c("Item_1", "Item_2"),
  min = 1,
  max = 5
)

second_scale <- ScaleSpec(
  name = "Second Scale",
  item_names = c("Item_3", "Item_4"),
  min = 0,
  max = 7,
  reverse = "Item_3"
)

third_scale <- ScaleSpec(
  name = "Third Scale",
  item_names = c("Item_5", "Item_6"),
  min = 1,
  max = 5
)

# You can combine few ScaleSpec objects into CombScaleSpec

first_comb <- CombScaleSpec(
  name = "First Comb",
  first_scale,
  second_scale,
  reverse = "Second Scale"
)

print(first_comb)

# And also other CombScaleSpec objects!

second_comb <- CombScaleSpec(
  name = "Second Comb",
  first_comb,
  third_scale
)

print(second_comb)

