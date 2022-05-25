# create the Scale Specifications for SLCS dataset
## Self-Liking specification
SL_spec <- ScaleSpec(
  name = "SL",
  item_names = paste("SLCS", c(1, 3, 5, 6, 7, 9, 11, 15), sep = "_"),
  reverse = paste("SLCS", c(1, 6, 7, 15), sep = "_"),
  min = 1,
  max = 5)

## Self-Competence specification
SC_spec <- ScaleSpec(
  name = "SC",
  item_names = paste("SLCS", c(2, 4, 8, 10, 12, 13, 14, 16), sep = "_"),
  reverse = paste("SLCS", c(8, 10, 13), sep = "_"),
  min = 1,
  max = 5)

## General Score specification
GS_spec <- ScaleSpec(
  name = "GS",
  item_names = paste("SLCS", 1:16, sep = "_"),
  reverse = paste("SLCS", c(1, 6, 7, 8, 10, 13, 15), sep = "_"),
  min = 1, 
  max = 5)

# Sum the raw item scores to raw scale scores
SLCS_summed <- sum_items_to_scale(SLCS, SL_spec, SC_spec, GS_spec, retain = "user_id")
summary(SLCS_summed)