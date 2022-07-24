# create temp files
ScaleSpecJSON <- tempfile(fileext = ".json")
CombScaleJSON <- tempfile(fileext = ".json")

####         import/export ScaleSpec        ####
# create scale spec for export
scaleSpec <- ScaleSpec(
  name = "First Scale", 
  item_names = c("Item_1", "Item_2"), 
  min = 1,  max = 5)

# export / import
export_ScaleSpec(scaleSpec, ScaleSpecJSON)

imported_scaleSpec <- import_ScaleSpec(ScaleSpecJSON)

# check if they are the same
all.equal(scaleSpec, imported_scaleSpec)

####      import/export CombScaleSpec       ####
# create second scale and CombScaleSpec object
second_scale <- ScaleSpec(
  name = "Second Scale", 
  item_names = c("Item_3", "Item_4"),  
  min = 0, max = 7, 
  reverse = "Item_3"
)
combScale <- CombScaleSpec(
  name = "First Comb", 
  scaleSpec, 
  second_scale,
  reverse = "Second Scale")

# export / import
export_ScaleSpec(combScale, CombScaleJSON)
imported_CombScale <- import_ScaleSpec(CombScaleJSON)

# check if they are the same
all.equal(combScale, imported_CombScale)
