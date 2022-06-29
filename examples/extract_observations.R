#### Create Group Conditions ####
sex_grouping <- GroupConditions(
  condition_category = "Sex",
  "M" ~ sex == "M",
  "F" ~ sex == "F",
  "O" ~ !sex %in% c("M", "F")
)

age_grouping <- GroupConditions(
  condition_category = "Age",
  "to 20" ~ age < 20,
  "20 to 40" ~ age >= 20 & age <= 40,
  "41 to 60" ~ age > 40 & age <= 60,
  "above 60" ~ age > 60
)

#### Create Group Assignement ####
# can be done both with indices, so later this can be used only on the same data
# or with IDs - so later it can be done with only subset or transformed original data

sex_assignment <- GroupAssignment(HEXACO_60, sex_grouping, id = "user_id")
age_assignment <- GroupAssignment(HEXACO_60, age_grouping, id = "user_id")

#### Intersect two Group Assignement ###
# with additional forcing set
intersected <- intersect_GroupAssignment(
  sex_assignment,
  age_assignment,
  force_exhaustive = TRUE,
  force_disjoint = FALSE
)

extracted <- extract_observations(
  HEXACO_60,
  groups = intersected,
  group_names = c("M"),
  extract_mode = "data.frame",
  strict_names = F)

# only groups created from "M" group were extracted
# groups without observations were dropped
table(extracted$GroupAssignment)
