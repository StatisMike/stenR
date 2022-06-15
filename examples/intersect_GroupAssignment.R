sex_grouping <- GroupConditions(
  "M" ~ sex == "M",
  "F" ~ sex == "F",
  "O" ~ !sex %in% c("M", "F")
)

age_grouping <- GroupConditions(
  "to 20" ~ age < 20,
  "20 to 40" ~ age >= 20 & age <= 40,
  "40 to 60" ~ age >= 40 & age < 60,
  force_exhaustive = TRUE,
  force_disjoint = FALSE
)

# intersect two distinct GroupAssignements

intersected <- intersect_GroupAssignment(
  GroupAssignment(HEXACO_60, sex_grouping),
  GroupAssignment(HEXACO_60, age_grouping),
  force_exhaustive = TRUE,
  force_disjoint = FALSE
)

summary(intersected)

