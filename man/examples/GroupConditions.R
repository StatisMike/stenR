# create GroupConditions with formula-style conditions per each group

sex_grouping <- GroupConditions(
  conditions_category = "Sex",
  "M" ~ sex == "M",
  "F" ~ sex == "F",
  "O" ~ !sex %in% c("M", "F")
)
print(sex_grouping)

# GroupConditions can also mark if the groups should be handled by default
# with forced disjoint (default `TRUE`) and exhaustiveness (default `FALSE`)

age_grouping <- GroupConditions(
  conditions_category = "Age",
  "to 20" ~ age < 20,
  "20 to 40" ~ age >= 20 & age <= 40,
  "40 to 60" ~ age >= 40 & age < 60,
  force_disjoint = FALSE,
  force_exhaustive = TRUE
)
print(age_grouping)
