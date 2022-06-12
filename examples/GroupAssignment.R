age_grouping <- GroupConditions(
  "to 20" ~ age < 20,
  "20 to 40" ~ age >= 20 & age <= 40,
  "40 to 60" ~ age >= 40 & age < 60
)

# on basis of GroupConditions create GroupAssignment

age_assignment <- GroupAssignment(
  data = HEXACO_60,
  age_grouping)

print(age_assignment)

# overwrite the default settings imposed by `GroupConditions`

age_assignment_forced <- GroupAssignment(
  data = HEXACO_60,
  age_grouping,
  force_exhaustive = TRUE)

summary(age_assignment_forced)

# you can also use other unique identifier from your data

age_assignment_forced_w_id <- GroupAssignment(
  data = HEXACO_60,
  age_grouping,
  id = "user_id",
  force_exhaustive = TRUE)

summary(age_assignment_forced_w_id)
