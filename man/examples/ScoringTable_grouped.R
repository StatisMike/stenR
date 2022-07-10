#### GroupConditions creation ####

sex_grouping <- GroupConditions(
  conditions_category = "Sex",
  "Male" ~ sex == "M",
  "Female" ~ sex == "F"
)

####   Creating ScoringTable   #### 
##     based on grouped data     ##

Neu_ST <- 
  # create FrequencyTable
  GroupedFrequencyTable(
    data = IPIP_NEO_300,
    conditions = sex_grouping, 
    var = "N") |>
  # create ScoreTable
  GroupedScoreTable(
    scale = STEN) |>
  # and transform into ScoringTable
  to_ScoringTable(
    min_raw = 60,
    max_raw = 300
  )

summary(Neu_ST)
