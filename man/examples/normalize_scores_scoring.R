# Scoring table to export / import #
Consc_ST <- 
  GroupedFrequencyTable(
    data = IPIP_NEO_300,
    conditions = GroupConditions("Sex", "M" ~ sex == "M", "F" ~ sex == "F"), 
    var = "C") |>
  GroupedScoreTable(scale = STEN) |>
  to_ScoringTable(min_raw = 60, max_raw = 300)

# normalize scores
Consc_norm <- 
  normalize_scores_scoring(
    data = IPIP_NEO_300,
    vars = "C",
    Consc_ST,
    group_col = "Group"
  )

str(Consc_norm)
