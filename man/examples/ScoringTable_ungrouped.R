Neu_ST <- 
  # create FrequencyTable
  FrequencyTable(data = IPIP_NEO_300$E) |>
  # create ScoreTable
  ScoreTable(scale = STEN) |>
  # and transform into ScoringTable
  to_ScoringTable(
    min_raw = 60,
    max_raw = 300
  )

summary(Neu_ST)