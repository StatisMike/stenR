# Scoring table to export / import #

Consc_ST <- 
  GroupedFrequencyTable(
    data = IPIP_NEO_300,
    conditions = GroupConditions("Sex", "M" ~ sex == "M", "F" ~ sex == "F"), 
    var = "C") |>
  GroupedScoreTable(scale = STEN) |>
  to_ScoringTable(min_raw = 60, max_raw = 300)

#### Export/import method: csv ####

scoretable_csv <- tempfile(fileext = ".csv")
conditions_csv <- tempfile(fileext = ".csv")

export_ScoringTable(
  table = Consc_ST,
  out_file = scoretable_csv,
  method = "csv",
  cond_file = conditions_csv
)

## check if these are regular csv files
writeLines(head(readLines(scoretable_csv)))
writeLines(head(readLines(conditions_csv)))

imported_from_csv <- import_ScoringTable(
  source = scoretable_csv,
  method = "csv",
  cond_file = conditions_csv
)

all.equal(Consc_ST, imported_from_csv)

#### Export/import method: json ####
scoretable_json <- tempfile(fileext = ".json")

export_ScoringTable(
  table = Consc_ST,
  out_file = scoretable_json,
  method = "json"
)

## check if this is regular json file
writeLines(head(readLines(scoretable_json)))

imported_from_json <- import_ScoringTable(
  source = scoretable_json,
  method = "json"
)

all.equal(Consc_ST, imported_from_json)
