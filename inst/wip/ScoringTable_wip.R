compare_tables <- function(t1, t2) {

  t1_attr <- attributes(t1)
  t2_attr <- attributes(t2)
  
  t1_cond <- t1_attr[["conditions"]]
  t2_cond <- t2_attr[["conditions"]]
  
  t1_attr <- t1_attr[-which(names(t1_attr) == "conditions")]
  t2_attr <- t2_attr[-which(names(t2_attr) == "conditions")]
  
  t1_to_t2 <- all(sapply(names(t1_attr), \(nm) all(t1_attr[[nm]] == t2_attr[[nm]])))
  t2_to_t1 <- all(sapply(names(t2_attr), \(nm) all(t2_attr[[nm]] == t1_attr[[nm]])))
  obj_to_obj <- all(t1 == t2)
  
  return(all(t1_to_t2, t2_to_t1, obj_to_obj))
  
}

#### GroupConditions ####

age_grouping <- GroupConditions(
  conditions_category = "Age",
  "below 18" ~ age < 18,
  "18-22" ~ age >= 18 & age <= 22,
  "23-26" ~ age >= 23 & age <= 26,
  "27-32" ~ age >= 27 & age <= 32,
  "33-40" ~ age >= 33 & age <= 40,
  "40-60" ~ age >= 40 & age <= 60,
  "above 60" ~ age > 60
)
sex_grouping <- GroupConditions(
  conditions_category = "Sex",
  "Male" ~ sex == "M",
  "Female" ~ sex == "F"
)

####   testing Scoring Tables creation   ####
##               ungrouped                 ##

NEO_N_ft <- FrequencyTable(IPIP_NEO_300$N)
NEO_N_st <- ScoreTable(NEO_N_ft, scale = list(STEN, STANINE))
NEO_N_ST <- to_ScoringTable(NEO_N_st, min_raw = 60, max_raw = 300, scale = "sten")

plots_ungrouped <- list(ft = plot(NEO_N_ft), 
                        st = plot(NEO_N_st, "sten"))

##        grouped on one condition         ##
NEO_N_1gft <- GroupedFrequencyTable(IPIP_NEO_300, age_grouping, var = "N")
NEO_N_1gst <- GroupedScoreTable(NEO_N_1gft, scale = STEN)
NEO_N_1gST <- to_ScoringTable(NEO_N_1gst, min_raw = 60, max_raw = 300, scale = "sten")

plots_1_grouped <- list(ft = plot(NEO_N_1gft),
                        ft_filt = plot(NEO_N_1gft, group_names = c("below 18", ".all")),
                        st = plot(NEO_N_1gst, "sten"),
                        st_filt = plot(NEO_N_1gst, "sten", group_names = ".all"))

##        grouped on two conditions        ##
NEO_N_2gft <- GroupedFrequencyTable(IPIP_NEO_300, list(age_grouping, sex_grouping), var = "N")
NEO_N_2gst <- GroupedScoreTable(NEO_N_2gft, scale = STEN)
NEO_N_2gST <- to_ScoringTable(NEO_N_2gst, min_raw = 60, max_raw = 300, scale = "sten")

plots_2_grouped <- list(ft = plot(NEO_N_2gft),
                        ft_filt = plot(NEO_N_2gft, strict_names = TRUE, group_names = c("below 18:Female", "40-60:Male")),
                        st = plot(NEO_N_2gst, "sten"),
                        st_filt = plot(NEO_N_2gst, "sten", strict_names = FALSE, group_names = "Female"))

##        normalize using ScoreTables      ##

retained <- normalize_scores_scoring(
  data = IPIP_NEO_300,
  vars = "N",
  NEO_N_ST,
  retain = T
)

retained_1g <- normalize_scores_scoring(
  data = IPIP_NEO_300,
  vars = "N",
  NEO_N_1gST,
  retain = T,
  group_col = "group"
)

####  export & import back ScoringTables  ####
ST_csv <- tempfile(fileext = ".csv")
GC_csv <- tempfile(fileext = ".csv")
ST_json <- tempfile(fileext = ".json")

for (ST in list(NEO_N_ST, NEO_N_1gST, NEO_N_2gST)) {
  
  export_ScoringTable(
    table = ST,
    out_file = ST_csv,
    method = "csv",
    conditions_file = GC_csv)
  
  export_ScoringTable(
    table = ST,
    out_file = ST_json,
    method = "json"
  )
  
  imported_csv <- import_ScoringTable(
    source = ST_csv,
    method = "csv",
    cond_file = GC_csv
  )
  
  imported_json <- import_ScoringTable(
    source = ST_json,
    method = "json"
  )
  
  if (!compare_tables(ST, imported_csv))
    stop("NEIN")
  if (!compare_tables(ST, imported_json))
    stop("NEIN2")
  
}


###  import scoretable created by hand  ###

SLCS_ST <- import_ScoringTable(
  "../stenR_scoringTable.csv",
  "csv",
  conditions = list(sex_grouping_sh, ageNstu_grouping)
)

## We have our raw data with "age" column and `Neuroiticism` saved as `N`
IPIP_NEO_300 |> head()

## regular csv ScoringTable for `Neuroticism` variable in various age groups
# (.all column is available in all `stenR` created tables by default)
readLines(ST_csv) |> writeLines()

## to import ScoringTable from csv we needed to create the grouping conditions
age_grouping <- GroupConditions(
  conditions_category = "Age",
  "below 18" ~ age < 18,
  "18-22" ~ age >= 18 & age <= 22,
  "23-26" ~ age >= 23 & age <= 26,
  "27-32" ~ age >= 27 & age <= 32,
  "33-40" ~ age >= 33 & age <= 40,
  "40-60" ~ age >= 40 & age <= 60,
  "above 60" ~ age > 60
)

## now, we can safely import
ST_imported <- import_ScoringTable(
  source_file = ST_csv,
  method = "csv",
  conditions = age_grouping
)

## and normalize!
normalized <- normalize_scores_scoring(
  data = IPIP_NEO_300,
  vars = "N",
  ST_imported,
  retain = T,
  group_col = "group"
)

head(normalized)
