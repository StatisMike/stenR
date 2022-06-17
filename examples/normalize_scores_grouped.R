#### create GroupConditions for calculating tables ####
age_grouping <- GroupConditions(
  "below 18" ~ age < 18,
  "18-22" ~ age >= 18 & age <= 22,
  "23-26" ~ age >= 23 & age <= 26,
  "27-32" ~ age >= 27 & age <= 32,
  "33-40" ~ age >= 33 & age <= 40,
  "40-60" ~ age >= 40 & age <= 60,
  "above 60" ~ age > 60
)
sex_grouping <- GroupConditions(
  "Male" ~ sex == "M",
  "Female" ~ sex == "F"
)

#### create GroupedFrequencyTable ####
NEU_gft <- GroupedFrequencyTable(
  data = IPIP_NEO_300,
  conditions = list(age_grouping, sex_grouping),
  var = "N"
)

#### create GroupedScoreTable ####
NEU_gst <- GroupedScoreTable(
  NEU_gft,
  scale = list(STEN, STANINE)
)

#### normalize scores ####
# to Z score or quantile using GroupedFrequencyTable
normalized_to_quan <- normalize_scores_grouped(
  IPIP_NEO_300,
  vars = "N",
  NEU_gft,
  what = "quan",
  retain = c("sex", "age")
)

# only 'sex' and 'age' are retained
head(normalized_to_quan)

# to StandardScale attached to GroupedScoreTable
normalized_to_STEN <- normalize_scores_grouped(
  IPIP_NEO_300,
  vars = "N",
  NEU_gst,
  what = "stanine",
  retain = F,
  group_col = "sex_age_group"
)

# none is retained, 'sex_age_goup' is created
head(normalized_to_STEN)
