# normalize with FrequencyTable
suppressMessages(
  ft <- FrequencyTable(HEXACO_60$HEX_H)
)

normalize_score(HEXACO_60$HEX_H[1:5], ft, what = "Z")

# normalize with ScoreTable
st <- ScoreTable(ft, list(STEN, STANINE))

normalize_score(HEXACO_60$HEX_H[1:5], st, what = "sten")
normalize_score(HEXACO_60$HEX_H[1:5], st, what = "stanine")
