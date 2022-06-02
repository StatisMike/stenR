# normalize with FrequencyTable
ft <- FrequencyTable(HEXACO_60$HEX_H)

normalize_score(HEXACO_60$HEX_H[1:5], ft)

# normalize with ScoreTable
st <- ScoreTable(ft, list(STEN, STANINE))

normalize_score(HEXACO_60$HEX_H[1:5], st, "sten")
normalize_score(HEXACO_60$HEX_H[1:5], st, "stanine")
