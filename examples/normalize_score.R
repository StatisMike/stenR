# normalize with FrequencyTable
ft <- FrequencyTable(HEXACO_60$HEX_H)

normalizeScore(HEXACO_60$HEX_H[1:5], ft)

# normalize with ScoreTable
st <- ScoreTable(ft, list(STEN, STANINE))

normalizeScore(HEXACO_60$HEX_H[1:5], st, "sten")
normalizeScore(HEXACO_60$HEX_H[1:5], st, "stanine")
