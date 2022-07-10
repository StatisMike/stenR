# having a ScoreTable object
st <- ScoreTable(FrequencyTable(HEXACO_60$HEX_X), TANINE)
class(st)

# revert it back to the FrequencyTable
ft <- strip_ScoreTable(st)
class(ft)
