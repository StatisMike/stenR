# having a ScoreTable with one StandardScale attached
st <- ScoreTable(FrequencyTable(HEXACO_60$HEX_C), STEN)
st$scale
names(st$table)

# possibly attach more scales to ScoreTable
st <- attach_scales(st, list(STANINE, WECHSLER_IQ))
st$scale
names(st$table)
