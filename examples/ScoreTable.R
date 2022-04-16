# firstly compute FrequencyTable for a variable
ft <- FrequencyTable(HEXACO_60$HEX_A)

# then create a ScoreTable
st <- ScoreTable(ft, STEN)

# possibly attach more scales to ScoreTable
st <- attach_scales(st, list(STANINE, WECHSLER_IQ))
