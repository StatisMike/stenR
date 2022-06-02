# normalize multiple variables with FrequencyTable
ft_H <- FrequencyTable(HEXACO_60$HEX_H)
ft_E <- FrequencyTable(HEXACO_60$HEX_E)
ft_X <- FrequencyTable(HEXACO_60$HEX_X)

normalize_scores_df(data = head(HEXACO_60), 
                    vars = c("HEX_H", "HEX_E", "HEX_X"),
                    ft_H,
                    ft_E,
                    ft_X,
                    what = "quan")

# normalize multiple variables with ScoreTable
st_H <- ScoreTable(ft_H, STEN)
st_E <- ScoreTable(ft_E, STEN)
st_X <- ScoreTable(ft_X, STEN)

normalize_scores_df(data = head(HEXACO_60), 
                    vars = c("HEX_H", "HEX_E", "HEX_X"),
                    st_H,
                    st_E,
                    st_X,
                    what = "sten")
