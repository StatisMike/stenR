# all .warnings for functions

.warnings <- list(
  # stop message for not probiding data in data.frame object
  data.frame_required = "Data should be provided in form of the 'data.frame' object.",

  # stop message for not providing the "scale" argument when it is needed
  valid_scale_required = "Please provide valid 'scale' argument. You can only get scores for scale which was computed first by using 'compute_scores()' method",

  # stop message for when id isn't specified with keep_data set to TRUE
  missing_id_for_keep = "While the 'keep_data' is set to TRUE, you also need to provide column name for 'id'",

  # warning for mussing values also returns for which variables the warnings are generated
  missing_values = function(name){
    paste0(
      "For [", name,
      "]: There are missing score values between minimum and maximum scores. They have been filled automatically, though have in mind that you should get more representative sample.")
  },

  # stop message for bad scale specification
  bad_scale_specification = "Currently, the 'scale' argument needs to be either character value: one of 'sten', 'stanine', 'tanine',
               'tetronic', 'wechsler-iq' or list containing objects 'M' (mean), 'SD' (standard deviation), 'min'
               (minimal score) and 'max' (maximal score) of other scoring scale.",

  # stop message if there is not kept data and it is needed
  need_source_data = "You tried to do something that requires your class to have kept the data.",

  # stop message if provided id is not valid
  valid_id_required = "Please provide valid ids. All of the provided ids should be in the data.",

  # stop message if provided variable name is not valid
  bad_var_name = "Please provide valid variable names.",

  # stop message if provided id name is not valid
  bad_id_name = "Please provide valid column name containing ids."
)
