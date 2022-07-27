sex_grouping <- GroupConditions(
  conditions_category = "Sex",
  "Male" ~ sex == "M",
  "Female" ~ sex == "F"
)

age_grouping <- GroupConditions(
  conditions_category = "Age",
  "to 20" ~ age < 20,
  "20 to 40" ~ age >= 20 & age <= 40,
  "40 to 60" ~ age >= 40 & age < 60
)

Neu_1g <- NULL
Neu_2g <- NULL

suppressMessages(
  Neu_1g <- GroupedFrequencyTable(
    data = IPIP_NEO_300,
    conditions = sex_grouping, 
    var = "N")    
)

suppressMessages(
  Neu_2g <- GroupedFrequencyTable(
    data = IPIP_NEO_300,
    conditions = list(sex_grouping, age_grouping), 
    var = "N"
  )  
)

test_that("GroupedScoreTable is created correctly", {
  
  Neu_1g <<- GroupedScoreTable(Neu_1g, STEN)
  Neu_2g <<- GroupedScoreTable(Neu_2g, STEN)
  
  expect_s3_class(Neu_1g, class = "GroupedScoreTable")
  expect_s3_class(Neu_2g, class = "GroupedScoreTable")
  
})

test_that("GroupedScoreTable prints correctly", {
  
  expect_message(print(Neu_1g), regexp = "<GroupedScoreTable>")
  expect_message(print(Neu_2g), regexp = "<GroupedScoreTable>")
  
})


test_that("GroupedScoreTable plots correctly", {
  
  plot_1g <- plot(Neu_1g)
  
  expect_s3_class(plot_1g$facet$super(), "FacetWrap")
  expect_equal(unique(plot_1g$data$group1), names(Neu_1g))
  
  expect_plot_groups(grouped_table = Neu_1g,
                     groups = c("Male", "Female"))
  
  plot_2g <- plot(Neu_2g)
  expect_s3_class(plot_2g$facet$super(), "FacetGrid")
  
  plot_2g <- plot(Neu_2g, plot_grid = F)
  expect_s3_class(plot_2g$facet$super(), "FacetWrap")
  
  expect_plot_groups(grouped_table = Neu_2g,
                     groups = c("Male"),
                     strict = F)
  
  expect_plot_groups(grouped_table = Neu_2g,
                     groups = c("Male:to 20", "Female:to 20"))
  
})