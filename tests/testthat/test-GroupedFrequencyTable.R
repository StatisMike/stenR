ft_summary_names <- 
  c("group", "n", "min", "max", "mean", "median", "sd", "skewness", "kurtosis", "incomplete")

expect_plot_groups <- function(grouped_table, groups, strict = TRUE) {
  
  plot <- plot(grouped_table, group_names = groups, strict_names = strict)
  
  if (isTRUE(strict) && is.intersected(grouped_table)) {
    
    table_group <- names(grouped_table)
    table_group <- table_group[table_group %in% groups]
    
    plot_group <- unique(paste(plot$data$group1, plot$data$group2, sep = ":"))
    plot_group <- plot_group[plot_group %in% groups]
    
    expect_equal(table_group, plot_group)
    
  } else if (!isTRUE(strict) && is.intersected(grouped_table)) {
    
    table_names <- strsplit(names(grouped_table), split = ":")
    table_group1 <- unique(sapply(table_names, \(x) x[1]))
    table_group2 <- unique(sapply(table_names, \(x) x[2]))
    table_group1 <- table_group1[table_group1 %in% groups]
    table_group2 <- table_group2[table_group2 %in% groups]
    
    plot_group1 <- unique(plot$data$group1)
    plot_group1 <- plot_group1[plot_group1 %in% groups]
    plot_group2 <- unique(plot$data$group2)
    plot_group2 <- plot_group2[plot_group2 %in% groups]
    
    expect_equal(table_group1, as.character(plot_group1))
    expect_equal(table_group2, as.character(plot_group2))
    
    
  } else {
    
    table_group <- names(grouped_table)
    table_group <- table_group[table_group %in% groups]
    
    plot_group <- unique(plot$data$group1)
    plot_group <- plot_group[plot_group %in% groups]
    
    expect_equal(table_group, plot_group)
    
  }
}

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

test_that("GroupedFrequencyTable is created correctly", {
  
  suppressMessages(
    Neu_1g <<- GroupedFrequencyTable(
      data = IPIP_NEO_300,
      conditions = sex_grouping, 
      var = "N")    
  )

  suppressMessages(
    Neu_2g <<- GroupedFrequencyTable(
      data = IPIP_NEO_300,
      conditions = list(sex_grouping, age_grouping), 
      var = "N"
    )  
  )
  
  expect_s3_class(Neu_1g, class = "GroupedFrequencyTable")
  expect_s3_class(Neu_2g, class = "GroupedFrequencyTable")
  
})

test_that("GroupedFrequencyTable prints correctly", {
  
  expect_message(print(Neu_1g), regexp = "<GroupedFrequencyTable>")
  expect_message(print(Neu_2g), regexp = "<GroupedFrequencyTable>")
  
})

test_that("GroupedFrequencyTable summaries correctly", {
  
  for (ft in list(Neu_1g, Neu_2g)) {
    
    ft_sum <- summary(ft)
    
    expect_s3_class(ft_sum, "data.frame")
    expect_equal(nrow(ft_sum), length(ft))
    expect_equal(names(ft_sum), ft_summary_names)
    
  }
})

test_that("GroupedFrequencyTable plots correctly", {
  
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