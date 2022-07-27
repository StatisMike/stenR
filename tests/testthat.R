library(testthat)
library(stenR)

expect_plot_groups <- function(grouped_table, groups, strict = TRUE) {
  
  plot <- plot(grouped_table, group_names = groups, strict_names = strict)
  
  if (isTRUE(strict) && is.intersected(grouped_table)) {
    
    table_group <- names(grouped_table)
    table_group <- table_group[table_group %in% groups]
    
    plot_group <- unique(paste(plot$data$group1, plot$data$group2, sep = ":"))
    plot_group <- plot_group[plot_group %in% groups]
    
    expect_equal(table_group, as.character(plot_group))
    
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
    
    expect_equal(table_group, as.character(plot_group))
    
  }
}

test_check("stenR")
