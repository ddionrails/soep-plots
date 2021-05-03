library(ggplot2)
library(testthat)

library(soep.plots)

# Set up
fields <- list(
  "years" = list("label" = "Years"),
  "meanincome" = list("label" = "Mean Income")
)
years <- as.factor(c("2000", "2001", "2002", "2003"))
meanincome <- c(1000, 2000, 3000, 1500)
input_table <- data.frame(years, meanincome)

#' Saves plots to image files and compares their file hashes.
#'
#' @param baseline The expected ggplot object to compare result to.
#' @param compare The ggplot result of a test.
#' @return The md5 hash of the result file.
#' @examples
#' expected <- ggplot(dataframe)
#' result <- test_function() # Returns a ggplot object.
#' expected_plots_equal(expected, result)
expect_plots_equal <- function(expected, result) {
  expected_file <- "baseline.png"
  compare_file <- "compare.png"
  withr::local_file(expected_file)
  withr::local_file(compare_file)
  ggplot2::ggsave(filename = expected_file, plot = expected, type = "cairo")
  ggplot2::ggsave(filename = compare_file, plot = result, type = "cairo")
  expect(
    tools::md5sum(files = expected_file) == tools::md5sum(files = compare_file),
    sprintf("Result Plot does not look like expected plot.")
  )
  return(tools::md5sum(files = compare_file))
}

# Tests
test_that("NumericPlot Object initialization", {
  numeric_plot <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table
  )
  expect_true(inherits(numeric_plot, "NumericPlot"))
  expect_type(numeric_plot$fields, "list")
  expect_identical(fields, numeric_plot$fields)
  expect_true(is.data.frame(numeric_plot$data))
  expect_identical(input_table, numeric_plot$data)
})


test_that("NumericPlot plotting.", {
  numeric_plot <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table
  )
  plot <- ggplot(
    input_table,
    aes(x = years, y = meanincome, group = "")
  ) +
    geom_line() +
    expand_limits(y = 0) +
    scale_x_discrete(breaks = input_table$years) +
    scale_y_continuous(breaks = seq(0, max(input_table$meanincome), by = 500)) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    ) +
    ylab("Mean Income") +
    xlab("Years")

  result <- numeric_plot$plot(
    x_axis = "years", y_axis = "meanincome", group_by = c()
  )

  expect_plots_equal(plot, result)


  fields_ <- list(
    "years" = list("label" = "Survey Year"),
    "meanincome" = list("label" = "Mean Income")
  )

  # Test variability
  years <- as.factor(c("2000", "2001", "2002", "2003", "2000", "2001", "2002", "2003"))
  meanincome <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
  groups <- as.factor(c("a", "a", "a", "a", "b", "b", "b", "b"))
  input_table <- data.frame(years, meanincome, groups)

  numeric_plot <- soep.plots::numeric_plot(
    fields = fields_,
    data = input_table
  )

  plot <- ggplot(
    input_table,
    aes(x = years, y = meanincome, group = groups, color = groups)
  ) +
    geom_line() +
    expand_limits(y = 0) +
    scale_x_discrete(breaks = input_table$years) +
    scale_y_continuous(breaks = seq(0, max(input_table$meanincome), by = 500)) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    ) +
    ylab("Mean Income") +
    xlab("Survey Year")

  result <- numeric_plot$plot(
    x_axis = "years", y_axis = "meanincome", group_by = c("groups")
  )

  expect_plots_equal(plot, result)
})
