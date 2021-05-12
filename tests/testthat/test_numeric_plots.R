library(ggplot2)
library(testthat)

library(soep.plots)

source("helpers.R")

# Set up
fields <- list(
  "years" = list("label" = "Survey Year"),
  "meanincome" = list("label" = "Mean Income")
)
years <- as.factor(c("2000", "2001", "2002", "2003"))
meanincome <- c(1000, 2000, 3000, 1500)
upper_confidence <- c(1100, 2100, 3200, 1600)
lower_confidence <- c(900, 1800, 2900, 1000)
input_table <- data.frame(years, meanincome, lower_confidence, upper_confidence)


test_that("NumericPlot Object initialization", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "years",
    y_axis = "meanincome",
    group_by = vector()
  )
  expect_true(inherits(result_plotting_object, "NumericPlot"))
  expect_type(result_plotting_object$fields, "list")
  expect_identical(fields, result_plotting_object$fields)
  expect_true(is.data.frame(result_plotting_object$data))
  expect_identical(input_table, result_plotting_object$data)
})


test_that("NumericPlot plotting.", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "years",
    y_axis = "meanincome",
    group_by = vector()
  )
  expected_plot <- ggplot(
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
    xlab("Survey Year") +
    geom_ribbon(
      aes(
        ymin = lower_confidence, ymax = upper_confidence
      ),
      linetype = 2, alpha = .1
    )

  result_plot <- result_plotting_object$plot()

  expect_plots_equal(expected_plot, result_plot)
})

test_that("Test grouping", {
  years <- as.factor(c(
    "2000",
    "2001",
    "2002",
    "2003",
    "2000",
    "2001",
    "2002",
    "2003"
  ))
  meanincome <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
  groups <- as.factor(c("a", "a", "a", "a", "b", "b", "b", "b"))
  upper_confidence <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  lower_confidence <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  group_input_table <- data.frame(
    years,
    meanincome,
    groups,
    lower_confidence,
    upper_confidence
  )

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = group_input_table,
    x_axis = "years",
    y_axis = "meanincome",
    group_by = c("groups")
  )

  expected_plot <- ggplot(
    group_input_table,
    aes(x = years, y = meanincome, group = groups, color = groups)
  ) +
    geom_line() +
    expand_limits(y = 0) +
    scale_x_discrete(breaks = group_input_table$years) +
    scale_y_continuous(
      breaks = seq(0, max(group_input_table$meanincome), by = 500)
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    ) +
    ylab("Mean Income") +
    xlab("Survey Year") +
    geom_ribbon(
      aes_string(
        ymin = "lower_confidence",
        ymax = "upper_confidence"
      ),
      linetype = 2, alpha = .1
    )

  result_plot <- result_plotting_object$plot()
  expect_plots_equal(expected_plot, result_plot)
})



test_that("Test confidence interval", {
  fields_ <- list(
    "years" = list("label" = "Survey Year"),
    "meanincome" = list("label" = "Mean Income")
  )

  years <- as.factor(c(
    "2000",
    "2001",
    "2002",
    "2003",
    "2000",
    "2001",
    "2002",
    "2003"
  ))
  meanincome <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
  groups <- as.factor(c("a", "a", "a", "a", "b", "b", "b", "b"))
  upper_confidence <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  lower_confidence <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  ci_input_table <- data.frame(
    years,
    meanincome,
    groups,
    lower_confidence,
    upper_confidence
  )

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields_,
    data = ci_input_table,
    x_axis = "years",
    y_axis = "meanincome",
    group_by = c("groups")
  )

  expected_plot <- ggplot(
    ci_input_table,
    aes(x = years, y = meanincome, group = groups, color = groups)
  ) +
    geom_line() +
    expand_limits(y = 0) +
    scale_x_discrete(breaks = ci_input_table$years) +
    scale_y_continuous(
      breaks = seq(
        0,
        max(ci_input_table$meanincome),
        by = 500
      )
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    ) +
    ylab("Mean Income") +
    xlab("Survey Year")

  expected_ci_plot <- expected_plot +
    geom_ribbon(
      aes_string(
        ymin = "lower_confidence",
        ymax = "upper_confidence"
      ),
      linetype = 2, alpha = .1
    )

  result <- result_plotting_object$plot()
  expect_plots_equal(expected_ci_plot, result)

  result_plotting_object$disable_confidence_interval()
  result <- result_plotting_object$plot()
  expect_plots_equal(expected_plot, result)

  result_plotting_object$enable_confidence_interval()
  result <- result_plotting_object$plot()
  expect_plots_equal(expected_ci_plot, result)
})
