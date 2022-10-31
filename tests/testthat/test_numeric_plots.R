library(ggplot2)
library(testthat)

library(soep.plots)

source("helpers.R")

# Set up
fields <- list(
  "year" = list("label" = "Survey Year"),
  "mean" = list("label" = "Mean Income"),
  "median" = list("label" = "Median Income")
)
year <- as.integer(c("2000", "2001", "2002", "2003"))
mean <- c(1000, 2000, 3000, 1500)
n <- c(5000, 5400, 4500, 5000)
upper_confidence_mean <- c(1100, 2100, 3200, 1600)
upper_confidence_median <- c(1100, 2100, 3200, 1600)
lower_confidence_mean <- c(900, 1800, 2900, 1000)
lower_confidence_median <- c(900, 1800, 2900, 1000)
percentile_10 <- c(500, 500, 1000, 500)
percentile_25 <- c(700, 1000, 2000, 1000)
percentile_75 <- c(1500, 2500, 4000, 2000)
percentile_90 <- c(2000, 3000, 5000, 3000)
median <- c(1000, 2000, 3000, 1500)
input_table <- data.frame(
  year,
  mean,
  n,
  lower_confidence_mean,
  lower_confidence_median,
  upper_confidence_mean,
  upper_confidence_median,
  percentile_10,
  percentile_25,
  percentile_75,
  percentile_90,
  median
)

plot_theme <- theme(
  axis.text = element_text(size = 12),
  axis.text.x = element_text(size = 11, angle = -50),
  axis.title = element_text(size = 14, face = "bold"),
  legend.text = element_text(size = 12),
  legend.title = element_blank()
)


test_that("NumericPlot Object initialization", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "mean"
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
    x_axis = "year",
    y_axis = "mean",
  )
  expected_plot <- ggplot(
    input_table,
    aes(x = year, y = mean, group = "")
  ) +
    geom_path() +
    geom_point(size = 2, shape = 3) +
    expand_limits(y = 0) +
    scale_x_continuous(breaks = input_table$year) +
    scale_y_continuous(breaks = seq(0, max(input_table$mean), by = 500)) +
    plot_theme +
    ylab("Mean Income") +
    xlab("Survey Year") +
    geom_ribbon(
      aes(
        ymin = lower_confidence_mean, ymax = upper_confidence_mean
      ),
      linetype = 2, alpha = .1
    )

  result_plot <- result_plotting_object$plot()

  expect_plots_equal(expected_plot, result_plot)
})

test_that("NumericPlot boxplot.", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "mean",
  )
  result_plotting_object$set_to_boxplot()
  expected_plot <- ggplot(
    input_table,
    aes(
      x = year,
      y = mean,
      ymin = percentile_10,
      ymax = percentile_90,
      lower = percentile_25,
      middle = median,
      upper = percentile_75,
      group = year
    )
  ) +
    geom_boxplot(stat = "identity") +
    coord_cartesian() +
    expand_limits(y = 0) +
    expand_limits(y = 0) +
    scale_x_continuous(breaks = input_table$year) +
    scale_y_continuous(breaks = seq(0, max(input_table$percentile_90), by = 500)) +
    plot_theme +
    ylab("Mean Income") +
    xlab("Survey Year")

  result_plot <- result_plotting_object$plot()

  expect_plots_equal(expected_plot, result_plot)
})

test_that("Test set_y_scale_limit.", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "mean",
  )
  result_plotting_object$set_y_scale_limits(y_scale_limits = c(1, 4000))
  expected_plot <- ggplot(
    input_table,
    aes(x = year, y = mean, group = "")
  ) +
    geom_path() +
    geom_point(size = 2, shape = 3) +
    expand_limits(y = 0) +
    scale_x_continuous(breaks = input_table$year) +
    scale_y_continuous(breaks = seq(1, 4000, by = 500), limits = c(1, 4000)) +
    plot_theme +
    ylab("Mean Income") +
    xlab("Survey Year") +
    geom_ribbon(
      aes(
        ymin = lower_confidence_mean, ymax = upper_confidence_mean
      ),
      linetype = 2, alpha = .1
    )

  result_plot <- result_plotting_object$plot()

  expect_plots_equal(expected_plot, result_plot)
})


test_that("Test grouping", {
  year <- as.integer(c(
    "2000",
    "2001",
    "2002",
    "2003",
    "2000",
    "2001",
    "2002",
    "2003"
  ))
  mean <- c(1000, 2000, 3000, NA, 1218, 1804, 3136, 1637)
  n <- c(1000, 2000, 3000, NA, 1218, 1804, 3136, 1637)
  groups <- as.factor(c("a", "a", "a", "a", "b", "b", "b", "b"))
  upper_confidence_mean <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  lower_confidence_mean <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  group_input_table <- data.frame(
    year,
    mean,
    n,
    groups,
    lower_confidence_mean,
    upper_confidence_mean
  )
  group_input_table <- group_input_table[complete.cases(group_input_table$mean), ]

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = group_input_table,
    x_axis = "year",
    y_axis = "mean",
    group_axis = c("groups")
  )

  expected_plot <- ggplot(
    group_input_table,
    aes(x = year, y = mean, group = groups, color = groups)
  ) +
    geom_path() +
    geom_point(size = 2, shape = 3) +
    expand_limits(y = 0) +
    scale_x_continuous(
      breaks = seq(
        min(group_input_table$year), max(group_input_table$year),
        by = 1
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, max(group_input_table$mean, na.rm = TRUE), by = 500)
    ) +
    plot_theme +
    ylab("Mean Income") +
    xlab("Survey Year") +
    geom_ribbon(
      aes_string(
        ymin = "lower_confidence_mean",
        ymax = "upper_confidence_mean"
      ),
      linetype = 2, alpha = .1
    )

  result_plot <- result_plotting_object$plot()
  expect_plots_equal(expected_plot, result_plot)
})


test_that("Test boxplot grouping", {
  year <- as.integer(c(
    "2000",
    "2001",
    "2002",
    "2003",
    "2000",
    "2001",
    "2002",
    "2003"
  ))
  mean <- c(1000, 2000, 3000, NA, 1218, 1804, 3136, 1637)
  median <- c(1000, 2000, 3000, NA, 1218, 1804, 3136, 1637)
  percentile_10 <- c(500, 500, 1000, NA, 500, 500, 1000, 500)
  percentile_25 <- c(700, 1000, 2000, NA, 700, 1000, 2000, 1000)
  percentile_75 <- c(1500, 2500, 4000, NA, 1500, 2500, 4000, 2000)
  percentile_90 <- c(2000, 3000, 5000, NA, 2000, 3000, 5000, 3000)
  random <- c(1, 2, 3, 4, 5, 6, 7, 8)
  n <- c(1000, 2000, 3000, NA, 1218, 1804, 3136, 1637)
  groups <- c("a", "a", "a", "a", "b", "b", "b", "b")
  upper_confidence_median <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  lower_confidence_median <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  group_input_table <- data.frame(
    year,
    mean,
    n,
    groups,
    lower_confidence_median,
    upper_confidence_median,
    percentile_10,
    percentile_25,
    percentile_75,
    percentile_90,
    median,
    random
  )
  group_input_table <- group_input_table[complete.cases(group_input_table$mean), ]

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = group_input_table,
    x_axis = "year",
    y_axis = "median",
    group_axis = c("groups")
  )
  result_plotting_object$set_to_boxplot()

  group_input_table <- result_plotting_object$get_data()

  expected_plot <-
    ggplot(
      group_input_table,
      aes(
        x = year,
        y = median,
        group = paste0(groups, year),
        ymin = percentile_10,
        ymax = percentile_90,
        lower = percentile_25,
        middle = median,
        upper = percentile_75,
        color = groups,
        text = ""
      )
    ) +
    geom_boxplot(stat = "identity") +
    coord_cartesian() +
    expand_limits(y = 0) +
    scale_x_continuous(breaks = seq(2000, 2003, by = 1)) +
    scale_y_continuous(breaks = seq(0, max(group_input_table$percentile_90), by = 500)) +
    plot_theme +
    ylab("Median Income") +
    xlab("Survey Year")

  result_plotting_object$disable_confidence_interval()
  result_plot <- result_plotting_object$plot()
  expect_plots_equal(expected_plot, result_plot)
})


test_that("Test several groups", {
  year <- as.integer(c(
    "2000",
    "2001",
    "2002",
    "2003",
    "2000",
    "2001",
    "2002",
    "2003"
  ))
  mean <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
  n <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
  first_dimension <- c("a", "a", "a", "a", "b", "b", "b", "b")
  second_dimension <- c("ba", "ba", "ba", "ba", "ba", "ab", "ab", "ab")
  combined_dimension <- c("a ba", "a ba", "a ba", "a ba", "b ba", "b ab", "b ab", "b ab")
  upper_confidence_mean <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  lower_confidence_mean <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  group_input_table <- data.frame(
    year,
    mean,
    n,
    first_dimension,
    second_dimension,
    lower_confidence_mean,
    upper_confidence_mean
  )

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = group_input_table,
    x_axis = "year",
    y_axis = "mean",
    group_axis = c("first_dimension", "second_dimension")
  )

  group_input_table["groups"] <- combined_dimension

  testthat::expect_equal(
    combined_dimension,
    result_plotting_object$data$merged_group_name
  )

  expected_plot <- ggplot(
    group_input_table,
    aes(x = year, y = mean, group = groups, color = groups)
  ) +
    geom_path() +
    geom_point(size = 2, shape = 3) +
    coord_cartesian() +
    expand_limits(y = 0) +
    scale_x_continuous(
      breaks = seq(
        min(group_input_table$year), max(group_input_table$year),
        by = 1
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, max(group_input_table$mean), by = 500)
    ) +
    plot_theme +
    ylab("Mean Income") +
    xlab("Survey Year") +
    geom_ribbon(
      aes_string(
        ymin = "lower_confidence_mean",
        ymax = "upper_confidence_mean"
      ),
      linetype = 2, alpha = .1
    )

  result_plot <- result_plotting_object$plot()
  expect_plots_equal(expected_plot, result_plot)
})




test_that("Test confidence interval", {
  fields_ <- list(
    "year" = list("label" = "Survey Year"),
    "mean" = list("label" = "Mean Income")
  )

  year <- as.integer(
    c("2000", "2001", "2002", "2003", "2000", "2001", "2002", "2003")
  )
  mean <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
  n <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
  groups <- as.factor(c("a", "a", "a", "a", "b", "b", "b", "b"))
  upper_confidence_mean <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  lower_confidence_mean <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  ci_input_table <- data.frame(
    year,
    mean,
    n,
    groups,
    lower_confidence_mean,
    upper_confidence_mean
  )

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields_,
    data = ci_input_table,
    x_axis = "year",
    y_axis = "mean",
    group_axis = c("groups")
  )

  expected_plot <- ggplot(
    ci_input_table,
    aes(x = year, y = mean, group = groups, color = groups)
  ) +
    geom_path() +
    geom_point(size = 2, shape = 3) +
    expand_limits(y = 0) +
    scale_x_continuous(
      breaks = seq(min(ci_input_table$year), max(ci_input_table$year), by = 1)
    ) +
    scale_y_continuous(
      breaks = seq(
        0,
        max(ci_input_table$mean),
        by = 500
      )
    ) +
    plot_theme +
    ylab("Mean Income") +
    xlab("Survey Year")

  expected_ci_plot <- expected_plot +
    geom_ribbon(
      aes_string(
        ymin = "lower_confidence_mean",
        ymax = "upper_confidence_mean"
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


test_that("Year Range", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "mean"
  )
  subset_table <- subset(input_table, year %in% seq(2000, 2002))
  expected_plot <- ggplot(
    subset_table,
    aes(x = year, y = mean, group = "")
  ) +
    geom_path() +
    geom_point(size = 2, shape = 3) +
    expand_limits(y = 0) +
    scale_x_continuous(breaks = seq(2000, 2002)) +
    scale_y_continuous(breaks = seq(0, max(subset_table$mean), by = 500)) +
    plot_theme +
    ylab("Mean Income") +
    xlab("Survey Year") +
    geom_ribbon(
      aes(
        ymin = lower_confidence_mean, ymax = upper_confidence_mean
      ),
      linetype = 2, alpha = .1
    )

  result_plotting_object$set_year_range(year_range = c(2000, 2002))
  result_plot <- result_plotting_object$plot()

  expect_plots_equal(expected_plot, result_plot)
})
