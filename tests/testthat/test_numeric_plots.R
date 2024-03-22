library(testthat)
library(plotly)

library(soep.plots)

source("helpers.R")

options(warn = -1)

# Set up
fields <- list(
  "year" = list("label" = "Survey Year"),
  "mean" = list("label" = "Mean Income"),
  "median" = list("label" = "Median Income")
)
create_input_table <- function() {
  year <- as.integer(c("2000", "2001", "2002", "2003"))
  mean <- c(1000, 2000, 3000, 1500)
  n <- c(5000, 5400, 4500, 5000)
  mean_upper_confidence <- c(1100, 2100, 3200, 1600)
  median_upper_confidence <- c(1100, 2100, 3200, 1600)
  mean_lower_confidence <- c(900, 1800, 2900, 1000)
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
    mean_lower_confidence,
    lower_confidence_median,
    mean_upper_confidence,
    median_upper_confidence,
    percentile_10,
    percentile_25,
    percentile_75,
    percentile_90,
    median
  )
  return(input_table)
}

input_table <- create_input_table()

data_range <- list(0, 3499)
default_color <- "rgb(252, 141, 98)"

get_xaxis_layout <- function(title) {
  return(list(
    title = title,
    showline = TRUE,
    showgrid = FALSE,
    showticklabels = TRUE,
    linecolor = "rgb(204, 204, 204)",
    linewidth = 2,
    autotick = FALSE,
    ticks = "outside",
    tickcolor = "rgb(204, 204, 204)",
    tickwidth = 2,
    ticklen = 5,
    tickfont = list(
      family = "Arial",
      size = 12,
      color = "rgb(82, 82, 82)"
    )
  ))
}



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

  expected_plot <- plotly::plot_ly(
    input_table,
    y = ~mean,
    x = ~year,
    type = "scatter",
    mode = "lines+markers",
    linetype = "solid",
    fillcolor = "rgba(236, 236, 236, 0.5)",
    line = list(color = default_color),
    color = default_color,
    marker = list(
      symbol = "diamond",
      size = 8,
      line = list(width = 2, color = "black")
    ),
    text = "Some Text",
    hovertemplate = "%{text}"
  )

  expected_plot <- plotly::add_ribbons(
    expected_plot,
    ymin = ~mean_lower_confidence,
    ymax = ~mean_upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    hoverinfo = "none"
  )

  expected_plot <- layout(expected_plot,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["mean"]][["label"]],
      dtick = 500,
      range = data_range
    )
  )
  result_plot <- result_plotting_object$plot()

  expect_plotly_plots_equal(expected_plot, result_plot, debug = FALSE)
})

test_that("NumericPlot boxplot.", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "median",
  )
  result_plotting_object$set_to_boxplot()
  expected_plot <- plotly::plot_ly(
    data = input_table,
    x = ~ factor(year),
    type = "box",
    q1 = ~percentile_25,
    q3 = ~percentile_75,
    median = ~median,
    lowerfence = ~percentile_10,
    upperfence = ~percentile_90
  )
  expected_plot <- layout(expected_plot,
    # title = "Title",
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["median"]][["label"]],
      dtick = 500,
      range = c(0, 5499)
    )
  )


  result_plot <- result_plotting_object$plot()

  expect_plotly_plots_equal(expected_plot, result_plot)
})

test_that("Test set_y_scale_limit.", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "mean",
  )

  result_plotting_object$set_y_scale_limits(y_scale_limits = c(1, 4000))

  expected_plot <- plotly::plot_ly(
    input_table,
    y = ~mean,
    x = ~year,
    type = "scatter",
    mode = "lines+markers",
    linetype = "solid",
    fillcolor = "rgba(236, 236, 236, 0.5)",
    line = list(color = default_color),
    color = default_color,
    marker = list(
      symbol = "diamond",
      size = 8,
      line = list(width = 2, color = "black")
    ),
    text = "Some Text",
    hovertemplate = "%{text}"
  )

  expected_plot <- plotly::add_ribbons(
    expected_plot,
    ymin = ~mean_lower_confidence,
    ymax = ~mean_upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    hoverinfo = "none"
  )
  expected_plot <- layout(expected_plot,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["mean"]][["label"]],
      dtick = 500,
      range = c(1, 4000)
    )
  )
  result_plot <- result_plotting_object$plot()

  expect_plotly_plots_equal(expected_plot, result_plot)
})

get_group_input_table <- function() {
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
  mean_upper_confidence <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  mean_lower_confidence <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  median_upper_confidence <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  lower_confidence_median <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  group_input_table <- data.frame(
    year,
    mean,
    n,
    groups,
    mean_lower_confidence,
    mean_upper_confidence,
    lower_confidence_median,
    median_upper_confidence,
    percentile_10,
    percentile_25,
    percentile_75,
    percentile_90,
    median,
    random
  )
  group_input_table <- group_input_table[complete.cases(group_input_table$mean), ]
  return(group_input_table)
}


test_that("Test grouping", {
  group_input_table <- get_group_input_table()

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = group_input_table,
    x_axis = "year",
    y_axis = "mean",
    group_axis = c("groups")
  )

  expected_plot <- plotly::plot_ly(
    group_input_table,
    y = ~mean,
    x = ~year,
    type = "scatter",
    mode = "lines+markers",
    linetype = ~groups,
    color = ~groups,
    line = list(color = ~groups),
    marker = list(
      symbol = "diamond",
      size = 8,
      line = list(width = 2, color = "black")
    ),
    legendgroup = ~groups
  )
  expected_plot <- plotly::add_ribbons(
    expected_plot,
    ymin = ~mean_lower_confidence,
    ymax = ~mean_upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    legendgroup = ~groups,
    hoverinfo = "none"
  )
  expected_plot <- layout(expected_plot,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["mean"]][["label"]],
      dtick = 500,
      range = c(0, max(group_input_table[["mean"]], na.rm = TRUE) + 499)
    )
  )

  result_plot <- result_plotting_object$plot()
  expect_plotly_plots_equal(expected_plot, result_plot, debug = FALSE)
})


test_that("Test median", {
  group_input_table <- get_group_input_table()

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "median",
  )


  expected_plot <- plotly::plot_ly(
    input_table,
    y = ~median,
    x = ~year,
    type = "scatter",
    mode = "lines+markers",
    linetype = "solid",
    fillcolor = "rgba(236, 236, 236, 0.5)",
    line = list(color = default_color),
    color = default_color,
    marker = list(
      symbol = "diamond",
      size = 8,
      line = list(width = 2, color = "black")
    ),
    text = "Some Text",
    hovertemplate = "%{text}"
  )

  expected_plot <- plotly::add_ribbons(
    expected_plot,
    ymin = ~lower_confidence_median,
    ymax = ~median_upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    hoverinfo = "none"
  )

  expected_plot <- layout(expected_plot,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["median"]][["label"]],
      dtick = 500,
      range = c(0, max(input_table[["median"]], na.rm = TRUE) + 499)
    )
  )


  result_plot <- result_plotting_object$plot()


  result_plot <- result_plotting_object$plot()
  expect_plotly_plots_equal(expected_plot, result_plot)
})


test_that("Test boxplot grouping", {
  group_input_table <- get_group_input_table()

  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = group_input_table,
    x_axis = "year",
    y_axis = "median",
    group_axis = c("groups")
  )
  result_plotting_object$set_to_boxplot()

  expected_plot <- plotly::plot_ly(
    data = group_input_table,
    x = ~ factor(year),
    color = ~groups,
    type = "box",
    q1 = ~percentile_25,
    q3 = ~percentile_75,
    median = ~median,
    lowerfence = ~percentile_10,
    upperfence = ~percentile_90
  )
  expected_plot <- layout(expected_plot,
    boxmode = "group",
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["median"]][["label"]],
      dtick = 500,
      range = c(0, 5499)
    )
  )

  result_plotting_object$disable_confidence_interval()
  result_plot <- result_plotting_object$plot()
  expect_plotly_plots_equal(expected_plot, result_plot, debug = FALSE)
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
  mean_upper_confidence <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
  mean_lower_confidence <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
  group_input_table <- data.frame(
    year,
    mean,
    n,
    first_dimension,
    second_dimension,
    mean_lower_confidence,
    mean_upper_confidence
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

  expected_plot <- plotly::plot_ly(
    group_input_table,
    y = ~mean,
    x = ~year,
    type = "scatter",
    mode = "lines+markers",
    linetype = ~groups,
    color = ~groups,
    line = list(color = ~groups),
    legendgroup = ~groups,
    marker = list(
      symbol = "diamond",
      size = 8,
      line = list(width = 2, color = "black")
    )
  )
  expected_plot <- plotly::add_ribbons(
    expected_plot,
    legendgroup = ~groups,
    ymin = ~mean_lower_confidence,
    ymax = ~mean_upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    hoverinfo = "none"
  )
  expected_plot <- layout(expected_plot,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["mean"]][["label"]],
      dtick = 500,
      range = c(0, max(group_input_table[["mean"]], na.rm = TRUE) + 499)
    )
  )

  result_plot <- result_plotting_object$plot()
  expect_plotly_plots_equal(expected_plot, result_plot, debug = FALSE)
})




test_that("Test confidence interval", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "mean",
  )

  expected_plot <- plotly::plot_ly(
    input_table,
    y = ~mean,
    x = ~year,
    type = "scatter",
    mode = "lines+markers",
    linetype = "solid",
    fillcolor = "rgba(236, 236, 236, 0.5)",
    line = list(color = default_color),
    color = default_color,
    marker = list(
      symbol = "diamond",
      size = 8,
      line = list(width = 2, color = "black")
    ),
    text = "Some Text",
    hovertemplate = "%{text}"
  )

  expected_plot <- layout(expected_plot,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["mean"]][["label"]],
      dtick = 500,
      range = data_range
    )
  )

  expected_plot_ci <- plotly::add_ribbons(
    expected_plot,
    ymin = ~mean_lower_confidence,
    ymax = ~mean_upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    hoverinfo = "none"
  )


  result_plot <- result_plotting_object$plot()

  result_plotting_object$disable_confidence_interval()
  result <- result_plotting_object$plot()
  expect_plotly_plots_equal(expected_plot, result)

  result_plotting_object$enable_confidence_interval()
  result <- result_plotting_object$plot()
  expect_plotly_plots_equal(expected_plot_ci, result)
})


test_that("Year Range", {
  result_plotting_object <- soep.plots::numeric_plot(
    fields = fields,
    data = input_table,
    x_axis = "year",
    y_axis = "mean"
  )
  subset_table <- subset(input_table, year %in% seq(2000, 2002))

  expected_plot <- plotly::plot_ly(
    subset_table,
    y = ~mean,
    x = ~year,
    type = "scatter",
    mode = "lines+markers",
    linetype = "solid",
    fillcolor = "rgba(236, 236, 236, 0.5)",
    line = list(color = default_color),
    color = default_color,
    marker = list(
      symbol = "diamond",
      size = 8,
      line = list(width = 2, color = "black")
    ),
    text = "Some Text",
    hovertemplate = "%{text}"
  )

  expected_plot <- plotly::add_ribbons(
    expected_plot,
    ymin = ~mean_lower_confidence,
    ymax = ~mean_upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    hoverinfo = "none"
  )


  expected_plot <- layout(expected_plot,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
      title = fields[["mean"]][["label"]],
      dtick = 500,
      range = data_range
    )
  )

  result_plotting_object$set_year_range(year_range = c(2000, 2002))
  result_plot <- result_plotting_object$plot()

  expect_plotly_plots_equal(expected_plot, result_plot, debug = FALSE)
})
