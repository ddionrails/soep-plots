library(testthat)
library(plotly)

library(soep.plots)

source("helpers.R")

options(warn = -1)

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

# Set up
fields <- list(
    "year" = list("label" = "Survey Year"),
    "proportion" = list("label" = "Proportion")
)
year <- as.numeric(
    c("2000", "2000", "2001", "2001", "2002", "2002", "2003", "2003")
)

dimension <- c("f", "f", "f", "f", "f", "f", "f", "f")
category <- c("a", "b", "a", "b", "a", "b", "a", "b")
merged_group_name <- category
proportion <- c(.1, .9, .6, .4, .1, .9, .6, .4)
n <- c(1, 9, 6, 4, 1, 9, 6, 4)
lower_confidence <- c(.09, .88, .59, .37, .09, .85, .54, .31)
upper_confidence <- c(.11, .92, .63, .42, .11, .92, .61, .44)
input_table <- data.frame(
    year,
    category,
    dimension,
    proportion,
    n,
    lower_confidence,
    upper_confidence,
    merged_group_name
)

plot_theme <- theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle = -50),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank()
)

expected_plot_line <- plotly::plot_ly(input_table,
    x = ~year,
    y = ~proportion,
    type = "scatter",
    mode = "lines+markers",
    linetype = ~merged_group_name,
    legendgroup = ~merged_group_name,
    color = ~merged_group_name,
    marker = list(
        symbol = "diamond",
        size = 8,
        line = list(width = 2, color = "black")
    )
)
expected_plot_line <- plotly::add_ribbons(
    expected_plot_line,
    legendgroup = ~merged_group_name,
    ymin = ~lower_confidence,
    ymax = ~upper_confidence,
    line = list(color = "transparent"),
    marker = list(color = "transparent", line = list(width = 0)),
    showlegend = FALSE,
    hoverinfo = "none"
)

expected_plot_line <- plotly::layout(expected_plot_line,
    xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
    yaxis = list(
        title = fields[["proportion"]][["label"]],
        tickformat = ",.0%", range(0, 100)
    )
)


expected_plot_bar <- function() {
    plot <- plotly::plot_ly(
        input_table,
        x = ~year,
        y = ~proportion,
        type = "bar",
        legendgroup = ~merged_group_name,
        color = ~merged_group_name
    )
    plot <- plotly::layout(plot, barmode = "stack")

    plot <- plotly::layout(plot,
        xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
        yaxis = list(
            title = fields[["proportion"]][["label"]],
            tickformat = ",.0%", range(0, 100)
        )
    )
}


test_that("CategoricalPlot Object initialization", {
    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "year",
        y_axis = "proportion",
        group_axis = c("category"),
        dimension_metadata = list()
    )
    category_input_table <- input_table
    category_input_table$merged_group_name <- category
    expect_true(inherits(result_plotting_object, "CategoricalPlot"))
    expect_type(result_plotting_object$fields, "list")
    expect_identical(fields, result_plotting_object$fields)
    expect_true(is.data.frame(result_plotting_object$data))
    expect_identical(category_input_table, result_plotting_object$data)
})


test_that("CategoricalPlot plotting.", {
    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "year",
        y_axis = "proportion",
        group_axis = c("category"),
        dimension_metadata = list("dimension" = "f")
    )

    result_plot <- result_plotting_object$plot()

    expect_plotly_plots_equal(expected_plot_line, result_plot, debug = FALSE)
})

### Content hash comparison does not work here
# test_that("Plot type switching", {
#    result_plotting_object <- soep.plots::categorical_plot(
#        fields = fields,
#        data = input_table,
#        x_axis = "year",
#        y_axis = "proportion",
#        group_axis = c("category"),
#        dimension_metadata = list("dimension" = "f"),
#    )
#
#    result_plotting_object$set_to_bar()
#
#    result_plot_bar <- result_plotting_object$plot()
#    expect_plotly_plots_equal(expected_plot_bar(), result_plot_bar)
#
#    result_plotting_object$set_to_line()
#    result_plot_line <- result_plotting_object$plot()
#
#    expect_plotly_plots_equal(expected_plot_line, result_plot_line)
# })
#
# TODO: Test plot is faulty
#
# test_that("Year Range", {
#    result_plotting_object <- soep.plots::categorical_plot(
#        fields = fields,
#        data = input_table,
#        x_axis = "year",
#        y_axis = "proportion",
#        group_axis = c("category"),
#        dimension_metadata = list("dimension" = "f")
#    )
#    subset_table <- subset(input_table, year %in% seq(2000, 2002))
#
#
#    expected_plot <- plotly::plot_ly(subset_table,
#        x = ~year,
#        y = ~proportion,
#        type = "scatter",
#        mode = "lines+markers",
#        linetype = ~merged_group_name,
#        legendgroup = ~merged_group_name,
#        color = ~merged_group_name,
#        marker = list(
#            symbol = "diamond",
#            size = 8,
#            line = list(width = 2, color = "black")
#        )
#    )
#    expected_plot <- plotly::add_ribbons(
#        expected_plot,
#        legendgroup = ~merged_group_name,
#        ymin = ~lower_confidence,
#        ymax = ~upper_confidence,
#        line = list(color = "transparent"),
#        marker = list(color = "transparent", line = list(width = 0)),
#        showlegend = FALSE,
#        hoverinfo = "none"
#    )
#
#    expected_plot <- plotly::layout(expected_plot,
#        xaxis = fields[["proportion"]][["label"]],
#        yaxis = list(
#            title = fields[["year"]][["label"]],
#            tickformat = ",.0%", range(0, 100)
#        )
#    )
#
#
#
#    result_plotting_object$set_year_range(year_range = c(2000, 2002))
#    result_plot <- result_plotting_object$plot()
#    expect_plotly_plots_equal(expected_plot, result_plot)
# })

test_that("Test dimension_metadata", {
    fields <- list(
        "year" = list("label" = "Survey Year"),
        "proportion" = list("label" = "Proportion")
    )
    year <- as.numeric(
        c(
            "2000",
            "2000",
            "2000",
            "2000",
            "2001",
            "2001",
            "2001",
            "2001",
            "2002",
            "2002",
            "2002",
            "2002"
        )
    )
    dimension <- c("f", "f", "m", "m", "f", "f", "m", "m", "f", "f", "m", "m")
    category <- c("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")
    proportion <- c(.1, .9, .6, .4, .9, .1, .6, .4, .1, .9, .6, .4)
    n <- c(1, 9, 6, 4, 9, 1, 6, 4, 1, 9, 6, 4)
    lower_confidence <- c(
        .09, .88, .59, .37, .85, .09, .54, .31, .09, .88, .59, .37
    )
    upper_confidence <- c(
        .11, .92, .63, .42, .92, .11, .61, .44, .11, .92, .63, .42
    )
    input_table <- data.frame(
        year,
        category,
        dimension,
        proportion,
        n,
        lower_confidence,
        upper_confidence
    )
    expected_table <- input_table[input_table$dimension == "f", ]

    expected_plot <- plotly::plot_ly(expected_table,
        x = ~year,
        y = ~proportion,
        type = "scatter",
        mode = "lines+markers",
        linetype = ~category,
        legendgroup = ~category,
        color = ~category,
        marker = list(
            symbol = "diamond",
            size = 8,
            line = list(width = 2, color = "black")
        )
    )
    expected_plot <- plotly::add_ribbons(
        expected_plot,
        legendgroup = ~category,
        ymin = ~lower_confidence,
        ymax = ~upper_confidence,
        line = list(color = "transparent"),
        marker = list(color = "transparent", line = list(width = 0)),
        showlegend = FALSE,
        hoverinfo = "none"
    )

    expected_plot <- plotly::layout(expected_plot,
        xaxis = get_xaxis_layout(fields[["year"]][["label"]]),
        yaxis = list(
            title = fields[["proportion"]][["label"]],
            tickformat = ",.0%", range(0, 100)
        )
    )


    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "year",
        y_axis = "proportion",
        group_axis = c("category"),
        dimension_metadata = list("dimension" = "f")
    )
    result_plot <- result_plotting_object$plot()
    expect_plotly_plots_equal(expected_plot, result_plot)
})
