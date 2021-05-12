library(ggplot2)
library(testthat)

library(soep.plots)

source("helpers.R")

# Set up
fields <- list(
    "years" = list("label" = "Survey Year"),
    "proportion" = list("label" = "Proportion"),
    "category" = list("label" = "A Category")
)
years <- as.factor(
    c("2000", "2000", "2001", "2001", "2002", "2002", "2003", "2003")
)
category <- c("a", "b", "a", "b", "a", "b", "a", "b")
proportion <- c(.1, .9, .6, .4, .1, .9, .6, .4)
lower_confidence <- c(.09, .88, .59, .37, .09, .85, .54, .31)
upper_confidence <- c(.11, .92, .63, .42, .11, .92, .61, .44)
input_table <- data.frame(
    years,
    category,
    proportion,
    lower_confidence,
    upper_confidence
)

expected_plot_line <- ggplot(
    input_table, aes(
        group = category, y = proportion, x = years, color = category
    )
) +
    geom_line() +
    ylab("Proportion") +
    xlab("Survey Year") +
    scale_x_discrete(breaks = unique(input_table$year)) +
    scale_y_continuous(
        breaks = seq(0, 1, by = .1),
        labels = sapply(
            c(seq(0, 100, 10)),
            function(x) paste(x, "%", sep = "")
        )
    ) +
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12)
    ) +
    labs(fill = "") +
    geom_ribbon(
        aes(ymin = lower_confidence, ymax = upper_confidence),
        linetype = 2,
        alpha = .1
    )


expected_plot_bar <- ggplot(
    input_table, aes(
        y = proportion, x = years, fill = category
    )
) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion") +
    xlab("Survey Year") +
    scale_x_discrete(breaks = unique(input_table$year)) +
    scale_y_continuous(
        breaks = seq(0, 1, by = .1),
        labels = sapply(
            c(seq(0, 100, 10)),
            function(x) paste(x, "%", sep = "")
        )
    ) +
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12)
    ) +
    labs(fill = "")


test_that("CategoricalPlot Object initialization", {
    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "years",
        y_axis = "proportion",
        group_by = c("category")
    )
    expect_true(inherits(result_plotting_object, "CategoricalPlot"))
    expect_type(result_plotting_object$fields, "list")
    expect_identical(fields, result_plotting_object$fields)
    expect_true(is.data.frame(result_plotting_object$data))
    expect_identical(input_table, result_plotting_object$data)
})


test_that("CategoricalPlot plotting.", {
    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "years",
        y_axis = "proportion",
        group_by = c("category")
    )

    result_plot <- result_plotting_object$plot()


    expect_plots_equal(expected_plot_line, result_plot)
})

test_that("Plot type switching", {
    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "years",
        y_axis = "proportion",
        group_by = c("category")
    )

    result_plotting_object$set_to_bar()

    result_plot_bar <- result_plotting_object$plot()
    expect_plots_equal(expected_plot_bar, result_plot_bar)

    result_plotting_object$set_to_line()
    result_plot_line <- result_plotting_object$plot()

    expect_plots_equal(expected_plot_line, result_plot_line)
})
