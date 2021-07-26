library(ggplot2)
library(testthat)

library(soep.plots)

source("helpers.R")

# Set up
fields <- list(
    "year" = list("label" = "Survey Year"),
    "proportion" = list("label" = "Proportion")
)
year <- as.factor(
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

expected_plot_line <- ggplot(
    input_table, aes(
        group = category, y = proportion, x = year, color = merged_group_name
    )
) +
    geom_path() +
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
    plot_theme +
    labs(fill = "") +
    geom_ribbon(
        aes(ymin = lower_confidence, ymax = upper_confidence),
        linetype = 2,
        alpha = .1
    )


expected_plot_bar <- ggplot(
    input_table, aes(
        y = proportion, x = year, fill = merged_group_name
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
    plot_theme +
    labs(fill = "")




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


    expect_plots_equal(expected_plot_line, result_plot)
})

test_that("Plot type switching", {
    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "year",
        y_axis = "proportion",
        group_axis = c("category"),
        dimension_metadata = list("dimension" = "f"),
    )

    result_plotting_object$set_to_bar()

    result_plot_bar <- result_plotting_object$plot()
    expect_plots_equal(expected_plot_bar, result_plot_bar)

    result_plotting_object$set_to_line()
    result_plot_line <- result_plotting_object$plot()

    expect_plots_equal(expected_plot_line, result_plot_line)
})


test_that("Year Range", {
    result_plotting_object <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table,
        x_axis = "year",
        y_axis = "proportion",
        group_axis = c("category"),
        dimension_metadata = list("dimension" = "f")
    )
    subset_table <- subset(input_table, year %in% seq(2000, 2002))

    expected_plot <- ggplot(
        subset_table, aes(
            group = category, y = proportion, x = year, color = category
        )
    ) +
        geom_path() +
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
        plot_theme +
        labs(fill = "") +
        geom_ribbon(
            aes(ymin = lower_confidence, ymax = upper_confidence),
            linetype = 2,
            alpha = .1
        )


    result_plotting_object$set_year_range(year_range = c(2000, 2002))
    result_plot <- result_plotting_object$plot()
    expect_plots_equal(expected_plot, result_plot)
})

test_that("Test dimension_metadata", {
    fields <- list(
        "year" = list("label" = "Survey Year"),
        "proportion" = list("label" = "Proportion")
    )
    year <- as.factor(
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

    expected_plot <- ggplot(
        expected_table, aes(
            group = category, y = proportion, x = year, color = category
        )
    ) +
        geom_path() +
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
        plot_theme +
        labs(fill = "") +
        geom_ribbon(
            aes(ymin = lower_confidence, ymax = upper_confidence),
            linetype = 2,
            alpha = .1
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
    expect_plots_equal(expected_plot, result_plot)
})
