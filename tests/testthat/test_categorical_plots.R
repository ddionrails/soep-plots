library(ggplot2)
library(testthat)

library(soep.plots)

# Set up
fields <- list(
    "years" = list("label" = "Years"),
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
        tools::md5sum(files = expected_file) ==
            tools::md5sum(files = compare_file),
        sprintf("Result Plot does not look like expected plot.")
    )
    return(tools::md5sum(files = compare_file))
}

# Tests
test_that("CategoricalPlot Object initialization", {
    categorical_plot <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table
    )
    expect_true(inherits(categorical_plot, "CategoricalPlot"))
    expect_type(categorical_plot$fields, "list")
    expect_identical(fields, categorical_plot$fields)
    expect_true(is.data.frame(categorical_plot$data))
    expect_identical(input_table, categorical_plot$data)
})


test_that("CategoricalPlot plotting.", {
    categorical_plot <- soep.plots::categorical_plot(
        fields = fields,
        data = input_table
    )

    result <- categorical_plot$plot(
        x_axis = "years",
        y_axis = "proportion",
        group_by = "category",
        type = "line"
    )


    fields_ <- list(
        "years" = list("label" = "Survey Year"),
        "proportion" = list("label" = "Proportion")
    )

    plot <- ggplot(
        input_table, aes(
            group = category, y = proportion, x = years, color = category
        )
    ) +
        geom_line() +
        ylab("Proportion") +
        xlab("Years") +
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
    expect_plots_equal(plot, result)
})