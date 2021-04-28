library(ggplot2)
library(testthat)

library(unnamed.shiny.project)

# Set up
fields <- list(
    "year" = list("label" = "Years"),
    "category" = list("label" = "A Category")
)
year <- as.factor(
    c("2000", "2000", "2001", "2001", "2002", "2002", "2003", "2003")
)
category <- c("a", "b", "a", "b", "a", "b", "a", "b")
percentage <- c(.1, .9, .6, .4, .1, .9, .6, .4)
lower <- c(.09, .88, .59, .37, .09, .85, .54, .31)
upper <- c(.11, .92, .63, .42, .11, .92, .61, .44)
input_table <- data.frame(year, category, percentage, lower, upper)

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
    categorical_plot <- unnamed.shiny.project::categorical_plot(
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
    categorical_plot <- unnamed.shiny.project::categorical_plot(
        fields = fields,
        data = input_table
    )

    result <- categorical_plot$plot(x = "year", y = "meanincome", group = c())


    fields_ <- list(
        "years" = list("label" = "Survey Year"),
        "meanincome" = list("label" = "Mean Income")
    )

    plot <- ggplot(
        input_table, aes(
            group = category, y = percentage, x = year, color = category
        )
    ) +
        geom_line() +
        ylab("Percentage") +
        xlab("Year") +
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
        geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = .1)
    expect_plots_equal(plot, result)
})

#     # Test variability
#     years <- as.factor(c("2000", "2001", "2002", "2003", "2000", "2001", "2002", "2003"))
#     meanincome <- c(1000, 2000, 3000, 1500, 1218, 1804, 3136, 1637)
#     groups <- as.factor(c("a", "a", "a", "a", "b", "b", "b", "b"))
#     input_table <- data.frame(years, meanincome, groups)

#     categorical_plot <- unnamed.shiny.project::NumericPlot(
#         fields = fields_,
#         data = input_table
#     )

#     plot <- ggplot(
#         input_table,
#         aes(x = years, y = meanincome, group = groups, color = groups)
#     ) +
#         geom_line() +
#         expand_limits(y = 0) +
#         scale_x_discrete(breaks = input_table$years) +
#         scale_y_continuous(breaks = seq(0, max(input_table$meanincome), by = 500)) +
#         theme(
#             axis.text = element_text(size = 12),
#             axis.title = element_text(size = 14, face = "bold"),
#             legend.text = element_text(size = 12),
#             legend.title = element_blank()
#         ) +
#         ylab("Mean Income") +
#         xlab("Survey Year")

#     result <- categorical_plot$plot(
#         x = "years", y = "meanincome", group = c("groups")
#     )

#     expect_plots_equal(plot, result)
# })
