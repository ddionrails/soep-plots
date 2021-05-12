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
