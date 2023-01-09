#' Saves plots to image files and compares their file hashes.
#'
#' @param expected The expected ggplot object to compare result to.
#' @param result The ggplot result of a test.
#' @return The md5 hash of the result file.
#' @examples
#' expected <- ggplot(dataframe)
#' result <- test_function() # Returns a ggplot object.
#' expected_plots_equal(expected, result)
expect_plotly_plots_equal <- function(expected, result, debug = FALSE) {
    test_id <- sample(1:10000, 1)
    expected_file <- paste0(test_id, "baseline.png")
    compare_file <- paste0(test_id, "compare.png")
    withr::local_file(expected_file)
    withr::local_file(compare_file)
    plotly::save_image(expected, file = expected_file)
    plotly::save_image(result, file = compare_file)
    files_match <- (
        tools::md5sum(files = expected_file) == tools::md5sum(files = compare_file)
    )
    if (debug) {
        print(expected)
        print(result)
    }
    failed_test_files_path <- file.path(
        tools::file_path_as_absolute("../"),
        "failed_test_files"
    )
    if (!files_match) {
        dir.create(failed_test_files_path, showWarnings = FALSE)
        file.copy(c(expected_file, compare_file), failed_test_files_path)
    }
    testthat::expect(
        files_match,
        sprintf(
            paste0(
                "Result Plot does not look like expected plot. Plots are at ",
                failed_test_files_path,
                " with id ",
                test_id
            )
        )
    )
    return(tools::md5sum(files = compare_file))
}
