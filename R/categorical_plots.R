#' @include plots.R

library(ggplot2)

#' @export  categorical_plot
#' @exportClass CategoricalPlot
#' @title CategoricalPlot
#' @description Handle Configuration and output of a categorical variable plot
#' @param fields metadata for the data columns
#' @param data data.frame for the plot data
#' @param x_axis column name from data to be plotted on the x axis
#' @param y_axis column name from data to be plotted on the y axis
#' @param dimension_metadata list of selected category column names as keys and
#'                           selected row values as list values.
#' @param group_axis vector with column names used as group argument in ggplot
#' @param type determies plot type; either 'line' or 'bar'
categorical_plot <- setRefClass(
    "CategoricalPlot",
    contains = "GeneralPlot",
    methods = list(
        set_to_bar = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "bar"
        },
        set_to_line = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "line"
        },
        get_data = function(...) {
            if (length(.self$row_selector) == 0) {
                output <- .self$data
            } else {
                output <- .self$data[.self$row_selector, ]
            }
            output <- subset(
                output,
                year %in% seq(year_selection[1], year_selection[2])
            )
            return(output)
        },
        prepare_dimensions = function(..., dimension_metadata, groups) {
            .self$data$merged_group_name <- do.call(paste, .self$data[groups])
            if (length(dimension_metadata) == 0) {
                .self$row_selector <- logical()
                return()
            }
            row_selectors <- lapply(
                names(dimension_metadata),
                FUN = function(x, grouping, data) {
                    data[, x] == grouping[x]
                },
                grouping = dimension_metadata,
                data = .self$data
            )
            .self$row_selector <- Reduce("&", row_selectors)
        },
        plot = function(...) {
            "Prepare ggplot output from data and config."
            plot_data <- .self$get_data()
            if (.self$type == "line") {
                output_plot <- ggplot(
                    plot_data,
                    aes(
                        x = !!sym(.self$x_axis),
                        y = !!sym(.self$y_axis),
                        group = merged_group_name,
                        color = merged_group_name,
                        text = sprintf(
                            paste0(
                                c(
                                    "%s",
                                    "Jahr: %s",
                                    "Anteil: %1.2f%%",
                                    "N: %s",
                                    "Obere Konfidenz: %1.2f%%",
                                    "Untere Konfidenz: %1.2f%%"
                                ),
                                collapse = "<br>"
                            ),
                            merged_group_name,
                            !!sym(.self$x_axis),
                            !!sym(.self$y_axis) * 100,
                            n,
                            upper_confidence * 100,
                            lower_confidence * 100
                        )
                    )
                ) +
                    geom_path()
            } else if (.self$type == "bar") {
                output_plot <- ggplot(
                    plot_data,
                    aes(
                        x = !!sym(.self$x_axis),
                        y = !!sym(.self$y_axis),
                        fill = merged_group_name,
                        text = sprintf(
                            paste0(
                                c(
                                    "%s",
                                    "Jahr: %s",
                                    "Anteil: %1.2f%%",
                                    "N: %s",
                                    "Obere Konfidenz: %1.2f%%",
                                    "Untere Konfidenz: %1.2f%%"
                                ),
                                collapse = "<br>"
                            ),
                            merged_group_name,
                            !!sym(.self$x_axis),
                            !!sym(.self$y_axis) * 100,
                            n,
                            upper_confidence * 100,
                            lower_confidence * 100
                        )
                    )
                ) +
                    geom_bar(position = "fill", stat = "identity")
            }

            output_plot <- output_plot +
                ylab(.self$fields[[.self$y_axis]][["label"]]) +
                xlab(.self$fields[[.self$x_axis]][["label"]]) +
                scale_x_discrete(
                    breaks = seq(
                        .self$year_selection[1],
                        .self$year_selection[2],
                        by = 1
                    )
                ) +
                scale_y_continuous(
                    breaks = seq(0, 1, by = .1),
                    labels = sapply(
                        c(seq(0, 100, 10)),
                        function(x) paste(x, "%", sep = "")
                    )
                ) +
                theme(
                    axis.text = element_text(size = 12),
                    axis.text.x = element_text(size = 11, angle = -50),
                    axis.title = element_text(size = 14, face = "bold"),
                    legend.text = element_text(size = 12),
                    legend.title = element_blank()
                ) +
                labs(fill = "")
            if (.self$confidence_interval) {
                output_plot <- output_plot +
                    geom_ribbon(
                        aes(ymin = lower_confidence, ymax = upper_confidence),
                        linetype = 2,
                        alpha = .1
                    )
            }
            return(output_plot)
        }
    )
)
