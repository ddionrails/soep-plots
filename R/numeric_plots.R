#' @include plots.R
#' @import ggplot2

#' Helper function to set y scale depending on values to display
#' @param column dataframe column with y scale values
#' @noRd
y_scale_breaks <- function(column) {
    column <- column[is.numeric(column)]
    maximum <- max(column, na.rm = TRUE)
    interval <- 500
    if (maximum < 1000) {
        interval <- 50
    }
    if (maximum < 100) {
        interval <- 5
    }
    if (maximum <= 10) {
        interval <- 1
    }
    return(seq(
        0,
        maximum,
        by = interval
    ))
}

#' @export numeric_plot
#' @exportClass NumericPlot
#' @title NumericPlot
#' @description Handle Configuration and output of a numeric variable plot
#' @param fields metadata for the data columns
#' @param data data.frame for the plot data
#' @param x_axis column name from data to be plotted on the x axis
#' @param y_axis column name from data to be plotted on the y axis
#' @param dimension_metadata vector of category column names
#' @param type determies plot type; either 'line' or 'bar'
numeric_plot <- setRefClass(
    "NumericPlot",
    contains = "GeneralPlot",
    methods = list(
        plot = function(...) {
            "Create a numerical plot from data and settings."
            plot_data <- .self$get_data()
            if ("merged_group_name" %in% names(plot_data)) {
                plot <- ggplot(
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
                                    "Durchschnitt: %1.2f",
                                    "N: %s",
                                    "Obere Konfidenz: %1.2f",
                                    "Untere Konfidenz: %1.2f"
                                ),
                                collapse = "<br>"
                            ),
                            merged_group_name,
                            !!sym(.self$x_axis),
                            !!sym(.self$y_axis),
                            n,
                            upper_confidence,
                            lower_confidence
                        )
                    )
                )
            } else {
                group <- ""
                plot <- ggplot(
                    plot_data,
                    aes(
                        x = !!sym(.self$x_axis), y = !!sym(.self$y_axis), group = "",
                        text = sprintf(
                            paste0(
                                c(
                                    "Jahr: %s",
                                    "Durchschnitt: %1.2f",
                                    "N: %s",
                                    "Obere Konfidenz: %1.2f",
                                    "Untere Konfidenz: %1.2f"
                                ),
                                collapse = "<br>"
                            ),
                            !!sym(.self$x_axis),
                            !!sym(.self$y_axis),
                            n,
                            upper_confidence,
                            lower_confidence
                        )
                    )
                )
            }
            plot <- plot +
                geom_path(na.rm = TRUE) +
                geom_point(size = 2, shape = 3) +
                coord_cartesian() +
                expand_limits(y = 0) +
                scale_x_continuous(
                    breaks = seq(
                        .self$year_selection[1],
                        .self$year_selection[2],
                        by = 1
                    )
                ) +
                scale_y_continuous(
                    breaks = y_scale_breaks
                ) +
                theme(
                    axis.text = element_text(size = 12),
                    axis.text.x = element_text(size = 11, angle = -50),
                    axis.title = element_text(size = 14, face = "bold"),
                    legend.text = element_text(size = 12),
                    legend.title = element_blank()
                ) +
                ylab(.self$fields[[.self$y_axis]][["label"]]) +
                xlab(.self$fields[[.self$x_axis]][["label"]])
            if (.self$confidence_interval) {
                plot <- plot +
                    geom_ribbon(
                        aes_string(
                            ymin = "lower_confidence",
                            ymax = "upper_confidence"
                        ),
                        linetype = 2, alpha = .1
                    )
            }


            return(plot)
        }
    )
)
