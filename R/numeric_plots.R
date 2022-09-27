#' @import ggplot2
#' @include plots.R

#' Helper function to set y scale depending on values to display
#' @param column dataframe column with y scale values
#' @noRd
y_scale_breaks <- function(column, limits = vector()) {
    column <- column[is.numeric(column)]
    if (length(limits) == 2) {
        minimum <- limits[1]
        maximum <- limits[2]
    } else {
        maximum <- max(column, na.rm = TRUE)
        minimum <- 0
    }
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
        minimum,
        maximum,
        by = interval
    ))
}

tooltip_template <- paste0(
    c(
        "Jahr: %s",
        "Durchschnitt: %1.2f",
        "N: %s",
        "Obere Konfidenz: %1.2f",
        "Untere Konfidenz: %1.2f"
    ),
    collapse = "<br/>"
)
tooltip_template_with_grouping <- paste("%s", tooltip_template, sep = "<br/>")

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
        get_y_scale_breaks = function(...) {
            if (length(.self$y_scale_limits) == 2) {
                return(y_scale_breaks(..., limits = .self$y_scale_limits))
            }
            return(y_scale_breaks(...))
        },
        set_y_scale_limits = function(..., y_scale_limits) {
            "Set y scale upper and lower limit."
            if (length(year_range) == 2) {
                y_scale_limits <<- sort(as.numeric(y_scale_limits))
            }
        },
        initialize_grouped_plot = function(plot_data) {
            plot <- ggplot(
                plot_data,
                aes(
                    x = !!sym(.self$x_axis),
                    y = !!sym(.self$y_axis),
                    group = merged_group_name,
                    color = merged_group_name,
                    text = sprintf(
                        tooltip_template_with_grouping,
                        merged_group_name,
                        !!sym(.self$x_axis),
                        !!sym(.self$y_axis),
                        n,
                        upper_confidence,
                        lower_confidence
                    )
                )
            )
            return(plot)
        },
        initialize_ungrouped_plot = function(plot_data) {
            plot <- ggplot(
                plot_data,
                aes(
                    x = !!sym(.self$x_axis), y = !!sym(.self$y_axis), group = "",
                    text = sprintf(
                        tooltip_template,
                        !!sym(.self$x_axis),
                        !!sym(.self$y_axis),
                        n,
                        upper_confidence,
                        lower_confidence
                    )
                )
            )
            return(plot)
        },
        plot = function(...) {
            "Create a numerical plot from data and settings."
            plot_data <- .self$get_data()
            if ("merged_group_name" %in% names(plot_data)) {
                plot <- .self$initialize_grouped_plot(plot_data)
            } else {
                plot <- .self$initialize_ungrouped_plot(plot_data)
            }
            plot <- plot +
                coord_cartesian() +
                expand_limits(y = 0) +
                geom_path(na.rm = TRUE) +
                geom_point(size = 2, shape = 3) +
                scale_x_continuous(
                    breaks = seq(
                        .self$year_selection[1],
                        .self$year_selection[2],
                        by = 1
                    )
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
            if (length(.self$y_scale_limits) == 2) {
                plot <- plot + scale_y_continuous(
                    breaks = .self$get_y_scale_breaks, limits = .self$y_scale_limits
                )
            } else {
                plot <- plot + scale_y_continuous(
                    breaks = .self$get_y_scale_breaks
                )
            }

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
