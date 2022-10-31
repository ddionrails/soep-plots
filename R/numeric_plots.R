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

boxplot_tooltip_template <- paste0(
    c(
        "Jahr: %s",
        "Erstes Quartil: %1.2f",
        "Median: %1.2f",
        "Drittes Quartil: %1.2f",
        "N: %s",
        "Obere Konfidenz: %1.2f",
        "Untere Konfidenz: %1.2f"
    ),
    collapse = "<br/>"
)
add_group_to_template <- function(template) {
    return(paste("%s", template, sep = "<br/>"))
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
        add_default_layers = function(plot) {
            plot <- plot +
                coord_cartesian() +
                expand_limits(y = 0) +
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

            return(plot)
        },
        get_y_scale_breaks = function(...) {
            if (length(.self$y_scale_limits) == 2) {
                return(y_scale_breaks(..., limits = .self$y_scale_limits))
            }
            return(y_scale_breaks(...))
        },
        set_to_boxplot = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "boxplot"
        },
        set_to_line = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "line"
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
                        add_group_to_template(tooltip_template),
                        merged_group_name,
                        !!sym(.self$x_axis),
                        !!sym(.self$y_axis),
                        n,
                        !!sym(paste0("upper_confidence_", .self$y_axis)),
                        !!sym(paste0("lower_confidence_", .self$y_axis))
                    )
                )
            ) +
                geom_path(na.rm = TRUE) +
                geom_point(size = 2, shape = 3)
            return(plot)
        },
        initialize_grouped_boxplot = function(plot_data) {
            plot <- ggplot(
                plot_data,
                aes(
                    x = !!sym(.self$x_axis),
                    y = !!sym(.self$y_axis),
                    group = paste0(merged_group_name, year),
                    ymin = percentile_10,
                    ymax = percentile_90,
                    lower = percentile_25,
                    middle = median,
                    upper = percentile_75,
                    color = merged_group_name,
                    text = sprintf(
                        add_group_to_template(boxplot_tooltip_template),
                        merged_group_name,
                        !!sym(.self$x_axis),
                        percentile_25,
                        median,
                        percentile_75,
                        n,
                        lower_confidence_median,
                        upper_confidence_median
                    )
                )
            ) +
                geom_boxplot(stat = "identity")
            return(plot)
        },
        initialize_ungrouped_boxplot = function(plot_data) {
            plot <- ggplot(
                plot_data,
                aes(
                    x = !!sym(.self$x_axis),
                    y = !!sym(.self$y_axis),
                    min = percentile_10,
                    max = percentile_90,
                    lower = percentile_25,
                    middle = median,
                    upper = percentile_75,
                    group = !!sym(.self$y_axis),
                    text = sprintf(
                        boxplot_tooltip_template,
                        !!sym(.self$x_axis),
                        percentile_25,
                        median,
                        percentile_75,
                        n,
                        lower_confidence_median,
                        upper_confidence_median
                    )
                )
            ) +
                geom_boxplot(stat = "identity")
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
                        !!sym(paste0("upper_confidence_", .self$y_axis)),
                        !!sym(paste0("lower_confidence_", .self$y_axis))
                    )
                )
            ) +
                geom_path(na.rm = TRUE) +
                geom_point(size = 2, shape = 3)
            return(plot)
        },
        plot = function(...) {
            "Create a numerical plot from data and settings."
            plot_data <- .self$get_data()
            if ("merged_group_name" %in% names(plot_data)) {
                if (.self$type == "line") {
                    plot <- .self$initialize_grouped_plot(plot_data)
                }
                if (.self$type == "boxplot") {
                    plot <- .self$initialize_grouped_boxplot(plot_data)
                }
            } else {
                if (.self$type == "line") {
                    plot <- .self$initialize_ungrouped_plot(plot_data)
                }
                if (.self$type == "boxplot") {
                    plot <- .self$initialize_ungrouped_boxplot(plot_data)
                }
            }

            plot <- .self$add_default_layers(plot)

            if (length(.self$y_scale_limits) == 2) {
                plot <- plot + scale_y_continuous(
                    breaks = .self$get_y_scale_breaks, limits = .self$y_scale_limits
                )
            } else {
                plot <- plot + scale_y_continuous(
                    breaks = .self$get_y_scale_breaks
                )
            }

            if (.self$confidence_interval & .self$type == "line") {
                plot <- plot +
                    geom_ribbon(
                        aes_string(
                            ymin = paste0("lower_confidence_", .self$y_axis),
                            ymax = paste0("upper_confidence_", .self$y_axis)
                        ),
                        linetype = 2, alpha = .1
                    )
            }


            return(plot)
        }
    )
)
