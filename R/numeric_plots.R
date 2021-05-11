#' @include plots.R
library(ggplot2)

#' @export numeric_plot
#' @exportClass NumericPlot
#' @title NumericPlot
#' @description Handle Configuration and output of a numeric variable plot
#' @param x_axis column name from data to be plotted on the x axis
#' @param y_axis column name from data to be plotted on the y axis
#' @param group_by vector of category column names"
numeric_plot <- setRefClass(
    "NumericPlot",
    contains = "GeneralPlot",
    methods = list(
        plot = function(...) {
            "Create a numerical plot from data and settings."
            if (length(.self$group_by) == 0) {
                group <- ""
                plot <- ggplot(
                    .self$data,
                    aes(x = !!sym(.self$x_axis), y = !!sym(.self$y_axis), group = "")
                )
            } else {
                plot <- ggplot(
                    .self$data,
                    aes(
                        x = !!sym(.self$x_axis),
                        y = !!sym(.self$y_axis),
                        group = !!sym(.self$group_by[1]),
                        color = !!sym(.self$group_by[1])
                    )
                )
            }
            plot <- plot +
                geom_line() +
                expand_limits(y = 0) +
                scale_x_discrete(breaks = .self$data[[.self$x_axis]]) +
                scale_y_continuous(
                    breaks = seq(0, max(.self$data[[.self$y_axis]]), by = 500)
                ) +
                theme(
                    axis.text = element_text(size = 12),
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
