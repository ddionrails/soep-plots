#' @include plots.R
library(ggplot2)

#' @exportClass NumericPlot
#' @title NumericPlot
#' @description Handle Configuration and output of a numeric variable plot
numeric_plot <- setRefClass(
    "NumericPlot",
    contains = "GeneralPlot",
    methods = list(
        plot = function(...,
                        x_axis = "year",
                        y_axis = "number",
                        group_by = c()) {
            "Put together a numerical plot

            @description
            Create a line plot from data.
            @param x column name from data to be plotted on the x axis
            @param y column name from data to be plotted on the y axis
            @param group vector of category column names"
            if (length(group_by) == 0) {
                group <- ""
                plot <- ggplot(
                    .self$data,
                    aes(x = !!sym(x_axis), y = !!sym(y_axis), group = "")
                )
            } else {
                plot <- ggplot(
                    .self$data,
                    aes(
                        x = !!sym(x_axis),
                        y = !!sym(y_axis),
                        group = !!sym(group_by[1]),
                        color = !!sym(group_by[1])
                    )
                )
            }
            plot <- plot +
                geom_line() +
                expand_limits(y = 0) +
                scale_x_discrete(breaks = .self$data[[x_axis]]) +
                scale_y_continuous(
                    breaks = seq(0, max(.self$data[[y_axis]]), by = 500)
                ) +
                theme(
                    axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14, face = "bold"),
                    legend.text = element_text(size = 12),
                    legend.title = element_blank()
                ) +
                ylab(.self$fields[[y_axis]][["label"]]) +
                xlab(.self$fields[[x_axis]][["label"]]) +
                geom_ribbon(
                    aes_string(
                        ymin = "lower_confidence",
                        ymax = "upper_confidence"
                    ),
                    linetype = 2, alpha = .1
                )


            return(plot)
        }
    )
)
