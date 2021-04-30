#' @include plots.R
library(ggplot2)
library(methods)

#' @export
numeric_plot <- setRefClass(
    "NumericPlot",
    contains = "GeneralPlot",
    methods = list(
        #' Put together a numerical plot
        #'
        #' @param x column name from data to be plotted on the x axis
        #' @param y column name from data to be plotted on the y axis
        #' @param group vector of category column names
        plot = function(..., x = "year", y = "number", group = c()) {
            plot_data <- .self$data
            if (length(group) == 0) {
                group <- ""
                plot <- ggplot(
                    plot_data, aes(x = !!sym(x), y = !!sym(y), group = "")
                )
            } else {
                plot <- ggplot(
                    plot_data,
                    aes(
                        x = !!sym(x),
                        y = !!sym(y),
                        group = !!sym(group[1]),
                        color = !!sym(group[1])
                    )
                )
            }
            plot <- plot +
                geom_line() +
                expand_limits(y = 0) +
                scale_x_discrete(breaks = plot_data[[x]]) +
                scale_y_continuous(
                    breaks = seq(0, max(plot_data[[y]]), by = 500)
                ) +
                theme(
                    axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14, face = "bold"),
                    legend.text = element_text(size = 12),
                    legend.title = element_blank()
                ) +
                ylab(.self$fields[[y]][["label"]]) +
                xlab(.self$fields[[x]][["label"]])


            return(plot)
        }
    )
)
