#' @include plots.R

library(ggplot2)

#' @export  categorical_plot
#' @exportClass CategoricalPlot
#' @title CategoricalPlot
#' @description Handle Configuration and output of a categorical variable plot
categorical_plot <- setRefClass(
    "CategoricalPlot",
    contains = "GeneralPlot",
    methods = list(
        plot = function(..., x_axis, y_axis, group_by, type) {
            "Prepare ggplot output for the plot

            @description
            Create a line or bar plot from objects data and fields metadata.
            @param x_axis column name from data to be plotted on the x axis
            @param y_axis column name from data to be plotted on the y axis
            @param group_by vector of category column names
            @param type determies plot type; either 'line' or 'bar'"
            if (type == "line") {
                type_function <- geom_line
            } else {
                type_function <- geom_bar
            }
            output_plot <- ggplot(
                .self$data,
                aes(
                    x = !!sym(x_axis),
                    y = !!sym(y_axis),
                    group = !!sym(group_by),
                    color = !!sym(group_by)
                )
            ) +
                type_function() +
                ylab(.self$fields[[y_axis]][["label"]]) +
                xlab(.self$fields[[x_axis]][["label"]]) +
                scale_x_discrete(breaks = unique(.self$data[[x_axis]])) +
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
                geom_ribbon(
                    aes(ymin = lower_confidence, ymax = upper_confidence),
                    linetype = 2,
                    alpha = .1
                )
        }
    )
)
