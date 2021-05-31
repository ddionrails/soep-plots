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
#' @param group_by vector of category column names
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
        plot = function(...) {
            "Prepare ggplot output from data and config."
            plot_data <- .self$get_data()
            if (.self$type == "line") {
                output_plot <- ggplot(
                    plot_data,
                    aes(
                        x = !!sym(.self$x_axis),
                        y = !!sym(.self$y_axis),
                        group = generated_group,
                        color = generated_group
                    )
                ) +
                    geom_line()
            } else if (.self$type == "bar") {
                output_plot <- ggplot(
                    plot_data,
                    aes(
                        x = !!sym(.self$x_axis),
                        y = !!sym(.self$y_axis),
                        fill = generated_group
                    )
                ) +
                    geom_bar(position = "fill", stat = "identity")
            }

            output_plot <- output_plot +
                ylab(.self$fields[[.self$y_axis]][["label"]]) +
                xlab(.self$fields[[.self$x_axis]][["label"]]) +
                scale_x_discrete(breaks = unique(plot_data[[.self$x_axis]])) +
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
                    legend.text = element_text(size = 12),
                    legend.title = element_blank()
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
