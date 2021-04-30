library(ggplot2)
library(methods)

categorical_plot <- setRefClass(
    "CategoricalPlot",
    fields = list(fields = "list", data = "data.frame"),
    methods = list(
        initialize = function(..., fields = list(), data = data.frame()) {
            fields <<- fields
            data <<- data
        },
        plot = function(..., x_axis, y_axis, group_by, type) {
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
                    aes(ymin = lower, ymax = upper),
                    linetype = 2,
                    alpha = .1
                )
        }
    )
)
