library(ggplot2)

NumericPlot <- setRefClass(
    "NumericPlot",
    fields = list(fields = "vector", data = "data.frame"),
    methods = list(
        initialize = function(..., fields = list(), data = data.frame()) {
            fields <<- fields
            data <<- data
        },
        plot = function(..., x = "year", y = "number", group = c()) {
            plot_data <- .self$data
            if (length(group) == 0) {
                group <- ""
                out <- ggplot(
                    plot_data, aes(x = !!sym(x), y = !!sym(y), group = "")
                )
            } else {
                out <- ggplot(
                    plot_data,
                    aes(
                        x = !!sym(x),
                        y = !!sym(y),
                        group = !!sym(group[1]),
                        color = !!sym(group[1])
                    )
                )
            }
            out <- out +
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


            return(out)
        }
    )
)
