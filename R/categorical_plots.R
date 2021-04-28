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
        plot = function() {}
    )
)
