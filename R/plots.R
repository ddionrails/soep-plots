
#' @title Superclass to define plot object initialization
general_plot <- setRefClass(
    "GeneralPlot",
    fields = list(fields = "list", data = "data.frame"),
    methods = list(
        initialize = function(..., fields = list(), data = data.frame()) {
            fields <<- fields
            data <<- data
        }
    )
)
