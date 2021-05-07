#' @import methods
#' @title Superclass to define plot object initialization
general_plot <- setRefClass(
    "GeneralPlot",
    fields = list(
        fields = "list",
        data = "data.frame",
        confidence_interval = "logical"
    ),
    methods = list(
        initialize = function(..., fields = list(), data = data.frame()) {
            fields <<- fields
            data <<- data
            confidence_interval <<- TRUE
        }
    )
)
