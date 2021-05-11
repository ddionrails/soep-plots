#' @import methods
#' @title Superclass to define plot object initialization
general_plot <- setRefClass(
    "GeneralPlot",
    fields = list(
        fields = "list",
        data = "data.frame",
        confidence_interval = "logical",
        type = "character"
    ),
    methods = list(
        disable_confidence_interval = function(...) {
            .self$confidence_interval <- FALSE
        },
        enable_confidence_interval = function(...) {
            .self$confidence_interval <- TRUE
        },
        initialize = function(..., fields = list(), data = data.frame()) {
            fields <<- fields
            data <<- data
            confidence_interval <<- TRUE
            type <<- "line"
        }
    )
)
