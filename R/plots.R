#' @import methods
#' @title Superclass to define plot object initialization
#' @description Handle Configuration and output of a categorical variable plot
#' @param x_axis column name from data to be plotted on the x axis
#' @param y_axis column name from data to be plotted on the y axis
#' @param group_by vector of category column names
#' @param type determies plot type; either 'line' or 'bar'
general_plot <- setRefClass(
    "GeneralPlot",
    fields = list(
        fields = "list",
        data = "data.frame",
        x_axis = "character",
        y_axis = "character",
        group_by = "vector",
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
        set_dimensions = function(...,
                                  x_axis = .self$x_axis,
                                  y_axis = .self$y_axis,
                                  group_by = .self$group_by) {
            .self$x_axis <- x_axis
            .self$y_axis <- y_axis
            .self$group_by <- group_by
        },
        initialize = function(...,
                              fields,
                              data,
                              x_axis,
                              y_axis,
                              group_by) {
            fields <<- fields
            data <<- data
            confidence_interval <<- TRUE
            type <<- "line"
            x_axis <<- x_axis
            y_axis <<- y_axis
            group_by <<- group_by
        }
    )
)
