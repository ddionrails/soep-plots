#' @import methods
#' @title Superclass to define plot object initialization
#' @description Handle Configuration and output of a categorical variable plot
#' @param fields metadata for the data columns
#' @param data data.frame for the plot data
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
        type = "character",
        year_range = "vector",
        year_selection = "vector"
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
        set_year_range = function(..., year_range) {
            if (
                length(year_range) == 2 &&
                    year_range[1] >= .self$year_range[1] &&
                    year_range[2] <= .self$year_range[2]
            ) {
                .self$year_selection <- year_range
            }
        },
        get_data = function(...) {
            if (!all(.self$year_range == .self$year_selection)) {
                output <- subset(
                    .self$data,
                    year %in% seq(year_selection[1], year_selection[2])
                )
                return(output)
            }
            return(.self$data)
        },
        initialize = function(...,
                              fields,
                              data,
                              x_axis,
                              y_axis,
                              group_by) {
            fields <<- fields
            data <<- data
            if (length(group_by) > 0) {
                data$generated_group <<- do.call(paste, data[group_by])
            }
            confidence_interval <<- TRUE
            type <<- "line"
            x_axis <<- x_axis
            y_axis <<- y_axis
            group_by <<- group_by
            year_range <<- range(levels(data[["year"]]))
            year_selection <<- year_range
        }
    )
)
