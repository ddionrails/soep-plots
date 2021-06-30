#' @import methods
#' @title Superclass to define plot object initialization
#' @description Handle Configuration and output of a categorical variable plot
#' @param fields metadata for the data columns
#' @param data data.frame for the plot data
#' @param x_axis column name from data to be plotted on the x axis
#' @param y_axis column name from data to be plotted on the y axis
#' @param dimension_metadata vector of category column names
#' @param type determies plot type; either 'line' or 'bar'
general_plot <- setRefClass(
    "GeneralPlot",
    fields = list(
        fields = "list",
        data = "data.frame",
        x_axis = "character",
        y_axis = "character",
        dimension_metadata = "list",
        confidence_interval = "logical",
        type = "character",
        year_range = "vector",
        year_selection = "vector",
        row_selector = "logical"
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
                                  dimension_metadata = .self$dimension_metadata) {
            .self$x_axis <- x_axis
            .self$y_axis <- y_axis
            .self$dimension_metadata <- dimension_metadata
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
        prepare_dimensions = function(..., groups) {
            if (!is.null(groups)) {
                .self$data$merged_group_name <- do.call(paste, data[groups])
            }
            .self$data <- .self$data[complete.cases(.self$data), ]
        },
        initialize = function(...,
                              fields,
                              data,
                              x_axis,
                              y_axis,
                              dimension_metadata,
                              group_axis = NULL) {
            fields <<- fields
            data <<- data
            prepare_dimensions(
                groups = group_axis,
                dimension_metadata = dimension_metadata,
            )
            confidence_interval <<- TRUE
            type <<- "line"
            x_axis <<- x_axis
            y_axis <<- y_axis
            if (is.factor(data$year)) {
                year_range <<- range(levels(data[["year"]]))
            }
            else {
                year_range <<- range(as.integer(data[["year"]]))
                data$year <<- as.factor(data$year)
            }
            year_selection <<- year_range
        }
    )
)
