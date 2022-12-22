#' @import plotly
#' @include plots.R

#' Helper function to set y scale depending on values to display
#' @param column dataframe column with y scale values
#' @noRd
y_scale_breaks <- function(column, limits = vector()) {
    column <- column[is.numeric(column)]
    if (length(limits) == 2) {
        minimum <- limits[1]
        maximum <- limits[2]
    } else {
        maximum <- max(column, na.rm = TRUE)
        minimum <- 0
    }
    interval <- 500
    if (maximum >= 1000) {
        maximum <- maximum + 499
    }
    if (maximum < 1000) {
        interval <- 50
        maximum <- maximum + 49
    }
    if (maximum < 100) {
        interval <- 5
        maximum <- maximum + 4
    }
    if (maximum <= 10) {
        interval <- 1
    }
    return(c(
        minimum,
        maximum,
        interval
    ))
}

tooltip_template <- paste0(
    c(
        "Jahr: %{x}",
        "Durchschnitt: %{y:1.2f}",
        "N: %s",
        "Obere Konfidenz: %1.2f",
        "Untere Konfidenz: %1.2f"
    ),
    collapse = "<br/>"
)
tooltip_template_with_grouping <- paste("%s", tooltip_template, sep = "<br/>")

get_xaxis_layout <- function(title) {
    return(list(
        title = title,
        showline = TRUE,
        showgrid = FALSE,
        showticklabels = TRUE,
        linecolor = "rgb(204, 204, 204)",
        linewidth = 2,
        autotick = FALSE,
        ticks = "outside",
        tickcolor = "rgb(204, 204, 204)",
        tickwidth = 2,
        ticklen = 5,
        tickfont = list(
            family = "Arial",
            size = 12,
            color = "rgb(82, 82, 82)"
        )
    ))
}

line_tooltip <- paste(
    c(
        "~",
        "paste(",
        paste(
            "'N: '", "n", "'<br>'",
            "'Obere Konfidenz: '",
            "upper_confidence_mean",
            "'<br>'",
            "'Untere Konfidenz: '",
            "lower_confidence_mean",
            "'<br>'",
            sep = ","
        ),
        ", sep='')"
    ),
    sep = "",
    collapse = ""
)

boxplot_tooltip_template <- paste0(
    c(
        "Jahr: %s",
        "Erstes Quartil: %1.2f",
        "Median: %1.2f",
        "Drittes Quartil: %1.2f",
        "N: %s",
        "Obere Konfidenz: %1.2f",
        "Untere Konfidenz: %1.2f"
    ),
    collapse = "<br/>"
)
add_group_to_template <- function(template) {
    return(paste("%s", template, sep = "<br/>"))
}

#' @export numeric_plot
#' @exportClass NumericPlot
#' @title NumericPlot
#' @description Handle Configuration and output of a numeric variable plot
#' @param fields metadata for the data columns
#' @param data data.frame for the plot data
#' @param x_axis column name from data to be plotted on the x axis
#' @param y_axis column name from data to be plotted on the y axis
#' @param dimension_metadata vector of category column names
#' @param type determies plot type; either 'line' or 'bar'
numeric_plot <- setRefClass(
    "NumericPlot",
    contains = "GeneralPlot",
    methods = list(
        get_y_scale_breaks = function(column) {
            if (length(.self$y_scale_limits) == 2) {
                return(y_scale_breaks(column, limits = .self$y_scale_limits))
            }
            return(y_scale_breaks(column))
        },
        set_to_boxplot = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "boxplot"
        },
        set_to_line = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "line"
        },
        set_y_scale_limits = function(..., y_scale_limits) {
            "Set y scale upper and lower limit."
            if (length(year_range) == 2) {
                y_scale_limits <<- sort(as.numeric(y_scale_limits))
            }
        },
        initialize_grouped_plot = function(plot_data) {
            plot <- plotly::plot_ly(
                plot_data,
                x = as.formula(paste0("~", .self$x_axis)),
                y = as.formula(paste0("~", .self$y_axis)),
                type = "scatter",
                mode = "lines+markers",
                linetype = ~merged_group_name,
                color = ~merged_group_name,
                marker = list(
                    symbol = "diamond",
                    size = 8,
                    line = list(width = 2, color = "black")
                ),
                text = as.formula(line_tooltip),
                hovertemplate = paste(
                    "<b>%{data.name}</b>",
                    "Year: %{x}",
                    paste(.self$fields[[.self$y_axis]][["label"]], ": %{y}", sep = ""),
                    "%{text}",
                    sep = "<br>"
                )
            )
            return(plot)
        },
        initialize_grouped_boxplot = function(plot_data) {
            plot <- plotly::plot_ly(
                data = plot_data,
                x = as.formula(paste0("~", .self$x_axis)),
                y = as.formula(paste0("~", .self$y_axis)),
                color = ~merged_group_name,
                type = "box",
                q1 = ~percentile_25,
                median = ~median,
                q3 = ~percentile_75,
                lowerfence = ~percentile_10,
                upperfence = ~percentile_90,
            )
            return(plot)
        },
        initialize_ungrouped_boxplot = function(plot_data) {
            plot <- plotly::plot_ly(
                data = plot_data,
                x = as.formula(paste0("~", .self$x_axis)),
                y = as.formula(paste0("~", .self$y_axis)),
                type = "box",
                q1 = ~percentile_25,
                median = ~median,
                q3 = ~percentile_75,
                lowerfence = ~percentile_10,
                upperfence = ~percentile_90,
            )
            return(plot)
        },
        initialize_ungrouped_plot = function(plot_data) {
            if (.self$confidence_interval & .self$type == "line") {
                plot <- plotly::plot_ly(
                    plot_data,
                    y = as.formula(paste0("~upper_confidence_", .self$y_axis)),
                    x = as.formula(paste0("~", .self$x_axis)),
                    type = "scatter",
                    mode = "lines",
                    name = "Obere Konfidenz",
                    line = list(color = "transparent"),
                    showlegend = FALSE,
                    hoverinfo = "none"
                )
                plot <- plotly::add_trace(
                    plot,
                    y = as.formula(paste0("~lower_confidence_", .self$y_axis)),
                    type = "scatter",
                    mode = "lines",
                    fill = "tonexty",
                    fillcolor = "rgba(238, 238, 238, 1)",
                    line = list(color = "transparent"),
                    name = "Untere Konfidenz",
                    showlegend = FALSE,
                    hoverinfo = "none"
                )
                plot <- plotly::add_trace(
                    plot,
                    y = as.formula(paste0("~", .self$y_axis)),
                    title = .self$fields[[.self$y_axis]][["label"]],
                    type = "scatter",
                    mode = "lines+markers",
                    line = list(color = "rgb(252, 141, 98)"),
                    color = "rgb(252, 141, 98)",
                    marker = list(
                        symbol = "diamond",
                        size = 8,
                        line = list(width = 2, color = "black")
                    ),
                    text = as.formula(line_tooltip),
                    hovertemplate = paste(
                        "Year: %{x}",
                        paste(
                            .self$fields[[.self$y_axis]][["label"]],
                            ": %{y}",
                            sep = ""
                        ),
                        "%{text}<extra></extra>",
                        sep = "<br>"
                    )
                )
            } else {
                plot <- plotly::plot_ly()(
                    plot_data,
                    x = as.formula(paste0("~", .self$x_axis)),
                    y = as.formula(paste0("~", .self$y_axis)),
                    type = "scatter",
                    mode = "lines+markers",
                    linetype = "solid",
                    color = "rgba(40,67,135, 1)",
                    marker = list(
                        symbol = "diamond",
                        size = 8,
                        line = list(width = 2, color = "black")
                    ),
                    text = as.formula(line_tooltip),
                    hovertemplate = paste(
                        "Year: %{x}",
                        paste(.self$fields[[.self$y_axis]][["label"]], ": %{y}", sep = ""),
                        "%{text}",
                        sep = "<br>"
                    )
                )
            }


            return(plot)
        },
        plot = function(...) {
            "Create a numerical plot from data and settings."
            plot_data <- .self$get_data()
            if ("merged_group_name" %in% names(plot_data)) {
                if (.self$type == "line") {
                    plot <- .self$initialize_grouped_plot(plot_data)
                }
                if (.self$type == "boxplot") {
                    plot <- .self$initialize_grouped_boxplot(plot_data)
                }
            } else {
                if (.self$type == "line") {
                    plot <- .self$initialize_ungrouped_plot(plot_data)
                }
                if (.self$type == "boxplot") {
                    plot <- .self$initialize_ungrouped_boxplot(plot_data)
                }
            }


            scale_breaks <- .self$get_y_scale_breaks(
                column = plot_data[[.self$y_axis]]
            )
            if (length(.self$y_scale_limits) == 2) {
                plot <- layout(plot,
                    # title = "Title",
                    xaxis = get_xaxis_layout(.self$fields[[.self$x_axis]][["label"]]),
                    yaxis = list(
                        title = .self$fields[[.self$y_axis]][["label"]],
                        dtick = scale_breaks[3],
                        range = .self$y_scale_limits
                    )
                )
            } else {
                plot <- layout(plot,
                    # title = "Title",
                    xaxis = get_xaxis_layout(.self$fields[[.self$x_axis]][["label"]]),
                    yaxis = list(
                        title = .self$fields[[.self$y_axis]][["label"]],
                        dtick = scale_breaks[3],
                        range = scale_breaks[1:2]
                    )
                )
            }



            return(plot)
        }
    )
)
