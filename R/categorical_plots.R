#' @include plots.R
#' @import ggplot2

tooltip_template <- paste0(
    c(
        "%s",
        "Jahr: %s",
        "Anteil: %1.2f%%",
        "N: %s",
        "Obere Konfidenz: %1.2f%%",
        "Untere Konfidenz: %1.2f%%"
    ),
    collapse = "<br>"
)




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


#' @export  categorical_plot
#' @exportClass CategoricalPlot
#' @title CategoricalPlot
#' @description Handle Configuration and output of a categorical variable plot
#' @param fields metadata for the data columns
#' @param data data.frame for the plot data
#' @param x_axis column name from data to be plotted on the x axis
#' @param y_axis column name from data to be plotted on the y axis
#' @param dimension_metadata list of selected category column names as keys and
#'                           selected row values as list values.
#' @param group_axis vector with column names used as group argument in ggplot
#' @param type determies plot type; either 'line' or 'bar'
categorical_plot <- setRefClass(
    "CategoricalPlot",
    contains = "GeneralPlot",
    methods = list(
        get_y_scale_breaks = function(column) {
            if (length(.self$y_scale_limits) == 2) {
                return(y_scale_breaks(column, limits = .self$y_scale_limits))
            }
            return(y_scale_breaks(column))
        },
        line_tooltip = function() {
            return(paste(
                c(
                    "~",
                    "paste(",
                    paste(
                        "'N: '", "n", "'<br>'",
                        "'Obere Konfidenz: '",
                        "upper_confidence * 100", "'%'",
                        "'<br>'",
                        "'Untere Konfidenz: '",
                        "lower_confidence *100", "'%'",
                        "'<br>'",
                        "'<extra></extra>'",
                        sep = ","
                    ),
                    ", sep='')"
                ),
                sep = "",
                collapse = ""
            ))
        },
        add_default_layers = function(plot) {
            plot <- plot +
                labs(fill = "") +
                scale_x_continuous(
                    breaks = seq(
                        .self$year_selection[1],
                        .self$year_selection[2],
                        by = 1
                    )
                ) +
                scale_y_continuous(
                    breaks = seq(0, 1, by = .1),
                    labels = sapply(
                        c(seq(0, 100, 10)),
                        function(x) paste(x, "%", sep = "")
                    )
                ) +
                theme(
                    axis.text = element_text(size = 12),
                    axis.text.x = element_text(size = 11, angle = -50),
                    axis.title = element_text(size = 14, face = "bold"),
                    legend.text = element_text(size = 12),
                    legend.title = element_blank()
                ) +
                xlab(.self$fields[[.self$x_axis]][["label"]]) +
                ylab(.self$fields[[.self$y_axis]][["label"]])

            return(plot)
        },
        set_to_bar = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "bar"
        },
        set_to_line = function(...) {
            "Set type of plot returned to a barchart"
            .self$type <- "line"
        },
        get_data = function(...) {
            if (length(.self$row_selector) == 0) {
                output <- .self$data
            } else {
                output <- .self$data[.self$row_selector, ]
            }
            output <- subset(
                output,
                year %in% seq(year_selection[1], year_selection[2])
            )
            return(output)
        },
        prepare_dimensions = function(..., dimension_metadata, groups) {
            .self$data$merged_group_name <- do.call(paste, .self$data[groups])
            if (length(dimension_metadata) == 0) {
                .self$row_selector <- logical()
                return()
            }
            row_selectors <- lapply(
                names(dimension_metadata),
                FUN = function(x, grouping, data) {
                    data[, x] == grouping[x]
                },
                grouping = dimension_metadata,
                data = .self$data
            )
            .self$row_selector <- Reduce("&", row_selectors)
        },
        initialize_bar_plot = function(plot_data) {
            plot <- plotly::plot_ly(
                plot_data,
                x = as.formula(paste0("~factor(", .self$x_axis, ")")),
                y = as.formula(paste0("~", .self$y_axis)),
                type = "bar",
                legendgroup = ~merged_group_name,
                color = ~merged_group_name,
                text = as.formula(.self$line_tooltip()),
                hovertemplate = paste(
                    "<b>%{data.name}</b>",
                    "Year: %{x}",
                    paste(
                        .self$fields[[.self$y_axis]][["label"]], ": %{y:.0%}",
                        sep = ""
                    ),
                    "%{text}",
                    sep = "<br>"
                )
            )
            plot <- plotly::layout(plot, barmode = "stack")
            return(plot)
        },
        initialize_line_plot = function(plot_data) {
            plot <- plotly::plot_ly(
                plot_data,
                x = as.formula(paste0("~factor(", .self$x_axis, ")")),
                y = as.formula(paste0("~", .self$y_axis)),
                type = "scatter",
                mode = "lines+markers",
                linetype = ~merged_group_name,
                legendgroup = ~merged_group_name,
                color = ~merged_group_name,
                marker = list(
                    symbol = "diamond",
                    size = 8,
                    line = list(width = 2, color = "black")
                ),
                text = as.formula(.self$line_tooltip()),
                hovertemplate = paste(
                    "<b>%{data.name}</b>",
                    "Year: %{x}",
                    paste(
                        .self$fields[[.self$y_axis]][["label"]], ": %{y:.0%}",
                        sep = ""
                    ),
                    "%{text}",
                    sep = "<br>"
                )
            )
            return(plot)
        },
        plot = function(...) {
            "Prepare ggplot output from data and config."
            plot_data <- .self$get_data()
            if (.self$type == "line") {
                plot <- .self$initialize_line_plot(plot_data)
            } else if (.self$type == "bar") {
                plot <- .self$initialize_bar_plot(plot_data)
            }


            plot <- plotly::layout(plot,
                xaxis = get_xaxis_layout(.self$fields[[.self$x_axis]][["label"]]),
                yaxis = list(
                    title = .self$fields[[.self$y_axis]][["label"]],
                    tickformat = ",.0%", range(0, 100)
                )
            )


            if (.self$confidence_interval && .self$type == "line") {
                plot <- plotly::add_ribbons(
                    plot,
                    legendgroup = ~merged_group_name,
                    ymin = ~lower_confidence,
                    ymax = ~upper_confidence,
                    line = list(color = "transparent"),
                    marker = list(color = "transparent", line = list(width = 0)),
                    showlegend = FALSE,
                    hoverinfo = "none"
                )
            }
            return(plot)
        }
    )
)
