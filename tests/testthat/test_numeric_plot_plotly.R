library(ggplot2)
library(testthat)
library(plotly)

library(soep.plots)


# Set up
fields <- list(
        "year" = list("label" = "Survey Year"),
        "mean" = list("label" = "Mean Income"),
        "median" = list("label" = "Median Income")
)
year <- as.integer(c("2000", "2001", "2002", "2003"))
mean <- c(1000, 2000, 3000, 1500)
n <- c(5000, 5400, 4500, 5000)
upper_confidence_mean <- c(1100, 2100, 3200, 1600)
upper_confidence_median <- c(1100, 2100, 3200, 1600)
lower_confidence_mean <- c(900, 1800, 2900, 1000)
lower_confidence_median <- c(900, 1800, 2900, 1000)
percentile_10 <- c(500, 500, 1000, 500)
percentile_25 <- c(700, 1000, 2000, 1000)
percentile_75 <- c(1500, 2500, 4000, 2000)
percentile_90 <- c(2000, 3000, 5000, 3000)
median <- c(1000, 2000, 3000, 1500)
input_table <- data.frame(
        year,
        mean,
        n,
        lower_confidence_mean,
        lower_confidence_median,
        upper_confidence_mean,
        upper_confidence_median,
        percentile_10,
        percentile_25,
        percentile_75,
        percentile_90,
        median
)


xaxis <- list(
        title = "",
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
)
plotly_plot <- plot_ly(
        input_table,
        x = ~year, y = ~upper_confidence_mean, type = "scatter",
        mode = "lines", showlegend = FALSE, name = "Upper Confidence",
        line = list(color = "transparent")
)
plotly_plot <- add_trace(
        plotly_plot,
        y = ~lower_confidence_mean, type = "scatter",
        mode = "lines", fill = "tonexty", fillcolor = "rgba(238, 238, 238, 1)",
        line = list(color = "transparent"), name = "Lower Confidence"
)
plotly_plot <- add_trace(
        plotly_plot,
        x = ~year, y = ~mean, type = "scatter", mode = "lines",
        line = list(color = "rgb(0,0,0)"),
        name = "Average"
)
plotly_plot <- layout(
        plotly_plot,
        yaxis = list(range = list(0, 4000), ticks = "outside"),
        xaxis = xaxis
)
plotly_plot



test_grouping <- function() {
        year <- as.integer(c(
                "2000",
                "2001",
                "2002",
                "2003",
                "2000",
                "2001",
                "2002",
                "2003"
        ))
        mean <- c(1000, 2000, 3000, NA, 1218, 1804, 3136, 1637)
        n <- c(1000, 2000, 3000, NA, 1218, 1804, 3136, 1637)
        groups <- as.factor(c("a", "a", "a", "a", "b", "b", "b", "b"))
        upper_confidence_mean <- c(1000, 2053, 3125, 1575, 1297, 1894, 3136, 1637)
        lower_confidence_mean <- c(894, 1903, 2776, 1400, 1136, 1772, 3122, 1605)
        group_input_table <- data.frame(
                year,
                mean,
                n,
                groups,
                lower_confidence_mean,
                upper_confidence_mean
        )
        fig <- plot_ly(
                data = group_input_table,
                x = as.formula("~year"),
                y = as.formula("~mean"),
                # text = as.formula("~ sprintf('Jahr'n, breakd, groups)"),
                type = "scatter",
                # text = as.formula("~ paste(n, \"<br>\", groups)"),
                text = as.formula(
                        paste(
                                c(
                                        "~",
                                        "paste(",
                                        paste(
                                                "'N: '", "n", "'<br>'",
                                                "'Untere Konfidenz: '",
                                                "lower_confidence_mean",
                                                "'<br>'",
                                                "'Obere Konfidenz: '",
                                                "upper_confidence_mean",
                                                "'<br>'",
                                                sep = ","
                                        ),
                                        ", sep='')"
                                ),
                                sep = "",
                                collapse = ""
                        )
                ),
                mode = "lines+markers",
                linetype = ~groups,
                color = ~groups,
                marker = list(
                        symbol = "diamond",
                        size = 8,
                        line = list(width = 2, color = "black")
                ),
                hovertemplate = paste(
                        "<b>%{data.name}</b><br>",
                        "Year: %{x}<br>",
                        "Mean: %{y}<br>",
                        "N: %{text}",
                        sep = ""
                )
        )
        fig <- layout(fig,
                title = "Title",
                xaxis = list(title = "xaxis Title"),
                yaxis = list(title = "yaxis Title")
        )
        fig
}
test_grouping()
