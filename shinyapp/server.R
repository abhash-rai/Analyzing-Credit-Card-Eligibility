library(shiny)
library(plotly)

server <- function(input, output) {
  df <- read.csv("credit_eligibility_clean.csv")

  output$distPlot <- renderPlotly({
    req(input$feature)
    feature <- input$feature

    if (grepl("count$", feature)) {
      # Bar chart if feature name ends with "count"
      value_counts <- table(df[[feature]])
      plot_ly(x = names(value_counts), y = as.numeric(value_counts), type = "bar") %>%
        layout(title = feature, xaxis = list(title = feature, categoryorder = "array", categoryarray = names(value_counts)),
               yaxis = list(title = "Frequency"),
               paper_bgcolor = "white", plot_bgcolor = "white")
    } else if (is.numeric(df[[feature]])) {
      # Histogram or Box Plot for numeric data
      if (input$plotType == "histogram") {
        # Histogram
        if (feature == "Income" || feature == "Customer.relative.age" || feature == "Employment.relative.length" || feature == "Account.relative.age") {
          plot_ly(x = df[[feature]], type = "histogram", nbinsx = input$bins) %>%
            layout(title = feature, xaxis = list(title = feature), yaxis = list(title = "Frequency"),
                   paper_bgcolor = "white", plot_bgcolor = "white")
        } else {
          plot_ly(x = df[[feature]], type = "histogram", nbinsx = input$bins) %>%
            layout(title = feature, xaxis = list(title = feature), yaxis = list(title = "Frequency"),
                   paper_bgcolor = "white", plot_bgcolor = "white")
        }
      } else {
        # Box Plot
        plot_ly(y = df[[feature]], type = "box") %>%
          layout(title = feature, yaxis = list(title = feature),
                 paper_bgcolor = "white", plot_bgcolor = "white")
      }
    } else {
      # Bar chart or Pie chart for categorical or logical data
      if (input$plotType == "bar") {
        # Bar chart
        value_counts <- table(df[[feature]])
        plot_ly(x = names(value_counts), y = as.numeric(value_counts), type = "bar") %>%
          layout(title = feature, xaxis = list(title = feature, categoryorder = "array", categoryarray = names(value_counts)),
                 yaxis = list(title = "Frequency"),
                 paper_bgcolor = "white", plot_bgcolor = "white")
      } else {
        # Pie chart using plotly
        value_count_df <- data.frame(table(df[[feature]]))
        colnames(value_count_df) <- c("group", "value")
        value_count_df$proportion <- value_count_df$value / sum(value_count_df$value)

        plot_ly(value_count_df, labels = ~group, values = ~proportion, type = "pie") %>%
          layout(title = feature,
                 paper_bgcolor = "white", plot_bgcolor = "white")
      }
    }
  })

  output$multivariatePlot <- renderPlotly({
    x_feature <- input$x_feature
    y_feature <- input$y_feature
    group_feature <- input$group_feature
    plot_type <- input$multivariatePlotType

    if (plot_type == "scatter") {
      # Scatter plot
      plot_ly(df, x = ~get(x_feature), y = ~get(y_feature), color = ~get(group_feature), type = "scatter", mode = "markers") %>%
        layout(title = paste("Scatter Plot of", y_feature, "vs", x_feature, "Grouped by", group_feature),
               xaxis = list(title = x_feature),
               yaxis = list(title = y_feature),
               paper_bgcolor = "white", plot_bgcolor = "white")
    } else {
      # Box plot
      plot_ly(df, x = ~get(group_feature), y = ~get(y_feature), color = ~get(group_feature), type = "box") %>%
        layout(title = paste("Box Plot of", y_feature, "Grouped by", group_feature),
               xaxis = list(title = group_feature),
               yaxis = list(title = y_feature),
               paper_bgcolor = "white", plot_bgcolor = "white")
    }
  })
}