# ui.R
library(shiny)
library(plotly)

df <- read.csv("credit_eligibility_clean.csv")

ui <- fluidPage(
  titlePanel("Credit Eligibility Data Exploration"),
  
  # Tabset Panel for Univariate and Multivariate Analysis
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Univariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("feature", "Select Feature to Analyze",
                             choices = names(df)),
                 conditionalPanel(
                   condition = "input.feature == 'Income' || input.feature == 'Customer.relative.age' || input.feature == 'Employment.relative.length' || input.feature == 'Account.relative.age'",
                   radioButtons("plotType", "Plot Type",
                                choices = c("Histogram" = "histogram", "Box Plot" = "boxplot")),
                   sliderInput("bins", "Number of Bins", min = 1, max = 100, value = 30)
                 ),
                 conditionalPanel(
                   condition = "input.feature != 'Children.count' && input.feature != 'Family.member.count' && input.feature != 'Has.a.car' && input.feature != 'Has.a.property' && input.feature != 'Has.a.work.phone' && input.feature != 'Has.a.phone' && input.feature != 'Has.an.email' && input.feature != 'Is.high.risk' && !grepl('count$', input.feature)",
                   radioButtons("plotType", "Plot Type",
                                choices = c("Bar Chart" = "bar", "Pie Chart" = "pie"))
                 )
               ),
               mainPanel(
                 plotlyOutput("distPlot")
               )
             )
    ),
    
    tabPanel("Multivariate Analysis", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_feature", "Select X-axis Feature",
                             choices = names(df)),
                 selectInput("y_feature", "Select Y-axis Feature",
                             choices = names(df)),
                 selectInput("group_feature", "Select Grouping Feature",
                             choices = names(df)),
                 radioButtons("multivariatePlotType", "Plot Type",
                              choices = c("Scatter Plot" = "scatter", "Box Plot" = "box"))
               ),
               mainPanel(
                 plotlyOutput("multivariatePlot")
               )
             )
    )
  )
)
