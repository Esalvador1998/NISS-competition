library(shiny)
setwd("C:/Users/Eduardo/Documents/CMDA 3654/hw1/")
cars <- data.frame(read.csv(file = "cars (1).csv"))
cars <- data.frame(cars$MPG, cars$Horsepower, cars$Weight, cars$Acceleration)

ui <- fluidPage(
  
  titlePanel("K-means clustering"),
  sidebarPanel(
    # Makes dropdown for X variable
    selectInput("x","X Variable", c(colnames(cars))),
    # Makes dropdown for Y variable
    selectInput("y","Y Variable", c(colnames(cars))),
    # Type input for the number of clusters to make 
    numericInput('clusters', 'Number of Clusters', 3, min = 1, max = 6)
  ),
  mainPanel(
    plotOutput('plot')
  )
)


server <- function(input, output, session)
{
  clusters <- reactive({
    kmeans(cars[, c(input$x, input$y)], input$clusters)
  })
  
  output$plot <- renderPlot({
    plot(cars[, c(input$x, input$y)], col = clusters()$cluster, pch = 20, cex = 2)
    points(clusters()$centers, pch = 8, cex = 3)
  })
}

shinyApp(ui, server)

