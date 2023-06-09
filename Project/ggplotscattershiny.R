library(shiny)
library(dplyr)
library(ggplot2)

# We will use the iris dataset.
flowers <- data.frame(iris)
colnames(flowers) <- c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width", "Species")

# This section controls the user interface.

# fluidpage is the type of interface we are going to use. It is very common 
#because of its versitility but other layouts include fillPage(), fixedPage(), 
#flowLayout(), navbarPage(), sidebarLayout(), splitLayout(), and verticalLayout().
ui <- fluidPage(
  
  #titlePanel makes a panel at the top of the application
  titlePanel("Scatterplot of Iris Variables"),
  
  # sidebarPanel makes a panel that takes up 1/3 of the application.
  # We will put the inputs in this side panel
  sidebarPanel(
    
    # selectInput makes a dropdown menu with the parameters (inputId, label, selections)
    # Makes dropdown for X variable
    selectInput("X","X Variable", c(colnames(select_if(flowers, is.numeric)))),
    # Makes dropdown for Y variable
    selectInput("Y","Y Variable", c(colnames(select_if(flowers, is.numeric)))),
    
  ),
  
  # mainpanel makes a panel that takes up the remaining 2/3 of the application.
  mainPanel(
    
    # plotOutput controls the name of the output the inputs are put into.
    plotOutput('ScatterPlot')
    
  )
)

# This section controls what happens to the inputs and outputs.
server <- function(input, output, session)
{
  
  
  # Sets the output named ScatterPlot
  output$ScatterPlot <- renderPlot({
    
    # Creates a plot of the inputs chosen colored by the different flower species
    ggplot(flowers, aes(flowers[, input$X], flowers[, input$Y], color = Species)) +
      theme_bw() +
      geom_point(pch = 19, cex = 2) +
      scale_color_manual(values = c(rainbow(nlevels(flowers$Species)))) +
      labs(title = c("Scatterplot of ", as.character(input$X), "vs", as.character(input$Y)), x = input$X, y = input$Y)

  })
  
}

# Creates the app from the given ui and server
shinyApp(ui, server)

