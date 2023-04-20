#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(MASS)

data <- read.csv("C:/Users/jarod/Desktop/Final Project/Boston_Housing_Data.csv")
data <- data %>%
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), coalesce(., 0)))
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Visualization of Boston Housing Data"),

    # Sidebar with a slider input for range of values. 
    sidebarLayout(
        sidebarPanel(
          selectInput("Xvariable", "Choose a Variable:", choices = colnames(data))),  
          sliderInput("Range",
                        "Select a Range of Values:",
                        min = 0,
                        max = 400,
                        value = c(0,400))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterplot")
        )
    )


# Define server logic required to draw a scatter plot.
server <- function(input, output) {
  
    output$scatterplot <- renderPlot({
        ggplot(data, aes_string(x = input$Xvariable, y = "MEDV", color = "MEDV", size = input$Xvariable)) +
        geom_point(alpha = 0.65) +
        stat_smooth(method = "lm") +
        xlab(input$Xvariable) +
        ylab("Median Value of Owner-Occupied Homes") +
        ggtitle("Boston Housing Data")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
