# shiny/centralTendency/app.R


# Libraries ---------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(ggridges)
library(plotly)
library(moments)


# UI ----------------------------------------------------------------------

# Define UI for application that shows central tendency of a distribution of data
ui <- fluidPage(
  
  # Application title
  titlePanel("Central Tendency"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Change n
      sliderInput("dist_n",
                  "n:",
                  min = 100,
                  max = 10000,
                  value = 1000),
      # Change mean
      sliderInput("dist_mean",
                  "mean:",
                  min = -1.8,
                  max = 35,
                  value = 17),
      # Change SD
      sliderInput("dist_sd",
                  "sd:",
                  min = 0,
                  max = 5,
                  value = 2)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    # Generate random numbers based on inputs
    x <- data.frame(y = rnorm(n = input$dist_n,
                              mean = input$dist_mean,
                              sd = input$dist_sd))
    
    # For testing
    # x <- data.frame(y = rnorm(n = 1000, mean = 2, sd = 2))
    
    # Get moments stats
    x_stats <- data.frame(mean = round(mean(x$y), 2),
                          sd = round(sd(x$y), 2),
                          skew = round(skewness(x$y), 2),
                          kurt = round(kurtosis(x$y), 2))
    
    # Draw the distribution
    dist_plot <- ggplot(data = x, aes(x = y)) +
      geom_rug(aes(text = paste0("Temp. [°C]: ", round(y, 2)))) +
      geom_density() +
      # geom_density_
      geom_vline(aes(xintercept = x_stats$mean, 
                     text = paste0("Mean: ",x_stats$mean)),
                 color = "black", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = x_stats$mean-x_stats$sd, 
                     text = paste0("sd -1: ", x_stats$mean-x_stats$sd)),
                 color = "blue", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = x_stats$mean-x_stats$sd*2, 
                     text = paste0("sd -2: ", x_stats$mean-x_stats$sd*2)),
                 color = "darkblue", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = x_stats$mean+x_stats$sd, 
                     text = paste0("sd +1: ", x_stats$mean+x_stats$sd)),
                 color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = x_stats$mean+x_stats$sd*2, 
                     text = paste0("sd +2: ", x_stats$mean+x_stats$sd*2)),
                 color = "darkred", linetype = "dashed", linewidth = 1) +
      labs(x = "Temperature [°C]", y = "Density") +
      theme(panel.border = element_rect(colour = "black", fill = NA))
    
    # Plotly
    # dist_plot
    ggplotly(dist_plot, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

