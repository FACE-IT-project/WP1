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
    x <- data.frame(x = rnorm(n = input$dist_n,
                              mean = input$dist_mean,
                              sd = input$dist_sd),
                    y = "name")
    
    # For testing
    # x <- data.frame(x = rnorm(n = 1000, mean = 2, sd = 2), y = "name")
    
    # Get moments stats
    x_stats <- data.frame(mean = round(mean(x$x), 2),
                          sd = round(sd(x$x), 2),
                          skew = round(skewness(x$x), 2),
                          kurt = round(kurtosis(x$x), 2))
    
    # https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
    
    # Draw the distribution
    dist_plot <- ggplot(data = x, aes(x = x, y = y)) +
      geom_density_ridges(jittered_points = TRUE) +
      # geom_rug(aes(text = paste0("Temp. [°C]: ", round(y, 2)))) +
      # geom_density() +
      # geom_density_
      # geom_vline(aes(xintercept = x_stats$mean, 
      #                text = paste0("Mean: ",x_stats$mean)),
      #            color = "black", linetype = "dashed", linewidth = 1) +
      # geom_vline(aes(xintercept = x_stats$mean-x_stats$sd, 
      #                text = paste0("sd -1: ", x_stats$mean-x_stats$sd)),
      #            color = "blue", linetype = "dashed", linewidth = 1) +
      # geom_vline(aes(xintercept = x_stats$mean-x_stats$sd*2, 
      #                text = paste0("sd -2: ", x_stats$mean-x_stats$sd*2)),
      #            color = "darkblue", linetype = "dashed", linewidth = 1) +
      # geom_vline(aes(xintercept = x_stats$mean+x_stats$sd, 
      #                text = paste0("sd +1: ", x_stats$mean+x_stats$sd)),
      #            color = "red", linetype = "dashed", linewidth = 1) +
      # geom_vline(aes(xintercept = x_stats$mean+x_stats$sd*2, 
      #                text = paste0("sd +2: ", x_stats$mean+x_stats$sd*2)),
      #            color = "darkred", linetype = "dashed", linewidth = 1) +
      labs(x = "Temperature [°C]", y = NULL) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA))
    # dist_plot
    
    # Plotly
    ggplotly(dist_plot, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

