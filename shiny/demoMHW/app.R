# shiny/demoMHW/app.R
# An interactive tool to explain how MHWs are defined

# TODO: Increase size of all text for all figures


# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(arrow)
library(ggplot2)
library(plotly)
library(heatwaveR)

# The MHW category colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# The MCS colour palette
MCS_colours <- c(
  "Moderate" = "#C7ECF2",
  "Strong" = "#85B7CC",
  "Severe" = "#4A6A94",
  "Extreme" = "#111433"
)

# Set line colours for categories
lineColCat <- c(
  "Temperature" = "black",
  "Climatology" = "gray20",
  "Threshold" = "darkgreen",
  "2x Threshold" = "darkgreen",
  "3x Threshold" = "darkgreen",
  "4x Threshold" = "darkgreen"
)


# Functions ---------------------------------------------------------------

# Function for ensuring 366 DOY for non-leap years
leap_every_year <- function(x) {
  ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
}

# Time plot wrapper
time_plot <- function(time_span, df, time_highlight){
  
  # Get time series and modify t columns
  df <- df |> 
    mutate(all = "All",
           month = lubridate::month(t, label = TRUE),
           year = lubridate::year(t),
           doy = leap_every_year(t),
           day = t)
  
  # Set t column to timeSelect
  if(time_span == "All"){
    df$t <- df$all
  } else if(time_span == "Month"){
    df$t <- df$month
  } else if(time_span == "Year"){
    df$t <- df$year
  } else if(time_span == "DOY"){
    df$t <- df$doy
  } else if(time_span == "Day"){
    df$t <- df$day
  }
  
  if(time_span == "DOY"){
    box_line_width = 0.1 
  } else if(time_span == "Year") {
    box_line_width = 1
  } else if(time_span == "Month") {
    box_line_width = 2
  } else if(time_span == "All") {
    box_line_width = 3
  }
  
  # Base plot
  timePlot <- ggplot(data = df, aes(x = t, y = temp)) + 
    labs(x = NULL, y = "Temperature [°C]") + theme_bw()
  
  # Add points
  if(time_highlight == "None")
    timePlot <- timePlot + geom_point(position = "jitter")
  if(time_highlight == "Month")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = month)) + 
    scale_colour_viridis_d()
  if(time_highlight == "Year")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = year)) + 
    scale_colour_viridis_c(option = "A")
  if(time_highlight == "DOY")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = doy)) + 
    scale_colour_viridis_c(option = "B")
  
  # Add boxplots
  if(time_span != "Day"){
    timePlot <- timePlot + geom_boxplot(aes(group = t), linewidth = box_line_width,
                                        colour = "grey", alpha = 0.2, outlier.shape = NA)
  }
  
  # Scale x axis
  if(time_span == "All")  timePlot <- timePlot + scale_x_discrete(expand = c(0, 0))
  if(time_span %in% c("Year", "DOY"))  timePlot <- timePlot + scale_x_continuous(expand = c(0, 0))
  if(time_span == "Day")  timePlot <- timePlot + scale_x_date(expand = c(0, 0))
  
  # Exit
  timePlot
}


# Data --------------------------------------------------------------------

# Explicitly load TS into memory
sst_WA <- heatwaveR::sst_WA
sst_NW_Atl <- heatwaveR::sst_NW_Atl
sst_Med <- heatwaveR::sst_Med


# UI ----------------------------------------------------------------------

# Define UI
ui <- dashboardPage(
  
  # The app title
  # TODO: Add messages etc
  # https://rstudio.github.io/shinydashboard/structure.html
  header = dashboardHeader(title = "MHW definition"),
  
  
  ## Sidebar -----------------------------------------------------------------
  
  sidebar = dashboardSidebar(
    
    # The side bar
    sidebarMenu(id = "mainMenu",
                
                # The Overview
                menuItem("Overview", tabName = "overview", icon = icon("desktop")),
                
                # Time menu
                menuItem("Time series", tabName = "time", icon = icon("clock"),
                         # Select time series
                         selectizeInput("seriesSelect", "Time series",
                                        choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                        selected = c("Western Australia")),
                         # Highlight data points by time period
                         shiny::radioButtons("timeSelect", "Highlight time", inline = TRUE,
                                             choices = c("Month", "Year", "DOY", "None"), 
                                             selected = "None")),
                
                # Statistics menu
                menuItem("Statistics", tabName = "perc_base", icon = icon("percent"),
                         # Select baseline period
                         shiny::sliderInput("baseSelect", "Baseline", min = 1982, max = 2022, 
                                            value = c(1982, 2011), sep = ""),
                         # Select quantile
                         shiny::numericInput("percSelect", "Percentile",
                                             value = 90, min = 1, max = 100),
                         # Detrend - no - linear - non-linear
                         shiny::radioButtons("trendSelect", "Remove trend", inline = TRUE,
                                             choices = c("No", "Yes"), selected = "No")),
                
                # Detection menu
                menuItem("Detection", tabName = "detect", icon = icon("magnifying-glass"),
                         # Set minimum MHW duration
                         shiny::numericInput("maxGap", "Max gap",
                                             value = 2, min = 0, max = 366),
                         # Set max gap between MHW
                         shiny::numericInput("minDuration", "Min. duration",
                                             value = 5, min = 1, max = 366)),
                
                # The main event
                menuItem("The main event", tabName = "event", icon = icon("temperature-high")))
  ),
  
  
  ## Body --------------------------------------------------------------------
  
  body = dashboardBody(
    
    tabsetPanel(
      
      # Accueil
      tabPanel(title = "Overview",
               fluidPage(
                 column(12,
                        img(src = "MHW_def.png", width = "600px", 
                            style = "float:left; margin-right: 20px; margin-top: 20px;"),
                        # h1(tags$b("Overview")),
                        h2(tags$b("Brief")),
                        p("This demo was designed to provide a visual and interactive explanation for how one may detect
                                 a marine heatwave (MHW) using the Hobday et al. (2016, 2018) definition."),
                        h2(tags$b("References")),
                        p("Hobday et al. (2016, 2018) etc.")
                 )
               )
      ),
      
      # Time menu
      tabPanel(title = "time",
               p("Once upon a time series..."),
               fluidRow(
                 # column(12,
                        tabBox(width = 12,
                          title = "", selected = "Day",
                          tabPanel("All", shinycssloaders::withSpinner(plotOutput("allTime"), 
                                                                type = 6, color = "#b0b7be")),
                          tabPanel("Month", shinycssloaders::withSpinner(plotOutput("monthTime"), 
                                                                         type = 6, color = "#b0b7be")),
                          tabPanel("Year", shinycssloaders::withSpinner(plotOutput("yearTime"), 
                                                                        type = 6, color = "#b0b7be")),
                          tabPanel("DOY", shinycssloaders::withSpinner(plotOutput("doyTime"), 
                                                                       type = 6, color = "#b0b7be")),
                          tabPanel("Day", shinycssloaders::withSpinner(plotOutput("dayTime"), 
                                                                       type = 6, color = "#b0b7be"))
                        )
                 # )
               )
      ),
      
      # Stats menu
      tabPanel(title = "Statistics",
               p("Lies, damn lies, and statistics..."),
               fluidPage(
                 column(12,
                        # Figures
                        box(width = 12, title = "", 
                            status = "info", solidHeader = FALSE, collapsible = FALSE,
                            # Overview of time series - baseline + trend
                            plotOutput("basePlot"),
                            # DOY panel - percentiles + trend effect
                            plotOutput("percPlot")
                        )
                        # TODO: Add table of MHW metrics
                 )
               )
      ),
      
      # Detection menu
      tabPanel(title = "Detection",
               fluidPage(
                 column(12,
                        # Figures
                        box(width = 12, title = "Data", 
                            status = "info", solidHeader = TRUE, collapsible = FALSE,
                            # Lolliplot
                            plotOutput("lolliPlot")
                        )
                        # TODO: Add table of MHW metrics)
                 )
               )
      ),
      
      # Main event
      tabItem(title = "The main event",
              fluidPage(
                column(12,
                       # Figures
                       box(width = 9, title = "Data", 
                           status = "info", solidHeader = TRUE, collapsible = FALSE,
                           # Overview of time series - baseline + trend
                           plotOutput("mainPlot")
                       )
                       # TODO: Add table of MHW metrics)
                )
              )
      )
    )
  )
)


# Server ------------------------------------------------------------------

# Server
server <- function(input, output, session) {
  
  
  ## Reactive UI -------------------------------------------------------------
  
  
  ## Reactive data -----------------------------------------------------------
  
  # Base time series and de-trending anomalies
  df_site_ts <- reactive({
    req(input$seriesSelect, input$trendSelect)
    if(input$seriesSelect == "Western Australia"){
      df_site_ts <- sst_WA
    } else if(input$seriesSelect == "NW Atlantic"){
      df_site_ts <- sst_NW_Atl
    } else if(input$seriesSelect == "Mediterranean"){
      df_site_ts <- sst_Med
    }
    return(df_site_ts)
  })
  
  # Run ts2clm
  df_ts2clm <- reactive({
    req(input$baseSelect, input$percSelect, input$trendSelect)
    df_site_ts <- df_site_ts()
    if(input$trendSelect == "No"){
      df_site_ts$temp <- df_site_ts$temp-mean(df_site_ts$temp)
    } else if(input$trendSelect == "Yes"){
      df_site_ts$temp <- as.vector(lm(temp ~ t, df_site_ts)$residuals)
    }
    df_ts2clm <- ts2clm(df_site_ts, 
                        climatologyPeriod = c(paste0(input$baseSelect[1],"-01-01"),
                                              paste0(input$baseSelect[2],"-12-31")),
                        pctile = input$percSelect)
    # For testing...
    # climatologyPeriod = c("1982-01-01", "2011-12-31"),
    # pctile = 90)
    return(df_ts2clm)
  })
  
  # Run detect_event
  df_detect <- reactive({
    req(input$minDuration, input$maxGap)
    df_ts2clm <- df_ts2clm()
    df_detect <- detect_event(df_ts2clm, 
                              # For testing...
                              # minDuration = 5, maxGap = 2,
                              minDuration = input$minDuration, maxGap = input$maxGap,
                              categories = TRUE, climatology = TRUE)
    return(df_detect)
  })
  
  
  ## Plots -------------------------------------------------------------------
  
  # All time plot
  output$allTime <- renderPlot({
    df_site_ts <- df_site_ts()
    time_plot("All", df_site_ts, input$timeSelect)
  })
  
  # Month time plot
  output$monthTime <- renderPlot({
    df_site_ts <- df_site_ts()
    time_plot("Month", df_site_ts, input$timeSelect)
  })
  
  # Year time plot
  output$yearTime <- renderPlot({
    df_site_ts <- df_site_ts()
    time_plot("Year", df_site_ts, input$timeSelect)
  })
  
  # DOY time plot
  output$doyTime <- renderPlot({
    df_site_ts <- df_site_ts()
    time_plot("DOY", df_site_ts, input$timeSelect)
  })
  
  # Day time plot
  output$dayTime <- renderPlot({
    df_site_ts <- df_site_ts()
    time_plot("Day", df_site_ts, input$timeSelect)
  })
  
  # Plot that illustrates baseline selection
  output$basePlot <- renderPlot({
    req(input$baseSelect)
    
    # Get base time series
    df_site_ts <- df_site_ts()
    
    # Get time series with stats
    df_ts2clm <- df_ts2clm()
    
    # Filter by baseline
    df_site_base <- df_site_ts |> 
      filter(t >= paste0(input$baseSelect[1],"-01-01"),
             t <= paste0(input$baseSelect[2],"-12-31"))
    
    # Points above threshold
    df_thresh <- df_ts2clm |> filter(temp >= thresh)
    df_thresh_base <- df_site_ts |> filter(t %in% df_thresh$t)
    
    # Plot
    basePlot <- ggplot(data = df_site_ts, aes(x = t, y = temp)) + geom_line() + 
      geom_line(data = df_site_base, colour = "darkblue", linewidth = 1.1, alpha = 0.6) +
      geom_vline(aes(xintercept = min(df_site_base$t)), 
                 linetype = "dashed", linewidth = 2, colour = "darkblue") +
      geom_vline(aes(xintercept = max(df_site_base$t)), 
                 linetype = "dashed", linewidth = 2, colour = "darkblue") +
      geom_rug(data = df_site_base, sides = "b", colour = "darkblue") +
      geom_point(data = df_thresh_base, colour = "purple") +
      scale_x_date(expand = c(0, 0)) +
      theme_bw() + labs(x = NULL, y = "Temperature [°C]")
    
    if(input$trendSelect == "No"){
      basePlot <- basePlot + geom_hline(yintercept = mean(df_site_ts$temp), colour = "tomato")
    } else if(input$trendSelect == "Yes"){
      basePlot <- basePlot + geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, colour = "tomato")
    }
    
    # Exit
    basePlot
  })
  
  # Plot that shows effect of percentile selection
  output$percPlot <- renderPlot({
    req(input$percSelect)
    
    # Run ts2clm
    df_ts2clm <- df_ts2clm()
    
    # Points above threshold
    df_thresh <- df_ts2clm |> filter(temp >= thresh)
    
    # Plot
    percPlot <- ggplot(data = df_ts2clm, aes(x = doy, y = temp)) +
      geom_point(aes(y = temp, colour = "temp")) +
      geom_point(data = df_thresh, aes(colour = "thresh")) +
      geom_line(aes(y = thresh, colour = "thresh"), linewidth = 1.2) +
      geom_line(aes(y = seas, colour = "seas"), linewidth = 1.2) +
      geom_hline(aes(yintercept = 0, colour = "anomaly"), linewidth = 1.2) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "thresh" =  "purple", 
                                     "seas" = "darkblue",
                                     "anomaly" = "tomato")) +
      scale_x_continuous(expand = c(0, 0)) +
      labs(x = NULL, y = "Temp. anomaly [°C]") +
      theme_bw() + theme(legend.position = "top")
    
    # Exit
    percPlot
  })
  
  output$lolliPlot <- renderPlot({
    req(input$percSelect)
    
    # Run ts2clm
    df_ts2clm <- df_ts2clm()
    
    # Points above threshold
    df_thresh <- df_ts2clm |> filter(temp >= thresh)
    
    # Plot
    lolliPlot <- ggplot(data = df_ts2clm, aes(x = doy, y = temp)) +
      geom_point(aes(y = temp), colour = "blue")
    
    # Exit
    lolliPlot
  })
  
  
  output$mainPlot <- renderPlot({
    req(input$percSelect)
    
    # Run ts2clm
    df_ts2clm <- df_ts2clm()
    
    # Points above threshold
    df_thresh <- df_ts2clm |> filter(temp >= thresh)
    
    # Plot
    mainPlot <- ggplot(data = df_ts2clm, aes(x = doy, y = temp)) +
      geom_point(aes(y = temp), colour = "red")
    
    # Exit
    mainPlot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
