# shiny/demoMHW/app.R
# An interactive tool to explain how MHWs are defined

# TODO: Increase size of all text for all figures


# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinycssloaders)
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

source("functions.R", local = TRUE)


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
                                             selected = "None")
                ),
                
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
                                             choices = c("No", "Yes"), selected = "No")
                ),
                
                # Detection menu
                menuItem("Detection", tabName = "detect", icon = icon("magnifying-glass"),
                         # Set minimum MHW duration
                         shiny::numericInput("maxGap", "Max gap",
                                             value = 2, min = 0, max = 366),
                         # Set max gap between MHW
                         shiny::numericInput("minDuration", "Min. duration",
                                             value = 5, min = 1, max = 366),
                         # Show categories
                         shiny::radioButtons("catSelect", "Categories", inline = TRUE,
                                             choices = c("No", "Yes"), selected = "No")
                )#,
                
                # The main event
                # menuItem("The main event", tabName = "event", icon = icon("temperature-high")))
    )
  ),
  
  
  ## Body --------------------------------------------------------------------
  
  body = dashboardBody(
    
    tabsetPanel(
      selected = "Detection",
      
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
                 tabBox(width = 12, title = "", selected = "Day",
                        tabPanel("All", withSpinner(plotOutput("allTime"), type = 6, color = "#b0b7be")),
                        tabPanel("Month", withSpinner(plotOutput("monthTime"), type = 6, color = "#b0b7be")),
                        tabPanel("Year", withSpinner(plotOutput("yearTime"), type = 6, color = "#b0b7be")),
                        tabPanel("DOY", withSpinner(plotOutput("doyTime"), type = 6, color = "#b0b7be")),
                        tabPanel("Day", withSpinner(plotOutput("dayTime"), type = 6, color = "#b0b7be"))
                 )
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
                            # fluidRow(
                            withSpinner(plotOutput("basePlot"), type = 6, color = "#b0b7be"),
                            # ),
                            # fluidRow(
                            # DOY panel - percentiles + trend effect
                            withSpinner(plotOutput("percPlot"), type = 6, color = "#b0b7be")
                        )
                 )
               )
      ),
      
      # Detection menu
      tabPanel(title = "Detection",
               fluidRow(
                 tabBox(width = 12, title = "", selected = "Table",
                        tabPanel("Table", withSpinner(DT::DTOutput("detectTable"), type = 6, color = "#b0b7be")),
                        tabPanel("Lolli", withSpinner(plotlyOutput("lolliPlot"), type = 6, color = "#b0b7be")),
                        tabPanel("Flame", withSpinner(plotlyOutput("flamePlot"), type = 6, color = "#b0b7be"))
                        )
                 )
      ),
      
      # Main event
      tabPanel(title = "The main event",
               fluidPage(
                 column(12,
                        # Figures
                        box(width = 12, title = "", 
                            status = "info", solidHeader = FALSE, collapsible = FALSE,
                            # Focus on the main event
                            withSpinner(plotOutput("mainPlot"), type = 6, color = "#b0b7be")
                        )
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
  
  
  ## Plots/tables -----------------------------------------------------------
  
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
  
  # Show time series with events as flames
  output$flamePlot <- renderPlotly({
    req(input$percSelect)
    
    # Get time series
    df_climatology <- df_detect()$climatology
    
    # Plot
    flamePlot <- ggplot(data = df_climatology, aes(x = t)) +
      geom_line(aes(y = temp, colour = "temp")) +
      geom_line(aes(y = thresh, colour = "thresh"), linewidth = 1.2, alpha = 0.2) +
      geom_line(aes(y = seas, colour = "seas"), linewidth = 1.2, alpha = 0.2) +
      heatwaveR::geom_flame(aes(y = temp, y2 = thresh), fill = "salmon", show.legend = T) +
      scale_colour_manual(name = "Line Colour",
                          values = c("temp" = "black", 
                                     "thresh" =  "purple", 
                                     "seas" = "darkblue")) +
      guides(colour = guide_legend(override.aes = list(fill = NA))) +
      labs(y = "Temperature [°C]", x = NULL) + theme_bw() +
      theme(legend.position = "bottom")
    # flamePlot
    
    # Exit
    ggplotly(flamePlot, tooltip = "text", dynamicTicks = F) |> layout(hovermode = 'compare') 
  })
  
  # Show detected events as lollis
  output$lolliPlot <- renderPlotly({
    
    # Get time series
    df_climatology <- df_detect()$climatology
    
    # Get events
    df_event <- df_detect()$event
    
    # The base lolli figure
    lolliPlot <- ggplot(data = df_event, aes(x = date_peak, y = intensity_max)) +
      geom_segment(aes(xend = date_peak, yend = 0)) +
      geom_hline(yintercept = 0) +
      labs(x = NULL, y = "Max. Intensity (°C)") +
      scale_y_continuous(limits = c(0, max(df_event$intensity_max)*1.1), expand = c(0, 0)) +
      scale_x_date(limits = c(min(df_climatology$t), max(df_climatology$t)), expand = c(0, 0)) + theme_bw()
    
    # Add lolli colour
    if(input$catSelect == "No"){
      suppressWarnings( # Cancel aes(text) warning
        lolliPlot <- lolliPlot +
          geom_point(fill = "salmon", shape = 21, size = 4,
                     aes(text = paste0("Event: ",event_no,
                                       "<br>Duration: ",duration," days",
                                       "<br>Start Date: ", date_start,
                                       "<br>Peak Date: ", date_peak,
                                       "<br>End Date: ", date_end,
                                       "<br>Mean Intensity: ",round(intensity_mean, 2),"°C",
                                       "<br>Max. Intensity: ",round(intensity_max, 2),"°C",
                                       "<br>Cum. Intensity: ",round(intensity_cumulative, 2),"°C")))
      )
    } else if(input$catSelect == "Yes"){
      suppressWarnings( # Cancel aes(text) warning
        lolliPlot <- lolliPlot + 
          geom_point(shape = 21, size = 4,
                     aes(fill = category, 
                         text = paste0("Event: ",event_no,
                                       "<br>Duration: ",duration," days",
                                       "<br>Start Date: ", date_start,
                                       "<br>Peak Date: ", date_peak,
                                       "<br>End Date: ", date_end,
                                       "<br>Mean Intensity: ",round(intensity_mean, 2),"°C",
                                       "<br>Max. Intensity: ",round(intensity_max, 2),"°C",
                                       "<br>Cum. Intensity: ",round(intensity_cumulative, 2),"°C"))) +
          scale_fill_manual(name = NULL, values = MHW_colours, guide = "none")
      )
    }

    # lolliPlot
    ggplotly(lolliPlot, tooltip = "text", dynamicTicks = F) |> style(showlegend = FALSE)
  })
  
  # The main event plot
  output$detectTable <- renderDT({
    req(input$percSelect)
    
    # Get event data and make it pretty
    event_data <- df_detect()$event |> 
      mutate(Event = "MHW") |> 
      dplyr::rename('#' = event_no,
                    Duration = duration,
                    'Start Date' = date_start,
                    'Peak Date' = date_peak,
                    'End Date' = date_end,
                    'Mean Intensity' = intensity_mean,
                    'Max. Intensity' = intensity_max,
                    'Cum. Intensity' = intensity_cumulative,
                    Category = category) |> 
      dplyr::arrange(`Peak Date`) |>  
      dplyr::select(Event, `#`, Duration, `Start Date`, `Peak Date`, `End Date`,
                    `Mean Intensity`, `Max. Intensity`, `Cum. Intensity`, Category)
    
    # Exit
    DT::datatable(event_data, options = list(pageLength = 10))
  })
  
  # The main event plot
  output$mainPlot <- renderPlot({
    req(input$catSelect)
    
    # Get time series
    df_climatology <- df_detect()$climatology
    
    # Get events
    df_event <- df_detect()$event
    
    # Get top event
    
    
    # Annotated flame
    flame_2 <- ggplot(data = sst_MHW_clim_top, aes(x = t, y = temp)) +
      geom_flame(aes(y2 = thresh), n = 5, n_gap = 2) +
      # geom_flame(data = peak_event, aes(y2 = thresh, fill = "Focal MHW"), n = 5, n_gap = 2) +
      geom_line(aes(y = seas, col = "clima"), size = 0.7, show.legend = F) +
      geom_line(aes(y = thresh, col = "thresh"), size = 0.7, show.legend = F) +
      geom_line(aes(y = temp, col = "temp"), size = 0.6, show.legend = F) +
      # Duration label
      geom_segment(colour = "springgreen",
                   aes(x = as.Date("2010-12-23"), xend = as.Date("2010-12-23"),
                       y = 23.08, yend = 22.5)) +
      geom_segment(colour = "springgreen",
                   aes(x = as.Date("2011-04-08"), xend = as.Date("2011-04-08"),
                       y = 24.7, yend = 22.5)) +
      geom_segment(colour = "springgreen",
                   aes(x = as.Date("2010-12-23"), xend = as.Date("2011-04-08"),
                       y = 22.5, yend = 22.5)) +
      geom_label(aes(label = "Duration = 105 days", x = as.Date("2011-02-15"), y = 22.5),
                 colour = "springgreen", label.size = 3) +
      geom_label(aes(label = "Duration = 105 days", x = as.Date("2011-02-15"), y = 22.5),
                 colour = "black", label.size = 0) +
      # Max intensity label
      geom_segment(colour = "forestgreen",
                   aes(x = as.Date("2011-01-15"), xend = as.Date("2011-02-28"),
                       y = 29.74, yend = 29.74)) +
      geom_segment(colour = "forestgreen",
                   aes(x = as.Date("2011-02-28"), xend = as.Date("2011-02-28"),
                       y = 23.1602, yend = 29.74)) +
      geom_label(aes(label = "Max. Intensity = 6.58°C", x = as.Date("2011-02-01"), y = 29.4),
                 colour = "forestgreen", label.size = 3) +
      geom_label(aes(label = "Max. Intensity = 6.58°C", x = as.Date("2011-02-01"), y = 29.4),
                 colour = "black", label.size = 0) +
      # Cumulative intensity label
      geom_label(aes(label = "Cum. Intensity = 293.21°CxDays", x = as.Date("2011-02-28"), y = 26),
                 colour = "salmon", label.size = 3) +
      geom_label(aes(label = "Cum. Intensity = 293.21°CxDays", x = as.Date("2011-02-28"), y = 26),
                 colour = "black", label.size = 0) +
      # Aesthetics
      scale_colour_manual(name = "Line Colour",
                          values = c("temp" = "black", 
                                     "thresh" =  "purple", 
                                     "seas" = "darkblue")) +
      scale_fill_manual(name = NULL, values = fillCol,
                        breaks = c("Focal MHW", "Other MHWs")) +
      scale_x_date(expand = c(0, 0), date_breaks = "2 months", date_labels = "%b %Y", ) +
      labs(x = "Date", y = "Temperature [°C]", title = "Western Australia MHW; Top event metrics") +
      theme(panel.background = element_rect(colour = "black"))
    
    # Exit
    mainPlot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
