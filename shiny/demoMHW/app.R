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
library(ggplot2)
library(ggpattern)
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

# Set global text size
global_text <- 18


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
    labs(x = NULL, y = "Temperature [°C]") + theme_bw() +
    theme(text = element_text(size = global_text))
  
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
                                        colour = "grey", alpha = 0.4, outlier.shape = NA)
  }
  
  # Scale x axis
  if(time_span == "All")  timePlot <- timePlot + scale_x_discrete(expand = c(0, 0))
  if(time_span %in% c("Year", "DOY"))  timePlot <- timePlot + scale_x_continuous(expand = c(0, 0))
  if(time_span == "Day")  timePlot <- timePlot + scale_x_date(expand = c(0, 0))
  
  # Exit
  timePlot
}

# Function needed for making geom_flame() work with plotly
geom2trace.GeomFlame <- function(data, params, p){
  
  x <- y <- y2 <- NULL
  
  # Create data.frame for ease of use
  data1 <- data.frame(x = data[["x"]],
                      y = data[["y"]],
                      y2 = data[["y2"]])
  
  # Grab parameters
  n <- params[["n"]]
  n_gap <- params[["n_gap"]]
  
  # Find events that meet minimum length requirement
  data_event <- heatwaveR::detect_event(data1, x = x, y = y,
                                        seasClim = y,
                                        threshClim = y2,
                                        minDuration = n,
                                        maxGap = n_gap,
                                        protoEvents = T)
  
  # Detect spikes
  data_event$screen <- base::ifelse(data_event$threshCriterion == FALSE, FALSE,
                                    ifelse(data_event$event == FALSE, TRUE, FALSE))
  
  # Screen out spikes
  data1 <- data1[data_event$screen != TRUE,]
  
  # Prepare to find the polygon corners
  x1 <- data1$y
  x2 <- data1$y2
  
  # # Find points where x1 is above x2.
  above <- x1 > x2
  above[above == TRUE] <- 1
  above[is.na(above)] <- 0
  
  # Points always intersect when above=TRUE, then FALSE or reverse
  intersect.points <- which(diff(above) != 0)
  
  # Find the slopes for each line segment.
  x1.slopes <- x1[intersect.points + 1] - x1[intersect.points]
  x2.slopes <- x2[intersect.points + 1] - x2[intersect.points]
  
  # # Find the intersection for each segment.
  x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes - x2.slopes))
  y.points <- x1[intersect.points] + (x1.slopes * (x.points - intersect.points))
  
  # Coerce x.points to the same scale as x
  x_gap <- data1$x[2] - data1$x[1]
  x.points <- data1$x[intersect.points] + (x_gap*(x.points - intersect.points))
  
  # Create new data frame and merge to introduce new rows of data
  data2 <- data.frame(y = c(data1$y, y.points), x = c(data1$x, x.points))
  data2 <- data2[order(data2$x),]
  data3 <- base::merge(data1, data2, by = c("x","y"), all.y = T)
  data3$y2[is.na(data3$y2)] <- data3$y[is.na(data3$y2)]
  
  # Remove missing values for better plotting
  data3$y[data3$y < data3$y2] <- NA
  missing_pos <- !stats::complete.cases(data3[c("x", "y", "y2")])
  ids <- cumsum(missing_pos) + 1
  ids[missing_pos] <- NA
  
  # Get the correct positions
  positions <- data.frame(x = c(data3$x, rev(data3$x)),
                          y = c(data3$y, rev(data3$y2)),
                          ids = c(ids, rev(ids)))
  
  # Convert to a format geom2trace is happy with
  positions <- plotly::group2NA(positions, groupNames = "ids")
  positions <- positions[stats::complete.cases(positions$ids),]
  positions <- dplyr::left_join(positions, data[,-c(2,3)], by = "x")
  if(length(stats::complete.cases(positions$PANEL)) > 1) 
    positions$PANEL <- positions$PANEL[stats::complete.cases(positions$PANEL)][1]
  if(length(stats::complete.cases(positions$group)) > 1) 
    positions$group <- positions$group[stats::complete.cases(positions$group)][1]
  
  # Run the plotly polygon code
  if(length(unique(positions$PANEL)) == 1){
    getFromNamespace("geom2trace.GeomPolygon", asNamespace("plotly"))(positions)
  } else{
    return()
  }
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
                         selectizeInput(
                           inputId = "seriesSelect",
                           label = "Select data",
                           choices = "Western Australia",
                           multiple = FALSE,
                           selected = "Western Australia",
                           options = list(
                             options = list(
                               list(
                                 hemi = "N", value = "NW Atlantic", name = "NW Atlantic", 
                                 tooltip = "Medium events, medium interannual variability, strong linear trend"
                               ),
                               list(
                                 hemi = "N", value = "Mediterranean", name = "Mediterranean", 
                                 tooltip = "Smaller events, low interannual variability, medium linear trend"
                               ),
                               list(
                                 hemi = "S", value = "Western Australia", name = "Western Australia",
                                 tooltip = "Extreme event, high interannual variability, weak linear trend"
                               )
                             ),
                             optgroups = list(
                               list(
                                 value = "N",  label = "Northern hemisphere",  
                                 tooltip = "Winter in January"
                               ),
                               list(
                                 value = "S",    label = "Southern hemisphere", 
                                 tooltip = "Summer in January"
                               )
                             ),
                             optgroupField = "hemi",
                             labelField = "name",
                             render = I(
                               "{
                               optgroup_header: function(data, escape) {
                               return '<div class=\"optgroup-header\"><span ' + 
                               'data-toggle=\"tooltip\" data-placement=\"right\" title=\"' + 
                               data.tooltip + '\">' + escape(data.label) + '</span></div>';
                               },
                               option: function(data, escape) {
                               return '<div class=\"option\"><span ' + 
                               'data-toggle=\"tooltip\" data-placement=\"top\" title=\"' + 
                               data.tooltip + '\">' + escape(data.name) + '</span></div>';
                               }
                               }"
                               ),
                             onDropdownOpen = I(
                               "function() {
                               setTimeout(function(){$('[data-toggle=tooltip]').tooltip();}, 100);
                               }"
                               ),
                             onChange = I(
                               "function() {
                               setTimeout(function(){$('[data-toggle=tooltip]').tooltip();}, 100);
                               }"
                               )
                             )
                           ),
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
                         # De-trend
                         shiny::radioButtons("trendSelect", "Remove trend", inline = TRUE,
                                             choices = c("No", "Yes"), selected = "No")
                         ),
                
                # Detection menu
                menuItem("Detection", tabName = "detect", icon = icon("magnifying-glass"),
                         # Set max gap between MHW
                         shiny::numericInput("minDuration", "Min. duration",
                                             value = 5, min = 1, max = 366),
                         # Set minimum MHW duration
                         shiny::numericInput("maxGap", "Max gap",
                                             value = 2, min = 0, max = 366),
                         # Show categories
                         shiny::radioButtons("catSelect", "Categories", inline = TRUE,
                                             choices = c("No", "Yes"), selected = "No")
                         )
                )
    ),
  
  
  ## Body --------------------------------------------------------------------
  
  body = dashboardBody(
    
    tabsetPanel(
      selected = "The main event",
      
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
      tabPanel(title = "Time",
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
                            withSpinner(plotOutput("basePlot"), type = 6, color = "#b0b7be"),
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
    time_plot("All", df_site_ts(), input$timeSelect)
  })
  
  # Month time plot
  output$monthTime <- renderPlot({
    time_plot("Month", df_site_ts(), input$timeSelect)
  })
  
  # Year time plot
  output$yearTime <- renderPlot({
    time_plot("Year", df_site_ts(), input$timeSelect)
  })
  
  # DOY time plot
  output$doyTime <- renderPlot({
    time_plot("DOY", df_site_ts(), input$timeSelect)
  })
  
  # Day time plot
  output$dayTime <- renderPlot({
    time_plot("Day", df_site_ts(), input$timeSelect)
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
    basePlot <- ggplot(data = df_site_ts, aes(x = t, y = temp)) + 
      geom_line(aes(colour = "temp"), linewidth = 0.5) + 
      # geom_line(data = df_site_base, colour = "darkblue", linewidth = 1.1, alpha = 0.6) +
      geom_hline(aes(yintercept = mean(df_site_ts$temp), colour = "mean"), linewidth = 2) +
      geom_smooth(aes(colour = "trend"),
                  method = "lm", formula = "y ~ x", se = FALSE, linewidth = 2) +
      geom_vline(aes(xintercept = min(df_site_base$t), colour = "seas"), 
                 linetype = "dashed", linewidth = 2) +
      geom_vline(aes(xintercept = max(df_site_base$t), colour = "seas"), 
                 linetype = "dashed", linewidth = 2) +
      geom_rug(data = df_site_base, sides = "b", aes(colour = "seas")) +
      geom_point(data = df_thresh_base, aes(colour = "thresh")) +
      scale_x_date(expand = c(0, 0)) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "seas" = "darkblue",
                                     "thresh" =  "purple", 
                                     "mean" = "darkgreen",
                                     "trend" = "tomato"),
                          breaks = c("temp", "seas", "thresh", "mean", "trend")) +
      guides(colour = guide_legend(override.aes = list(shape = 15, size = 5))) +
      labs(x = NULL, y = "Temperature [°C]") + theme_bw() + 
      theme(legend.position = "bottom",
            text = element_text(size = global_text))
    
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
    
    # Get de-trend info
    if(input$trendSelect == "No"){
      detrend <- "mean"
    } else if(input$trendSelect == "Yes"){
      detrend <- "trend"
    }
    
    # Plot
    percPlot <- ggplot(data = df_ts2clm, aes(x = doy, y = temp)) +
      geom_point(aes(y = temp, colour = "temp")) +
      geom_point(data = df_thresh, aes(colour = "thresh")) +
      geom_line(aes(y = thresh, colour = "thresh"), linewidth = 2) +
      geom_line(aes(y = seas, colour = "seas"), linewidth = 2) +
      geom_hline(aes(yintercept = 0, colour = {{ detrend }}), linewidth = 2) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "thresh" =  "purple", 
                                     "seas" = "darkblue",
                                     "mean" = "darkgreen",
                                     "trend" = "tomato")) +
      labs(x = NULL, y = "Temp. anomaly [°C]") + theme_bw() + 
      theme(legend.position = "none",
            text = element_text(size = global_text))
    
    # Exit
    percPlot
  })
  
  # Show time series with events as flames
  output$flamePlot <- renderPlotly({
  # flamePlot <- reactive({
    req(input$percSelect)
    
    # Get time series
    df_climatology <- df_detect()$climatology
    
    # Plot
    flamePlot <- ggplot(data = df_climatology, aes(x = t)) +
      geom_line(aes(y = temp, colour = "temp")) +
      geom_line(aes(y = thresh, colour = "thresh"), linewidth = 1.2, alpha = 0.2) +
      geom_line(aes(y = seas, colour = "seas"), linewidth = 1.2, alpha = 0.2) +
      heatwaveR::geom_flame(aes(y = temp, y2 = thresh), fill = "salmon", show.legend = T) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "thresh" =  "purple", 
                                     "seas" = "darkblue")) +
      scale_x_date(expand = c(0, 0)) +
      # guides(colour = guide_legend(override.aes = list(fill = NA))) +
      labs(y = "Temperature [°C]", x = NULL) + theme_bw() +
      theme(legend.position = "bottom")
    # flamePlot
    
    # Exit
    ggplotly(flamePlot, tooltip = "text", dynamicTicks = F) |> layout(hovermode = 'compare')
  })
  
  # flamePlotly <- reactive({
  #   
  #   # Grab static plot
  #   p <- flamePlot()
  #   
  #   # Convert to plotly
  #   # NB: Setting dynamicTicks = T causes the flames to be rendered incorrectly
  #   pp <- ggplotly(p, tooltip = "text", dynamicTicks = F) %>% 
  #     layout(hovermode = 'compare') 
  #   pp
  # })
  
  # output$flamePlot <- renderPlotly({
  #   flamePlotly()
  # })
  
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
  # TODO: Rather show stats as small floating coloured labels in the top left corner
  # Highlight duration by overplotting along threshold line
  # Add category visuals
  output$mainPlot <- renderPlot({
    req(input$catSelect)
    
    # Get time series and create category threshold values
    df_climatology <- df_detect()$climatology |> 
      mutate(diff = thresh-seas,
             thresh_2x = thresh+diff,
             thresh_3x = thresh+diff*2,
             thresh_4x = thresh+diff*3)
    
    # Get events
    df_event <- df_detect()$event
    
    # Get top event
    df_event_top <- df_event |> 
      filter(intensity_cumulative == max(intensity_cumulative))
    if(nrow(df_event_top) > 1) df_event_top <- df_event_top[1,]
    
    # Get plotting parameters
    te_min <- df_event_top$date_start
    te_peak <- df_event_top$date_peak
    te_max <- df_event_top$date_end
    ts_min <- df_event_top$date_peak-182
    ts_max <- df_event_top$date_peak+182
    
    # Catch edge cases for very long events
    while(ts_min > te_min){
      ts_min <- ts_min-100
    }
    while(ts_max < te_max){
      ts_max <- ts_max+100
    }
    
    # Subset time series for faster plotting
    df_clim_sub <- df_climatology |> 
      filter(t >= ts_min, t <= ts_max)
    df_clim_top <- df_climatology |> 
      filter(t >= te_min-1, t <= te_max+1)
    
    # More plotting parameters
    temp_min <- df_clim_sub$temp[df_clim_sub$t == te_min]
    temp_max <- df_clim_sub$temp[df_clim_sub$t == te_max]
    temp_mean <- mean(temp_min, temp_max)
    seas_peak <- df_clim_sub$seas[df_clim_sub$t == te_peak]
    temp_peak <- df_clim_sub$temp[df_clim_sub$t == te_peak]
    thresh_peak <- df_clim_sub$thresh[df_clim_sub$t == te_peak]
    thresh2x_peak <- df_clim_sub$thresh_2x[df_clim_sub$t == te_peak]
    thresh3x_peak <- df_clim_sub$thresh_3x[df_clim_sub$t == te_peak]
    thresh4x_peak <- df_clim_sub$thresh_4x[df_clim_sub$t == te_peak]
    
    # Base annotated flame
    mainPlot <- ggplot(data = df_clim_sub, aes(x = t, y = temp)) +
      scale_x_date(expand = c(0, 0)) +
      labs(x = NULL, y = "Temperature [°C]") + theme_bw() +
      theme(text = element_text(size = global_text))
    
    # Change based on category selection
    if(input$catSelect == "No"){
      mainPlot <- mainPlot +
        geom_flame(aes(y2 = thresh, fill = "other MHWs"), n = 5, n_gap = 2) +
        geom_flame(data = df_clim_top, aes(y2 = thresh, fill = "focal MHW"), n = 5, n_gap = 2) +
        geom_line(aes(y = seas, colour = "seas"), linewidth = 1) +
        geom_line(aes(y = thresh, colour = "thresh"), linewidth = 1) +
        geom_line(aes(y = temp, colour = "temp"), linewidth = 0.6) +
        # Cumulative intensity line
        geom_ribbon_pattern(data = df_clim_top, aes(ymin = seas, ymax = temp), 
                            pattern_fill = "skyblue",
                            pattern = 'stripe', fill = NA, colour  = 'black', alpha = 0.3) +
        # Duration line
        geom_line(data = filter(df_clim_sub, t >= te_min-1, t <= te_max+1),
                  colour = "slateblue", aes(y = thresh), linewidth = 2) +
        # Max intensity line
        geom_segment(colour = "navy", linewidth = 2, 
                     arrow = arrow(ends = "both", type = "closed"),
                     aes(x = te_peak, xend = te_peak, y = seas_peak, yend = temp_peak)) +
        # Duration label
        geom_label(data = data.frame(),colour = "slateblue", label.size = 3, hjust = 0, size = 6,
                   aes(label = paste0("Duration = ",df_event_top$duration," days"), 
                       x = te_max, y = temp_max)) +
        geom_label(data = data.frame(), colour = "black", label.size = 0, hjust = 0, size = 6,
                   aes(label = paste0("Duration = ",df_event_top$duration," days"),
                       x = te_max, y = temp_max)) +
        # Max intensity label
        geom_label(data = data.frame(), colour = "navy", label.size = 3, size = 6,
                   aes(label = paste0("Max. Intensity = ",round(df_event_top$intensity_max, 2),"°C"), 
                       x = te_peak, y = temp_peak*1.01)) +
        geom_label(data = data.frame(), colour = "black", label.size = 0, size = 6,
                   aes(label = paste0("Max. Intensity = ",round(df_event_top$intensity_max, 2),"°C"), 
                       x = te_peak, y = temp_peak*1.01)) +
        # Cumulative intensity label
        geom_label(data = data.frame(), colour = "skyblue", label.size = 3, size = 6,
                   aes(label = paste0("Cum. Intensity = ",round(df_event_top$intensity_cumulative, 2),"°CxDays"), 
                       x = te_max, y = mean(c(temp_max, temp_peak)))) +
        geom_label(data = data.frame(), colour = "black", label.size = 0, size = 6,
                   aes(label = paste0("Cum. Intensity = ",round(df_event_top$intensity_cumulative, 2),"°CxDays"), 
                       x = te_max, y = mean(c(temp_max, temp_peak)))) +
        # Colour palettes
        scale_colour_manual(name = "Values",
                            values = c("temp" = "black", 
                                       "seas" = "darkblue",
                                       "thresh" =  "purple"),
                            breaks = c("temp", "seas", "thresh")) +
        scale_fill_manual(name = "Events", 
                          values = c("focal MHW" = "red",
                                     "other MHWs" = "salmon"),
                          breaks = c("focal MHW", "other MHWs")) +
        guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))
    } else if(input$catSelect == "Yes"){
      mainPlot <- mainPlot +
        geom_flame(aes(y2 = thresh, fill = "I Moderate")) +
        geom_flame(aes(y2 = thresh_2x, fill = "II Strong")) +
        geom_flame(aes(y2 = thresh_3x, fill = "III Severe")) +
        geom_flame(aes(y2 = thresh_4x, fill = "IV Extreme")) +
        geom_line(aes(y = thresh_2x, col = "2x thresh"), linewidth = 0.7, linetype = "dashed") +
        geom_line(aes(y = thresh_3x, col = "3x thresh"), linewidth = 0.7, linetype = "dotdash") +
        geom_line(aes(y = thresh_4x, col = "4x thresh"), linewidth = 0.7, linetype = "dotted") +
        geom_line(aes(y = seas, col = "seas"), linewidth = 0.7) +
        geom_line(aes(y = thresh, col = "thresh"), linewidth = 0.7) +
        geom_line(aes(y = temp, col = "temp"), linewidth = 0.6) +
        scale_colour_manual(name = "Values", 
                            values = c("temp" = "black",
                                       "seas" = "darkblue",
                                       "thresh" = "purple",
                                       "2x thresh" = "purple",
                                       "3x thresh" = "purple",
                                       "4x thresh" = "purple"),
                            breaks = c("temp", "seas", "thresh",
                                       "2x thresh", "3x thresh", "4x thresh")) +
        scale_fill_manual(name = "Categories", 
                          values = MHW_colours) +
        guides(colour = guide_legend(order = 1, 
                                     override.aes = list(linetype = c("solid", "solid", "solid",
                                                                      "dashed", "dotdash", "dotted"))),
               fill = guide_legend(order = 2))
      
      # Add category percent labels as necessary
      if("I Moderate" %in% unique(df_clim_top$category)){
        mainPlot <- mainPlot +
          geom_label(data = data.frame(), colour = "#ffc866", label.size = 3, size = 6,
                     aes(label = paste0("I Moderate = ", df_event_top$p_moderate,"%"), 
                         x = te_peak, y = thresh_peak)) +
          geom_label(data = data.frame(), colour = "black", label.size = 0, size = 6,
                     aes(label = paste0("I Moderate = ", df_event_top$p_moderate,"%"), 
                         x = te_peak, y = thresh_peak))
      }
      if("II Strong" %in% unique(df_clim_top$category)){
        mainPlot <- mainPlot +
          geom_label(data = data.frame(), colour = "#ff6900", label.size = 3, size = 6,
                     aes(label = paste0("II Strong = ", df_event_top$p_strong,"%"), 
                         x = te_peak, y = thresh2x_peak)) +
          geom_label(data = data.frame(), colour = "black", label.size = 0, size = 6,
                     aes(label = paste0("II Strong = ", df_event_top$p_strong,"%"), 
                         x = te_peak, y = thresh2x_peak))
      }
      if("III Severe" %in% unique(df_clim_top$category)){
        mainPlot <- mainPlot +
          geom_label(data = data.frame(), colour = "#9e0000", label.size = 3, size = 6,
                     aes(label = paste0("III Severe = ", df_event_top$p_severe,"%"), 
                         x = te_peak, y = thresh3x_peak)) +
          geom_label(data = data.frame(), colour = "black", label.size = 0, size = 6,
                     aes(label = paste0("III Severe = ", df_event_top$p_severe,"%"), 
                         x = te_peak, y = thresh3x_peak))
      }
      if("IV Extreme" %in% unique(df_clim_top$category)){
        mainPlot <- mainPlot +
          geom_label(data = data.frame(), colour = "#2d0000", label.size = 3, size = 6,
                     aes(label = paste0("IV Extreme = ", df_event_top$p_extreme,"%"), 
                         x = te_peak, y = thresh4x_peak)) +
          geom_label(data = data.frame(), colour = "black", label.size = 0, size = 6,
                     aes(label = paste0("IV Extreme = ", df_event_top$p_extreme,"%"), 
                         x = te_peak, y = thresh4x_peak))
      }
    }
    
    # Exit
    mainPlot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
