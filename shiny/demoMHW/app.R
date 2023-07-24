# shiny/demoMHW/app.R
# An interactive tool to explain how MHWs are defined

# TODO: Add quarters labels (seasons) to the DOY panel in Detection.
# Consider having both DOY detection panels next to each other, with and without trend
# Perhaps using a sliding update window widget
# Allow a user to upload a dataset


# Setup -------------------------------------------------------------------

# library(shiny)
library(shinycssloaders)
library(bslib)
library(bsicons)
library(DT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpattern)
library(plotly)
library(heatwaveR)

# Enable thematic 
thematic::thematic_shiny(font = "auto")

# Set global ggplot2 theme
theme_set(theme_bw(base_size = 16))

# Light and dark themes
# https://rstudio.github.io/bslib/articles/theming.html
# light <- bs_theme()
# dark <- bs_theme(bg = "black", fg = "white", primary = "purple")

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

# Functions
source("functions.R", local = TRUE)


# Data --------------------------------------------------------------------

# Explicitly load TS into memory
sst_WA <- heatwaveR::sst_WA
sst_NW_Atl <- heatwaveR::sst_NW_Atl
sst_Med <- heatwaveR::sst_Med


# Inputs ------------------------------------------------------------------

## Time series controls
ts_input <- list(
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
)

## Statistics controls
stats_input <- list(
  # Select baseline period
  shiny::sliderInput("baseSelect", "Baseline", min = 1982, max = 2022, 
                     value = c(1982, 2011), sep = ""),
  # Select quantile
  shiny::numericInput("percSelect", "Percentile",
                      value = 90, min = 1, max = 100),
  # De-trend
  shiny::radioButtons("trendSelect", "Remove trend", inline = TRUE,
                      choices = c("No", "Yes"), selected = "No")
)

## Detection controls
detect_input <- list(
  # Set max gap between MHW
  shiny::numericInput("minDuration", "Min. duration",
                      value = 5, min = 1, max = 366),
  # Set minimum MHW duration
  shiny::numericInput("maxGap", "Max gap",
                      value = 2, min = 0, max = 366),
  # Show categories
  shiny::radioButtons("catSelect", "Categories", inline = TRUE,
                      choices = c("No", "Yes"), selected = "No"))


# Cards -------------------------------------------------------------------

## The main content of the app
cards <- list(
  page_fillable(
    # full_screen = FALSE,
    # card_header("Welcome"),
    # h1(tags$b("Overview")),
    h2(tags$b("Welcome")),
    img(src = "MHW_def.png", width = "600px", 
        style = "float:left; margin-right: 20px; margin-top: 20px;"),
    p("This demo was designed to provide a visual and interactive explanation for how one may detect
                                 a marine heatwave (MHW) using the Hobday et al. (2016, 2018) definition."),
    h2(tags$b("References")),
    p("Hobday et al. (2016, 2018) etc.")
  ),
  navset_card_pill(
    # full_screen = FALSE, 
    selected = "Day",
    # title = "Time series",
    sidebar = sidebar(ts_input[[2]], position = "right", open = FALSE),
    # p("Once upon a time series...")
    nav_panel("All", withSpinner(plotOutput("allTime"), type = 6, color = "#b0b7be")),
    nav_panel("Month", withSpinner(plotOutput("monthTime"), type = 6, color = "#b0b7be")),
    nav_panel("Year", withSpinner(plotOutput("yearTime"), type = 6, color = "#b0b7be")),
    nav_panel("DOY", withSpinner(plotOutput("doyTime"), type = 6, color = "#b0b7be")),
    nav_panel("Day", withSpinner(plotOutput("dayTime"), type = 6, color = "#b0b7be"))
  ),
  card(
    full_screen = FALSE,
    style = "resize:vertical;",
    # card_header("Statistics"),
    # p("Lies, damn lies, and statistics...")
    card_body(
      accordion(
        accordion_panel(title = "Baseline",
                        withSpinner(plotOutput("basePlot"), type = 6, color = "#b0b7be")
        )
      ),
      accordion(
        accordion_panel(title = "Threshold (percentile)",
                        withSpinner(plotOutput("percPlot"), type = 6, color = "#b0b7be")
        )
      )
    )
  ),
  navset_card_tab(
    full_screen = FALSE,
    # title = "Detection",
    nav_panel("Table", withSpinner(DT::DTOutput("detectTable"), type = 6, color = "#b0b7be")),
    nav_panel("Lolli", withSpinner(plotlyOutput("lolliPlot"), type = 6, color = "#b0b7be"))#,
    # nav_panel("Flame", withSpinner(plotlyOutput("flamePlot"), type = 6, color = "#b0b7be"))
  ),
  card(
    full_screen = FALSE,
    withSpinner(plotOutput("mainPlot"), type = 6, color = "#b0b7be")
  )
)


# UI ----------------------------------------------------------------------

# Define UI
ui <- page_navbar(
  
  title = "MHW definition",
  
  # Bootstrap version used during development
  theme = bs_theme(version = 5, bootswatch = "minty"),

  
  ## Sidebar -----------------------------------------------------------------
  
  sidebar = bslib::sidebar(title = "Controls",
                             accordion(open = FALSE,
                               accordion_panel("Time series", ts_input[[1]])
                             ),
                             accordion(open = FALSE,
                               accordion_panel("Statistics", stats_input)
                             ),
                             accordion(open = FALSE,
                               accordion_panel("Detection", detect_input)
                             )
                           
  ), 
  
  
  ## Body --------------------------------------------------------------------
 
  nav_panel("Overview", cards[[1]]),
  nav_panel("Time series", cards[[2]]),
  nav_panel("Statistics", cards[[3]]),
  nav_panel("Detection", cards[[4]]),
  nav_panel("The main event", cards[[5]])
  )


# Server ------------------------------------------------------------------

# Server
server <- function(input, output, session) {
  
  # Demo of available themes
  # bs_themer()
  # Decent themes:
  # journal, litera, minty, morph, spacelab, united, yeti, zephyr
  
  
  ## Reactive UI -------------------------------------------------------------
  
  
  ## Reactive data -----------------------------------------------------------
  
  # Base time series and de-trending anomalies
  df_site_ts <- reactive({
    req(input$seriesSelect)
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
  # TODO: Add switch that hides option to remove linear trend
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
      geom_line(data = df_site_base, aes(colour = "seas"), linewidth = 0.4, alpha = 0.4) +
      geom_hline(aes(yintercept = mean(temp), colour = "mean"), 
                 linetype = "dashed", linewidth = 2) +
      geom_smooth(aes(colour = "trend"),
                  method = "lm", formula = "y ~ x", se = FALSE, 
                  linetype = "dashed", linewidth = 2) +
      geom_vline(aes(xintercept = min(df_site_base$t), colour = "baseline"), 
                 linetype = "solid", linewidth = 2) +
      geom_vline(aes(xintercept = max(df_site_base$t), colour = "baseline"), 
                 linetype = "solid", linewidth = 2) +
      geom_rug(data = df_site_base, sides = "b", aes(colour = "baseline")) +
      geom_point(data = df_thresh_base, aes(colour = "thresh")) +
      scale_x_date(expand = c(0, 0)) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "baseline" = "khaki",
                                     "seas" = "darkblue",
                                     "thresh" =  "purple",
                                     "mean" = "darkgreen",
                                     "trend" = "tomato"),
                          breaks = c("temp", "baseline", "seas", "thresh", "mean", "trend")) +
      guides(colour = guide_legend(override.aes = list(shape = 15, size = 5))) +
      labs(x = NULL, y = "Temperature [°C]") + theme_bw() + 
      theme(legend.position = "bottom")
    
    # Exit
    basePlot
  })
  
  # Plot that shows effect of percentile selection
  output$percPlot <- renderPlot({
    req(input$percSelect)
    
    # Run ts2clm
    df_ts2clm <- df_ts2clm()
    
    # Filter by baseline
    df_site_base <- df_ts2clm |> 
      filter(t >= paste0(input$baseSelect[1],"-01-01"),
             t <= paste0(input$baseSelect[2],"-12-31"))
    
    # Points above threshold
    df_thresh <- df_site_base |> filter(temp >= thresh)
    
    # Get de-trend info
    if(input$trendSelect == "No"){
      detrend <- "mean"
    } else if(input$trendSelect == "Yes"){
      detrend <- "trend"
    }
    
    # Plot
    percPlot <- ggplot(data = df_site_base, aes(x = doy, y = temp)) +
      geom_point(aes(y = temp, colour = "temp")) +
      geom_point(data = df_thresh, aes(colour = "thresh")) +
      geom_line(aes(y = thresh, colour = "thresh"), linewidth = 4) +
      geom_line(aes(y = seas, colour = "seas"), linewidth = 4) +
      geom_hline(aes(yintercept = 0, colour = {{ detrend }}), linetype = "dashed", linewidth = 4) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "thresh" =  "purple", 
                                     "seas" = "darkblue",
                                     "mean" = "darkgreen",
                                     "trend" = "tomato")) +
      labs(x = NULL, y = "Temp. anomaly [°C]") + theme_bw() + 
      theme(legend.position = "none")
    
    # Exit
    percPlot
  })
  
  # Show time series with events as flames
  output$flamePlot <- plotly::renderPlotly({
    req(input$minDuration, input$maxGap)
    
    # Get time series
    df_climatology <- df_detect()$climatology
    
    # Plot
    flamePlot <- ggplot(data = df_climatology, aes(x = t, y = temp)) +
      heatwaveR::geom_flame(aes(y2 = thresh, fill = "MHW"), 
                            # For testing...
                            # n = 5, n_gap = 2) +
                            n = input$minDuration, n_gap = input$maxGap) +
      geom_line(aes(y = temp, colour = "temp"), linewidth = 0.6) +
      geom_line(aes(y = thresh, colour = "thresh"), linewidth = 1.2, alpha = 0.2) +
      geom_line(aes(y = seas, colour = "seas"), linewidth = 1.2, alpha = 0.2) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "thresh" =  "purple", 
                                     "seas" = "darkblue")) +
      scale_fill_manual(name = "Events",
                        values = c("MHW" = "salmon")) +
      scale_x_date(expand = c(0, 0)) +
      # guides(colour = guide_legend(override.aes = list(fill = NA))) +
      labs(y = "Temperature [°C]", x = NULL) + theme_bw() +
      theme(legend.position = "bottom")
    # flamePlot
    
    # Exit
    p <- plotly::ggplotly(flamePlot, tooltip = "text", dynamicTicks = F) |> plotly::layout(hovermode = 'compare')
    p
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
      labs(x = NULL, y = "Temperature [°C]") + theme_bw()
    
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

