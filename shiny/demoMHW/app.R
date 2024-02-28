# shiny/demoMHW/app.R
# An interactive tool to explain how MHWs are defined

# TODO: Add quarters labels (seasons) to the DOY panel in Detection.
# Consider having both DOY detection panels next to each other, with and without trend
# Perhaps using a sliding update window widget


# Setup -------------------------------------------------------------------

# tester...
# library(shiny)
# setwd("shiny/demoMHW/")

# Libraries
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
library(cicerone)

# Enable thematic 
thematic::thematic_shiny(font = "auto")

# Set global ggplot2 theme
# Unexpected behaviour
# theme_set(theme_bw(base_size = 16))

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
sst_Kong <- readr::read_csv("sst_kong.csv")


# Inputs ------------------------------------------------------------------

## Time series controls
ts_input <- list(
  # Select time series
  selectizeInput(
    inputId = "seriesSelect",
    label = "Select",
    choices = c("Western Australia", "Mediterranean", "NW Atlantic", "Kongsfjorden"),
    multiple = FALSE,
    selected = "Western Australia",
    options = list(
      options = list(
        list(hemi = "N", value = "NW Atlantic", name = "NW Atlantic"),
        list(hemi = "N", value = "Mediterranean", name = "Mediterranean"),
        list(hemi = "N", value = "Kongsfjorden", name = "Kongsfjorden"),
        list(hemi = "S", value = "Western Australia", name = "Western Australia")
      ),
      optgroups = list(
        list(value = "N", label = "Northern hemisphere"),
        list(value = "S", label = "Southern hemisphere")
      ),
      optgroupField = "hemi",
      labelField = "name"
    )
  ),
  
  # Time series upload button
  fileInput("seriesUpload", "Upload",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"),
            placeholder = ".csv"),
  
  
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
  page_fluid(
    
    img(src = "MHW_def.png", width = "400px", 
        style = "float:left; margin-right: 20px; margin-top: 20px;"),
    
    h2(tags$b("Welcome")),
    p("This demo was designed to provide a visual and interactive explanation for how one may detect
      a marine heatwave (MHW) using the Hobday et al. (2016, 2018) definition. The demo is laid out via different navigation 
      tabs at the top of the page. These represent the three overarching steps we follow to arrive at a MHW definition. 
      On the left of this demo is a control panel. Each dropdown control menu is named according to the navigation tab that 
      it will effect. We start by selecting one of three time series. Then we move on to see how different statistics 
      (e.g. baselines and percentiles) affect climatologies and thresholds. Finally we decide how long a marine heatwave should
      be and detect all of the events in our time series that meet our criteria. The final panel 'The main event' showcases
      the results for the largest event detected in the time series. Please see the glossary below for the definitions of the 
      values found throughout the legends in this demo."),
    p(tags$b("NB:")," this demo is under active development. Please send any feedback to: robert.schlegel@imev-mer.fr"),
    p("Click here for a ", shinyWidgets::actionBttn("guidedTour", "Guided Tour", icon = bs_icon("map"), style = "jelly")),
    
    h2(tags$b("Glossary")),
    p(tags$span(style = "color:black; font-size:25px; font-weight:bold;", "temp"),
      ": The temperature values (°C) of a given time series"),
    p(tags$span(style = "color:darkgreen; font-size:25px; font-weight:bold;", "mean"),
      ": The overall mean value in a time series. Determined by averaging all daily data together to find a single value.
      This is used to determine the temperature anomalies in a time series, which may be used in place of the normal 
      temperatures depending on a given investigation. Note that this is not standard."),
    p(tags$span(style = "color:tomato; font-size:25px; font-weight:bold;", "trend"),
      ": The linear trend present in a time series. One may use this value instead of the mean to create de-trended
      daily anomaly values for further MHW investigations. Note that this is not standard."),
    p(tags$span(style = "color:goldenrod; font-size:25px; font-weight:bold;", "baseline"),
      ": The period of time from which data are taken in order to calculate the seasonally varying climatology and threshold
      used in the detection of MHWs."),
    p(tags$span(style = "color:darkblue; font-size:25px; font-weight:bold;", "seas"),
      ": The seasonally varying climatology (i.e. seasonal mean value). Typically this is calculated by taking all data 
      within the baseline, grouping them by their day-of-year (DOY) and averaging+smoothing them with a rolling mean. 
      The default rolling mean is a 5-day double-sided window (11 day total width), and the second smoothing pass is a 
      15-day double-sided window (31 day total width)."),
    p(tags$span(style = "color:purple; font-size:25px; font-weight:bold;", "thresh"),
      ": The seasonally varying threshold. using the data within the baseline, the 90th percentile (default) is applied to find
      the DOY value above which daily temperatures must exceed for a MHW to be detected."),
    p(tags$span(style = "color:black; font-size:25px; font-weight:bold;", "min. duration"),
      ": The minimum duration (days) that temperatures must consistently be above the threshold value before a MHW is detected."),
    p(tags$span(style = "color:black; font-size:25px; font-weight:bold;", "max gap"),
      ": Once a MHW has been detected, temperatures are allowed to dip below the threshold for this many days (default is 2) 
      before a new event is detected. I.e. if an event is going for 7 days, then drops below the threshold for 2 days, but 
      goes up again for 5 more days, this will be counted as a 14 day event. However, if that dip lasts for 3 days, it will 
      be counted as two separate events."),
    p(tags$span(style = "color:salmon; font-size:25px; font-weight:bold;", "MHW"),
      ": An extreme event, a marine heatwave (MHW) is detected (by default) when daily temperatures are in exceedance of the 90th
      percentile threshold for 5+ days. There are many options that can be changed to alter the detection of events, 
      as documented above."), 
    p(tags$span(style = "color:red; font-size:25px; font-weight:bold;", "focal MHW"),
      ": When plotting one MHW in particular, it tends to be shown in red when other smaller events are
      also visible in the time series."),
    p(tags$span(style = "color:slateblue; font-size:25px; font-weight:bold;", "duration"),
      ": The length of an MHW (days). Measured as the distance from the start date to the end date."),
    p(tags$span(style = "color:navy; font-size:25px; font-weight:bold;", "max. intensity"),
      ": The maximum temperature anomaly (°C) detected during a given MHW. This is measured as the distance from the 
      seasonal climatology to the observed temperature. Note, it is a common mistake to think that this value is measured from
      the threshold value to the observed temperaature. See the plot in 'The main event' tab for a visual explanation."),
    p(tags$span(style = "color:skyblue; font-size:25px; font-weight:bold;", "cum. intensity"),
      ": The sum of all temperature anomalies during an event (°C x days). Note that the temperature anomalies are the 
      distance from the seasonal climatology to the observed temperature."),
    p(tags$span(style = "color:#ffc866; font-size:25px; font-weight:bold;", "I Moderate"),
      ": The least intense category of MHW. If the max. intensity of an event is not more than double the distance from the 
      seasonal climatology to the 90th percentile threshold, the event is classified as 'I moderate'
      For example, let's say that the seasonal climatology on the warmest day of a MHW happens to be 16°C, 
      and the 90th percentile threshold is 18°C, this means that as long as the max. intensity is below 20°C it is considered
      to be simply a category 1 event (I Moderate). Generally speaking these have not been associated with impactful
      events in the literature."),
    p(tags$span(style = "color:#ff6900; font-size:25px; font-weight:bold;", "II Strong"),
      ": An event when the max. intensity is double, but not triple the distance from the seasonal climatology to the
      90th percentile threshold. Using our example above, this would mean the max. intensity did not exceed 22°C.
      Events of this magnitude tend not to be associated with long-lasting ecological impacts, but when they occur during the 
      spring or Autumn they can have significant impacts on the phenology of local species reproduction."),
    p(tags$span(style = "color:#9e0000; font-size:25px; font-weight:bold;", "III Severe"),
      ": The next step up. These events are often associated with mass mortality of local species."),
    p(tags$span(style = "color:#2d0000; font-size:25px; font-weight:bold;", "IV Extreme"),
      ": One final step higher. The (currently) highest category value. These events have been known to crash local ecosystems.
      Removing the established ecosystem in favour of a warmer neighbouring system. It is in this way that climate change is
      re-writing the ecology of coastlines more rapidly than projected."),
    
    h2(tags$b("Acknowledgement")),
    p("The development of this application was supported by FACE-IT 
      (The Future of Arctic Coastal Ecosystems – Identifying Transitions in Fjord Systems and Adjacent Coastal Areas). 
      FACE-IT has received funding from the European Union’s Horizon 2020 research and innovation programme under 
      grant agreement No 869154."),
    img(src = "FACE-IT_Logo_900.png", width = "350px", style = "margin-bottom: 20px;"), 
    img(src = "h2020_grant.png", width = "450px", style = "margin-bottom: 20px; margin-top: 30px;"),

    h2(tags$b("References")),
    p("Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C., ... & Wernberg, T. 
      (2016). A hierarchical approach to defining marine heatwaves. Progress in Oceanography, 141, 227-238."),
    p("Hobday, A. J., Oliver, E. C., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., ... & Smale, D. A. 
      (2018). Categorizing and naming marine heatwaves. Oceanography, 31(2), 162-173.")
  ),
  navset_card_pill(
    id = "ts_cards",
    selected = "Day",
    sidebar = sidebar(ts_input[[3]], position = "right", open = FALSE),
    nav_panel("All", withSpinner(plotOutput("allTime", height = "700px"), type = 6, color = "#b0b7be")),
    nav_panel("Month", withSpinner(plotOutput("monthTime", height = "700px"), type = 6, color = "#b0b7be")),
    nav_panel("Year", withSpinner(plotOutput("yearTime", height = "700px"), type = 6, color = "#b0b7be")),
    nav_panel("DOY", withSpinner(plotOutput("doyTime", height = "700px"), type = 6, color = "#b0b7be")),
    nav_panel("Day", withSpinner(plotOutput("dayTime", height = "700px"), type = 6, color = "#b0b7be"))
  ),
  card(
    full_screen = FALSE,
    card_body(
      accordion(
        accordion_panel(title = "Climatology (baseline)",
                        withSpinner(plotOutput("basePlot", height = "400px"), type = 6, color = "#b0b7be")
        )
      ),
      accordion(
        accordion_panel(title = "Threshold (percentile)",
                        withSpinner(plotOutput("percPlot", height = "400px"), type = 6, color = "#b0b7be")
        )
      )
    )
  ),
  navset_card_tab(
    full_screen = FALSE,
    nav_panel("Table", withSpinner(DT::DTOutput("detectTable", height = "700px"), type = 6, color = "#b0b7be")),
    nav_panel("Lolli", withSpinner(plotlyOutput("lolliPlot", height = "700px"), type = 6, color = "#b0b7be"))
  ),
  card(
    full_screen = FALSE,
    withSpinner(plotOutput("mainPlot", height = "700px"), type = 6, color = "#b0b7be")
  )
)


# UI ----------------------------------------------------------------------

# Define UI
ui <- page_navbar(
  
  title = "MHW definition", 
  header = list(use_cicerone()),
  id = "nav_bar",
  
  # Bootstrap version used during development
  theme = bs_theme(version = 5, bootswatch = "minty"),

  # Colour baseline slider
  shinyWidgets::setSliderColor(color = "goldenrod", sliderId = 1),
  
  
  ## Sidebar -----------------------------------------------------------------
  
  sidebar = bslib::sidebar(title = "Controls", id = "control_panel",
                           accordion(open = FALSE,
                                     accordion_panel("Time series", ts_input[[1]], 
                                                     p(tags$b("OR")),
                                                     ts_input[[2]])
                           ),
                           accordion(open = FALSE,
                                     accordion_panel("Statistics", stats_input)
                           ),
                           accordion(open = FALSE,
                                     accordion_panel("Detection", detect_input)
                           )
                           
  ), 
  
  
  ## Body --------------------------------------------------------------------
 
  nav_panel("Overview", value = "overview_tab", cards[[1]]),
  nav_panel("Time series", value = "ts_tab", cards[[2]]),
  nav_panel("Statistics", value = "stats_tab", cards[[3]]),
  nav_panel("Detection", value = "detect_tab", cards[[4]]),
  nav_panel("The main event", value = "event_tab", cards[[5]])
  
)


# Server ------------------------------------------------------------------

# Server
server <- function(input, output, session) {
  
  # Demo of available themes
  # bs_themer()
  # Decent themes:
  # journal, litera, minty, morph, spacelab, united, yeti, zephyr
  
  
  ## Reactive UI -------------------------------------------------------------

  observeEvent(input$guidedTour, {
      guide$init()$start()
  })
  
  
  ## Reactive data -----------------------------------------------------------
  
  # Base reactive for inputs and uploads
  df_reactive <- reactiveValues(ts = sst_WA)
  
  # Pre-loaded time series
  df_preload <- reactive({
    req(input$seriesSelect)
    if(input$seriesSelect == "Western Australia"){
      df_preload <- sst_WA
    } else if(input$seriesSelect == "NW Atlantic"){
      df_preload <- sst_NW_Atl
    } else if(input$seriesSelect == "Mediterranean"){
      df_preload <- sst_Med
    } else if(input$seriesSelect == "Kongsfjorden"){
      df_preload <- sst_Kong
    }
    return(df_preload)
  })
  
  # Uploaded time series
  df_upload <- reactive({
    req(input$seriesUpload)
    df_upload <- readr::read_csv(input$seriesUpload$datapath[1])
    return(df_upload)
  })
  
  # Observe selecting
  observeEvent(input$seriesSelect, {
    df_reactive$ts <- df_preload()
  })
  
  # Observe uploading
  observeEvent(input$seriesUpload, {
    df_reactive$ts <- df_upload()
  })
  
  # Run ts2clm
  df_ts2clm <- reactive({
    req(input$baseSelect, input$percSelect, input$trendSelect)
    df_site_ts <- df_reactive$ts
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
    time_plot("All", df_reactive$ts, input$timeSelect)
  })
  
  # Month time plot
  output$monthTime <- renderPlot({
    time_plot("Month", df_reactive$ts, input$timeSelect)
  })
  
  # Year time plot
  output$yearTime <- renderPlot({
    time_plot("Year", df_reactive$ts, input$timeSelect)
  })
  
  # DOY time plot
  output$doyTime <- renderPlot({
    time_plot("DOY", df_reactive$ts, input$timeSelect)
  })
  
  # Day time plot
  output$dayTime <- renderPlot({
    time_plot("Day", df_reactive$ts, input$timeSelect)
  })
  
  # Plot that illustrates baseline selection
  output$basePlot <- renderPlot({
    req(input$baseSelect)
    
    # Get base time series
    df_site_ts <- df_reactive$ts
    
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
                 linetype = "dashed", linewidth = 3, alpha = 0.8) +
      geom_line(aes(colour = "trend"),
                stat  ="smooth", method = "lm", formula = "y ~ x",
                linetype = "dashed", linewidth = 3, alpha = 0.8) +
      geom_vline(aes(xintercept = min(df_site_base$t), colour = "baseline"), 
                 linetype = "solid", linewidth = 2) +
      geom_vline(aes(xintercept = max(df_site_base$t), colour = "baseline"), 
                 linetype = "solid", linewidth = 2) +
      geom_rug(data = df_site_base, sides = "b", aes(colour = "baseline")) +
      geom_point(data = df_thresh_base, aes(colour = "thresh")) +
      scale_x_date(expand = c(0, 0)) +
      scale_colour_manual(name = "Values",
                          values = c("temp" = "black", 
                                     "baseline" = "goldenrod",
                                     "seas" = "darkblue",
                                     "thresh" =  "purple",
                                     "mean" = "darkgreen",
                                     "trend" = "tomato"),
                          breaks = c("temp", "baseline", "seas", "thresh", "mean", "trend")) +
      guides(colour = guide_legend(override.aes = list(shape = 15, size = 5))) +
      labs(x = NULL, y = "Temperature [°C]") + theme_bw(base_size = 20) + 
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
      labs(x = NULL, y = "Temp. anomaly [°C]") + theme_bw(base_size = 20) + 
      theme(legend.position = "none")
    
    # Exit
    percPlot
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
      scale_x_date(limits = c(min(df_climatology$t), max(df_climatology$t)), expand = c(0, 0)) + 
      theme_bw(base_size = 30)
    
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
    ts_min <- df_event_top$date_peak-364
    ts_max <- df_event_top$date_peak+364
    
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
      labs(x = NULL, y = "Temperature [°C]") + theme_bw(base_size = 30)
    
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
        guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 5)), 
               fill = guide_legend(order = 2))
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
                                                                      "dashed", "dotdash", "dotted"),
                                                         linewidth = c(5, 5, 5, 0.8, 0.8, 0.8))),
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

