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

# Function for ensuring 366 DOY for non-leap years
leap_every_year <- function(x) {
    ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
}


# Data --------------------------------------------------------------------

# Explicitly load TS into memory
sst_WA <- heatwaveR::sst_WA
sst_NW_Atl <- heatwaveR::sst_NW_Atl
sst_Med <- heatwaveR::sst_Med


# UI ----------------------------------------------------------------------

# Define UI
ui <- navbarPage(title = "test",
    
    # The app title
    # TODO: Add messages etc
    # https://rstudio.github.io/shinydashboard/structure.html
    header = dashboardHeader(title = "MHW demo"),
    
    # The primary options
    sidebar = dashboardSidebar(
        
        # The side bar
        sidebarMenu(id = "mainMenu",
                    
                    # The various menus
                    menuItem("Overview", icon = icon("desktop")),
                    menuItem("Time series", tabName = "time", icon = icon("clock"),
                             selectizeInput("seriesSelect", "Time series",
                                            choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                            selected = c("Western Australia"))),
                    menuItem("Statistics", tabName = "perc_base", icon = icon("percent"),
                             selectizeInput("seriesSelect2", "Time series",
                                            choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                            selected = c("Western Australia"))),
                    menuItem("Detection", tabName = "detect", icon = icon("magnifying-glass"), selected = TRUE),
                    menuItem("The main event", tabName = "event", icon = icon("temperature-high"))),
        
        # The reactive controls based on the primary option chosen
        # uiOutput(outputId = "sidebar_controls")),
        
        # Instructions popup
        br(),
        useSweetAlert(),
        actionBttn(
            inputId = "info",
            label = "Instructions",
            icon = icon("book"),
            style = "material-flat",
            color = "success"
        )
    ),
    
    # The dashboard
    body = dashboardBody(
        # tabItems(
        tabsetPanel(
            
            ## Overview menu -----------------------------------------------------------
            
            tabPanel(title = "Overview",
                     fluidPage(
                         column(12,
                                img(src = "MHW_def.png", width = "600px", style = "float:left; margin-right: 20px;"),
                                # h1(tags$b("Overview")),
                                h2(tags$b("Brief")),
                                p("This demo was designed to provide a visual and interactive explanation for how one may detect
                                 a marine heatwave (MHW) using the Hobday et al. (2016, 2018) definition."),
                                h2(tags$b("References")),
                                p("Hobday et al. (2016, 2018) etc.")
                         )
                     )
            ),
            
            
            ## Time menu ---------------------------------------------------------------
            
            # TODO:
            # Add radio buttons to show colour for dots by month, year, or both (maybe not both...)
            tabPanel(title = "time",
                     p("Once upon a time series..."),
                     fluidPage(
                         column(12,
                                # box(width = 3, title = "Controls", # height = "880px",
                                #     status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                #     
                                #     # Site select
                                #     # uiOutput("selectSeriesUI"),
                                #     selectizeInput("seriesSelect", "Time series",
                                #                    choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                #                    selected = c("Western Australia")),
                                #     
                                #     # Time select
                                #     selectizeInput("timeSelect", "Time period",
                                #                    choices = c("All", "Month", "Year", "DOY", "Day"),
                                #                    selected = c("Day")
                                #     )
                                # ),
                                
                                # Time series plot
                                box(width = 12, title = "Data", 
                                    status = "info", solidHeader = TRUE, collapsible = FALSE,
                                    plotOutput("timePlot")
                                )
                         )
                     )
            ),
            
            ## Perc+base menu ----------------------------------------------------------
            
            tabItem(tabName = "perc_base",
                    p("Lies, damn lies, and statistics..."),
                    fluidPage(
                        column(12,
                               # Controls
                               box(width = 3, title = "Controls",
                                   status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                   
                                   # Site select
                                   # uiOutput("selectSeriesUI"),
                                   # selectizeInput("seriesSelect2", "Time series",
                                   #                choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                   #                selected = c("Western Australia")),
                                   
                                   # Select baseline period
                                   shiny::sliderInput("baseSelect", "Baseline", min = 1982, max = 2022, 
                                                      value = c(1982, 2011), sep = ""
                                   ),
                                   
                                   # Select quantile
                                   shiny::numericInput("percSelect", "Percentile",
                                                       value = 90, min = 1, max = 100
                                   ),
                                   
                                   # Detrend - no - linear - non-linear
                                   shiny::radioButtons("trendSelect", "Remove trend", inline = TRUE,
                                                       choices = c("No", "Yes"), selected = "No"
                                   ),
                               ),
                               
                               # Figures
                               box(width = 9, title = "Data", 
                                   status = "info", solidHeader = TRUE, collapsible = FALSE,
                                   # Overview of time series - baseline + trend
                                   plotOutput("basePlot"),
                                   # DOY panel - percentiles + trend effect
                                   plotOutput("percPlot")
                               )
                               # TODO: Add table of MHW metrics
                        )
                    )                
            ),
            
            
            ## Detection menu ----------------------------------------------------------
            
            tabItem(tabName = "detect",
                    fluidPage(
                        column(12,
                               # Controls
                               box(width = 3, title = "Controls",
                                   status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                   # Site select
                                   # uiOutput("selectSeriesUI"),
                                   selectizeInput("seriesSelect3", "Time series",
                                                  choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                                  selected = c("Western Australia")),
                               ),
                               
                               # Figures
                               box(width = 9, title = "Data", 
                                   status = "info", solidHeader = TRUE, collapsible = FALSE,
                                   # Overview of time series - baseline + trend
                                   plotOutput("lolliPlot")
                               )
                               # TODO: Add table of MHW metrics)
                        )
                    )
            ),
            
            ## Main event menu ----------------------------------------------------------
            
            tabItem(tabName = "event",
                    fluidPage(
                        column(12,
                               # Controls
                               box(width = 3, title = "Controls",
                                   status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                   # Site select
                                   # uiOutput("selectSeriesUI"),
                                   selectizeInput("seriesSelect3", "Time series",
                                                  choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                                  selected = c("Western Australia")),
                               ),
                               
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
    
    # Observe tim series selections
    observe(shiny::updateSelectInput(session, "seriesSelect2", selected = input$seriesSelect))
    observe(shiny::updateSelectInput(session, "seriesSelect", selected = input$seriesSelect2))
    
    
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
            df_site_ts$temp <- pracma::detrend(df_site_ts$temp)[,1]
        }
        df_ts2clm <- ts2clm(df_site_ts, 
                            climatologyPeriod = c(paste0(input$baseSelect[1],"-01-01"),
                                                  paste0(input$baseSelect[2],"-12-31")),
                            pctile = input$percSelect,
                            # For testing...
                            # climatologyPeriod = c("1982-01-01", "2011-12-31"),
                            # pctile = 90
        )
        return(df_ts2clm)
    })
    
    # Run detect_event
    df_detect <- reactive({
        req(input$baseSelect, input$percSelect, input$trendSelect)
        df_detect <- detect_event()
    })
    
    ## Time plot ---------------------------------------------------------------
    
    # NB: Too many data points for plotly to run quickly
    output$timePlot <- renderPlot({
        
        # Get time series
        df_site_ts <- df_site_ts()
        
        # Create time columns
        df_site_ts <- df_site_ts |> 
            mutate(all = "All",
                   month = lubridate::month(t, label = TRUE),
                   year = lubridate::year(t),
                   doy = leap_every_year(t),
                   day = t)
        
        # Set t column to timeSelect
        if(input$timeSelect == "All"){
            df_site_ts$t <- df_site_ts$all
        } else if(input$timeSelect == "Month"){
            df_site_ts$t <- df_site_ts$month
        } else if(input$timeSelect == "Year"){
            df_site_ts$t <- df_site_ts$year
        } else if(input$timeSelect == "DOY"){
            df_site_ts$t <- df_site_ts$doy
        } else if(input$timeSelect == "Day"){
            df_site_ts$t <- df_site_ts$day
        }
        
        if(input$timeSelect == "DOY"){
            box_line_width = 1  
        } else {
            box_line_width = 3
        }
        
        # Plot
        timePlot <- ggplot(data = df_site_ts, aes(x = t, y = temp)) + geom_point(position = "jitter") +
            theme_bw() + labs(x = NULL, y = "Temperature [°C]")
        
        if(input$timeSelect != "Day"){
            timePlot <- timePlot + geom_boxplot(aes(group = t), linewidth = box_line_width,
                                                colour = "blue", alpha = 0.2, outlier.shape = NA)
        }
        
        # Exit
        timePlot
    })
    
    
    ## Baseline plot -----------------------------------------------------------
    
    output$basePlot <- renderPlot({
        req(input$baseSelect)
        
        # Get time series
        df_ts2clm <- df_ts2clm()
        
        # Filter by baseline
        df_site_base <- df_ts2clm |> 
            filter(t >= paste0(input$baseSelect[1],"-01-01"),
                   t <= paste0(input$baseSelect[2],"-12-31"))
        
        # Points above threshold
        df_thresh <- df_ts2clm |> filter(temp >= thresh)
        
        # Plot
        basePlot <- ggplot(data = df_ts2clm, aes(x = t, y = temp)) + geom_line() + 
            geom_line(data = df_site_base, colour = "darkblue", linewidth = 1.1, alpha = 0.6) +
            geom_vline(aes(xintercept = min(df_site_base$t)), 
                       linetype = "dashed", linewidth = 2, colour = "darkblue") +
            geom_vline(aes(xintercept = max(df_site_base$t)), 
                       linetype = "dashed", linewidth = 2, colour = "darkblue") +
            geom_rug(data = df_site_base, sides = "b", colour = "darkblue") +
            geom_point(data = df_thresh, colour = "purple") +
            scale_x_date(expand = c(0, 0)) +
            theme_bw() + labs(x = NULL, y = "Temperature [°C]")
        
        # Exit
        basePlot
    })
    
    ## Percentile plot ---------------------------------------------------------
    
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
            scale_colour_manual(name = "Values",
                                values = c("temp" = "black", 
                                           "thresh" =  "purple", 
                                           "seas" = "darkblue")) +
            scale_x_continuous(expand = c(0, 0)) +
            labs(x = NULL, y = "Temperature [°C]") +
            theme_bw() + theme(legend.position = "top")
        
        # Exit
        percPlot
    })
    
    
    ## Lolli plot --------------------------------------------------------------
    
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
    
    
    ## Main event plot ---------------------------------------------------------
    
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
