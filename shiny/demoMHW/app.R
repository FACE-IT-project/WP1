# shiny/demoMHW/app.R
# An interactive tool to explain how MHWs are defined

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
ui <- dashboardPage(
    
    # The app title
    dashboardHeader(title = "MHW demo"),
    
    # The primary options
    dashboardSidebar(
        
        # The side bar
        sidebarMenu(id = "mainMenu",
                    
                    # The various menus
                    menuItem("Overview", tabName = "overview", icon = icon("desktop")),
                    menuItem("Time series", tabName = "time", icon = icon("clock")),
                    menuItem("Statistics", tabName = "perc_base", icon = icon("percent"), selected = TRUE),
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
    dashboardBody(
        tabItems(
            
            
            ## Overview menu -----------------------------------------------------------
            
            tabItem(tabName = "overview",
                    fluidPage(
                        column(12,
                               h1(tags$b("Overview")),
                               img(src = "MHW_def.png", width = "400px", style = "float:left; margin-right: 20px;"),
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
            tabItem(tabName = "time",
                    p("Once upon a time series..."),
                    fluidPage(column(12,
                                     box(width = 3, title = "Controls", # height = "880px",
                                         status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                         
                                         # Site select
                                         selectizeInput("seriesSelect", "Time series",
                                                        choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                                        selected = c("Western Australia")
                                         ),
                                         
                                         # Time select
                                         selectizeInput("timeSelect", "Time period",
                                                        choices = c("All", "Month", "Year", "DOY", "Day"),
                                                        selected = c("Day")
                                         )
                                     ),
                                     
                                     # Time series plot
                                     box(width = 9, title = "Data", 
                                         status = "info", solidHeader = TRUE, collapsible = FALSE,
                                         plotOutput("timePlot")
                                     )
                    )
                    )
            ),
            
            ## Perc+base menu ----------------------------------------------------------
            
            tabItem(tabName = "perc_base",
                    p("Lies, damn lies, and statistics..."),
                    fluidPage(column(12,
                                     # Controls
                                     box(width = 3, title = "Controls",
                                         status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                         
                                         # Site select
                                         # Perhaps rather have this be determined by the first tab
                                         # selectizeInput("seriesSelect", "Time series",
                                         #                choices = c("Western Australia", "NW Atlantic", "Mediterranean"),
                                         #                selected = c("Western Australia")
                                         # ),
                                         
                                         # Select baseline period
                                         shiny::sliderInput("baseSelect", "Baseline", min = 1982, max = 2022, 
                                                            value = c(1982, 2011), sep = ""
                                         )#,
                                         
                                         # Select quantile
                                         
                                         # Detrend - no - linear - non-linear
                                     ),
                                     
                                     # Figures - show in panel of three
                                     # DOY panel - percentiles + trend effect
                                     # Table of MHW metrics
                                     box(width = 9, title = "Data", 
                                         status = "info", solidHeader = TRUE, collapsible = FALSE,
                                     # Overview of time series - baseline + trend
                                         plotOutput("basePlot"),
                                     
                                     )
                    )
                    )
                                     
                                     
                                    
            )#,
            
            
            ## Main event menu ----------------------------------------------------------
            
            # tabItem(tabName = "event",
            #         fluidPage(column(12,
            #                          img(src = "MHW_def.png", align = "center", width = "600px")))
            # ),
        )
    )
)


# Server ------------------------------------------------------------------

# Server
server <- function(input, output) {
    
    # Base time series
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
        # "All", "Month", "Year", "DOY", "Day"
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
        
        # Plot
        timePlot <- ggplot(data = df_site_ts, aes(x = t, y = temp)) + geom_point(position = "jitter") +
            theme_bw() + labs(x = NULL, y = "Temperature [°C]")
        
        if(input$timeSelect %in% c("All", "Month", "Year")){
            timePlot <- timePlot + geom_boxplot(aes(group = t), linewidth = 2,
                                                colour = "blue", alpha = 0.2, outlier.shape = NA)
        }
        
        # Exit
        timePlot
        # return(basePlot)
        # ggplotly(basePlot, tooltip = "text")
        
    })
    
    
    ## Baseline plot -----------------------------------------------------------
    
    output$basePlot <- renderPlot({
        req(input$baseSelect)
        
        # Get time series
        df_site_ts <- df_site_ts()
        
        # Filter TS by time
        df_site_base <- df_site_ts |> 
            filter(t >= paste0(input$baseSelect[1],"-01-01"),
                   t <= paste0(input$baseSelect[2],"-12-31"))
        
        # Plot
        basePlot <- ggplot(data = df_site_ts, aes(x = t, y = temp)) + geom_line() + 
            geom_line(data = df_site_base, colour = "darkblue", size = 1.1, alpha = 0.6) +
            geom_vline(aes(xintercept = min(df_site_base$t)), 
                       linetype = "dashed", linewidth = 2, colour = "darkblue") +
            geom_vline(aes(xintercept = max(df_site_base$t)), 
                       linetype = "dashed", linewidth = 2, colour = "darkblue") +
            geom_rug(data = df_site_base, sides = "b", colour = "darkblue") +
            theme_bw() + labs(x = NULL, y = "Temperature [°C]")
        
        # Exit
        basePlot
    })
    
    ## Percentile plot ---------------------------------------------------------
    
    output$percPlot <- renderPlot({
        req(input$percSelect)
        
        # Get time series
        df_site_ts <- df_site_ts()
        
        # Filter TS by time
        df_site_base <- df_site_ts |> 
            filter(t >= paste0(input$baseSelect[1],"-01-01"),
                   t <= paste0(input$baseSelect[2],"-12-31"))
        
        # Plot
        percPlot <- ggplot(data = df_site_ts, aes(x = t, y = temp)) + geom_line() + 
            geom_line(data = df_site_base, colour = "darkblue", size = 1.1, alpha = 0.6) +
            geom_vline(aes(xintercept = min(df_site_base$t)), 
                       linetype = "dashed", linewidth = 2, colour = "darkblue") +
            geom_vline(aes(xintercept = max(df_site_base$t)), 
                       linetype = "dashed", linewidth = 2, colour = "darkblue") +
            geom_rug(data = df_site_base, sides = "b", colour = "darkblue") +
            theme_bw() + labs(x = NULL, y = "Temperature [°C]")
        
        # Exit
        percPlot
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
