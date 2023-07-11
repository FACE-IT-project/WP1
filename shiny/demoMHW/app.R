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
                    menuItem("Time", tabName = "time", icon = icon("clock"), selected = TRUE),
                    menuItem("Perc+base", tabName = "perc_base", icon = icon("percent")),
                    menuItem("Main event", tabName = "event", icon = icon("temperature-high"))),
        
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
                                 a marine heatwave (MHW) using the Hobday et al. (2016, 2018) definition. The "),
                               h2(tags$b("References")),
                               p("Hobday et al. 2016 etc.")
                        )
                    )
            ),
            
            
            ## Time menu ---------------------------------------------------------------
            
            tabItem(tabName = "time",
                    p("Once upon a time series..."),
                    fluidPage(column(12,
                                     box(width = 3, title = "Time choice", # height = "880px",
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
                    box(width = 9, title = "Time series", 
                        status = "info", solidHeader = TRUE, collapsible = FALSE,
                        plotOutput("timePlot")
                               # shinycssloaders::withSpinner(plotlyOutput("timePlot"), 
                               #                              type = 6, color = "#b0b7be"))
                        )
                    )
                    )
            )#,
            
            ## Perc+base menu ----------------------------------------------------------
            
            # tabItem(tabName = "perc_base",
            #         fluidPage(column(12,
            #                          img(src = "MHW_def.png", align = "center", width = "600px")))
            # ),
            
            
            ## Main event men-----------------------------------------------------------
            
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


    ## Time plot ---------------------------------------------------------------
    
    # NB: Too many data points for plotly to run quickly
    output$timePlot <- renderPlot({
        req(input$seriesSelect)
        
        # Determine time series to use
        if(input$seriesSelect == "Western Australia"){
            df_site_ts <- sst_WA
        } else if(input$seriesSelect == "NW Atlantic"){
            df_site_ts <- sst_NW_Atl
        } else if(input$seriesSelect == "Mediterranean"){
            df_site_ts <- sst_Med
        }
        
        # Create time columns
        df_site_ts <- df_site_ts |> 
            mutate(all = "All",
                   month = lubridate::month(t, label = TRUE),
                   year = lubridate::year(t),
                   # NB: technically this is incorrect as it doesn't respect leap years
                   # But it's quick and easy...
                   doy = as.numeric(strftime(t, format = "%j")),
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
        
        # Plot and exit
        basePlot <- ggplot(data = df_site_ts, aes(x = t, y = temp)) + geom_point(position = "jitter") +
            theme_bw() + labs(x = NULL, y = "Temperature [°C]")
        basePlot
        # return(basePlot)
        
        # ggplotly(basePlot, tooltip = "text")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
