# shiny/dataAccess/app.R
# The code used to generate the UI for accessing the FACE-IT data product


# Setup -------------------------------------------------------------------

# setwd("shiny/dataAccess/") # For in-app testing
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(plotly)

# Function for re-loading .RData files as necessary
loadRData <- function(fileName){
    load(fileName)
    get(ls()[ls() != "fileName"])
}


# Data --------------------------------------------------------------------

# For testing
input <- data.frame(selectSite = "_stor")

# Full data file paths
full_data_paths <- dir("../../data/full_data", full.names = T)

# Named sites for subsetting paths
sites_named <- c("Kongsfjorden" = "_kong", "Isfjorden" = "_is", "Storfjorden" = "_stor",
                 "Young Sound" = "_young", "Disko Bay" = "_disko", "Nuup Kangerlua" = "_nuup",
                 "Porsangerfjorden" = "_por")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FACE-IT Data Access"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            # Site name
            selectizeInput(
                'selectSite', '1. Site name', 
                choices = sites_named,
                options = list(
                    placeholder = 'Select a site to begin',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
            
            ## Cat
            uiOutput("selectCatUI"),
            
            ## Var
            uiOutput("selectVarUI"),
            
            ### Lon
            uiOutput("slideLonUI"),
            
            ### Lat
            uiOutput("slideLatUI"),
            
            ### Depth
            uiOutput("slideDepthUI"),
            
            ### Date
            uiOutput("slideDateUI"),
            
            ### Value
            uiOutput("slideValueUI"),
            # h5("NB: Value range filtering only makes sense if working with a single type of data (e.g. salinity)")
            
            ### Percentile
        ),

        # Show a plot of the generated distribution
        mainPanel(
            column(width = 12,
                fluidRow(
                    plotlyOutput("tsPlot")
                ),
                fluidRow(
                    plotlyOutput("mapPlot")
                )
            )
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

    
    ## Process data ------------------------------------------------------------

    # Initial site load
    df_load <- reactive({
        req(input$selectSite)
        file_path <- full_data_paths[grep(input$selectSite, full_data_paths)]
        df_load <- loadRData(file_path)
        return(df_load)
    })

    # Subset by category
    df_cat <- reactive({
        req(input$selectCat)
        df_cat <- df_load() %>% filter(var_type %in% input$selectCat)
        return(df_cat)
    })
    
    # Subset by category
    df_var <- reactive({
        req(input$selectVar)
        df_var <- df_cat() %>% filter(var_name %in% input$selectVar)
        return(df_var)
    })
    
    # Filter by smaller details 
    df_filter <- reactive({
        req(input$slideLon)
        df_filter <- df_var() %>%
            filter(lon >= input$slideLon[1], lon <= input$slideLon[2],
                   lat >= input$slideLat[1], lat <= input$slideLat[2],
                   depth >= input$slideDepth[1], depth <= input$slideDepth[2],
                   date >= input$slideDate[1], date <= input$slideDate[2],
                   value >= input$slideValue[1], value <= input$slideValue[2])
        return(df_filter)
    })

    
    ## Reactive UI -------------------------------------------------------------

    # Subset by category
    output$selectCatUI <- renderUI({
        req(input$selectSite)
        df_load <- df_load()
        selectizeInput(
            'selectCat', '2. Data categories',
            choices = unique(df_load$var_type), multiple = T,
            options = list(
                placeholder = 'Select data category(s)',
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    # Subset by var name
    output$selectVarUI <- renderUI({
        req(input$selectCat)
        df_cat <- df_cat()
        selectizeInput(
            'selectVar', '3. Key drivers',
            choices = unique(df_cat$var_name), multiple = T,
            options = list(
                placeholder = 'Select key driver(s)',
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    # Filter data
    # Lon
    output$slideLonUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideLon", "4. Longitude range", value = range(df_var()$lon, na.rm = T),
                           min = min(df_var()$lon, na.rm = T), max = max(df_var()$lon, na.rm = T))
    })
    
    # Lat
    output$slideLatUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideLat", "5. Latitude range", value = range(df_var()$lat, na.rm = T),
                           min = min(df_var()$lat, na.rm = T), max = max(df_var()$lat, na.rm = T))
    })
    
    # Depth
    output$slideDepthUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideDepth", "7. Depth range", value = range(df_var()$depth, na.rm = T),
                           min = min(df_var()$depth, na.rm = T), max = max(df_var()$depth, na.rm = T))
    })
    
    # Date
    output$slideDateUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideDate", "6. Date range", value = range(df_var()$date, na.rm = T),
                           min = min(df_var()$date, na.rm = T), max = max(df_var()$date, na.rm = T))
    })
    
    # Value range
    output$slideValueUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideValue", "8. Value range", value = range(df_var()$value, na.rm = T),
                           min = min(df_var()$value, na.rm = T), max = max(df_var()$value, na.rm = T))
    })


    ## Time series -------------------------------------------------------------

    output$tsPlot <- renderPlotly({
        req(is.data.frame(df_filter()))
        
        # Fill date gaps in TS
        df_fill <- df_filter()
        
        # May want to create dummy dataframe if filtering removes all rows
        # if(nrow(df_fill == 0)) df_fill <- data.frame(date = as.Date("2000-01-01"), value = 1, var_name = "NA")
        
        # Plot and exit
        basePlot <- ggplot(data = df_fill, aes(x = date, y = value)) +
            geom_line(aes(colour = var_name))
        ggplotly(basePlot)
    })
    

    ## Map ---------------------------------------------------------------------

    output$mapPlot <- renderPlotly({
        # Base map reacts to site selection
        req(input$selectSite)
        
        # Show bbox created by lon/lat sliders
        x    <- faithful
        basePlot <- ggplot(data = x, aes(x = waiting, y = eruptions)) +
            geom_line()
        ggplotly(basePlot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
