# shiny/dataAccess/app.R
# The code used to generate the UI for accessing the FACE-IT data product

# TODO: Add FACE-IT funding to app

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
# input <- data.frame(selectSite = "_stor")

# Full data file paths
full_data_paths <- dir("full_data", full.names = T)

# Named sites for subsetting paths
sites_named <- c("Svalbard" = "_sval", "Kongsfjorden" = "_kong", "Isfjorden" = "_is", "Storfjorden" = "_stor",
                 "Young Sound" = "_young", "Disko Bay" = "_disko", "Nuup Kangerlua" = "_nuup",
                 "Porsangerfjorden" = "_por")

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_sval <- c(9, 30, 76, 81)
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(12.97, 17.50, 77.95, 78.90)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.08)
bbox_stor <- c(17.35, 21.60, 77.33, 78.13)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8)
bbox_por <- c(24.5, 27, 70, 71.2)

# The base global map
map_base <- readRDS("../../metadata/map_base.Rda")


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
            # uiOutput("slideValueUI"),
            # h5("NB: Value range filtering only makes sense if working with a single type of data (e.g. salinity)")
            
            ### Percentile
            
            ### Download
            hr(),
            fluidRow(column(width = 6, uiOutput("downloadFilterTypeUI")),
                     column(width = 6, uiOutput("downloadFilterUI")))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            column(width = 12,
                   fluidRow(plotlyOutput("tsPlot")),
                   fluidRow(plotlyOutput("mapPlot"))
            )
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

    
    ## Reactive UI -------------------------------------------------------------

    # Subset by category
    output$selectCatUI <- renderUI({
        req(input$selectSite)
        selectizeInput(
            'selectCat', '2. Data categories',
            choices = unique(df_load()$var_type), multiple = T,
            options = list(
                placeholder = 'Select data category(s)',
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    # Subset by var name
    output$selectVarUI <- renderUI({
        req(input$selectCat)
        selectizeInput(
            'selectVar', '3. Key drivers',
            choices = unique(df_cat()$var_name), multiple = T,
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
        shiny::sliderInput("slideDepth", "6. Depth range", value = range(df_var()$depth, na.rm = T),
                           min = min(df_var()$depth, na.rm = T), max = max(df_var()$depth, na.rm = T))
    })
    
    # Date
    output$slideDateUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideDate", "8. Date range", value = range(df_var()$date, na.rm = T),
                           min = min(df_var()$date, na.rm = T), max = max(df_var()$date, na.rm = T))
    })

    # Value range
    # output$slideValueUI <- renderUI({
    #     req(input$selectVar)
    #     shiny::sliderInput("slideValue", "8. Value range", value = range(df_var()$value, na.rm = T),
    #                        min = min(df_var()$value, na.rm = T), max = max(df_var()$value, na.rm = T))
    # })

    
    ## Download UI -------------------------------------------------------------

    # Reactive download type button
    output$downloadFilterTypeUI <- renderUI({
        req(input$selectVar)
        radioButtons("downloadFilterType", "File type", choices = c(".csv", ".Rds"), 
                     selected = ".csv", inline = T)
    })
    
    # Reactive download button
    output$downloadFilterUI <- renderUI({
        req(input$selectVar)
        downloadButton("downloadFilter", "Download data")
    })
    
    # Download handler
    output$downloadFilter <- downloadHandler(
        filename = function() {
            paste0("FACE-IT_data",input$downloadFilterType[1])
        },
        content <- function(file) {
            if(input$downloadFilterType == ".Rds"){
                saveRDS(df_filter(), file = file)
            } else if(input$downloadFilterType == ".csv"){
                readr::write_csv(df_filter(), file)
            }
        }
    )
    
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
    
    # Subset by variable name
    df_var <- reactive({
        req(input$selectVar)
        df_var <- df_cat() %>% filter(var_name %in% input$selectVar)
        return(df_var)
    })
    
    # Filter by smaller details 
    df_filter <- reactive({
        # req(input$selectSite)
        if(length(input$selectVar) == 0){
            df_filter <- data.frame(date = as.Date("2000-01-01"), value = 1, var_name = "Select variables",
                                    lon = 1, lat = 1, depth = 1)
        } else if(length(input$selectVar) > 0){
            # base::Sys.sleep(5)
            req(input$slideLon)
            # if(length(input$selectValue) == 2){
                df_filter <- df_var() %>%
                    filter(lon >= input$slideLon[1], lon <= input$slideLon[2],
                           lat >= input$slideLat[1], lat <= input$slideLat[2],
                           depth >= input$slideDepth[1], depth <= input$slideDepth[2],
                           date >= input$slideDate[1], date <= input$slideDate[2])#,
                           # value >= input$slideValue[1], value <= input$slideValue[2])
            } else {
                df_filter <- data.frame(date = as.Date("2000-01-01"), value = 1, var_name = "last",
                                        lon = 1, lat = 1, depth = 1)
            }
        # }
        return(df_filter)
    })
    
    
    ## Time series -------------------------------------------------------------

    output$tsPlot <- renderPlotly({
        req(input$selectSite)
        # req(input$slideLon)
        
        df_filter <- df_filter()
        
        # Fill date gaps in TS
        
        # May want to create dummy dataframe if filtering removes all rows
        # if(nrow(df_fill == 0)) df_fill <- data.frame(date = as.Date("2000-01-01"), value = 1, var_name = "NA")
        
        # Plot and exit
        basePlot <- ggplot(data = df_filter, aes(x = date, y = value)) +
            geom_line(aes(colour = var_name, group = depth)) +
            labs(x = NULL) +
            theme_bw() +
            theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
                  axis.text = element_text(size = 12, colour = "black"),
                  axis.ticks = element_line(colour = "black"))
        ggplotly(basePlot)
    })
    

    ## Map ---------------------------------------------------------------------

    output$mapPlot <- renderPlotly({
        req(input$selectSite)
        
        # Base map reacts to site selection
        if(input$selectSite == "_sval") bbox_name <- bbox_sval
        if(input$selectSite == "_kong") bbox_name <- bbox_kong
        if(input$selectSite == "_is") bbox_name <- bbox_is
        if(input$selectSite == "_stor") bbox_name <- bbox_stor
        if(input$selectSite == "_young") bbox_name <- bbox_young
        if(input$selectSite == "_disko") bbox_name <- bbox_disko
        if(input$selectSite == "_nuup") bbox_name <- bbox_nuup
        if(input$selectSite == "_por") bbox_name <- bbox_por
        
        # Get buffer area for plot
        xmin <- bbox_name[1]-(bbox_name[2]-bbox_name[1])/4
        xmax <- bbox_name[2]+(bbox_name[2]-bbox_name[1])/4
        ymin <- bbox_name[3]-(bbox_name[4]-bbox_name[3])/8
        ymax <- bbox_name[4]+(bbox_name[4]-bbox_name[3])/8
        
        # Filtered data for plotting
        df_filter <- df_filter()
        
        # Show bbox created by lon/lat sliders
        basePlot <- ggplot() + 
            geom_polygon(data = map_base, fill = "grey80", colour = "black",
                         aes(x = lon, y = lat, group = group)) +
            annotate(geom = "text", x = bbox_name[1], y = bbox_name[3], label = nrow(df_filter), colour = "red") +
            geom_rect(aes(xmin = bbox_name[1], xmax = bbox_name[2], ymin = bbox_name[3], ymax = bbox_name[4]),
                      fill = "khaki", alpha = 0.1) +
            coord_equal(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
            labs(x = NULL, y = NULL) +
            theme_bw() +
            theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
                  axis.text = element_text(size = 12, colour = "black"),
                  axis.ticks = element_line(colour = "black"))
        
        if(nrow(df_filter) > 0){
            basePlot <- basePlot +
                geom_point(data = df_filter, aes(x = lon, y = lat, colour = var_name))
        }
        
        ggplotly(basePlot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
