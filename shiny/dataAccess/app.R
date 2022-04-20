# shiny/dataAccess/app.R
# The code used to generate the UI for accessing the FACE-IT data product

# TODO: Add FACE-IT funding to app
# Add clean vs full switch for data selection


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

# Project wide category colours
CatCol <- c(
    "Cryosphere" = "mintcream",
    "Physical" = "skyblue",
    "Chemistry" = "#F6EA7C",
    "Biology" = "#A2ED84",
    "Social" = "F48080"
)

# Same but with abbreviations for the categories
CatColAbr <- c(
    "cryo" = "mintcream",
    "phys" = "skyblue",
    "chem" = "#F6EA7C",
    "bio" = "#A2ED84",
    "soc" = "#F48080"
)

# Data --------------------------------------------------------------------

# For testing
# input <- data.frame(selectSite = "_stor")

# Full data file paths
full_data_paths <- dir("full_data", full.names = T)

# Named sites for subsetting paths
# sites_named <- c("Kongsfjorden" = "_kong", "Isfjorden" = "_is", "Storfjorden" = "_stor",
#                  # "Svalbard" = "_sval",
#                  "Young Sound" = "_young", "Disko Bay" = "_disko", "Nuup Kangerlua" = "_nuup",
#                  "Porsangerfjorden" = "_por")
sites_named <- list(Svalbard = c("Kongsfjorden" = "_kong", "Isfjorden" = "_is", "Storfjorden" = "_stor"),
                 # "Svalbard" = "_sval",
                 Greenland = c("Young Sound" = "_young", "Disko Bay" = "_disko", "Nuup Kangerlua" = "_nuup"),
                 Norway = c("Porsangerfjorden" = "_por"))
# cats_named <- 

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
map_base <- readRDS("map_base.Rda")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Access to data collected for FACE-IT"),
    
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        # sidebarMenu(id = "mainMenu",
            # Site name
            selectizeInput(
                'selectSite', '1. Site name',
                choices = sites_named,
                options = list(
                    placeholder = 'Select a site to begin',
                    onInitialize = I('function() { this.setValue(""); }'))
            ),

            ## Cat
            # uiOutput("selectCatUI"),
            selectizeInput(
                'selectCat', '2. Data categories',
                # choices = unique(df_load()$var_type),
                choices = c("Cryosphere" = "_cryo", "Physical" = "_phys", "Chemistry" = "_chem", 
                            "Biology" = "_bio", "Social" = "_soc"),
                multiple = T,
                options = list(
                    placeholder = 'Select data category(s)',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),

            ### Value
            # uiOutput("slideValueUI"),
            # h5("NB: Value range filtering only makes sense if working with a single type of data (e.g. salinity)")

            ### Percentile

            ### Download
            # uiOutput("downloadFilterTypeUI"),
            radioButtons("downloadFilterType", "9. File type", choices = c(".csv", ".Rds"), 
                         selected = ".csv", inline = T),
            fluidRow(column(width = 2), column(width = 10, downloadButton("downloadFilter", "Download data"))),
            # fluidRow(column(width = 2), column(width = 10, uiOutput("downloadFilterUI"))),
            # hr(),
            # h3("9. Download data"),
            # fluidRow(column(width = 6, uiOutput("downloadFilterTypeUI")),
            #          column(width = 6, uiOutput("downloadFilterUI")))#,

            # Add FACE-IT logo at bottom of menu bar
            br(), br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            img(src = "FACE-IT_Logo_900.png", align = "centre", width = "225")
        # ),
    ),
    
    # Show a plot of the generated distribution
    dashboardBody(
        fluidRow(
            column(width = 3,
                   box(width = 12, height = "780px", title = "Filter",
                       status = "warning", solidHeader = TRUE, collapsible = FALSE,
                       uiOutput("selectVarUI"), uiOutput("slideLonUI"), uiOutput("slideLatUI"),
                       uiOutput("slideDepthUI"), uiOutput("slideDateUI"))),
            column(width = 5,
                   box(width = 12, height = "380px", title = "Lon/lat",
                       status = "info", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("mapPlot", height = "325px"), 
                                                    type = 6, color = "#b0b7be")),
                   box(width = 12, height = "380px", title = "Date",
                       status = "primary", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("tsPlot", height = "325px"), 
                                                    type = 6, color = "#b0b7be"))),
            column(width = 4,
                   box(width = 12, height = "780px", title = "Depth",
                       status = "success", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("depthPlot", height = "720px"), 
                                                    type = 6, color = "#b0b7be")))
            # column(width = 3,
            # fluidRow(plotlyOutput("depthPlot")))
            # column(width = 3, plotlyOutput("depthPlot"))
        )
    ),
    
    title = "Data Access",
    skin = "purple"

)


# Server ------------------------------------------------------------------

server <- function(input, output) {

    
    ## Reactive UI -------------------------------------------------------------

    # Subset by category
    # output$selectCatUI <- renderUI({
    #     req(input$selectSite)
    #     selectizeInput(
    #         'selectCat', '2. Data categories',
    #         # choices = unique(df_load()$var_type),
    #         choices = c("Cryosphere" = "_cryo", "Physical" = "_phys", "Chemistry" = "_chem", 
    #                     "Biology" = "_bio", "Social" = "_soc"),
    #         multiple = T,
    #         options = list(
    #             placeholder = 'Select data category(s)',
    #             onInitialize = I('function() { this.setValue(""); }')
    #         )
    #     )
    # })
    
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
        min_pad <- round(min(df_var()$lon, na.rm = T), 3)-0.001
        max_pad <- round(max(df_var()$lon, na.rm = T), 3)+0.001
        shiny::sliderInput("slideLon", "4. Longitude range",
                           value = c(min_pad, max_pad), min = min_pad, max = max_pad)
    })
    
    # Lat
    output$slideLatUI <- renderUI({
        req(input$selectVar)
        min_pad <- round(min(df_var()$lat, na.rm = T), 3)-0.001
        max_pad <- round(max(df_var()$lat, na.rm = T), 3)+0.001
        shiny::sliderInput("slideLat", "5. Latitude range",
                           value = c(min_pad, max_pad), min = min_pad, max = max_pad)
    })
    
    # Date
    output$slideDateUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideDate", "6. Date range", value = range(df_var()$date, na.rm = T),
                           min = min(df_var()$date, na.rm = T), max = max(df_var()$date, na.rm = T))
    })
    
    # Depth
    output$slideDepthUI <- renderUI({
        req(input$selectVar)
        shiny::sliderInput("slideDepth", "7. Depth range", value = range(df_var()$depth, na.rm = T),
                           min = min(df_var()$depth, na.rm = T), max = max(df_var()$depth, na.rm = T))
    })

    # Value range
    # output$slideValueUI <- renderUI({
    #     req(input$selectVar)
    #     shiny::sliderInput("slideValue", "8. Value range", value = range(df_var()$value, na.rm = T),
    #                        min = min(df_var()$value, na.rm = T), max = max(df_var()$value, na.rm = T))
    # })

    
    ## Download UI -------------------------------------------------------------

    # Reactive download type button
    # output$downloadFilterTypeUI <- renderUI({
    #     req(input$selectVar)
    #     radioButtons("downloadFilterType", "9. File type", choices = c(".csv", ".Rds"), 
    #                  selected = ".csv", inline = T)
    # })
    
    # Reactive download button
    # output$downloadFilterUI <- renderUI({
    #     req(input$selectVar)
    #     # h4("10. Download")
    #     downloadButton("downloadFilter", "Download data")
    # })
    
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
    # df_load <- reactive({
    #     req(input$selectSite)
    #     file_path <- full_data_paths[grep(input$selectSite, full_data_paths)]
    #     df_load <- loadRData(file_path)
    #     return(df_load)
    # })
    
    # Subset by category
    df_cat <- reactive({
        req(input$selectCat)
        file_list <- paste0("full_data/full",
                            # c("_cryo", "_phys"), # testing
                            input$selectCat,
                            input$selectSite,".RData")
        df_cat <- purrr::map_dfr(file_list, loadRData)
        return(df_cat)
    })
    
    # Subset by variable name
    df_var <- reactive({
        req(input$selectVar)
        df_var <- df_cat() %>% 
            filter(var_name %in% input$selectVar)
        return(df_var)
    })
    
    # Filter by smaller details 
    df_filter <- reactive({
        # req(input$selectSite)
        if(length(input$selectVar) == 0){
            df_filter <- data.frame(date = as.Date("2000-01-01"), value = 1, var_name = "Select drivers",
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
                df_filter <- data.frame(date = as.Date("2000-01-01"), value = 1, 
                                        var_name = "last", lon = 1, lat = 1, depth = 1)
            }
        # }
        return(df_filter)
    })
    
    # Unrounded data.frame for downloading
    # df_filter_dl <- reactive({            
    #     req(input$slideLon)
    #     # if(length(input$selectValue) == 2){
    #     df_filter_dl <- df_cat() %>% 
    #         filter(var_name %in% input$selectVar) %>% 
    #         filter(lon >= input$slideLon[1], lon <= input$slideLon[2],
    #                lat >= input$slideLat[1], lat <= input$slideLat[2],
    #                depth >= input$slideDepth[1], depth <= input$slideDepth[2],
    #                date >= input$slideDate[1], date <= input$slideDate[2])
    #     return(df_filter)
    # })
    
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
        df_filter <- df_filter() %>% 
            filter(!is.na(lon), !is.na(lat)) %>% 
            mutate(lon = round(lon, 3), lat = round(lat, 3)) %>% 
            group_by(lon, lat, var_name) %>% 
            summarise(count = n(), .groups = "drop")
        
        # Show bbox created by lon/lat sliders
        basePlot <- ggplot() + 
            geom_polygon(data = map_base, fill = "grey80", colour = "black",
                         aes(x = lon, y = lat, group = group, text = "Land")) +
            # annotate(geom = "text", x = bbox_name[1], y = bbox_name[3], label = nrow(df_filter), colour = "red") +
            # geom_rect(aes(xmin = bbox_name[1], xmax = bbox_name[2], ymin = bbox_name[3], ymax = bbox_name[4]),
                      # fill = "khaki", alpha = 0.1) +
            # coord_equal(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
            coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
            labs(x = NULL, y = NULL, colour = "Driver") +
            theme_bw() +
            theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
                  axis.text = element_text(size = 12, colour = "black"),
                  axis.ticks = element_line(colour = "black"), legend.position = "none")
        
        if(nrow(df_filter) > 0){
            basePlot <- basePlot + geom_point(data = df_filter, 
                                              aes(x = lon, y = lat, colour = var_name,
                                                  text = paste0("Driver: ",var_name,
                                                                "<br>Lon: ",lon,
                                                                "<br>Lat: ",lat,
                                                                "<br>Count: ",count)))
        }
        
        ggplotly(basePlot, tooltip = "text")
    })
    
    
    ## Time series -------------------------------------------------------------

    output$tsPlot <- renderPlotly({
        req(input$selectVar)
        # req(input$slideLon)
        
        df_filter <- df_filter() %>% 
            filter(!is.na(date)) %>% 
            group_by(date, var_name) %>% 
            summarise(count = n(), .groups = "drop")

        # May want to create dummy dataframe if filtering removes all rows
        # Doesn't work presently due to object dependencies
        # if(nrow(df_filter == 0)) df_filter <- data.frame(date = as.Date("2000-01-01"), value = 1, var_name = "NA")
        
        # Plot and exit
        basePlot <- ggplot(data = df_filter, aes(x = date, y = log10(count))) +
            # geom_point(aes(colour = var_name)) +
            # geom_line(aes(colour = var_name, group = depth)) +
            labs(x = NULL, y = "Count (log10)", colour = "Driver") +
            theme_bw() +
            theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
                  axis.text = element_text(size = 12, colour = "black"),
                  axis.ticks = element_line(colour = "black"), legend.position = "none")
        
        if(nrow(df_filter) > 0){
            basePlot <- basePlot + geom_point(data = df_filter, aes(colour = var_name,
                                                                    text = paste0("Driver: ",var_name,
                                                                                  "<br>Date: ",date,
                                                                                  "<br>Count: ",count)))
        }
        
        ggplotly(basePlot, tooltip = "text")
    })
    

    ## Depth plot --------------------------------------------------------------

    output$depthPlot <- renderPlotly({
        req(input$selectVar)
        # req(input$slideLon)
        
        df_filter <- df_filter() %>% 
            filter(!is.na(depth)) %>% 
            # mutate(depth = ifelse(is.na(depth), 0, depth)) %>%
            mutate(depth = round(depth, -1)) %>%
            group_by(depth, var_name) %>% 
            dplyr::summarise(count = n(), .groups = "drop")
        
        # Count of data at depth by var name
        basePlot <- ggplot(df_filter) +
            geom_col(aes(x = depth, y = log10(count), fill = var_name,
                         text = paste0("Driver: ",var_name,
                                       "<br>Depth: ",depth,
                                       "<br>Count: ",count))) +
            scale_x_reverse() +
            coord_flip(expand = F) +
            # scale_fill_manual(values = CatColAbr, aesthetics = c("colour", "fill")) +
            # guides(fill = guide_legend(nrow = length(unique(full_product$var_type)))) +
            labs(x = NULL, fill = "Driver", y = "Count (log10)") +
            theme(panel.border = element_rect(fill = NA, colour = "black"), legend.position = "none")
        
        # if(nrow(df_filter) > 0){
        #     basePlot <- basePlot + geom_col(data = df_filter, aes(x = depth, y = log10(count), fill = var_name)) 
        # }
        
        ggplotly(basePlot, tooltip = "text")# %>% config(displayModeBar = F) %>% 
            # layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

