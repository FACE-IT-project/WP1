# shiny/dataAccess/app.R
# The code used to generate the UI for accessing the FACE-IT data product

# TODO:
# HiRes coastline for smaller sites
# Troubleshoot clean vs full switch for data selection


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
# input <- data.frame(selectSite = "_kong",
#                     selectCat = "_phys")

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

# Load PANGAEA driver metadata sheet
## NB: Code only run once from home dir to get slimmed down parameter list
# pg_parameters <- read_tsv("metadata/pangaea_parameters.tab")
# var_names <- distinct(select(df_cat, var_name))
# param_list <- pg_parameters %>% 
#     mutate(Driver = paste0(Abbreviation," [",Unit,"]")) %>% 
#     filter(Driver %in% var_names$var_name) %>% 
#     dplyr::select(Driver, Parameter)
# write_csv(param_list, "shiny/dataAccess/param_list.csv")
param_list <- read_csv("param_list.csv")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # Application title
    # dashboardHeader(title = "Access to data collected for FACE-IT"),
    dashboardHeader(title = "Data access"),
    
    # Sidebar
    dashboardSidebar(
        
        # Add FACE-IT logo to menu bar
        br(),
        img(src = "FACE-IT_Logo_900.png", align = "centre", width = "225"),
        br(), br(),
        
        # Site name
        selectizeInput(
            'selectSite', '1. Site',
            choices = sites_named,
            options = list(
                placeholder = 'Select a site to begin',
                onInitialize = I('function() { this.setValue(""); }'))
        ),
        
        # Category
        selectizeInput(
            'selectCat', '2. Categories',
            # choices = unique(df_load()$var_type),
            choices = c("Cryosphere" = "_cryo", "Physical" = "_phys", "Chemistry" = "_chem", 
                        "Biology" = "_bio", "Social" = "_soc"),
            multiple = T,
            options = list(
                placeholder = 'Select driver category(s)',
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        
        # Switch for Clean vs Full data
        # shinyWidgetsGallery()
        radioGroupButtons(
            inputId = "cleanVSfull",
            label = "3. Clean or full data", 
            selected = "Full",
            choices = c("Clean", "Full"),
            status = "warning"
        ),
        
        # Filter PANGAEA data
        radioGroupButtons(
            inputId = "PANGAEAfilter",
            label = "4. Filter PANGAEA", 
            choices = c("Yes", "No"), 
            selected = "No",
            status = "warning"
        ),
        
        # Drivers
        uiOutput("selectVarUI"),
        
        # Download
        radioButtons("downloadFilterType", "6. Download", choices = c(".csv", ".Rds"), 
                     selected = ".csv", inline = T),
        fluidRow(column(width = 2), column(width = 10, offset = 1, downloadButton("downloadFilter", "Download data"))),
        
        # Info popup
        # shinyWidgets::dropdown(shiny::renderPrint("Test"))
        # Activate sweet alerts
        br(),
        hr(),
        br(),
        # br(),
        useSweetAlert(),
        actionBttn(
            inputId = "info",
            label = "Instructions",
            icon = icon("book"),
            style = "material-flat",
            color = "success"
        )
        
        # ),
    ),
    
    # Show a plot of the generated distribution
    dashboardBody(
        fluidRow(
            column(width = 3,
                   box(width = 12, height = "380px", title = "Full names",
                       status = "warning", solidHeader = TRUE, collapsible = FALSE,
                       DT::dataTableOutput("longVarDT")),
                   box(width = 12, height = "440px", title = "Filter data",
                       status = "warning", solidHeader = TRUE, collapsible = FALSE,
                       #uiOutput("selectVarUI"), 
                       uiOutput("slideLonUI"), uiOutput("slideLatUI"),
                       uiOutput("slideDepthUI"), uiOutput("slideDateUI"))),
            column(width = 5,
                   box(width = 12, height = "410px", title = "Lon/lat",
                       status = "info", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("mapPlot", height = "355px"), 
                                                    type = 6, color = "#b0b7be")),
                   box(width = 12, height = "410px", title = "Date",
                       status = "primary", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("tsPlot", height = "355px"), 
                                                    type = 6, color = "#b0b7be"))),
            column(width = 4,
                   box(width = 12, height = "840px", title = "Depth",
                       status = "success", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("depthPlot", height = "780px"), 
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

server <- function(input, output, session) {

    
    ## Reactive UI -------------------------------------------------------------
    
    # Select drivers
    output$selectVarUI <- renderUI({
        # req(input$selectCat)
        
        if(length(input$selectCat) > 0){
            var_choices <- unique(df_cat()$var_name)
        } else{
            var_choices <- ""
        }
        
        selectizeInput(
            'selectVar', '5. Drivers',
            choices = var_choices, multiple = T,
            options = list(
                placeholder = 'Select driver(s)',
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    # List of long names for drivers
    output$longVarDT <- DT::renderDataTable({
        # req(input$selectCat)
        # unique(df_cat()$var_name)
        df_names <- param_list#filter(param_list, Driver %in% )
        df_names_DT <- datatable(df_names, rownames = FALSE,
                                 options = list(pageLength = 1000, scrollX = TRUE, scrollY = 240, info = FALSE,
                                                lengthChange = FALSE, paging = FALSE,
                                                columnDefs = list(list(searchable = FALSE, targets = 1))))
        return(df_names_DT)
    })
    
    # Filter data
    # Lon
    output$slideLonUI <- renderUI({
        req(input$selectVar)
        if(length(na.omit(df_var()$lon)) == 0){
            min_val <- 0; max_val <- 0
        } else {
            min_val <- round(min(df_var()$lon, na.rm = T), 2)-0.01
            max_val <- round(max(df_var()$lon, na.rm = T), 2)+0.01
        }
        shiny::sliderInput("slideLon", "Longitude range", value = c(min_val, max_val), min = min_val, max = max_val)
    })
    
    # Lat
    output$slideLatUI <- renderUI({
        req(input$selectVar)
        if(length(na.omit(df_var()$lat)) == 0){
            min_val <- 0; max_val <- 0
        } else {
            min_val <- round(min(df_var()$lat, na.rm = T), 2)-0.01
            max_val <- round(max(df_var()$lat, na.rm = T), 2)+0.01
        }
        shiny::sliderInput("slideLat", "Latitude range", value = c(min_val, max_val), min = min_val, max = max_val)
    })
    
    # Date
    output$slideDateUI <- renderUI({
        req(input$selectVar)
        if(length(na.omit(df_var()$date)) == 0){
            min_val <- as.Date("2000-01-01"); max_val <- as.Date("2000-01-01")
        } else {
            min_val <- min(df_var()$date, na.rm = T)
            max_val <- max(df_var()$date, na.rm = T)
        }
        shiny::sliderInput("slideDate", "Date range", value = c(min_val, max_val), min = min_val, max = max_val)
    })
    
    # Depth
    output$slideDepthUI <- renderUI({
        req(input$selectVar)
        if(length(na.omit(df_var()$depth)) == 0){
            min_val <- 0; max_val <- 0
        } else {
            min_val <- min(df_var()$depth, na.rm = T)
            max_val <- max(df_var()$depth, na.rm = T)
        }
        shiny::sliderInput("slideDepth", "Depth range", value = c(min_val, max_val), min = min_val, max = max_val)
    })

    observeEvent(input$info, {
        sendSweetAlert(
            session = session,
            title = "Instructions",
            text = tags$span(
                "The FACE-IT data access app is laid out as a dashboard with a ",tags$em("side bar"),
                " on the left and additional information and plots in the ",tags$em("body"),".",
                tags$h3(icon("bars"),"Side bar", style = "color: cadetblue;"),
                tags$h4(tags$b("1. Site"),": Select a site to begin the data exploration process. Currently it is only possible to select one site at a time. After selecting a site the 'Lon/lat' figure in the body will display a rough map."),
                tags$h4(tags$b("2. Categories"),": Select one or more categories of interest."),
                tags$h4(icon("hammer"),tags$b("3. Clean or full data"),": Much of the data sourced for FACE-IT are formatted differently from one another. In an effort to adress this issue the data are undergoing an additional level of cleaning. This is currently under construction."),
                tags$h4(tags$b("4. Filter PANGAEA"),": The data downloaded from PANGAEA are voluminous, but often contain data that are not of interest. Switch this value to 'Yes' to remove PANGAEA data from the list of drivers selected in the next step. ."),
                tags$h4(tags$b("5. Drivers"),": The four preceeding steps will have filtered down the possible drivers that can now be selected here. After selecting one or more drivers the figures in the body will begin to be populated by the data."),
                tags$h4(tags$b("6. Download"),": When one is happy with the selected/filtered data they may be downloaded here. First choose the file type, then click the download button."),
                tags$h3(icon("images"),"Body", style = "color: steelblue;"),
                tags$h4(tags$b("Full names"),": This menu allows the user to search for the full names of the drivers seen in ", tags$em("5. Drivers"), "in the sidebar."),
                tags$h4(tags$b("Filter data"),": The ranges in the data for: longitude, latitude, depth, and date will be displayed here. Moving these sliders will filter out the data and this will be reflected in the figures to the right."),
                tags$h4(tags$b("Lon/lat"),": Displays a map of where the selected data are located. Different colours show different drivers. Mouse over the dots on within the map to see more information."),
                tags$h4(tags$b("Date"),": Displays the selected/filtered data as a time series. May mouse over the points to see more information."),
                tags$h4(tags$b("Depth"),": Shows the selected data as a stacked bar plot from the surface (0 m) down to the deepest depth in the data. Data are binned into 10 m groups. Negative values show data above the sea surface (altimetry). Mouse over for more information."),
                # tags$br(),
                tags$h3(icon("address-book"),"Contact", style = "color: royalblue;"),
                "For questions etc. please contact Robert Schlegel: robert.schlegel@imev-mer.fr"
            ),
            html = TRUE,
            type = "info"
        )
    })
    
    
    ## Download UI -------------------------------------------------------------
    
    # Download handler
    output$downloadFilter <- downloadHandler(
        filename = function() {
            paste0("filtered_data",input$downloadFilterType[1])
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
    
    # Subset by category
    df_cat <- reactive({
        req(input$selectCat)
        
        # Load initial data
        if(input$cleanVSfull == "Full"){
            file_list <- paste0("full_data/full",
                                # c("_cryo", "_phys"), # testing
                                input$selectCat,
                                input$selectSite,".csv")
            df_cat <- purrr::map_dfr(file_list, data.table::fread)
        } else {
            file_list <- paste0("full_data/clean",
                                # c("_cryo", "_phys"), # testing
                                input$selectCat,
                                # input$selectSite,
                                "_all.csv")
            df_cat <- purrr::map_dfr(file_list, data.table::fread) %>% # This should be improved to be site specific
                filter(site == stringr::str_remove(input$selectSite, "_"),
                       # var_type %in% str_remove(input$selectCat, "_"),
                       !grepl("g-e-m", URL), # Remove GEM data
                       !grepl("GRDC", URL), # Remove GRDC data
                       !grepl("Received directly from Mikael Sejr", URL)) # Remove embargoed PAR data from Mikael
            
            # Remove GRDC data
            ## Not yet amalgamated
        }

        # Filter PANGAEA data
        if(input$PANGAEAfilter == "Yes"){
            df_cat <- filter(df_cat, !grepl("PANGAEA", URL))
        }
        
        # Format dates
        df_cat$date <- as.Date(df_cat$date)
        
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
        if(length(input$selectVar) == 0){
            df_filter <- data.frame(date = as.Date("2000-01-01"), value = NA, var_name = "No drivers selected",
                                    lon = NA, lat = NA, depth = NA)
        } else if(length(input$selectVar) > 0){
            req(input$slideDate)
            df_filter <- df_var() #%>%
            if(!is.na(input$slideLon[1])){
                df_filter <- df_filter %>% 
                    filter(lon >= input$slideLon[1] | is.na(lon)) %>% 
                    filter(lon <= input$slideLon[2] | is.na(lon))}
            if(!is.na(input$slideLat[1])){
                df_filter <- df_filter %>% 
                    filter(lat >= input$slideLat[1] | is.na(lat)) %>% 
                    filter(lat <= input$slideLat[2] | is.na(lat))}
            if(!is.na(input$slideDepth[1])){
                df_filter <- df_filter %>% 
                    filter(depth >= input$slideDepth[1] | is.na(depth)) %>% 
                    filter(depth <= input$slideDepth[2] | is.na(depth))}
            if(!is.na(input$slideDate[1])){
                df_filter <- df_filter %>% 
                    filter(date >= input$slideDate[1] | is.na(date)) %>% 
                    filter(date <= input$slideDate[2] | is.na(date))}
                # filter(lon >= input$slideLon[1], lon <= input$slideLon[2],
                #        lat >= input$slideLat[1], lat <= input$slideLat[2],
                #        depth >= input$slideDepth[1], depth <= input$slideDepth[2],
                #        date >= input$slideDate[1], date <= input$slideDate[2])
        } else {
            df_filter <- data.frame(date = as.Date("2000-01-01"), value = NA,
                                    var_name = "All data have been filtered out", lon = NA, lat = NA, depth = NA)
        }
        return(df_filter)
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
            # geom_col(aes(x = depth, y = log10(count), fill = var_name,
            #              text = paste0("Driver: ",var_name,
            #                            "<br>Depth: ",depth,
            #                            "<br>Count: ",count))) +
            scale_x_reverse() +
            coord_flip(expand = F) +
            # scale_fill_manual(values = CatColAbr, aesthetics = c("colour", "fill")) +
            # guides(fill = guide_legend(nrow = length(unique(full_product$var_type)))) +
            labs(x = NULL, fill = "Driver", y = "Count (log10)") +
            theme(panel.border = element_rect(fill = NA, colour = "black"), legend.position = "none")
        
        if(nrow(df_filter) > 0){
            basePlot <- basePlot + geom_col(data = df_filter, aes(x = depth, y = log10(count), fill = var_name,
                                                                  text = paste0("Driver: ",var_name,
                                                                                "<br>Depth: ",depth,
                                                                                "<br>Count: ",count)))
        }
        
        ggplotly(basePlot, tooltip = "text")# %>% config(displayModeBar = F) %>% 
            # layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

