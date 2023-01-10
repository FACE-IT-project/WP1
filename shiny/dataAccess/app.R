# shiny/dataAccess/app.R
# The code used to generate the UI for accessing the FACE-IT data product

# TODO:
# HiRes coastline for smaller sites


# Setup -------------------------------------------------------------------

# setwd("shiny/dataAccess/") # For in-app testing
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(arrow)
library(ggplot2)
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
    "Social" = "#F48080"
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
# input <- data.frame(cleanVSfull = "clean",
#                     selectSite = "is",
#                     selectCat = c("cryo", "chem"),
#                     selectDriv = c("sea ice", "nutrients"))

# Full data file paths
full_data_paths <- dir("full_data", full.names = T)

# Named sites for subsetting paths
sites_named <- list(Svalbard = c("Kongsfjorden" = "kong", "Isfjorden" = "is", "Storfjorden" = "stor"),
                    # "Svalbard" = "sval",
                    Greenland = c("Young Sound" = "young", "Disko Bay" = "disko", "Nuup Kangerlua" = "nuup"),
                    Norway = c("Porsangerfjorden" = "por"))

# Long and short names for categories and drivers
long_names <- data.frame(category = c("cryo", "cryo", "cryo", "phys", "phys", "phys",
                                      "chem", "chem", "bio", "bio", "bio", "soc", "soc", "soc"),
                         category_long = c("cryosphere", "cryosphere", "cryosphere", "physics", "physics", "physics",
                                           "chemistry", "chemistry", "biology", "biology", "biology",
                                           "social", "social", "social"),
                         driver = c("sea ice", "glacier", "runoff", "sea temp", "salinity", "light",
                                    "carb", "nutrients", "prim prod", "biomass", "spp rich", 
                                    "gov", "tourism", "fisheries"),
                         driver_long = c("sea ice", "glacier mass balance", "terrestrial runoff",
                                         "seawater temperature", "salinity", "light",
                                         "carbonate chemistry", "nutrients",
                                         "primary production", "biomass", "species richness",
                                         "governance", "tourism", "fisheries")) %>% 
  mutate(driver_long = factor(driver_long, 
                              levels = c("sea ice", "glacier mass balance", "terrestrial runoff",
                                         "seawater temperature", "salinity", "light",
                                         "carbonate chemistry", "nutrients",
                                         "primary production", "biomass", "species richness",
                                         "governance",  "tourism", "fisheries")))

# Bounding boxes
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(12.97, 17.50, 77.95, 78.90)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.08)
bbox_stor <- c(17.35, 21.60, 77.33, 78.13)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8)
bbox_por <- c(24.5, 27, 70, 71.2)

# The base global map
# NB: Considered creating smaller maps, but wasn't effective
map_base <- readRDS("map_base.Rda")

# Load PANGAEA driver metadata sheet
## NB: Code only run once to get slimmed down parameter list
# pg_parameters <- read_delim_arrow("~/WP1/metadata/pangaea_parameters.tab", delim = "\t")
# if(!exists("clean_all")) clean_all <- map_dfr(full_data_paths[grepl("clean_", full_data_paths)], read_csv_arrow)
# param_list <- distinct(select(clean_all, category, driver, variable)) %>%
#   mutate(var_name = sapply(base::strsplit(variable, " \\["), "[[", 1)) %>%
#   left_join(pg_parameters, by = c("var_name" = "Abbreviation")) %>%
#   dplyr::select(category, driver, variable, Parameter) %>% distinct() %>%
#   left_join(long_names, by = c("category", "driver")) %>% 
#   dplyr::rename(Category = category_long, Driver = driver_long, Variable = variable, Name = Parameter) %>% 
#   dplyr::select(Category, Driver, Variable, Name)
# write_csv_arrow(param_list, "~/WP1/shiny/dataAccess/param_list.csv")
param_list <- read_csv_arrow("param_list.csv")


# UI ----------------------------------------------------------------------

# Define UI
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
            'selectSite', '1. Study site',
            choices = sites_named,
            options = list(
                placeholder = 'Select a site to begin',
                onInitialize = I('function() { this.setValue(""); }'))
        ),
        
        # Category - 2. Category(s)
        uiOutput("selectCatUI"),
        
        # Drivers - 3. Driver(s)
        uiOutput("selectDrivUI"),
        
        # Variables - 4. Variable(s)
        uiOutput("selectVarUI"),
        
        # Switch for Clean vs Full data
        # shinyWidgetsGallery()
        # radioGroupButtons(
        #   inputId = "cleanVSfull",
        #   label = "5. Clean or full data", 
        #   selected = "clean",
        #   choices = c("Clean" = "clean", "Full" = "full"),
        #   status = "warning"
        # ),
        
        # Filter PANGAEA data
        # radioGroupButtons(
        #     inputId = "PANGAEAfilter",
        #     label = "4. Filter PANGAEA", 
        #     choices = c("Yes", "No"), 
        #     selected = "No",
        #     status = "warning"
        # ),
        
        # Download
        radioButtons("downloadFilterType", "5. Download", choices = c(".csv", ".Rds"), 
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
    ),
    
    # Info, filters, and plots for the selected data
    dashboardBody(
        fluidRow(
            column(width = 3,
                   box(width = 12, height = "380px", title = "Data info",
                       status = "warning", solidHeader = TRUE, collapsible = FALSE,
                       DT::dataTableOutput("longVarDT")),
                   box(width = 12, height = "440px", title = "Filter data",
                       status = "warning", solidHeader = TRUE, collapsible = FALSE,
                       uiOutput("slideLonUI"), uiOutput("slideLatUI"),
                       uiOutput("slideDepthUI"), uiOutput("slideDateUI"))),
            column(width = 6,
                   box(width = 12, height = "410px", title = "Lon/lat",
                       status = "info", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("mapPlot", height = "355px"), 
                                                    type = 6, color = "#b0b7be")),
                   box(width = 12, height = "410px", title = "Date",
                       status = "primary", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("tsPlot", height = "355px"), 
                                                    type = 6, color = "#b0b7be"))),
            column(width = 3,
                   box(width = 12, height = "840px", title = "Depth",
                       status = "success", solidHeader = TRUE, collapsible = FALSE,
                       shinycssloaders::withSpinner(plotlyOutput("depthPlot", height = "780px"), 
                                                    type = 6, color = "#b0b7be")))
        )
    ),
    
    title = "Data Access",
    skin = "purple"
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

    
    ## Reactive UI -------------------------------------------------------------
    
    # Select drivers
    output$selectCatUI <- renderUI({
    # req(input$selectSite)
    
    if(input$selectSite %in%  c("kong", "is", "stor", "young", "disko", "nuup", "por")){
      cat_choices <- c("Cryosphere" = "cryo", "Physical" = "phys", "Chemistry" = "chem",
                       "Biology" = "bio", "Social" = "soc")
    } else{
      cat_choices <- ""
    }
    
    selectizeInput(
      'selectCat', '2. Category(s)',
      # choices = unique(df_load()$var_type),
      choices = cat_choices,
      multiple = T,
      options = list(
        placeholder = 'Select category(s)',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  
    # Select drivers
    output$selectDrivUI <- renderUI({
        # req(input$selectCat)
        
        if(input$selectSite %in%  c("kong", "is", "stor", "young", "disko", "nuup", "por") & length(input$selectCat) > 0){
            driv_choices_base <- droplevels(unique(filter(long_names, category %in% input$selectCat)))
            driv_choices <- as.character(driv_choices_base$driver)
            names(driv_choices) <- driv_choices_base$driver_long
        } else{
            driv_choices <- ""
        }
        
        selectizeInput(
            'selectDriv', '3. Driver(s)',
            choices = driv_choices, multiple = T,
            options = list(
                placeholder = 'Select driver(s)',
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    # Select variables
    output$selectVarUI <- renderUI({
      # req(df_driv())
      
      if(input$selectSite %in%  c("kong", "is", "stor", "young", "disko", "nuup", "por") & 
         length(input$selectCat) > 0 & length(input$selectDriv) > 0){
        var_choices <- unique(df_driv()$variable)
      } else{
        var_choices <- ""
      }
      
      selectizeInput(
        'selectVar', '4. Variable(s)',
        choices = var_choices, multiple = T,
        options = list(
          placeholder = 'Select variable(s)',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    })
    
    # List of long names for variables
    output$longVarDT <- DT::renderDataTable({
      # req(input$selectCat)
      
      df_names <- param_list
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


  ## Instructions ------------------------------------------------------------

    observeEvent(input$info, {
        sendSweetAlert(
            session = session,
            title = "Instructions",
            text = tags$span(
                "The FACE-IT data access app is laid out as a dashboard with a ",tags$em("side bar"),
                " on the left and additional information and plots in the ",tags$em("body"),".",
                tags$h3(icon("bars"),"Side bar", style = "color: cadetblue;"),
                tags$h4(tags$b("1. Study site"),": Select one of the seven FACE-IT study sites to begin the data exploration process. After selecting a site the 'Lon/lat' figure in the body will display a rough map."),
                tags$h4(tags$b("2. Category(s)"),": Categories are the broadest classification possible of the data. Select one or more values to move to step 3."),
                # tags$h4(icon("hammer"),tags$b("3. Clean or full data"),": Much of the data sourced for FACE-IT are formatted differently from one another. In an effort to adress this issue the data are undergoing an additional level of cleaning. This is currently under construction."),
                # tags$h4(tags$b("4. Filter PANGAEA"),": The data downloaded from PANGAEA are voluminous, but often contain data that are not of interest. Switch this value to 'Yes' to remove PANGAEA data from the list of drivers selected in the next step. ."),
                tags$h4(tags$b("3. Driver(s)"),": Drivers are more specific than categories. One may select multiple values here."),
                tags$h4(tags$b("4. Variable(s)"),": Variables are specific types of measurements of the social/natural world. The values available here are a product of steps 2+3."),
                tags$h4(tags$b("5. Download"),": When one is happy with the selected/filtered data they may be downloaded here. First choose the file type, then click the download button."),
                tags$h3(icon("images"),"Body", style = "color: steelblue;"),
                tags$h4(tags$b("Data info"),": This menu allows the user to search for more information for the variables seen in ", tags$em("4. Variable(s)"), "in the sidebar."),
                tags$h4(tags$b("Filter data"),": The ranges in the data for: longitude, latitude, depth, and date will be displayed here. Moving these sliders will filter out the data and this will be shown in the figures in the body."),
                tags$h4(tags$b("Lon/lat"),": Displays a map of where the selected data are located. Different drivers are shown by shape, and colours show variables. Mouse over the dots to see more information."),
                tags$h4(tags$b("Date"),": Displays the selected data as a time series. Mouse over the points to see more information."),
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
                saveRDS(df_filter()[["filter_data"]], file = file)
            } else if(input$downloadFilterType == ".csv"){
                arrow::write_csv_arrow(df_filter()[["filter_data"]], file)
            }
        }
    )
    
    
    ## Process data ------------------------------------------------------------
    
    # Subset by category
    df_driv <- reactive({
      req(input$selectDriv)
      
      # Assemble possible files
      # file_choice <- expand.grid(input$cleanVSfull, input$selectCat, input$selectDriv, input$selectSite) %>% 
      file_choice <- expand.grid("clean", input$selectCat, input$selectDriv, input$selectSite) %>% 
        unite(col = "all", sep = "_")
      file_list <- full_data_paths[grepl(paste(file_choice$all, collapse = "|"), full_data_paths)]

      # Load initial data
      df_driv <- purrr::map_dfr(file_list, read_csv_arrow)
      if(nrow(df_driv) > 0){
        df_driv <- df_driv %>% 
          mutate(date = as.Date(date)) %>% 
          filter(!grepl("g-e-m", URL), # Remove GEM data
                 !grepl("GRDC", URL), # Remove GRDC data
                 !grepl("Received directly from Mikael Sejr", URL)) # Remove embargoed PAR data from Mikael
      }

      # Filter PANGAEA data
      # if(input$PANGAEAfilter == "Yes"){
      #     df_driv <- filter(df_driv, !grepl("PANGAEA", URL))
      # }

      # Exit
      return(df_driv)
    })
    
    # Subset by variable name
    df_var <- reactive({
        req(input$selectVar)
        df_var <- df_driv() %>% 
            filter(variable %in% input$selectVar)
        return(df_var)
    })
    
    # Filter by smaller details 
    df_filter <- reactive({
      req(df_var())
        if(length(input$selectVar) == 0){
            df_filter <- data.frame(date = as.Date("2000-01-01"), value = NA, var_name = "No variables selected",
                                    lon = NA, lat = NA, depth = NA)
        } else if(length(input$selectVar) > 0){
            req(input$slideDate)
            df_filter <- df_var()
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
      
      # Count data for map
      df_filter_map <- df_filter %>% 
        filter(!is.na(lon), !is.na(lat)) %>% 
        mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
        group_by(lon, lat, category, driver, variable) %>% 
        summarise(count = n(), .groups = "drop") %>% 
        left_join(long_names, by = c("category", "driver"))
      if(nrow(df_filter_map) == 0) 
        df_filter_map <- data.frame(lon = NA, lat = NA, driver = NA, variable = NA, 
                                    category_long = NA, driver_long = NA, count = NA)
      
      # Count data for time series
      df_filter_ts <- df_filter %>% 
        filter(!is.na(date)) %>% 
        mutate(year = year(date)) %>% 
        group_by(year, category, driver, variable) %>% 
        summarise(count = n(), .groups = "drop") %>% 
        left_join(long_names, by = c("category", "driver"))
      
      # Count data for depth plot
      df_filter_depth <- df_filter %>% 
        filter(!is.na(depth)) %>% 
        mutate(depth = round(depth, -1)) %>%
        group_by(depth, category, driver, variable) %>% 
        dplyr::summarise(count = n(), .groups = "drop") %>% 
        left_join(long_names, by = c("category", "driver"))
      
      # Compile list and exit
      df_final <- list(filter_data = df_filter,
                       map_count = df_filter_map,
                       ts_count = df_filter_ts,
                       depth_count = df_filter_depth)
      return(df_final)
    })

    
    ## Map ---------------------------------------------------------------------
    
    output$mapPlot <- renderPlotly({
      req(input$selectSite)
      
      # Base map reacts to site selection
      if(input$selectSite == "por") bbox_name <- bbox_por
      if(input$selectSite == "kong") bbox_name <- bbox_kong
      if(input$selectSite == "is") bbox_name <- bbox_is
      if(input$selectSite == "stor") bbox_name <- bbox_stor
      if(input$selectSite == "young") bbox_name <- bbox_young
      if(input$selectSite == "disko") bbox_name <- bbox_disko
      if(input$selectSite == "nuup") bbox_name <- bbox_nuup
      
      # Get buffer area for plot
      xmin <- bbox_name[1]-(bbox_name[2]-bbox_name[1])/4
      xmax <- bbox_name[2]+(bbox_name[2]-bbox_name[1])/4
      ymin <- bbox_name[3]-(bbox_name[4]-bbox_name[3])/8
      ymax <- bbox_name[4]+(bbox_name[4]-bbox_name[3])/8
      
      # Filter map size for minor speed boost
      map_base_sub <- filter(map_base, between(lon, bbox_name[1]-5, bbox_name[1]+5),
                             between(lon, bbox_name[3]-3, bbox_name[4]+3))
      
      # Show data filtered by lon/lat sliders
      basePlot <- ggplot() + 
        geom_polygon(data = map_base, fill = "grey80", colour = "black",
                     aes(x = lon, y = lat, group = group, text = "Land")) +
        coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
        labs(x = NULL, y = NULL, shape = "Driver", fill = "Variable") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
              axis.text = element_text(size = 12, colour = "black"),
              axis.ticks = element_line(colour = "black"), legend.position = "none")
      
      if(length(input$selectVar) > 0){
        # Data count for map
        df_filter_map <- df_filter()[["map_count"]]
        
        basePlot <- basePlot + 
          geom_point(data = df_filter_map,
                     aes(x = lon, y = lat, shape = driver, colour = variable,
                         text = paste0("Category: ",category_long,
                                       "<br>Driver: ",driver_long,
                                       "<br>Variable: ",variable,
                                       "<br>Lon: ",lon,
                                       "<br>Lat: ",lat,
                                       "<br>Count: ",count)))
      }
      
      ggplotly(basePlot, tooltip = "text")
      
    })
    
    
    ## Time series -------------------------------------------------------------

    output$tsPlot <- renderPlotly({
      req(input$selectVar)
      
      df_filter_ts <- df_filter()[["ts_count"]]

      # Plot and exit
      if(nrow(df_filter_ts) > 0){
      basePlot <- ggplot(data = df_filter_ts, aes(x = year, y = log10(count))) +
        geom_point(aes(shape = driver, colour = variable,
                       text = paste0("Category: ",category_long,
                                     "<br>Driver: ",driver_long,
                                     "<br>Variable: ",variable,
                                     "<br>Year: ",year,
                                     "<br>Count: ",count))) +
        labs(x = NULL, y = "Count (log10)", colour = "Variable") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
              axis.text = element_text(size = 12, colour = "black"),
              axis.ticks = element_line(colour = "black"), legend.position = "none")
      } else {
        basePlot <- ggplot() + geom_blank()
      }
      
      ggplotly(basePlot, tooltip = "text")
      
    })
    

    ## Depth plot --------------------------------------------------------------

    output$depthPlot <- renderPlotly({
      req(input$selectVar)
      # req(df_filter())
      
      df_filter_depth <- df_filter()[["depth_count"]]
      
      # Count of data at depth by var name
      if(nrow(df_filter_depth) > 0){
      basePlot <- ggplot(df_filter_depth, aes(x = depth, y = log10(count))) +
        geom_col(aes(fill = variable,
                     text = paste0("Category: ",category_long,
                                   "<br>Driver: ",driver_long,
                                   "<br>Variable: ",variable,
                                   "<br>Depth: ",depth,
                                   "<br>Count: ",count))) +
        scale_x_reverse() + coord_flip(expand = F) +
        # scale_fill_manual(values = CatColAbr, aesthetics = c("colour", "fill")) +
        # guides(fill = guide_legend(nrow = length(unique(full_product$var_type)))) +
        labs(x = NULL, fill = "Variable", y = "Count (log10)") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
              axis.text = element_text(size = 12, colour = "black"),
              axis.ticks = element_line(colour = "black"), legend.position = "none")
      } else {
        basePlot <- ggplot() + geom_blank()
      }
      
      ggplotly(basePlot, tooltip = "text")# %>% config(displayModeBar = F) %>% 
      # layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

