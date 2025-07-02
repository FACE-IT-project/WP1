# shiny/dataAccess/app.R
# The code used to generate the UI for accessing the FACE-IT data product


# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(dplyr)
library(tidyr)
# library(purrr)
library(lubridate)
library(arrow)
library(ggplot2)
# library(plotly)

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

# Enable thematic 
# thematic::thematic_shiny(font = "auto")


# Data --------------------------------------------------------------------

# For testing
# setwd("shiny/dataAccess/")
# input <- data.frame(cleanVSfull = "clean",
#                     selectSite = "is",
#                     selectCat = c("cryo", "chem"),
#                     selectDriv = c("sea ice", "nutrients"),
#                     selectSiteSummary = c("stor", "young"),
#                     selectCatSummary = c("chem"),
#                     selectDrivSummary = c("carb", "nutrients"),
#                     selectColourSummary = "Site")

# Embargoed data
data_shadow <- "g-e-m|GRDC|Received directly from Mikael Sejr"

# Full data
df_full <- loadRData("full_data/clean_all.RData")

# Full data file paths
full_data_paths <- dir("full_data", full.names = T)

# Named sites for subsetting paths
sites_named <- list(Svalbard = c("Kongsfjorden" = "kong", "Isfjorden" = "is", "Storfjorden" = "stor"),
                    # "Svalbard" = "sval",
                    Greenland = c("Young Sound" = "young", "Disko Bay" = "disko", "Nuup Kangerlua" = "nuup"),
                    Norway = c("Porsangerfjorden" = "por"))

# Long and short names for sites
long_sites <- data.frame(site = c("kong", "is", "stor", "young", "disko", "nuup", "por"),
                         site_long = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                       "Young Sound", "Disko Bay", "Nuup Kangerlua", "Porsangerfjorden"))

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
                                         "governance", "tourism", "fisheries")) |> 
  mutate(category_long = factor(category_long,
                                levels = c("cryosphere", "physics", "chemistry", "biology", "social")),
         driver_long = factor(driver_long, 
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

# The base site maps
map_base <- readRDS("map_base.Rda")
map_hi <- read_csv_arrow("coastline_hi_sub.csv")

# Load site data summary
## NB: Code run once to generate spreadsheet
# if(!exists("clean_all")) load("~/WP1/data/analyses/clean_all.RData")
# clean_meta <- clean_all |>
#   filter(site %in% long_sites$site) |>
#   group_by(site, category, driver) |>
#   summarise(count = n(), .groups = "drop") |>
#   tidyr::complete(site, category, driver) |>
#   left_join(long_names, by = c("category", "driver")) |>
#   left_join(long_sites, by = "site") |>
#   filter(!is.na(driver_long)) |>
#   mutate(count = replace_na(count, 0)) |>
#   dplyr::select(site, site_long, category, category_long, driver, driver_long, count)
# save(clean_meta, file = "~/WP1/shiny/dataAccess/clean_meta.RData")
load("clean_meta.RData")

# Load citation summary
# if(!exists("clean_all")) load("~/WP1/data/analyses/clean_all.RData")
# clean_citation <- clean_all |>
#   filter(site %in% long_sites$site) |>
#   group_by(date_accessed, URL, citation, site, category, driver) |>
#   summarise(count = n(), .groups = "drop") |>
#   left_join(long_names, by = c("category", "driver")) |>
#   left_join(long_sites, by = "site") |>
#   filter(!is.na(driver_long)) |>
#   dplyr::select(date_accessed, URL, citation, site_long, category_long, driver_long, count)
# save(clean_citation, file = "~/WP1/shiny/dataAccess/clean_citation.RData")
load("clean_citation.RData")

# Load PANGAEA driver metadata sheet
## NB: Code only run once to get slimmed down parameter list
# pg_parameters <- read_delim_arrow("~/WP1/metadata/pangaea_parameters.tab", delim = "\t")
# if(!exists("clean_all")) clean_all <- map_dfr(full_data_paths[grepl("clean_", full_data_paths)], read_csv_arrow)
# param_list <- distinct(select(clean_all, category, driver, variable)) |>
#   mutate(var_name = sapply(base::strsplit(variable, " \\["), "[[", 1)) |>
#   left_join(pg_parameters, by = c("var_name" = "Abbreviation")) |>
#   dplyr::select(category, driver, variable, Parameter) |> distinct() |>
#   left_join(long_names, by = c("category", "driver")) |> 
#   dplyr::rename(Category = category_long, Driver = driver_long, Variable = variable, Name = Parameter) |> 
#   dplyr::select(Category, Driver, Variable, Name)
# write_csv_arrow(param_list, "~/WP1/shiny/dataAccess/param_list.csv")
param_list <- read_csv_arrow("param_list.csv")

# Static category choices
cat_choices <- c("Cryosphere" = "cryo", "Physical" = "phys", "Chemistry" = "chem",
                 "Biology" = "bio", "Social" = "soc")
# driv_choices_base <- long_names
# driv_choices <- as.character(driv_choices_base$driver)
# names(driv_choices) <- driv_choices_base$driver_long


# UI ----------------------------------------------------------------------

# Define UI
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(
    title = "Data Access"
    # This is an interesting approach:
    # https://ashbaldry.github.io/2021-06-17-shinytitle-0-1-0-release/
  ),
  
  # Sidebar
  dashboardSidebar(width = 200,
                   
                   # FACE-IT logo
                   tags$a(href = "https://www.face-it-project.eu/",
                          target = "_blank", rel = "noopener noreferrer",
                          tags$img(src = "FACE-IT_h2020.jpg",
                                   alt = "FACE-IT",
                                   height = "220", width = "200"),
                          style = "vertical-align:middle;margin:0px 0px"),
                   
                   # User manual pop-up
                   br(), br(),
                   useSweetAlert(),
                   actionBttn(
                     inputId = "instructions",
                     label = "User manual",
                     icon = icon("book"),
                     style = "material-flat",
                     color = "success"
                   ),
                   br(),
                   
                   # Sidebar menu with multiple drop downs
                   sidebarMenu(
                     id = "tabs",
                     menuItem("Data summary", tabName = "summary", icon = icon("book"), selected = TRUE),
                     menuItem("Data download", tabName = "download", icon = icon("download")),
                     menuItem("Advanced search", tabName = "information", icon = icon("table")),
                     menuItem("Related pubs.", tabName = "publications", icon = icon("scroll"))
                   )
  ),
  
  # Info, filters, and plots for the selected data
  dashboardBody(
    # Allow modal window to adjust width correctly
    tags$style(
      type = 'text/css',
      '.modal-dialog { width: fit-content !important; }'
    ),
    # Custom CSS to change app colours
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #4472c4;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #4472c4;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #4472c4;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #008980;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #7fad24;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #008980;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #7fad24;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #008980;
                              }
                              '))),
    tabItems(
      
      tabItem(tabName = "summary",
              fluidRow(
                column(width = 3,
                       box(width = 12, title = "Filter data",
                           status = "warning", solidHeader = TRUE, collapsible = FALSE,
                           
                           # Load full clean_all dataset
                           # h5(HTML("<b>0. Load full dataset (optional)</b>")),
                           # shiny::actionButton("loadCleanAll", "", class = "btn-success", icon = icon("book")),
                           
                           # Site name
                           selectizeInput(
                             'selectSiteSummary', '1. Study site',
                             choices = sites_named,
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Select a site to begin',
                               onInitialize = I('function() { this.setValue(""); }'))
                           ),
                           
                           # Category - 2. Category(s)
                           selectizeInput(
                             'selectCatSummary', '2. Category(s)',
                             choices = cat_choices,
                             multiple = T,
                             options = list(
                               placeholder = 'Select category(s)',
                               onInitialize = I('function() { this.setValue(""); }')
                             )
                           ),
                           
                           # Drivers - 3. Driver(s)
                           uiOutput("selectDrivSummaryUI"),
                       ),
                       box(width = 12, title = "Summarise data",
                           status = "warning", solidHeader = TRUE, collapsible = FALSE,

                           # High level grouping
                           selectInput(
                             'selectGroupSummary', '1. Grouping (colour)',
                             choices = c("Category", "Driver", "Variable"),
                             selected = "Category",
                             ),

                           # Date grouping
                           selectInput(
                             'selectDateSummary', '2. Date (x-axis)',
                             choices = c("Year", "Climatology", "Day"),
                             selected = "Year"
                             ),

                           # Data grouping
                           selectInput(
                             'selectDataSummary', '3. Data (y-axis)',
                             choices = c("Data points", "Real values", "Unique variables", "Unique citations"),
                             selected = "Data points"
                             ),
                           
                           # Plot type
                           selectInput(
                             'selectPlotSummary', '4. Plot type', 
                             choices = c("Bar plot", "Dot plot"),
                             selected = "Bar plot"
                           )
                       )
                ),
                
                # Map and time series plots
                column(width = 9,
                       box(width = 12, title = "Summary", height = "850px", 
                           status = "info", solidHeader = TRUE, collapsible = FALSE,
                           # shinycssloaders::withSpinner(plotlyOutput("summaryPlotly", height = "750px"),
                           #                              type = 6, color = "#b0b7be"))
                           shinycssloaders::withSpinner(plotOutput("summaryPlotly", height = "750px"),
                                                        type = 6, color = "#b0b7be"))
                       )
                )
      ),
      
      tabItem(tabName = "download",
              fluidRow(
                column(width = 3,
                       box(width = 12, title = "Filter data",
                           status = "warning", solidHeader = TRUE, collapsible = FALSE,
                           
                           # Site name
                           selectizeInput(
                             'selectSite', '1. Study site',
                             multiple = TRUE,
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
                           
                           # Filter - 5. lon 
                           uiOutput("slideLonUI"), 
                           
                           # Filter - 6. lat
                           uiOutput("slideLatUI"),
                           
                           # Filter -7. Date
                           uiOutput("slideDateUI"), 
                           
                           # Filter - 8. Depth
                           uiOutput("slideDepthUI"), 
                           
                           fluidRow(
                             
                             # Process - 9. Load
                             column(width = 6,
                                    h5(tags$b("9. Filter data")),
                                    uiOutput("filterDataUI")),
                             
                             # Download - 10. Download
                             column(width = 6,
                                    h5(tags$b("10. Preview data")),
                                    uiOutput("previewDataUI"))
                           )
                       )
                ),
                
                # Map and time series plots
                column(width = 6,
                       box(width = 12, title = "Location", height = "410px", 
                           status = "info", solidHeader = TRUE, collapsible = FALSE,
                           # shinycssloaders::withSpinner(plotlyOutput("mapPlot", height = "355px"), 
                           #                              type = 6, color = "#b0b7be")),
                           shinycssloaders::withSpinner(plotOutput("mapPlot", height = "355px"), 
                                                        type = 6, color = "#b0b7be")),
                       box(width = 12, title = "Date", height = "410px", 
                           status = "primary", solidHeader = TRUE, collapsible = FALSE,
                           # shinycssloaders::withSpinner(plotlyOutput("tsPlot", height = "355px"), 
                           #                              type = 6, color = "#b0b7be"))),
                           shinycssloaders::withSpinner(plotOutput("tsPlot", height = "355px"), 
                                                        type = 6, color = "#b0b7be"))),
                
                # Depth plot
                column(width = 3,
                       box(width = 12, height = "840px", title = "Depth",
                           status = "success", solidHeader = TRUE, collapsible = FALSE,
                           # shinycssloaders::withSpinner(plotlyOutput("depthPlot", height = "780px"), 
                           #                              type = 6, color = "#b0b7be")))
                           shinycssloaders::withSpinner(plotOutput("depthPlot", height = "780px"), 
                                                        type = 6, color = "#b0b7be")))
              )
      ),
      
      # Data tables
      tabItem(tabName = "information",
              box(width = 12, title = "Data availability per citation",
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  DT::dataTableOutput("countCitationDT")),
              box(width = 12, title = "Data availability per site",
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  DT::dataTableOutput("countDrivDT")),
              box(width = 12, title = "Full variable names",
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  DT::dataTableOutput("longVarDT"))
              ),

      # Data tables
      tabItem(tabName = "publications",
              fluidPage(
                column(12,
                       h2(tags$b("PANGAEA")),
                       p("The data hosted on this portal are published on ",
                         tags$a("PANGAEA", target = "_blank",
                                href = "https://doi.pangaea.de/10.1594/PANGAEA.953115"),
                         ", where they may be downloaded in their entirety."),
                       h2(tags$b("ESSD")),
                       p("For an in-depth analysis of these data, as well as the details on how they were collected, please see the paper
                         currently in review at ",
                         tags$a("Earth Systems Science Data (ESSD)", target = "_blank",
                                href = "https://essd.copernicus.org/preprints/essd-2022-455/"), "."),
                       img(src = "dp_fig_1.png", align = "center", width = "1000px"),
                       p("Figure 1 from ESSD paper. Map of the EU Arctic showing the decadal trends in sea surface temperature (SST).
                         Pop-out windows for each of the seven FACE-IT study sites show the annual trend in sea-ice cover days."),
                       h2(tags$b("Coastal futures")),
                       p("The rational for the data collected, and for how they are classified (e.g. categories + drivers) are 
                         detailed in a publication available at ",
                         tags$a("coastal futures", target = "_blank",
                                href = "https://tinyurl.com/5ejsjpc7"), "."),
                       img(src = "rp_fig_2.png", align = "center", width = "1000px"),
                       p("Figure 2 from Coastal Futures paper. A network plot showing the trends in the 14 key drivers of
                          change in EU Arctic fjords and what effects they are having on each other.")
                       
                )
              )
      )
    )
  ),
  # NB: Don't change colour here. This is done above via in-line CSS.
  # skin = "purple"
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  ## Reactive UI -------------------------------------------------------------
  
  # Data downloading
  ## Select categories
  output$selectCatUI <- renderUI({
    if(any(c("kong", "is", "stor", "young", "disko", "nuup", "por") %in%  input$selectSite)){
      cat_choices <- c("Cryosphere" = "cryo", "Physical" = "phys", "Chemistry" = "chem",
                       "Biology" = "bio", "Social" = "soc")
    } else{
      cat_choices <- ""
    }
    selectizeInput(
      'selectCat', '2. Category(s)',
      choices = cat_choices,
      multiple = T,
      options = list(
        placeholder = 'Select category(s)',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  
  ## Select drivers
  output$selectDrivUI <- renderUI({
    if(any(c("kong", "is", "stor", "young", "disko", "nuup", "por") %in% input$selectSite) & length(input$selectCat) > 0){
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
  
  ## Select variables
  output$selectVarUI <- renderUI({
    if(any(c("kong", "is", "stor", "young", "disko", "nuup", "por") %in% input$selectSite) & 
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
  
  ## Select drivers
  output$selectDrivSummaryUI <- renderUI({
    if(any(c("kong", "is", "stor", "young", "disko", "nuup", "por") %in% input$selectSiteSummary) & length(input$selectCatSummary) > 0){
      driv_choices_base <- droplevels(unique(filter(long_names, category %in% input$selectCatSummary)))
      driv_choices <- as.character(driv_choices_base$driver)
      names(driv_choices) <- driv_choices_base$driver_long
    } else{
      driv_choices <- ""
    }
  selectizeInput(
    'selectDrivSummary', '3. Driver(s)',
    choices = driv_choices, multiple = T,
    options = list(
      placeholder = 'Select driver(s)',
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
  })
  
  ## Select colour groupings
  # output$selectColourSummaryUI <- renderUI({
  #   colour_group_base <- c("Site", "Category")
  #   if(input$selectGroupSummary == "Driver"){
  #     colour_group_group <- unique(c(colour_group_base, "Driver"))
  #   } else if(input$selectGroupSummary == "Variable"){
  #     colour_group_group <- unique(c(colour_group_base, "Driver", "Variable"))
  #   } else {
  #     colour_group_group <- colour_group_base
  #   }
  #   if(input$selectDataSummary == "Variable"){
  #     colour_group_data <- unique(c(colour_group_group, "Variable"))
  #   } else if(input$selectDataSummary == "Citation"){
  #     colour_group_data <- unique(c(colour_group_group, "Citation"))
  #   } else {
  #     colour_group_data <- colour_group_group
  #   }
  #   selectInput(
  #     'selectColourSummary', '1. Group colour',
  #     choices = colour_group_data,
  #     selected = "Site"
  #   )
  # })
  
  # Filter data
  ## Lon
  output$slideLonUI <- renderUI({
    if(length(input$selectVar) == 0){
      min_val <- 0; max_val <- 0
    } else if(length(na.omit(df_var()$lon)) == 0){
      min_val <- 0; max_val <- 0
    } else {
      min_val <- round(min(df_var()$lon, na.rm = T), 2)-0.01
      max_val <- round(max(df_var()$lon, na.rm = T), 2)+0.01
    }
    shiny::sliderInput("slideLon", "5. Longitude range", value = c(min_val, max_val), min = min_val, max = max_val)
  })
  
  ## Lat
  output$slideLatUI <- renderUI({
    if(length(input$selectVar) == 0){
      min_val <- 0; max_val <- 0
    } else if(length(na.omit(df_var()$lat)) == 0){
      min_val <- 0; max_val <- 0
    } else {
      min_val <- round(min(df_var()$lat, na.rm = T), 2)-0.01
      max_val <- round(max(df_var()$lat, na.rm = T), 2)+0.01
    }
    shiny::sliderInput("slideLat", "6. Latitude range", value = c(min_val, max_val), min = min_val, max = max_val)
  })
  
  ## Date
  output$slideDateUI <- renderUI({
    if(length(input$selectVar) == 0){
      min_val <- 0; max_val <- 0
    } else if(length(na.omit(df_var()$date)) == 0){
      min_val <- as.Date("2000-01-01"); max_val <- as.Date("2000-01-01")
    } else {
      min_val <- min(df_var()$date, na.rm = T)
      max_val <- max(df_var()$date, na.rm = T)
    }
    shiny::sliderInput("slideDate", "7. Date range", value = c(min_val, max_val), min = min_val, max = max_val)
  })
  
  ## Depth
  output$slideDepthUI <- renderUI({
    if(length(input$selectVar) == 0){
      min_val <- 0; max_val <- 0
    } else if(length(na.omit(df_var()$depth)) == 0){
      min_val <- 0; max_val <- 0
    } else {
      min_val <- min(df_var()$depth, na.rm = T)
      max_val <- max(df_var()$depth, na.rm = T)
    }
    shiny::sliderInput("slideDepth", "8. Depth range", value = c(min_val, max_val), min = min_val, max = max_val)
  })
  
  # Filter button
  observeEvent(c(input$selectSite, input$selectCat, input$selectDriv, input$selectvar), {
    output$filterDataUI <- renderUI({
      if(!any(input$selectSite %in% long_sites$site)) {
        actionBttn(inputId = "filterNA", label = "Select site", color = "danger", size = "sm", block = TRUE, style = "pill")
      } else if(length(input$selectCat) == 0){
        actionBttn(inputId = "filterNA", label = "Select category(s)", color = "danger", size = "sm", block = TRUE, style = "pill")
      } else if(length(input$selectDriv) == 0){
        actionBttn(inputId = "filterNA", label = "Select driver(s)", color = "danger", size = "sm", block = TRUE, style = "pill")
      } else if(length(input$selectVar) == 0){
        actionBttn(inputId = "filterNA", label = "Select variable(s)", color = "danger", size = "sm", block = TRUE, style = "pill")
      } else if(length(input$selectVar) > 0){
        actionBttn(inputId = "filterData", label = "Filter", color = "success")
      }
    })
  })
  filter_button <- reactiveValues(clicked = FALSE)
  observeEvent(input$filterData, {
    if(!filter_button$clicked) filter_button$clicked <- TRUE
  })
  
  # Data preview button
  output$previewDataUI <- renderUI({
    if(!filter_button$clicked) {
      actionBttn(inputId = "previewNA", label = "...", color = "danger", size = "sm", block = TRUE, style = "pill")
    } else {
      actionBttn(inputId = "previewData", label = "Preview", color = "success")
    }
  })
  
  # Open the modal panel
  observeEvent(input$previewData, {
    showModal(modalDialog(
      title = "Filtered data to download",
      easyClose = TRUE,
      DT::dataTableOutput("filterDT"),
      footer = fluidRow(
        column(width = 2,
               radioButtons("downloadFilterType", "Download data as",
                            choices = c(".csv", ".Rds"), selected = ".csv", inline = T),
               downloadButton("downloadFilter", "Download")))
    ))
  })
  
  
  ## Process data ------------------------------------------------------------
  
  # Subset by category+driver
  df_driv <- reactive({
    req(input$selectDriv)
    
    # Assemble possible files
    file_choice <- expand.grid("clean", input$selectCat, input$selectDriv, input$selectSite) |> 
      unite(col = "all", sep = "_")
    file_list <- full_data_paths[grepl(paste(file_choice$all, collapse = "|"), full_data_paths)]
    
    # Load initial data
    df_driv <- df_full |> 
      dplyr::filter(category %in% input$selectCat) |> 
      dplyr::filter(driver %in% input$selectDriv) |> 
      dplyr::filter(site %in% input$selectSite) |> 
      mutate(embargo = FALSE)
    if(nrow(df_driv) > 0){
      
      # Remove embargoed data and add shadows
      df_driv <- df_driv |> 
        mutate(date = as.Date(date)) |> 
        mutate(embargo = case_when(grepl(data_shadow, URL) ~ TRUE, TRUE ~ FALSE))
    }
    return(df_driv)
  })
  
  # Subset by variable name
  df_var <- reactive({
    req(input$selectVar)
    df_var <- df_driv() |> 
      filter(variable %in% input$selectVar)
    return(df_var)
  })
  
  # Filter by smaller details 
  df_filter <- eventReactive(input$filterData, {
    req(df_var())
    if(length(input$selectVar) == 0){
      df_filter <- data.frame(date = as.Date("2000-01-01"), value = NA, var_name = "No variables selected",
                              lon = NA, lat = NA, depth = NA, embargo = NA)
    } else if(length(input$selectVar) > 0){
      req(input$slideDate)
      df_filter <- df_var()
      if(!is.na(input$slideLon[1])){
        df_filter <- df_filter |>
          filter(lon >= input$slideLon[1] | is.na(lon)) |>
          filter(lon <= input$slideLon[2] | is.na(lon))}
      if(!is.na(input$slideLat[1])){
        df_filter <- df_filter |>
          filter(lat >= input$slideLat[1] | is.na(lat)) |>
          filter(lat <= input$slideLat[2] | is.na(lat))}
      if(!is.na(input$slideDepth[1])){
        df_filter <- df_filter |> 
          filter(depth >= input$slideDepth[1] | is.na(depth)) |> 
          filter(depth <= input$slideDepth[2] | is.na(depth))}
      if(!is.na(input$slideDate[1])){
        df_filter <- df_filter |> 
          filter(date >= input$slideDate[1] | is.na(date)) |> 
          filter(date <= input$slideDate[2] | is.na(date))}
    } else {
      df_filter <- data.frame(date = as.Date("2000-01-01"), value = NA, var_name = "All data have been filtered out", 
                              lon = NA, lat = NA, depth = NA, embargo = NA)
    }
    
    # Count data for map
    df_filter_map <- df_filter |> 
      filter(!is.na(lon), !is.na(lat)) |> 
      mutate(lon = round(lon, 2), lat = round(lat, 2)) |> 
      group_by(lon, lat, category, driver, variable, embargo) |> 
      summarise(count = n(), .groups = "drop") |> 
      left_join(long_names, by = c("category", "driver"))
    
    # Count data for time series
    df_filter_ts <- df_filter |> 
      filter(!is.na(date)) |> 
      mutate(year = year(date)) |> 
      group_by(year, category, driver, variable, embargo) |> 
      summarise(count = n(), .groups = "drop") |> 
      left_join(long_names, by = c("category", "driver"))
    
    # Count data for depth plot
    df_filter_depth <- df_filter |> 
      filter(!is.na(depth)) |> 
      mutate(depth = round(depth, -1)) |>
      group_by(depth, category, driver, variable, embargo) |> 
      dplyr::summarise(count = n(), .groups = "drop") |> 
      left_join(long_names, by = c("category", "driver"))
    
    # Compile list and exit
    df_final <- list(filter_data = df_filter,
                     map_count = df_filter_map,
                     ts_count = df_filter_ts,
                     depth_count = df_filter_depth)
    return(df_final)
  })
  
  # Remove embargoed data before downloading
  df_dl <- eventReactive(input$filterData, {
    req(df_filter())
    
    # Prep data
    df_filter <- df_filter()[["filter_data"]]
    
    # Apply data shadows
    data_shadow_df <- filter(df_filter, grepl(data_shadow, URL)) |> 
      mutate(lon = NA, lat = NA, date = NA, depth = NA, value = NA) |> 
      mutate(variable = case_when(driver %in% c("biomass", "spp rich") ~ as.character(NA), TRUE ~ variable)) |> 
      distinct()
    
    # Remove embargoed data and add shadows
    df_dl <- df_filter |> 
      filter(!grepl(data_shadow, URL)) |> 
      rbind(data_shadow_df) |> 
      arrange(-embargo)
    return(df_dl)
  })

  # 
  # Load full dataset with button click
  # loadfull <- reactiveValues(loaded = 0)
  # df_full <- reactive({
  #   
  #   # Look for button click
  #   input$loadCleanAll
  #   
  #   # Load data
  #   if(input$loadCleanAll >= 1 & loadfull$loaded == 0){
  #     df_full <- loadRData("full_data/clean_all.RData")
  #     loadfull$loaded <- loadfull$loaded+1
  #   } 
  #   
  #   return(df_full)
  # })
  
  # Subset by category+driver for summary panel
  df_summary <- reactive({
    req(input$selectDrivSummary)
    
    # Assemble possible files
    # file_choice <- expand.grid("clean", input$selectCatSummary, input$selectDrivSummary, input$selectSiteSummary) |> 
    #   unite(col = "all", sep = "_")
    # file_list <- full_data_paths[grepl(paste(file_choice$all, collapse = "|"), full_data_paths)]
    
    # Check if full dataset has been loaded
    # df_full <- df_full()
    # if("site" %in% colnames(df_full)){
      df_summary <- df_full |> 
        filter(site %in% input$selectSiteSummary, category %in% input$selectCatSummary, driver %in% input$selectDrivSummary)
    # } else {
    #   # Load initial data
    #   df_summary <- purrr::map_dfr(file_list, read_csv_arrow)
    # }
    
    if(nrow(df_summary) > 0){
      
      # Remove embargoed data and add shadows
      df_summary <- df_summary |> 
        mutate(date = as.Date(date)) |> 
        mutate(embargo = case_when(grepl(data_shadow, URL) ~ TRUE, TRUE ~ FALSE))
    }
    return(df_summary)
  })
  
  
  ## Download UI -------------------------------------------------------------
  
  # Download handler
  output$downloadFilter <- downloadHandler(
    filename = function() {
      paste0("filtered_data",input$downloadFilterType[1])
    },
    content <- function(file) {
      if(input$downloadFilterType == ".Rds"){
        saveRDS(df_dl(), file = file)
      } else if(input$downloadFilterType == ".csv"){
        arrow::write_csv_arrow(df_dl(), file)
      }
    }
  )
  
  
  ## Map ---------------------------------------------------------------------
  
  # The map data
  map_base <- reactive({
    req(input$selectSite)
    
    if(length(input$selectSite) > 1) return()
    
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
    map_sub <- filter(map_hi,
                      between(lon, bbox_name[1], bbox_name[2]),
                      between(lat, bbox_name[3], bbox_name[4]))
    map_sub_fix <- filter(map_hi, group %in% map_sub$group)
    
    # The base map
    baseMap <- ggplot() + 
      geom_polygon(data = map_sub_fix, fill = "grey80", colour = "black",
                   aes(x = lon, y = lat, group = group, text = "Land")) +
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
      labs(x = NULL, y = NULL, shape = "Driver", fill = "Variable") +
      theme_bw() +
      theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
            axis.text = element_text(size = 12, colour = "black"),
            axis.ticks = element_line(colour = "black"), legend.position = "none")
    return(baseMap)
  })
  
  output$mapPlot <- renderPlot({
    req(input$filterData)
    
    # TODO: Implement a wrapper function that can create faceted maps for multiple site choices
    if(length(input$selectSite) > 1) return()
    
    baseMap <- map_base()
    
    # Map data for plotting
    df_filter_map <- df_filter()[["map_count"]]
    if(nrow(df_filter_map) > 0){
      baseMap <- baseMap +
        geom_point(data = df_filter_map,
                   aes(x = lon, y = lat, shape = embargo, colour = variable,
                       text = paste0("Category: ",category_long,
                                     "<br>Driver: ",driver_long,
                                     "<br>Variable: ",variable,
                                     "<br>Lon: ",lon,
                                     "<br>Lat: ",lat,
                                     "<br>Count: ",count,
                                     "<br>Embargoed: ",embargo)))
    }
    
    # ggplotly(baseMap, tooltip = "text")
    baseMap
    
  })
  
  
  ## Time series -------------------------------------------------------------
  
  output$tsPlot <- renderPlot({
    req(input$filterData)
    
    df_filter_ts <- df_filter()[["ts_count"]]
    
    # Plot and exit
    if(nrow(df_filter_ts) > 0){
      basePlot <- ggplot(data = df_filter_ts, aes(x = year, y = log10(count))) +
        geom_point(aes(shape = embargo, colour = variable,
                       text = paste0("Category: ",category_long,
                                     "<br>Driver: ",driver_long,
                                     "<br>Variable: ",variable,
                                     "<br>Year: ",year,
                                     "<br>Count: ",count,
                                     "<br>Embargoed: ",embargo))) +
        labs(x = "Year", y = "Count (log10)", colour = "Variable") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
              axis.text = element_text(size = 12, colour = "black"),
              axis.ticks = element_line(colour = "black"), legend.position = "none")
    } else {
      basePlot <- ggplot() + geom_blank()
    }
    
    # ggplotly(basePlot, tooltip = "text")
    basePlot
    
  })
  
  
  ## Depth plot --------------------------------------------------------------
  
  output$depthPlot <- renderPlot({
    req(input$filterData)
    
    df_filter_depth <- df_filter()[["depth_count"]]
    
    # Count of data at depth by var name
    if(nrow(df_filter_depth) > 0){
      basePlot <- ggplot(df_filter_depth, aes(x = depth, y = log10(count))) +
        geom_col(aes(fill = variable, colour = embargo,
                     text = paste0("Category: ",category_long,
                                   "<br>Driver: ",driver_long,
                                   "<br>Variable: ",variable,
                                   "<br>Depth: ",depth,
                                   "<br>Count: ",count,
                                   "<br>Embargoed: ",embargo))) +
        scale_x_reverse() + coord_flip(expand = F) +
        labs(x = NULL, fill = "Variable", y = "Count (log10)") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
              axis.text = element_text(size = 12, colour = "black"),
              axis.ticks = element_line(colour = "black"), legend.position = "none")
    } else {
      basePlot <- ggplot() + geom_blank()
    }
    
    # ggplotly(basePlot, tooltip = "text")
    basePlot
    
  })
  

  ## Summary plot ------------------------------------------------------------
  
  # Interactive plotly summary figure
  output$summaryPlotly <- renderPlot({
    # req(input$selectDrivSummary)
    
    # testing...
    # df_summary <- clean_all |>
    #   dplyr::filter(site %in% c("kong", "is"), category == "chem", driver == "carb") |>
    #   mutate(date = as.Date(date), year = year(date), clim = month(date)) |> 
    #   mutate(embargo = case_when(grepl(data_shadow, URL) ~ TRUE, TRUE ~ FALSE))
    
    # Initial data
    if(length(input$selectDrivSummary) > 0){
      
      # Establish dataset
      df_summary <- df_summary()
      
      # Grouping vector
      if(input$selectGroupSummary == "Category"){
        grouping_vars <- c("category")
        join_vars <- c("category")
        long_names_sub <- dplyr::select(long_names, category, category_long) |> distinct()
        group_lab <- "Category"
      } else if(input$selectGroupSummary == "Driver"){
        grouping_vars <- c("category", "driver")
        join_vars <- c("category", "driver")
        long_names_sub <- long_names
        group_lab <- "Driver"
      } else if(input$selectGroupSummary == "Variable"){
        grouping_vars <- c("category", "driver", "variable")
        join_vars <- c("category", "driver")
        long_names_sub <- long_names
        group_lab <- "Driver"
      }
      
      # Summarise by date: Daily, Yearly, monthly clim
      if(input$selectDateSummary == "Year"){
        df_date <- df_summary |> 
          dplyr::mutate(year = lubridate::year(date)) |>
          dplyr::select(-date) |> 
          dplyr::rename(date = year)
        date_lab <- "Year"
      } else if(input$selectDateSummary == "Climatology"){
        df_date <- df_summary |> 
          dplyr::mutate(clim = lubridate::month(date)) |>
          dplyr::select(-date) |> 
          dplyr::rename(date = clim)
        date_lab <- "Monthly climatology"
      } else if(input$selectDateSummary == "Day"){
        df_date <- df_summary
        date_lab <- "Date"
      }
      
      # Summarise by value: "Data points", "Real values", "Unique variables", "Unique citations"
      if(input$selectDataSummary == "Data points"){
        df_crunch <- df_date |> 
          summarise(value = n(), .by = c("site", "date", "embargo", all_of(grouping_vars)))
        value_lab <- "Count of data points per goruping"
      } else if(input$selectDataSummary == "Real values"){
        df_crunch <- df_date  |> 
          filter(embargo == FALSE) |> 
          summarise(value = mean(value, na.rm = TRUE), .by = c("site", "date", "embargo", all_of(grouping_vars)))
        value_lab <- "Mean value per grouping (hover over points to see units)"
      } else if(input$selectDataSummary == "Unique variables"){
        df_crunch <- df_date  |>
          dplyr::select("site", "date", "embargo", "variable", all_of(grouping_vars)) |> 
          distinct() |> 
          summarise(value = n(), .by = c("site", "date", "embargo", all_of(grouping_vars)))
        value_lab <- "Unique variables per grouping"
      } else if(input$selectDataSummary == "Unique citations"){
        df_crunch <- df_date  |>
          dplyr::select("site", "date", "embargo", "citation", all_of(grouping_vars)) |> 
          distinct() |> 
          summarise(value = n(), .by = c("site", "date", "embargo", "citation", all_of(grouping_vars)))
        value_lab <- "Unique citations per grouping"
      }
      
      # Join for pretty names
      df_final <- df_crunch |> 
        left_join(long_names_sub, by = join_vars) |> 
        mutate(colour_choice = .data[[tolower(input$selectGroupSummary[1])]])
      
      # Automatic shift to dodge for barplots if showing Raw values
      if(input$selectDataSummary == "Real values"){
        col_dodge <- "dodge"
      } else {
        col_dodge <- "stack"
      }
      
      # Plot and exit
      if(nrow(df_summary) > 0){
        basePlot <- ggplot(data = df_final, aes(x = date, y = value)) +
          labs(x = date_lab, y = value_lab, 
               colour = input$selectGroupSummary[1], fill = input$selectGroupSummary[1]) +
          theme_bw() +
          theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
                axis.text = element_text(size = 12, colour = "black"),
                axis.ticks = element_line(colour = "black"),
                legend.position = "bottom")
        # legend.position = "none") # Disable legend
        if(input$selectGroupSummary == "Category"){
          if(input$selectPlotSummary == "Dot plot"){
            basePlot <- basePlot +
              geom_point(size = 2,
                         aes(shape = embargo, colour = colour_choice,
                             text = paste0("Site: ",site,
                                           "<br>Category: ",category_long,
                                           "<br>Date: ",date,
                                           "<br>Value: ",value,
                                           "<br>Embargoed: ",embargo)
                         )
              )
          } else {
            basePlot <- basePlot +
              geom_col(aes(shape = embargo, fill = colour_choice,
                           text = paste0("Site: ",site,
                                         "<br>Category: ",category_long,
                                         "<br>Date: ",date,
                                         "<br>Value: ",value,
                                         "<br>Embargoed: ",embargo)
              ),
              position = col_dodge
              ) 
          }
        } else if(input$selectGroupSummary == "Driver"){
          if(input$selectPlotSummary == "Dot plot"){
            basePlot <- basePlot +
              geom_point(size = 2,
                         aes(shape = embargo, colour = colour_choice,
                             text = paste0("Site: ",site,
                                           "<br>Category: ",category_long,
                                           "<br>Driver: ",driver_long,
                                           "<br>Date: ",date,
                                           "<br>Value: ",value,
                                           "<br>Embargoed: ",embargo)
                         )
              )
          } else {
            basePlot <- basePlot +
              geom_col(aes(shape = embargo, fill = colour_choice,
                           text = paste0("Site: ",site,
                                         "<br>Category: ",category_long,
                                         "<br>Driver: ",driver_long,
                                         "<br>Date: ",date,
                                         "<br>Value: ",value,
                                         "<br>Embargoed: ",embargo)
              ),
              position = col_dodge
              )
          }
        } else if(input$selectGroupSummary == "Variable"){
          if(input$selectPlotSummary == "Dot plot"){
            basePlot <- basePlot +
              geom_point(size = 2,
                         aes(shape = embargo, colour = colour_choice,
                             text = paste0("Site: ",site,
                                           "<br>Category: ",category_long,
                                           "<br>Driver: ",driver_long,
                                           "<br>Variable: ",variable,
                                           "<br>Date: ",date,
                                           "<br>Value: ",value,
                                           "<br>Embargoed: ",embargo)
                         )
              )
          } else {
            basePlot <- basePlot +
              geom_col(aes(shape = embargo, fill = colour_choice,
                           text = paste0("Site: ",site,
                                         "<br>Category: ",category_long,
                                         "<br>Driver: ",driver_long,
                                         "<br>Variable: ",variable,
                                         "<br>Date: ",date,
                                         "<br>Value: ",value,
                                         "<br>Embargoed: ",embargo)
              ),
              position = col_dodge
              )
          }
        }
        if(input$selectDateSummary == "Year"){
          x_breaks <- unique(round(seq(min(df_final$date, na.rm = TRUE), 
                                       max(df_final$date, na.rm = TRUE), 5)))
          basePlot <- basePlot +
            scale_x_continuous(breaks = x_breaks)
        }
        if(input$selectDateSummary == "Climatology"){
          basePlot <- basePlot +
            scale_x_continuous(breaks = seq(2, 12, 2))
        }
        # Remove embargo shape from legend
        basePlot <- basePlot +
          guides(shape = "none")
      } else {
        basePlot <- ggplot() + geom_blank()
      }
      
      # Create plotly
      # ggplotly(basePlot, tooltip = "text") #|> 
        # plotly::layout(legend = list(x = 0, xanchor = "left", yanchor = "bottom", orientation = "h"))
      basePlot
      
    } else {

      # No data to plot
      basePlot <- ggplot() + geom_blank() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Finish selection to plot data", size = 6, colour = "black") +
        labs(x = NULL, y = NULL) +
        theme_void()

      # Create plotly
      # ggplotly(basePlot, tooltip = "text") #|>
        # plotly::layout(legend = list(x = 0, xanchor = "left", yanchor = "bottom", orientation = "h"))
      basePlot

    }
    
  })
  
  
  ## Info tables -------------------------------------------------------------
  
  # List of long names for variables
  output$filterDT <- DT::renderDataTable({
    req(input$filterData)
    df_dl <- df_dl()
    df_dl$URL <- paste0('<a target="_blank" rel="noopener noreferrer" href="',df_dl$URL,'">',df_dl$URL,'</a>')
    df_dl_DT <- datatable(df_dl, rownames = FALSE, escape = FALSE,
                          options = list(pageLength = 10, scrollX = TRUE, scrollY = 400, searchHighlight = TRUE,
                                         columnDefs = list(list(
                                           # targets = c(1, 2),
                                           targets = c(2), # Just the citation
                                           render = JS(
                                                 "function(data, type, row, meta) {",
                                                 "return type === 'display' && data.length > 6 ?",
                                                 "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                                 "}")
                                             ))
                              ))
    return(df_dl_DT)
  })
  
  # List of long names for variables
  output$longVarDT <- DT::renderDataTable({
    df_names <- param_list |> 
      dplyr::rename(`Long name` = Name)
    df_names_DT <- datatable(df_names, rownames = FALSE, 
                             options = list(pageLength = 10, scrollX = TRUE, scrollY = 210,
                                            searchHighlight = TRUE))
    return(df_names_DT)
  })
  
  # Count of data points per driver per site
  output$countDrivDT <- DT::renderDataTable({
    df_meta <- clean_meta |> 
      dplyr::select(site_long, category_long, driver_long, count) |> 
      dplyr::rename(Site = site_long, Category = category_long, Driver = driver_long, `Data points` = count) |> 
      arrange(Site, Category, Driver)
    df_meta_DT <- datatable(df_meta, rownames = FALSE, 
                            options = list(pageLength = 10, scrollX = TRUE, scrollY = 210,
                                           searchHighlight = TRUE))
    return(df_meta_DT)
  })
  
  # Count of data points per citation per site
  output$countCitationDT <- DT::renderDataTable({
    df_citation <- clean_citation |> 
      dplyr::rename(Citation = citation, Site = site_long, Category = category_long, Driver = driver_long, `Data points` = count) |> 
      arrange(Citation, Site, Category, Driver)
    df_citation_DT <- datatable(df_citation, rownames = FALSE, 
                                options = list(pageLength = 10, scrollX = TRUE, scrollY = 210,
                                           searchHighlight = TRUE))
    return(df_citation_DT)
  })
  
  ## Instructions ------------------------------------------------------------
  
  observeEvent(input$instructions, {
    sendSweetAlert(
      session = session,
      width = "900px",
      title = "User manual",
      text = tags$span(
        "The FACE-IT data access portal is laid out as a dashboard with a ",tags$em("side bar"),
        " on the left and information and plots in the ",tags$em("body"),". The side bar buttons change the content of the body.",
        tags$h3(icon("book"),"Data summary interface", style = "color: #008980;"),
        tags$h4(tags$b("Filter data", style = "color: #f39c12;"),": This menu allows the user to filter data amalgemated for FACE-IT."),
        tags$h4(tags$em("1. Study site"),": Select one or more of the seven FACE-IT study sites to begin summarising."),
        tags$h4(tags$em("2. Category(s)"),": Categories are the broadest classification possible of the data. Select one or more values to move to step 3."),
        tags$h4(tags$em("3. Driver(s)"),": Drivers are more specific than categories. Multiple values may be selected."),
        tags$h4(tags$b("Summarise data", style = "color: #f39c12;"),": This menu allows the user to create grouped summaries."),
        tags$h4(tags$em("1. Grouping"),": Select a value around which to group the summary output."),
        tags$h4(tags$em("2. Date"),": Choose the temporal grouping of the data."),
        tags$h4(tags$em("3. Data"),": Decide what aspect of the data grouping should be returned."),
        tags$h4(tags$b("Visualise data", style = "color: #f39c12;"),": Here one may choose how to visualise the summary."),
        tags$h4(tags$em("1. Group colour"),": The colour of the grouping to be shown. This is reactive to the choices above."),
        tags$h4(tags$em("2. Plot type"),": For most choices a bar plot is ideal. For the raw data plots it is better to switch to 'Dot plot'."),
        tags$h3(icon("download"),"Data download interface", style = "color: #008980;"),
        tags$h4(tags$b("Filter data", style = "color: #f39c12;"),": This menu allows the user to filter and download from the data amalgemated for FACE-IT."),
        tags$h4(tags$em("1. Study site"),": Select one or more of the seven FACE-IT study sites to begin the data exploration process."),
        tags$h4(tags$em("2. Category(s)"),": Categories are the broadest classification possible of the data. Select one or more values to move to step 3."),
        tags$h4(tags$em("3. Driver(s)"),": Drivers are more specific than categories. Multiple values may be selected."),
        tags$h4(tags$em("4. Variable(s)"),": Variables are specific types of measurements of the social/natural world. The values available here are a product of steps 2+3."),
        tags$h4(tags$em("5-8"),": The ranges in the data for: longitude, latitude, date, and depth. Moving these sliders will filter out the data accordingly."),
        tags$h4(tags$em("9. Filter data"),": After selecting all of the desired variables and sliding the filters, click here to load the data. 
                  This will create figures in the three boxes to the right."),
        tags$h4(tags$em("10. Download"),": When one is happy with the selected/filtered data click here to open a final preview menu. 
                  After confirming that the desired data have been selected, scroll to the bottom of the window were one may choose the file type and then click the download button."),
        tags$h4(tags$b("Location", style = "color: #00c0ef;"),": Displays a map of where the selected data are located. Different drivers are shown by shape, and colours show variables. Mouse over the dots to see more information."),
        tags$h4(tags$b("Date", style = "color: #3c8dbc"),": Displays the selected data as an annual time series. Mouse over the points to see more information."),
        tags$h4(tags$b("Depth", style = "color: #00a65a"),": Shows the selected data as a stacked bar plot from the surface (0 m) down to the deepest depth in the data. Data are binned into 10 m groups. Negative values show data above the sea surface (altimetry). Mouse over for more information."),
        tags$h3(icon("table"),"Advanced search tool", style = "color: #008980;"),
        tags$h4(tags$b("Data availability per citation", style = "color: #f39c12;"),": This table shows a count of the number of data points per driver per citation.
                  This is meant to be a helpful guide to see if the data for a desired driver is available within a given citation (i.e. dataset) or not."),
        tags$h4(tags$b("Data availability per site", style = "color: #f39c12;"),": This table shows a count of the number of data points per driver per site.
                  This is meant to be a helpful guide to see if the data for a desired driver is available at a given site or not."),
        tags$h4(tags$b("Full variable names", style = "color: #f39c12;"),": This table shows the full names for all categories, drivers, and their variables. 
                  The final column 'Long name' shows a longer name for a given variavle if one is available in the meta-data."),
        tags$h3(icon("address-book"),"Contact", style = "color: #4472c4;"),
        tags$h4("For questions etc. please contact Robert Schlegel: robert.schlegel@imev-mer.fr")
      ),
      html = TRUE,
      type = "info"
    )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

