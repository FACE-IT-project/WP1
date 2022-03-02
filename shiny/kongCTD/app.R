# shiny/kongCTD/app.R
# This single script contains the code used to run the app for uploading Kongsfjorden CTD data

# TODO: It would be useful for a user that the settings could be saved in between uploads
# Add a column with the name of the uploader and the date of the upload
# Have an 6) Editing tab that is password protected to go back and fix issues
# User login would automagically attach their name to the data upload
# Increase bounding box to include Krosfjorden

# Have a popup after clicking download that lists the unique DOIs of the data

# QC needs to be handled at some point
# For starters would have a raw or QC flag to add to the data
# Look into what the OCE package has for CTD QC
# Any changes should be communicated via a metadata output

# Add historic data to backend to allow users to see how their newly updated data fit into the historic data
# Allow for colour to show on maps or time series for which sources have contributed the historic data
# Numbers for colours, and then a lookup table with product name via join by number

# The data upload repository needs to start to be considered

# Authorship order could be based on the number of individual files uploaded over the year

# Add FACE-IT branding to the about section of the app


# Libraries ---------------------------------------------------------------

# setwd("shiny/kongCTD/") # For in-app testing
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(rhandsontable)
# library(ggraph)
# library(plotly)
# library(see)
# library(ggtext)


# Data --------------------------------------------------------------------

# For testing...
# test_load <- read.csv("data/August Bailey_0408_1141.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1")
# test_load <- read.csv("data/KB3_018630_20210416_1728.csv", skip = 5, sep = ",", dec = ".", fileEncoding = "UTF-8")
# test_text <- read_file("data/August Bailey_0408_1141.txt")
# test_text <- read_file("data/KB3_018630_20210416_1728.csv")

# Default upload start values
# default_opts <- data.frame(skip = 0,
#                            header = TRUE,
#                            sep = ",",
#                            dec = ".",
#                            quote = '"',
#                            encoding = "UTF-8")

## The base land polygon
## NB: Only needed to run following code once. It is left here for posterity.
## Load shapefile
# coastline_full <- sf::read_sf("~/pCloudDrive/FACE-IT_data/maps/GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp")
## Convert to data.frame
# coastline_full_df <- sfheaders::sf_to_df(coastline_full, fill = TRUE)
## Subset to Kongsfjorden area and save
# coastline_kong <- coastline_full_df %>%
#   filter(x >= 9, x <= 13.5, y >= 78, y <= 80) %>%
#   dplyr::select(x, y, polygon_id) %>%
#   rename(lon = x, lat = y)
# save(coastline_kong, file = "coastline_kong.RData")
load("coastline_kong.RData")

# The base map
frame_base <- ggplot() +
  geom_polygon(data = coastline_kong, aes(x = lon, y = lat, group = polygon_id), 
               fill = "grey70", colour = "black") +
  scale_x_continuous(breaks = c(11.5, 12.0, 12.5),
                     # position = "top",
                     labels = scales::unit_format(suffix = "°E", accuracy = 0.1, sep = "")) +
  scale_y_continuous(breaks = c(78.9, 79.0, 79.1),
                     labels = scales::unit_format(suffix = "°N", accuracy = 0.1, sep = "")) +
  coord_cartesian(xlim = c(11, 12.69), ylim = c(78.85, 79.15), expand = F) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))
# frame_base


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # The app title
  dashboardHeader(title = "Kongsfjorden CTD"),
  
  # The primary options
  dashboardSidebar(
    sidebarMenu(id = "mainMenu",
                
                # The various menus
                menuItem("1) Load file", tabName = "load", icon = icon("book"), selected = TRUE),
                menuItem("2) Metadata", tabName = "meta", icon = icon("clock")),
                menuItem("3) QC", tabName = "tidy", icon = icon("shower")),
                menuItem("4) Upload", tabName = "upload", icon = icon("upload")),
                menuItem("5) Download", tabName = "download", icon = icon("download")),
                menuItem("About", tabName = "about", icon = icon("question")),
                
                # The reactive controls based on the primary option chosen
                uiOutput(outputId = "sidebar_controls"))
  ),
  
  # The dashboard
  dashboardBody(
    tabItems(
      

      ## Load menu ---------------------------------------------------------------

      tabItem(tabName = "load",
              
              box(width = 3, 
                  # height = "730px", # Length when tips are shown
                  height = "550px",
                  title = "File format",
                  status = "primary", solidHeader = TRUE, collapsible = FALSE,
                  
                  # Select a file
                  # h4("Load file to begin"),
                  fileInput("file1", "Choose CSV/TXT File",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv"),
                            placeholder = "Choose one or more files to begin"),
                  # uiOutput("fileNameUI"),
                  
                  # Select file schema
                  uiOutput("schemaUI"),
                  
                  # Horizontal line
                  tags$hr(),
                  
                  # Select header and skip rows
                  fluidRow(
                    column(6, h5(tags$b("Header")), uiOutput("headerUI")),
                    column(6, uiOutput("skipUI"))
                  ),
                  
                  # Select column separator and decimal place
                  fluidRow(
                    column(6, uiOutput("sepUI")),
                    column(6, uiOutput("decUI"))
                  ),
                  
                  # Select text quoting and file encoding
                  fluidRow(
                    column(6, uiOutput("quoteUI")),
                    column(6, uiOutput("encodingUI"))
                  )#,
                  
                  # Horizontal line
                  # tags$hr()#,
                  
                  # Hints for how to handle issues
                  # h4("Hints"),
                  # h6("Only one column -> Change 'Separator'"),
                  # h6("Commas instead of decimal places -> Change 'Decimal'"),
                  # h6("'Error: more columns than column names' -> Increase 'Skip rows'"),
                  # h6("'Error: invalid multibyte string 6' -> Change 'Encoding'")
              ),
              
              # The data display
              box(width = 9, height = "900px", title = "Data",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contents_load"))
      ),
      

      ## Meta menu ---------------------------------------------------------------
      
      tabItem(tabName = "meta",

              # The single metadata inputs
              box(width = 4, height = "300px", title = "Fill all values", 
                  status = "danger", solidHeader = TRUE, collapsible = FALSE,
                  shiny::selectInput("allSite", "Site", 
                                     choices = c("", "Site 1", "Site 2"), selected = ""),
                  fluidRow(column(4, shiny::numericInput("allLon", "Longitude", value = NA, min = 10, max = 14)),
                           column(4, shiny::numericInput("allLat", "Single latitude", value = NA, min = 78, max = 80)),
                           column(4, shiny::textInput("allDataOwner", "Data owner"))),
                  fluidRow(column(4, shiny::textInput("allSensorOwner", "Sensor owner")),
                           column(4, shiny::textInput("allSensorBrand", "Sensor brand")),
                           column(4, shiny::textInput("allSensorNumber", "Sensor number")))),
              
              # The interactive table
              box(width = 8, height = "300px", 
                  title = "Fill individual values", style = 'height:250px;overflow-y: scroll;',
                  status = "primary", solidHeader = TRUE, collapsible = FALSE,
                  rHandsontableOutput("table1output")
              ),
              
              # The data display
              box(width = 8, height = "550px", title = "Data",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contentsTime")
                  ),
              
              # The map display
              box(width = 4, height = "550px", title = "Map",
                  status = "warning", solidHeader = TRUE, collapsible = FALSE,
                  h4("Location of CTD cast"),
                  h5("Red border = no lon/lat"),
                  h5("Yellow border = lon/lat not in the fjord region"),
                  h5("Green border = lon/lat within fjord region"),
                  plotOutput("mapPlot", height = "350px")
              )
      ),

      ## Tidy menu ---------------------------------------------------------------

      tabItem(tabName = "tidy",

              fluidRow(

                column(width = 4,

                       box(width = 12, height = "300px", title = "Controls",
                           status = "primary", solidHeader = TRUE, collapsible = FALSE,

                           h4("Choose column order to begin"),

                           # Combine date and time columns
                           uiOutput("selectColsUI"),

                           # Filter head and tail of df
                           fluidRow(
                             column(6, shiny::numericInput("sliceHead", "Remove first n rows", value = 0, min = 0, step = 1)),
                             column(6, shiny::numericInput("sliceTail", "Remove last n rows", value = 0, min = 0, step = 1)),
                           )
                       ),

                       box(width = 12, height = "550px", title = "Map",
                           status = "warning", solidHeader = TRUE, collapsible = FALSE,
                           h4("Location of CTD cast"),
                           h5("Red border = no lon/lat"),
                           h5("Yellow border = lon/lat not in the fjord region"),
                           h5("Green border = lon/lat within fjord region")#,
                           # plotOutput("mapPlot", height = "350px")
                           )
                ),

                # The data display
                column(8,
                       box(width = 12, height = "500px", title = "Data",
                           status = "success", solidHeader = TRUE, collapsible = FALSE,
                           DT::dataTableOutput("contents_tidy")
                       ),
                       box(width = 12, height = "350px", title = "Time series",
                           status = "danger", solidHeader = TRUE, collapsible = FALSE,
                           fluidRow(
                             column(2,
                                    # dropdownButton(
                                    h4("Axis controls:"),
                                    uiOutput("plotXUI"),
                                    uiOutput("plotYUI")),
                             # circle = TRUE, status = "danger", icon = icon("gear"))),
                             # column(10, plotOutput("tsPlot", height = "250px"))
                           )
                       )
                )

              ),

      ),

      # tabItem(tabName = "tidy",
      #         fluidPage(
      #           column(12,
      #                  # h2(tags$b("About")),
      #                  p("Currently no QC functionality exists. Waiting on more progress on a unified plan.")
      #           )
      #         )
      # ),
      
      ## Upload menu -------------------------------------------------------------

      tabItem(tabName = "upload",
              fluidPage(
                box(width = 2, 
                    # height = "730px", # Length when tips are shown
                    height = "550px",
                    title = "Upload",
                    status = "primary", solidHeader = TRUE, collapsible = FALSE,
                    h3("Click to upload data to batabase"),
                    actionButton("upload", "Upload", icon = icon("upload"))),
                
                # The uploaded data display
                box(width = 5, height = "900px", title = "Current data",
                    status = "success", solidHeader = TRUE, collapsible = FALSE,
                    DT::dataTableOutput("uploadedDT")
                ),
                
                # The database display
                box(width = 5, height = "900px", title = "Database",
                    status = "success", solidHeader = TRUE, collapsible = FALSE,
                    DT::dataTableOutput("dataBase"))
              )
      ),

      ## Download menu ----------------------------------------------------------

      tabItem(tabName = "download",

              fluidRow(

                column(width = 3,

                       box(width = 12, height = "800px", title = "Controls",
                           status = "primary", solidHeader = TRUE, collapsible = FALSE,

                           ## Data owner
                           uiOutput("selectDOUI"),
                           
                           ## Sensor owner
                           uiOutput("selectSOUI"),
                           
                           ### Lon
                           uiOutput("slideLonUI"),
                           
                           ### Lat
                           uiOutput("slideLatUI"),
                           
                           ### Depth
                           uiOutput("slideDepthUI"),
                           
                           ### Date
                           uiOutput("slideDateUI"),

                           ### Download
                           hr(),
                           fluidRow(column(width = 6, uiOutput("downloadFilterTypeUI")),
                                    column(width = 6, uiOutput("downloadFilterUI")))
                       )
                ),

                # The data display
                column(9,
                       box(width = 12, height = "400px", title = "Data",
                           status = "success", solidHeader = TRUE, collapsible = FALSE,
                           DT::dataTableOutput("dataBaseFilter")),
                       box(width = 5, height = "350px", title = "Map",
                           status = "warning", solidHeader = TRUE, collapsible = FALSE,
                           # h4("Location of data"),
                           # h5("Red border = no lon/lat"),
                           # h5("Yellow border = lon/lat not in the fjord region"),
                           # h5("Green border = lon/lat within fjord region")#,
                           plotOutput("mapDL", height = "350px")),
                       box(width = 7, height = "350px", title = "Time series",
                           status = "danger", solidHeader = TRUE, collapsible = FALSE,
                           # fluidRow(
                           # column(2,
                           # dropdownButton(
                           # h4("Axis controls:"),
                           # uiOutput("plotXUI"),
                           # uiOutput("plotYUI")),
                           # circle = TRUE, status = "danger", icon = icon("gear"))),
                           column(10, plotOutput("tsPlot", height = "250px"))
                           # )
                           # )
                       )
                       
                              
              ),

      )
      ),
      
      
      ## App explanation ---------------------------------------------------------
      
      tabItem(tabName = "about", 
              fluidPage(
                column(12,
                       h2(tags$b("About")),
                       p("The purpose of this app is to provide a platform through which users may upload their CTD 
                       data collected in or around Kongsfjorden."),
                       p("This app was created as part of the output of WP1 of the Horizon2020 funded FACE-IT project (869154)."),
                       img(src = "FACE-IT_h2020.png", align = "left")
                )
              )
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {


  ## Load server -------------------------------------------------------------

  # Text output of uploaded file name
  # output$fileNameUI <- renderUI({
  #   req(input$file1)
  #   # print(input$file1$name)
  #   print(upload_opts$sep, upload_opts$skip, 
  #         upload_opts$dec, upload_opts$encoding)
  # })
  
  # Reactive category filters
  upload_opts <- reactiveValues(schema = "None",
                                header = TRUE,
                                skip = 0,
                                sep = ",",
                                dec = ".",
                                quote = '"',
                                encoding = "UTF-8")
  
  # Observe uploading of file(s)
  observeEvent(input$file1, {
    # req(input$file1)
    file_text <- read_file(input$file1$datapath[1])
    if(str_sub(file_text, 1, 10) == "From file:"){
      upload_opts$schema <- "SAIV"
    } else if(str_sub(file_text, 1, 8) == "RBR data"){
      upload_opts$schema <- "RBR"
    } else {
      # Intentionally blank
    }
  })
  
  # Reactive UI for file schema
  ## NB: These could potentially be moved to the UI and rather updated via observations as done in the meta section
  output$schemaUI <- renderUI({
    selectInput("schema", "File schema", choices = c("None", "SAIV", "RBR"), selected = upload_opts$schema)
  })
  
  # Reactive UI for header check box
  output$headerUI <- renderUI({
    shinyWidgets::prettyCheckbox("header", NULL, upload_opts$header, 
                                 # Unnecessary. Just for fun.
                                 status = "success", shape = "round", outline = TRUE, fill = TRUE, 
                                 animation = "jelly", icon = icon("check"), plain = TRUE, bigger = TRUE)
  })
  
  # Reactive UI for number of rows to skip
  output$skipUI <- renderUI({
    numericInput("skip", "Skip rows", value = upload_opts$skip, min = 0, step = 1)
  })
  
  # Reactive UI for column separator options
  output$sepUI <- renderUI({
    shinyWidgets::prettyRadioButtons("sep", "Separator",
                                     choiceNames = c("Comma", "Semicolon", "Tab"),
                                     choiceValues = c(",", ";", "/t"),
                                     selected = upload_opts$sep)
  })
  
  # Reactive UI for decimal options
  output$decUI <- renderUI({
    shinyWidgets::prettyRadioButtons("dec", "Decimal",
                                     choiceNames = c("Full stop", "Comma"),
                                     choiceValues = c(".", ","),
                                     selected = upload_opts$dec)
  })
  
  # Reactive UI for text quoting options
  output$quoteUI <- renderUI({
    shinyWidgets::prettyRadioButtons("quote", "Quote", 
                                     choiceNames = c("None", "Double quote", "Single quote"),
                                     choiceValues = c("", '"', "'"),
                                     selected = upload_opts$quote)
  })
  
  # Reactive UI for encoding options
  output$encodingUI <- renderUI({
    shinyWidgets::prettyRadioButtons("encoding", "Encoding",
                                     choiceNames = c("UTF8", "Latin"),
                                     choiceValues = c("UTF-8", "latin1"),
                                     selected = upload_opts$encoding)
  })
  
  # The file temp and real name data.frame
  file_info_df <- reactive({
    req(input$file1)
    df <- data.frame(file_temp = input$file1$datapath,
                     file_name = input$file1$name) #%>% 
      # mutate(file_num = paste0("file_", 1:n()))
    return(df)
  })
  
  # Load the file with the reactive file options
  df_load <- reactive({
    req(input$file1)
    
    # Reactive function that is applied to all files in upload list
    # Also need to pass the file name
    df_load_func <- function(file_temp){
      df <- read.csv(file_temp,
                     header = upload_opts$header,
                     skip = upload_opts$skip,
                     sep = upload_opts$sep,
                     dec = upload_opts$dec,
                     quote = upload_opts$quote,
                     fileEncoding = upload_opts$encoding, 
                     blank.lines.skip = TRUE) %>%
        mutate(file_temp = file_temp)
      if(input$schema == "SAIV"){
        df <- df %>% 
          dplyr::rename(Salinity = `Sal.`, Conductivity = `Cond.`, Temperature = Temp,
                        Fluorescence_ugChla_l = `F..µg.l.`, T_FTU = `T..FTU.`, Depth = `Depth.u.`) %>% 
          mutate(date_time = dmy_hms(paste(Date, Time, sep = " "))) %>% 
          dplyr::select(file_temp, date_time, Depth, Salinity:Density)
      } else if(input$schema == "RBR"){
        df <- df %>% 
          dplyr::rename(Fluorescence_ugChla_l = `Fluorometry.Chlorophyll`,  Specific_Conductivity = `Specific.Conductivity`,
                        Density_Anomaly = `Density.Anomaly`, Speed_of_sound = `Speed.of.sound`) %>% 
          mutate(date_time = dmy_hms(Timestamp)) %>% 
          dplyr::select(file_temp, date_time, Depth, Conductivity:Fluorescence_ugChla_l, Salinity:Speed_of_sound)
      }
      return(df)
    }

    # Upload all selected files
    df_load <- purrr::map_dfr(input$file1$datapath, df_load_func) %>% 
      left_join(file_info_df(), by = "file_temp") %>% 
      dplyr::select(file_name, everything(),  -file_temp)
    
    # Exit
    return(df_load)
  })
  
  # Observe the changing of the upload UI options
  ## Schema
  observeEvent(input$schema, {
    if(input$schema == "SAIV"){
      upload_opts$header <- TRUE; upload_opts$skip <- 3; upload_opts$sep <- ";"
      upload_opts$dec <- ","; upload_opts$quote <- '"'; upload_opts$encoding <- "latin1"
    } else if(input$schema == "RBR"){
      upload_opts$header <- TRUE; upload_opts$skip <- 5; upload_opts$sep <- ","
      upload_opts$dec <- "."; upload_opts$quote <- '"'; upload_opts$encoding <- "UTF-8"
    } else {
      # Intentionally blank
    }
  })
  ## header
  observeEvent(input$header, {
    upload_opts$header <- input$header
  })
  ## Skip
  observeEvent(input$skip, {
    upload_opts$skip <- input$skip
  })
  ## sep
  observeEvent(input$sep, {
    upload_opts$sep <- input$sep
  })
  ## dec
  observeEvent(input$dec, {
    upload_opts$dec <- input$dec
  })
  ## quote
  observeEvent(input$quote, {
    upload_opts$quote <- input$quote
  })
  ## encoding
  observeEvent(input$encoding, {
    upload_opts$encoding <- input$encoding
  })
  
  # Table showing the uploaded file
  output$contents_load <- DT::renderDataTable({
    req(input$file1)
    df_load <- df_load()
    # file_info_df <- file_info_df()
    df_load_DT <- datatable(df_load, 
                            options = list(pageLength = 20, scrollX = TRUE, scrollY = 700))
    return(df_load_DT)
  })
  

  ## Meta server -------------------------------------------------------------

  ## NB: date_time is currently created automagically in the load step
  ## For future iterations of the app I should created a red/green light that shows if date_time has been created
  ## And that must be accompanied by a red light = shown (green light = hidden) UI that allows users to create date_time
  
  # Create dataframe of file temp names and their metadata taken from the file headers
  file_meta_all <- reactive({
    req(input$file1)
    
    # Extract meta-data from file headers
    file_meta_func <- function(file_temp){
      file_text <- read_file(file_temp)
      if(str_sub(file_text, 1, 10) == "From file:"){
        ins_no_raw <- sapply(str_split(file_text, "Instrument no.:"), "[[", 2)
        # mini_df_1 <- read_delim("data/August Bailey_0408_1141.txt", n_max = 1, skip = 1, delim = ";")
        mini_df_1 <- read_delim(file_temp, n_max = 1, skip = 1, delim = ";")
        df_meta <- data.frame(file_temp = file_temp,
                              Site = as.character(NA),
                              Lon = as.numeric(NA),
                              Lat = as.numeric(NA),
                              Data_owner = as.character(NA),
                              Sensor_owner = "Kings Bay",
                              Sensor_brand = "SAIV",
                              Sensor_number = as.character(gsub("[^0-9.-]", "", str_sub(ins_no_raw, 1, 15))),
                              Air_pressure = mini_df_1$`Air pressure`)
      } else if(str_sub(file_text, 1, 8) == "RBR data"){
        # ins_no_raw <- sapply(str_split(file_text, "Serial Number:"), "[[", 2)
        # mini_df_1 <- read_csv("data/KB3_018630_20210416_1728.csv", n_max = 1)
        mini_df_1 <- read_csv(file_temp, n_max = 1)
        df_meta <- data.frame(file_temp = file_temp,
                              Site = as.character(NA),
                              Lon = as.numeric(NA),
                              Lat = as.numeric(NA),
                              Data_owner = as.character(NA),
                              Sensor_owner = as.character(NA),
                              Sensor_brand = "RBR",
                              Sensor_number = as.character(mini_df_1$...4))
      } else {
        df_meta <- data.frame(file_temp = file_temp,
                              Site = as.character(NA),
                              Lon = as.numeric(NA),
                              Lat = as.numeric(NA),
                              Data_owner = as.character(NA),
                              Sensor_owner = as.character(NA),
                              Sensor_brand = as.character(NA),
                              Sensor_number = as.character(NA))
      }
      return(df_meta)
    }
    
    # Extract meta-data for all uploaded files
    df_meta <- purrr::map_dfr(input$file1$datapath, file_meta_func) %>% 
      left_join(file_info_df(), by = "file_temp") %>% 
      dplyr::select(file_name, everything(),  -file_temp)
    
    # Exit
    return(df_meta)
  })
  
  # Interactive metadatatable
  table <- reactiveValues()
  output$table1output <- renderRHandsontable({
    rhandsontable(file_meta_all(), stretchH = "all", useTypes = F) %>% 
      # hot_col("file_num", readOnly = TRUE) %>% 
      hot_col("file_name", readOnly = TRUE) %>% 
      hot_col(col = "Sensor_number", strict = FALSE)})
  observeEvent(input$table1output, {
    df <- hot_to_r(input$table1output)
    df <- as.data.frame(df)
    table$table1 <- df
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Observe the changing of the single metadata UI options
  ## Site
  observeEvent(input$allSite, {
    table$table1$Site <- input$allSite
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Site), col = 2, session = session, table$table1$Site[1])
    if(input$allSite == "Site 1"){
      shiny::updateNumericInput(inputId = "allLon", value = 12)
      shiny::updateNumericInput(inputId = "allLat", value = 78.95)
    } else if(input$allSite == "Site 2"){
      shiny::updateNumericInput(inputId = "allLon", value = 11.5)
      shiny::updateNumericInput(inputId = "allLat", value = 79.05)
    } else {
      # Intentionally blank
    }
  })
  ## Longitude
  observeEvent(input$allLon, {
    table$table1$Lon <- input$allLon
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Lon), col = 3, session = session, table$table1$Lon[1])
  })
  # Latitude
  observeEvent(input$allLat, {
    table$table1$Lat <- input$allLat
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Lat), col = 4, session = session, table$table1$Lat[1])
  })
  ## Data owner
  observeEvent(input$allDataOwner, {
    table$table1$Data_owner <- input$allDataOwner
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Data_owner), col = 5, session = session, table$table1$Data_owner[1])
  })
  ## Sensor owner
  observeEvent(input$allSensorOwner, {
    table$table1$Sensor_owner <- input$allSensorOwner
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Sensor_owner), col = 6, session = session, table$table1$Sensor_owner[1])
  })
  ## Sensor brand
  observeEvent(input$allSensorBrand, {
    table$table1$Sensor_brand <- input$allSensorBrand
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Sensor_brand), col = 7, session = session, table$table1$Sensor_brand[1])
  })
  ## Sensor number
  observeEvent(input$allSensorNumber, {
    table$table1$Sensor_number <- input$allSensorNumber
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Sensor_number), col = 8, session = session, table$table1$Sensor_number[1])
  })
  
  # Reactive data for datatable
  df_time <- reactive({
    req(input$file1, table$table1)
    df_meta <- as.data.frame(table$table1) %>%
      mutate(file_name = as.character(file_name))
    df_time <- df_load() %>% 
      mutate(file_name = as.character(file_name)) %>% 
      left_join(df_meta, by = c("file_name")) #, "file_num")) %>% 
    # dplyr::select(-file_num)
    return(df_time)
  })
  
  output$contentsTime <- DT::renderDataTable({
    req(input$file1, table$table1)
    df_time <- df_time()
    df_time_DT <- datatable(df_time, options = list(pageLength = 20, scrollX = TRUE, scrollY = 350))
    return(df_time_DT)
  })

  output$mapPlot <- renderPlot({
    req(input$file1, table$table1)
    
    df_coords <- df_time()
    
    # Check that coords are present
    if(length(na.omit(df_coords$Lon)) == 0 | length(na.omit(df_coords$Lat)) == 0){
      
      # If no coords, red border
      mp <- frame_base +
        theme(panel.border = element_rect(colour = "red", size = 5))
      
    } else {
      
      # Get unique coordinates and count of data
      df_point <- df_coords %>%
        group_by(Lon, Lat) %>% 
        summarise(count = n(), .groups = "drop")

      # Check that coords are within he fjord region
      if(max(df_point$Lon, na.rm = T) > 12.69 | min(df_point$Lon, na.rm = T) < 11 | 
         min(df_point$Lat, na.rm = T) < 78.85 | max(df_point$Lat, na.rm = T) > 79.15){
        
        # If not create a yellow border
        border_colour <- "yellow"

      } else {
        
        # If yes create green border
        border_colour <- "green"
        
      }
      # Create bordered figure
      mp <- frame_base +
        # geom_point(data = df_point, aes(x = Lon, y = Lat), size = 5, colour = "green") +
        geom_label(data = df_point, aes(x = Lon, y = Lat, label = count), size = 6, colour = border_colour) +
        geom_text(data = df_point, aes(x = Lon, y = Lat, label = count), size = 6, colour = "black") +
        theme(panel.border = element_rect(colour = border_colour, size = 5))
    }
    return(mp)
  })
  

  ## Tidy server -------------------------------------------------------------
  
  # output$selectColsUI <- renderUI({
  #   req(input$file1)
  #   req(is.data.frame(df_time()))
  #   shiny::selectInput("selectCols", "Select column order", choices = colnames(df_time()), multiple = TRUE)
  # })
  # 
  # output$plotXUI <- renderUI({
  #   req(input$selectCols)
  #   shiny::selectInput("plotX", "X axis", multiple = FALSE, 
  #                      choices = colnames(df_tidy())[!colnames(df_tidy()) %in% c("lon", "lat")])
  # })
  # 
  # output$plotYUI <- renderUI({
  #   req(input$selectCols)
  #   shiny::selectInput("plotY", "Y axis", multiple = FALSE, 
  #                      choices = colnames(df_tidy())[!colnames(df_tidy()) %in% c("lon", "lat")])
  # })
  # 
  # output$tsPlot <- renderPlot({
  #   req(input$plotX)
  #   
  #   df_tidy <- df_tidy()
  #   
  #   ts <- ggplot(data = df_tidy, aes_string(x = input$plotX, y = input$plotY)) +
  #     geom_point() + geom_line()
  #   return(ts)
  # })
  # 
  # df_tidy <- reactive({
  #   req(input$file1, input$selectCols)
  # 
  #   df_tidy <- df_time()
  # 
  #   # Set column order
  #   df_tidy <- df_tidy %>%
  #     dplyr::select(input$selectCols)
  # 
  #   # Remove top and bottom of time series
  #   if(input$sliceHead > 0){
  #     df_tidy <- df_tidy[-seq_len(input$sliceHead),]
  #   }
  #   if(input$sliceTail > 0){
  #     tail_front <- nrow(df_tidy)-input$sliceTail+1
  #     df_tidy <- df_tidy[-seq(tail_front, nrow(df_tidy)),]
  #   }
  # 
  #   # Exit
  #   return(df_tidy)
  # })
  # 
  # output$contents_tidy <- DT::renderDataTable({
  #   req(input$file1)
  #   req(input$selectCols)
  #   df_tidy <- df_tidy()
  #   df_tidy_DT <- datatable(df_tidy, options = list(pageLength = 10, scrollX = TRUE, scrollY = 300))
  #   return(df_tidy_DT)
  # })


  ## Upload server -----------------------------------------------------------

  # Create reactive data_base object that recognizes uploads of new data
  data_base <- reactiveValues()
  data_base$df <- read_rds("data_base.Rds")
  
  # When the Upload button is clicked, save df_time()
  observeEvent(input$upload, {
    # saveData(df_time())
    df_res <- bind_rows(data_base$df, df_time()) %>% distinct()
    write_rds(df_res, file = "data_base.Rds")
    data_base$df <- read_rds("data_base.Rds")
  })
  
  # Reactive data for datatable
  df_data_base <- reactive({
    df_data_base <- as.data.frame(data_base$df)
    return(df_data_base)
  })
  
  # Show the uploaded data
  output$uploadedDT <- DT::renderDataTable({
    req(input$file1, table$table1)
    df_time <- df_time()
    df_time_DT <- datatable(df_time, options = list(pageLength = 20, scrollX = TRUE, scrollY = 700))
    return(df_time_DT)
  })
  
  # Show the newly expanded database
  output$dataBase <- DT::renderDataTable({
    data_base_DT <- datatable(df_data_base(), options = list(pageLength = 20, scrollX = TRUE, scrollY = 700))
    return(data_base_DT)
  })
  

  ## Download server --------------------------------------------------------

  # Filter data
  ## Subset by data owners
  output$selectDOUI <- renderUI({
    # req(input$selectSite)
    # selectizeInput('selectDO', 'Data owner',
    #   choices = unique(df_data_base()$Data_owner), multiple = T,
    #   # selected = unique(df_data_base()$Data_owner),
    #   options = list(
    #     placeholder = 'Select data owner(s)',
    #     onInitialize = I('function() { this.setValue(""); }')
    #   )
    # )
    selectInput("selectDO", "Data owner", multiple = T,
                choices = unique(df_data_base()$Data_owner), 
                selected = unique(df_data_base()$Data_owner))
  })
  
  ## Subset by sensor owner
  output$selectSOUI <- renderUI({
    # req(df_data_base())
    # selectizeInput('selectSO', 'Sensor owner',
    #   choices = unique(df_data_base()$Sensor_owner), multiple = T,
    #   options = list(
    #     placeholder = 'Select sensor owner(s)',
    #     onInitialize = I('function() { this.setValue(""); }')
    #   )
    # )
    selectInput("selectSO", "Sensor owner", multiple = T,
                choices = unique(df_data_base()$Sensor_owner), 
                selected = unique(df_data_base()$Sensor_owner))
  })
  
  ## Lon
  output$slideLonUI <- renderUI({
    # req(input$selectVar)
    shiny::sliderInput("slideLon", "Longitude range", value = range(df_data_base()$Lon, na.rm = T),
                       min = min(df_data_base()$Lon, na.rm = T), max = max(df_data_base()$Lon, na.rm = T))
  })
  
  # Lat
  output$slideLatUI <- renderUI({
    # req(input$selectVar)
    shiny::sliderInput("slideLat", "Latitude range", value = range(df_data_base()$Lat, na.rm = T),
                       min = min(df_data_base()$Lat, na.rm = T), max = max(df_data_base()$Lat, na.rm = T))
  })
  
  # Depth
  output$slideDepthUI <- renderUI({
    # req(input$selectVar)
    shiny::sliderInput("slideDepth", "Depth range", value = range(df_data_base()$Depth, na.rm = T),
                       min = min(df_data_base()$Depth, na.rm = T), max = max(df_data_base()$Depth, na.rm = T))
  })
  
  # Date
  output$slideDateUI <- renderUI({
    # req(input$selectVar)
    shiny::sliderInput("slideDate", "Date range", value = range(as.Date(df_data_base()$date_time), na.rm = T),
                       min = min(as.Date(df_data_base()$date_time), na.rm = T), max = max(as.Date(df_data_base()$date_time), na.rm = T))
  })
  
  # Reactive download type button
  output$downloadFilterTypeUI <- renderUI({
    # req(input$selectVar)
    radioButtons("downloadFilterType", "File type", choices = c(".csv", ".Rds"), 
                 selected = ".csv", inline = T)
  })
  
  # Filter by smaller details 
  df_filter <- reactive({
    # req(input$selectSite)
    if(length(input$selectDO) == 0){
      df_filter <- data.frame(warning = "Select at least one data owner from the drop down list.")
    } else if(length(input$selectDO) > 0){
      req(input$slideLon)
      df_filter <- df_data_base() %>%
        filter(Data_owner %in% input$selectDO,
               Sensor_owner %in% input$selectSO,
               Lon >= input$slideLon[1], Lon <= input$slideLon[2],
               Lat >= input$slideLat[1], Lat <= input$slideLat[2],
               Depth >= input$slideDepth[1], Depth <= input$slideDepth[2],
               date_time >= as.POSIXct(input$slideDate[1]), date_time <= as.POSIXct(input$slideDate[2]))
    } else {
      df_filter <- data.frame(warning = "Something has gone wrong :(")
    }
    return(df_filter)
  })
  
  # Show the filtered database
  output$dataBaseFilter <- DT::renderDataTable({
    data_base_filter_DT <- datatable(df_filter(), options = list(pageLength = 20, scrollX = TRUE, scrollY = 200))
    return(data_base_filter_DT)
  })
  
  # Reactive download button
  output$downloadFilterUI <- renderUI({
    # req(input$selectVar)
    downloadButton("downloadFilter", "Download data")
  })
  
  # Download handler
  output$downloadFilter <- downloadHandler(
    filename = function() {
      paste0("Kong_CTD_data",input$downloadFilterType[1])
    },
    content <- function(file) {
      if(input$downloadFilterType == ".Rds"){
        saveRDS(df_filter(), file = file)
      } else if(input$downloadFilterType == ".csv"){
        readr::write_csv(df_filter(), file)
      }
    }
  )
  
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

