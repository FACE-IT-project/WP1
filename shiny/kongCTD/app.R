# shiny/kongCTD/app.R
# This single script contains the code used to run the app for uploading Kongsfjorden CTD data

# TODO: It would be useful for a user that the settings could be saved in between uploads
# Need to look into how different lon/lat can be added for many different files uploaded in a batch
# Could create a menu of the file names with input boxes next to them for lon/lat
# That info is then joined to the main data via a left_join by keeping the file name as a column
# Also have a master site and lon/lat box that can populate all rows
# Rather allow users to enter all of this info via a reactive spreadsheet
# https://stackoverflow.com/questions/22272571/data-input-via-shinytable-in-r-shiny-application
# This should still have buttons at the top that populate all values in a colum
# Should also investigate if a user could copy paste rows of meta data from an existing file
# Add a column of metadata for data owner: PI or Investigator
# Also provide a column of drop downs for pre-known stations
# Create a red to green next button to go to the next tab once all of the criteria for that tab are met

# QC needs to be handled at some point
# For starters would have a raw or QC flag to add to the data
# Look into what the OCE package has for CTD QC
# Any changes should be communicated via a metadata output

# Add historic data to backend to allow users to see how their newly updated data fit into the historic data
# Allow for colour to show on maps or time series for which sources have contributed the historic data
# Numbers for colours, and then a lookup table with product name via join by number

# The data upload repository needs to start to be considered.

# Authorship order could be based on the number of individual files uploaded over the year


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
                menuItem("2) Meta + lon/lat", tabName = "time", icon = icon("clock")),
                menuItem("3) Clean data", tabName = "tidy", icon = icon("shower")),
                menuItem("4) Upload", tabName = "upload", icon = icon("upload")),
                menuItem("About", tabName = "about", icon = icon("question")),
                
                # The reactive controls based on the primary option chosen
                uiOutput(outputId = "sidebar_controls"))
  ),
  
  # The dashboard
  dashboardBody(
    tabItems(
      

      ## Load menu ---------------------------------------------------------------

      tabItem(tabName = "load",
              
              box(width = 3, height = "730px", title = "Controls",
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
                  ),
                  
                  # Horizontal line
                  tags$hr()#,
                  
                  # Hints for how to handle issues
                  # h4("Hints"),
                  # h6("Only one column -> Change 'Separator'"),
                  # h6("Commas instead of decimal places -> Change 'Decimal'"),
                  # h6("'Error: more columns than column names' -> Increase 'Skip rows'"),
                  # h6("'Error: invalid multibyte string 6' -> Change 'Encoding'")
              ),
              
              # The data display
              box(width = 9, height = "800px", title = "Data",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contents_load"))
      ),
      

      ## Time menu ---------------------------------------------------------------
      
      tabItem(tabName = "time",

              box(width = 12, height = "300px", 
                  title = "Controls", style = 'height:250px;overflow-y: scroll;',
                  status = "primary", solidHeader = TRUE, collapsible = FALSE,
                  
                  # Combine date and time columns
                  # h4("Choose date/time column(s) to begin"),
                  # h5("If two columns -> date + time"),
                  # h5("Only acknowledges first two columns selected"),
                  # uiOutput("timeColsUI"),
                  
                  # Coerce to POSIXCt
                  # h5("Change selection if 'date_time_posix' is blank"),
                  # radioButtons("posix", "Date/Time format",
                  #              choices = c("dmy", "mdy", "ymd",
                  #                          "dmy_hms", "mdy_hms", "ymd_hms"),
                  #              selected = "dmy_hms"),#, inline = TRUE),
                  
                  # Set time zone
                  # Currently not an option because all CTDs in Kong should be UTZ

                  # Add lon/lat
                  # h6("It is preferable but not required to have lon/lat coordinates"),
                  
                  # Individual boxes for all files uploaded
                  column(4, uiOutput("fileNameUI")),
                  column(1, uiOutput("lonInputUI")),
                  column(1, uiOutput("latInputUI")),
                  column(2, uiOutput("brandInputUI")),
                  column(2, uiOutput("ownerInputUI")),
                  column(2, uiOutput("numberInputUI")),
                  
                  # One set of coords only
                  # fluidRow(
                  #   column(6, shiny::numericInput("addLon", "Longitude", value = 0)),
                  #   column(6, shiny::numericInput("addLat", "Latitude", value = 0)),
                  # )
              ),
              
              # The data display
              box(width = 8, height = "550px", title = "Data",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contentsTime")
                  ),
              box(width = 4, height = "550px", title = "Map",
                  status = "warning", solidHeader = TRUE, collapsible = FALSE,
                  h4("Location of CTD cast"),
                  h5("Red border = no lon/lat"),
                  h5("Yellow border = lon/lat not in the fjord region"),
                  h5("Green border = lon/lat within fjord region")#,
                  # plotOutput("mapPlot", height = "350px")
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
                           h5("Green border = lon/lat within fjord region"),
                           plotOutput("mapPlot", height = "350px")
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
                             column(10, plotOutput("tsPlot", height = "250px"))
                           )
                       )
                )
                
              ),
              
      ),

      
      ## Upload menu -------------------------------------------------------------

      tabItem(tabName = "upload",
              fluidPage(
                column(12,
                       # h2(tags$b("About")),
                       p("Currently no upload functionality exists. The issue of how to control column names needs
                         to be addressed first.")
                )
              )
      ),

      ## App explanation ---------------------------------------------------------
      
      tabItem(tabName = "about", 
              fluidPage(
                column(12,
                       h2(tags$b("About")),
                       p("The purpose of this app is to provide a platform through which users may upload their CTD 
                       data collected in or around Kongsfjorden.")
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
    req(input$file1)
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
                     file_name = input$file1$name) %>% 
      mutate(file_num = paste0("file_", 1:n()))
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
                            options = list(pageLength = 20, scrollX = TRUE, scrollY = 600))
    return(df_load_DT)
  })
  

  ## Time server -------------------------------------------------------------

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
                              Sensor_brand = "SAIV",
                              Sensor_owner = "Kings Bay",
                              Sensor_number = gsub("[^0-9.-]", "", str_sub(ins_no_raw, 1, 15)),
                              Air_pressure = mini_df_1$`Air pressure`)
      } else if(str_sub(file_text, 1, 8) == "RBR data"){
        # ins_no_raw <- sapply(str_split(file_text, "Serial Number:"), "[[", 2)
        # mini_df_1 <- read_csv("data/KB3_018630_20210416_1728.csv", n_max = 1)
        mini_df_1 <- read_csv(file_temp, n_max = 1)
        df_meta <- data.frame(file_temp = file_temp,
                              Sensor_brand = "RBR",
                              Sensor_owner = NA,
                              Sensor_number = mini_df_1$...4)
      } else {
        df_meta <- data.frame(file_temp = file_temp,
                              Sensor_brand = NA,
                              Sensor_owner = NA,
                              Sensor_number = NA)
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
  
  ## NB: Rather I think we will create an interactive datatable where users input their info
  # Dynamic generation of meta-data input UIs
  output$fileNameUI <- renderUI({
    file_count <- as.integer(length(input$file1$datapath))
    lapply(1:file_count, function(i){
      renderPrint(expr = file_info_df()$file_name[[i]])
      # renderPrint(expr = paste("\n", file_info_df()$file_name[i]))
      # renderText(paste("\n", file_info_df()$file_name[i]))
    })
  })
  output$lonInputUI <- renderUI({
    file_count <- as.integer(length(input$file1$datapath))
    lapply(1:file_count, function(i){
      numericInput(inputId = paste0("file_lon_", i), label = paste("Lon", i), value = NA)
    })
  })
  output$latInputUI <- renderUI({
    file_count <- as.integer(length(input$file1$datapath))
    lapply(1:file_count, function(i){
      numericInput(inputId = paste0("file_lat_", i), label = paste("Lat", i), value = NA)
    })
  })
  output$brandInputUI <- renderUI({
    file_count <- as.integer(length(input$file1$datapath))
    lapply(1:file_count, function(i){
      textInput(inputId = paste0("sensor_brand_", i), label = paste("Brand", i), 
                value = file_meta_all()$Sensor_brand[file_meta_all()$file_num == paste0("file_",i)][1])
    })
  })
  output$ownerInputUI <- renderUI({
    file_count <- as.integer(length(input$file1$datapath))
    lapply(1:file_count, function(i){
      textInput(inputId = paste0("sensor_owner_", i), label = paste("Owner", i), 
                value = file_meta_all()$Sensor_owner[file_meta_all()$file_num == paste0("file_",i)][1])
    })
  })
  output$numberInputUI <- renderUI({
    file_count <- as.integer(length(input$file1$datapath))
    lapply(1:file_count, function(i){
      textInput(inputId = paste0("sensor_number_", i), label = paste("Number", i), 
                value = file_meta_all()$Sensor_number[file_meta_all()$file_num == paste0("file_",i)][1])
    })
  })
  
  # Observe when lon/lat and sensor metadata boxes are changed
    
  # Time + Date selection columns
  # output$timeColsUI <- renderUI({
  #   req(input$file1)
  #   req(is.data.frame(df_load()))
  #   shiny::selectInput("timeCols", "Date + Time column(s)", choices = colnames(df_load()), multiple = T)
  # })
  
  df_time <- reactive({
    req(input$file1)
    # req(input$timeCols)
  
    df_meta <- file_meta_all()
        
    df_time <- df_load() %>% 
      left_join(df_meta, by = c("file_name", "file_num"))
      # mutate(lon = input$addLon[1],
      #        lat = input$addLat[1])
    
    # Create specific date_time column
    # if(length(input$timeCols) == 1){
    #   df_time <- df_time %>% 
    #     mutate(date_time_char = df_time[,input$timeCols])
    # } else if(length(input$timeCols) >= 2){
    #   df_time <- df_time %>% 
    #     mutate(date_time_char = paste(df_time[,input$timeCols[1]],
    #                                   df_time[,input$timeCols[2]], sep = " "))
    # }
    
    # Convert to POSIXCt
    # if(input$posix == "dmy"){
    #   df_time <- df_time %>% 
    #     mutate(date_time_posix = dmy(date_time_char))
    # } else if(input$posix == "dmy_hms"){
    #   df_time <- df_time %>% 
    #     mutate(date_time_posix = dmy_hms(date_time_char))
    # } else if(input$posix == "mdy"){
    #   df_time <- df_time %>% 
    #     mutate(date_time_posix = mdy(date_time_char))
    # } else if(input$posix == "mdy_hms"){
    #   df_time <- df_time %>% 
    #     mutate(date_time_posix = mdy_hms(date_time_char))
    # } else if(input$posix == "ymd"){
    #   df_time <- df_time %>% 
    #     mutate(date_time_posix = ymd(date_time_char))
    # } else if(input$posix == "ymd_hms"){
    #   df_time <- df_time %>% 
    #     mutate(date_time_posix = ymd_hms(date_time_char))
    # }
    
    # Bring date+time columns to front of data.frame
    # df_time <- df_time %>%
      # dplyr::select(lon, lat, date_time_posix, date_time_char, input$timeCols, everything())
    
    # Exit
    return(df_time)
  })
  
  output$contentsTime <- DT::renderDataTable({
    req(input$file1)
    df_time <- df_time()
    df_time_DT <- datatable(df_time, options = list(pageLength = 20, scrollX = TRUE, scrollY = 350))
    return(df_time_DT)
  })

  output$mapPlot <- renderPlot({
    req(input$file1)
    
    df_time <- df_time()
    # df_tidy <- test
    # df_tidy <- df_tidy %>% 
    # mutate(lon = 12.1, lat = 79.1)
    
    # Check that coords are present
    if("lon" %in% colnames(df_time) & "lat" %in% colnames(df_time)){
      df_point <- df_time %>%
        dplyr::select(lon, lat) %>%
        distinct()
      
      # Check that coords are within he fjord region
      if(df_point$lon > 12.69 | df_point$lon < 11 | df_point$lat < 78.85 | df_point$lat > 79.15){
        # If not create a yellow border
        mp <- frame_base +
          theme(panel.border = element_rect(colour = "yellow", size = 5))
      } else {
        # If yes create green border
        mp <- frame_base +
          geom_point(data = df_point, aes(x = lon, y = lat), size = 5, colour = "green") +
          theme(panel.border = element_rect(colour = "green", size = 5))
      }
    } else {
      # If no coords, red border
      mp <- frame_base +
        theme(panel.border = element_rect(colour = "red", size = 5))
    }
    # return(mp)
  })
  

  ## Tidy server -------------------------------------------------------------
  
  output$selectColsUI <- renderUI({
    req(input$file1)
    req(is.data.frame(df_time()))
    shiny::selectInput("selectCols", "Select column order", choices = colnames(df_time()), multiple = TRUE)
  })

  output$plotXUI <- renderUI({
    req(input$selectCols)
    shiny::selectInput("plotX", "X axis", multiple = FALSE, 
                       choices = colnames(df_tidy())[!colnames(df_tidy()) %in% c("lon", "lat")])
  })
  
  output$plotYUI <- renderUI({
    req(input$selectCols)
    shiny::selectInput("plotY", "Y axis", multiple = FALSE, 
                       choices = colnames(df_tidy())[!colnames(df_tidy()) %in% c("lon", "lat")])
  })
  
  output$tsPlot <- renderPlot({
    req(input$plotX)
    
    df_tidy <- df_tidy()
    
    ts <- ggplot(data = df_tidy, aes_string(x = input$plotX, y = input$plotY)) +
      geom_point() + geom_line()
    return(ts)
  })
  
  df_tidy <- reactive({

    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    req(input$selectCols)

    df_tidy <- df_time()

    # Set column order
    df_tidy <- df_tidy %>%
      dplyr::select(input$selectCols)

    # Remove top and bottom of time series
    if(input$sliceHead > 0){
      df_tidy <- df_tidy[-seq_len(input$sliceHead),]
    }
    if(input$sliceTail > 0){
      tail_front <- nrow(df_tidy)-input$sliceTail+1
      df_tidy <- df_tidy[-seq(tail_front, nrow(df_tidy)),]
    }

    # Exit
    return(df_tidy)
  })

  output$contents_tidy <- DT::renderDataTable({
    req(input$file1)
    req(input$selectCols)
    df_tidy <- df_tidy()
    df_tidy_DT <- datatable(df_tidy, options = list(pageLength = 10, scrollX = TRUE, scrollY = 300))
    return(df_tidy_DT)
  })


  ## Upload server -----------------------------------------------------------

  # Nothing here yet
  # Need to decide on a back-end
  # This in turn is to be dictated by a number of things
  # Also need to figure out what to do with random column names
  
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

