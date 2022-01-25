# shiny/kongCTD/app.R
# This single script contains the code used to run the app for uploading Kongsfjorden CTD data

# TODO: Create an option in the first tab where a user can select the type of the file
# E.g. this would set all of the options to match a certain schema
# This in turn could be informed by how it reads the file and sees a pre-existing file name informed schema
# Look into how to upload a batch of files
# It would be useful for a user that the settings could be saved in between uploads
# Need to look into how different lon/lat can be added for many different files uploaded in a batch
# Could create a menu of the file names with input boxes next to them for lon/lat
# That info is then joined to the main data via a left_join by keeping the file name as a column
# Also have a master site and lon/lat box that can populate all rows
# Also provide a column of drop downs for pre-known stations
# Create a red to green next button to go to the next tab once all of the criteria for that tab are met

# QC needs to be handled at some point
# For starters would have a raw or QC flag to add to the data
# Look into what the OCE package has for CTD QC
# Any changes should be communicated via a metadata output

# Add historic data to backend to allow users to see how their newly updated data fit into the historic data
# Allow for colour to show on maps or time series for which sources have contributed the historic data
# Numbers for colours, and then a lookup table with product name via join by number

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
# library(ggraph)
# library(plotly)
# library(see)
# library(ggtext)


# Data --------------------------------------------------------------------

# For testing...
# test <- read.csv("shiny/kongCTD/data/August Bailey_0408_1141.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1")

# Default upload start values
default_opts <- data.frame(skip = 0,
                           header = TRUE,
                           sep = ",",
                           dec = ".",
                           quote = '"',
                           encoding = "UTF-8")

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
                menuItem("2) Time + space", tabName = "time", icon = icon("clock")),
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
                  tags$hr(),
                  
                  # Hints for how to handle issues
                  h4("Hints"),
                  h6("Only one column -> Change 'Separator'"),
                  h6("Commas instead of decimal places -> Change 'Decimal'"),
                  h6("'Error: more columns than column names' -> Increase 'Skip rows'"),
                  h6("'Error: invalid multibyte string 6' -> Change 'Encoding'")
              ),
              
              # The data display
              box(width = 9, height = "800px", title = "Data",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contents_load"))
      ),
      

      ## Time menu ---------------------------------------------------------------
      
      tabItem(tabName = "time",

              box(width = 3, height = "550px", title = "Controls",
                  status = "primary", solidHeader = TRUE, collapsible = FALSE,
                  
                  # Combine date and time columns
                  h4("Choose date/time column(s) to begin"),
                  h5("If two columns -> date + time"),
                  h5("Only acknowledges first two columns selected"),
                  uiOutput("timeColsUI"),
                  
                  # Coerce to POSIXCt
                  h5("Change selection if 'date_time_posix' is blank"),
                  radioButtons("posix", "Date/Time format",
                               choices = c("dmy", "mdy", "ymd",
                                           "dmy_hms", "mdy_hms", "ymd_hms"),
                               selected = "dmy_hms"),#, inline = TRUE),
                  
                  # Set time zone
                  # Currently not an option because all CTDs in Kong should be UTZ

                  # Add lon/lat
                  h6("It is preferable but not required to have lon/lat coordinates"),
                  fluidRow(
                    column(6, shiny::numericInput("addLon", "Longitude", value = 0)),
                    column(6, shiny::numericInput("addLat", "Latitude", value = 0)),
                  )
              ),
              
              # The data display
              box(width = 9, height = "800px", title = "Data",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contents_time")
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
    if(grepl("August", input$file1$name)){
      upload_opts$schema <- "SAIV"
    } else if(grepl("KB3", input$file1$name)){
      upload_opts$schema <- "RBR"
    } else {
      # Intentionally blank
    }
  })
  
  # Observe choice of file schema
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
  
  # Load the file with the reactive file options
  df_load <- reactive({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    
    df_load <- read.csv(input$file1$datapath,
                        header = upload_opts$header,
                        skip = upload_opts$skip,
                        sep = upload_opts$sep,
                        dec = upload_opts$dec,
                        quote = upload_opts$quote,
                        fileEncoding = upload_opts$encoding, 
                        blank.lines.skip = TRUE)
    
    # Exit
    return(df_load)
  })
  
  # Observe the changing of the upload UI options
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
    df_load_DT <- datatable(df_load, 
                            options = list(pageLength = 20, scrollX = TRUE, scrollY = 600))
    return(df_load_DT)
  })
  

  ## Time server -------------------------------------------------------------

  output$timeColsUI <- renderUI({
    req(input$file1)
    req(is.data.frame(df_load()))
    shiny::selectInput("timeCols", "Date + Time column(s)", choices = colnames(df_load()), multiple = T)
  })
  
  df_time <- reactive({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    req(input$timeCols)
    
    df_time <- df_load() %>% 
      mutate(lon = input$addLon[1],
             lat = input$addLat[1])
    
    # Create specific date_time column
    if(length(input$timeCols) == 1){
      df_time <- df_time %>% 
        mutate(date_time_char = df_time[,input$timeCols])
    } else if(length(input$timeCols) >= 2){
      df_time <- df_time %>% 
        mutate(date_time_char = paste(df_time[,input$timeCols[1]],
                                      df_time[,input$timeCols[2]], sep = " "))
    }
    
    # Convert to POSIXCt
    if(input$posix == "dmy"){
      df_time <- df_time %>% 
        mutate(date_time_posix = dmy(date_time_char))
    } else if(input$posix == "dmy_hms"){
      df_time <- df_time %>% 
        mutate(date_time_posix = dmy_hms(date_time_char))
    } else if(input$posix == "mdy"){
      df_time <- df_time %>% 
        mutate(date_time_posix = mdy(date_time_char))
    } else if(input$posix == "mdy_hms"){
      df_time <- df_time %>% 
        mutate(date_time_posix = mdy_hms(date_time_char))
    } else if(input$posix == "ymd"){
      df_time <- df_time %>% 
        mutate(date_time_posix = ymd(date_time_char))
    } else if(input$posix == "ymd_hms"){
      df_time <- df_time %>% 
        mutate(date_time_posix = ymd_hms(date_time_char))
    }
    
    # Bring date+time columns to front of data.frame
    df_time <- df_time %>%
      dplyr::select(lon, lat, date_time_posix, date_time_char, input$timeCols, everything())
    
    # Exit
    return(df_time)
  })
  
  output$contents_time <- DT::renderDataTable({
    req(input$file1)
    df_time <- df_time()
    df_time_DT <- datatable(df_time, options = list(pageLength = 20, scrollX = TRUE, scrollY = 600))
    return(df_time_DT)
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
  
  output$mapPlot <- renderPlot({
    req(input$selectCols)
    
    df_tidy <- df_tidy()
    # df_tidy <- test
    # df_tidy <- df_tidy %>% 
      # mutate(lon = 12.1, lat = 79.1)
    
    # Check that coords are present
    if("lon" %in% colnames(df_tidy) & "lat" %in% colnames(df_tidy)){
      df_point <- df_tidy %>% 
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
    return(mp)
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

