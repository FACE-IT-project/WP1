# shiny/kongCTD/app.R
# This single script contains the code used to run the app for uploading Kongsfjorden CTD data

### TODO: 
## General fixes
# Change column names to match NMDC terminology

## Graphical features
# Time series by site/user of count of when and how much data were uploaded
# Bar plot showing which users are in the lead w.r.t. data uploads
  # Do this by count of files, different days of upload, count of rows of data
# Allow for colour to show on maps or time series for which sources have contributed the historic data
  # Numbers for colours, and then a lookup table with product name via join by number

## UI features
# Popup when data are uploaded saying how much new data has been uploaded
# Create a NetCDF file format for saving data
# Have a '4) Editing' tab that is password protected to go back and fix issues
  # Make this attached to the username so that users can only edit the data they uploaded
  # Have superusers too whose name == ALL
  # This requires the database to have a last edited column
  # This would be added at first upload as the upload date
  # Have a dropdown that allowed different levels of QC: Level 0 - Level 4
  # This would be attached to the data as a QC_level column
  # Or have a flag column with numbers that show what the issue is
# It would be useful for a user that the settings could be saved in between uploads:
# https://discindo.org/post/2022-05-09-building-a-multi-session-shiny-application-with-brochure/
# Prevent uploading if not all boxes are filled except for Site

## Documentation
# Spruce up the landing page with some nice pictures of the fjord
# Add all sorts of info to the landing page that motivates people to use it
# Provide specific info for where the data are saved, and where they will be published; a diagram could be good
# Allow users to download a user manual

## QC
# Consider looking at the Argo standards for CTD QC - Look into the R Argo package
  # This is unfortunately not possible to use
# For starters would have a raw or QC flag to add to the data
# Any changes should be communicated via a metadata output

## DOI
# Allow for an embargo DOI option
# Have a popup after clicking download that lists the unique DOIs of the data
# May need another column for the DOI of data that were already published and being uploaded here afterwards
# Authorship order could be based on the number of individual files uploaded over the year

## Data management
# The data upload repository needs to start to be considered
# Don't upload all of the data at the launch of the app
  # Rather have a meta-data file ready that provides the parametres for a user to chose what to load
  # Allow filtering of download data by variable
  # Allow filter on downloading of published vs unpublished data
# May want to implement a database of meta-data per file, as well as the raw data
  # Via a look-up table one could then avoid having extra columns with the raw data
# Add historic data to backend to allow users to see how their newly updated data fit into the historic data

## Next steps
# Fix several key issues
# Identify five test subjects to stress test the app
# Before being public we need a final decision on how the data are published
# We need a timeline for all of these steps
  

# Libraries ---------------------------------------------------------------

# setwd("shiny/kongCTD/") # For in-app testing
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinymanager)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(rhandsontable)
# library(argoFloats)
# library(ggraph)
# library(plotly)
# library(see)
# library(ggtext)


# Data --------------------------------------------------------------------

# Workflow for adding new data schema
# 1) First figure out how to upload the file correctly via the 'file_load' examples below
# 2) Then load the file as text via 'file_text' to identify how many characters must be read to identify something unique
# 3) If the file requires a new schema follow these steps:
# 3.1) Add the unique text identifier (step 2) in the`observeEvent(input$file1` and `output$schemaUI <- renderUI({` code chunks
# 3.2) In the `observeEvent(input$schema, {` code chunk add the csv settings for the new schema
# 3.3) Then in the `df_load <- reactive({` object add the new chunk matching the new schema requirements
# 3.3.1) Note that this code chunk may need to be expanded to account for minor variations within a given schema
# 4) If no new schema is required then one should be able to tweak the above code chunks as necessary
# 5) Once the upload appears to work go through all of the tabs to check for normal behaviour

# Testing: load file correctly
# file_load <- read.csv("../test_data/August Bailey_0408_1141.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1")
# file_load <- read.csv("../test_data/KB3_018630_20210416_1728.csv", skip = 5, sep = ",", dec = ".", fileEncoding = "UTF-8")
# file_load <- read.csv("../test_data/010621_S.txt", skip = 5, sep = ";", dec = ",", fileEncoding = "latin1")
# file_load <- read.csv("../test_data/011021_KB1.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1")
# file_load <- read.table("../test_data/070521_S.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1", fill = T)
# file_load <- read.table("../test_data/080621_S.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1", fill = T)
# file_load <- read.table("../test_data/DAU_20200722_0750_SBE_7868.asc", skip = 57, sep = ",", dec = ".", fileEncoding = "UTF-8", header = F, fill = T)
# file_load <- read.table("../test_data/DAU_20200804_0722_SBE_13894_001.asc", skip = 79, sep = ",", dec = ".", fileEncoding = "UTF-8", header = F, fill = T)
# file_load <- read.table("../test_data/DAU_20200722_0804_CTD_454.txt", sep = "", skip = 30, dec = ".", fileEncoding = "latin1", header = F, fill = T)
# file_load <- read.table("../test_data/DAU_20200722_0804_CTD_454.TOB", sep = "", skip = 30, dec = ".", fileEncoding = "latin1", header = F, fill = T)

# Testing: read text for unique identifier
# file_text <- read_file("../test_data/August Bailey_0408_1141.txt")
# file_text <- read_file("../test_data/KB3_018630_20210416_1728.csv")
# file_text <- read_file("../test_data/010621_S.txt")
# file_text <- read_file("../test_data/011021_KB1.txt")
# file_text <- read_file("../test_data/070521_S.txt")
# file_text <- read_file("../test_data/080621_S.txt")
# file_text <- read_file("../test_data/DAU_20200722_0750_SBE_7868.asc")
# file_text <- read_file("../test_data/DAU_20200804_0722_SBE_13894_001.asc")
# file_text <- read_file("../test_data/DAU_20200722_0800_CTD_275.TOB")

# Testing: default upload start values
# default_opts <- data.frame(skip = 0,
#                            header = TRUE,
#                            sep = ",",
#                            dec = ".",
#                            quote = '"',
#                            encoding = "UTF-8")

# The base land polygon
## NB: Only needed to run following code once. It is left here for posterity.
## Load shapefile
# coastline_full <- sf::read_sf("~/pCloudDrive/FACE-IT_data/maps/GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp")
## Convert to data.frame
# coastline_full_df <- sfheaders::sf_to_df(coastline_full, fill = TRUE)
## Subset to Kongsfjorden area and save
# coastline_kong <- coastline_full_df %>%
#   filter(x >= 9, x <= 13.5, y >= 78, y <= 79.5) %>%
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
  scale_y_continuous(breaks = c(78.9, 79.0, 79.1, 79.2, 79.3),
                     labels = scales::unit_format(suffix = "°N", accuracy = 0.1, sep = "")) +
  coord_cartesian(xlim = c(11, 12.69), ylim = c(78.85, 79.35), expand = F) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))
# frame_base

# login credentials
credentials <- data.frame(
  user = c("r", "Allison", "Clara", "Jean-Pierre", "Philipp", "shinymanager"), # mandatory
  password = c("r", "R", "R", "Antibes", "Argo", "12345"), # mandatory
  start = c("2019-04-15"), # optional (all others)
  expire = c(NA, NA, NA, NA, NA, "2023-12-31"),
  admin = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
  comment = "Simple and secure authentification mechanism for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# Increase file upload size limit
options(shiny.maxRequestSize = 50*1024^2)

# Rounding functions
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(

  # The app title
  dashboardHeader(title = "Kongsfjorden CTD"),
  
  # The primary options
  dashboardSidebar(
    
    # Get more awesome icons
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),
    
    # The side bar
    sidebarMenu(id = "mainMenu",
                
                # The various menus
                menuItem("1) Load file(s)", tabName = "load", icon = icon("desktop"), selected = TRUE),
                menuItem("2) Metadata", tabName = "meta", icon = icon("cog")),
                menuItem("3) QC/Upload", tabName = "QCup", icon = icon("upload")),
                menuItem("4) Edit", tabName = "edit", icon = icon("shower")),
                menuItem("Download", tabName = "download", icon = icon("download")),
                menuItem("About", tabName = "about", icon = icon("question"))),
                
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
    ),
    
    # Add FACE-IT logo at bottom of menu bar
    br(), br(), br(), br(),br(), br(), br(), br(), br(), br(), br(),
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
    img(src = "FACE-IT_Logo_900.png", align = "centre", width = "225")
  ),
  
  # The dashboard
  dashboardBody(
    tabItems(
      

      ## Load menu ---------------------------------------------------------------

      tabItem(tabName = "load",
              
              box(width = 3,
                  # height = "730px", # Length when tips are shown
                  height = "650px",
                  title = "File format",
                  status = "primary", solidHeader = TRUE, collapsible = FALSE,
                  
                  # Select a file
                  # h4("Load file to begin"),
                  fileInput("file1", "Select CTD file(s) to upload (.csv or .txt)",
                            multiple = TRUE,
                            # accept = c("text/csv",
                            #            "text/comma-separated-values,text/plain",
                            #            ".csv"),
                            placeholder = "Choose one or more files to begin"),
                  # uiOutput("fileNameUI"),
                  
                  # Select file schema
                  uiOutput("schemaUI"),
                  
                  # Horizontal line
                  tags$hr(),
                  
                  # Addvice text
                  h6("Once data have been uploaded, check dataframe to the right."),
                  h6("If data are not correctly parsed into columns, use the following options."),
                  
                  # Select header and skip rows
                  fluidRow(
                    column(6, h5(tags$b("File includes header")), uiOutput("headerUI")),
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
              box(width = 9, height = "850px", title = "Data preview",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contents_load"))
      ),
      

      ## Meta menu ---------------------------------------------------------------
      
      tabItem(tabName = "meta",

              # The single metadata inputs
              box(width = 4, height = "375px", title = "Metadata: Fill values here that apply to all files", 
                  status = "info", solidHeader = TRUE, collapsible = FALSE,
                  # selectizeInput('x1', 'X1', choices = list(
                  #   Eastern = c(`New York` = 'NY', `New Jersey` = 'NJ'),
                  #   Western = c(`California` = 'CA', `Washington` = 'WA')
                  # ), multiple = TRUE)
                  h6(tags$b("Location:"), "Select a standard site or fill latitude/longitude."),
                  fluidRow(column(4, selectizeInput("allSite", "Site", 
                                                    choices = list(New = c("No name" = "No name"),
                                                                   AWI = c("Site 1" = "Site 1"),
                                                                   SAMS = c("Site 2" = "Site 2"),
                                                                   NPI = c("Kb0" = "Kb0", "Kb1" = "Kb1", "Kb2" = "Kb2", "Kb3" = "Kb3", 
                                                                           "Kb4" = "Kb4", "Kb5" = "Kb5", "Kb6" = "Kb6", "Kb7" = "Kb7", 
                                                                           "Kb8" = "Kb8", "V6" = "V6", "V10" = "V10", "V12" = "V12")),
                                                    selected = "No name")),
                           column(4, shiny::numericInput("allLon", "Longitude", value = NA, min = 10, max = 14)),
                           column(4, shiny::numericInput("allLat", "Latitude", value = NA, min = 78, max = 80))),
                  h6(tags$b("Reference:"), "Provide information on the ownership of the data."),
                  fluidRow(column(4, shiny::textInput("allDataOwnerPerson", "Person")),
                           column(4, shiny::textInput("allDataOwnerInstitute", "Institute")),
                           column(4, shiny::textInput("allDOI", "DOI"))),
                  h6(tags$b("Sensor:"), "Complete ID info not detected during upload."),
                  fluidRow(column(4, shiny::textInput("allSensorOwner", "Owner")),
                           column(4, shiny::textInput("allSensorBrand", "Brand")),
                           column(4, shiny::textInput("allSensorNumber", "Number")))),
              
              # The interactive table
              box(width = 8, height = "375px", 
                  title = "Metadata: Fill file-specific values here", style = 'height:250px;overflow-y: scroll;',
                  status = "primary", solidHeader = TRUE, collapsible = FALSE,
                  rHandsontableOutput("table1output")
              ),
              
              # The data display
              box(width = 8, height = "480px", title = "Data preview",
                  status = "success", solidHeader = TRUE, collapsible = FALSE,
                  DT::dataTableOutput("contentsTime")
                  ),
              
              # The map display
              box(width = 4, height = "480px", title = "Map",
                  status = "warning", solidHeader = TRUE, collapsible = FALSE,
                  h4("Check location of CTD cast"),
                  h5("Red border = no lon/lat"),
                  h5("Yellow border = lon/lat not in the fjord region"),
                  h5("Green border = lon/lat within fjord region"),
                  plotOutput("mapPlot", height = "300px")
              )
      ),

      ## QC/Up menu --------------------------------------------------------------

      tabItem(tabName = "QCup",

              fluidRow(

                column(width = 4,

                       box(width = 12, height = "400px", title = "Controls",
                           status = "primary", solidHeader = TRUE, collapsible = FALSE,

                           # h4("Choose column order to begin"),
                           h4("This will contain controls for choosing QC level"),
                           h5("QC is currently planned to be based on the ARGO standards"),

                           # Combine date and time columns
                           uiOutput("selectColsUI"),

                           # Filter head and tail of df
                           fluidRow(
                             column(6, shiny::numericInput("sliceHead", "Remove first n rows", value = 0, min = 0, step = 1)),
                             column(6, shiny::numericInput("sliceTail", "Remove last n rows", value = 0, min = 0, step = 1)),
                             ),
                           
                           h4("Meta-data status:"),
                           htmlOutput("uploadText"),
                           uiOutput("uploadButton")
                           ),
                       
                       # box(width = 12,
                       #     height = "250px",
                       #     title = "Upload",
                       #     status = "primary", solidHeader = TRUE, collapsible = FALSE,
                       #     # h4("Click to upload data to database"),
                       #     # actionButton("upload", "Upload", icon = icon("upload"))
                       #     # uiOutput("uploadButton")
                       #     ),
                ),

                # The data display
                column(8,
                       
                       box(width = 12, height = "450px", title = "Preview of data to be uploaded",
                           status = "success", solidHeader = TRUE, collapsible = FALSE,
                           DT::dataTableOutput("uploadedDT")
                           ),
                       
                       box(width = 12, height = "400px", title = "Summary of data in Database",
                           status = "danger", solidHeader = TRUE, collapsible = FALSE,
                           DT::dataTableOutput("dataBase")
                           ),
                       )
                ),
              ),
      

      ## Edit menu ---------------------------------------------------------------

      tabItem(tabName = "edit",
              
              fluidRow(h4("   No one here but us chickens.")),
              img(src = "CTD_cast1.png", align = "center", width = "1000px")),
      
      ## Download menu -----------------------------------------------------------

      tabItem(tabName = "download",
              
              fluidRow(
                
                column(width = 3,
                       
                       box(width = 12, height = "870px", title = "Download from database",
                           status = "primary", solidHeader = TRUE, collapsible = FALSE,
                           
                           # Message
                           h6("Download all data, or subset using the filters below."),
                           
                           ## Data uploader
                           uiOutput("selectUpUI"),
                           
                           ## Data owner (person)
                           uiOutput("selectDOPUI"),
                           
                           ## Data owner (institute)
                           uiOutput("selectDOInUI"),
                           
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
                           # br(),
                           fluidRow(column(width = 6, uiOutput("downloadFilterTypeUI")),
                                    column(width = 6, uiOutput("downloadFilterUI")))
                       )
                ),
                
                # The data display
                column(9,
                       box(width = 12, height = "450px", title = "Data selected from database",
                           status = "danger", solidHeader = TRUE, collapsible = FALSE,
                           DT::dataTableOutput("dataBaseFilter")),
                       box(width = 7, height = "400px", title = "Time series of selected data",
                           status = "success", solidHeader = TRUE, collapsible = FALSE,
                           fluidRow(
                             # column(2,
                             #        dropdownButton(
                             #          h4("Axis controls:"),
                             #          uiOutput("plotXUIDL"),
                             #          uiOutput("plotYUIDL")),
                             #        circle = TRUE, status = "danger", icon = icon("gear")),
                             column(2, h4("Axis controls:")),
                             column(4, uiOutput("plotXUIDL")),
                             column(4, uiOutput("plotYUIDL")),
                             column(12, plotOutput("tsPlot", height = "250px")))),
                       box(width = 5, height = "400px", title = "Map of selected data",
                           status = "warning", solidHeader = TRUE, collapsible = FALSE,
                           # h4("Location of data"),
                           # h5("Red border = no lon/lat"),
                           # h5("Yellow border = lon/lat not in the fjord region"),
                           # h5("Green border = lon/lat within fjord region")#,
                           plotOutput("mapDL", height = "330px"))
                )
              )
      ),
      
      
      ## App explanation ---------------------------------------------------------
      
      tabItem(tabName = "about", 
              fluidPage(
                column(12,
                       img(src = "Portal process visualization.png", align = "center", width = "1000px"),
                       # img(src = "CTD_cast1.png", align = "center", width = "1000px"),
                       h2(tags$b("About")),
                       p("The Kongsfjorden CTD portal is developed out of a common desire by marine researchers working in Kongsfjorden, by Ny-Ålesund, Svalbard, 
                         to safeguard and collect the numerous CTD profiles collected by diverse researchers in the fjord. As a natural laboratory of Arctic change, 
                         with many international research efforts focused on understanding the area, archiving of data is of utmost importance. 
                         This portal allows for easy storage, standardization, and use of CTD data from Kongsfjorden, with a pipeline for annual, 
                         open access publishing of the data to a trusted, well-established open-data-publisher, where all contributors are co-authors."),
                       # h3(tags$b("General layout")),
                       # tags$ul(
                       #   tags$li("After logging in, you will see the first page of the app ( ‘1) Upload’ )."),
                       #   tags$li("On the left, you can navigate between the different pages. To move to the next step of your data upload, you need to click on the next tab once you are finished with the current one."),
                       #   tags$li("You can move back and forth between the different tabs without losing or having to save your data in between.")
                       # ),
                       # h3(tags$b("Prepare your data")),
                       # tags$ul(
                       #   tags$li("You can upload the raw files exported from the CTD, but do not use versions where you have manually changed the file or added additional text."),
                       #   tags$li("Datasets can be uploaded as multiple individual txt or csv files per profile."),
                       #   tags$li("It is easiest to upload your data sorted by station ID (if you use those) or lat long location."),
                       #   tags$li("If you want to upload data from different stations in one go, make sure you have the lat long data for each profile available (you can copy past that data into the app)."),
                       #   tags$li("In one upload session, only files from the same device brand and data structure can be uploaded."),
                       # ),
                       h3(tags$b("Create user account")),
                       # tags$ul(
                         # tags$li(
                           p("Please contact the administrator to have a new user account created. Provide them with your name and e-mail, as well as your desired username and password."
                       # )
                         ),
                       h3(tags$b("Data publishing")),
                       tags$ul(
                         tags$li("If your data has already been published, please add the DOI [DOI field still needs to be implemented] of the primary data publication when uploading the data (see 'Instructions')."),
                         tags$li("If your data needs be published in another database first, write ‘xxx’ into the DOI field."),
                         tags$li("In this case, your data will also not be included into the Pangaea publication of that year."),
                         tags$li("If you do not add a DOI within 1 year, the data will be added to the next Pangaea publication with you as the primary data owner."),
                         tags$li("In case you want your data to be published via the automated pipeline provided by the app, leave the DOI field empty."),
                         tags$li("The data will be published together with the other data from that year with all data providers and/or data owners as coauthors [yet to be determined]."),
                         tags$li("The order of the author list will be dictated by the number of provided datasets [yet to be determined].")
                         ),
                       h3(tags$b("Acknowledgments")),
                       p("This portal was created out of the Kongsfjorden System Flagship, 
                         and developed by Robert Schlegel as a part of the output of WP1 of the Horizon2020 funded FACE-IT project (869154)."),
                       img(src = "Ny-Ålesund logo_with Flagship_2.png", align = "center", width = "1000px"),
                       img(src = "FACE-IT_h2020.png", align = "center", width = "600px")
                )
              )
      )
    )
  )
)

# Wrap the UI with secure_app
ui <- secure_app(ui)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {


  ## login creds -------------------------------------------------------------

  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Text output of the credentials
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  

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
    file_text_list <- str_split(file_text, "\r\n")
    if(str_sub(file_text_list[[1]][1], 1, 16) == "From file: Alt_S"){
      upload_opts$schema <- "Alt_S"
    } else if(str_sub(file_text_list[[1]][1], 1, 10) == "From file:"){
      upload_opts$schema <- "SAIV"
    } else if(str_sub(file_text_list[[1]][1], 1, 8) == "RBR data"){
      upload_opts$schema <- "RBR"
    } else if(str_sub(file_text_list[[1]][1], 1, 10) == "* Sea-Bird" & str_sub(file_text_list[[1]][17], 1, 15) == "* output oxygen"){
        upload_opts$schema <- "Sea-Bird O2"
    } else if(str_sub(file_text_list[[1]][1], 1, 10) == "* Sea-Bird"){
      upload_opts$schema <- "Sea-Bird"
    } else if(str_sub(file_text_list[[1]][1], 1, 14) == "SSDA Sea & Sun"){
      upload_opts$schema <- "Sea & Sun"
    } else {
      # Intentionally blank
    }
  })
  
  # Reactive UI for file schema
  ## NB: These could potentially be moved to the UI and rather updated via observations as done in the meta section
  output$schemaUI <- renderUI({
    selectInput("schema", "CTD type", choices = c("None", "Alt_S", "SAIV", "RBR", "Sea-Bird", "Sea-Bird O2", "Sea & Sun"), 
                selected = upload_opts$schema)
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
    numericInput("skip", "# of rows to skip", value = upload_opts$skip, min = 0, step = 1)
  })
  
  # Reactive UI for column separator options
  output$sepUI <- renderUI({
    shinyWidgets::prettyRadioButtons("sep", "Column separator",
                                     choiceNames = c("None", "Comma", "Semicolon", "Tab"),
                                     choiceValues = c("", ",", ";", "\t"),
                                     selected = upload_opts$sep)
  })
  
  # Reactive UI for decimal options
  output$decUI <- renderUI({
    shinyWidgets::prettyRadioButtons("dec", "Decimal mark",
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
  
  # Observe the changing of the upload UI options
  ## Schema
  observeEvent(input$schema, {
    if(input$schema == "Alt_S"){
      upload_opts$header <- TRUE; upload_opts$skip <- 5; upload_opts$sep <- ";"
      upload_opts$dec <- ","; upload_opts$quote <- '"'; upload_opts$encoding <- "latin1"
    } else if(input$schema == "SAIV"){
      upload_opts$header <- TRUE; upload_opts$skip <- 3; upload_opts$sep <- ";"
      upload_opts$dec <- ","; upload_opts$quote <- '"'; upload_opts$encoding <- "latin1"
    } else if(input$schema == "RBR"){
      upload_opts$header <- TRUE; upload_opts$skip <- 5; upload_opts$sep <- ","
      upload_opts$dec <- "."; upload_opts$quote <- '"'; upload_opts$encoding <- "UTF-8"
    } else if(input$schema == "Sea-Bird"){
      upload_opts$header <- FALSE; upload_opts$skip <- 57; upload_opts$sep <- ","
      upload_opts$dec <- "."; upload_opts$quote <- '"'; upload_opts$encoding <- "UTF-8"
    } else if(input$schema == "Sea-Bird O2"){
      upload_opts$header <- FALSE; upload_opts$skip <- 79; upload_opts$sep <- ","
      upload_opts$dec <- "."; upload_opts$quote <- '"'; upload_opts$encoding <- "UTF-8"
    } else if(input$schema == "Sea & Sun"){
      upload_opts$header <- FALSE; upload_opts$skip <- 30; upload_opts$sep <- ""
      upload_opts$dec <- "."; upload_opts$quote <- '"'; upload_opts$encoding <- "latin1"
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
  
  # The file temp and real name data.frame
  file_info_df <- reactive({
    req(input$file1)
    df <- data.frame(file_temp = input$file1$datapath,
                     file_name = input$file1$name,
                     upload_date = Sys.Date()) #%>% 
    # mutate(file_num = paste0("file_", 1:n()))
    return(df)
  })
  
  # Load the file with the reactive file options
  df_load <- reactive({
    req(input$file1)
    
    # Reactive function that is applied to all files in upload list
    # Also need to pass the file name
    df_load_func <- function(file_temp){
      df <- read.table(file_temp,
                       header = upload_opts$header,
                       skip = upload_opts$skip,
                       sep = upload_opts$sep,
                       dec = upload_opts$dec,
                       quote = upload_opts$quote,
                       fileEncoding = upload_opts$encoding, 
                       blank.lines.skip = TRUE,
                       fill = TRUE) %>%
        mutate(file_temp = file_temp)
      if(input$schema == "Alt_S"){
        df <- df %>% 
          dplyr::rename(Salinity = `Sal.`, Temperature = Temp,
                        Fluorescence_ugChla_l = `F..µg.l.`, T_FTU = `T..FTU.`, Depth = `Depth.u.`) %>% 
          mutate(date_time = dmy_hms(paste(Date, Time, sep = " "))) %>% 
          dplyr::select(file_temp, date_time, Depth, Salinity:Density)
      } else if(input$schema == "SAIV"){
        # NB: As more exceptions pop up, it may be better to have a logic gate for each column name
        # Or some other clever way to attempt soft column names changes
        if("Sal." %in% colnames(df) & "Con." %in% colnames(df) & "Depth.u." %in% colnames(df)){
          df <- df %>% 
            dplyr::rename(Salinity = `Sal.`, Conductivity = `Cond.`, Temperature = Temp,
                          Fluorescence_ugChla_l = `F..µg.l.`, T_FTU = `T..FTU.`, Depth = `Depth.u.`) %>% 
            mutate(date_time = dmy_hms(paste(Date, Time, sep = " "))) %>% 
            dplyr::select(file_temp, date_time, Depth, Salinity:Density)
        } else if("Sal." %in% colnames(df) & !"Con." %in% colnames(df) & "Depth.u." %in% colnames(df)){
          df <- df %>% 
            dplyr::rename(Salinity = `Sal.`, Temperature = Temp,
                          Fluorescence_ugChla_l = `F..µg.l.`, T_FTU = `T..FTU.`, Depth = `Depth.u.`) %>% 
            mutate(date_time = dmy_hms(paste(Date, Time, sep = " "))) %>% 
            dplyr::select(file_temp, date_time, Depth, Salinity:Density)
        } else if("Press" %in% colnames(df)){
          df <- df %>% 
            dplyr::rename(Salinity = `Sal.`, Conductivity = `Cond.`, Temperature = Temp,
                          Fluorescence_ugChla_l = `F..µg.l.`, T_FTU = `T..FTU.`, Pressure = Press) %>% 
            mutate(date_time = dmy_hms(paste(Date, Time, sep = " "))) %>% 
            dplyr::select(file_temp, date_time, Pressure, Salinity:Density)
        } else {
          df <- df %>% 
            dplyr::rename(Conductivity = `Cond.`, Temperature = Temp,
                          Fluorescence_ugChla_l = `F..µg.l.`, T_FTU = `T..FTU.`, Depth = `Depth.u.`) %>% 
            mutate(date_time = dmy_hms(paste(Date, Time, sep = " "))) %>% 
            dplyr::select(file_temp, date_time, Depth, Conductivity:Density)
        }
        
      } else if(input$schema == "RBR"){
        df <- df %>% 
          dplyr::rename(Fluorescence_ugChla_l = `Fluorometry.Chlorophyll`,  Specific_Conductivity = `Specific.Conductivity`,
                        Density_Anomaly = `Density.Anomaly`, Speed_of_sound = `Speed.of.sound`) %>% 
          mutate(date_time = dmy_hms(Timestamp)) %>% 
          dplyr::select(file_temp, date_time, Depth, Conductivity:Fluorescence_ugChla_l, Salinity:Speed_of_sound)
        
      } else if(input$schema == "Sea-Bird"){
        suppressWarnings( # Forcing numeric temperatures causes warning
        df <- df %>% 
          dplyr::rename(Temperature = V1, Conductivity = V2, Pressure = V3, Salinity = V4, date = V5, time = V6) %>%
          mutate(Temperature = as.numeric(Temperature)) %>%
          filter(!is.na(Temperature)) %>% 
          mutate(date_time = dmy_hms(paste0(date, time))) %>%
          dplyr::select(file_temp, date_time, Pressure, Temperature, Salinity, Conductivity)
        )
      } else if(input$schema == "Sea-Bird O2"){
        suppressWarnings( # Forcing numeric temperatures causes warning
          df <- df %>% 
            dplyr::rename(Temperature = V1, Conductivity = V2, Pressure = V3, Oxygen = V4, 
                          Salinity = V5, Sound_velocity = V6, date = V7, time = V8) %>%
            mutate(Temperature = as.numeric(Temperature)) %>%
            filter(!is.na(Temperature)) %>% 
            mutate(date_time = dmy_hms(paste0(date, time))) %>%
            dplyr::select(file_temp, date_time, Pressure, Temperature, Salinity, Conductivity, Oxygen, Sound_velocity)
        )
      } else if(input$schema == "Sea & Sun"){
          df <- df %>% 
            dplyr::rename(row_n = V1, date = V2, time = V3, Pressure = V4, 
                          Temperature = V5, Conductivity = V6, Salinity = V7) %>%
            mutate(date_time = dmy_hms(paste0(date, time)),
                   # NB: This requires better troubleshooting
                   Temperature = as.numeric(Temperature),
                   Pressure = as.numeric(Pressure),
                   Salinity = as.numeric(Salinity),
                   Conductivity = as.numeric(Conductivity)) %>%
            dplyr::select(file_temp, date_time, Pressure, Temperature, Salinity, Conductivity)
      }
      
      return(df)
    }
    
    # Upload all selected files
    df_load <- purrr::map_dfr(input$file1$datapath, df_load_func) %>% 
      left_join(file_info_df(), by = "file_temp") %>% 
      mutate(Uploader = reactiveValuesToList(res_auth)[[1]]) %>% 
      dplyr::select(Uploader, upload_date, file_name, everything(), -file_temp)
    
    # Exit
    return(df_load)
  })
  
  # Table showing the uploaded file
  output$contents_load <- DT::renderDataTable({
    req(input$file1)
    df_load <- df_load()
    # file_info_df <- file_info_df()
    df_load_DT <- datatable(df_load, 
                            options = list(pageLength = 20, scrollX = TRUE, scrollY = 650))
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
      # NB: This first schema procs for both SAIV and ALt_S
      if(str_sub(file_text, 1, 10) == "From file:"){
        ins_no_raw <- sapply(str_split(file_text, "Instrument no.:"), "[[", 2)
        # mini_df_1 <- read.csv("../test_data/150621_KB4.txt", nrows = 1, skip = 1, sep = ";", dec = ",")
        mini_df_1 <- read.csv(file_temp, nrows = 1, skip = 1, sep = ";", dec = ",")
        df_meta <- data.frame(file_temp = file_temp,
                              Site = as.character(NA),
                              Lon = as.numeric(NA),
                              Lat = as.numeric(NA),
                              Owner_person = as.character(NA),
                              Owner_institute = as.character(NA),
                              DOI = as.character(NA),
                              Sensor_owner = "Kings Bay",
                              Sensor_brand = "SAIV",
                              Sensor_number = as.character(gsub("[^0-9.-]", "", str_sub(ins_no_raw, 1, 15))),
                              Air_pressure = mini_df_1$Air.pressure)
      } else if(str_sub(file_text, 1, 8) == "RBR data"){
        # ins_no_raw <- sapply(str_split(file_text, "Serial Number:"), "[[", 2)
        # mini_df_1 <- read_csv("data/KB3_018630_20210416_1728.csv", n_max = 1)
        mini_df_1 <- read_csv(file_temp, n_max = 1)
        df_meta <- data.frame(file_temp = file_temp,
                              Site = as.character(NA),
                              Lon = as.numeric(NA),
                              Lat = as.numeric(NA),
                              Owner_person = as.character(NA),
                              Owner_institute = as.character(NA),
                              DOI = as.character(NA),
                              Sensor_owner = as.character(NA),
                              Sensor_brand = "RBR",
                              Sensor_number = as.character(mini_df_1$...4))
      } else {
        df_meta <- data.frame(file_temp = file_temp,
                              Site = as.character(NA),
                              Lon = as.numeric(NA),
                              Lat = as.numeric(NA),
                              Owner_person = as.character(NA),
                              Owner_institute = as.character(NA),
                              DOI = as.character(NA),
                              Sensor_owner = as.character(NA),
                              Sensor_brand = as.character(NA),
                              Sensor_number = as.character(NA))
      }
      return(df_meta)
    }
    
    # Extract meta-data for all uploaded files
    df_meta <- purrr::map_dfr(input$file1$datapath, file_meta_func) %>% 
      left_join(file_info_df(), by = "file_temp") %>% 
      mutate(upload_date = as.character(upload_date)) %>%  # For rHandsOnTable date formatting
      dplyr::select(upload_date, file_name, everything(), -file_temp)
    
    # Exit
    return(df_meta)
  })
  
  # Interactive meta-datatable
  table <- reactiveValues()
  output$table1output <- renderRHandsontable({
    rhandsontable(file_meta_all(), stretchH = "all", useTypes = F) %>% 
      # hot_col("file_num", readOnly = TRUE) %>% 
      hot_col("file_name", readOnly = TRUE) %>% 
      hot_col("upload_date", readOnly = TRUE) %>% 
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
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Site), col = 3, session = session, table$table1$Site[1])
    if(input$allSite == "Site 1"){
      shiny::updateNumericInput(inputId = "allLon", value = 12)
      shiny::updateNumericInput(inputId = "allLat", value = 78.95)
    } else if(input$allSite == "Site 2"){
      shiny::updateNumericInput(inputId = "allLon", value = 11.5)
      shiny::updateNumericInput(inputId = "allLat", value = 79.05)
    } else if(input$allSite == "Kb0"){
      shiny::updateNumericInput(inputId = "allLon", value = 11.1393333333333)
      shiny::updateNumericInput(inputId = "allLat", value = 79.0463333333333)
    } else if(input$allSite == "Kb1"){
      shiny::updateNumericInput(inputId = "allLon", value = 11.4276666666667)
      shiny::updateNumericInput(inputId = "allLat", value = 79.0111666666667)
    } else if(input$allSite == "Kb2"){
      shiny::updateNumericInput(inputId = "allLon", value = 11.7318333333333)
      shiny::updateNumericInput(inputId = "allLat", value = 78.978)
    } else if(input$allSite == "Kb3"){
      shiny::updateNumericInput(inputId = "allLon", value = 11.9563333333333)
      shiny::updateNumericInput(inputId = "allLat", value = 78.954)
    } else if(input$allSite == "Kb4"){
      shiny::updateNumericInput(inputId = "allLon", value = 12.19645)
      shiny::updateNumericInput(inputId = "allLat", value = 78.9106)
    } else if(input$allSite == "Kb5"){
      shiny::updateNumericInput(inputId = "allLon", value = 12.4408333333333)
      shiny::updateNumericInput(inputId = "allLat", value = 78.8965)
    } else if(input$allSite == "Kb6"){
      shiny::updateNumericInput(inputId = "allLon", value = 12.3851666666667)
      shiny::updateNumericInput(inputId = "allLat", value = 78.9301666666667)
    } else if(input$allSite == "Kb7"){
      shiny::updateNumericInput(inputId = "allLon", value = 12.3766666666667)
      shiny::updateNumericInput(inputId = "allLat", value = 78.9663333333333)
    } else if(input$allSite == "Kb8"){
      shiny::updateNumericInput(inputId = "allLon", value = 12.522)
      shiny::updateNumericInput(inputId = "allLat", value = 78.8888)
    } else if(input$allSite == "V6"){
      shiny::updateNumericInput(inputId = "allLon", value = 7.77066666666667)
      shiny::updateNumericInput(inputId = "allLat", value = 78.9065)
    } else if(input$allSite == "V10"){
      shiny::updateNumericInput(inputId = "allLon", value =  8.547)
      shiny::updateNumericInput(inputId = "allLat", value = 78.9326666666667)
    } else if(input$allSite == "V12"){
      shiny::updateNumericInput(inputId = "allLon", value = 9.49616666666667)
      shiny::updateNumericInput(inputId = "allLat", value = 78.9798333333333)
    } else {
      # Intentionally blank
    }
  })
  ## Longitude
  observeEvent(input$allLon, {
    table$table1$Lon <- input$allLon
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Lon), col = 4, session = session, table$table1$Lon[1])
  })
  ## Latitude
  observeEvent(input$allLat, {
    table$table1$Lat <- input$allLat
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Lat), col = 5, session = session, table$table1$Lat[1])
  })
  ## Data owner (person)
  observeEvent(input$allDataOwnerPerson, {
    table$table1$Owner_person <- input$allDataOwnerPerson
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Owner_person), col = 6, session = session, table$table1$Owner_person[1])
  })
  ## Data owner (institute)
  observeEvent(input$allDataOwnerInstitute, {
    table$table1$Owner_institute <- input$allDataOwnerInstitute
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Owner_institute), col = 7, session = session, table$table1$Owner_institute[1])
  })
  ## Data owner (institute)
  observeEvent(input$allDOI, {
    table$table1$DOI <- input$allDOI
    rhandsontable::set_data("table1output", row = 1:length(table$table1$DOI), col = 8, session = session, table$table1$DOI[1])
  })
  ## Sensor owner
  observeEvent(input$allSensorOwner, {
    table$table1$Sensor_owner <- input$allSensorOwner
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Sensor_owner), col = 9, session = session, table$table1$Sensor_owner[1])
  })
  ## Sensor brand
  observeEvent(input$allSensorBrand, {
    table$table1$Sensor_brand <- input$allSensorBrand
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Sensor_brand), col = 10, session = session, table$table1$Sensor_brand[1])
  })
  ## Sensor number
  observeEvent(input$allSensorNumber, {
    table$table1$Sensor_number <- input$allSensorNumber
    rhandsontable::set_data("table1output", row = 1:length(table$table1$Sensor_number), col = 11, session = session, table$table1$Sensor_number[1])
  })
  
  # Reactive data for datatable
  df_time <- reactive({
    req(input$file1, table$table1$file_name, df_load())
    df_meta <- as.data.frame(table$table1) %>%
      mutate(file_name = as.character(file_name)) %>% 
      mutate(upload_date = as.Date(upload_date))
    df_time <- df_load() %>% 
      mutate(file_name = as.character(file_name)) %>%
      left_join(df_meta, by = c("file_name", "upload_date")) %>%  #, "file_num")) %>% 
      dplyr::select(Uploader, upload_date, file_name, Owner_person, Owner_institute, DOI,
                    Sensor_owner, Sensor_brand, Sensor_number, Site, Lon, Lat, everything())
    return(df_time)
  })
  
  output$contentsTime <- DT::renderDataTable({
    req(input$file1, table$table1)
    df_time <- df_time()
    df_time_DT <- datatable(df_time, options = list(pageLength = 20, scrollX = TRUE, scrollY = 300))
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
  

  ## QC/Up server -------------------------------------------------------------

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

  # Create reactive data_base object that recognizes uploads of new data
  data_base <- reactiveValues()
  data_base$df <- read_rds("data_base.Rds")
  
  # Check that all necessary meta-data has been provided
  # NB: Would be good to add group_by(file_name)
  df_meta_check <- reactive({
    req(input$file1, table$table1$file_name, df_time())
    df_meta_check <- df_time() %>% 
      dplyr::select(Owner_person, Owner_institute, Sensor_owner, Sensor_brand, Sensor_number, Site, Lon, Lat) %>% 
      mutate(Owner_person = case_when(is.na(Owner_person) ~ 1, TRUE ~ 0),
             Owner_institute = case_when(is.na(Owner_institute) ~ 1, TRUE ~ 0),
             # DOI = case_when(is.na(DOI) ~ 1, TRUE ~ 0), # Not required
             Sensor_owner = case_when(is.na(Sensor_owner) ~ 1, TRUE ~ 0),
             Sensor_brand = case_when(is.na(Sensor_brand) ~ 1, TRUE ~ 0),
             Sensor_number = case_when(is.na(Sensor_number) ~ 1, TRUE ~ 0),
             Site = case_when(is.na(Site) ~ 1, TRUE ~ 0),
             Lon = case_when(is.na(Lon) ~ 1, TRUE ~ 0),
             Lat = case_when(is.na(Lat) ~ 1, TRUE ~ 0)) %>% 
      summarise_all(sum)
    return(df_meta_check)
  })
  
  # Provide message for missing meta-data
  output$uploadText <- renderText({
    req(df_meta_check())
    df_meta_check <- df_meta_check()
    if(sum(df_meta_check) == 0){
      meta_check_text <-  "All good. <br> Click to upload data to database. <br>"
    } else {
      meta_check_text <- paste0("Add missing meta-data before uploading: <br>",
                                "Owner_person: ",df_meta_check$Owner_person," rows <br>",
                                "Owner_institute: ",df_meta_check$Owner_institute," rows <br>",
                                "DOI: Not required <br>",
                                "Sensor_owner: ",df_meta_check$Sensor_owner," rows <br>",
                                "Sensor_brand: ",df_meta_check$Sensor_brand," rows <br>",
                                "Site: ",df_meta_check$Site," rows <br>",
                                "Lon: ",df_meta_check$Lon," rows <br>",
                                "Lat: ",df_meta_check$Lat," rows")
      }
    return(meta_check_text)
  })
  
  # Reactive upload button
  output$uploadButton <- renderUI({
    req(df_meta_check())
    if(sum(df_meta_check()) == 0){
      actionButton("upload", "Upload", icon = icon("upload"))
    } else {
      # actionButton("uploadFail", "Missing meta-data", icon = icon("skull"))
    }
  })
  
  # When the Upload button is clicked, save df_time()
  observeEvent(input$upload, {
    # saveData(df_time())
    df_res <- bind_rows(df_time(), data_base$df) %>% distinct()
    write_rds(df_res, file = "data_base.Rds", compress = "gz")
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
    df_time_DT <- datatable(df_time, options = list(pageLength = 20, scrollX = TRUE, scrollY = 250))
    return(df_time_DT)
  })
  
  # Show the newly expanded database
  output$dataBase <- DT::renderDataTable({
    df_db_summary <- df_data_base() %>% 
      group_by(Uploader, upload_date, Owner_person, Owner_institute, DOI, Sensor_owner, Sensor_brand, Sensor_number, Site) %>% 
      summarise(rows = n(), .groups = "drop")
    data_base_DT <- datatable(df_db_summary, options = list(pageLength = 20, scrollX = TRUE, scrollY = 200))
    return(data_base_DT)
  })
  
  
  ## Download server --------------------------------------------------------

  # Filter data
  ## Subset by data uploaders
  output$selectUpUI <- renderUI({
    # req(input$selectSite)
    selectizeInput('selectUp', 'Data uploader',
                   choices = unique(df_data_base()$Uploader), multiple = T,
                   selected = unique(df_data_base()$Uploader)#,
                   # NB: If one wants to start with no values selected
                   # options = list(
                   #   placeholder = 'Select data uploader(s)',
                   #   onInitialize = I('function() { this.setValue(""); }')
                   # )
    )
  })
  
  ## Subset by data owners (person)
  output$selectDOPUI <- renderUI({
    selectizeInput('selectDOP', 'Data owner (person)',
      choices = unique(df_data_base()$Owner_person), multiple = T,
      selected = unique(df_data_base()$Owner_person))
  })
  
  ## Subset by data owners (person)
  output$selectDOInUI <- renderUI({
    selectizeInput('selectDOIn', 'Data owner (institute)',
                   choices = unique(df_data_base()$Owner_institute), multiple = T,
                   selected = unique(df_data_base()$Owner_institute))
  })
  
  ## Subset by sensor owner
  output$selectSOUI <- renderUI({
    selectizeInput('selectSO', 'Sensor owner',
      choices = unique(df_data_base()$Sensor_owner), multiple = T,
      selected = unique(df_data_base()$Sensor_owner))
  })
  
  ## Lon
  output$slideLonUI <- renderUI({
    # req(input$selectVar)
    min_lon <- floor_dec(min(df_data_base()$Lon), 2); max_lon <- ceiling_dec(max(df_data_base()$Lon), 2)
    shiny::sliderInput("slideLon", "Longitude range", value = c(min_lon, max_lon), min = min_lon, max = max_lon)
  })
  
  # Lat
  output$slideLatUI <- renderUI({
    # req(input$selectVar)
    min_lat <- floor_dec(min(df_data_base()$Lat), 2); max_lat <- ceiling_dec(max(df_data_base()$Lat), 2)
    shiny::sliderInput("slideLat", "Latitude range", value = c(min_lat, max_lat), min = min_lat, max = max_lat)
  })
  
  # Depth
  output$slideDepthUI <- renderUI({
    # req(input$selectVar)
    max_depth <- ceiling(max(df_data_base()$Depth))
    shiny::sliderInput("slideDepth", "Depth range", value = c(0, max_depth), min = 0, max = max_depth)
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
    if(length(input$selectDOP) == 0){
      df_filter <- data.frame(Empty = "Please expand your search in the blue 'Controls' panel on the left.")
    } else if(length(input$selectDOP) > 0){
      req(input$slideLon)
      df_filter <- df_data_base() %>%
        filter(Uploader %in% input$selectUp,
               Owner_person %in% input$selectDOP,
               Owner_institute %in% input$selectDOIn,
               Sensor_owner %in% input$selectSO,
               Lon >= input$slideLon[1], Lon <= input$slideLon[2],
               Lat >= input$slideLat[1], Lat <= input$slideLat[2],
               Depth >= input$slideDepth[1], Depth <= input$slideDepth[2],
               date_time >= as.POSIXct(input$slideDate[1]-1), date_time <= as.POSIXct(input$slideDate[2]+1))
    } else {
      df_filter <- data.frame(warning = "Something has gone wrong :(")
    }
    return(df_filter)
  })
  
  # Show the filtered database
  output$dataBaseFilter <- DT::renderDataTable({
    data_base_filter_DT <- datatable(df_filter(), options = list(pageLength = 20, scrollX = TRUE, scrollY = 250))
    return(data_base_filter_DT)
  })
  
  # Controls for X-axis of DL time series plot
  output$plotXUIDL <- renderUI({
    # req(input$selectCols)
    shiny::selectInput("plotXDL", "X axis", multiple = FALSE,
                       choices = colnames(df_data_base())[!colnames(df_data_base()) %in% c("file_name", "lon", "lat")], 
                       selected = "date_time")
  })
  
  # Controls for Y-axis of DL time series plot
  output$plotYUIDL <- renderUI({
    # req(input$selectCols)
    shiny::selectInput("plotYDL", "Y axis", multiple = FALSE,
                       choices = colnames(df_data_base())[!colnames(df_data_base()) %in% c("file_name", "lon", "lat")],
                       selected = "Temperature")
  })
  
  # Time series plot for downloadable data
  output$tsPlot <- renderPlot({
    req(input$plotXDL)

    df_filter <- df_filter()
    
    if(nrow(df_filter) == 1){
      ts <- ggplot() + geom_blank()
    } else{
      ts <- ggplot(data = df_filter, aes_string(x = input$plotXDL, y = input$plotYDL)) +
        geom_point() + #geom_line() +
        theme(panel.border = element_rect(colour = "black", size = 1, fill = NA))
    }
    return(ts)
  })
  
  # Map for downloadable data
  output$mapDL <- renderPlot({
    # req(input$file1, table$table1)

    df_filter <- df_filter()

    # Check that coords are present
    if(length(na.omit(df_filter$Lon)) == 0 | length(na.omit(df_filter$Lat)) == 0){

      # If no coords, red border
      mp <- frame_base +
        theme(panel.border = element_rect(colour = "red", size = 5))

    } else {

      # Get unique coordinates and count of data
      df_point <- df_filter %>%
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
  

  ## Instructions ------------------------------------------------------------

  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = tags$span(
        "The Kongsfjorden CTD data portal is laid out on a dashboard with a ",tags$em("side bar"),
        " on the left and additional controls and plots in the ",tags$em("body"),".",
        # tags$h3(icon("bars"),"Side bar", style = "color: skyblue;"),
        # img(src = "Portal process visualization.png", align = "center", width = "800px"),
        tags$h3(icon("desktop"),tags$b("1. Load file(s)")),  
        tags$ul(style = "text-align: justify;",
          tags$li("In the blue box ('File format') you can identify the files you want to upload and in which format they are"),
          tags$ul(
            tags$li("Click on browse and select the file(s) you want to upload (can be multiple files at once)"),
            tags$li("If the software recognized the file type, it will suggest to you a pre-described file format (e.g. for SAIV or RBR), you can also select a file schema in the drop down menu"),
            tags$li("Alternatively, you can change the file format settings manually, and check in the green box whether your data is not shown correctly"),
            tags$li("f you want to upload several files for a CTD that is not a SAIV (e.g. the instrument provided by Kingsbay) or RBR instrument (e.g. the instrument operated by AWIPEV), contact us in advance to see whether it is possible to include an automated upload scheme for your instrument type")
          ),
          tags$li("In the green box, you can check how the data is being uploaded. Double check e.g. if decimals are displayed correctly")
        ),
        
        tags$h3(icon("cog"),tags$b("2. Metadata")),
        tags$ul(style = "text-align: justify;",
          tags$li("The information to be provided in this page is critically needed to upload data"),
          tags$li("In the red box ('Fill all values') you can add information to all currently uploaded files"),
          tags$ul(
            tags$li("If your profiles have been collected at one of the pre-described stations (e.g. KB3), you can select this station from the drop down menu"),
            tags$li("If you select a pre-described station, lat/long values will appear in the blue box for all uploaded files"),
            tags$li("If your station is not in the drop down menu or if your station has no fixed ID, the individual lat/long values can be edited manually in the blue box (see below)"),
            tags$li("Add the name of the data owner"),
            tags$li("The sensor owner (e.g. Kingsbay for the SAIV data format), brand and number are pre-described based on the file structure"),
            tags$li("If these values are missing or incorrect, they can be changed/added in the blue box (see below) for all currently uploaded files"),
            ),
          tags$li("The blue box ('Fill individual Values') can be used to add or edit metadata for individual files (e.g. if you upload files form different stations together)"),
          tags$ul(
            tags$li("This works like a normal spreadsheet (e.g. excel), where you can type, drag, and copy past from a different file)"),
            tags$li("If you want to upload files form different stations together, it makes sense to prepare a table with site, lat, long info that can be directly copy-pasted into this box")
            ),
          tags$li("The map (orange box) shows you the lat long position of your data"),
          tags$ul(
            tags$li("If the box around the map is green, all data are within the geographic range, if it is orange some or all data are located outside"),
            tags$li("In this case you may want to double check your lat/long entries")
          ),
          tags$li("If your data have already been published, please add the DOI [DOI field still needs to be implemented] of the primary data publication (in this case, the data will be accessible via the database, but will not be published in Pangaea)"),
          tags$ul(
            tags$li("If you want your data to be published via the automated pipeline provided by the app, leave the DOI field empty"),
            tags$li("If your data needs be published in another database first, write ‘xxx’ into the DOI field"),
            tags$li("You can add a DOI to your dataset later via the '4) Edit' tab (see below)"),
            tags$li("Please see the 'About' tab for more details on DOI's and publishing of data")
          )
        ),
        
        tags$h3(icon("upload"),tags$b("3. QC/Upload")),
        tags$ul(style = "text-align: justify;",
          tags$li("[Not yet implemented]"),
          ), 
        
        tags$h3(icon("shower"),tags$b("4. Edit")),
        tags$ul(style = "text-align: justify;",
          tags$li("[Not yet implemented]"),
          ), 
        
        tags$h3(icon("download"),tags$b("5. Download")),
        tags$ul(style = "text-align: justify;",
          tags$li("On this page, uploaded data (and published data with DOI [not yet implemented]) can be found and downloaded"),
          tags$li("In the blue box (‘controls’), you can select the range of data you want to download in terms of 6 characteristics"),
          tags$ul(
            tags$li("All data and sensor owners are displayed, but can be omitted by clicking on the name and deleting it [to be improved]"),
            tags$li("Deleted data or sensor owners can be added back via the appearing drop down menu"),
            tags$li("Data can be downloaded as .csv or .rds files via the download data button"),
            ),
          tags$li("The selected data points are shown in the green data box and their locations are illustrated in the orange map box"),
          tags$li("To illustrate data distribution and ranges, all selected data can be plotted in the red time series box by selecting the variables for the x and y axis of the plot")
          ),
        
        tags$h3(icon("question"),tags$b("About")),
        tags$ul(style = "text-align: justify;",
          tags$li("This tab contains more specific information and acknowledgments for the portal."),
          ), 
        
        tags$h3(icon("address-book"),"Contact", style = "color: royalblue;"),
        "For questions etc. please contact Robert Schlegel: robert.schlegel@imev-mer.fr"
      ),
      
      html = TRUE,
      type = "info",
      width = "50%"
    )
    
  })
  
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

