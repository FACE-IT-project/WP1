# shiny/kongCTD/app.R
# This single script contains the code used to run the app for uploading Kongsfjorden CTD data


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

# Datatable options for all tables
# options(DT.options = list(pageLength = 10,
#                           # autoWidth = TRUE,
#                           # This chunk of code only allows six characters to be shown in a column
#                           # But it interferes with the HTML code for popups and links
#                           # columnDefs = list(list(
#                           #   targets = list(9),
#                           #   render = JS(
#                           #     "function(data, type, row, meta) {",
#                           #     "return type === 'display' && data.length > 6 ?",
#                           #     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
#                           #     "}"))),
#                           # columnDefs = list(list(width = '20%', targets = c(2))),
#                           # deferRender = TRUE,
#                           scrollX = TRUE,
#                           # scrollY = TRUE
#                           scrollY = 300#,
#                           # scrollCollapse = TRUE,
#                           # lengthMenu = c(5, 10, 25, 50, 100)))
#                           ))

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
                menuItem("2) Set time", tabName = "time", icon = icon("clock")),
                menuItem("3) Clean data", tabName = "tidy", icon = icon("shower")),
                menuItem("4) Upload", tabName = "upload", icon = icon("upload")),
                menuItem("About", tabName = "about", icon = icon("question")),
                
                # The reactive controls based on the primary option chosen
                uiOutput(outputId = "sidebar_controls"))
  ),
  
  # The dashboard
  dashboardBody(
    tabItems(
      

      # Load menu ---------------------------------------------------------------

      tabItem(tabName = "load",
              
              sidebarPanel(width = 3,
                           
              # Input: Select a file
              h4("Load file to begin"),
              fileInput("file1", "Choose CSV/TXT File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              
              # Horizontal line
              # tags$hr(),
              
              fluidRow(
                # Checkbox if file has header
                column(6, checkboxInput("header", "Header", TRUE)),
                # Skip header rows
                column(6, shiny::numericInput("skip", "Skip rows", value = 0, min = 0, step = 1))
              ),
          
              fluidRow(
                # Select separator
                column(6, radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ",")),
                # Select decimal
                column(6, radioButtons("dec", "Decimal",
                                       choices = c("Full stop" = ".",
                                                   Comma = ","),
                                       selected = "."))
              ),
              
              fluidRow(
                # Select quotes
                column(6, radioButtons("quote", "Quote", 
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),),
                # Select file encoding
                column(6, radioButtons("encoding", "Encoding",
                                       choices = c(UTF8 = "UTF-8",
                                                   Latin = "latin1"),
                                       selected = "UTF-8"))
              ),
              
              # Horizontal line
              # tags$hr(),
              
              # Input: Select rows to display
              radioButtons("disp1", "Display",
                           choices = c(Head = "head",
                                       Tail = "tail"),
                           selected = "head"),
              
              # Load data once they are decent
              # actionButton("load", "Load"),
              
              # Hints for how to handle issues
              h4("Hints"),
              h6("Only one column -> Change 'Separator'"),
              h6("Commas instead of decimal places -> Change 'Decimal'"),
              h6("'Error: more columns than column names' -> Increase 'Skip rows'"),
              h6("'Error: invalid multibyte string 6' -> Change 'Encoding'")
              ),
              
              # The data display
              mainPanel(DT::dataTableOutput("contents_load"), width = 9)
      ),
      

      # Time menu ---------------------------------------------------------------
      
      tabItem(tabName = "time",

              sidebarPanel(width = 3,

                           # Combine date and time columns
                           h4("Choose date/time column(s) to begin"),
                           h6("If two columns -> date + time"),
                           h6("Only acknowledges first two columns selected"),
                           uiOutput("timeColsUI"),

                           # Coerce to POSIXCt
                           h6("Change selection if 'date_time_posix' is blank"),
                           radioButtons("posix", "Date/Time format",
                                        choices = c("dmy", "mdy", "ymd",
                                                    "dmy_hms", "mdy_hms", "ymd_hms"),
                                        selected = "dmy"),

                           # Set time zone
                           # Currently not an option because all CTDs in Kong should be UTC0

                           # Input: Select rows to display
                           radioButtons("disp2", "Display",
                                        choices = c(Head = "head",
                                                    Tail = "tail"),
                                        selected = "head")

              ),
              
              # The data display
              mainPanel(DT::dataTableOutput("contents_time"), width = 9)

      ),

      # Tidy menu ---------------------------------------------------------------

      tabItem(tabName = "tidy",

              fluidRow(
                box(h5("Choose column order to begin"),
                    
                    # Combine date and time columns
                    uiOutput("selectColsUI"),
                    
                    # Filter head of df
                    shiny::numericInput("sliceHead", "Remove first n rows", value = 0, min = 0, step = 1),
                    
                    # Filter tail of df
                    shiny::numericInput("sliceTail", "Remove last n rows", value = 0, min = 0, step = 1),
                    
                    # Add lon column
                    
                    # Add lat column
                    
                    # Input: Select rows to display
                    radioButtons("disp3", "Display",
                                 choices = c(Head = "head",
                                             Tail = "tail"),
                                 selected = "head"), 
                    width = 3, height = "500px", title = "Controls",
                    status = "primary", solidHeader = TRUE, collapsible = FALSE),
                
                # The data display
                box(DT::dataTableOutput("contents_tidy", height = "300px"), width = 9, height = "500px", title = "Tidy data",
                    status = "primary", solidHeader = TRUE, collapsible = FALSE)
              ),
              
              fluidRow(
                box(                    
                  dropdownButton(
                    h4("Plot axis controls:"),
                    uiOutput("plotXUI"),
                    uiOutput("plotYUI"),
                    circle = TRUE, status = "danger", icon = icon("gear")),
                  plotOutput("tsPlot", height = "250px"), width = 12, height = "350px", title = "Time series",
                  status = "primary", solidHeader = TRUE, collapsible = FALSE)
              )
      ),

      
      # Upload menu -------------------------------------------------------------

      tabItem(tabName = "upload",
              fluidPage(
                column(12,
                       # h2(tags$b("About")),
                       p("Currently no upload functionality exists. The issue of how to control column names needs
                         to be addressed first.")
                )
              )
      ),

      # App explanation ---------------------------------------------------------
      
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {


  # Load server -------------------------------------------------------------

  df_load <- reactive({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    
    df_load <- read.csv(input$file1$datapath,
                        skip = input$skip,
                        header = input$header,
                        sep = input$sep,
                        dec = input$dec,
                        quote = input$quote,
                        fileEncoding = input$encoding, 
                        blank.lines.skip = TRUE)
    
    # Exit
    return(df_load)
  })
  
  output$contents_load <- DT::renderDataTable({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    
    # test <- read.csv("shiny/kongCTD/data/August Bailey_0408_1141.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1")
    
    df_load <- df_load()
    
    df_load_DT <- datatable(df_load, 
                            options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE))
    # if(input$disp1 == "head") {
    #   return(head(df_load, 10))
    # }
    # else {
    #   return(tail(df_load, 10))
    # }
  })
  

  # Time server -------------------------------------------------------------

  output$timeColsUI <- renderUI({
    req(input$file1)
    req(is.data.frame(df_load()))
    shiny::selectInput("timeCols", "Date + Time column(s)", choices = colnames(df_load()), multiple = T)
  })
  
  df_time <- reactive({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    req(input$timeCols)
    
    df_time <- df_load()
    
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
      dplyr::select(date_time_posix, date_time_char, input$timeCols, everything())
    
    # Exit
    return(df_time)
  })
  
  output$contents_time <- DT::renderDataTable({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    
    df_time <- df_time()
    
    # if(input$disp2 == "head") {
    #   return(head(df_time, 10))
    # }
    # else {
    #   return(tail(df_time, 10))
    # }
  })
  

  # Tidy server -------------------------------------------------------------
  
  output$selectColsUI <- renderUI({
    req(input$file1)
    req(is.data.frame(df_time()))
    shiny::selectInput("selectCols", "Select column order", choices = colnames(df_time()), multiple = TRUE)
  })

  output$plotXUI <- renderUI({
    req(input$selectCols)
    shiny::selectInput("plotX", "X axis", choices = colnames(df_tidy()), multiple = FALSE)
  })
  
  output$plotYUI <- renderUI({
    req(input$selectCols)
    shiny::selectInput("plotY", "Y axis", choices = colnames(df_tidy()), multiple = FALSE)
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
    # input <- data.frame(sliceHead = 2,
    #                     sliceTail = 2)
    if(input$sliceHead > 0){
      # df_tidy <- test
      df_tidy <- df_tidy[-seq_len(input$sliceHead),]
    }
    if(input$sliceTail > 0){
      # df_tidy <- test
      tail_front <- nrow(df_tidy)-input$sliceTail+1
      df_tidy <- df_tidy[-seq(tail_front, nrow(df_tidy)),]
    }

    # Exit
    return(df_tidy)
  })

  output$contents_tidy <- DT::renderDataTable({

    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    req(input$selectCols)

    df_tidy <- df_tidy()

    # datatable()
    
    # if(input$disp3 == "head") {
    #   return(head(df_tidy, 10))
    # }
    # else {
    #   return(tail(df_tidy, 10))
    # }
  })


  # Upload server -----------------------------------------------------------

  # Nothing here yet
  # Need to decide on a back-end
  # This in turn is to be dictated by a number of things
  # Also need to figure out what to do with random column names
  
  session$onSessionEnded(stopApp)
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

