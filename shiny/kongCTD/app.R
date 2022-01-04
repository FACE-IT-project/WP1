# shiny/kongCTD/app.R
# This single script contains the code used to run the app for uploading Kongsfjorden CTD data

# TODO: Put the loading options in the proper side bar. That way when user go from load to tidy the plotting area remains the same
# The plotting area should contain a table, a time series, and a map window. So users get a more complete representation of the data.
# The map will require that lon/lat be entered, and the time series will require that the data column is sorted.
# The creation of the date column represents an intriguing challenge that I haven't yet thought of a solution to.
# Another issue is how to rename the columns appropriately. One option would be to provide a list of pre-approved column names.
# There could also be the option to provide a wildcard name.


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
# library(ggraph)
library(plotly)
# library(see)
# library(ggtext)


# Data --------------------------------------------------------------------

# Load data
# load("CTD_pre_interp.Rdata")
# load("CTD_interp_clim.Rdata")
# load("CTD_interp_yearly.Rdata")
# load("CTD_interp_monthly.Rdata")

# List of desired variables
# choose_vars <- c("sst", "bottomT", "sss", "mld_cum", "mld_1_cum", "t2m", "tcc_cum", "p_e_cum", "mslp_cum", "wind_spd_cum",
#                  "lwr_mld_cum", "swr_mld_cum", "lhf_mld_cum", "shf_mld_cum", "qnet_mld_cum",
#                  "lwr_budget", "swr_budget", "lhf_budget", "shf_budget", "qnet_budget", "sst_thresh")

# List of variable names used in selection widgets
# name_vars <- c("SST", "T_bottom", "SSS", "MLD_cum", "MLD_1_cum", "T_air", "TCC_cum", "Wind_cum",
#                "P-E_cum", "MSLP_cum", "T_Qlw", "T_Qsw", "T_Qlh", "T_Qsh", "T_Qnet")

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
  # dashboardHeader(title = "Correlation between SST and other variable anomalies during MHWs"),
  dashboardHeader(title = "Kongsfjorden CTD"),
  
  # The primary options
  dashboardSidebar(
    sidebarMenu(id = "mainMenu",
                
                # The various menus
                menuItem("Upload", tabName = "upload", icon = icon("book"), selected = TRUE),
                menuItem("Tidy", tabName = "tidy", icon = icon("gear", lib = "glyphicon")),
                # menuItem("Plot", tabName = "plot", icon = icon("map")),
                # menuItem("Analyse", tabName = "analyse", icon = icon("chart-pie")),
                # menuItem("Download", tabName = "download", icon = icon("chart-bar")),
                menuItem("About", tabName = "about", icon = icon("question")),
                
                # The reactive controls based on the primary option chosen
                uiOutput(outputId = "sidebar_controls"))
  ),
  
  # The dashboard
  dashboardBody(
    tabItems(
      

      # Upload menu -------------------------------------------------------------

      tabItem(tabName = "upload",
              
              sidebarPanel(width = 3,
              # Input: Select a file
              fileInput("file1", "Choose CSV/TXT File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              
              # Horizontal line
              tags$hr(),
              
              shiny::numericInput("skip", "Skip rows", value = 0, min = 0, step = 1),
              
              # Input: Checkbox if file has header
              checkboxInput("header", "Column names", TRUE),
              
              # Input: Select separator
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ","),
              
              # Input: Select decimal
              radioButtons("dec", "Decimal",
                           choices = c("Full stop" = ".",
                                       Coma = ","),
                           selected = "."),
              
              # Input: Select quotes
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"'),
              
              # Input: Select file encoding
              radioButtons("encoding", "Encoding",
                           choices = c(UTF8 = "UTF-8",
                                       Latin = "latin1"),
                           selected = "UTF-8"),
              
              # Horizontal line
              tags$hr(),
              
              # Input: Select rows to display
              radioButtons("disp", "Display",
                           choices = c(Head = "head",
                                       Tail = "tail"),
                           selected = "head"),
              
              # Load data once they are decent
              # actionButton("load", "Load"),
              
              # Hints for how to handle issues
              h3("Hints"),
              h4("Error: more columns than column names -> Increase 'Skip rows'"),
              h4("Error: invalid multibyte string 6 -> Change 'Encoding'")
              ),
              
              # The data display
              mainPanel(width = 9,
                tableOutput("contents_load")
              )
              
      ),
      

      # Tidy menu ---------------------------------------------------------------
      
      tabItem(tabName = "tidy",
              
              sidebarPanel(width = 3,
                           
                           # Column names
                           textInput("colNames", "Column names", value = NULL),
                           
                           # Combine date and time columns
                           shiny::selectInput("colOpts", "Time + Date columns", choices = colnames(), multiple = T),
                           
                           # Coerce to POSIXCt
                           
                           # Set time zone
                           
                           # Input: Select rows to display
                           radioButtons("disp", "Display",
                                        choices = c(Head = "head",
                                                    Tail = "tail"),
                                        selected = "head"),
                           
                           # Hints for correcting issues
                           h3("Hints"),
                           h4("Error: more columns than column names -> Increase 'Skip rows'"),
                           h4("Error: invalid multibyte string 6 -> Change 'Encoding'")
              ),
              mainPanel(width = 9,
                        tableOutput("contents_tidy")
              )
              
      ),
      
      
      # Map figures -------------------------------------------------------------
      
      # tabItem(tabName = "map",
      #         fluidRow(box(plotOutput("mapRegions", height = "680px"), width = 6, height = "740px", title = "Region map",
      #                      status = "primary", solidHeader = TRUE, collapsible = TRUE),
      #                  box(plotlyOutput("eventLolli", height = "680px"), width = 6, height = "740px", title = "MHW Lollis",
      #                      status = "primary", solidHeader = TRUE, collapsible = TRUE))),
      
      
      # Event figures -----------------------------------------------------------
      
      # tabItem(tabName = "event",
      #         # The event metric table
      #         fluidRow(box(dataTableOutput("eventTable"), width = 12, title = "Event metrics", 
      #                      status = "primary", solidHeader = TRUE, collapsible = TRUE)),
      #         
      #         fluidRow(
      #           # The Magnitude/RMSE plot
      #           box(width = 4, title = "Magnitude/RMSE plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("RMSE controls:"),
      #                 selectInput(inputId = "rmse_var", label = "Qx:",
      #                             choices = c("T_Qnet", "T_Qlw", "T_Qsw", "T_Qlh", "T_Qsh"),
      #                             selected = "T_Qnet"),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("rmsePlot")),
      #           # The correlation plot
      #           box(width = 4, title = "Correlation plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Correlation controls:"),
      #                 selectInput(inputId = "vars2", label = "Variables:",
      #                             # choices = list(
      #                             #   Flux = c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh"),
      #                             #   Air = c("Air temp", "Cloud cover (c)", "Precip-Evap (c)", "MSLP (c)"),
      #                             #   Sea = c("SST", "SSS", "MLD", "MLD_1", "SBT")
      #                             # ), 
      #                             choices = unique(ALL_cor$Parameter2), 
      #                             multiple = TRUE, 
      #                             selected = c("SST", "T_bottom", "SSS", "MLD_cum", "T_air", "MSLP_cum")),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("correlationPlot")),
      #           # The scatterplot
      #           box(width = 4, title = "Scatterplot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Scatter controls:"),
      #                 # These inputs need to be ordered by category
      #                 selectInput(inputId = "scat_x", label = "X axis:",
      #                             choices = unique(ALL_cor$Parameter2),
      #                             selected = "T_Qnet"),
      #                 selectInput(inputId = "scat_y", label = "Y axis:",
      #                             choices = unique(ALL_cor$Parameter2),
      #                             selected = "SST"),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("scatterPlot")),
      #         )),
      # Test box
      # fluidRow(box(verbatimTextOutput("devel")))),
      
      # Magnitude figures -------------------------------------------------------
      
      # tabItem(tabName = "magnitude",
      #         fluidRow(
      #           # Scatterplot box
      #           box(width = 6, title = "Scatterplot - proportion", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Scatterplot controls:"),
      #                 h5("No one here but us chickens."),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("scatterPlotMag")),
      #           # Boxplot box
      #           box(width = 6, title = "Boxplot - phase", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Boxplot controls:"),
      #                 radioButtons(inputId = "notch_mag", label = "Notches:",
      #                              choices = c(TRUE, FALSE),
      #                              selected = TRUE, inline = T),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("boxPlotMag")))),
      
      
      # RMSE figures ------------------------------------------------------------
      
      # tabItem(tabName = "rmse", 
      #         fluidRow(
      #           # Histogram box
      #           box(width = 6, title = "Boxplot - phase", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Boxplot controls:"),
      #                 radioButtons(inputId = "notch_ts", label = "Notches:", 
      #                              choices = c(TRUE, FALSE),
      #                              selected = TRUE, inline = T),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("boxPlotRMSEts")),
      #           # Boxplot box
      #           box(width = 6, title = "Boxplot - variable", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Boxplot controls:"),
      #                 radioButtons(inputId = "notch_var", label = "Notches:", 
      #                              choices = c(TRUE, FALSE),
      #                              selected = TRUE, inline = T),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("boxPlotRMSEvar"))),
      #         # Lineplot box
      #         fluidRow(box(plotOutput("linePlotRMSE"), width = 12, title = "Lineplot", status = "primary", 
      #                      solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))),

      
      # Correlations figures ----------------------------------------------------
      
      # tabItem(tabName = "correlations", 
      #         fluidRow(
      #           # Histogram box
      #           box(width = 6, title = "Histogram", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Histogram controls:"),
      #                 radioButtons(inputId = "position", label = "Position:", 
      #                              choices = c("stack", "dodge"),
      #                              selected = "stack", inline = T),
      #                 sliderInput(inputId = "bins", label = "Number of bins:",
      #                             min = 1, max = 20, value = 10),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("histPlotCor")),
      #           # Boxplot box
      #           box(width = 6, title = "Boxplot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
      #               dropdownButton(
      #                 h4("Boxplot controls:"),
      #                 radioButtons(inputId = "notch", label = "Notches:", 
      #                              choices = c(TRUE, FALSE),
      #                              selected = TRUE, inline = T),
      #                 circle = TRUE, status = "danger", icon = icon("gear")),
      #               plotOutput("boxPlotCor"))),
      #         # Lineplot box
      #         fluidRow(box(plotOutput("linePlotCor"), width = 12, title = "Lineplot", status = "primary", 
      #                      solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))),
      
      
      # App explanation ---------------------------------------------------------
      
      tabItem(tabName = "about", 
              fluidPage(
                column(12,
                       h2(tags$b("About")),
                       p("The purpose of this app is to provide interactive access to the results from the",
                         a(target = '_blank', rel = 'noopener noreferrer',
                           href = "https://robwschlegel.github.io/MHWflux/", "MHW Flux project"), ". There are many ways to visualise
                               the results and I have chosen three primary ways of doing so. These are outlined in the following sub-headers.
                               For more information about this project and the data used in it please follow the link in this paragraph.
                               The column on the left of the screen contains the names of the tabs that the user may click on to bring up
                               different dashboards. As the tabs are clicked on the other controls in this column will change accordingly.
                               The controls generated for each tab are explained in the specific headers below."),
                       h2(tags$b("Map")),
                       p("This tab shows the study area and the region polygons therein in the left panel. Underneath the colourful polygons
                               are labels that show the count of MHWs per season against the total count of MHWs in that season. This number will change
                               if the duration slider in the control column is moved. Note that this is a dual-ended slider and so one may chose to filter
                               out MHWs from both short and long durations simultaneously. One may also choose to de-select any of the regions or
                               seasons as desired. The panel to the right shows the MHWs detected in each region. The colours between the panels correspond
                               to the regions. The MHW panel is a plotly figure so one may hover over the lolliplots for more information."),
                       h2(tags$b("Event")),
                       p("This tab allows users to generate visualisations for individual events. One must first click on an event in the table
                               before the bottom three panels will show anything. Note that one may filter and search in the table to more quickly find
                               a MHW of interest. The filtering options in the control column also work on the table. Once an event has been clicked it is
                               recommended to minimise the table by clicking in the top right corner. Also note that the portion of the event may also be
                               selected. This means one may visualise the stats for just the onset or decline portion of the MHWs, in addition to the full event.
                               With an event selected one may now see in the left panel a correlation plot showing the relationship of SST with the other
                               Qx terms. The colour of the lines shows the strength of the correlations, and the thickness shows the significance.
                               With thicker lines having smaller p values. The variables being shown may be changed by clicking the red gear icon
                               and then clicking in the variables selection box. The centre panel shows a scatterplot of SST against Qnet. Layered on top of this
                               is a linear model that helps to demonstrate the strength of a correlation seen in the right panel. The red gear icon here allows
                               user to change the variables seen in the X and Y axes. The right panel shows the RMSE between SST and Qnet. The red gear icon allows
                               one to select a different Qx term. Note that the SST values shown here are the real SST, and not SST - seasonal
                               climatology, as seen throughout the rest of the project. The blue line shows the SST value from day 1 of the time series, with the
                               cumulative seasonal anomaly Qx term added to it for each time step."),
                       h2(tags$b("Magnitude")),
                       p("This tab contains two plots. The first is a scatterplot that shows the relationship between SSTa and T_Qnet. This allows the user to 
                         look at how the relationship between these two variables may change when filtering the data. The second is a boxplot that shows
                         the proportions of T_Qnet/SSTa. It may also be filtered and grouped to explore how this affects the results."),
                       h2(tags$b("RMSE")),
                       p("This tab contains the RMSE results. These are shown here as two different boxplot options. The first is faceted by the phase(s)
                         chosen from the results. The second boxplot shows the same as the first, but faceted by variable. Both of these boxplots respond to the
                         same filtering and grouping as all of the other plots in this shiny app. Between these two different faceting approaches one is able to
                         arrive at a deeper understanding of the results and how they realte to one anther."),
                       h2(tags$b("Correlations")),
                       p("The summary tab shows a higher level report on the correlations between variables and SST. The left panel shows the correlation results
                               via histograms. The X-axes in the histogram show the strength of the correlations between SST and the given variable. These will range
                               from -1 to 1. Each column of histograms belongs to one variable, as seen in the top header. Each row of histograms belongs to one
                               portion of the time series. By default these are the onset and decline portions of the events. This can be changed in the control column
                               to the left. Different variables may be chosen in the control column, and regions and seasons may be filtered out. Scrolling down, one
                               will see that events may be filtered by duration, as well as the p-value of the correlation. It is also possible to chose to group the
                               histograms by regions or seasons. The vertical red lines in the centre of the histograms serve as a visual aid/reminder that a correlation
                               of 0 means no relationship. This red line relates directly to the horizontal red line in the right panel, which is showing the same data as
                               the left panel but grouped into boxplots instead. The same groupings and filtering may be applied to these boxplots as to the histograms.
                               Lastly, there is a third panel on the bottom that is minimised by default. This is a series of scatterplots with linear models fitted to them
                               that show the relationship between the duration of a MHW (X-axis) and the correlation strength (Y-axis) of variables. It basically shows
                               that there isn't much of a pattern. That longer MHWs tend to be more unique in their drivers so any relationship with the duration of an
                               event and physical drivers tends to break down after 30 days or so."),
                )
              )
      )
    )
  )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

  # Upload file display table -----------------------------------------------
  
  output$contents_load <- renderTable({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    
    # test <- read.csv("shiny/kongCTD/data/August Bailey_0408_1141.txt", skip = 3, sep = ";", dec = ",", fileEncoding = "latin1")
    
    df <- read.csv(input$file1$datapath,
                   skip = input$skip,
                   header = input$header,
                   sep = input$sep,
                   dec = input$dec,
                   quote = input$quote,
                   fileEncoding = input$encoding)
    
    if(input$disp == "head") {
      return(head(df, 20))
    }
    else {
      return(tail(df, 20))
    }
    
  })
  

  # Tidy file display table -------------------------------------------------
  
  output$contents_tidy <- renderTable({
    
    # file1 is NULL on startup, which will cause an error
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   skip = input$skip,
                   header = input$header,
                   sep = input$sep,
                   dec = input$dec,
                   quote = input$quote,
                   fileEncoding = input$encoding)
    
    # Create date_time column
    
    # Coerce to POSIXCt
    
    # Set TZ
    
    if(input$disp == "head") {
      return(head(df, 20))
    }
    else {
      return(tail(df, 20))
    }
    
  })
  
  # Render UI ---------------------------------------------------------------
  
  # Select variables from a dropdown
  # picker_vars <- pickerInput(inputId = "vars", label = "Variables:",
  #                            choices = list(
  #                              Flux = c("T_Qnet", "T_Qlw", "T_Qsw", "T_Qlh", "T_Qsh"),
  #                              Air = c("T_air", "TCC_cum", "P-E_cum", "MSLP_cum", "Wind_cum"),
  #                              Sea = c("SST", "SSS", "MLD_cum", "MLD_1_cum", "T_bottom")
  #                            ),
  #                            multiple = TRUE,
  #                            options = list(size = 6),
  #                            selected = c("T_air", "MLD_cum"))
  # Select variables from a dropdown for RMSE only
  # picker_vars_rmse <- pickerInput(inputId = "vars_rmse", label = "Variables:",
  #                                 choices = c("T_Qnet", "T_Qlw", "T_Qsw", "T_Qlh", "T_Qsh"), multiple = TRUE,
  #                                 options = list(`actions-box` = TRUE, size = 6),
  #                                 selected = c("T_Qlw", "T_Qsw", "T_Qlh", "T_Qsh"))
  # picker_vars_rmse <- selectInput(inputId = "vars_rmse", label = "Variables:",
  #                                 multiple = TRUE,
  #                                 choices = c("Qnet_budget", "Qlw_budget", "Qsw_budget", "Qlh_budget", "Qsh_budget"),
  #                                 selected = c("Qnet_budget", "Qlw_budget", "Qsw_budget", "Qlh_budget", "Qsh_budget"))
  
  # Select regions from a dropdown
  # picker_regions <- pickerInput(inputId = "regions", label = "Regions:",
  #                               choices = levels(MHW_event$region), multiple = TRUE,
  #                               options = list(`actions-box` = TRUE, size = 6),
  #                               selected = levels(MHW_event$region))
  
  # Select seasons from a dropdown
  # picker_seasons <- pickerInput(inputId = "seasons", label = "Seasons:",
  #                               choices = levels(ALL_cor$season), multiple = TRUE,
  #                               options = list(`actions-box` = TRUE, size = 4),
  #                               selected = levels(ALL_cor$season))
  
  # Select parts of a time series
  # picker_ts_multiple <- pickerInput(inputId = "ts_multiple", label = "MHW phases:",
  #                                   choices = levels(ALL_cor$ts), multiple = TRUE,
  #                                   options = list(`actions-box` = TRUE, size = 3),
  #                                   selected = c("onset", "decline"))
  # picker_ts_single <- pickerInput(inputId = "ts_single", label = "MHW phase:",
  #                                 choices = levels(ALL_cor$ts), multiple = FALSE,
  #                                 selected = "onset")
  
  # Radio buttons to choose fill of histograms and boxplots
  # radio_fill <- prettyRadioButtons(inputId = "fill", label = "Grouping:", 
  #                                  choices = c("region", "season", "none"),
  #                                  selected = "none", inline = T,
  #                                  status = "primary", fill = TRUE)
  
  # Filter events by their duration
  # slider_duration_min <- sliderInput(inputId = "duration_min", label = "Min Duraion:",
  #                                    min = 1, max = max(ALL_cor$n_Obs),
  #                                    value = c(1, max(ALL_cor$n_Obs)))
  # slider_duration <- sliderInput(inputId = "duration", label = "Duration:",
  #                                min = 1, max = max(MHW_event$duration),
  #                                value = c(1, max(MHW_event$duration)))
  
  # Filter correlations by p-value
  # slider_p_val <- sliderInput(inputId = "p_val", label = "Max p:",
  #                             min = 0, max = 1, value = 1)
  
  # The chosen controls per tab
  output$sidebar_controls <- renderUI({
    if(input$mainMenu == "correlations"){
      sidebarMenu(picker_vars, picker_regions, picker_seasons, picker_ts_multiple, 
                  radio_fill, slider_duration, slider_p_val)
    } else if(input$mainMenu == "rmse"){
      sidebarMenu(picker_vars_rmse, picker_regions, picker_seasons,
                  picker_ts_multiple, radio_fill, slider_duration)
    } else if(input$mainMenu == "event"){
      sidebarMenu(picker_regions, picker_seasons, picker_ts_single, slider_duration)
    } else if(input$mainMenu == "map"){
      sidebarMenu(picker_regions, picker_seasons, slider_duration)
    } else if(input$mainMenu == "magnitude"){
      sidebarMenu(picker_regions, picker_seasons, radio_fill,
                  picker_ts_multiple, slider_duration)
    } else {
      # Intentionally empty
    }
  })
  
  
  # Filter data -------------------------------------------------------------
  
  # The correlations results
  cor_data <- reactive({
    req(input$vars); req(input$p_val)
    ALL_cor_sub <- ALL_cor %>% 
      filter(Parameter1 == "SST",
             Parameter2 %in% input$vars,
             region %in% input$regions,
             ts %in% input$ts_multiple,
             season %in% input$seasons,
             p <= input$p_val,
             n_Obs >= input$duration[1],
             n_Obs <= input$duration[2])
    return(ALL_cor_sub)
  })
  
  # The magnitude results
  mag_data <- reactive({
    req(input$ts_multiple)#; req(input$p_val)
    ALL_mag_sub <- ALL_mag %>% 
      filter(Parameter1 == "SST",
             Parameter2 %in% c("SST", "T_Qnet"),
             region %in% input$regions,
             ts %in% input$ts_multiple,
             season %in% input$seasons,
             # p <= input$p_val,
             n_Obs >= input$duration[1],
             n_Obs <= input$duration[2]) %>% 
      dplyr::select(region:ts, Parameter2, mag) %>% 
      pivot_wider(names_from = Parameter2, values_from = mag) %>% 
      mutate(prop = T_Qnet/SST)
    return(ALL_mag_sub)
  })
  
  # The RMSE results
  rmse_data <- reactive({
    req(input$vars_rmse)#; req(input$p_val)
    ALL_RMSE_sub <- ALL_RMSE %>% 
      filter(Parameter1 == "SST",
             Parameter2 %in% input$vars_rmse,
             region %in% input$regions,
             ts %in% input$ts_multiple,
             season %in% input$seasons,
             # p <= input$p_val,
             n_Obs >= input$duration[1],
             n_Obs <= input$duration[2])
    return(ALL_RMSE_sub)
  })
  
  # The OISST MHW metrics
  MHW_data <- reactive({
    req(input$regions)
    MHW_event_sub <- MHW_event %>% 
      dplyr::rename(i_mean = intensity_mean, i_max = intensity_max, i_cum = intensity_cumulative,
                    start = date_start, peak = date_peak, end = date_end, event = event_no) %>% 
      dplyr::select(region, season, event, start, peak, end, duration,
                    i_mean, i_max, i_cum, rate_onset, rate_decline) %>% 
      filter(region %in% input$regions,
             season %in% input$seasons,
             duration >= input$duration[1],
             duration <= input$duration[2]) %>% 
      mutate_if(is.numeric, round, 2)
    return(MHW_event_sub)
  })
  
  # Data for the selected MHW
  MHW_single <- reactive({
    req(length(input$eventTable_cell_clicked) > 0)
    
    # Find selected event
    MHW_data <- MHW_data()
    event_sub <- MHW_data[input$eventTable_cell_clicked$row,]
    
    # Subset data accordingly
    ts_wide <- ALL_ts_anom_full_wide %>%
      filter(t >= event_sub$start,
             t <= event_sub$end,
             region == event_sub$region,
             ts == input$ts_single)
    return(ts_wide)
  })
  
  
  # Map figures -------------------------------------------------------------
  
  # The map
  output$mapRegions <- renderPlot({
    
    MHW_data <- MHW_data()
    
    # Add other MHW metric filters
    
    # Proportion of MHWs in each region in each node
    region_info <- MHW_data %>%
      left_join(region_count, by = "region") %>% 
      group_by(region) %>%
      mutate(prop = round(n()/count, 2)) %>%
      select(region, count, prop) %>%
      unique() %>%
      ungroup()
    
    # Create labels for number of MHWs per region
    region_prop_label <- NWA_coords %>%
      left_join(region_info, by = "region") %>%
      group_by(region) %>%
      mutate(lon_center = mean(lon), lat_center = mean(lat)) %>%
      na.omit() %>% 
      mutate(lon_center = case_when(region == "GSL" ~ lon_center+2,
                                    region == "SS" ~ lon_center+1,
                                    region == "GM" ~ lon_center-1,
                                    region == "MAB" ~ lon_center+1.8,
                                    TRUE ~ lon_center),
             lat_center = case_when(region == "GM" ~ lat_center-1.5,
                                    region == "MAB" ~ lat_center+0.8,
                                    TRUE ~ lat_center)) %>%
      ungroup()
    
    # Count of seasons
    season_info <- MHW_data %>%
      left_join(season_count, by = "season") %>% 
      group_by(season) %>%
      mutate(prop = round(n()/count, 2)) %>%
      select(season, count, prop) %>%
      unique() %>%
      ungroup()
    
    # The map
    mr <- frame_base +
      geom_polygon(data = region_prop_label, size = 2, alpha = 0.7,
                   aes(fill = region, colour = region, #alpha = prop,
                       text = paste0("Region: ",region)), show.legend = F) +
      geom_polygon(data = map_base, aes(group = group), show.legend = F) +
      # Count per region
      geom_label(data = region_prop_label,
                 aes(x = lon_center, y = lat_center, 
                     label = paste0(round(prop*count),"/",count))) +
      # Count per seasons
      geom_label(data = filter(season_info, season == "Spring"), 
                 aes(x = -55, y = 39, label = paste0(season,": ",round(prop*count),"/",count))) +
      geom_label(data = filter(season_info, season == "Summer"), 
                 aes(x = -55, y = 38, label = paste0(season,": ",round(prop*count),"/",count))) +
      geom_label(data = filter(season_info, season == "Autumn"), 
                 aes(x = -55, y = 37, label = paste0(season,": ",round(prop*count),"/",count))) +
      geom_label(data = filter(season_info, season == "Winter"), 
                 aes(x = -55, y = 36, label = paste0(season,": ",round(prop*count),"/",count)))
    mr
    # ggplotly(mr,  tooltip = "text") %>% 
    #     layout(showlegend = FALSE)
  })
  
  # The lolliplot
  output$eventLolli <- renderPlotly({
    
    MHW_data <- MHW_data()
    
    # Lolliplot
    el <- ggplot(data = MHW_data, aes(x = peak, y = i_max)) +
      geom_segment(aes(xend = peak, yend = 0)) +
      geom_point(shape = 21, size = 2, show.legend = F,
                 aes(fill = region,
                     text = paste0("Event: ",event,
                                   "<br>Duration: ",duration," days",
                                   "<br>Start Date: ", start,
                                   "<br>Peak Date: ", peak,
                                   "<br>End Date: ", end,
                                   "<br>Mean Intensity: ",i_mean,"°C",
                                   "<br>Max. Intensity: ",i_max,"°C",
                                   "<br>Cum. Intensity: ",i_cum,"°C"))) +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y", 
                   limits = c(min(MHW_data$start-61), max(MHW_data$end+61))) +
      scale_y_continuous(expand = c(0,0), limits = c(0, max(MHW_data$i_max)*1.1)) +
      labs(x = "", y = "Max. Intensity (°C)") +
      facet_wrap(~region, ncol = 1)
    ggplotly(el, tooltip = "text", dynamicTicks = F) %>% 
      layout(showlegend = FALSE)
  })
  
  
  # Event figures -----------------------------------------------------------
  
  # Table showing filtered events from sidebar controls
  # Add a title explicitly stating what this shows
  output$eventTable = renderDataTable({
    MHW_data() %>% 
      left_join(ALL_cor_wide, by = c("region", "season", "event"))
  }, selection = 'single', server = TRUE, options = list(scrollX = TRUE))
  
  # Correlation plot for single figure
  output$correlationPlot <- renderPlot({
    req(input$vars2)
    MHW_single() %>%
      dplyr::select(-event_no, -ts) %>% 
      pivot_longer(cols = c(-region, -t)) %>% 
      filter(name %in% input$vars2) %>%
      pivot_wider(names_from = name, values_from = value) %>% 
      correlation() %>%
      plot()
  })
  
  # Scatterplot of two variable during onset, full, or decline of a single event
  output$scatterPlot <- renderPlot({
    MHW_single <- MHW_single()
    sp <- ggplot(data = MHW_single, aes_string(x = input$scat_x, y = input$scat_y)) +
      geom_smooth(method = "lm", formula = 'y ~ x') +
      geom_point(aes(colour = t)) +
      scale_colour_date(low = "red", high = "blue") +
      labs(colour = "Date")
    sp
    # ggplotly(sp, tooltip = "text", dynamicTicks = F) %>% 
    #   layout(showlegend = FALSE)
  })
  
  # RMSE scatterplot of SST and a Qx variable during onset, full, or decline of a single event
  output$rmsePlot <- renderPlot({
    # Prep ts data
    MHW_single <- MHW_single()
    
    # Find RMSE value
    MHW_data <- MHW_data()
    event_sub <- MHW_data[input$eventTable_cell_clicked$row,]
    MHW_rmse <- ALL_cor %>% 
      filter(region == event_sub$region,
             event_no == event_sub$event,
             ts == input$ts_single,
             Parameter2 == input$rmse_var) %>% 
      na.omit()
    
    # Find magnitude values
    MHW_mag <- ALL_cor %>% 
      filter(region == event_sub$region,
             event_no == event_sub$event,
             ts == input$ts_single,
             Parameter2 %in% c(input$rmse_var, "SST")) %>%
      filter(!is.na(mag)) %>% 
      mutate(mag = round(mag, 2)) %>% 
      arrange(Parameter2)
    
    # Label coordinates for Qx term
    Qx_x <- MHW_single$t[nrow(MHW_single)]
    Qx_y <- MHW_single[[nrow(MHW_single), input$rmse_var]]
    
    # The plot
    rp <- ggplot(data = MHW_single, aes(x = t)) +
      geom_point(aes(y = SST), colour = "red") +
      geom_line(aes(y = SST), colour = "red") +
      geom_point(aes_string(y = input$rmse_var), colour = "blue") +
      geom_line(aes_string(y = input$rmse_var), colour = "blue") +
      geom_label(aes(x = t[nrow(MHW_single)], y = SST[nrow(MHW_single)],
                     label = paste0("SST\n",MHW_mag$mag[1])), colour = "red") +
      geom_label(aes(x = Qx_x, y = Qx_y,
                     label = paste0(input$rmse_var,"\n",MHW_mag$mag[2])), colour = "blue") +
      geom_label(aes(x = mean(t), y = quantile(SST, 0.2),
                     label = paste0("Mag. Prop. = ",round(MHW_mag$mag[2]/MHW_mag$mag[1], 2)))) +
      geom_label(aes(x = mean(t), y = quantile(SST, 0.1),
                     label = paste0("RMSE = ",MHW_rmse$rmse[1]))) +
      labs(x = NULL, y = "SSTa (°C) || SSTa[1] + Qx/MLD[cum] (°C)")
    rp
  })
  
  # Test text output for table interaction
  # output$devel <- renderPrint({
  #     req(length(input$eventTable_cell_clicked) > 0)
  #     # input$eventTable_cell_clicked
  #     MHW_single()
  # })
  
  
  # Magnitude figures -------------------------------------------------------
  
  # Scatterplot of T_Qnet vs. SSTa
  output$scatterPlotMag <- renderPlot({
    req(input$ts_multiple)#; req(input$p_val)
    
    mag_data <- mag_data()
    
    sp <- mag_data %>% 
      filter(prop >= -2, prop <= 2) %>% 
      ggplot(aes(x = SST, y = T_Qnet)) +
      # geom_smooth(method = "lm", formula = 'y ~ x') +
      geom_point(aes(colour = prop)) +
      scale_colour_gradient2(low = "blue", high = "yellow") +
      labs(colour = "T_Qnet/SST")
    sp
    # ggplotly(sp, tooltip = "text", dynamicTicks = F) %>% 
    #   layout(showlegend = FALSE)
  })
  
  # Boxplot faceted by ts
  output$boxPlotMag <- renderPlot({
    req(input$ts_multiple)#; req(input$p_val)
    
    mag_data <- mag_data()
    
    if(input$fill != "none"){
      ggplot(data = mag_data, aes(x = ts, y = prop)) +
        # geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(aes_string(fill = input$fill), notch = input$notch_mag) +
        coord_cartesian(ylim = c(-2, 2)) +
        labs(x = "Phase",
             y = "𝚫T<sub>Qnet</sub> / 𝚫SSTa") +
        theme(axis.title.y = ggtext::element_markdown())
    } else {
      ggplot(data = mag_data, aes(x = ts, y = prop)) +
        # geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(notch = input$notch_mag) +
        coord_cartesian(ylim = c(-2, 2)) +
        labs(x = "Phase", 
             y = "𝚫T<sub>Qnet</sub> / 𝚫SSTa") +
        theme(axis.title.y = ggtext::element_markdown())
    }
  })
  
  # RMSE figures ------------------------------------------------------------
  
  # Boxplot faceted by ts
  output$boxPlotRMSEts <- renderPlot({
    req(input$vars_rmse); req(input$ts_multiple)#; req(input$p_val)
    
    rmse_data <- rmse_data()
    
    if(input$fill != "none"){
      ggplot(data = rmse_data, aes(x = Parameter2, y = rmse)) +
        # geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(aes_string(fill = input$fill), notch = input$notch_ts) +
        facet_wrap(~ts) +
        labs(x = NULL)
    } else {
      ggplot(data = rmse_data, aes(x = Parameter2, y = rmse)) +
        # geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(notch = input$notch_ts) +
        facet_wrap(~ts) +
        labs(x = NULL)
    }
  })
  
  # Boxplot faceted by variables
  output$boxPlotRMSEvar <- renderPlot({
    req(input$vars_rmse); req(input$ts_multiple)#; req(input$p_val)
    
    rmse_data <- rmse_data()
    
    if(input$fill != "none"){
      ggplot(data = rmse_data, aes(x = ts, y = rmse)) +
        # geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(aes_string(fill = input$fill), notch = input$notch_var) +
        facet_wrap(~Parameter2) +
        labs(x = NULL)
    } else {
      ggplot(data = rmse_data, aes(x = ts, y = rmse)) +
        # geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(notch = input$notch_var) +
        facet_wrap(~Parameter2) +
        labs(x = NULL)
    }
  })
  
  # Lineplot
  output$linePlotRMSE <- renderPlot({
    req(input$vars_rmse); req(input$ts_multiple)#; req(input$p_val)
    if(input$fill != "none"){
      ggplot(rmse_data(), aes(x = n_Obs, y = rmse)) +
        geom_point(aes_string(colour = input$fill)) +
        geom_smooth(aes_string(colour = input$fill, linetype = "ts"), method = "lm", se = F) +
        facet_wrap(~Parameter2)
    } else {
      ggplot(rmse_data(), aes(x = n_Obs, y = rmse)) +
        geom_point() +
        geom_smooth(aes(linetype = ts), method = "lm", se = F) +
        facet_wrap(~Parameter2)
    }
  })
  
  
  # Correlation figures -----------------------------------------------------
  
  # Histogram
  output$histPlotCor <- renderPlot({
    req(input$vars); req(input$p_val); req(input$ts_multiple)
    if(input$fill != "none"){
      ggplot(cor_data(), aes(x = r)) +
        geom_vline(aes(xintercept = 0), colour = "red", size = 1) +
        geom_histogram(aes_string(fill = input$fill), bins = input$bins, position = input$position) +
        facet_grid(ts ~ Parameter2)
    } else {
      ggplot(cor_data(), aes(x = r)) +
        geom_vline(aes(xintercept = 0), colour = "red", size = 1) +
        geom_histogram(bins = input$bins, position = input$position) +
        facet_grid(ts ~ Parameter2)
    }
  })
  
  # Boxplot
  output$boxPlotCor <- renderPlot({
    req(input$vars); req(input$p_val); req(input$ts_multiple)
    
    cor_data <- cor_data()
    
    if(input$fill != "none"){
      ggplot(data = cor_data, aes(x = ts, y = r)) +
        geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(aes_string(fill = input$fill), notch = input$notch) +
        facet_wrap(~Parameter2)
    } else {
      ggplot(data = cor_data, aes(x = ts, y = r)) +
        geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
        geom_boxplot(notch = input$notch) +
        facet_wrap(~Parameter2)
    }
  })
  
  # Lineplot
  output$linePlotCor <- renderPlot({
    req(input$vars); req(input$p_val); req(input$ts_multiple)
    if(input$fill != "none"){
      ggplot(cor_data(), aes(x = n_Obs, y = r)) +
        geom_point(aes_string(colour = input$fill)) +
        geom_smooth(aes_string(colour = input$fill, linetype = "ts"), method = "lm", se = F) +
        facet_wrap(~Parameter2)
    } else {
      ggplot(cor_data(), aes(x = n_Obs, y = r)) +
        geom_point() +
        geom_smooth(aes(linetype = ts), method = "lm", se = F) +
        facet_wrap(~Parameter2)
    }
  })
  
  # This automatically ends the session when the app is closed
  session$onSessionEnded(stopApp)

  session$onSessionEnded(stopApp)
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)


# The UI ------------------------------------------------------------------

# ui <- fluidPage(
  
  # Application title
  # titlePanel("Kongsfjorden CTD Data"),
  

  # The side bar filters ----------------------------------------------------
  
  # sidebarLayout(
  #   sidebarPanel(
      
      # Change to a dashboard layout
      # First board has a UI for uploading a file and checking that it looks correct:
      # https://shiny.rstudio.com/articles/upload.html
      # https://shiny.rstudio.com/reference/shiny/latest/fileInput.html
      # Once the user is certain that the data look correct, they then upload them to the database:
      # https://shiny.rstudio.com/articles/persistent-data-storage.html
      # There are many ways of handling this and we will need to discuss them
      # Once the data are uploaded the user can then select a visualisation board
      # At the moment I'm thinking of having a time series board and a map board
      # The map board would show the data in space and provide some interpolating
      # The time series board would show the data in time, also with some interpolating
      # Both boards would share similar filtering and interpolating options
      # So perhaps it would be better to have one board, but with a time series or map option
      # There should also be an analysis board
      # MHWs would be fun
      
      # Eventually it would be good to allow the user to choose the type
      # of instrument(s) from which to create the data products.
      # This is not done yet due to the paucity of the raw data.
      # selectInput(inputId = "Type",
      #             label = "Choose instrument type(s)", 
      #             choices = type, multiple = T),
      # radioButtons(inputId = "Base",
      #              label = "Choose product",
      #              choices = list("Clean", "Monthly", "Yearly", "Climatology"),
      #              inline = TRUE,
      #              selected = "Clean"),
      # uiOutput("Year"),
      # uiOutput("Month"),
      # radioButtons(inputId = "Measure",
      #             label = "Measure from",
      #             choices = list("Top", "Bottom"),
      #             inline = TRUE,
      #             selected = "Top"),
      # uiOutput("Depth"),
      # selectInput(inputId = "Variable",
      #             label = "Choose one variable",
      #             choices = list("temp", "salinity", "oxygen"),
      #             multiple = F),
      # actionButton("Filter", "Filter Data")),
    
    
    # The main panel for figure plotting --------------------------------------
    
#     mainPanel(
#       tabsetPanel(
#         tabPanel("About",
#                  p(),
#                  p("Hello and welcome to the CTD project."),
#                  br(),
#                  p("The purpose of this web app is to serve as a proof of concept that the CTD
#                    and other similar 3D data collected within the oceans bordering South Africa 
#                    may be hosted online in a location that is freely accessible to anyone that
#                    would like to use the data for research purposes."),
#                  p("The 'Information' tab above contains several sub-tabs that will explain in 
#                    detail how this web app functions."),
#                  p("The 'Map' tab above is where the data visualisation occurs."),
#                  br(),
#                  p("Please send any input you may have to Robert Schlegel at:"),
#                  p("robwschlegel@gmail.com")),
#         navbarMenu(title = "Information",
#                    tabPanel("Data Selection",
#                             p(),
#                             p("Below please find an itemised explanation for how to use the filters found 
#                               in the menu column on the left-hand side of the screen."),
#                             p(strong("Choose product")),
#                             p("In order to make a sub-setted selection of the data available please
#                               first choose which base product you would like to work from by clicking
#                               on the appropriate radio button. The descriptions of the base products 
#                               available are as follows:"),
#                             column(1), p(strong("Clean"), "- This product is only a cleaned up version of the 
#                                          raw data. The data were first filtered for any obvious outliers
#                                          before being binned into lon/lat values within 0.1 degrees of
#                                          one another, depths of 10 metre intervals, and the dates were
#                                          rounded from days to months. Any data points found to be 
#                                          overlapping one another were meaned. All of the temperature,
#                                          salinity, and oxygen values were then rounded to the second
#                                          decimal values. All data deeper than 1000 metres were excluded."),
#                             column(1), p(strong("Monthly"), "- This product takes the above product and
#                                          performs a linear interpolation without otherwise binning any of 
#                                          the data further. This product is most useful if a researcher is 
#                                          interested in using these CTD data as a monthly time series but
#                                          would like to fill in the large gaps as much as possible."),
#                             column(1), p(strong("Yearly"), "- This product is the same as above but the 
#                                          data are first binned into yearly groups before the linear 
#                                          interpolation is performed. This provides a much more complete 
#                                          product spatially, but looses out on temporal precision. This 
#                                          product is of best use to a researcher that is interested in
#                                          long-term (annual) trends or would like more spatial coverage than
#                                          the monthly product provides."),
#                             column(1), p(strong("Climatology"), "- This product bins all of the data into the
#                                          months they were sampled, irrespective of the year, before then
#                                          performing the linear interpolation. This product provides the best
#                                          spatial coverage, but the worst temporal coverage. This product is best
#                                          used for a research that would like to know, on average, what the abiotic
#                                          values along the coast of South Africa are during a given month of the 
#                                          year."),
#                             p(strong("Choose year(s)/month(s)")),
#                             p("After chosing the data product the next step is to decide how to filter it based
#                               on the date values available. For the", strong("Clean"), "and", strong("Monthly"), 
#                               "products one may choose to filter by both year and month values. Select as many of
#                               either value as is desirable by clicking on the year and/or month value in the 
#                               drop-down list. Please note that it will only be possible to choose year values for
#                               the", strong("Yearly"), "product and only month values for the", 
#                               strong("Climatology"), "product. Please note that if multiple years/months are
#                               selected the visualisation panel will show all of the overlapping values meaned
#                               together."),
#                             p(strong("Measure from")),
#                             p("Selected the radio button 'Top' if one would like to filter the data product based
#                               on the distance from the sea surface. Select 'Bottom' to rather filter data based on 
#                               their distance from the sea floor."),
#                             p(strong("Choose depth(s)")),
#                             p("Once the direction of measurement has been selected, one may then choose which depth 
#                               layer(S) (rounded to the nearest 10 metres) to filter. Please note that if multiple
#                               depths are selected the visualisation will show them meaned together."),
#                             p(strong("Choose one variable")),
#                             p("With all of the filters set, the last step is to select which abiotic variable one 
#                               is interested in. The choices are 'temp' (temperature in °C), 'salinity' 
#                               (parts per thousand; PPT), and 'oxygen' (miligrams per litre; mg/L)."),
#                             p(strong("Filter Data")),
#                             p("Click this button to apply the chosen filters. Please note that the visualisations
#                               are not generated until this button is clicked.")),
#                    tabPanel("Downloading Data",
#                             p(),
#                             p("In order to download data, plesae first make the selection as outlined in 
#                               the 'Data Selection' sub-tab and then click on one of the download buttons in 
#                               the bottom left corner of this app. The 'Download All' button will download all
#                               of the filtered data whereas the 'Download Mean' button will bin together and
#                               average all of the data if multiple different depth values were selected.")),
#                    tabPanel("Meta-data definitions",
#                             p(),
#                             p("Presently the meta-data that exists 'behind' the data shown here are not
#                               available with the products hosted here as they are an amalgamation of all
#                               of the data presently available. If more raw data are to be made available
#                               it may then be possible to create products that show the the accompanying 
#                               meta-data upon download of the filtered product.")),
#                    tabPanel("Updates",
#                             p(),
#                             p("2018-06-14: The project goes live."),
#                             p("2018-06-14: Added land mask, changed colour palettes for 
#                               different variables, and added some explanatory text."),
#                             p("2018-06-22: Fixes to linearly interpolated monthly climatologies."),
#                             p("2018-07-05: Bottom depth filter added as well as multiple products."),
#                             p("2018-07-06: Filter button added and help text updated.")),
#                   tabPanel("Acknowledgments",
#                            p(),
#                            p("The data found within this web app are an amalgamation of data that were
#                              collected during DAFF and DEA research/survey cruises. The data were mainly
#                              collected with CTDs though there are some BONGO and CALVET data blended in
#                              for the upper mixed layer. The majority of the raw data from which the data
#                              products in this app were created may be requested from SADCO. The links for
#                              the sources referenced here may be found below:"),
#                            column(1), p("DAFF - ", a(href = "www.daff.gov.za", "Department of Agriculture Forestry and Fisheries")),
#                            column(1), p("DEA - ", a(href = "www.environment.gov.za", "Department of Environmental Affairs")),
#                            column(1), p("SADCO - ", a(href = "www.sadco.csir.co.za", "Southern African Data Centre for Oceanography")))),
#         tabPanel("Map", plotOutput("map1")))
#     )
#   ),
#   downloadButton("save_data_all", "Download All"),
#   downloadButton("save_data_mean", "Download Mean")
# )


# The server --------------------------------------------------------------

# server <- function(input, output) {


  # Choosing the base product -----------------------------------------------
  
  # base <- reactive({
  #   if(input$Base == "Clean"){
  #     base <- CTD_pre_interp
  #   } else if(input$Base == "Climatology"){
  #     base <- CTD_interp_clim
  #   } else if(input$Base == "Yearly"){
  #     base <- CTD_interp_yearly
  #   } else if(input$Base == "Monthly"){
  #     base <- CTD_interp_monthly
  #   }
  # })
  

  # Choosing year and month filters -----------------------------------------

  # output$Year <-  renderUI({
  #   base <- base()
  #   if(input$Base %in% c("Clean", "Monthly", "Yearly")){
  #     selectInput('year2', 'Choose years(s)', 
  #                 choices = unique(base$year),
  #                 multiple = T)
  #   } else if(input$Base == "Climatology") {
  #   }
  # })
  # 
  # output$Month <-  renderUI({
  #   base <- base()
  #   if(input$Base %in% c("Clean", "Monthly", "Climatology")){
  #     selectInput('month2', 'Choose month(s)', 
  #                 choices = unique(base$month)[order(unique(base$month))],
  #                 multiple = T)
  #   } else if(input$Base == "Yearly") {
  #   }
  # })

  
  # Choosing depths filters -------------------------------------------------

  # output$Depth <- renderUI({
  #   base <- base()
  #   if(input$Measure == "Top"){
  #     selectInput('depth2', 'Choose depth(s)', choices = unique(base$depth),
  #                 multiple = T)
  #   } else if(input$Measure == "Bottom") {
  #     selectInput('depth2', 'Choose depth(s)',
  #                 choices = unique(base$depth_to)[order(unique(base$depth_to))],
  #                 multiple = T)
  #   }
  # })
  

  # Filtering the final data product ----------------------------------------

  # CTD <- eventReactive(input$Filter, {
  #     CTD <- base()
  #     if(input$Base %in% c("Clean", "Monthly")){
  #       CTD <- CTD[CTD$year %in% input$year2, ]
  #       CTD <- CTD[CTD$month %in% input$month2, ]
  #       CTD$date <- paste0(CTD$year, "-", CTD$month)
  #       CTD$month <- NULL
  #       CTD$year <- NULL
  #     } else if(input$Base == "Yearly") {
  #       CTD <- CTD[CTD$year %in% input$year2, ]
  #       CTD$date <- CTD$year
  #       CTD$year <- NULL
  #     } else if(input$Base == "Climatology") {
  #       CTD <- CTD[CTD$month %in% input$month2, ]
  #       CTD$date <- CTD$month
  #       CTD$month <- NULL
  #     }
  #     CTD <- as.data.frame(CTD)
  #     CTD$z <- as.vector(CTD[,colnames(CTD) == input$Variable])
  #     CTD$var <- rep(as.character(input$Variable[1]), nrow(CTD))
  #     CTD <- CTD[complete.cases(CTD$z),]
  #     CTD <- CTD %>% 
  #       select(-temp, -salinity, -oxygen)
  #     if(input$Measure == "Top") {
  #       CTD <- CTD[CTD$depth %in% input$depth2, ]
  #     } else if(input$Measure == "Bottom"){
  #       CTD <- CTD[CTD$depth_to %in% input$depth2, ]
  #     }
  #     })


  # The map -----------------------------------------------------------------

  # output$map1 <- renderPlot({
    
    # To control the colour scale to be static based on the full range 
    # of values found within the base product requires a bit of wizardry
    # I've chosen for now not to do this, and to rather allow the colour 
    # scale to change based on the filtered value range.
    # The first step towards changing this is commented out below,
    # but quite a bit more work is necessary tofinish this issue.
    # base <- base()
    # 
    # col_range <- c(min(base[,colnames(base) == input$Variable], na.rm = T),
    #                max(base[,colnames(base) == input$Variable], na.rm = T))
    
  #   CTD <- CTD()
  #   if(CTD$var[1] == "temp") viri_col <- "A"
  #   if(CTD$var[1] == "salinity") viri_col <- "D"
  #   if(CTD$var[1] == "oxygen") viri_col <- "E"
  #   
  #   CTD2 <- CTD %>% 
  #     group_by(lon, lat) %>% 
  #     summarise(z = mean(z, na.rm = T)) %>% 
  #     ungroup()
  # 
  #   # The map
  #   sa_map +
  #     geom_raster(data = CTD2, aes(x = lon, y = lat, fill = z), stat = "identity") +
  #     scale_fill_viridis_c(CTD$var, option = viri_col)
  # })
  

  # The saving --------------------------------------------------------------

  # output$save_data_all <- downloadHandler(
  #   filename = "CTD_all.csv",
  #   content = function(file) {
  #     CTD <- CTD()
  #     CTD <- CTD %>% 
  #       select(-var) %>% 
  #       select(date, everything())
  #     colnames(CTD)[colnames(CTD) == "z"] <- input$Variable
  #     write.csv(CTD, file, row.names = F)
  #   }
  # )
  
#   output$save_data_mean <- downloadHandler(
#     filename = "CTD_mean.csv",
#     content = function(file) {
#       CTD <- CTD()
#       CTD <- CTD %>%
#         group_by(date, lon, lat, depth_bot)
#       if(input$Measure == "Top") {
#         CTD <- CTD %>% 
#           mutate(depth_range = paste0(min(depth, na.rm = T),
#                                       "-", max(depth, na.rm = T))) %>% 
#           group_by(date, lon, lat, depth_range, depth_bot) %>%
#           summarise(z = round(mean(z, na.rm = T), 2))
#       } else if(input$Measure == "Bottom"){
#         CTD <- CTD %>% 
#           mutate(depth_to_range = paste0(min(depth_to, na.rm = T),
#                                       "-", max(depth_to, na.rm = T))) %>% 
#           group_by(date, lon, lat, depth_to_range, depth_bot) %>%
#           summarise(z = round(mean(z, na.rm = T), 2))
#       }
#       colnames(CTD)[colnames(CTD) == "z"] <- input$Variable
#       write.csv(CTD, file, row.names = F)
#     }
#   )
#   
# }


# Run the application  ----------------------------------------------------

# shinyApp(ui = ui, server = server)

