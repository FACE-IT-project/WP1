# shiny/demoMHW/functions.R
# Script to hold functions and other functionality for demoMHW app

# Functions ---------------------------------------------------------------

# Function for ensuring 366 DOY for non-leap years
leap_every_year <- function(x) {
  ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
}

# Time plot wrapper
time_plot <- function(time_span, df, time_highlight){
  
  # Check that ts has a 't' and 'temp' column
  col_t_error <- "Ensure uploaded time series has a date column 't'."
  col_temp_error <- "Ensure uploaded time series has a temperature column 'temp'."
  col_error <- NULL
  if(!"t" %in% colnames(df)) 
    col_error <- col_t_error
  if(!"temp" %in% colnames(df)){
    if(is.null(col_error)){
      col_error <- col_temp_error
    } else {
      col_error <- paste0(col_t_error,"\n",col_temp_error)
    }
  }
  
  # Plot error message
  if(!is.null(col_error)){
    timePlot <- ggplot() + 
      geom_text(aes(x = 1, y = 1, label = col_error), size = 12) +
      labs(x = NULL, y = "Temperature [°C]") + theme_bw(base_size = 30)
    return(timePlot)
  }
  
  # Get time series and modify t columns
  df <- df |> 
    mutate(all = "All",
           month = lubridate::month(t, label = TRUE),
           year = lubridate::year(t),
           doy = leap_every_year(t),
           day = t)
  
  # Set t column to timeSelect
  if(time_span == "All"){
    df$t <- df$all
  } else if(time_span == "Month"){
    df$t <- df$month
  } else if(time_span == "Year"){
    df$t <- df$year
  } else if(time_span == "DOY"){
    df$t <- df$doy
  } else if(time_span == "Day"){
    df$t <- df$day
  }
  
  if(time_span == "DOY"){
    box_line_width = 0.1 
  } else if(time_span == "Year") {
    box_line_width = 1
  } else if(time_span == "Month") {
    box_line_width = 2
  } else if(time_span == "All") {
    box_line_width = 3
  }
  
  # Base plot
  timePlot <- ggplot(data = df, aes(x = t, y = temp)) + 
    labs(x = NULL, y = "Temperature [°C]") + theme_bw(base_size = 30)
  
  # Add points
  if(time_highlight == "None")
    timePlot <- timePlot + geom_point(position = "jitter")
  if(time_highlight == "Month")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = month)) + 
    scale_colour_viridis_d() + guides(colour = guide_legend(override.aes = list(shape = 15, size = 10)))
  if(time_highlight == "Year")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = year)) + 
    scale_colour_viridis_c(option = "A")
  if(time_highlight == "DOY")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = doy)) + 
    scale_colour_viridis_c(option = "B")
  
  # Add boxplots
  if(time_span != "Day"){
    timePlot <- timePlot + geom_boxplot(aes(group = t), linewidth = box_line_width,
                                        colour = "grey", alpha = 0.4, outlier.shape = NA)
  }
  
  # Scale x axis
  if(time_span == "All")  timePlot <- timePlot + scale_x_discrete(expand = c(0, 0))
  if(time_span %in% c("Year", "DOY"))  timePlot <- timePlot + scale_x_continuous(expand = c(0, 0))
  if(time_span == "Day")  timePlot <- timePlot + scale_x_date(expand = c(0, 0))
  
  # Exit
  timePlot
}

# Guided tour -------------------------------------------------------------

guide <- Cicerone$
  new(id = "guide")$
  step(
    "control_panel",
    "Control panel",
    HTML("The controls are laid out as a series of accordions. 
           Clicking on any of them will open up additional options.
           The controls here relate to the similarly named tabs in the navigation bar at the top of the page."),
    position = "right"
  )$
  step(
    el = "nav_bar",
    title = "Navigation tabs",
    description = HTML("Click on different tabs to travel to various portions of the app."),
    position = "bottom"
  )$
  step(
    "[data-value='ts_tab']",
    "Time series tab",
    HTML("This tab shows what the underlying data in the chosen time series look like.
         Click the different tabs to see the data organised by time. 
         There is a small menu button on the right that will open a control panle to colour the
         data points by different time dimensions.
         <hr>
         <b>NB:</b> if one choses to upload a time series, it must contain a daily date column named 't'
         and a temperature (or any numeric values) named 'temp'."),
    is_id = FALSE, position = "bottom"
  )$
  step(
    "[data-value='stats_tab']",
    "Statistics tab",
    HTML("Here we see two plots.
         The first shows the full time series, and what the selection of different baselines can look like.
         The second plot shows how the thresholds are calculated, given the desired value (e.g. 90th percentile)."),
    is_id = FALSE, position = "bottom"
  )$
  step(
    "[data-value='detect_tab']",
    "Detection tab",
    HTML("After selecting a time series, and applying statistics to determine our climatology and threshold,
         this tab will show which MHWs have been detected in our time series.
         It is possible to view the events in a table, or as interactive lolliplots."),
    is_id = FALSE, position = "bottom"
  )$
  step(
    "[data-value='event_tab']",
    "The main event",
    HTML("The single plot shown on this tab highlights the largest event (via cumulative intensity) detected in the time series.
         The various important metrics are labeled on the plot."),
    is_id = FALSE, position = "bottom"
  )$
  step(
    "nav_bar",
    "Interactions",
    HTML("There are many interactions between the control panel and the content found throughout the navigation tabs.
         Explore the buttons and how they affect the visuals in the plots. There are also categories to think about..."),
    position = "bottom"
  )
