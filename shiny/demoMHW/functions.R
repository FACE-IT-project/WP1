# shiny/demoMHW/functions.R
# Script to hold functions and other functionality for demoMHW app

# Functions ---------------------------------------------------------------

# Function for ensuring 366 DOY for non-leap years
leap_every_year <- function(x) {
  ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
}

# Time plot wrapper
time_plot <- function(time_span, df, time_highlight){
  
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
    labs(x = NULL, y = "Temperature [°C]") + theme_bw() +
    theme(text = element_text(size = global_text))
  
  # Add points
  if(time_highlight == "None")
    timePlot <- timePlot + geom_point(position = "jitter")
  if(time_highlight == "Month")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = month)) + 
    scale_colour_viridis_d()
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

# Function needed for making geom_flame() work with plotly
geom2trace.GeomFlame <- function(data, params, p){
  
  x <- y <- y2 <- NULL
  
  # Create data.frame for ease of use
  data1 <- data.frame(x = data[["x"]],
                      y = data[["y"]],
                      y2 = data[["y2"]])
  
  # Grab parameters
  n <- params[["n"]]
  n_gap <- params[["n_gap"]]
  
  # Find events that meet minimum length requirement
  data_event <- heatwaveR::detect_event(data1, x = x, y = y,
                                        seasClim = y,
                                        threshClim = y2,
                                        minDuration = n,
                                        maxGap = n_gap,
                                        protoEvents = T)
  
  # Detect spikes
  data_event$screen <- base::ifelse(data_event$threshCriterion == FALSE, FALSE,
                                    ifelse(data_event$event == FALSE, TRUE, FALSE))
  
  # Screen out spikes
  data1 <- data1[data_event$screen != TRUE,]
  
  # Prepare to find the polygon corners
  x1 <- data1$y
  x2 <- data1$y2
  
  # # Find points where x1 is above x2.
  above <- x1 > x2
  above[above == TRUE] <- 1
  above[is.na(above)] <- 0
  
  # Points always intersect when above=TRUE, then FALSE or reverse
  intersect.points <- which(diff(above) != 0)
  
  # Find the slopes for each line segment.
  x1.slopes <- x1[intersect.points + 1] - x1[intersect.points]
  x2.slopes <- x2[intersect.points + 1] - x2[intersect.points]
  
  # # Find the intersection for each segment.
  x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes - x2.slopes))
  y.points <- x1[intersect.points] + (x1.slopes * (x.points - intersect.points))
  
  # Coerce x.points to the same scale as x
  x_gap <- data1$x[2] - data1$x[1]
  x.points <- data1$x[intersect.points] + (x_gap*(x.points - intersect.points))
  
  # Create new data frame and merge to introduce new rows of data
  data2 <- data.frame(y = c(data1$y, y.points), x = c(data1$x, x.points))
  data2 <- data2[order(data2$x),]
  data3 <- base::merge(data1, data2, by = c("x","y"), all.y = T)
  data3$y2[is.na(data3$y2)] <- data3$y[is.na(data3$y2)]
  
  # Remove missing values for better plotting
  data3$y[data3$y < data3$y2] <- NA
  missing_pos <- !stats::complete.cases(data3[c("x", "y", "y2")])
  ids <- cumsum(missing_pos) + 1
  ids[missing_pos] <- NA
  
  # Get the correct positions
  positions <- data.frame(x = c(data3$x, rev(data3$x)),
                          y = c(data3$y, rev(data3$y2)),
                          ids = c(ids, rev(ids)))
  
  # Convert to a format geom2trace is happy with
  positions <- plotly::group2NA(positions, groupNames = "ids")
  positions <- positions[stats::complete.cases(positions$ids),]
  positions <- dplyr::left_join(positions, data[,-c(2,3)], by = "x")
  if(length(stats::complete.cases(positions$PANEL)) > 1) 
    positions$PANEL <- positions$PANEL[stats::complete.cases(positions$PANEL)][1]
  if(length(stats::complete.cases(positions$group)) > 1) 
    positions$group <- positions$group[stats::complete.cases(positions$group)][1]
  
  # Run the plotly polygon code
  if(length(unique(positions$PANEL)) == 1){
    getFromNamespace("geom2trace.GeomPolygon", asNamespace("plotly"))(positions)
  } else{
    return()
  }
}

