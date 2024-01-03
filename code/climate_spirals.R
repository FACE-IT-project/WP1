# code/climate_spirals
# Code used to create climate spirals from temperature data


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(gganimate)


# Northwestern Med --------------------------------------------------------

# Load+subset ERSST data
# File locations
ERSST_files <- dir("~/pCloudDrive/FACE-IT_data/ERSST", full.names = TRUE)

# Function for loading and subsetting
ERSST_region_mean <- function(ERSST_file, lon_range, lat_range){
  vec_file <- sapply(strsplit(ERSST_file, "/"), "[[", 7)
  vec_date <- sapply(strsplit(vec_file, "\\."), "[[", 3)
  df_mean <- tidync(ERSST_file) |> 
    hyper_filter(lon = between(lon, lon_range[1], lon_range[2]),
                 lat = between(lat, lat_range[1], lat_range[2])) |> 
    hyper_tibble() |> 
    mutate(t = as.Date(paste0(vec_date,"01"), format = "%Y%m%d")) |> 
    summarise(sst = mean(sst), ssta = mean(ssta), .by = t)
  # rm(vec_file, vec_date, df_mean)
}

# Run on all files
ERSST_NW_med <- plyr::ldply(ERSST_files, ERSST_region_mean, .parallel = T,
                            lon_range = c(0, 10), lat_range = c(40, 50))

# Create monthly clim anomalies
# NB: Rather using the defaule anomalies based on a 1971-2000 climatology
# ERSST_NW_med_anoms <- ERSST_NW_med |> 
#   mutate(year = year(t),
#          month = month(t)) |> 
#   group_by(month) |> 
#   mutate(sst_clim = mean(sst)) |> 
#   ungroup() |> 
#   mutate(ssta_clim = sst-sst_clim)

## Code adapted from:
# https://github.com/riffomonas/climate_viz/blob/57916db41f0392780af3e27a44985093c6e5675c/code/climate_spiral_nasa.R

# Prep data
t_diff <- ERSST_NW_med |> 
  mutate(year = year(t),
         month = month(t, label = TRUE, abbr = TRUE),
         t_diff = ssta) |> 
  dplyr::select(year, month, t_diff) |> 
  drop_na()

# Create gap plug for polar projection
next_jan <- t_diff |> 
  filter(month == "Jan") |> 
  mutate(year = year - 1,
         month = "next_Jan")

# Bind them and filter if necessary
t_data <- bind_rows(t_diff, next_jan) |> 
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month))  |> 
  arrange(year, month) |> 
  filter(t_diff < 3, t_diff > -3) |> # NB: These are wonky
  # filter(year >= 1950, year <= 1980) |> # For testing
  filter(year != 1853) |> # Remove fake year introduced for polar plot plug
  mutate(step_number = 1:n())

# Plot annotation, not used
# annotation <- t_data %>%
#   slice_max(year) %>%
#   slice_max(month_number)

# The temperature anomaly lines to plot
temp_lines <- tibble(
  x = 1,
  y = c(1, 0, -1),
  labels = c("+1\u00B0 C", "0\u00B0 C", "-1\u00B0 C")
)

# Month labels around plot edge
month_labels <- tibble(
  x = 1:12,
  labels = toupper(month.abb),
  y = 2.2
)

# The length of the segments used for the anomaly lines
gridlines <- tibble(
  x = c(1.2, 1.3, 1.6),
  xend = c(12.8, 12.7, 12.4),
  y = c(1, 0, -1), 
  yend = y
  
)

# Create plot/animation
a <- t_data %>% 
  ggplot(aes(x = month_number, y = t_diff, group = year, color = t_diff)) +
  geom_label(aes(x = 1, y = -4.7, label = year),
             fill = "black",
             label.size = 0,
             size = 6) +
  geom_line() +
  geom_segment(data = gridlines, aes(x = x, y = y, xend = xend, yend = yend),
               color = c("yellow", "green", "yellow"),
               inherit.aes = FALSE) +
  geom_text(data = temp_lines, aes(x = x, y = y, label = labels),
            color = c("yellow", "green", "yellow"), size = 2.5, fontface = "bold",
            inherit.aes = FALSE) +
  geom_text(data = month_labels, aes(x = x, y = y, label = labels),
            inherit.aes = FALSE, color = "yellow") +
  scale_y_continuous(
    limits = c(-5.0, 3.0), expand = c(0, -0.3), 
  ) +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                        guide = "none") +
  coord_polar(start = 0) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill = "black", color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  ) +
  transition_manual(frames = year, cumulative = TRUE)
# a
#ggsave("figures/climate_spiral_nasa.png", width=4.155, height=4.5, unit="in")

# Create GIF
animate(a, width = 4.5, height = 4.5, unit = "in", res = 300, duration = 18)
anim_save("figures/climate_spiral_NW_Med.gif")

# Create video
gganimate::animate(a, width = 4.5, height = 4.5, unit = "in", 
                   res = 300, fps = 10, duration = 18, 
                   renderer = av_renderer("figures/climate_spiral_NW_Med.mp4")
                   )

