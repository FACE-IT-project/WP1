# code/climate_spirals
# Code used to create climate spirals from temperature data


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(gganimate)


# Northwestern Med --------------------------------------------------------

# Load+subset ERSST data
# File locations
ERSST_files <- dir("~/pCloudDrive/FACE-IT_data/ERSST", full.names = TRUE)

# Run on all files
ERSST_NW_med <- plyr::ldply(ERSST_files, ERSST_region_mean, .parallel = T,
                            lon_range = c(0, 10), lat_range = c(40, 50))

# Create monthly clim anomalies
# NB: Rather using the default anomalies based on a 1971-2000 climatology
# ERSST_NW_med_anoms <- ERSST_NW_med |> 
#   mutate(year = year(t),
#          month = month(t)) |> 
#   group_by(month) |> 
#   mutate(sst_clim = mean(sst)) |> 
#   ungroup() |> 
#   mutate(ssta_clim = sst-sst_clim)

# Load HadCRUT.5 data and subset to NW Med
Had_NW_med <- tidync("~/pCloudDrive/FACE-IT_data/HadCRUT.5/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc") |> 
  hyper_filter(longitude = between(longitude, -2.5, 12.5),
               latitude = between(latitude, 37.5, 52.5)) |> 
  hyper_tibble() |> 
  mutate(t = as.Date(time, origin = "1850-01-01")) |> 
  summarise(tas_mean = mean(tas_mean), .by = t)

## Code adapted from:
# https://github.com/riffomonas/climate_viz/blob/57916db41f0392780af3e27a44985093c6e5675c/code/climate_spiral_nasa.R

# Prep data
t_diff <- Had_NW_med |> 
  mutate(year = year(t),
         month = month(t, label = TRUE, abbr = TRUE),
         # t_diff = ssta) |> 
         t_diff = tas_mean) |> 
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
  mutate(t_diff = case_when(t_diff > 2 ~ 2, t_diff < -2 ~ -2,
                            TRUE ~ t_diff)) |> # NB: This is done to help the visualisation 
  # filter(t_diff < 3, t_diff > -3) |> # NB: These are wonky
  # filter(year != 1853) |> # Remove fake year introduced for polar plot plug
  filter(year != 1849) |> # Remove fake year introduced for polar plot plug
  filter(year >= 1950, year <= 1980) |> # For testing
  mutate(step_number = 1:n())

# Annual means for year label
t_annual <- t_diff |> 
  filter(year >= 1950, year <= 1980) |>  # For testing
  summarise(t_diff = mean(t_diff), .by = "year")

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
  x = c(1.3, 1.4, 1.7),
  xend = c(12.7, 12.6, 12.3),
  y = c(1, 0, -1), 
  yend = y
  
)

# Create plot/animation
a <- t_data |> 
  ggplot(aes(x = month_number, y = t_diff, group = year, color = t_diff)) +
  geom_label(data = t_annual, inherit.aes = FALSE,
             aes(x = 1, y = -2.7, label = year, colour = t_diff),
             fill = "black",
             label.size = 0,
             size = 5) +
  geom_line() +
  geom_segment(data = gridlines, aes(x = x, y = y, xend = xend, yend = yend),
               color = c("yellow", "green", "yellow"),
               inherit.aes = FALSE) +
  geom_text(data = temp_lines, aes(x = x, y = y, label = labels),
            color = c("yellow", "green", "yellow"), size = 3, fontface = "bold",
            inherit.aes = FALSE) +
  geom_text(data = month_labels, aes(x = x, y = y, label = labels),
            inherit.aes = FALSE, color = "yellow") +
  scale_y_continuous(
    limits = c(-3.0, 2.5), expand = c(0, -0.3), 
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

# Create video
gganimate::animate(a, width = 4.155, height = 4.7, unit = "in", 
                   res = 100, fps = 5, duration = 6, # For testing
                   # res = 300, fps = 5, duration = 36, # For full data
                   renderer = av_renderer("figures/climate_spiral_NW_Med.mp4"))

# Create GIF
animate(a, width = 4.5, height = 4.5, unit = "in", 
        res = 100, fps = 5, duration = 6) # For testing
# res = 300,fps = 5, duration = 36)# For full data
anim_save("figures/climate_spiral_NW_Med.gif")


## Ed Hawkins version

# Prep data
t_diff <- Had_NW_med |> 
  mutate(year = year(t),
         month = month(t, label = TRUE, abbr = TRUE),
         # t_diff = ssta) |> 
         t_diff = tas_mean) |> 
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
  mutate(t_diff = case_when(t_diff > 3 ~ 3, t_diff < -2.0 ~ -2.0,
                            TRUE ~ t_diff)) |> # NB: This is done to help the visualisation 
  # filter(t_diff < 3, t_diff > -3) |> # NB: These are wonky
  # filter(year != 1853) |> # Remove fake year introduced for polar plot plug
  filter(year != 1849) |> # Remove fake year introduced for polar plot plug
  # filter(year >= 1970, year <= 1980) |> # For testing
  mutate(step_number = 1:n())

# Annual means for year label
t_annual <- t_data |> 
  summarise(t_diff = mean(t_diff), .by = "year")

temp_lines <- tibble(
  x = 12,
  y = c(0.0, 1.0, 2.0),
  labels = c("0.0\u00B0C", "1.0\u00B0C", "2.0\u00B0C")
)

month_labels <- tibble(
  x = 1:12,
  labels = month.abb,
  y = 3.2
)

a <- t_data %>% 
  ggplot(aes(x = month_number, y = t_diff, group = year, color = year)) +
  geom_rect(aes(xmin = 1, xmax = 13, ymin = -4, ymax = 2.7),
            color = "black", fill = "black",
            inherit.aes = FALSE) +
  geom_hline(yintercept = temp_lines$y, color = "red") +
  geom_label(data = temp_lines, aes(x = x, y = y, label = labels),
             color = "red", fill = "black", label.size = 0,
             inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x = x, y = y, label = labels),
            inherit.aes = FALSE, color="white",
            angle = seq(360 - 360/12, 0, length.out = 12)) +
  geom_label(aes(x = 1, y = -3.3, label = year, colour = year),
             # color = "white", 
             fill = "black",
             label.padding = unit(10, "pt"), label.size = 0,
             size = 6) +
  geom_line() +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb, expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-4, 3.7), expand = c(0, -0.5), 
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_color_viridis_c(breaks = seq(1880, 2020, 20),
                        guide = "none") +
  coord_polar(start = 2*pi/12) +
  labs(x = NULL,
       y = NULL,
       title = "NW Med temperature change (1850-2023)", 
       # caption = "HadCRUT.5.0.2.0\nBaseline: 1961 to 1990\nLon: -2.5 to 12.5\nLat: 37.5 to 52.5") +
       caption = "HadCRUT.5.0.2.0\nBaseline: 1961-1990") +
  theme(
    panel.background = element_rect(fill = "#444444", linewidth = 1),
    plot.background = element_rect(fill = "#444444", color = "#444444"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    # axis.title = element_text(color="white", size=13),
    plot.title = element_text(color = "white", hjust = 0.5, size = 15),
    plot.caption = element_text(color = "white", size = 10)
  ) +
  transition_manual(frames = year, cumulative = TRUE)

# Create video
gganimate::animate(a, width = 4.2, height = 4.9, unit = "in", 
                   # res = 100, fps = 5, duration = 2, # For testing
                   res = 300, fps = 5, duration = 36, # For full data
                   renderer = av_renderer("figures/climate_spiral_NW_Med.mp4"))

# Create GIF
# NB: This doesn't work well
gganimate::animate(a, width = 4.2, height = 4.9, unit = "in", 
                   # res = 100, fps = 5, duration = 6) # For testing
                   res = 300, fps = 5)# For full data
anim_save("figures/climate_spiral_NW_Med.gif")

