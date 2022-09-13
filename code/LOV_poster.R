# code/LOV_poster_2022.R
# Code used for the figure contributed to the LOV poster for the 2022 September seminar


# Setup -------------------------------------------------------------------

# library(ggraph) # Loaded in functions.R where an internal function is overwritten
source("code/functions.R")
library(geomtextpath)
library(ggpmisc)
library(ggpattern)
library(igraph) # Create network

# Ice cover colours
# ice_cover_colours <- c(
#   "ocean" = "navy",
#   "land" = "slategrey",
#   "sea ice" = "mintcream",
#   "coast" = "dodgerblue",
#   "exclude" = "gold"
# )

# Key driver category colours
driver_cat_colours <- c(
  "cryo" = "mintcream",
  "phys" = "skyblue",
  "chem" = "#F6EA7C",
  "bio" = "#A2ED84",
  "soc" = "#F48080"
)


# Data --------------------------------------------------------------------

# Load network nodes and edges
# NB: These were created by hand by RWS based on the text of the review paper
nodes <- read_csv("data/figures/rp_fig_2_nodes.csv") %>% 
  filter(level == "key")
edges <- read_csv("data/figures/rp_fig_2_edges.csv") %>% 
  filter(!from %in% c("s15", "s16", "s17", "s18", "s19"),
         !to %in% c("s15", "s16", "s17", "s18", "s19")) %>%  # Could put these as smaller networks in the appendix
  mutate(from_num = case_when(from_sign == "decrease" ~ 1, from_sign == "complex" ~ 2, from_sign == "increase" ~ 3),
         to_num = case_when(to_sign == "decrease" ~ 1, to_sign == "complex" ~ 2, to_sign == "increase" ~ 3))


# Panel A -----------------------------------------------------------------

# Horizontal barplots showing time over which data are available
# Colour by category with sub bars for each driver
# Text labels showing total number of datasets with data for that driver

# Load all clean data
load("data/analyses/clean_all.RData")

# Get metadata for driver data coverage and total datasets
data_meta <- clean_all %>% 
  filter(type == "in situ",
         var_group %in% c("Ice vars", "Glacier vars", "river", 
                          "Sea temp", "Salinity", "PAR",          
                          "pCO2", "Nutrients",
                          "Chla", "Biomass", "Species",
                          "Tourism", "Shipping" )) %>% 
  group_by(citation, var_group) %>% 
  summarise(start_date = min(date, na.rm = T), end_date = max(date, na.rm = T), 
            data_points = n(), .groups = "drop") %>% 
  group_by(var_group) %>% 
  summarise(min_date = min(start_date, na.rm = T), max_date = max(end_date, na.rm = T), 
            data_sets = n(), data_points = sum(data_points, na.rm = T), .groups = "drop") %>% 
  rbind(data.frame(var_group = "Governance", min_date = NA, max_date = NA, data_sets = 1, data_points = 1))

# Clean up for plotting
panel_a_data <- data_meta %>% 
  mutate(driver = case_when(var_group == "Biomass" ~ "biomass",
                            var_group == "Chla" ~ "primary production",
                            var_group == "Glacier vars" ~ "glacier mass balance",
                            var_group == "Ice vars" ~ "sea ice",
                            var_group == "Nutrients" ~ "nutrients",
                            var_group == "PAR" ~ "light",
                            var_group == "pCO2" ~ "carbonate system",
                            var_group == "river" ~ "glacial + river discharge",
                            var_group == "Salinity" ~ "salinity",
                            var_group == "Sea temp" ~ "seawater temperature",
                            var_group == "Shipping" ~ "fisheries",
                            var_group == "Species" ~ "species richness",
                            var_group == "Tourism" ~ "tourism",
                            var_group == "Governance" ~ "governance"),
         category = case_when(driver %in% c("sea ice", "glacier mass balance", "glacial + river discharge") ~ "cryosphere",
                              driver %in% c("seawater temperature", "salinity", "light") ~ "physics",
                              driver %in% c("carbonate system", "nutrients") ~ "chemistry",
                              driver %in% c("primary production", "biomass", "species richness") ~ "biology",
                              driver %in% c("governance", "tourism", "fisheries") ~ "social"),
         driver = factor(driver, levels = c("sea ice", "glacier mass balance", "glacial + river discharge",
                                            "seawater temperature", "salinity", "light",
                                            "carbonate system", "nutrients",
                                            "primary production", "biomass", "species richness",
                                            "governance", "tourism", "fisheries")))#,
         # data_points = scales::comma(data_points))

# Plot the data
panel_a <- ggplot(panel_a_data, aes(y = driver, x = log10(data_points))) +
  geom_segment(aes(y = driver, yend = driver, x = 0, xend = log10(data_points)),
               colour = "black", size =  11) +
  geom_point(aes(x = min_date, y = driver, fill = category), 
             shape = 21, size = 14, stroke = 1.2, show.legend = F) +
  geom_point(aes(x = max_date, y = driver, fill = category), 
             shape = 21, size = 14, stroke = 1.2, show.legend = F) +
  geom_segment(aes(x = min_date, xend = max_date, y = driver, yend = driver, colour = category), 
               size = 9, show.legend = F) +
  geom_label(aes(label = paste0(driver,"\nDatasets: ",data_sets,"\nData points: ",data_points),
                 x = as.Date("2024-12-31"), y = driver, hjust = "right"), alpha = 1, size = 6,
             label.padding = unit(0.3, "lines"), label.size = unit(0.7, "lines")) +
  scale_y_discrete(limits = rev, position = "right") +
  scale_x_date(limits = c(as.Date("1871-01-01"), as.Date("2070-12-31")),
               date_labels = "%Y",
               breaks = seq(as.Date("1877-01-01"), as.Date("2021-01-01"), length = 7),
               expand = c(0, 0)) +
  scale_colour_manual("Category", aesthetics = c("colour", "fill"),
                      breaks = c("cryosphere", "physics", "chemistry", "biology", "social"),
                      values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(fill = "grey90", colour = NA),
        axis.text.y = element_blank(),
        plot.margin = margin(t = 10, r = -20, b = 10, l = 20, unit = "pt"),
        panel.grid = element_line(colour = "black"),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 15))
# panel_a
ggsave("~/Desktop/panel_a.png", panel_b, width = 16, height = 14)


# Panel B -----------------------------------------------------------------

# Create network
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Generate colors based on category:
cat_colours <- c("violet", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")
trend_colours <- c("blue", "purple", "red")
V(net)$cat_colour <- cat_colours[V(net)$cat_num]
V(net)$trend_colour <- trend_colours[V(net)$trend_num]
V(net)$name

# Dataframe to manually place individual labels
net_label_coords <- data.frame(label = nodes$driver[1:14],
                               x = c(1.07, 1.02, 0.83, # Cryosphere
                                     0.23, -0.22, -0.63, # Physics
                                     -0.92, -1.0, # Chemistry
                                     -0.95, -0.63, -0.225, # Biology
                                     0.225, 0.63, 0.91), # Social
                               y = c(0.088, 0.523, 0.87, # Cryosphere
                                     1.062, 1.062, 0.872, # Physics
                                     0.52, 0.09, # Chemistry
                                     -0.52, -0.87, -1.063, # Biology
                                     -1.063, -0.871, -0.523)) # Social

# Circle network
panel_b <- ggraph(net, layout = "circle") +
  geom_edge_fan(aes(colour = to_sign), width = 2, alpha = 0.8, show.legend = F,
                 arrow = arrow(length = unit(0.025, "npc"), type = "closed")) +
  geom_node_point(aes(fill = category, colour = trend),
                  shape = 21, size = 16, stroke = 3) +
  # geom_node_label(aes(label = driver), size = 4, color = "black", alpha = 0.8,
  #                 label.padding = unit(0.3, "lines"), label.size = unit(0.7, "lines")) +
  geom_label(data = net_label_coords, aes(label = label, x = x, y = y), size = 6, color = "black",
             label.padding = unit(0.3, "lines"), label.size = unit(0.7, "lines")) +
  scale_edge_colour_manual("Trend/\nImpact", 
                           breaks = c("increase", "decrease", "complex"),
                           values = c("purple", "blue", "red"),
                           guide_legend(order = 2, override.aes = list(shape = 15, size = 16))) +
  scale_colour_manual("Trend or impact\nfor driver",
                      breaks = c("increase", "decrease", "complex"),
                      values = c("red", "blue", "purple")) +
  scale_fill_manual("Category", 
                    breaks = c("cryosphere", "physics", "chemistry", "biology", "social"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  scale_x_continuous(limits = c(-1.2, 1.2)) +
  guides(colour = guide_legend(order = 1, label.position = "right", override.aes = list(shape = 15, size = 16)),
  # guides(colour = guide_legend(order = 1),
         # edge_colour = guide_legend(order = 2, override.aes = list(edge_colour_shape = 15)),
         fill = guide_legend(order = 3, label.position = "left", override.aes = list(stroke = 2))) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", colour = NA),
        plot.margin = margin(t = -10, r = 170, b = -10, l = -20, unit = "pt"),
        legend.position = c(1.08, 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.margin = margin(t = 10, r = 20, b = 10, l = 20, unit = "pt"))
# panel_b
ggsave("~/Desktop/panel_a.png", panel_a, width = 16, height = 14)


# Final -------------------------------------------------------------------

# Combine
fig_1 <- ggpubr::ggarrange(panel_a, panel_b, ncol = 2, widths = c(1, 1),
                           labels = c("A)", "B)"), font.label = list(size = 22))

# Save
ggsave("figures/LOV_fig_1.png", fig_1, width = 32, height = 14)

# Text
# Figure XX: The two primary objectives of WP1: A) understanding the relationships between the key drivers of change 
# in socio-ecological Arctic fjords systems and B) managing & analysing the corresponding data.
# A) The trend in the change of each driver (i.e. increasing, decreasing, or complex) 
# is shown via the coloured borders of the labelled circles. 
# The impacts that the drivers have on each other are shown with coloured arrows.
# B) The date range of data collected for each of the drivers. 
# Note that there are no datasets/points currently available for governance.

