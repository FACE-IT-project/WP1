# code/LOV_poster_2022.R
# Code used for the figure contributed to the LOV poster for the 2022 September seminar


# Setup -------------------------------------------------------------------

# library(ggraph) # Loaded in functions.R where an internal function is overwritten
source("code/functions.R")
library(geomtextpath)
library(ggpmisc)
library(ggpattern)
library(ggtext)
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

# Horizontal barplots showing the total data points available
# Colour by category with sub bars for each driver

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
                                            "governance", "tourism", "fisheries")),
         driver_num = as.numeric(driver),
         data_points_log10 = log10(data_points))
         # data_points = scales::comma(data_points))

# Labels for categories
panel_a_labels <- panel_a_data %>% 
  arrange(driver_num) %>% 
  slice(2, 5, 7, 10, 13) %>% 
  mutate(y_num = c(2, 5, 7.5, 10, 13))

# Plot the data
panel_a <- ggplot(panel_a_data, aes(x = data_points_log10, y = driver_num)) +
  geom_point(aes(x = 0, y = driver_num), alpha = 0) + # Keep governance driver in the y axis
  geom_segment(data = panel_a_data[-14,],
               aes(x = -0.01, xend = data_points_log10, y = driver_num, yend = driver_num),
               colour = "black", size =  13) +
  geom_point(data = panel_a_data[-14,], 
             aes(x = data_points_log10, y = driver_num, fill = category), 
             shape = 21, size = 16, stroke = 0.8, show.legend = F) +
  geom_segment(data = panel_a_data[-14,],
               aes(x = 0, xend = data_points_log10, y = driver_num, yend = driver_num, colour = category), 
               size = 12, show.legend = F) +
  geom_text(aes(x = 0.1, label = driver), hjust = "left", size = 7) +
  geom_richtext(data = panel_a_labels, hjust = 0.5,
                aes(x = -0.2, y = y_num, label = category, fill = category),
                angle = 90, size = 9, show.legend = F, label.r = unit(1, "lines"),
                label.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
                label.padding = unit(c(0.5, 1.5, 0.5, 0.5), "lines")) + # Note that ggarrange messes this up
  scale_y_reverse() +
  scale_x_continuous(limits = c(-0.4, 6.8),
                     breaks = seq(1, 6, 1),
                     labels = c("10", "100", "1,000", "10,000", "100,000", "1,000,000"),
                     expand = c(0, 0)) +
  scale_colour_manual("Category", aesthetics = c("colour", "fill"),
                      breaks = c("cryosphere", "physics", "chemistry", "biology", "social"),
                      values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  labs(x = "Data points [log10]", y = NULL) +
  theme(plot.background = element_rect(fill = "grey90", colour = NA),
        # axis.text.y = element_blank(),
        plot.margin = margin(t = 30, r = 40, b = 10, l = 0, unit = "pt"),
        panel.grid = element_line(colour = "black"),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.minor.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major.x = element_line(colour = "black"),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 18, colour = "black"),
        # axis.text.y = element_text(angle = 45))
        axis.text.y = element_blank())
# panel_a
ggsave("~/Desktop/panel_a.png", panel_a, width = 14, height = 12)


# Panel B -----------------------------------------------------------------


# Create network
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Dataframe to manually place individual labels
net_label_coords <- data.frame(label = nodes$driver[1:14],
                               category = nodes$category[1:14],
                               x = c(1.07, 0.98, 0.85, # Cryosphere
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
  geom_label(data = net_label_coords, aes(label = label, x = x, y = y, fill = category), 
             size = 7, color = "black", show.legend = F,
             label.padding = unit(0.4, "lines"), label.size = unit(0.7, "lines")) +
  scale_edge_colour_manual("Trend/\nImpact", 
                           breaks = c("increase", "decrease", "complex"),
                           values = c("purple", "blue", "red")) +
  scale_colour_manual("Driver trends\nand impacts",
                      breaks = c("increase", "decrease", "complex"),
                      labels = c("increse", "decrease", "both"),
                      values = c("red", "blue", "purple")) +
  scale_fill_manual("Category", 
                    breaks = c("cryosphere", "physics", "chemistry", "biology", "social"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  scale_x_reverse(limits = c(1.2, -1.2)) +
  guides(colour = guide_legend(order = 1, label.position = "right", override.aes = list(shape = 15, size = 16)),
         fill = "none") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", colour = NA),
        plot.margin = margin(t = -10, r = -10, b = -10, l = 0, unit = "pt"),
        legend.position = c(0.908, 0.136),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.margin = margin(t = 10, r = 15, b = 10, l = 10, unit = "pt"))
# panel_b
ggsave("~/Desktop/panel_b.png", panel_b, width = 14, height = 12)


# Final -------------------------------------------------------------------

# Combine
fig_11 <- ggpubr::ggarrange(panel_a, panel_b, ncol = 2,
                           labels = c("A)", "B)"), font.label = list(size = 24))

# Save
ggsave("figures/LOV_fig_1.png", fig_1, width = 28, height = 12)

# Text
# Figure XX: The socio-ecological fjord systems of the Arctic may be classified into one of five categories, 
# with a total of 14 key drivers of change therein. 
# A) The number of data points collected, managed, and analysed in pursuit of the understanding of each of the key drivers (log10 scale).
# Note that there are no data points currently available for governance. 
# B) The trend in the change of each driver (i.e. increasing, decreasing, or both) is shown 
# via the coloured borders of the labelled circles. 
# The impacts that the drivers have on each other are shown with coloured arrows.

