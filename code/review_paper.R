# code/review_paper.R
# The code used for the tables and figures in the WP1 review paper (D1.3)


# Setup -------------------------------------------------------------------

# library(ggraph) # Loaded in functions.R where an internal function is overwritten
source("code/functions.R")
library(geomtextpath)
library(ggpmisc)
library(ggpattern)
library(igraph) # Create network


# Data --------------------------------------------------------------------

# Load network nodes and edges
# NB: These were created by hand by RWS based on the text of the review paper
nodes <- read_csv("data/figures/rp_fig_2_nodes.csv") %>% filter(level == "key")
edges <- read_csv("data/figures/rp_fig_2_edges.csv") %>% 
  filter(!from %in% c("s15", "s16", "s17", "s18", "s19"),
         !to %in% c("s15", "s16", "s17", "s18", "s19")) %>%  # Could put these as smaller networks in the appendix
  mutate(from_num = case_when(from_sign == "decrease" ~ 1, from_sign == "complex" ~ 2, from_sign == "increase" ~ 3),
         to_num = case_when(to_sign == "decrease" ~ 1, to_sign == "complex" ~ 2, to_sign == "increase" ~ 3))
#

# Figure 1 ----------------------------------------------------------------
# Map of the study area that shows all geographic features referenced in the text

# EU bbox
bbox_EU_poly <- bbox_to_poly(bbox_EU, "EU")

# EU bbox from Copernicus
bbox_EU_CMEMS <- c(-25, 60, 66, 90)
bbox_EU_CMEMS_poly <- bbox_to_poly(bbox_EU_CMEMS, "EU CMEMS")

# EU Arctic land shapes
coastline_Arctic <- filter(coastline_full_df, y > 50, x < 90, x > -90)

# Study sites
site_points <- data.frame(site = factor(x = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                              "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                              "Porsangerfjorden"),
                                        levels = c("Kongsfjorden", "Isfjorden","Storfjorden", 
                                                   "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                                   "Porsangerfjorden")),
                          lon = c(11.845, 14.365, 19.88, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.78, 74.517, 69.36, 64.405, 70.6))

# Full study area
rp_fig_1 <- basemap(limits = c(-50, 50, 61, 90), bathymetry = T) +
  annotation_spatial(bbox_EU_CMEMS_poly, fill = "darkgreen", colour = "black", alpha = 0.1) +
  # WSC arrow and label
  geom_spatial_segment(
    aes(x = 15, xend = 6, y = 75, yend = 80.5), 
    arrow = arrow(length = unit(0.03, "npc")),
    colour = "red", linewidth = 3, crs = 4326, great_circle = T) +
  geom_spatial_label(aes(x = 14, y = 76, label = "WSC"), 
                     colour = "red", crs = 4326, size = 4, alpha = 0.9) +
  # Other labels
  geom_spatial_label(aes(x = 0, y = 78, label = "Fram\nStrait"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 27, y = 79, label = "Svalbard"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 40, y = 74, label = "Barents Sea"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 0, y = 73.5, label = "Greenland\nSea"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 0, y = 68, label = "Norwegian\nSea"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = -40, y = 76, label = "Greenland"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 19, y = 70.5, label = "Northern\nNorway"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  # Site points
  geom_spatial_point(data = site_points, size = 6, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points, size = 5, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  scale_colour_manual("Site", values = site_colours) +
  # Other minutia
  labs(x = NULL, y = NULL) +
  # labs(title = "FACE-IT study area and focal sites",
  #      colour = "Site",
  #      caption = "robert.schlegel@imev-mer.fr\nSorbonne Université") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = NA, colour = "black"),
        # plot.background = element_rect(fill = "grey90", colour = "black"),
        # plot.margin = margin(t = -2, r = 0, b = -2, l = 0, unit = "cm"),
        axis.text = element_text(colour = "black", size = 10),
        legend.position = c(0.9195, 0.31),
        legend.box.margin = margin(10, 10, 10, 10), 
        legend.box.background = element_rect(fill = "white", colour = "black"))
# rp_fig_1
ggsave("figures/rp_fig_1.png", rp_fig_1, height = 8, width = 12)#, dpi = 600)
ggsave("figures/rp_fig_1.eps", rp_fig_1, height = 8, width = 12, dpi = 1000)
# ggsave("figures/rp_fig_1.tiff", rp_fig_1, height = 8, width = 12, dpi = 1000) # lol. 348 MB


# Figure 2 ----------------------------------------------------------------
# Flow chart showing interactions of all key drivers

# If the network figure is used, the blog should be cited in the acknowledgements.
# https://kateto.net/network-visualization
# Ognyanova, K. (2021) Network visualization with R. Retrieved from www.kateto.net/network-visualization.

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
rp_fig_2_plot <- ggraph(net, layout = "circle") +
  geom_edge_fan(aes(colour = to_sign), width = 2, alpha = 0.8, show.legend = F,
                arrow = arrow(length = unit(0.025, "npc"), type = "closed")) +
  geom_node_point(aes(fill = category, colour = trend),
                  shape = 21, size = 16, stroke = 3) +
  geom_label(data = net_label_coords, aes(label = label, x = x, y = y, fill = category), 
             size = 7, color = "black", show.legend = F,
             label.padding = unit(0.4, "lines"), label.size = unit(0.7, "lines")) +
  scale_edge_colour_manual("Trend/\nImpact", 
                           breaks = c("increase", "decrease", "complex"),
                           values = c("red", "blue", "purple")) +
  scale_colour_manual("Driver trends\nand impacts",
                      breaks = c("increase", "decrease", "complex"),
                      labels = c("increase", "decrease", "uncertain"),
                      values = c("red", "blue", "purple")) +
  scale_fill_manual("Category", 
                    breaks = c("cryosphere", "physics", "chemistry", "biology", "social"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  scale_x_reverse(limits = c(1.2, -1.2)) +
  guides(colour = guide_legend(order = 1, label.position = "right", override.aes = list(shape = 15, size = 16)),
         fill = "none") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", colour = "black"),
        panel.background = element_rect(fill = NA, colour = "black"),
        # panel.border = element_rect(fill = NA, colour = "black"),
        plot.margin = margin(t = -10, r = -10, b = -10, l = 0, unit = "pt"),
        legend.position = c(0.908, 0.1285),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.margin = margin(t = 10, r = 15, b = 10, l = 10, unit = "pt"))

# Combine and save
ggsave("figures/rp_fig_2.png", rp_fig_2_plot, width = 14, height = 12)

### Interactive networks
# https://kateto.net/network-visualization

## ndtv
# NB: With more work this could allow for describing the relationships with popup notes
library(ndtv)

# Create base
net3 <- network(edges,#[c(-3, -22, -25),], # Remove parallels
                vertex.attr = nodes, matrix.type = "edgelist", 
                loops = T, multiple = F, ignore.eval = F)

# Force dynamic (i.e. animated) network
# net3 <- networkDynamic(net3)

# Attempt at creating preset coordinates
# NB: Neither currently work
nodes_coords <- matrix(data = c(c(1, 2, 3, 4, 5, 6, 7, 7, 6, 5, 4, 3, 2, 1),
                                c(4, 5, 6, 7, 7, 6, 5, 4, 3, 2 , 1, 1, 2, 3)),
                       nrow = 14, ncol = 2)
nodes_coords <- matrix(data = c(c(0, 0.1, 0.2, 0.3, 0.4, 0.5 , 0.6, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0),
                                c(0.3, 0.4, 0.5, 0.6, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0, 0, 0.1, 0.2)),
                       nrow = 14, ncol = 2)

# Plot
render.d3movie(net3,
               # render.par = list(initial.coords = nodes_coords),
               render.par = list(initial.coords = 1),
               usearrows = T, 
               displaylabels = T, 
               label = (net3 %v% 'driver'),
               # label.col = (net3 %e% "cat_colour"),
               label.col = "black",
               label.cex = 2,
               main = "Relationships of key drivers within Arctic fjord socio-ecologicals systems",
               xlab = "Double click on a node to highlight all relationships.",
               bg = "grey90", 
               vertex.col = cat_colours[(net3 %v% "cat_num")],
               # vertex.col = "white",
               vertex.border = "black",
               # vertex.sides = 7,
               # vertex.rot = 90, # Rotation of ploygons
               vertex.cex = 4,
               vertex.lwd = 4,
               edge.lwd = 8,
               edge.col = "grey30",
               vertex.tooltip = paste("<b>Category:</b>", (net3 %v% 'category') , "<br>",
                                      "<b>Driver:</b>", (net3 %v% 'driver')),
               edge.tooltip = paste((net3 %e% 'note')),
               # edge.tooltip = paste("<b>Edge type:</b>", (net3 %e% 'note'), "<br>", 
               #                      "<b>Edge weight:</b>", (net3 %e% "note" ) ),
               # output.mode = "htmlWidget", # Can be used in shiny app
               launchBrowser = T, filename = "docs/driver_network.html")

