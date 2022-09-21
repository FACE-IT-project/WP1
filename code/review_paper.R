# code/review_paper.R
# The code used for the tables and figures in the WP1 review paper (D1.3)


# Setup -------------------------------------------------------------------

# library(ggraph) # Loaded in functions.R where an internal function is overwritten
source("code/functions.R")
library(geomtextpath)
library(ggpmisc)
library(ggpattern)
library(igraph) # Create network

# Ice cover colours
ice_cover_colours <- c(
  "ocean" = "navy",
  "land" = "slategrey",
  "sea ice" = "mintcream",
  "coast" = "dodgerblue",
  "exclude" = "gold"
)

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

# EU Arctic land shapes
coastline_Arctic <- filter(coastline_full_df, y > 50, x < 90, x > -90)

# Study sites
site_points <- data.frame(site = factor(x = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                              "Young Sound", "Disko Bay", "Nuup Kangerlua", 
                                              "Porsangerfjorden"),
                                        levels = c("Kongsfjorden", "Isfjorden","Storfjorden", 
                                                   "Young Sound", "Disko Bay", "Nuup Kangerlua", 
                                                   "Porsangerfjorden")),
                          lon = c(11.845, 14.365, 19.88, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.78, 74.517, 69.36, 64.405, 70.6))
# Full study area
rp_fig_1 <- basemap(limits = c(-60, 60, 60, 90), bathymetry = T) +
  annotation_spatial(bbox_EU_poly, fill = "darkgreen", colour = "black", alpha = 0.1) +
  geom_spatial_point(data = site_points, size = 9, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points, size = 8, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  # geom_spatial_segment(
  #   aes(x = 0, xend = 10, y = 60, yend = 65),
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   crs = 4326) +
  # geom_curve(
  #   aes(x = 10, xend = 20, y = 60, yend = 65),
  #   arrow = arrow(length = unit(0.03, "npc"))
  # ) +
  # labs(colour = "Site", x = NULL, y = NULL) +
  # labs(title = "FACE-IT study area and focal sites",
  #      colour = "Site",
  #      caption = "robert.schlegel@imev-mer.fr\nSorbonne Université") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = c(0.948, 0.29),
        # legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(10, 10, 10, 10), 
        legend.box.background = element_rect(fill = "white", colour = "black"))
rp_fig_1
ggsave("figures/rp_fig_1.png", rp_fig_1, height = 10, width = 16)


# Figure 2 ----------------------------------------------------------------
# Flow chart showing interactions of all key drivers

# If the network figure is used, the blog should be cited in the acknowledgements.
# https://kateto.net/network-visualization
# Ognyanova, K. (2021) Network visualization with R. Retrieved from www.kateto.net/network-visualization.

# Create network
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# print(net, e=TRUE, v=TRUE)

# Generate colors based on media type:
# Cryosphere can be either "mintcream" or "violet"
cat_colours <- c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")
trend_colours <- c("blue", "purple", "red")
V(net)$cat_colour <- cat_colours[V(net)$cat_num]
V(net)$trend_colour <- trend_colours[V(net)$trend_num]
# V(net)$rel_fac <- base::factor(x = V(net)$relationship, levels = c("positive", "negative", "both"))


## Heatmap
netm <- get.adjacency(net, attr = "to_num", sparse = F)
colnames(netm) <- V(net)$driver
rownames(netm) <- V(net)$driver

palf <- colorRampPalette(c("white", "blue", "purple", "red")) 
heatmap(netm, Rowv = NA, Colv = NA, col = palf(4), 
        symm = T, # Flip the axes so that x-axis relates the direction of effect
        scale = "none", margins = c(10,10), revC = T,
        ColSideColors = trend_colours[nodes$trend_num],
        RowSideColors = trend_colours[nodes$trend_num])

## Network charts
# Layout options:
# ‘star’, ‘circle’, ‘grid’, ‘sphere’, ‘kk’, ‘fr’, ‘mds’, ‘lgl’, 
# Consistent results: circle, star, kk, mds

# Linear network
ggraph(net, layout = "linear") + 
  geom_node_point(aes(fill = category), colour = V(net)$trend_colour, 
                  shape = 21, size = 8, stroke = 2) +
  geom_edge_arc(aes(color = relationship), 
                arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
  geom_node_label(aes(label = driver), size = 3, color = "black", repel = T, segment.colour = NA) +
  scale_edge_colour_manual("Trend/\nRelationship", values = c("purple", "blue", "red")) +
  theme_void() + theme(plot.background = element_rect(fill = "white", colour = "black"))
ggsave("~/Desktop/network_linear.png", height = 5, width = 6)

# Web network
ggraph(net, layout = "mds") + # kk OR mds
  geom_node_point(aes(fill = category), colour = V(net)$trend_colour, 
                  shape = 21, size = 8, stroke = 2) + # size by audience size  
  geom_edge_link(aes(color = relationship), 
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +   
  geom_node_label(aes(label = driver), size = 3, color = "black", repel = T, segment.colour = NA) +
  scale_edge_colour_manual("Trend/\nRelationship", values = c("purple", "blue", "red")) +
  theme_void() + theme(plot.background = element_rect(fill = "white", colour = "black"))
ggsave("~/Desktop/network_web.png", height = 5, width = 6)

# Circle network
ggraph(net,  layout = "circle") +
  geom_node_point(aes(fill = category), colour = V(net)$trend_colour, 
                  shape = 21, size = 8, stroke = 2) + # size by audience size  
  geom_edge_link(aes(colour = to_sign), show.legend = T,
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +     
  geom_node_label(aes(label = driver), size = 3, color = "black", repel = T, segment.colour = NA) +
  scale_edge_colour_manual("Trend/\nRelationship", values = c("purple", "blue", "red")) +
  theme_void() + theme(plot.background = element_rect(fill = "white", colour = "black"))
ggsave("~/Desktop/network_circle.png", height = 5, width = 6)

# Combine and save
ggsave("figures/rp_fig_2.png", rp_fig_2_plot, width = 10, height = 10)

### Interactive networks
# https://kateto.net/network-visualization

## visNetwork
library(visNetwork)

# Base example
visNetwork(nodes, edges, width="100%", height="400px")

# Prep for more complexity
vis.nodes <- nodes
vis.links <- edges

# Change dot aesthetics
vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$category # Text on click
vis.nodes$label  <- vis.nodes$driver # Node label
# vis.nodes$size   <- vis.nodes$audience.size # Node size
vis.nodes$borderWidth <- 2 # Node border width

# Change click aesthetics
vis.nodes$color.background <- c("violet", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")[nodes$cat_num]
vis.nodes$color.border <- c("blue", "purple", "red")[nodes$trend_num]
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- vis.nodes$color.border

# Change line aesthetics
# vis.links$width <- 1+links$weight/8 # line width
# vis.links$color <- "gray"    # line color  
vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow

# Assign to object
visnet <- visNetwork(vis.nodes, vis.links)

# Visualise
visnet

# More options
visOptions(visnet, highlightNearest = TRUE, selectedBy = "category")

## threejs
# NB: Must be visualised in a web browser, not RStudio
library(threejs)

# Set base
net.js <- net
graph_attr(net.js, "layout") <- NULL 

# Visualise
graphjs(net.js, main = "Arctic Fjords", bg = "gray10", showLabels = T, stroke = F, 
        curvature = 0.1, attraction = 0.9, repulsion = 0.8, opacity = 0.9)

## networkD3
library(networkD3)

# Convert node IDs
# This somehow gets messed up...
links.d3 <- data.frame(from=as.numeric(factor(edges$from))-1, 
                       to=as.numeric(factor(edges$to))-1 )
# nodes.d3 <- cbind(idn = factor(nodes$category, levels = unique(nodes$category)), nodes)
nodes.d3 <- cbind(idn = nodes$driver, nodes)

# Visualise
forceNetwork(Links = links.d3, Nodes = nodes.d3, Source = "from", Target = "to",
             NodeID = "idn", Group = "category", linkWidth = 1,
             linkColour = "#afafaf", fontSize = 12, zoom = T, legend = T,
             # Nodesize = 7, # Column number where sizes are stored
             opacity = 0.8, charge = -300,
             width = 500, height = 500)

# With arrows ... needs work
forceNetwork(Links = links.d3, Nodes = nodes.d3, Source = "from",
             Target = "to", 
             Value = "from",
             NodeID = "idn",
             Group = "category", opacity = 0.4, arrows = TRUE)

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


# Table 1 -----------------------------------------------------------------

# Create table of sources for each category/group
load("data/analyses/all_ref.RData") # NB: Must add data sources to this output

# Pivot wider to get var_type
all_ref_var_type <- all_ref %>% 
  distinct() %>% 
  mutate(var_type_count = 1) %>% 
  pivot_wider(id_cols = c(type, URL, citation), names_from = var_type, values_from = var_type_count, values_fn = mean)

# Combine
all_ref_wide <- all_ref %>% 
  left_join(all_ref_var_type, by = c("type", "URL", "citation")) %>% 
  dplyr::select(-var_type) %>% distinct() %>% 
  filter(type == "in situ") # Remove non in situ sources

# Get summaries by site
table_1_func <- function(site_name, site_long){
  # site_col <- colnames(all_ref_wide)[which(colnames(all_ref_wide) == site_name)]
  # df_sub <- all_ref_wide
  site_name <- enquo(site_name)
  df_ref <- all_ref_wide %>% 
    filter(!is.na(!!site_name)) %>% 
    dplyr::select(-type, -URL, -citation) %>% 
    summarise_all(sum, na.rm = T) %>% 
    mutate(Site = site_long) %>% 
    dplyr::select(Site, !!site_name, cryo:soc, PANGAEA, NPDC, GEM, NMDC, NSIDC:`Port of Longyearbyen`)
  colnames(df_ref)[2] <- "Total"
  return(df_ref)
}

# Final table
table_1_kong <- table_1_func(kong, "Kongsfjorden")
table_1_is <- table_1_func(is, "Isfjorden")
table_1_stor <- table_1_func(stor, "Storfjorden")
table_1_young <- table_1_func(young, "Young Sound")
table_1_disko <- table_1_func(disko, "Disko Bay")
table_1_nuup <- table_1_func(nuup, "Nuup Kangerlua")
table_1_por <- table_1_func(por, "Porsangerfjorden")
table_1 <- rbind(table_1_kong, table_1_is, table_1_stor, table_1_young, table_1_disko, table_1_nuup, table_1_por) %>% 
  mutate(Other = Reduce("+",.[12:23])) %>% 
  dplyr::select(Site:NMDC, Other)
write_csv(table_1, "data/analyses/table_1.csv")
knitr::kable(table_1)
table_1_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_1)) +
  theme_void()
ggsave("figures/table_1.png", table_1_plot, width = 6.2, height = 1.55)

# Some acronyms:
# "NSIDC" = National Snow & Ice Data Center
# "NMDC" = Norwegian Marine Data Centre
# "NMI" = Norwegian Meteorological Institute
# "NIRD" = National Infrastructure for Research Data

