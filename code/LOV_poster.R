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

# Create network
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Generate colors based on category:
cat_colours <- c("violet", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")
trend_colours <- c("blue", "purple", "red")
V(net)$cat_colour <- cat_colours[V(net)$cat_num]
V(net)$trend_colour <- trend_colours[V(net)$trend_num]

# Circle network
panel_a <- ggraph(net, layout = "circle") +
  geom_edge_fan(aes(colour = to_sign), width = 2, alpha = 0.8,
                 arrow = arrow(length = unit(0.025, "npc"), type = "closed")) +
  geom_node_point(aes(fill = category, colour = trend),
                  shape = 21, size = 16, stroke = 3) +
  geom_node_label(aes(label = driver), size = 4, color = "black", alpha = 0.8,
                  label.padding = unit(0.3, "lines"), label.size = unit(0.7, "lines")) +
  scale_edge_colour_manual("Trend/\nImpact", 
                           breaks = c("increase", "decrease", "complex"),
                           values = c("purple", "blue", "red")) +
  scale_colour_manual("Trend/\nImpact",
                      breaks = c("increase", "decrease", "complex"),
                      values = c("red", "blue", "purple")) +
  scale_fill_manual("Category", 
                    breaks = c("cryosphere", "physics", "chemistry", "biology", "social"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11))
panel_a
ggsave("~/Desktop/panel_a.png", width = 12, height = 9)


# Panel B -----------------------------------------------------------------



# Final -------------------------------------------------------------------


