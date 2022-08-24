# code/review_paper.R
# The code used for the tables and figures in the WP1 review paper (D1.3)


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(geomtextpath)
library(ggpmisc)
library(ggpattern)

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

# Create base for layering plots
base_df <- data.frame(x = c(-1, 0, 1), y = c(-1, 0, 1))
rp_fig_2_base <- ggplot(data = base_df, aes(x = x, y = y)) + 
  geom_point(colour = "grey50") + 
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1)) +
  coord_equal(expand = F) +
  # theme_void() +
  theme(panel.background = element_rect(fill = "grey50"))
rp_fig_2_base

# Coordinate data.frames for rectangles to be drawn on polar plot
coords_CO2 <- data.frame(x1 = 0,
                         y1 = 3.5,
                         x2 = 14,
                         y2 = 5,
                         group = letters[6],
                         alpha = 0.4)
# coords_temp_pH <- data.frame() # Not adding this at the moment
coords_drivers <- data.frame(x1 = seq(0, 13),
                             y1 = rep(1.5, 14),
                             x2 = seq(0, 13)+1,
                             y2 = rep(3, 14),
                             group = letters[c(rep(1, 3), rep(2, 3), rep(3, 2), rep(4, 3), rep(5, 3))],
                             alpha = rep(0.9, 14))

rp_fig_2_rim <- ggplot(coords_CO2, aes(x = x1, y = y1)) +
  # Rectangles
  geom_rect_pattern(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, alpha = alpha), 
                    size = 2, colour = NA, pattern = "circle", pattern_colour = "yellow",
                    pattern_density = 0.3) +
  # geom_rect(data = coords_temp_pH,
  #           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group, alpha = alpha), 
  #           size = 2, colour = "black") +
  geom_rect(data = coords_drivers,
            aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group, alpha = alpha), 
            size = 2, colour = "grey50") +
  # Text
  geom_textpath(data = data.frame(x1 = seq(0, 14, length = 700),
                                  y1 = rep(4.25, 700),
                                  label = "anthropogenic CO2 emissions"),
                aes(label = label), linetype = 0, size = 5.6, color = "black",
                upright = TRUE) +
  geom_textpath(data = data.frame(x1 = seq(0, 14, length = 700),
                                  y1 = rep(2.25, 700),
                                  label = rep(c("sea ice", "glacier mass\nbalance", "glacial + riverine\ndischarge",
                                                "sea water\ntemperature", "salinity", "light",
                                                "carbonate\nsystem", "nutrients",
                                                "primary\nproduction", "biomass", "species\nrichness",
                                                "governance", "tourism", "fisheries"),
                                              each = 50)),
                aes(label = label), linetype = 0, linewidth = 3.6, color = "black",
                upright = TRUE) +
  scale_y_continuous(limits = c(-5, 5)) +
  scale_x_continuous(limits = c(0, 14)) +
  # scale_fill_manual(aesthetics = c("colour", "fill"), 
  scale_fill_manual(values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080", "grey40")) +
  # scale_alpha_identity() +
  theme_void() +
  theme(legend.position = "none") + 
  coord_polar()
rp_fig_2_rim

# Standard arrow function
gg_arrow <- function(x, xend, y, yend, colour1, colour2, size1 = 2, size2 = 0.5, ...){
  df = data.frame(z = 1) # Dummy data.frame to break free from base plot
  return(list(geom_curve(data = df, aes(x = x, xend = xend, y = y, yend = yend),
                           size = size1, colour = colour1,
                           lineend = "round", arrow = arrow(length = unit(0.03, "npc")), ...),
              geom_curve(data = df, aes(x = x, xend = xend, y = y, yend = yend),
                           size = size2, colour = colour2,
                           lineend = "round", arrow = arrow(length = unit(0.03, "npc")), ...)))

}

# Add rim figure to base plot to remove polar coordinate system
rp_fig_2_plot <- rp_fig_2_base +
  ## Rim plot
  geom_grob(aes(x = 0, y = 0, label = list(cowplot::as_grob(rp_fig_2_rim))), vp.width = 1.23, vp.height = 1.23, add.segments = F) +
  
  ## emissions arrows
  # geom_segment()
  
  ## Cryosphere arrows
  # sea ice arrows
  gg_arrow(0.15, 0.4, 0.62, -0.5, "black", "mintcream") +
  # GMB arrows
  # discharge arrows
  
  ## Physics arrows
  
  ## Chemistry arrows
  
  ## Biology arrows
  
  ## Social arrows
  
  ## AW inflow
  geom_point(data = data.frame(x = 0.3, y = -0.1), aes(x, y), shape = 21, size = 20, colour = "grey50", fill = "skyblue") +
  annotate("text", x = 0.3, y = -0.1, label = "AW\ninflow") +
  # geom_label(data = data.frame(x = 0.3, y = -0.1), aes(x, y, label = "AW\ninflow"), 
  #            size = 6, colour = "black", fill = "skyblue") +
  # Other
  labs(x = NULL, y = NULL) #+ theme_void()
rp_fig_2_plot

# Add legends

# Combine and save

ggsave("figures/rp_fig_2.png", rp_fig_2_plot, width = 10, height = 10)


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

