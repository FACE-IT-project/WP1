# code/review_paper.R
# The code used for the tables and figures in the WP1 review paper (D1.3)


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(geomtextpath)

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

df <- data.frame(x1 = c(seq(0, 10/6 * pi, pi/3),
                        seq(0, 10/6 * pi, 2*pi/3)),
                 y1 = c(rep(2, 6), rep(-1, 3)),
                 x2 = c(seq(0, 10/6 * pi, pi/3)  + pi/3,
                        seq(0, 10/6 * pi, 2*pi/3) + 2*pi/3),
                 y2 = c(rep(4, 6), rep(2, 3)),
                 group = letters[c(1:6, (1:3) * 2)],
                 alpha = c(rep(1, 6), rep(0.4, 3)))

p <- ggplot(df, aes(x1, y1)) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group,
                alpha = alpha),
            color = "white", size = 2) +
  geom_textpath(data = data.frame(x1 = seq(0, 2 * pi, length = 300),
                                  y1 = rep(0.5, 300),
                                  label = rep(c("stats", "effects", "polar"), each = 100)),
                aes(label = label), linetype = 0, size = 8,
                upright = TRUE) +
  geom_textpath(data = data.frame(x1 = seq(0, 2 * pi, length = 300),
                                  y1 = rep(3, 300),
                                  label = rep(c("density", "smooth", "unique", "organic",
                                                "easy to use", "automatic"), 
                                              each = 50)),
                aes(label = label), linetype = 0, size = 4.6, color = "white",
                upright = TRUE) +
  scale_y_continuous(limits = c(-5, 4)) +
  scale_x_continuous(limits = c(0, 2*pi)) +
  scale_fill_manual(values = c("deepskyblue3", "deepskyblue4",
                               "green3", "green4","tomato", "tomato2")) +
  scale_alpha_identity() +
  theme_void() +
  theme(legend.position = "none") 
p


# Combine
ggsave("figures/rp_fig_2.png", rp_fig_2, width = 15, height = 15)


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

