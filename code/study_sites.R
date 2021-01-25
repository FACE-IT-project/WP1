# code/study_sites.R
# This script houses code used in managing study site info

# Notes on site coordinators contacted for bbox and transect coordinates
# Western Greenland: Nuup Kangerlua (Godthåbsfjord) (GF), Qeqertarsuup Tunua (Disko Bay) (DB): Thomas Juul-Pedersen
# Eastern Greenland: Young Sound (YS): Mikael Kristian Sejr
# Svalbard: Kongsfjorden (KF): Allison Bailey
# Svalbard: Isfjorden (IF), Inglefieldbukta (IB) and Agardhbukta: Janne Søreide
# Norway: Porsangerfjorden (PF), Finnmark: Lis Lindal Jørgensen

# Search the literature for the necessary bounding boxes and transects
# Isfjorden nneds to be extended to the east
# Add the bathymetry data from JP to the young sound bounding box
# IBCAO for bathy data
# Add bathymetry for all of the sites


# Setup -------------------------------------------------------------------

# Start with common project code
source("code/functions.R")


# Bounding boxes ----------------------------------------------------------

# European Arctic
bbox_EU <- data.frame(lon1 = -25,
                      lon2 = 60,
                      lat1 = 66,
                      lat2 = 90)

# Svalbard
bbox_sval <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta"),
                        lon1 = c(11, 12.95, 18.04),
                        lon2 = c(12.69, 15.78, 18.58),
                        lat1 = c(78.86, 78.04, 77.92),
                        lat2 = c(79.1, 78.43, 77.82))

# Finland
bbox_fin <- data.frame(site = c("Porsangerfjorden"),
                       lon1 = c(24.5),
                       lon2 = c(27),
                       lat1 = c(70),
                       lat2 = c(71.2))

# Eastern Greenland
bbox_east <- data.frame(site = c("Young Sound"),
                        lon1 = c(-21),
                        lon2 = c(-16),
                        lat1 = c(75.58),
                        lat2 = c(74.43))

# Western Greenland
bbox_west <- data.frame(site = c("Disko Bay"),
                        lon1 = c(-55.56),
                        lon2 = c(-49.55),
                        lat1 = c(70.5),
                        lat2 = c(68.22))

# Transects ---------------------------------------------------------------

# Svalbard
trnsct_sval <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta"),
                          lon1 = c(12.440833, 15.1, 18.19),
                          lon2 = c(11.139333, 13.39, 18.47),
                          lat1 = c(78.89650, 78.32, 77.89),
                          lat2 = c( 79.04633, 78.13, 77.91))

# Finland
trnsct_fin <- data.frame(site = c("Porsangerfjorden"),
                         lat1 = c(69.7),
                         lat2 = c(71.5),
                         lon1 = c(24.5),
                         lon2 = c(27))

# East Greenland
trnsct_east <- data.frame(site = c("Young Sound"),
                          lat1 = c(75.19),
                          lat2 = c(74.76),
                          lon1 = c(-20.37),
                          lon2 = c(-18.23))

# East Greenland
trnsct_west <- data.frame(site = c("Disko Bay"),
                          lat1 = c(69.15),
                          lat2 = c(68.89),
                          lon1 = c(-51.49),
                          lon2 = c(-54.15))


# Map ---------------------------------------------------------------------

# Svalbard study sites
map_sval <- ggplot(data = bbox_sval) +
  borders(fill = "grey80", colour = "black") +
  geom_rect(aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site), alpha = 0.1) +
  geom_segment(data = trnsct_sval, aes(x = lon1, xend = lon2, y = lat1, yend = lat2, colour = site)) +
  coord_quickmap(xlim = c(9, 30), ylim = c(76, 81)) +
  # coord_quickmap(xlim = c(bbox_EU$lon1, bbox_EU$lon2), ylim = c(bbox_EU$lat1, bbox_EU$lat2)) +
  # coord_quickmap(xlim = c(bboxs$lon1, bboxs$lon2), ylim = c(bboxs$lat1, bboxs$lat2)) +
  # facet_wrap(~site) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")

# Finland study sites
map_fin <- ggplot(data = bbox_fin) +
  borders(fill = "grey80", colour = "black") +
  geom_rect(aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site), alpha = 0.1) +
  geom_segment(data = trnsct_fin, aes(x = lon1, xend = lon2, y = lat1, yend = lat2, colour = site)) +
  coord_quickmap(xlim = c(21, 31), ylim = c(69, 72)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")

# Eastern Greenland study sites
map_east <- ggplot(data = bbox_east) +
  borders(fill = "grey80", colour = "black") +
  geom_rect(aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site), alpha = 0.1) +
  geom_segment(data = trnsct_east, aes(x = lon1, xend = lon2, y = lat1, yend = lat2, colour = site)) +
  coord_quickmap(xlim = c(-24, -15), ylim = c(73.5, 76)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")

# Western Greenland study sites
map_west <- ggplot(data = bbox_west) +
  borders(fill = "grey80", colour = "black") +
  geom_rect(aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site), alpha = 0.1) +
  geom_segment(data = trnsct_west, aes(x = lon1, xend = lon2, y = lat1, yend = lat2, colour = site)) +
  coord_quickmap(xlim = c(-56, -49), ylim = c(60, 72)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")

# Full study area
map_full <- ggplot() +
  borders(fill = "grey80", colour = "black") +
  geom_rect(data = bbox_EU, fill = NA, colour = "black",
            aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2)) +
  geom_rect(data = bbox_sval,
            aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site)) +
  geom_rect(data = bbox_fin,
            aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site)) +
  geom_rect(data = bbox_east,
            aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site)) +
  geom_rect(data = bbox_west,
            aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site)) +
  coord_quickmap(xlim = c(-60, 60), ylim = c(58, 90), expand = F) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")

# Assemble smaller figures
map_small <- ggpubr::ggarrange(map_sval, map_fin, map_east, map_west, ncol = 2, nrow = 2)

# Put them together
map_all <- ggpubr::ggarrange(map_full, map_small, ncol = 2)
map_all
