# code/study_sites.R
# This script houses code used in managing study site info

# Notes on site coordinators contacted for bbox and transect coordinates
# Western Greenland: Nuup Kangerlua (Godthåbsfjord) (GF), Qeqertarsuup Tunua (Disko Bay) (DB): Thomas Juul-Pedersen
# Eastern Greenland: Young Sound (YS): Mikael Kristian Sejr
# Svalbard: Kongsfjorden (KF): Allison Bailey
# Svalbard: Isfjorden (IF), Inglefieldbukta (IB) and Agardhbukta: Janne Søreide
# Norway: Porsangerfjorden (PF), Finnmark: Lis Lindal Jørgensen


# Setup -------------------------------------------------------------------

library(tidyverse)


# Bounding boxes ----------------------------------------------------------

# European Arctic
bbox_EU <- data.frame(lon1 = -25,
                      lon2 = 60,
                      lat1 = 66,
                      lat2 = 90)

# Sites
bboxs <- data.frame(site = c("Kongsfjorden", "Isfjorden"),
                    lon1 = c(10.5, 12.95),
                    lon2 = c(13, 15.78),
                    lat1 = c(78.8, 78.04),
                    lat2 = c(79.1, 78.43))


# Transects ---------------------------------------------------------------

trnscts <- data.frame(site = c("Kongsfjorden", "Isfjorden"),
                      lon1 = c(11, 15.1),
                      lon2 = c(12.4, 13.39),
                      lat1 = c(79.05, 78.32),
                      lat2 = c(78.9, 78.13))


# Map ---------------------------------------------------------------------

# Svalbard study sites
ggplot(data = bboxs) +
  borders(fill = "grey80", colour = "black") +
  geom_rect(aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site), alpha = 0.1) +
  geom_segment(data = trnscts, aes(x = lon1, xend = lon2, y = lat1, yend = lat2, colour = site)) +
  coord_quickmap(xlim = c(9, 30), ylim = c(76, 81)) +
  # coord_quickmap(xlim = c(bbox_EU$lon1, bbox_EU$lon2), ylim = c(bbox_EU$lat1, bbox_EU$lat2)) +
  # coord_quickmap(xlim = c(bboxs$lon1, bboxs$lon2), ylim = c(bboxs$lat1, bboxs$lat2)) +
  # facet_wrap(~site) +
  labs(x = NULL, y = NULL)

# Finland study sites

# Greenland study sites

# Full study area
ggplot() +
  borders(fill = "grey80", colour = "black") +
  geom_rect(data = bbox_EU, fill = NA, colour = "black",
            aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2)) +
  geom_rect(data = bboxs,
            aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                fill = site, colour = site)) +
  coord_quickmap(xlim = c(-60, 60), ylim = c(58, 90)) +
  labs(x = NULL, y = NULL)

