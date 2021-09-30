# talks/FACE_IT_2021.R
# This script contains the code used for the analyses shown in 
# the talk given for the FACE-IT 2021 annual meeting


# Setup -------------------------------------------------------------------

source("code/functions.R")

# Load PANGAEA driver metadata sheet
pg_parameters <- read_tsv("metadata/pangaea_parameters.tab") %>% 
  mutate(var_name = paste0(Abbreviation," [",Unit,"]"), .keep = "unused") %>% 
  dplyr::select(-`ID parameter`)


# Data --------------------------------------------------------------------

# Svalbard data
load("data/full_data/full_product_sval.RData")

# Isfjorden data
load("data/full_data/full_product_is.RData")

# Isfjorden nutrient data
# unique(is_nutrient$var_name)
is_nutrient <- full_product_is %>% 
  filter(var_type == "chem",
         !is.na(date),
         var_name %in% c("[NO3]- [µmol/l]", "[PO4]3- [µmol/l]", "Si(OH)4 [µmol/l]",
                         "PO4 [µg-at/l]", "[NO2]- [µg-at/l]", "NO3 [µg-at/l]", 
                         "PO4 biog [%]", "CaCO3 [%]")) %>% 
  mutate(year = lubridate::year(date), .keep = "unused") %>% 
  dplyr::select(lon:year, -var_type) %>% 
  group_by(lon, lat, year, depth, var_name) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Isfjorden ice data
# unique(is_cryo$var_name)
# unique(is_cryo$Parameter)
is_cryo <- full_product_is %>% 
  filter(var_type == "cryo",
         !is.na(date)) %>% 
         # var_name == "Ice extent") # NB: This is not useful
  left_join(pg_parameters, by = "var_name") %>% 
  mutate(Parameter = ifelse(is.na(Parameter), var_name, Parameter),
         year = lubridate::year(date))

# Isfjorden model data
model_is <- load_model("isfjorden_rcp")

# Svalbard tourist info
sval_soc <- full_product_sval %>% 
  filter(var_type == "soc")


# Analyses ----------------------------------------------------------------

# Population change over time
plot_pop <- sval_soc %>% 
  filter(grepl("pop", var_name)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_bar(aes(fill = var_name), stat = "identity",
           position = position_stack(reverse = TRUE)) +
  theme(legend.position = "bottom")
plot_pop
ggsave("docs/assets/plot_pop.png", plot_pop)

# Tourist arrival change over time
plot_arrival <- sval_soc %>% 
  filter(grepl("arrival", var_name)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_bar(aes(fill = var_name), stat = "identity",
           position = position_stack(reverse = TRUE)) +
  theme(legend.position = "bottom")
plot_arrival
ggsave("docs/assets/plot_arrival.png", plot_arrival)

# Guest night change over time
plot_guest <- sval_soc %>% 
  filter(grepl("guest", var_name)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_bar(aes(fill = var_name), stat = "identity",
           position = position_stack(reverse = TRUE)) +
  theme(legend.position = "bottom")
plot_guest
ggsave("docs/assets/plot_guest.png", plot_guest)

# Plot population and tourist arrivals side-by-side


# Plot population nights and guest nights side-by-side


# Nutrient change over time
plot_nutrient <- ggplot(data = is_nutrient, aes(x = year, y = value)) +
  geom_point(aes(colour = depth)) +
  facet_wrap(~var_name, scales = "free_y") +
  theme(legend.position = "bottom")
ggsave("docs/assets/plot_nutrient.png", plot_nutrient)

# Cryosphere change over time
plot_cryo <- ggplot(data = is_cryo, aes(x = year, y = value)) +
  geom_point(aes(colour = depth)) +
  facet_wrap(~Parameter, scales = "free_y") +
  theme(legend.position = "bottom")
ggsave("docs/assets/plot_cryo.png", plot_cryo)

# Show what the relationship has been between ship mileage and temperature or ice change
# Then show what the different model RCP projections are and what the future may hold
# Also use these relationship projections for any sort of biomass

# TODO:
# Create statistics about how many data files there are. 
# Who has been contacted. 
# How many we have and what has been added
# Solicit audience for new datasets that have not been identified and that are available
# Read the Kongsfjorden ecosystem book chapter about mooring data
# Have a slide at the end of the science talk about co-authorship for the review article
  # It requires contribution of data, text, figures, ideas etc.
  # The obvious contributors are the site coordinators and who they think are specialists in certain aspects at there site
# Think of a timeline for the review article


# Inglefieldbukta bbox figure ---------------------------------------------

# Expanded bbox
bbox_ingle_plus <- c(17.35, 21.60, 77.43, 78.13)

## Inglefieldbukta expanded bbox figures
coastline_ingle_plus <- coastline_full_df %>% 
  filter(x >= bbox_ingle_plus[1]-10, x <= bbox_ingle_plus[2]+10, 
         y >= bbox_ingle_plus[3]-10, y <= bbox_ingle_plus[4]+10)
plot_ingle_plus <- ggplot() +
  geom_polygon(data = coastline_ingle_plus, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  annotate("rect", colour = "orange", fill = "orange", alpha = 0.1,
           xmin = bbox_ingle_plus[1], xmax = bbox_ingle_plus[2], ymin = bbox_ingle_plus[3], ymax = bbox_ingle_plus[4]) +
  annotate("rect",  colour = "darkgreen", fill = "darkgreen", alpha = 0.1,
           xmin = bbox_ingle[1], xmax = bbox_ingle[2], ymin = bbox_ingle[3], ymax = bbox_ingle[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_ingle_plus[1]-3, bbox_ingle_plus[2]+3), 
                 ylim = c(bbox_ingle_plus[3]-0.5, bbox_ingle_plus[4]+0.5)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Proposed expansion of Inglefieldbukta study site")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_ingle_plus
ggsave("docs/assets/plot_ingle_plus.png", plot_ingle_plus, height = 4)


# Figures -----------------------------------------------------------------

## Kongsfjorden data summary figure
# Load full product
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
# Summary plot
kong_anal_fig <- data_summary_plot(full_product_kong, "Kongsfjorden")
# ggsave("docs/assets/kong_fig_1.png", kong_anal_fig, width = 8, height = 8) # Not needed


## Kongsfjorden data summary figure
# Load full product
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
# Summary plot
is_anal_fig <- data_summary_plot(full_product_is, "Isfjorden")
ggsave("docs/assets/is_fig_1.png", is_anal_fig, width = 8, height = 8)

