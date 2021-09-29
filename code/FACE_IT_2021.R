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

# Isfjorden data
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")

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

# Population counts
sval_pop <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/svalbard_population_stats.csv", delim = "\t") %>% 
  dplyr::select(-contents) %>% 
  rename_at(.vars = vars(ends_with("H1")), .funs = list(~gsub("H1", "", .))) %>% 
  # filter(settlement != "Hornsund")
  pivot_longer(`1990`:`2021`, names_to = "year", values_to = "pop") %>% 
  mutate(year = as.numeric(year),
         settlement = case_when(grepl("Resident", settlement) ~ "Longyearbyen & Ny-Alesund mainland",
                                grepl("abroad", settlement) ~ "Longyearbyen & Ny-Alesund abroad",
                                TRUE ~ settlement))


# Svalbard tourist arrivals
sval_tour <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/svalbard_tourist_arrivals.csv", delim = "\t")


# Analyses ----------------------------------------------------------------

# Population change over time
ggplot(data = sval_pop, aes(x = year, y = pop)) +
  geom_bar(aes(fill = settlement), stat = "identity",
           position = position_stack(reverse = TRUE), width = 1)

# Nutrient change over time
ggplot(data = is_nutrient, aes(x = year, y = value)) +
  geom_point(aes(colour = depth)) +
  facet_wrap(~var_name, scales = "free_y")

# Cryosphere change over time
ggplot(data = is_cryo, aes(x = year, y = value)) +
  geom_point(aes(colour = depth)) +
  facet_wrap(~Parameter, scales = "free_y")

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


# Figures -----------------------------------------------------------------

## Inglefieldbukta expanded bbox figures
coastline_ingle <- coastline_full_df %>% 
  filter(x >= bbox_ingle[1]-1, x <= bbox_ingle[2]+1, y >= bbox_ingle[3]-1, y <= bbox_ingle[4]+1)
plot_problems_ingle <- full_product_ingle %>%
  mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_ingle, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect",  colour = "green", fill = NA, alpha = 0.1,
           xmin = bbox_ingle[1], xmax = bbox_ingle[2], ymin = bbox_ingle[3], ymax = bbox_ingle[4]+0.03) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_ingle[1], xmax = bbox_ingle[2], ymin = bbox_ingle[3], ymax = bbox_ingle[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_ingle[1]-0.5, bbox_ingle[2]+0.4), 
                 ylim = c(bbox_ingle[3]-0.1, bbox_ingle[4]+0.2)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.01° (~1 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_ingle
ggsave("figures/bbox_ingle.png", plot_problems_ingle, height = 6)

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

