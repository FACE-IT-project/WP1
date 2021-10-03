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
load("~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.RData")
# load("data/full_data/full_product_sval.RData")

# Isfjorden data
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
# load("data/full_data/full_product_is.RData")

# Isfjorden ChlA data
is_ChlA <- full_product_is %>% 
  filter(grepl("Chl", var_name))
is_ChlA_annual <- is_ChlA %>% 
  filter(!grepl("GFF", var_name),
         depth <= 30) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  dplyr::rename(ChlA = value)

# Isfjorden nutrient data
# unique(is_nutrient$var_name)
is_nutrient <- full_product_is %>% 
  filter(var_type == "chem",
         !is.na(date),
         var_name %in% c("[NO3]- [µmol/l]", "[PO4]3- [µmol/l]", "Si(OH)4 [µmol/l]",
                         "PO4 [µg-at/l]", "[NO2]- [µg-at/l]", "NO3 [µg-at/l]", 
                         "PO4 biog [%]")) %>% 
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

# Svalbard population
sval_pop <- sval_soc %>% 
  filter(grepl("pop", var_name),
         !grepl("Hornsund", var_name)) %>%
  mutate(var_name = gsub("pop ", "", var_name)) %>% 
  mutate(var_name = sub("\\[", "", var_name)) %>% 
  mutate(var_name = sub("\\]", "", var_name)) %>% 
  mutate(var_name = case_when(grepl("Longyear", var_name) ~ "Longyearbyen and Ny-Alesund", 
                              TRUE ~ var_name))


# Svalbard tourist arrivals
sval_arrival <- sval_soc %>% 
  filter(grepl("arrival", var_name)) %>%
  mutate(var_name = "Tourists")

# Svalbard guest nights
sval_guest <- sval_soc %>% 
  filter(grepl("guest", var_name)) %>% 
  mutate(var_name = "Tourists")
sval_nights_annual <- sval_pop %>%
  filter(!grepl("Pyr", var_name)) %>% 
  mutate(var_name = "Residents",
         value = value*365) %>% 
  rbind(sval_guest) %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year >= 2011) %>% 
  group_by(year) %>% 
  summarise(nights = sum(value))


# Analyses ----------------------------------------------------------------

# Set colour palette for consistent colours between figures
pop_palette <- RColorBrewer::brewer.pal(6, "Set1")

# Population change over time
plot_pop <- ggplot(data = sval_pop, aes(x = date, y = value)) +
  geom_bar(aes(fill = var_name), stat = "identity",
           position = position_stack(reverse = TRUE)) +
  labs(y = "Population", x = NULL, fill = "Location") +
  scale_fill_manual(values = pop_palette[1:2]) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_pop
ggsave("docs/assets/plot_pop.png", plot_pop, width = 6)

# Tourist arrival change over time
plot_arrival <- sval_pop %>% 
  filter(!grepl("Pyr", var_name),
         date >= as.Date("2008-01-01")) %>% 
  mutate(var_name = "Residents") %>%
  rbind(sval_arrival) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_bar(aes(fill = var_name), stat = "identity",
           position = "dodge") +
  scale_fill_manual(values = pop_palette[2:3]) +
  labs(y = "Annual population", x = NULL, fill = "Longyearbyen") +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_arrival
ggsave("docs/assets/plot_arrival.png", plot_arrival, width = 6)

# Guest night change over time
plot_guest <- sval_pop %>%
  filter(!grepl("Pyr", var_name),
         date >= as.Date("2008-01-01")) %>% 
  mutate(var_name = "Residents",
         value = value*365) %>% 
  rbind(sval_guest) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_bar(aes(fill = var_name), stat = "identity",
           position = "dodge") +
  scale_fill_manual(values = pop_palette[2:3]) +
  labs(y = "Annual nights of stay", x = NULL, fill = "Longyearbyen") +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_guest
ggsave("docs/assets/plot_guest.png", plot_guest, width = 6)

# Nutrient change over time
plot_nutrient <- is_nutrient %>% 
  group_by(var_name, year) %>%
  summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = count)) +
  geom_point(aes(colour = var_name), position = position_dodge(width = 3), size = 3) +
  # facet_wrap(~var_name, scales = "free_y") +
  labs(y = "Data points", x = NULL, colour = "Nutrients") +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_nutrient
ggsave("docs/assets/plot_nutrient.png", plot_nutrient, width = 6)

# Plot ChlA change over time
plot_ChlA <- is_ChlA %>% 
  filter(!grepl("GFF", var_name)) %>% 
  ggplot(aes(x = date, y = depth)) +
  geom_point(aes(colour = log10(value))) +
  scale_y_reverse() +
  scale_colour_distiller(palette = "Greens", direction = 1) +
  labs(x = NULL, y = "Depth (m)", colour = "log10[ChlA] (ug/l)") +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_ChlA
ggsave("docs/assets/plot_ChlA.png", plot_ChlA, width = 6)

# ChlA with human stays
cor_ChlA_pop <- cor.test(x = is_ChlA_annual$ChlA, y = sval_nights_annual$nights[1:9])
plot_ChlA_pop <- left_join(is_ChlA_annual, sval_nights_annual, by = "year") %>% 
  ggplot(aes(x = nights, y = ChlA)) +
  geom_point(aes(colour = as.factor(year))) +
  geom_smooth(method = "lm", colour = "black") +
  annotate(geom = "label", x = 890000, y = 0, 
           label = paste0("r = ",round(cor_ChlA_pop$estimate, 2),"; p = ",round(cor_ChlA_pop$p.value,2),"; df = ",cor_ChlA_pop$parameter)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(colour = "Year", y = "ChlA (ug/l)", x = "Nights of stay",
       subtitle = "Average ChlA in the top 30 m vs. sum of nights of human habitation") +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_ChlA_pop
ggsave("docs/assets/plot_ChlA_pop.png", plot_ChlA_pop, width = 6)

# Cryosphere change over time
plot_cryo <- ggplot(data = is_cryo, aes(x = year, y = value)) +
  geom_point(aes(colour = depth)) +
  facet_wrap(~Parameter, scales = "free_y") +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_cryo
ggsave("docs/assets/plot_cryo.png", plot_cryo)

# Show what the relationship has been between ship mileage and temperature or ice change
# Then show what the different model RCP projections are and what the future may hold
# Also use these relationship projections for any sort of biomass


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

