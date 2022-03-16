# code/review_paper.R
# The code used for the analyses in the WP1 review paper (D1.3)


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Data --------------------------------------------------------------------

# FACE-IT collected data
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")

# Model data
model_kong <- load_model("kongsfjorden_rcp")
model_is <- load_model("isfjorden_rcp")
model_stor <- load_model("storfjorden_rcp")
model_young <- load_model("young_sound_rcp")
model_por <- load_model("porsangerfjorden_rcp")
model_trom <- load_model("tromso_rcp")

# NOAA OISST extractions
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_kong.RData")
sst_kong_bbox <- filter(sst_kong, between(lon, bbox_kong[1], bbox_kong[2]), between(lat, bbox_kong[3], bbox_kong[4]))
load("~/pCloudDrive/FACE-IT_data/isfjorden/sst_is.RData")
sst_is_bbox <- filter(sst_is, between(lon, bbox_is[1], bbox_is[2]), between(lat, bbox_is[3], bbox_is[4]))
load("~/pCloudDrive/FACE-IT_data/storfjorden/sst_stor.RData")
sst_stor_bbox <- filter(sst_stor, between(lon, bbox_stor[1], bbox_stor[2]), between(lat, bbox_stor[3], bbox_stor[4]))
load("~/pCloudDrive/FACE-IT_data/young_sound/sst_young.RData")
sst_young_bbox <- filter(sst_young, between(lon, bbox_young[1], bbox_young[2]), between(lat, bbox_young[3]-0.25, bbox_young[4])) # Mouth only
load("~/pCloudDrive/FACE-IT_data/disko_bay/sst_disko.RData")
sst_disko_bbox <- filter(sst_disko, between(lon, bbox_disko[1], bbox_disko[2]), between(lat, bbox_disko[3], bbox_disko[4]))
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/sst_nuup.RData")
sst_nuup_bbox <- filter(sst_nuup, between(lon, bbox_nuup[1], bbox_nuup[2]), between(lat, bbox_nuup[3], bbox_nuup[4]))
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/sst_por.RData")
sst_por_bbox <- filter(sst_por, between(lon, bbox_por[1], bbox_por[2]), between(lat, bbox_por[3], bbox_por[4]))

# CCI SST extractions
## NB: Load one-by-one, not all in one go
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_CCI_kong.RData")
sst_CCI_kong_bbox <- filter(sst_CCI_kong, between(lon, bbox_kong[1], bbox_kong[2]), between(lat, bbox_kong[3], bbox_kong[4]))
load("~/pCloudDrive/FACE-IT_data/isfjorden/sst_CCI_is.RData")
sst_CCI_is_bbox <- filter(sst_CCI_is, between(lon, bbox_is[1], bbox_is[2]), between(lat, bbox_is[3], bbox_is[4]))
load("~/pCloudDrive/FACE-IT_data/storfjorden/sst_CCI_stor.RData")
sst_CCI_stor_bbox <- filter(sst_CCI_stor, between(lon, bbox_stor[1], bbox_stor[2]), between(lat, bbox_stor[3], bbox_stor[4]))
load("~/pCloudDrive/FACE-IT_data/young_sound/sst_CCI_young.RData")
sst_CCI_young_bbox <- filter(sst_CCI_young, between(lon, bbox_young[1], bbox_young[2]), between(lat, bbox_young[3], bbox_young[4]))
load("~/pCloudDrive/FACE-IT_data/disko_bay/sst_CCI_disko.RData")
sst_CCI_disko_bbox <- filter(sst_CCI_disko, between(lon, bbox_disko[1], bbox_disko[2]), between(lat, bbox_disko[3], bbox_disko[4]))
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/sst_CCI_nuup.RData")
sst_CCI_nuup_bbox <- filter(sst_CCI_nuup, between(lon, bbox_nuup[1], bbox_nuup[2]), between(lat, bbox_nuup[3], bbox_nuup[4]))
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/sst_CCI_por.RData")
sst_CCI_por_bbox <- filter(sst_CCI_por, between(lon, bbox_por[1], bbox_por[2]), between(lat, bbox_por[3], bbox_por[4]))

# Comparisons of SST pixels in/out of the site bbox
ggplot(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
  geom_tile(colour = "red") +
  # geom_raster(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
  geom_rect(aes(xmin = bbox_young[1], xmax = bbox_young[2], 
                ymin = bbox_young[3], ymax = bbox_young[4]))

# Section 2 ---------------------------------------------------------------
# Mostly annual and monthly mean states of key drivers
# Alongfjord gradients is about as complex as we want to get
# Limit to the top 10 metres. Consider the bottom 10 metres.
# Line plots comparing averages values across sites
# Solid colour for in situ data, dashed line for NOAA, dotted for CCI


## Temperature -------------------------------------------------------------

# Kongsfjorden
unique(full_product_kong$var_name)
kong_SST <- full_product_kong %>% 
  # Filter out the top ten metres
  # NB: All air temperatures should have depth = NA, not depth = 0
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("temp|°C", var_name, ignore.case = T)) %>% 
  # Remove air, CO2, and pH related temperature values
  # TTT is air temperature from cruise data on PANGAEA. e.g. https://doi.pangaea.de/10.1594/PANGAEA.326679
  # MAAT + MAGT = ground temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.808512
  # MAT = mean annual temperature e.g. https://doi.pangaea.de/10.1594/PANGAEA.907818
  filter(!grepl("air|co2|ph_|pHint_|TTT|MAAT|MAGT|MAT", var_name, ignore.case = T)) %>% 
  # Remove overly processed variables (e.g. average summer SST)
  filter(!grepl("SST sum|SST win|Temp min|Temp max|Temp interp", var_name, ignore.case = T)) %>% 
  # Remove slightly different variables
  # tequ = temperature at equilibrium; ~+0.6°C than the corresponding water temp
    # e.g. https://doi.pangaea.de/10.1594/PANGAEA.849863
  # T intern [°C] = internal temperature; ~+0.03°C than the corresponding water temp
    # e.g. https://doi.pangaea.de/10.1594/PANGAEA.930028
  # Removing tpot (Potential temperature) is a potentially controversial decision...
  filter(!grepl("tequ|tpot|T intern", var_name, ignore.case = T)) %>% 
  # Temporarily remove Ferry box data until units are assigned to temperature values
  filter(!grepl("temp_", var_name, ignore.case = T)) %>% 
  mutate(site = "Kong")
# Check final variables + citations
unique(kong_SST$var_name)
kong_SST_citations <- data.frame(citation = unique(kong_SST$citation))
# Look at citation(s) for a given variable
unique(kong_SST$citation[kong_SST$var_name == "MAT [°C]"])
# Look at all data within a given citation
citation_check <- kong_SST[grepl("Gattuso", kong_SST$citation, ignore.case = T),]

# Isfjorden
unique(full_product_is$var_name)
is_SST <- full_product_is %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("temp|°C", var_name, ignore.case = T)) %>% 
  # t [°C] = ground/snow temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.930472
  # T tech [°C] + T cal [°C] = Temperatures from an experiment e.g. https://doi.pangaea.de/10.1594/PANGAEA.847626
  filter(!grepl("SST sum|SST win|TTT|MAT|MAGT|MAAT|Tpot|Tequ|air|T intern|T tech|T cal|pHT", 
                var_name, ignore.case = T),
         var_name != "t [°C]") %>%
  mutate(site = "Is")
# Check final variables + citations
unique(is_SST$var_name)
is_check <- data.frame(table(is_SST$var_name, is_SST$citation))
is_SST_citations <- data.frame(citation = unique(is_SST$citation))
# Look at citation(s) for a given variable
filter(is_SST, var_name == "Temperature [ITS-90, deg C]")
unique(is_SST$citation[is_SST$var_name == "Temperature [ITS-90, deg C]"])
# Look at all data within a given citation
citation_check <- is_SST[grepl("VEINS Members;", is_SST$citation, ignore.case = T),]

# Storfjorden
unique(full_product_stor$var_name)
stor_SST <- full_product_stor %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("temp|°C", var_name, ignore.case = T)) %>% 
  filter(!grepl("Tpot|Tequ|theta|fco2", var_name, ignore.case = T)) %>%
  mutate(site = "Stor")
# Check final variables + citations
unique(stor_SST$var_name)
stor_SST_citations <- data.frame(citation = unique(stor_SST$citation))

# Young Sound
unique(full_product_young$var_name)
young_SST <- full_product_young %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("temp|°C", var_name, ignore.case = T)) %>% 
  # pot_temp [°C] is probably potential temperature e.g. https://zenodo.org/record/5572041#.YjIULDwo85k
  filter(!grepl("Tpot|Tequ|theta|fco2|pot_temp", var_name, ignore.case = T)) %>%
  mutate(site = "Young")
# Check final variables + citations
unique(young_SST$var_name)
unique(young_SST$citation[young_SST$var_name == "pot_temp [°C]"])
young_SST_citations <- data.frame(citation = unique(young_SST$citation))

# Disko Bay
unique(full_product_disko$var_name)
disko_SST <- full_product_disko %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("temp|°C", var_name, ignore.case = T)) %>% 
  # potential_temperature [°C] e.g. https://zenodo.org/record/4062024#.YjIVczwo85k
  filter(!grepl("Tequ|potential|theta|fco2", var_name, ignore.case = T)) %>%
  mutate(site = "Disko")
# Check final variables + citations
unique(disko_SST$var_name)
unique(disko_SST$citation[disko_SST$var_name == "potential_temperature [°C]"])
disko_SST_citations <- data.frame(citation = unique(disko_SST$citation))

# Nuup Kangerlua
unique(full_product_nuup$var_name)
nuup_SST <- full_product_nuup %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("temp|°C", var_name, ignore.case = T)) %>% 
  filter(!grepl("Tequ|T tech", var_name, ignore.case = T)) %>%
  mutate(site = "Nuup")
# Check final variables + citations
unique(nuup_SST$var_name)
nuup_SST_citations <- data.frame(citation = unique(nuup_SST$citation))

# Porsangerfjorden
unique(full_product_por$var_name)
por_SST <- full_product_por %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("temp|°C", var_name, ignore.case = T)) %>% 
  filter(!grepl("Tequ|Tpot|TTT", var_name, ignore.case = T)) %>%
  mutate(site = "Por")
# Check final variables + citations
unique(por_SST$var_name)
por_SST_citations <- data.frame(citation = unique(por_SST$citation))

## Combine all
ALL_SST <- rbind(kong_SST, is_SST, stor_SST, young_SST, disko_SST, nuup_SST, por_SST)

## Monthly averages
ALL_SST_monthly <- ALL_SST %>% 
  # mutate(year = lubridate::year(date),
         # month = lubridate::month(date)) %>% 
  mutate(date = lubridate::round_date(date, "month")) %>% 
  group_by(site, date) %>% 
  # group_by(site, year, month) %>% 
  summarise(temp = round(mean(value, na.rm = T), 2),
            count = n(),
            count_days = length(unique(date)), .groups = "drop") %>% 
  complete(nesting(site), date = seq(min(date), max(date), by = "month"))

## Trends
ALL_SST_monthly_trend <- ALL_SST_monthly %>% 
  group_by(site) %>%
  mutate(row_idx = 1:n()) %>% 
  do(fit_site = broom::tidy(lm(temp ~ row_idx, data = .))) %>% 
  unnest(fit_site) %>% 
  filter(term == "row_idx") %>% 
  mutate(temp_dec_trend = round(estimate*120, 4), 
         p.value = round(p.value, 4)) %>% 
  dplyr::select(site, temp_dec_trend, p.value)

## Plot monthly values
ggplot(ALL_SST_monthly, aes(x = date, y = temp, colour = site)) +
  geom_point() + geom_line() + geom_smooth(method = "lm", se = F)
  
## Monthly climatologies
ALL_SST_monthly_clim <- ALL_SST_monthly %>% 
  filter(!is.na(temp)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(site, month) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 2),
            count = n(), .groups = "drop")

## Plot monthly clims
ggplot(ALL_SST_monthly_clim, aes(x = month, y = temp, fill = site, colour = count)) +
  geom_col(position = "dodge") + scale_colour_viridis_c()

## Plot showing spatial difference between temperature products
### This may not work well across all sites


# Section 3 ---------------------------------------------------------------
# Relationships between data analysed for Section 2


# Section 4 ---------------------------------------------------------------
# Future projections of data analysed for Section 2 and relationships from Section 3


# Figure 1 ----------------------------------------------------------------


