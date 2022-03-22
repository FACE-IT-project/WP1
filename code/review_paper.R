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

# GEM data
load("data/restricted/young_GEM.RData")
load("data/restricted/disko_GEM.RData")
load("data/restricted/nuup_GEM.RData")

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
## NB: Load one-by-one, not all in one go, they are very large files
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
# Also produce summary stats
## Mean, median, min, max, skewness, kurtosis


## Ocean Temperature -------------------------------------------------------

# Kongsfjorden
kong_OISST <- sst_kong_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
kong_CCI <- sst_CCI_kong_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
# mutate(type = "in situ") %>% 
# bind_rows(kong_OISST, kong_CCI) %>% 
# Remove air, CO2, and pH related temperature values
# TTT is air temperature from cruise data on PANGAEA. e.g. https://doi.pangaea.de/10.1594/PANGAEA.326679
# MAAT + MAGT = ground temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.808512
# MAT = mean annual temperature e.g. https://doi.pangaea.de/10.1594/PANGAEA.907818
# Remove overly processed variables (e.g. average summer SST)
# Remove slightly different variables
# tequ = temperature at equilibrium; ~+0.6°C than the corresponding water temp
# e.g. https://doi.pangaea.de/10.1594/PANGAEA.849863
# T intern [°C] = internal temperature; ~+0.03°C than the corresponding water temp
# e.g. https://doi.pangaea.de/10.1594/PANGAEA.930028
# Removing tpot (Potential temperature) is a potentially controversial decision...
kong_SST <- review_filter_var(full_product_kong, "Kong", "temp|°C",
                              "air|co2|ph_|pHint_|TTT|MAAT|MAGT|MAT|
                              |SST sum|SST win|Temp min|Temp max|Temp interp|
                              |tequ|tpot|T intern|temp_") # Ultimately we want the "temp_" values
# review_filter_check(kong_SST, "MAT [°C]", "Gattuso")

# Isfjorden
is_OISST <- sst_is_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
is_CCI <- sst_CCI_is_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
# t [°C] = ground/snow temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.930472
# T tech [°C] + T cal [°C] = Temperatures from an experiment e.g. https://doi.pangaea.de/10.1594/PANGAEA.847626
is_SST <- review_filter_var(full_product_is, "Is", "temp|°C", 
                            "SST sum|SST win|TTT|MAT|MAGT|MAAT|Tpot|Tequ|air|T intern|T tech|T cal|pHT", "t [°C]")
# review_filter_check(is_SST, "Temperature [ITS-90, deg C]", "VEINS Members;")

# Storfjorden
stor_OISST <- sst_stor_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
stor_CCI <- sst_CCI_stor_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
stor_SST <- review_filter_var(full_product_stor, "Stor", "temp|°C", "Tpot|Tequ|theta|fco2")
# review_filter_check(stor_SST)

# Young Sound
young_OISST <- sst_young_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
young_CCI <- sst_CCI_young_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
young_SST <- review_filter_var(rbind(full_product_young, young_GEM), "Young", "temp|°C", "Tpot|Tequ|theta|fco2|pot_temp")
# review_filter_check(young_SST, "pot_temp [°C]")

# Disko Bay
disko_OISST <- sst_disko_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
disko_CCI <- sst_CCI_disko_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
disko_SST <- review_filter_var(rbind(full_product_disko, disko_GEM), "Disko", "temp|°C", "Tequ|potential|theta|fco2")
# review_filter_check(disko_SST)

# Nuup Kangerlua
nuup_OISST <- sst_nuup_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
nuup_CCI <- sst_CCI_nuup_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
nuup_SST <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "Nuup", "temp|°C", "Tequ|T tech")
# review_filter_check(nuup_SST)

# Porsangerfjorden
por_OISST <- sst_por_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
por_CCI <- sst_CCI_por_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
por_SST <- review_filter_var(full_product_por, "Por", "temp|°C", "Tequ|Tpot|TTT")
# review_filter_check(por_SST)

# Combine all
summary_SST <- review_summary(rbind(kong_SST, is_SST, stor_SST, young_SST, disko_SST, nuup_SST, por_SST))

# Plot results
review_summary_plot(summary_SST, "temp")

## Plot showing spatial difference between temperature products
### This may not work well across all sites


## Air temperature --------------------------------------------------------


## Salinity ---------------------------------------------------------------

# Kongsfjorden
kong_sal <- full_product_kong %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("sal|PSU", var_name, ignore.case = T)) %>% 
  # Remove overly processed variables
  # sal interp e.g. https://doi.org/10.1594/PANGAEA.877869
  filter(!grepl("sal interp", var_name, ignore.case = T)) %>% 
  # Remove glacial drainage land stations
  filter(!grepl("land station|drainage|meltwater", citation, ignore.case = T)) %>% 
  # Temporarily remove Ferry box data until units are assigned to temperature values
  filter(!grepl("sal_", var_name, ignore.case = T)) %>% 
  mutate(site = "Kong")
# Check final variables + citations
# unique(kong_sal$var_name)
kong_sal_citations <- data.frame(citation = unique(kong_sal$citation))
# Look at citation(s) for a given variable
# unique(kong_sal$citation[kong_sal$var_name == "psal [1e-3]"])
# Look at all data within a given citation
# citation_check <- kong_sal[grepl("Skogseth", kong_sal$citation, ignore.case = T),]

# Isfjorden
is_sal <- full_product_is %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("sal|PSU", var_name, ignore.case = T)) %>% 
  filter(!grepl("sal interp", var_name, ignore.case = T)) %>% 
  mutate(site = "Is")
# Check final variables + citations
# unique(is_sal$var_name)
is_sal_citations <- data.frame(citation = unique(is_sal$citation))
# Look at citation(s) for a given variable
# unique(is_sal$citation[is_sal$var_name == "Sal [mg/l]"])
# Look at all data within a given citation
# citation_check <- is_sal[grepl("Knittel", is_sal$citation, ignore.case = T),]

# Storfjorden
stor_sal <- full_product_stor %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("sal|PSU", var_name, ignore.case = T)) %>% 
  mutate(site = "Stor")
# Check final variables + citations
# unique(is_sal$var_name)
stor_sal_citations <- data.frame(citation = unique(stor_sal$citation))
# Look at citation(s) for a given variable
# unique(is_sal$citation[is_sal$var_name == "Sal [mg/l]"])
# Look at all data within a given citation
# citation_check <- stor_sal[grepl("Olsen, Are", stor_sal$citation, ignore.case = T),]

# Young Sound
young_sal <- full_product_young %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("sal|PSU", var_name, ignore.case = T)) %>% 
  filter(!grepl("sal interp", var_name, ignore.case = T)) %>% 
  mutate(site = "Young")
# Check final variables + citations
# unique(young_sal$var_name)
young_sal_citations <- data.frame(citation = unique(young_sal$citation))
# Look at citation(s) for a given variable
# unique(is_sal$citation[is_sal$var_name == "Sal [mg/l]"])
# Look at all data within a given citation
# citation_check <- stor_sal[grepl("Olsen, Are", stor_sal$citation, ignore.case = T),]

# Disko Bay
disko_sal <- full_product_disko %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("sal|PSU", var_name, ignore.case = T)) %>% 
  filter(!grepl("sal interp", var_name, ignore.case = T)) %>% 
  mutate(site = "Disko")
# Check final variables + citations
unique(disko_sal$var_name)
disko_sal_citations <- data.frame(citation = unique(disko_sal$citation))
# Look at citation(s) for a given variable
# unique(is_sal$citation[is_sal$var_name == "Sal [mg/l]"])
# Look at all data within a given citation
# citation_check <- stor_sal[grepl("Olsen, Are", stor_sal$citation, ignore.case = T),]

# Nuup Kangerlua
nuup_sal <- full_product_nuup %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("sal|PSU", var_name, ignore.case = T)) %>% 
  filter(!grepl("sal interp", var_name, ignore.case = T)) %>% 
  mutate(site = "Nuup")
# Check final variables + citations
unique(nuup_sal$var_name)
nuup_sal_citations <- data.frame(citation = unique(nuup_sal$citation))
# Look at citation(s) for a given variable
# unique(is_sal$citation[is_sal$var_name == "Sal [mg/l]"])
# Look at all data within a given citation
# citation_check <- stor_sal[grepl("Olsen, Are", stor_sal$citation, ignore.case = T),]

# Porsangerfjorden
por_sal <- full_product_por %>% 
  filter(depth <= 10, depth >= 0, !is.na(date)) %>% 
  filter(grepl("sal|PSU", var_name, ignore.case = T)) %>% 
  # Some dubious values in "Mankettikkara"
  filter(value > 0) %>% 
  mutate(site = "Por")
# Check final variables + citations
# unique(nuup_sal$var_name)
por_sal_citations <- data.frame(citation = unique(por_sal$citation))
# Look at citation(s) for a given variable
# unique(is_sal$citation[is_sal$var_name == "Sal [mg/l]"])
# Look at all data within a given citation
# citation_check <- stor_sal[grepl("Olsen, Are", stor_sal$citation, ignore.case = T),]

## Combine all
ALL_sal <- rbind(kong_sal, is_sal, stor_sal, young_sal, disko_sal, nuup_sal, por_sal)
ALL_sal_citations <- unique(rbind(kong_sal_citations, is_sal_citations, stor_sal_citations, young_sal_citations,
                                  disko_sal_citations, nuup_sal_citations, por_sal_citations))
## Monthly averages
ALL_sal_monthly <- ALL_sal %>% 
  mutate(date_round = lubridate::round_date(date, "month")) %>% 
  group_by(site, date_round) %>% 
  group_by(site, date_round) %>%
  summarise(sal_mean = round(mean(value, na.rm = T), 2),
            sal_median = round(median(value, na.rm = T), 2),
            sal_min = round(min(value, na.rm = T), 2),
            sal_max = round(max(value, na.rm = T), 2),
            count = n(), 
            count_days = length(unique(date)), .groups = "drop") %>%
  dplyr::rename(date = date_round) %>% 
  complete(nesting(site), date = seq(min(date), max(date), by = "month"))

## Trends
ALL_sal_trends <- ALL_sal_monthly %>% 
  # filter(date >= "1982-01-01", date <= "2020-12-31") %>%
  group_by(site) %>%
  mutate(row_idx = 1:n()) %>% 
  do(fit_site = broom::tidy(lm(sal_mean ~ row_idx, data = .))) %>% 
  unnest(fit_site) %>% 
  filter(term == "row_idx") %>% 
  mutate(sal_dec_trend = round(estimate*120, 4), 
         p.value = round(p.value, 4)) %>% 
  dplyr::select(site, sal_dec_trend, p.value)

## Plot monthly values
ggplot(ALL_sal_monthly, aes(x = date, y = sal_mean, colour = site)) +
  geom_point(alpha = 0.1) + geom_line(alpha = 0.1) + geom_smooth(method = "lm", se = F) +
  labs(y = "Salinity [PSU]", x = NULL, colour = "Site", linetype = "Source") +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/Desktop/sal_ts.png", width = 12, height = 5)

## Monthly climatologies
ALL_sal_monthly_clim <- ALL_sal_monthly %>% 
  filter(!is.na(sal_mean)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(site, month) %>% 
  summarise(sal_mean = round(mean(sal_mean, na.rm = T), 2),
            count = n(), .groups = "drop")

## Plot monthly clims
ggplot(ALL_sal_monthly_clim, aes(x = as.factor(month), y = sal_mean, fill = site)) +
  geom_col(position = "dodge") + scale_colour_viridis_d() +#scale_colour_viridis_c()
  labs(y = "Salinity [PSU]", x = "Month", fill = "Site", colour = "Source") +
  # facet_wrap(~site) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/Desktop/sal_clim.png", width = 12, height = 7)
ALL_sal_monthly %>% 
  filter(!is.na(sal_mean)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  ggplot(aes(x = as.factor(month), y = sal_mean, fill = site)) +
  geom_boxplot(position = "dodge") + scale_colour_viridis_d() +#scale_colour_viridis_c()
  labs(y = "Salinity [PSU]", x = "Month", fill = "Site", colour = "Source") +
  # facet_wrap(~site) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/Desktop/sal_clim_box.png", width = 12, height = 7)


# Section 3 ---------------------------------------------------------------
# Relationships between data analysed for Section 2


# Section 4 ---------------------------------------------------------------
# Future projections of data analysed for Section 2 and relationships from Section 3


# Figure 1 ----------------------------------------------------------------


