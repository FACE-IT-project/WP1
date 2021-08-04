# code/functions.R
# This script houses functions and other preparatory things used throughout the project


# Setup -------------------------------------------------------------------

# Libraries used in all other scripts
library(tidyverse)
library(ggOceanMaps)
library(sp)
library(sf)
library(pangaear)
library(doParallel); registerDoParallel(cores = 15)

# Find who is the user and define the pCloud path
if (Sys.getenv("LOGNAME") == "gattuso"){
  pCloud_path = "~/pCloud\ Drive/"
} else {
  pCloud_path = "~/pCloudDrive/" 
}


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
# system.time(
#   workflowr::wflow_publish(files = c("analysis/index.Rmd",
#                                      # "analysis/socat-glodap.Rmd", # Don't knit this unless necessary, it takes a long time
#                                      "analysis/key_drivers.Rmd",
#                                      "analysis/metadatabase.Rmd",
#                                      "analysis/review.Rmd"
#   ),
#   message = "Re-built site.")
# ) # 311 seconds with the SOCAT analysis, 6 seconds without


# Meta-data ---------------------------------------------------------------

# The base global map
map_base <- readRDS("metadata/map_base.Rda")


# Key drivers -------------------------------------------------------------

# Load PANGAEA driver metadata sheet
pg_parameters <- read_tsv("metadata/pangaea_parameters.tab")

# Aids for better filtering
sp_abb_one <- paste0(c(paste0(LETTERS,"[.] "), "sp[.]", "spp[.]"), collapse = "|")
sp_abb_sep <- paste0(LETTERS,". ")

# Function for consistent search word processing
query_params <- function(yes_words, no_words = NA, no_units = NA, yes_case = T){
  res <- pg_parameters %>% 
    filter(grepl(yes_words, Parameter, ignore.case = yes_case),
           !grepl(sp_abb_one, Abbreviation))
  if(!is.na(no_words)){
    res <- res %>% 
      filter(!grepl(no_words, Parameter, ignore.case = T))
  }
  if(!is.na(no_units)){
    res <- res %>% 
      filter(!grepl(no_units, Unit))
  }
  res <- res %>% 
    mutate(pg_col_name = case_when(!is.na(Unit) ~ paste0(Abbreviation," [",Unit,"]"),
                                   TRUE ~ Abbreviation))
  return(res)
}


## Metadata variables
# Longitude
query_longitude <- query_params("longitude", "file ")
# Latitude
query_latitude <- query_params("latitude", "file ")
# Date/Time
query_date <- query_params("date", "age|consolidated|birth|death|germination|iodate|cordate|MATLAB|file ")
# Depth
query_depth <- query_params("depth|bathymetry|pressure|density|elevation",
                            "Acarina|active layer|algae|Aerosol|Amphipoda|Appendicularia|areal|
                            |Argon|living|Balanidae|Bankful|Bivalvia|body|bone|Bosmina|Bryozoa|
                            |Calanoides|Calcification|Capitulum|Cell pressure|Chaetognatha|soil|
                            |Cladocera|Coelenterata|Crossaster|Cryoconite|Ctenophora|Cyclopoida|
                            |vapour|partial pressure|Bromine monoxide|bulk|Cell density|cell-specific|
                            |photosynthesis|Map Type|abundance|depth of|photosyn|desert|digging|
                            |Domicile|Earlywood|overburden|electric|larvae|Excitation|fission|forest|
                            |Fascicle|Foraminifera|Formaldehyde|fouling|fracture|Gammaridae|Gastropoda|
                            |Glyoxal|Habitat|Harpacticoida|HLDS|HRDD|Hydrogen|Hydromedusae|Hydroxyapatite|
                            |Hyperiidae|Hypocentre|Incubation|Indentation|Invertebrata|Iodine|Larval|
                            |Latewood|leaf|Lithosphere|magnet|litter|modern|Mohorovicic|Morphospecies|
                            |Mysidacea|nautical|Nematoda|Nitrate|Nitrogen|Nodule|Notch|number of|Ostracoda|
                            |Oxic|Oxid|Oxyd|Oxyg|Paleo|Particle|Pisces|plant|platform|pockmark|podon|file|
                            |Polychaeta|Polychlorinated|Population|pore|Pteropoda|Radiodensity|Radiolarians|
                            |Copepoda|optical|organic|shell|shoots|sigma|skeletal|snow|ice|velocity|irradiance|
                            |mixed layer|mixing|crack|Curie|absorption|Rotatoria|Rotifera|Sclerite|composite|
                            |Stomatal|Symbiodinium|Synchaeta|Thaliacea|Thecosomata|Thorium|Time at|Tissue|Trochophora",
                            "hPa|kPa|MPa|arbitrary|#|µm|g/cm|±|A/m|dB|1/mm")
# Combined
query_Meta <- rbind(query_longitude, query_latitude, query_date, query_depth)
rm(query_longitude, query_latitude, query_date, query_depth); gc()


## Cryosphere
# Coastal ice
# Fast ice
# Sea ice
query_ice <- query_params("ice", 
                          "abies|aegiceras|aminicenantes|avicenni|biosiliceous|bryozoa|calcite|cf[.]|Chvaleticeite|
                          |cicendia|cicer|cichoriceae|cribricellina|Cricetidae|Cunoniceae|Cymatiosphaera|Daphne|Dehydroi|
                          |device|Diatoms|Digalac|foraminifera|Galact|Griceite|Hepaticeae|lattice|laonice|leontice|
                          |Lonicera|Macellice|methyl|Monticellite|Oedicerotidae|Ovicell|Paniceae|Picea|Pluricell|distance|
                          |Pseudotrice|Pumice|price|quartz|Radicel|Sabicea|Scolecith|Siliceous|Stauroneis|statice|
                          |volcanic ash|Tetragonic|Timeslice|Tree-ring|Trifolium|Ultraviolet|Unicellular|Urticeae|Zelkova") 
# Glacier
query_glacier <- query_params("glacier|glacial", "Foraminifera|glacialis")
# Snow cover
query_snow <- query_params("snow")
# Permafrost
query_permafrost <- query_params("permafrost")
# Combined
query_Cryosphere <- rbind(query_ice, query_glacier, query_snow, query_permafrost)
rm(query_ice, query_glacier, query_snow, query_permafrost); gc()


## Physical
# Bathymetry (bathy) - see depth query
# Current: direction, location, volume (current, vel, direction, vol, u , v )
query_current <- query_params("current|velocity|direction|volume", 
                              "Air |Aircraft|Angle|Aggregates|Anhysteretic|Biomass|fecal|isotop|ARM, |
                              |pipe|Back-IRM|bed dip|Biovolume|Blue light|brightness|Calcite|Calcium|Carbon|
                              |Cardiac|Cell|Chloro|Cloud|electrical|occupational|Deformation|ribonucleic|
                              |frequency|wind|dry|dust|egg|flux|hydrate|glacier|glacial|Hard-IRM|heat|Heterotrophic|
                              |Hysteresis|ice |ice-|Incubation|iodine|Gonad|Diatoms|Settling|ship|Green light|
                              |description|iron|journal|corpuscular|Methane|molar|Nectar|Nodule|Organic|roll|
                              |Oxic|Oxid|Oxyd|Oxyg|Ozone|Particle|Phytoplankton|Piston|biphenyl|Porosity|Pteropoda|
                              |reservoir|Root|sample|Sinking|Soil|sonic|sound|Stroke|backscattering|Susceptibility|
                              |bladder|chamber|Tintinnid|tissue|tree|Ventilatory|lava|percentage|wave|zooplankton",
                              "#|pg/ml|µl/l|ml/l|nmol/l|ng/ml|µm|±|mg/cm|µg/m|db|pA/m|arbitrary|nmol|µl")
# Evaporation/Precipitation: (evap, precip, e-p, p-e)
query_evap_precip <- query_params("evaporation|precipitation", "δ")
# Heatflux: net, latent/sensible, long/shortwave radiation (Q, flux, latent, sensible, longwave, shortwave, radiation)
query_heatflux <- query_params("heatflux|heat-flux|heat flux|latent|sensible|
                               |longwave|long-wave|long wave|shortwave|short-wave|short wave")
# Light extinction coefficient (kd, absorption)
# NB: "absorption" not used because of how wide those data are
query_light_extinction <- query_params("extinction", "aerosol|foraminifera|Delta")
# Mixed layer depth (mixed, MLD)
query_MLD <- query_params("mixed layer|mixed-layer|mixedlayer", "Foraminifera|Illite|clay|smectite")
# River discharge (river, discharge)
query_river <- query_params("river|discharge", "Diatoms|smoke|glacier|Dust|pixel|Riversideite", "#|±")
# Salinity (sal, psu)
query_salinity <- query_params("salinity", "Diatoms|Dinoflagellate|Radium|Snow|Treatment", "±")
# Sea level pressure (slp)
query_slp <- query_params("pressure", no_units = "±|dbar",
                          "Argon|Blood|Cell|partial pressure|Fouling|laboratory|experiment|Vapour|velocity|Sound")
# Sedimentation rate (sedim)
query_sedimentation <- query_params("sedimentation") 
# Suspended matter: organic, mineral (pom, pim, som, spm)
# NB: This one is questionable. I decided to keep most parameters but maybe shouldn't have.
query_suspended <- query_params("suspended", "Backscattering", "±")
# (Seawater+air) temperature: surface, mid, bottom (°C, temp, sst)
query_temperature <- query_params("temperature", no_units = "±|K/100m",
                                  "Acid|Body|Fugacity|processes|Number|partial pressure|atoms|treatment|xCO2|δ")
# Wind: direction, speed (wind, speed, direction, u, v)
query_wind <- query_params("wind|speed|direction", 
                           "Sigma|window|Aurelia|bed dip|Brightness|cloud|Coiling|Current|deform|Gamete|
                           |Growing|ice |ice-|sperm|pixel|Plastic|polen|Predator|prey|Ship|snow|swim|swell|
                           |temperature|Tidal|Towing|wave", "±")
# combined
query_Physical <- rbind(query_current, query_evap_precip, query_heatflux, query_light_extinction, query_MLD, query_river, 
                        query_salinity, query_slp, query_sedimentation, query_suspended, query_temperature, query_wind)
rm(query_current, query_evap_precip, query_heatflux, query_light_extinction, query_MLD, query_river, 
   query_salinity, query_slp, query_sedimentation, query_suspended, query_temperature, query_wind); gc()


## Chemistry
# CaCO3 saturation state (CaCO3, Arg, Ara, Cal, omega)
# NB: Decided to keep almost everything
query_calc_carb <- query_params("calcium carbonate", "δ", "±")
# Dissolved inorganic carbon (DIC)
# Dissolved organic carbon (DOC)
# Dissolved organic nitrogen (DON)
query_dissolved <- query_params("dissolved inorganic carbon|dissolved organic carbon|dissolved organic nitrogen")
# Dissolved O2 (DO, O2)
query_oxygen <- query_params("oxygen", 
                             "Aerobic|demand|oxygenase|respiration|Foraminifer|Biological|carbon|chamber|Community|
                             |Electron|exercise|fecal|Fluorescence|chlorophyll|photosynthesis|primary production|
                             |Haemolymph|hydrod|leaf|Mesozooplankton|Metabolic|Mitochondria|Mollusca|consumption|
                             |Nitrogen|utilization|Argon|uptake|Photosynthetic|species|Seston|swim|isotope",
                             "#|±")
# Nutrients: nitrate (NO3), nitrite (NO2), ammonium (NH3), phosphate (PO4), silicate (Si04)
query_nutrients <- query_params("nitrate|nitrite|ammonium|phosphate|silicate", 
                                "Adenosine|Affinity|Alkalin|Aluminosilicate|soil|Bacteria|Calcium|Mannose|Cytidine|
                                |Ethyl|hydrosilicate|Guanosine|Haemolymph|Inverse|Isopropyl|Lithiophosphate|Lithium|
                                |Mesozooplankton|Natrophosphate|Nicotinamide|non-silicates|propyl|Ortho|oxide|
                                |carboxylase|Phosphorus|Phyllosilicate|Ribulose|butyl|Thymidine|oxyradical|Tributyl|
                                |Tricresyl|Triisobutyl|Triphenyl|Triphosphates|Uridine|δ15|Δ17|δ18", 
                                "±")
# Partial pressure of CO2 (pCO2)
query_pCO2 <- query_params("partial pressure", 
                           "Blood|Coelomic|Extrapallial|Haemolymph|oxygen|Methane|nitro|Ozone|Treatment|vapour", "±")
# pH (ph)
query_pH <- query_params("pH", 
                         "Calcifying|Coelomic|Extrapallial|Haemolymph|Metabolic|cellular|periv|seminal|soil|treatment|voltage", 
                         "±|#", yes_case = F)
# Total alkalinity (TA, AT)
query_alkalinity <- query_params("alkalinity", "borate|chlorine|Coelomic", "±")
# Combined
query_Chemistry <- rbind(query_calc_carb, query_dissolved, query_oxygen, query_nutrients, query_pCO2, query_pH, query_alkalinity)
rm(query_calc_carb, query_dissolved, query_oxygen, query_nutrients, query_pCO2, query_pH, query_alkalinity); gc()

## Biology
# Calcification
query_calcification <- query_params("calcification", no_units = "±")
# Nitrogen fixation
query_nitro_fix <- query_params("Nitrogen fixation", no_units = "±")
# Photosynthesis
query_photosynthesis <- query_params("Photosynthesis", "Carbon-14", "±")
# Primary production
query_prim_prod <- query_params("Primary production", no_units = "±")
# Respiration
# Nb: Not sure about the need for this one...
query_respiration <- query_params("Community respiration", no_units = "±")
# Species: presence/absence, abundance/biomass
# NB: Not doing this at the moment due to how wide these data are...
# Combined
query_Biology <- rbind(query_calcification, query_nitro_fix, query_photosynthesis, query_prim_prod, query_respiration)
rm(query_calcification, query_nitro_fix, query_photosynthesis, query_prim_prod, query_respiration); gc()

## Social
# Fish landings: commercial, recreational, quotas, seasonality
# Game landings: quotas, seasonality
query_landings <- query_params("landings")
# Local and national resource management
query_management <- query_params("management")
# National statistics: demography, income, unemployment
query_nat_stat <- query_params("demography|income|unemployment")
# Tourist arrivals: per month, nationality
query_tourism <- query_params("touris|nationality")
# Tourist vessels: count, mileage
query_vessels <- query_params("vessel|mileage")
# Combine
query_Social <- rbind(query_landings, query_management, query_nat_stat, query_tourism, query_vessels)
rm(query_landings, query_management, query_nat_stat, query_tourism, query_vessels); gc()

## All variables together
query_ALL <- rbind(query_Meta, query_Cryosphere, query_Physical, query_Chemistry, query_Biology, query_Social)


# Functions ---------------------------------------------------------------

# Function that takes 4 bounding box coordinates and converts them to a polygon for ggOceanMaps
# The 'ID' value can be used to hold the name of the site for the bounding box
bbox_to_poly <- function(coords, ID = 1){
  
  # Get the coordinates
  if(is.data.frame(coords)){
    lon1 <- min(coords$lon1); lon2 <- max(coords$lon2)
    lat1 <- min(coords$lat1); lat2 <- max(coords$lat2)
  } else if(is.vector(coords)){
    lon1 <- coords[1]; lon2 <- coords[2]
    lat1 <- coords[3]; lat2 <- coords[4]
  } else {
    stop("Uh oh")
  }
  
  # Create bounding box that can curve on a polar projection
  bbox_top <- data.frame(lon = seq(lon1, lon2, length.out = 100), 
                         lat = lat1, id = "bbox")
  bbox_bottom <- data.frame(lon = seq(lon2, lon1, length.out = 100), 
                            lat = lat2, id = "bbox")
  bbox_df <- rbind(bbox_top, bbox_bottom)
  
  # only want lon-lats in the list, not the names
  bbox_list <- lapply(split(bbox_df, bbox_df$id), function(x) { x["id"] <- NULL; x })
  
  # Convert to polygon and add id variable 
  bbox_poly <- Polygons(sapply(bbox_list, Polygon), ID = ID)
  
  # Create SpatialPolygons object
  bbox_spatial <- SpatialPolygons(list(bbox_poly), 
                                  proj4string = CRS("+init=epsg:4326 +proj=longlat")) 
  return(bbox_spatial)
}

# Function that prepares custom bathymetry for ggOceanMaps based on bounding box
# lon1 <- bbox_nor$lon1; lon2 <- bbox_nor$lon2; lat1 <- bbox_nor$lat1; lat2 <- bbox_nor$lat2
bbox_to_bathy <- function(coords, lon_pad = 0, lat_pad = 0,
                          bathy_file = NA, projection = NA,
                          depths = c(25, 50, 100, 200, 300, 500, 1000, 2000, 10000)){
  
  # Get the coordinates
  if(is.data.frame(coords)){
    lon1 <- min(coords$lon1); lon2 <- max(coords$lon2)
    lat1 <- min(coords$lat1); lat2 <- max(coords$lat2)
  } else if(is.vector(coords)){
    lon1 <- coords[1]; lon2 <- coords[2]
    lat1 <- coords[3]; lat2 <- coords[4]
  } else if(is(coords, "SpatialPolygons")) {
    lon1 <- coords@bbox[1,1]; lon2 <- coords@bbox[1,2]
    lat1 <- coords@bbox[2,1]; lat2 <- coords@bbox[2,2]
  } else {
    stop("Uh oh")
  }
  
  # Use the default hi-res Arctic bathy unless the user specifies something else
  # if(is.na(bathy_file)) bathy_file <- paste0(pCloud_path,"FACE-IT_data/shape_files/IBCAO_v4_200m.nc") # Super hi-res, but doesn't work...
  # if(is.na(bathy_file)) bathy_file <- paste0(pCloud_path,"FACE-IT_data/shape_files/ETOPO1_Ice_g_gmt4.grd")
  if(is.na(bathy_file)) bathy_file <- paste0(pCloud_path,"FACE-IT_data/shape_files/GEBCO_2020.nc")
    
  # Set limits for bathy projection
  xlon <- c(lon1-lon_pad, lon2+lon_pad)
  xlat <- c(lat1-lat_pad, lat2+lat_pad)
  lims <- c(xlon, xlat)
  
  # Set projection
  if(is.na(projection)){
    # projection <- "+init=epsg:6070"
    projection <- "+init=epsg:3995" # Arctic Polar Stereographic
    # projection <- "+init=epsg:4326" # Cartesian global
    # projection <- "+init=epsg:32636"
  } 
  
  # Check the limits
  # basemap(limits = lims)
  
  # Convert NetCDF to raster
  rb <- raster_bathymetry(bathy = bathy_file,
                          depths = depths, 
                          proj.out = projection, 
                          boundary = lims)
  
  # Check raster output
  # class(rb)
  # names(rb)
  # raster::plot(rb$raster)
  
  # Convert to raster vector for plotting
  bs_bathy <- vector_bathymetry(rb)
  # sp::plot(bs_bathy)
  
  # Convert land file for use with new bathy file
  world <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_land.shp"), verbose = F)
  islands <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_minor_islands.shp"), verbose = F)
  world <- rbind(world, islands)
  bs_land <- clip_shapefile(world, lims)
  bs_land <- sp::spTransform(bs_land, CRSobj = sp::CRS(projection))
  if(!rgeos::gIsValid(bs_land)){  # Has to return TRUE, if not use rgeos::gBuffer
    bs_land <- rgeos::gBuffer(bs_land, byid = TRUE, width = 0)
  }
  # sp::plot(bs_land)
  
  # Create glacier shape files
  glaciers <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_glaciated_areas.shp"), verbose = F)
  if(!rgeos::gIsValid(glaciers)){ # Needs buffering
    glaciers <- rgeos::gBuffer(glaciers, byid = TRUE, width = 0)
  }
  bs_glacier <- clip_shapefile(glaciers, lims)
  if(dim(bs_glacier)[1] > 0){
    bs_glacier <- sp::spTransform(bs_glacier, CRSobj = sp::CRS(projection))
    # rgeos::gIsValid(bs_glacier)
    # sp::plot(bs_glacier)
  } else { 
    bs_glacier <- NA
  }
  # sp::plot(bs_glacier)
  # basemap(shapefiles = list(land = bs_land, glacier = bs_glacier, bathy = bs_bathy), bathymetry = TRUE, glaciers = TRUE)
  
  # Return results
  res <- list(bathy = bs_bathy, land = bs_land, glacier = bs_glacier)
  return(res)
}

# Convenience function that allows a user to directly produce a ggOceanMaps from a bounding box
# lon1=9; lon2=30; lat1=76; lat2=81
bbox_to_ggOcean <- function(coords, bathy_file = NA, lon_pad = 0, lat_pad = 0, add_bbox = F,
                            depths = c(25, 50, 100, 200, 300, 500, 1000, 2000, 10000)){
  
  # Prep the shape files
  bs_res <- bbox_to_bathy(coords, bathy_file = bathy_file, lon_pad = lon_pad, lat_pad = lat_pad, depths = depths)
  
  # Plot on ggOceanMaps
  if(!is.na(bs_res[[3]])){
    map_res <- basemap(shapefiles = list(bathy = bs_res$bathy, land = bs_res$land, glacier = bs_res$glacier),
                       bathymetry = TRUE, glaciers = TRUE)
  } else{
    map_res <- basemap(shapefiles = list(bathy = bs_res$bathy, land = bs_res$land, glacier = NULL), 
                       bathymetry = TRUE, glaciers = FALSE)
  }
  map_res <- map_res + scale_fill_viridis_d("Depth (m)")
  
  # Add the bounding box as desired
  if(add_bbox){
    if(!is(coords, "SpatialPolygons")){
      bbox_spatial <- bbox_to_poly(coords)
    } else {
      bbox_spatial <- coords
    }
    map_res <- map_res + annotation_spatial(bbox_spatial, fill = "cadetblue1", alpha = 0.2)
  }
  return(map_res)
}

# Function for smoother meta-data creation
# Not currently used
make_meta_data <- function(dat, type, data_name, file_name, URL, reference, note = NA){
  
  # Find longitude range
  lon_col <- c(colnames(dat)[str_detect(colnames(dat), "lon")],
               colnames(dat)[str_detect(colnames(dat), "Lon")])
  if(length(lon_col) > 0){
    lon_min <- round(min(dat[,lon_col], na.rm = T), 2)
    lon_max <- round(max(dat[,lon_col], na.rm = T), 2)
    lon_range <- paste0(lon_min," to ", lon_max)
  } else {
    lon_min <- NA; lon_max <- NA
    lon_range <- NA
  }
  
  # Find latitude range
  lat_col <- c(colnames(dat)[str_detect(colnames(dat), "lat")],
               colnames(dat)[str_detect(colnames(dat), "Lat")])
  if(length(lat_col) > 0){
    lat_min <- round(min(dat[,lat_col], na.rm = T), 2)
    lat_max <- round(max(dat[,lat_col], na.rm = T), 2)
    lat_range <- paste0(lat_min," to ", lat_max)
  } else {
    lat_min <- NA; lat_max <- NA
    lat_range <- NA
  }
  
  # Find depth range
  depth_col <- c(colnames(dat)[str_detect(colnames(dat), "depth")],
                 colnames(dat)[str_detect(colnames(dat), "Depth")])
  depth_col <- depth_col[!str_detect(depth_col, "bot")]
  depth_col <- depth_col[!str_detect(depth_col, "Bot")]
  if(length(depth_col) > 0){
    depth_min <- round(min(dat[,depth_col], na.rm = T), 2)
    depth_max <- round(max(dat[,depth_col], na.rm = T), 2)
    depth_range <- paste0(depth_min," to ", depth_max)
  } else {
    depth_min <- NA; depth_max <- NA
    depth_range <- NA
  }
  
  # Find depth range
  date_col <- c(colnames(dat)[str_detect(colnames(dat), "date")],
                colnames(dat)[str_detect(colnames(dat), "Date")])
  year_col <- c(colnames(dat)[str_detect(colnames(dat), "year")],
                colnames(dat)[str_detect(colnames(dat), "Year")])
  if(length(date_col) > 0){
    date_dat <- dat[,date_col] %>% `colnames<-`("t")
    if(!is.na(as.Date(date_dat$t[1], "%Y-%m-%d"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%Y-%m-%d")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%Y-%m-%d")), na.rm = T)
    } else if(!is.na(as.Date(date_dat$t[1], "%Y%m%d"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%Y%m%d")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%Y%m%d")), na.rm = T)
    }else if(!is.na(as.Date(date_dat$t[1], "%Y/%m/%d"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%Y/%m/%d")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%Y/%m/%d")), na.rm = T)
    } else if(!is.na(as.Date(date_dat$t[1], "%d/%m/%Y"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%d/%m/%Y")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%d/%m/%Y")), na.rm = T)
    } else if(!is.na(as.Date(date_dat$t[1], "%m/%d/%Y"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%m/%d/%Y")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%m/%d/%Y")), na.rm = T)
    } else {
      date_min <- NA; date_max <- NA
    }
  }
  if(is.na(depth_min) & length(year_col) > 0) {
    year_dat <- dat[,year_col] %>% `colnames<-`("year")
    date_min <- min(year_dat$year, na.rm = T)
    date_max <- max(year_dat$year, na.rm = T)
  }
  if(!is.na(date_min)){
    date_range <- paste0(date_min," to ", date_max)
  } else {
    date_range <- NA
  }
  
  # Determine ecoregions
  # Find point in MEOW polygons
  
  # Combine and output
  res <- data.frame(type, data_name, 
                    date_range, lon_range, lat_range, depth_range,
                    # date_min, date_max, lon_min, lon_max, lat_min, lat_max, depth_min, depth_max, 
                    file_name, URL, reference,
                    note)
  return(res)
}

# Function for loading raster files with utm coords and converting to lon/lat
load_utm <- function(file_name){
  ras_1 <- raster(file_name)
  crs_1 <- ras_1@crs
  utm1 <- as.data.frame(ras_1, xy = T)
  coordinates(utm1) <- ~x+y 
  proj4string(utm1) <- crs_1
  utm2 <- spTransform(utm1, CRS("+proj=longlat +datum=WGS84"))
  utm3 <- as.data.frame(utm2) %>% 
    dplyr::rename(lon = x, lat = y) %>% 
    dplyr::select(lon, lat, everything())
}

