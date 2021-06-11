# code/data_collection.R
# The location of collected data and the code used when possible


# Setup -------------------------------------------------------------------

# TODO: Add a marine or terrestrial column for users to be able to select one as they prefer
# This may be problematic when dealing with very high-res coastal data

# Libraries used in this script
library(tidyverse)
library(pangaear)
library(doParallel); registerDoParallel(cores = 15)

# Previously downloaded PANGAEA data
pg_files <- dir("~/pCloudDrive/FACE-IT_data", pattern = "pg_", recursive = T, full.names = T)
pg_files <- pg_files[grepl(".csv", pg_files)]
pg_doi_list <- read_csv("~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


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
  return(res)
}


## Metadata variables
# Longitude
query_longitude <- query_params("longitude")
# Latitude
query_latitude <- query_params("latitude")
# Date/Time
query_date <- query_params("date", "age|consolidated|birth|death|germination|iodate|cordate|MATLAB")
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
                            |Oxic|Oxid|Oxyd|Oxyg|Paleo|Particle|Pisces|plant|platform|pockmark|podon|
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
                          |Lonicera|Macellice|methyl|Monticellite|Oedicerotidae|Ovicell|Paniceae|Picea|Pluricell|
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
                              |description|iron|journal|corpuscular|Methane|molar|Nectar|Nodule|Organic|
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
query_respiration <- query_params("Respiration", no_units = "±")
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
rm(query_Meta, query_Cryosphere, query_Physical, query_Chemistry, query_Biology, query_Social); gc()


# Functions ---------------------------------------------------------------

# Function for printing PANGAEA meta-data
pg_meta_print <- function(pg_doi){
  pg_test <- pangaear::pg_data(pg_doi)
}

# Function for extracting info from PANGAEA data
pg_dl_prep <- function(pg_dl){
  # Extract data.frame and attach URL + citation
  if(is.data.frame(pg_dl$data)){
    if(length(unique(colnames(pg_dl$data))) == length(colnames(pg_dl$data))){
      dl_single <- pg_dl$data %>% 
        dplyr::select(contains(c("date", "longitude", "latitude", "depth", "bathy", "press", "density", "elev",  
                                 "temp", "°C", "sst", "sal", "O2", "DO", "ice", "snow", "turb", "PAR", "current", "vel", "u", "v", 
                                 "direction", "vol", "evap", "precip", "e-p", "p-e", "Q", "flux", "latent", "sensible", "longwave", 
                                 "shortwave", "radiation", "kd", "mixed", "MLD", "river", "discharge", "slp", "sedim", "som", "sim",
                                 "wind", "speed", "direction",  "DIC", "DOC", "DON", "pH", "pCO2", "CaCO3", "Arg", "Ara", 
                                 "Cal", "NO3", "NO2", "NH3", "PO4", "Si", "AT", "TA", "Chl a", "Chl b"))) %>% 
        # Don't filter "[#]" or "[%]" as these may be used with ice data
        dplyr::select(-contains(c("Rock", "feldspar", "Grain size", "Cluster", "File ", "URL ", "std dev", "Device", 
                                  "Binary", "taxa", "phenotype", "Part conc ", "Part vol frac ", "Part conc frac ",
                                  "Comment", "residue", "Stage", "Sample", "Country", "Province", "Station", "Event",
                                  "Persistent Identifier"))) %>% 
        dplyr::select(-starts_with(c("Lu_", "Zn ", "Cu ", "Ni ", "Cd ", "As ", "Pb ", "Cr ", "Th ", "Mn ", 
                                     "Co ", "Zr ", "Sr ", "Ba ", "K ", "Na ", "Ti ", "Fe ", "Mg ", "No ", "Al ",
                                     "A. ", "B. ", "C. ", "D. ", "E. ", "F. ", "G. ", "H. ", "I. ", "J. ", "K. ", "L. ", "M. ",
                                     "N. ", "O. ", "P. ", "Q. ", "R. ", "S. ", "T. ", "U. ", "V. ", "W. ", "X. ", "Y. ", "Z. "))) %>% 
        janitor::remove_empty(which = c("rows", "cols")) %>% 
        mutate(URL = pg_dl$url,
               citation = pg_dl$citation)
      # Filter out 
      if("Longitude" %in% colnames(dl_single)){
        dl_single <- dl_single %>% 
          filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
      }
      # Check for any useful columns
      if(all(colnames(dl_single) %in% c("URL", "citation", "Longitude", "Latitude", "Date/Time"))){
        dl_single <- data.frame(URL = pg_dl$url,
                                citation = pg_dl$citation)
      }
    } else {
      dl_single <- data.frame(URL = pg_dl$url,
                              citation = pg_dl$citation)
    }
  } else {
    dl_single <- data.frame(URL = pg_dl$url,
                            citation = pg_dl$citation)
  }
  # Finish up and exit
  dl_single <- dl_single %>% 
    mutate(date_accessed = Sys.Date()) %>% 
    dplyr::select(date_accessed, URL, citation, everything())
  return(dl_single)
}

# Function for downloading and processing PANGAEA data for merging
pg_dl_proc <- function(pg_doi){
  # Get data
  dl_dat <- tryCatch(pg_data(pg_doi), error = function(pg_doi) NA)
  
  # Extract data from multiple lists as necessary
  if(!is.na(dl_dat)){
    dl_df <- plyr::ldply(dl_dat, pg_dl_prep) #%>% 
      # dplyr::select(URL, parent_doi, citation, everything())
  } else {
    dl_df <- NULL
  }
  
  # Exit
  return(dl_df)
}

# Function for performing a more thorough query of PANGAEA data by bbox
pg_full_search <- function(...){
  pg_res_all <- data.frame()
  # query_min_score <- 100
  query_offset <- 0
  while(query_offset < 10000){
    pg_res_query <- pangaear::pg_search(count = 500, offset = query_offset, ...)
    pg_res_all <- rbind(pg_res_all, pg_res_query)
    query_offset <- query_offset+500
    # query_min_score <- min(pg_EU_cruise_all$score)
  }
  pg_res_all <- distinct(arrange(pg_res_all, citation)) %>% 
    filter(!grepl("video|photograph|image|station list|master tracks|aircraft|flight|
                  |airborne|soil |metadata list|land use", citation, ignore.case = T),
           !grepl("ACLOUD", citation))
  return(pg_res_all)
}


# European Arctic ---------------------------------------------------------

### Cryosphere
## Sea ice concentration
# 25 km, 1978 - 2019: daily
# ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02202_V3/north/daily/
# 6.25 km, possibly 10 m, 2002 - 2021: daily
# https://seaice.uni-bremen.de/data/amsr2/

### Geochemistry
## SOCAT datasets on PANGAEA
# Bakker et al...


# Specific author queries
# pg_Riebesell <- pg_full_search(query = "Riebesell", bbox = c(-60, 63, 60, 90))
# pg_Fransson <- pg_full_search(query = "Fransson", bbox = c(-60, 63, 60, 90))
# pg_Chierici <- pg_full_search(query = "Chierici", bbox = c(-60, 63, 60, 90))
# pg_Fischer <- pg_full_search(query = "Chierici", bbox = c(-60, 63, 60, 90))


# Test specific files
pg_test_1 <- pg_data(doi = "10.1594/PANGAEA.868371")[[1]]$data
pg_test_2 <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.868371")


## EU Arctic cruise Oceans data on PANGAEA - 270 - Some issues
pg_EU_cruise_oceans <- pg_full_search(query = "cruise", topic = "Oceans", bbox = c(-60, 63, 60, 90)) 
pg_doi_list <- distinct(data.frame(doi = pg_EU_cruise_oceans$doi))
pg_EU_cruise_oceans_dl <- plyr::ldply(pg_EU_cruise_oceans$doi, pg_dl_proc)
# pg_EU_cruise_oceans_clean <- pg_EU_cruise_oceans_dl %>%
  # filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_oceans_clean)
data.table::fwrite(pg_EU_cruise_oceans_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Oceans.csv")
data.table::fwrite(pg_EU_cruise_oceans_clean, "data/pg_data/pg_EU_cruise_Oceans.csv")
rm(pg_EU_cruise_oceans_dl, pg_EU_cruise_oceans_clean); gc()


## EU Arctic cruise Atmosphere data on PANGAEA - 138 - Some issues
pg_EU_cruise_atmosphere <- pg_full_search(query = "cruise", topic = "Atmosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_atmosphere$doi)))
pg_EU_cruise_atmosphere_dl <- plyr::ldply(pg_EU_cruise_atmosphere$doi, pg_dl_proc)
pg_EU_cruise_atmosphere_clean <- pg_EU_cruise_atmosphere_dl %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_atmosphere_clean)
data.table::fwrite(pg_EU_cruise_atmosphere_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Atmosphere.csv")
data.table::fwrite(pg_EU_cruise_atmosphere_clean, "data/pg_data/pg_EU_cruise_Atmosphere.csv")
rm(pg_EU_cruise_atmosphere_dl, pg_EU_cruise_atmosphere_clean); gc()


## EU Arctic cruise Cryosphere data on PANGAEA - 7
pg_EU_cruise_cryosphere <- pg_full_search(query = "cruise", topic = "Cryosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_cryosphere$doi)))
pg_EU_cruise_cryosphere_dl <- plyr::ldply(pg_EU_cruise_cryosphere$doi, pg_dl_proc)
pg_EU_cruise_cryosphere_clean <- pg_EU_cruise_cryosphere_dl %>%
  select(-contains(c("Perc ", "Particles ", "Polymer particles ", "Polymer particle conc ", "(Calculated)"))) %>% 
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_cryosphere_clean)
data.table::fwrite(pg_EU_cruise_cryosphere_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Cryosphere.csv")
data.table::fwrite(pg_EU_cruise_cryosphere_clean, "data/pg_data/pg_EU_cruise_Cryosphere.csv")
rm(pg_EU_cruise_cryosphere_dl, pg_EU_cruise_cryosphere_clean); gc()


## EU Arctic cruise Biological Classification data on PANGAEA - 262 - Some issues
pg_EU_cruise_bio_class <- pg_full_search(query = "cruise", topic = "Biological Classification", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_bio_class$doi)))
pg_EU_cruise_bio_class_dl <- plyr::ldply(pg_EU_cruise_bio_class$doi, pg_dl_proc)
pg_EU_cruise_bio_class_clean <- pg_EU_cruise_bio_class_dl %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_bio_class_clean)
data.table::fwrite(pg_EU_cruise_bio_class_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Bio_class.csv")
data.table::fwrite(pg_EU_cruise_bio_class_clean, "data/pg_data/pg_EU_cruise_Bio_class.csv")
rm(pg_EU_cruise_bio_class_dl, pg_EU_cruise_bio_class_clean); gc()


## EU Arctic cruise Biosphere data on PANGAEA - 3
pg_EU_cruise_biosphere <- pg_full_search(query = "cruise", topic = "Biosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_biosphere$doi)))
pg_EU_cruise_biosphere_dl <- plyr::ldply(pg_EU_cruise_biosphere$doi, pg_dl_proc)
pg_EU_cruise_biosphere_clean <- pg_EU_cruise_biosphere_dl %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_biosphere_clean)
data.table::fwrite(pg_EU_cruise_biosphere_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Biosphere.csv")
data.table::fwrite(pg_EU_cruise_biosphere_clean, "data/pg_data/pg_EU_cruise_Biosphere.csv")
rm(pg_EU_cruise_biosphere_dl, pg_EU_cruise_biosphere_clean); gc()


## EU Arctic cruise Ecology data on PANGAEA - 674 - Some issues
## NB: Takes ~19 minutes
pg_EU_cruise_ecology <- pg_full_search(query = "cruise", topic = "Ecology", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_ecology$doi)))
pg_EU_cruise_ecology_dl <- plyr::ldply(pg_EU_cruise_ecology$doi, pg_dl_proc)
pg_EU_cruise_ecology_clean <- pg_EU_cruise_ecology_dl %>%
  dplyr::select(-contains(c("Perc total form", " indet", " cysts", "Reads ", "Alb ", "C upt ", 
                            "Parameter ", "Vol ", "Fatty acids ", "Grain size",
                            "Phae", "Turbellaria", "Tanaidacea", "Signal", "Taxa", "Polymer ",
                            "Persistent Identifier", "morphospecies", "Reference",
                            "Flag", "δ30Si", "Certainty", "Sensor", "Acryloni"))) %>% 
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_ecology_clean)
data.table::fwrite(pg_EU_cruise_ecology_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Ecology.csv")
data.table::fwrite(pg_EU_cruise_ecology_clean, "data/pg_data/pg_EU_cruise_Ecology.csv")
rm(pg_EU_cruise_ecology_dl, pg_EU_cruise_ecology_clean); gc()


## EU Arctic cruise Human Dimensions data on PANGAEA - 0
pg_EU_cruise_human <- pg_full_search(query = "cruise", topic = "Human Dimensions", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)


## EU Arctic cruise Chemistry data on PANGAEA west - 3798 - Some issues
## MB ~16 minutes
pg_EU_cruise_chemistry_west <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(-60, 63, 0, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_west$doi)))
pg_EU_cruise_chemistry_west_dl <- plyr::ldply(pg_EU_cruise_chemistry_west$doi, pg_dl_proc)
pg_EU_cruise_chemistry_west_clean <- pg_EU_cruise_chemistry_west_dl %>%
  # select(-contains(c("Lu_", "Ed_", "Es_", "QC ", "OD", "ac std dev ",
  #                    "ac1", "ac2", "ac3", "ac4", "ac5", "ac6", "ac7", "ac8", "ac9"))) %>% 
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_chemistry_west_clean)
data.table::fwrite(pg_EU_cruise_chemistry_west_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_west.csv")
data.table::fwrite(pg_EU_cruise_chemistry_west_clean, "data/pg_data/pg_EU_cruise_Chemistry_west.csv")
rm(pg_EU_cruise_chemistry_west_dl, pg_EU_cruise_chemistry_west_clean); gc()


## EU Arctic cruise Chemistry data on PANGAEA east - 7989 - Some issues
## NB: ~58 minutes
pg_EU_cruise_chemistry_east <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(0, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_east$doi)))
pg_EU_cruise_chemistry_east_dl <- plyr::ldply(pg_EU_cruise_chemistry_east$doi, pg_dl_proc)
pg_EU_cruise_chemistry_east_clean <- pg_EU_cruise_chemistry_east_dl %>%
  select(-contains(c("Flag ", "acCDOM "))) %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_chemistry_east_clean)
data.table::fwrite(pg_EU_cruise_chemistry_east_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_east.csv")
data.table::fwrite(pg_EU_cruise_chemistry_east_clean, "data/pg_data/pg_EU_cruise_Chemistry_east.csv")
rm(pg_EU_cruise_chemistry_east_dl, pg_EU_cruise_chemistry_east_clean); gc()


## EU Arctic CTD data on PANGAEA - 923 - Some issues
## NB: ~20 minutes
pg_EU_CTD <- pg_full_search(query = "CTD", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_CTD$doi)))
pg_EU_CTD_dl <- plyr::ldply(pg_EU_CTD$doi, pg_dl_proc)
pg_EU_CTD_clean <- pg_EU_CTD_dl %>%
  select(-contains(c("dpm/m", "Perc ", "Ratio ", "14C ", "Bact ", "heterotrophic", "DSI", "kappa",
                     "University", "Woods ", "Geological", "GSI", "HSI", "C15", "C17", "C19",
                     "Dinosterol", "Brassicasterol", "Foram", "Calibration ", "Chronozone",
                     "T tech ", "delta T ", "Species", "Spec code", "Chromatographic", "NOBS",
                     "Method comm ", "Mortality", "Microscopy", "Bacteria", "Pore size", 
                     "umbilical", "/TOC", "size fraction", "Bathy ", "pixel ", "Data record",
                     "Prasinophytes", "Chrysophytes", "MATLAB", "Total counts", "Locality",
                     "T-RF", "algae", "Picophytopl", "Cryptophytes", "δ13C", "Comm resp ", "Size ",
                     "Phylum", "Kingdom", "alpha ", "Rotaliina", "bacp660 ", "Pyrophytin a ",
                     "Phytin a ", "Phide a ", "Grain size ", "Status", "Sidesc", "Haptophytes ", 
                     "/Chl a", "ASAL ", "Fe ", "Sensitivity", "POC", "GCP", "Aphanizo ", "Conf ",
                     "Phytopl ", "CO3", "CSC", "Biogeograph", "E bur ", "DOF", "POC", "Seastate",
                     "228Ra", "228Th", "Transmission ", "beta470", "bac660 ", "DOS ", "LSi ",
                     "Mn2+ ", "Al ", "Ca ", "Phae ", "Acetate ", "C carb part ", "Sigma500",
                     "Fragilariopsis", "CSP", " TEP", "ESD", "SA dome ", "Vol dome ", "Catalog",
                     "Phaeodaria", "Dated material", "Age dated", "Basis", "Microzoopl", "δ18O",
                     "Frac Factor"))) %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_CTD_clean)
write_csv(pg_EU_CTD_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_CTD.csv")
write_csv(pg_EU_CTD_clean, "data/pg_data/pg_EU_CTD.csv")
rm(pg_EU_CTD_dl, pg_EU_CTD_clean); gc()

# Load file
# pg_EU_ctd_all_clean <- data.table::fread("~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_ctd_all.csv", nThread = 15)

# Save DOI list
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Kongsfjorden ------------------------------------------------------------

## Salinity: Mooring. Scottish Association of Marine Sciences (SAMS): 
# Haakon Hop (NP) Finlo Cottier (SAMS) Jørgen Berge (UiT, Tromsø)

## Temperature: Mooring. Scottish Association of Marine Sciences (SAMS): 
# Haakon Hop (NP) Finlo Cottier (SAMS) Jørgen Berge (UiT, Tromsø)

## Carbonate chemistry: pH, DIC, TA, pCO2. Available upon request:
# http://www.obs-vlfr.fr/~gattuso/data/awipev-CO2_web.html

## Nutrients: Vertical profiles. NPI
# https://data.npolar.no/dataset/c9de2d1f-54c1-49ca-b58f-a04cf5decca5

## Seabirds: Abundance, vital rates, diet. NPI, S Descamps.

## Macroalgae: Along fjord axis. Hop et al. 2016
# https://github.com/MikkoVihtakari/MarineDatabase

## All Kongsfjorden bbox data files - 3227
pg_kong_bbox <- pg_full_search(query = "", bbox = c(11, 78.86, 12.69, 79.1)) %>% # 2852 files
  filter(!doi %in% pg_doi_list$doi)
pg_kong_name_1 <- pg_full_search(query = "kongsfjord") %>% # 7 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi)
pg_kong_name_2 <- pg_full_search(query = "kongsfjorden") %>% # 13 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi)
pg_kong_name_3 <- pg_full_search(query = "ny alesund") %>% # 11 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, !doi %in% pg_kong_name_2$doi)
pg_kong_name_4 <- pg_full_search(query = "ny-alesund") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, 
         !doi %in% pg_kong_name_2$doi, !doi %in% pg_kong_name_3$doi)
pg_kong_all <- rbind(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4); gc()

# Download files
pg_kong_dl <- plyr::ldply(pg_kong_all$doi, pg_dl_proc)
# table(pg_kong_dl$citation) # Investigate which files contribute the most size
pg_kong_clean <- pg_kong_dl %>%
  select(-contains(c("dN/dlogDp", "MeSO2-PCB ", "3-MeSO2-pp-DDE ", "Bryozoa",
                     " cov ", " biom wm ", "AND ", "16b-OHT ", "6b-OHT ", "δ18O", "δ13C",
                     "Fatty alc ", "NOBS ", "Method comm ", "Particles ", "Flag",
                     "juveniles", "Description", "Species", "#/m", "ID ", "clays",
                     "g/m", " indet ", "Size algal disc ")))
                     # , "Perc ", "Particles ",
                     # "Size algal disc ", "Pogonophora", "Polychaeta", "Tunicata", 
                     # "Sipunculida", "Crustacea", "Tablets ", "Taxa", "Flag",
                     # "/TOC", "Turbellaria", "Tachidiidae", "Oligochaeta", "Harpacticoida",
                     # "Oedicerotidae", "Persistent Identifier", "Data ", "Fossils")))
colnames(pg_kong_clean)
data.table::fwrite(pg_kong_clean, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong.csv")
data.table::fwrite(pg_kong_clean, "data/pg_data/pg_kong.csv")
rm(pg_kong_dl, pg_kong_clean); gc()

# Append DOI list
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Isfjorden ---------------------------------------------------------------

## All Isfjorden data files - 350
pg_is_bbox <- pg_full_search(query = "", bbox = c(13.62, 78.03, 17.14, 78.71)) %>% # 171 files
  filter(!doi %in% pg_doi_list$doi)
pg_is_name_1 <- pg_full_search(query = "isfjord") %>% # 10 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi)
pg_is_name_2 <- pg_full_search(query = "isfjorden") %>% # 2 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi)
pg_is_name_3 <- pg_full_search(query = "longyearbyen") %>% # 369 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi, !doi %in% pg_is_name_2$doi)
pg_is_all <- rbind(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3); gc()

# Download files
pg_is_dl <- plyr::ldply(pg_is_all$doi, pg_dl_proc)
pg_is_clean <- pg_is_dl %>%
  select(-contains(c("Diatom zone ", "Diatom spore ", "Diatoms indet ", "Liradiscus sp",
                     " indet ", "[#]", "Thick [mm]", "Biom std", "Plants vasc biom",
                     "Arom HC", "EF ", "N subset", "LCC ", "Perc ", "dN/dlogDp ",
                     "#/m", "#/10", "Sum [%]", "Method comm ")))
colnames(pg_is_clean)
data.table::fwrite(pg_is_clean, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is.csv")
data.table::fwrite(pg_is_clean, "data/pg_data/pg_is.csv")
rm(pg_is_dl, pg_is_clean); gc()

# Update DOI list with Isfjorden
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_is_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Inglefieldbukta ---------------------------------------------------------

## All Inglefieldbukta data files - 15
pg_ingle_bbox <- pg_full_search(query = "", bbox = c(18.15, 77.87, 18.79, 78.05)) %>% # 15 files
  filter(!doi %in% pg_doi_list$doi)
# pg_ingle_name_1 <- pg_full_search(query = "inglefieldbukta") # 0 files
pg_ingle_all <- rbind(pg_ingle_bbox) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_ingle_bbox, pg_ingle_name_1); gc()

# Download files
pg_ingle_dl <- plyr::ldply(pg_ingle_all$doi, pg_dl_proc)
pg_ingle_clean <- pg_ingle_dl %>%
  select(-contains(c("N subset ", "OH-PCB ")))
colnames(pg_ingle_clean)
data.table::fwrite(pg_ingle_clean, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle.csv")
data.table::fwrite(pg_ingle_clean, "data/pg_data/pg_ingle.csv")
rm(pg_ingle_dl, pg_ingle_clean); gc()

# Update DOI list with Inglefieldbukta
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_ingle_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Svalbard ----------------------------------------------------------------

## All Svalbard data files - 271
# NB: These files were searched for after the specific sites intentionally
# This was so that site specific files would be allocated to the appropriate folders
# And the files not attributed to a given site would be downloaded in this chunk
# However, I'm currently thinking we don't want these data...
pg_sval_all <- pangaear::pg_search(query = "svalbard", count = 500, bbox = c(9, 76, 30, 81)) %>% 
  filter(!grepl("core", citation), !grepl("video", citation), !grepl("photograph", citation)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>%  arrange(citation)

# Svalbard team files - 10 datasets


# Young Sound -------------------------------------------------------------

## All Young Sound data files - 209
pg_young_bbox <- pg_full_search(query = "", bbox = c(-22.367917, 74.210137, -19.907644, 74.624304)) %>% # 206 files
  filter(!doi %in% pg_doi_list$doi)
pg_young_name_1 <- pg_full_search(query = "zackenberg") %>% # 5 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_young_bbox$doi)
pg_young_all <- rbind(pg_young_bbox, pg_young_name_1) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation),
         # This file causes weird date issues and doesn;t have any key drivers
         !doi == "10.1594/PANGAEA.786674") %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_young_bbox, pg_young_name_1); gc()

# Download files
pg_young_dl <- plyr::ldply(pg_young_all$doi, pg_dl_proc)
pg_young_clean <- pg_young_dl %>%
  select(-contains(c("Tephra/", "LCC ", "Coeff ","ICP-MS")))
colnames(pg_young_clean)
data.table::fwrite(pg_young_clean, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young.csv")
data.table::fwrite(pg_young_clean, "data/pg_data/pg_young.csv")
rm(pg_young_dl, pg_young_clean); gc()

# Update DOI list with Young Sound
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_young_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Disko Bay ---------------------------------------------------------------

## All Disko Bay data files - 255
pg_disko_bbox <- pg_full_search(query = "", bbox = c(-55.56, 68.22, -49.55, 70.5)) %>% # 243 files
  filter(!doi %in% pg_doi_list$doi)
pg_disko_name_1 <- pg_full_search(query = "Qeqertarsuup") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi)
pg_disko_name_2 <- pg_full_search(query = "disko bay") %>% # 12 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi)
pg_disko_name_3 <- pg_full_search(query = "disko_bay") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi, !doi %in% pg_disko_name_2$doi)
pg_disko_all <- rbind(pg_disko_bbox, pg_disko_name_1, pg_disko_name_2, pg_disko_name_3) %>% 
  arrange(citation) %>% 
  filter(!grepl("sea level", citation),
         !doi %in% c("10.1594/PANGAEA.770250", # These two files are too massive
                     "10.1594/PANGAEA.770249")) %>% #,
                     # "10.1594/PANGAEA.847501", # These two have broken date columns that can't be reconciled
                     # "10.1594/PANGAEA.905012")) %>% 
  distinct()
rm(pg_disko_bbox, pg_disko_name_1, pg_disko_name_2, pg_disko_name_3); gc()

# Download files
pg_disko_dl <- plyr::ldply(pg_disko_all$doi, pg_dl_proc)
# table(pg_disko_dl$citation) # Investigate which files contribute the most size
pg_disko_clean <- pg_disko_dl %>%
  select(-contains(c("N subset ", "[#/m**3]", "Coeff (", "Grazing imp")))
colnames(pg_disko_clean)
data.table::fwrite(pg_disko_clean, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko.csv")
data.table::fwrite(pg_disko_clean, "data/pg_data/pg_disko.csv")
rm(pg_disko_dl, pg_disko_clean); gc()

# Update DOI list with Disko Bay
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_disko_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Nuup Kangerlua ----------------------------------------------------------

## All Nuup Kangerlua data files - 232
pg_nuup_bbox <- pg_full_search(query = "", bbox = c(-53.32, 64.01, -48.93, 64.8)) %>% # 158 files
  filter(!doi %in% pg_doi_list$doi)
pg_nuup_name_1 <- pangaear::pg_search(query = "kangerlua", count = 500) # 0 files
pg_nuup_name_2 <- pangaear::pg_search(query = "nuuk", count = 500) %>% # 149 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_nuup_bbox$doi)
pg_nuup_all <- rbind(pg_nuup_bbox, pg_nuup_name_1, pg_nuup_name_2) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_nuup_bbox, pg_nuup_name_1, pg_nuup_name_2); gc()

# Download files
pg_nuup_dl <- plyr::ldply(pg_nuup_all$doi, pg_dl_proc)
pg_nuup_clean <- pg_nuup_dl %>%
  select(-contains(c("Grain size", "Atterberg", "Missing ", 
                     "Diat spec", "Diatoms", "Isotope", "X-ray flu",
                     "Age date", "Age unc", "Age max", "Age min",
                     "Hiatus ", "umbilical", "spiral", "Persistent Identifier")))
colnames(pg_nuup_clean)
data.table::fwrite(pg_nuup_clean, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup.csv")
data.table::fwrite(pg_nuup_clean, "data/pg_data/pg_nuup.csv")
rm(pg_nuup_dl, pg_nuup_clean); gc()

# Update DOI list with Nuup Kangerlua
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_nuup_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Porsangerfjorden --------------------------------------------------------

## All Porsangerfjorden data files - 104
pg_por_bbox <- pg_full_search(query = "", bbox = c(24.5, 70, 27, 71.2)) %>% # 104 files
  filter(!doi %in% pg_doi_list$doi)
pg_por_name_1 <- pangaear::pg_search(query = "Porsangerfjord", count = 500) %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_por_bbox$doi)
pg_por_all <- rbind(pg_por_bbox, pg_por_name_1) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_por_bbox, pg_por_name_1); gc()

# Download files
pg_por_dl <- plyr::ldply(pg_por_all$doi, pg_dl_proc)
pg_por_clean <- pg_por_dl %>%
  select(-contains(c("Lu_", "fec pel", "EF ", "LCC ", "Biomass live", "‰ PDB",
                     "Bq/kg", "X-ray fluor", "ng/m", "Isotope ratio", "/Al",
                     "Instrumental neutron", "Flameless atomic")))
colnames(pg_por_clean)
data.table::fwrite(pg_por_clean, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por.csv")
data.table::fwrite(pg_por_clean, "data/pg_data/pg_por.csv")
rm(pg_por_dl, pg_por_clean); gc()

# Update DOI list with Porsangerfjord
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_por_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

