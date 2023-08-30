# code/key_drivers.R
# This script contains the queries used for scraping PANGAEA


# Key drivers -------------------------------------------------------------

# Load PANGAEA driver metadata sheet
if(!exists("pg_parameters")) pg_parameters <- read_tsv("metadata/pangaea_parameters.tab")

# Aids for better filtering
if(!exists("sp_abb_one")) sp_abb_one <- paste0(c(paste0(LETTERS,"[.] "), "sp[.]", "spp[.]"), collapse = "|")
if(!exists("sp_abb_sep")) sp_abb_sep <- paste0(LETTERS,". ")

# Function for consistent search word processing
if(!exists("query_params")){
  query_params <- function(driver_name, yes_words, no_words = NA, no_units = NA, yes_case = T){
    res <- pg_parameters |>  
      filter(grepl(yes_words, Parameter, ignore.case = yes_case),
             !grepl(sp_abb_one, Abbreviation))
    if(!is.na(no_words)){
      res <- res |> filter(!grepl(no_words, Parameter, ignore.case = T))
    }
    if(!is.na(no_units)){
      res <- res |> filter(!grepl(no_units, Unit))
    }
    res <- res |> 
      mutate(pg_col_name = case_when(!is.na(Unit) ~ paste0(Abbreviation," [",Unit,"]"),
                                     TRUE ~ Abbreviation),
             driver = driver_name)
    return(res)
  } 
}

## Metadata variables
if(!exists("query_meta")){
  # Longitude
  query_longitude <- query_params("lon", "longitude", "file ")
  # Latitude
  query_latitude <- query_params("lat", "latitude", "file ")
  # Date/Time
  query_date <- query_params("date", "date", "age|consolidated|birth|death|germination|iodate|cordate|MATLAB|file ")
  # Depth
  query_depth <- query_params("depth", "depth|bathymetry|pressure|density|elevation",
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
  query_meta <- rbind(query_longitude, query_latitude, query_date, query_depth) |> mutate(category = "meta")
  rm(query_longitude, query_latitude, query_date, query_depth); gc()
}

## Cryosphere
if(!exists("query_cryo")){
  # Coastal ice; Fast ice; Sea ice; snow cover
  query_sea_ice <- query_params("sea ice", "ice|snow", 
                                "abies|aegiceras|aminicenantes|avicenni|biosiliceous|bryozoa|calcite|cf[.]|Chvaleticeite|
                                |cicendia|cicer|cichoriceae|cribricellina|Cricetidae|Cunoniceae|Cymatiosphaera|Daphne|Dehydroi|
                                |device|Diatoms|Digalac|foraminifera|Galact|Griceite|Hepaticeae|lattice|laonice|leontice|
                                |Lonicera|Macellice|methyl|Monticellite|Oedicerotidae|Ovicell|Paniceae|Picea|Pluricell|distance|
                                |Pseudotrice|Pumice|price|quartz|Radicel|Sabicea|Scolecith|Siliceous|Stauroneis|statice|
                                |volcanic ash|Tetragonic|Timeslice|Tree-ring|Trifolium|Ultraviolet|Unicellular|Urticeae|Zelkova") 
  # Glacier mass balance and other measurements
  query_glacier <- query_params("glacier", "glacier|glacial", "Foraminifera|glacialis")
  # River discharge (river, discharge) including permafrost
  query_runoff <- query_params("runoff", "river|discharge|permafrost", "Diatoms|smoke|Dust|pixel|Riversideite", "#|±")
  # Combined
  query_cryo <- rbind(query_sea_ice, query_glacier, query_runoff) |> mutate(category = "cryo")
  rm(query_sea_ice, query_glacier, query_runoff); gc()
}

## Physical
if(!exists("query_phys")){
  # Seawater temperature: surface, mid, bottom (°C, temp, sst)
  query_sea_temp <- query_params("sea temp", "temperature",
                                 "Acid|Body|Fugacity|processes|Number|partial pressure|atoms|treatment|xCO2|δ|
                                 |Quality flag|Sonic|Acclimation|air | air|atmospher|gas|ice|snow|
                                 |index|soil|sediment|zircon",
                                 "±|K/100m")
  # Salinity (sal, psu); NB: Adding"PSU" to search is not helpful
  query_salinity <- query_params("salinity", "salinity", 
                                 "Diatoms|Dinoflagellate|Radium|Snow|Treatment|Paleosalinity", "±")
  # Light extinction coefficient (kd, absorption); NB: "absorption" not used because of how wide those data are
  query_light <- query_params("light", "photosynthetic|UV-B|extinction|turbidity|sedimentation|suspended", 
                              "aerosol|foraminifera|juvenile|bacteri|partial pressure|Aluminium|Backscattering|
                              |acid|Apparent|Dichloro", 
                              "#|‰ air|Ohm m")
  # combined
  query_phys <- rbind(query_sea_temp, query_salinity, query_light) |> mutate(category = "phys")
  rm(query_sea_temp, query_salinity, query_light); gc() 
}

## Chemistry
if(!exists("query_chem")){
  # Carbonate chemistry
  # CaCO3 saturation state (CaCO3, Arg, Ara, Cal, omega)
  # Partial pressure of CO2 (pCO2)
  # Dissolved inorganic carbon (DIC); Dissolved organic carbon (DOC); Dissolved organic nitrogen (DON)
  # Total alkalinity (TA, AT)
  query_carb <- query_params("carb", 
                             "calcium carbonate|CaCO3|aragonite|omega|
                             |partial pressure|pco2|co2|
                             |dissolved inorganic carbon|dissolved organic carbon|dissolved organic nitrogen| 
                             |alkalinity",
                             "δ|Blood|Coelomic|Extrapallial|Haemolymph|oxygen|Methane|nitro|Ozone|Treatment|vapour|gene|
                             |xCO2|δ13|borate|chlorine|Coelomic|Paragonite", 
                             "±|#")
  # NB: Run pH separately in able to force the capital case of the word
  query_pH <- query_params("carb", "pH", 
                           "Calcifying|Coelomic|Extrapallial|Haemolymph|Metabolic|cellular|periv|seminal|soil|treatment|voltage", 
                           "±|#", yes_case = F)
  
  # Nutrients
  query_nutrients <- query_params("nutrients", "nitrate|nitrite|ammonium|phosphate|silicate", 
                                  "Adenosine|Affinity|Alkalin|Aluminosilicate|soil|Bacteria|Calcium|Mannose|Cytidine|
                                  |Ethyl|hydrosilicate|Guanosine|Haemolymph|Inverse|Isopropyl|Lithiophosphate|Lithium|
                                  |Mesozooplankton|Natrophosphate|Nicotinamide|non-silicates|propyl|Ortho|oxide|
                                  |carboxylase|Phosphorus|Phyllosilicate|Ribulose|butyl|Thymidine|oxyradical|Tributyl|
                                  |Tricresyl|Triisobutyl|Triphenyl|Triphosphates|Uridine|δ15|Δ17|δ18", 
                                  "±")
  
  # Combined
  query_chem <- rbind(query_carb, query_pH, query_nutrients) |> mutate(category = "chem")
  rm(query_carb, query_pH, query_nutrients); gc()
}

## Biology
if(!exists("query_bio")){
  # Primary production
  # Primary production; chlorophyll; Calcification; Nitrogen fixation; Photosynthesis; Respiration
  query_prim_prod <- query_params("prim prod", "Primary production|chlorophyll|calcification|nitrogen fixation|
                                  |Photosynthesis|community respiration", no_units = "±")
  # Species: presence/absence, abundance/biomass
  query_species <- filter(pg_parameters, grepl(sp_abb_one, Abbreviation)) %>% 
    mutate(pg_col_name = case_when(!is.na(Unit) ~ paste0(Abbreviation," [",Unit,"]"),
                                   TRUE ~ Abbreviation), 
           driver = "biomass", category = "bio")
  # Combined
  # NB: Specifically not adding 'query_species' here as this is handled differently due to data width
  query_bio <- rbind(query_prim_prod) |> mutate(category = "bio")
  rm(query_prim_prod); gc()
}

## Social
if(!exists("query_soc")){
  # Governance
  # Local and national resource management
  # National statistics: demography, income, unemployment
  query_gov <- query_params("gov", "management|demography|income|unemployment")
  # Tourism
  # Tourist arrivals: per month, nationality
  # Tourist vessels: count, mileage
  query_tourism <- query_params("tourism", "touris|nationality|vessel|mileage")
  # Fisheries
  # Fish landings: commercial, recreational, quotas, seasonality
  query_fisheries <- query_params("fisheries", "landings")
  # Combine
  query_soc <- rbind(query_gov, query_tourism, query_fisheries) |> mutate(category = "soc")
  rm(query_gov, query_tourism, query_fisheries); gc()
}

## All variables together
if(!exists("query_ALL")) query_ALL <- rbind(query_meta, query_cryo, query_phys, query_chem, query_bio, query_soc)


# Old queries -------------------------------------------------------------
# NB: These queries are from a pre v1.0 pipeline
# They have been kept here in case a FACE-IT member is looking for these
# types of data specifically, but they are outside of the scope of WP1

# Snow cover
# query_snow <- query_params("snow")

# Permafrost
# query_permafrost <- query_params("permafrost")

# Current: direction, location, volume (current, vel, direction, vol, u , v )
# query_current <- query_params("current|velocity|direction|volume", 
#                               "Air |Aircraft|Angle|Aggregates|Anhysteretic|Biomass|fecal|isotop|ARM, |
#                               |pipe|Back-IRM|bed dip|Biovolume|Blue light|brightness|Calcite|Calcium|Carbon|
#                               |Cardiac|Cell|Chloro|Cloud|electrical|occupational|Deformation|ribonucleic|
#                               |frequency|wind|dry|dust|egg|flux|hydrate|glacier|glacial|Hard-IRM|heat|Heterotrophic|
#                               |Hysteresis|ice |ice-|Incubation|iodine|Gonad|Diatoms|Settling|ship|Green light|
#                               |description|iron|journal|corpuscular|Methane|molar|Nectar|Nodule|Organic|roll|
#                               |Oxic|Oxid|Oxyd|Oxyg|Ozone|Particle|Phytoplankton|Piston|biphenyl|Porosity|Pteropoda|
#                               |reservoir|Root|sample|Sinking|Soil|sonic|sound|Stroke|backscattering|Susceptibility|
#                               |bladder|chamber|Tintinnid|tissue|tree|Ventilatory|lava|percentage|wave|zooplankton",
#                               "#|pg/ml|µl/l|ml/l|nmol/l|ng/ml|µm|±|mg/cm|µg/m|db|pA/m|arbitrary|nmol|µl")

# Evaporation/Precipitation: (evap, precip, e-p, p-e)
# query_evap_precip <- query_params("evaporation|precipitation", "δ")

# Heatflux: net, latent/sensible, long/shortwave radiation (Q, flux, latent, sensible, longwave, shortwave, radiation)
# query_heatflux <- query_params("heatflux|heat-flux|heat flux|latent|sensible|
#                                |longwave|long-wave|long wave|shortwave|short-wave|short wave")

# Mixed layer depth (mixed, MLD)
# query_MLD <- query_params("mixed layer|mixed-layer|mixedlayer", "Foraminifera|Illite|clay|smectite")

# Sea level pressure (slp)
# query_slp <- query_params("pressure", no_units = "±|dbar",
#                           "Argon|Blood|Cell|partial pressure|Fouling|laboratory|experiment|Vapour|velocity|Sound")

# Sedimentation rate (sedim)
# query_sedimentation <- query_params("sedimentation") 

# Suspended matter: organic, mineral (pom, pim, som, spm)
# query_suspended <- query_params("suspended", "Backscattering", "±")

# (Seawater+air) temperature: surface, mid, bottom (°C, temp, sst)
# query_temperature <- query_params("temperature", no_units = "±|K/100m",
#                                   "Acid|Body|Fugacity|processes|Number|partial pressure|atoms|treatment|xCO2|δ|
#                                   |Quality flag|Sonic")

# Wind: direction, speed (wind, speed, direction, u, v)
# query_wind <- query_params("wind|speed|direction", 
#                            "Sigma|window|Aurelia|bed dip|Brightness|cloud|Coiling|Current|deform|Gamete|
#                            |Growing|ice |ice-|sperm|pixel|Plastic|polen|Predator|prey|Ship|snow|swim|swell|
#                            |temperature|Tidal|Towing|wave", "±")

# Dissolved O2 (DO, O2)
# query_oxygen <- query_params("oxygen", 
#                              "Aerobic|demand|oxygenase|respiration|Foraminifer|Biological|carbon|chamber|Community|
#                              |Electron|exercise|fecal|Fluorescence|chlorophyll|photosynthesis|primary production|
#                              |Haemolymph|hydrod|leaf|Mesozooplankton|Metabolic|Mitochondria|Mollusca|consumption|
#                              |Nitrogen|utilization|Argon|uptake|Photosynthetic|species|Seston|swim|isotope",
#                              "#|±")

# Chlorophyll
# query_chl <- query_params("chlorophyll", no_units = "±")

# Calcification
# query_calcification <- query_params("calcification", no_units = "±")

# Nitrogen fixation
# query_nitro_fix <- query_params("Nitrogen fixation", no_units = "±")

# Photosynthesis
# query_photosynthesis <- query_params("Photosynthesis", "Carbon-14", "±")

# Primary production
# query_prim_prod <- query_params("Primary production", no_units = "±")

# Respiration
# query_respiration <- query_params("Community respiration", no_units = "±")

# Species: presence/absence, abundance/biomass
# query_species <- filter(pg_parameters, grepl(sp_abb_one, Abbreviation)) %>% 
#   mutate(pg_col_name = case_when(!is.na(Unit) ~ paste0(Abbreviation," [",Unit,"]"),
#                                  TRUE ~ Abbreviation))

