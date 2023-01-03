# code/key_drivers.R
# This script contains the queries used for scraping PANGAEA


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
query_Meta <- rbind(query_longitude, query_latitude, query_date, query_depth) %>% distinct()
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
# River discharge (river, discharge)
query_river <- query_params("river|discharge", "Diatoms|smoke|Dust|pixel|Riversideite", "#|±")
# Snow cover
query_snow <- query_params("snow")
# Permafrost
query_permafrost <- query_params("permafrost")
# Combined
query_Cryosphere <- rbind(query_ice, query_glacier, query_river, query_snow, query_permafrost) %>% distinct()
rm(query_ice, query_glacier, query_river, query_snow, query_permafrost); gc()


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
query_light <- query_params("extinction|turbidity|photosynthetic", 
                            "aerosol|foraminifera")
# Mixed layer depth (mixed, MLD)
query_MLD <- query_params("mixed layer|mixed-layer|mixedlayer", "Foraminifera|Illite|clay|smectite")
# Salinity (sal, psu)
query_salinity <- query_params("salinity", "Diatoms|Dinoflagellate|Radium|Snow|Treatment", "±")
# Sea level pressure (slp)
query_slp <- query_params("pressure", no_units = "±|dbar",
                          "Argon|Blood|Cell|partial pressure|Fouling|laboratory|experiment|Vapour|velocity|Sound")
# Sedimentation rate (sedim)
query_sedimentation <- query_params("sedimentation") 
# Suspended matter: organic, mineral (pom, pim, som, spm)
query_suspended <- query_params("suspended", "Backscattering", "±")
# (Seawater+air) temperature: surface, mid, bottom (°C, temp, sst)
query_temperature <- query_params("temperature", no_units = "±|K/100m",
                                  "Acid|Body|Fugacity|processes|Number|partial pressure|atoms|treatment|xCO2|δ|
                                  |Quality flag|Sonic")
# Wind: direction, speed (wind, speed, direction, u, v)
query_wind <- query_params("wind|speed|direction", 
                           "Sigma|window|Aurelia|bed dip|Brightness|cloud|Coiling|Current|deform|Gamete|
                           |Growing|ice |ice-|sperm|pixel|Plastic|polen|Predator|prey|Ship|snow|swim|swell|
                           |temperature|Tidal|Towing|wave", "±")
# combined
query_Physical <- rbind(query_current, query_evap_precip, query_heatflux, query_light, query_MLD, query_salinity, 
                        query_slp, query_sedimentation, query_suspended, query_temperature, query_wind) %>% distinct()
rm(query_current, query_evap_precip, query_heatflux, query_light, query_MLD, 
   query_salinity, query_slp, query_sedimentation, query_suspended, query_temperature, query_wind); gc()


## Chemistry
# CaCO3 saturation state (CaCO3, Arg, Ara, Cal, omega)
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
query_pCO2 <- query_params("partial pressure|pco2|co2", 
                           "Blood|Coelomic|Extrapallial|Haemolymph|oxygen|Methane|nitro|Ozone|Treatment|vapour|gene|xCO2|δ13", "±")
# pH (ph)
query_pH <- query_params("pH", 
                         "Calcifying|Coelomic|Extrapallial|Haemolymph|Metabolic|cellular|periv|seminal|soil|treatment|voltage", 
                         "±|#", yes_case = F)
# Total alkalinity (TA, AT)
query_alkalinity <- query_params("alkalinity", "borate|chlorine|Coelomic", "±")
# Combined
query_Chemistry <- rbind(query_calc_carb, query_dissolved, query_oxygen, query_nutrients, query_pCO2, 
                         query_pH, query_alkalinity) %>% distinct()
rm(query_calc_carb, query_dissolved, query_oxygen, query_nutrients, query_pCO2, query_pH, query_alkalinity); gc()

## Biology
# Chlorophyll
query_chl <- query_params("chlorophyll", no_units = "±")
# Calcification
query_calcification <- query_params("calcification", no_units = "±")
# Nitrogen fixation
query_nitro_fix <- query_params("Nitrogen fixation", no_units = "±")
# Photosynthesis
query_photosynthesis <- query_params("Photosynthesis", "Carbon-14", "±")
# Primary production
query_prim_prod <- query_params("Primary production", no_units = "±")
# Respiration
query_respiration <- query_params("Community respiration", no_units = "±")
# Species: presence/absence, abundance/biomass
query_species <- filter(pg_parameters, grepl(sp_abb_one, Abbreviation)) %>% 
  mutate(pg_col_name = case_when(!is.na(Unit) ~ paste0(Abbreviation," [",Unit,"]"),
                                 TRUE ~ Abbreviation))
# Combined
# NB: Specifically not adding 'query_species' here as this is handled differently due to data width
query_Biology <- rbind(query_chl, query_calcification, query_nitro_fix, query_photosynthesis, query_prim_prod, 
                       query_respiration) %>% distinct()
rm(query_chl, query_calcification, query_nitro_fix, query_photosynthesis, query_prim_prod, query_respiration); gc()

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
query_ALL <- rbind(query_Meta, query_Cryosphere, query_Physical, query_Chemistry, query_Biology, query_Social) %>% distinct()
# NB: Specifically not removing 'query_meta'
rm(query_Cryosphere, query_Physical, query_Chemistry, query_Biology, query_Social, 
   pg_parameters, sp_abb_one, sp_abb_sep, query_params); gc()

