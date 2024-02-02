# code/functions.R
# This script houses functions and other preparatory things used throughout the project


# Setup -------------------------------------------------------------------

source("code/metadata.R")


# Functions ---------------------------------------------------------------

# Rename variables to a common project term
# Tester...
# df <- filter(full_ALL, category == "soc") |> dplyr::select(citation, category, driver, variable) |> distinct()
check_variable <- function(df){
  if(!exists("full_var_list")) full_var_list <- read_csv("metadata/full_var_list.csv")
  df_var <- df |> 
                                    # Cryo
    mutate(variable_new = case_when(variable == "Snow depth [m]" ~ "snow depth [m]",
                                    variable == "Sea ice snow thickness [cm]" ~ "sea ice snow thickness [cm]",
                                    # Phys
                                    variable %in% c("CDNC [S m-1]", "cond [S/m]", "conductivity [S/m]", "cndc [S m-1]") ~ "cndc [S m-1]",
                                    variable %in% c("cndc [mS/cm]", "cond [mS/cm]",
                                                    "conductivity [mS cm-1]", "conductivity [mS/cm]") ~ "cndc [mS cm-1]",
                                    variable %in% c("DEN [kg m-3]", "dens [kg/m3]", "density [kg/m^3]",
                                                    "density [kg/m3]", "density_kg_m3 [kg m-3]") ~ "density [kg m-3]",
                                    variable %in% c("dens_sigtheta [kg/m-3]", "sigma [kg/m3]", "Sigma-t", 
                                                    "sigmaT_kg_m3 [kg m-3]") ~ "density sigtheta [kg m-3]",
                                    variable %in% c("par_V [volt]") ~ "PAR [volt]",
                                    variable %in% c("par [micromol m-2 s-1]", "par [micromol photons / m^2 / s]",
                                                    "par [mmol/m-2]", "PAR [µmol/m**2/s/sr]", # TODO: Double check these
                                                    "PAR [umol m-2 s-1]", "PAR [umol m-2 s-1]",
                                                    "PAR [µmol photons m2/sec]", "PAR [µmol quanta/m**2/s]",
                                                    "PAR [µmol/m**2/s]", "PAR [µMol/m²/s]", "PAR [µmol/sec/m2]") ~ "PAR [µmol photons m-2 sec-1]",
                                    variable %in% c("PDEN [kg m-3]") ~ "pot density [kg m-3]",
                                    variable %in% c("pot_temp [°C]", "potential_temperature [°C]",
                                                    "theta [°C]", "THETA [°C]", "Tpot [°C]") ~ "pot temp [°C]",
                                    variable %in% c("pres [db]", "PRES [dbar]", "press [dbar]", "pressure [dbar]") ~ "pres [dbar]",
                                    variable %in% c("press [psi]", "pressure [psi]") ~ "pres [psi]",
                                    variable %in% c("psal [1e-3]", "PSAL [1e-3]", "sal [ppt]", "sal [PSS-78]",
                                                    "sal [PSU]", "Sal [PSU]", "Sal", "salinity", "Salinity",
                                                    "salinity [1]", "salinity [PSU]", "Salinity [PSU]") ~ "sal",
                                    variable %in% c("ASAL [g/kg]") ~ "sal [g kg-1]",
                                    variable %in% c("Temp [°C]", "TEMP [°C]", "temp [ITS-90]", "temperature [°C]",
                                                    "Temperature [ITS-90, deg C]") ~ "temp [°C]",
                                    variable == "Temp max [°C]" ~ "temp max [°C]",
                                    variable == "Temp min [°C]" ~ "temp min [°C]",
                                    variable %in% c("Tmax [°C]") ~ "temp max [°C]",
                                    variable %in% c("turb [NTU]", "Turbidity [NTU]") ~ "turbidity [NTU]",
                                    variable %in% c("Turbidity [FTU]") ~ "turbidity [FTU]",
                                    variable %in% c("turbidity [mg/l]") ~ "turbidity [mg l-1]",
                                    variable %in% c("turbidity_V [volt]") ~ "turbidity [volt]",
                                    variable %in% c("UV-A [W*m^2]") ~ "UVA [W m-2]",
                                    variable %in% c("UV-B [W*m^2]") ~ "UVB [W m-2]",
                                    # Chem
                                    variable %in% c("AT [µmol/kg]", "talk [μmol kg-1]") ~ "TA [µmol kg-1]",
                                    variable %in% c("DIC [µmol/kg]") ~ "DIC [µmol kg-1]",
                                    variable %in% c("doc [μmol L-1 d]") ~ "DOC [μmol l-1 d-1]",
                                    variable %in% c("EP TA [µmol/kg]") ~ "EP TA [µmol kg-1]",
                                    variable == "nitrate [μmol kg-1]" ~ "NO3 [µmol kg-1]",   
                                    variable == "nitrite [μmol kg-1]" ~ "NO2 [µmol kg-1]",   
                                    variable == "silicate [μmol kg-1]" ~ "SiO4 [µmol kg-1]",
                                    variable %in% c("phosphate [μmol kg-1]", "PO4 [µmol/kg]") ~ "PO4 [µmol kg-1]",
                                    variable %in% c("[NO3]- [µmol/l]",
                                                    # "NO3 [µmol/kg]", # Possible units issue
                                                    "NO3 [µg-at/l]") ~ "NO3 [µmol/l]", 
                                    variable %in% c("[PO4]3- [µmol/l]", "PO4 [µg-at/l]") ~ "PO4 [µmol l-1]",
                                    variable %in% c("[NH4]+ [µmol/l]", "[NH4]+ [µg-at/l]") ~ "NH4 [µmol l-1]",
                                    variable %in% c("[NO2]- [µmol/l]", "[NO2]- [µg-at/l]") ~ "NO2 [µmol l-1]",
                                    variable %in% c("nitrate+nitrite [µmol/l]", "[NO3]- + [NO2]- [µmol l-1]",
                                                    "NO2_NO3 [µmol l-1]") ~ "NO3+NO2 [µmol l-1]",
                                    # Bio
                                    variable == "chla [μg kg-1 d]" ~ "chl a [μg kg-1 d-1]",
                                    variable == "fluo_V [volt]" ~ "fluor [volt]",
                                    variable == "fluorescence" ~ "fluor",
                                    variable == "fluor_max [m]" ~ "fluor max [m]",
                                    variable == "chl_max [m]" ~ "chl max [m]",
                                    variable == "Biomass - Polar Cod [10^6 kg]" ~ "Polar Cod [10^6 kg]",
                                    # Soc
                                    variable == "trips [n/year]" ~ "trips [n year-1]",
                                    variable == "trips [n/month]" ~ "trips [n month-1]",
                                    TRUE ~ variable)) |> 
    mutate(variable_new = gsub("\\/l", "l-1", variable_new),
           variable_new = gsub("\\/kg", "kg-1", variable_new),
           variable_new = gsub("\\/h", "h-1", variable_new),
           variable_new = gsub("\\/d", "d-1", variable_new),
           variable_new = gsub("ice_T", "ice temp T", variable_new))
  
  # Report on changes
  df_no_var <- filter(df_var, variable == variable_new) |> 
    dplyr::select(variable, variable_new) |> distinct() |> 
    filter(!(variable %in% full_var_list$variable))
  if(nrow(df_no_var) > 0){
    cat("These variables were not able to be changed to fit the variable list, so they were removed: \n")
    print(df_no_var$variable)
  } else {
    cat("All variables accounted for.\n")
  }
  
  # Replace variable column and exit
  df_res <- df_var |> mutate(variable = variable_new) |> dplyr::select(-variable_new) |> 
    filter(variable %in% full_var_list$variable)
  return(df_res)
  # rm(df, df_var, df_no_var, df_res); gc()
}

# Correctly merge variables into categories and drivers
# Tester...
# df <- filter(full_ALL, category == "cryo")
check_driver <- function(df){
  if(!exists("full_var_list")) full_var_list <- read_csv("metadata/full_var_list.csv")
  if("category" %in% colnames(df)) df$category <- NULL
  if("driver" %in% colnames(df)) df$driver <- NULL
  df_driv <- df |> distinct() |> left_join(full_var_list, by = "variable")
  
  # Report on changes
  df_no_driv <- filter(df_driv, is.na(driver)) |> 
    dplyr::select(category, variable) |> distinct()
  if(nrow(df_no_driv) > 0){
    cat("These variables have no driver, so were removed: \n")
    print(df_no_driv$variable)
  } else {
    cat("All variables were matched to a category+driver.\n")
  }

  # Filter and exit
  df_res <- df_driv |> filter(!is.na(driver))
  return(df_res)
  # rm(df, df_driv, df_no_driv, df_res); gc()
}

# Looks at variable names within the bio category and assigns a species classification if possible
check_spp <- function(df, spp_type = "cat"){
  
  # Split dataframe by bio category
  df_other <- filter(df, category != "bio")
  df_PP <- filter(df, category == "bio", driver == "prim prod")
  df_not_PP <- filter(df, category == "bio", driver != "prim prod")
  
  # Exit if no species data
  if(nrow(df_not_PP) == 0){
    cat("No species data.\n")
    return(df)
  } else{
    df_not_PP <- df_not_PP |> 
      dplyr::select(variable) |> distinct() |> 
      separate(variable, into = c("var", "units"), sep = "\\[", remove = FALSE) |>
      separate(var, into = c("spp1", "spp2"), sep = " ", extra = "drop", remove = FALSE) |> 
      unite(spp1, spp2, col = "spp", sep = " ") |> 
      mutate(units = trimws(paste0("[",units)),
             spp = trimws(gsub(";| -", "", spp)))
  }
  
  # Quick check of taxonomy for a given dataset
  if(spp_type == "tax"){
    df_res <- plyr::ldply(df_not_PP$spp, wm_records_df, .parallel = T, res_type = "tax") |> distinct()
    return(df_res)
  }
  
  # Get unique species names and search WoRMS to determine broad categorisation
  df_cat <- plyr::ldply(df_not_PP$spp, wm_records_df, .parallel = T, res_type = "cat") |> distinct()
  
  # Report on changes
  df_no_cat <- filter(df_cat, is.na(cat)) |> 
    dplyr::select(species) |> distinct() |> arrange()
  if(nrow(df_no_cat) > 0){
    cat("These species were not found on WoRMS: \n")
    print(df_no_cat$species)
  } else {
    cat("All species were matched.\n")
  }
  
  # Add category back into dataframe, merge, and exit
  df_not_PP_cat <- left_join(df_not_PP, df_cat, by = c("spp" = "species")) |> 
    dplyr::select(variable, cat) |> distinct()
  df_res <- left_join(df, df_not_PP_cat, by = "variable") |> 
    mutate(variable = case_when(!is.na(cat) ~ paste0(cat," ",variable), TRUE ~ variable)) |> 
    dplyr::select(-cat)
  return(df_res)
  # rm(df, spp_type, df_other, df_PP, df_not_PP); gc()
}

# Convert sites to FACE-IT site names when possible
## NB: Site list was first created in the 'Site conversion' section of 'code/data_product.R' for v1.4
check_site <- function(df){
  if(!exists("full_site_list")) full_site_list <- read_csv("metadata/full_site_list.csv")
  df_site <- left_join(df, full_site_list, by = c("site" = "site_alt"))
  
  # Report on changes
  df_no_site <- filter(df_site, is.na(site.y)) |> 
    dplyr::select(site) |> distinct()
  if(nrow(df_no_site) > 0){
    cat("These sites could not be match, so were removed: \n")
    print(df_no_site$site)
  } else {
    cat("All sites were matched.\n")
  }
  
  # Filter and exit
  df_res <- df_site |> 
    filter(!is.na(site.y)) |> 
    mutate(variable = case_when(site != site.y ~ paste0(site," - ",variable), TRUE ~ variable),
           site = site.y) |> dplyr::select(-site.y, -site_long)
  return(df_res)
  # rm(df, df_site, df_no_site, df_res)
}

# Checks that a given dataset meets all of the project standards
# Tester...
# df <- filter(full_ALL, category == "soc")
check_data <- function(df, assign_site = NULL){
  
  # Convert variable names to a project standard
  df_var <- check_variable(df)
  
  # Add categories and drivers and note any issues
  df_driv <- check_driver(df_var)
  
  # Add species classifications and note any issues
  df_spp <- check_spp(df_driv)
  
  # Confirm site names and note any issues
  if(!is.null(assign_site)) df_driv$site <- assign_site
  df_site <- check_site(df_spp)
  
  # Finish and exit
  df_res <- dplyr::select(df_site, 
                          date_accessed, URL, citation, type, site, lon, lat, 
                          date, depth, category, driver, variable, value)
  return(df_res)
}

# Convenience wrapper to query clean dataset for specific files/DOI etc
query_clean <- function(df_clean, q_text, q_col = "URL"){
  if(q_col == "URL"){
    res <- dplyr::filter(df_clean, grepl(q_text, URL))
  } else if(q_col == "citation") {
    res <- dplyr::filter(df_clean, grepl(q_text, citation))
  }
  return(res)
}

# Add species classification
# NB: Currently under construction
spp_class <- function(){
  ECC_type  <- ECC_data %>% 
    mutate(classification = case_when(grepl("|FIS|", variable, fixed = TRUE) == TRUE ~ "Fish",
                                      grepl("|MAM|", variable, fixed = TRUE) == TRUE ~ "Mammal",
                                      grepl("|SBI|", variable, fixed = TRUE) == TRUE~ "Bird (Sea)",
                                      grepl("|NBI|", variable, fixed = TRUE) == TRUE ~ "Bird (Non-sea)",
                                      grepl("|BIR|", variable, fixed = TRUE) == TRUE ~ "Bird",
                                      grepl("|ZOO|", variable, fixed = TRUE) == TRUE ~ "Zooplankton",
                                      grepl("phyto", variable, fixed = TRUE) == TRUE ~ "Phytoplankton"))
}

# Name to latin + english one
## Use it as -> mutate(nomsp = map(nomSpecies, latin_eng))
# Classify pelagic moluscs as ZOO, benthic molluscs as MOL
# NB: May need to include the WORMS code
latin_eng <- function(nomSpecies){
  # Fish |FIS|
  if(nomSpecies == "Benthosema glaciale") nom_long <- "|FIS| Benthosema glaciale (glacier lantern fish)"
  if(nomSpecies == "Boreogadus saida") nom_long <- "|FIS| Boreogadus saida (polar cod)"
  if(nomSpecies == "Clupea harengus") nom_long <- "|FIS| Clupea harengus (herring)"
  if(nomSpecies == "Gadus morhua") nom_long <- "|FIS| Gadus morhua (northeast arctic cod)"
  if(nomSpecies == "Mallotus villosus") nom_long <- "|FIS| Mallotus villosus (capelin)"
  if(nomSpecies == "Sebastes mentella") nom_long <- "|FIS| Sebastes mentella (beaked redfish)"
  if(nomSpecies == "Sebastes norvegicus") nom_long <- "|FIS| Sebastes norvegicus (golden redfish)"
  # Mammal |MAM|
  if(nomSpecies == "Cystophora cristata") nom_long <- "|MAM| Cystophora cristata (hooded seal)"
  if(nomSpecies == "Megaptera novaeangliae") nom_long <- "|MAM| Megaptera novaeangliae (humpback whale)"
  if(nomSpecies == "Odobenus marinus") nom_long <- "|MAM| Odobenus marinus (walrus)"
  if(nomSpecies == "Pagophilus groenlandicus") nom_long <- "|MAM| Pagophilus groenlandicus (harp seal)"
  if(nomSpecies == "Ursus maritimus") nom_long <- "|MAM| Ursus maritimus (polar bear)"
  # Sea Bird |SBI|
  if(nomSpecies == "Alca torda") nom_long <- "|SBI| Alca torda (razorbill)"
  if(nomSpecies == "Alle alle") nom_long <- "|SBI| Alle alle (little auk)"
  if(nomSpecies == "Cepphus grylle") nom_long <- "|SBI| Cepphus grylle (black guillemot)"
  if(nomSpecies == "Chroicocephalus ridibundus") nom_long <- "|SBI| Chroicocephalus ridibundus (black-headed gull)"
  if(nomSpecies == "Falco peregrinus") nom_long <- "|SBI| Falco peregrinus (peregrine falcon)"
  if(nomSpecies == "Fratercula arctica") nom_long <- "|SBI| Fratercula arctica (atlantic puffin)"
  if(nomSpecies == "Fulmarus glacialis") nom_long <- "|SBI| Fulmarus glacialis (northern fulmar)"
  if(nomSpecies == "Gavia adamsii") nom_long <- "|SBI| Gavia adamsii (white-billed diver)"
  if(nomSpecies == "Gavia immer") nom_long <- "|SBI| Gavia immer (great northern diver)"
  if(nomSpecies == "Gulosus aristotelis") nom_long <- "|SBI| Gulosus aristotelis (european shag)"
  if(nomSpecies == "Haliaeetus albicilla") nom_long <- "|SBI| Haliaeetus albicilla (white-tailed eagle)"
  if(nomSpecies == "Larus argentatus") nom_long <- "|SBI| Larus argentatus (herring gull)"
  if(nomSpecies == "Larus canus") nom_long <- "|SBI| Larus canus (common gull)"
  if(nomSpecies == "Larus delawarensis") nom_long <- "|SBI| Larus delawarensis (ring-billed gull)"
  if(nomSpecies == "Larus fuscus") nom_long <- "|SBI| Larus fuscus (lesser black-backed gull)"
  if(nomSpecies == "Larus glaucoides") nom_long <- "|SBI| Larus glaucoides (iceland gull)"
  if(nomSpecies %in% c("Larus hyperboreus", "Larus hypeboreus", "GLGU")) nom_long <- "|SBI| Larus hyperboreus (glaucous gull)"
  if(nomSpecies == "Larus marinus") nom_long <- "|SBI| Larus marinus (great black-backed gull)"
  if(nomSpecies == "Larus philadelphia") nom_long <- "|SBI| Larus philadelphia (bonaparte’s gulls)"
  if(nomSpecies == "Larus smithonianus") nom_long <- "|SBI| Larus smithonianus (american herring gull)"
  if(nomSpecies %in% c("Larus sp.", "Larus glaucoides/hyperboreus")) nom_long <- "|SBI| Larus sp. (gull unidentified)"
  if(nomSpecies == "Melanitta fusca") nom_long <- "|SBI| Melanitta fusca (velvet scoter)"
  if(nomSpecies == "Melanitta nigra") nom_long <- "|SBI| Melanitta nigra (black scoter)"
  if(nomSpecies == "Morus bassanus") nom_long <- "|SBI| Morus bassanus (northern gannet)"
  if(nomSpecies == "Oceanodroma leucorhoa") nom_long <- "|SBI| Oceanodroma leucorhoa (leach’s storm-petrels)"
  if(nomSpecies == "Pagophila eburnea") nom_long <- "|SBI| Pagophila eburnea (ivory gull)"
  if(nomSpecies == "Phalacrocorax auritus") nom_long <- "|SBI| Phalacrocorax auritus (double-crested cormorant)"
  if(nomSpecies == "Phalacrocorax carbo") nom_long <- "|SBI| Phalacrocorax carbo (great cormorant)"
  if(nomSpecies == "Phalaropus fulicarius") nom_long <- "|SBI| Phalaropus fulicarius (red phalarope)"
  if(nomSpecies == "Podiceps grisegena") nom_long <- "|SBI| Podiceps grisegena (red-necked grebe)"
  if(nomSpecies == "Polysticta stelleri") nom_long <- "|SBI| Polysticta stelleri (steller’s duck)"
  if(nomSpecies == "Puffinus gravisn") nom_long <- "|SBI| Puffinus gravisn (greater shearwater)"
  if(nomSpecies == "Puffinus puffinus") nom_long <- "|SBI| Puffinus puffinus (manx shearwater)"
  if(nomSpecies == "Rissa tridactyla") nom_long <- "|SBI| Rissa tridactyla (black-legged kittiwake)"
  if(nomSpecies %in% c("Somateria mollissima", "Somateria mollissima borealis", "Sommateria mollissima")) nom_long <- "|SBI| Somateria mollissima (common eider)"
  if(nomSpecies == "Somateria spectabilis") nom_long <- "|SBI| Somateria spectabilis (king eider)"
  if(nomSpecies == "Stercorarius longicaudus") nom_long <- "|SBI| Stercorarius longicaudus (long-tailed skua)"
  if(nomSpecies == "Stercorarius parasiticus") nom_long <- "|SBI| Stercorarius parasiticus (arctic skua)"
  if(nomSpecies == "Stercorarius skua") nom_long <- "|SBI| Stercorarius skua (great skua)"
  if(nomSpecies == "Sterna hirundo") nom_long <- "|SBI| Sterna hirundo (common tern)"
  if(nomSpecies == "Sterna paradisaea") nom_long <- "|SBI| Sterna paradisaea (arctic tern)"
  if(nomSpecies == "Uria aalge") nom_long <- "|SBI| Uria aalge (common guillemot)"
  if(nomSpecies == "Uria lomvia") nom_long <- "|SBI| Uria lomvia (brünnich’s guillemot)"
  if(nomSpecies == "Xema sabini") nom_long <- "|SBI| Xema sabini (sabine’s gull)"
  # Non-sea Bird |NBI|
  if(nomSpecies == "Acanthis hornemanni") nom_long <- "|NBI| Acanthis hornemanni(arctic redpoll)"
  if(nomSpecies %in% c("Carduelis flammea","RP")) nom_long <- "|NBI| Carduelis flammea (common redpoll)"
  if(nomSpecies %in% c("Lagopus muta", "Lagopus mutus")) nom_long <- "|NBI| Lagopus mutus (rock ptarmigan)"
  if(nomSpecies %in% c("Plectrophenax nivalis","SB")) nom_long <- "|NBI| Plectrophenax nivalis (snow bunting)"
  # Bird |BIR
  if(nomSpecies == "Anas platyrhynchos") nom_long <- "|BIR| Anas platyrhynchos (mallard)"
  if(nomSpecies == "Anser brachyrhynchus") nom_long <- "|BIR| Anser brachyrhynchus (pink-footed goose)"
  if(nomSpecies == "Arenaria interpres") nom_long <- "|BIR| Arenaria interpres (ruddy turnstone)"
  if(nomSpecies == "Branta Canadensis") nom_long <- "|BIR| Branta canadensis (canada goose)"
  if(nomSpecies == "Branta bernicla") nom_long <- "|BIR| Branta bernicla (brant goose)"
  if(nomSpecies == "Branta leucopsis") nom_long <- "|BIR| Branta leucopsis (barnacle goose)"
  if(nomSpecies %in% c("Bubo scandiacus", "Bubo scandiaca")) nom_long <- "|BIR| Bubo scandiacus (snowy owl)"
  if(nomSpecies %in% c("Calcarius lapponicus","LB")) nom_long <- "|BIR| Calcarius lapponicus (lapland longspur)"
  if(nomSpecies == "Calidris alba") nom_long <- "|BIR| Calidris alba (sanderling)"
  if(nomSpecies == "Calidris alpina") nom_long <- "|BIR| Calidris alpina (dunlin)"
  if(nomSpecies == "Calidris canutus") nom_long <- "|BIR| Calidris canutus (red knot)"
  if(nomSpecies == "Calidris maritima") nom_long <- "|BIR| Calidris maritima (purple sandpiper)"
  if(nomSpecies == "Calidris melanotos") nom_long <- "|BIR| Calidris melanotos (pectoral sandpiper)"
  if(nomSpecies == "Charadrius hiaticula") nom_long <- "|BIR| Charadrius hiaticula (common ringed plover)"
  if(nomSpecies == "Clangula hyemalis") nom_long <- "|BIR| Clangula hyemalis (long-tailed duck)"
  if(nomSpecies == "Corvus corax") nom_long <- "|BIR| Corvus corax (raven)"
  if(nomSpecies %in% c("Gavia stallata", "Gavia stellata")) nom_long <- "|BIR| Gavia stallata (red-throated loon)"
  if(nomSpecies == "Histrionicus histrionicus") nom_long <- "|BIR| Histrionicus histrionicus (harlequin duck)"
  if(nomSpecies == "Mergus serratus") nom_long <- "|BIR| Mergus serratus (red-breasted merganser)"
  if(nomSpecies %in% c("Oenanthe oenanthe","NW")) nom_long <- "|BIR| Oenanthe oenanthe (northern wheatear)"
  if(nomSpecies == "Phalaropus lobatus") nom_long <- "|BIR| Phalaropus lobatus (red-necked phalarope)"
  if(nomSpecies == "Pluvialis apricaria") nom_long <- "|BIR| Pluvialis apricaria (european golden plover)"
  # Zooplankton |ZOO|
  if(nomSpecies %in% c("Calanus finmarchicus","atlantic species")) nom_long <- "|ZOO| Calanus finmarchicus (atlantic calanus)"
  if(nomSpecies %in% c("Calanus glacialis","arctic species")) nom_long <- "|ZOO| Calanus glacialis (arctic calanus)"
  if(nomSpecies == "Zooplankton") nom_long <- "|ZOO| Zooplankton (Zooplankton)"
  # Other
  if(nomSpecies %in% c("", "NA", "NULL")) nom_long <- NA
  return(nom_long)
}

# Convenience wrapper for WORMS database query
# TODO: Get the worms lookup to work with genus initials
# E.g. P. rubens
wm_records_df <- function(sp_name, res_type = "df"){
  URL_error <- NULL
  
  # Trim trailing and leading bits
  sp_no_sp <- trimws(gsub("sp\\.$|spp\\.$|-Ciliata$", "", sp_name))
  sp_no_pre <- trimws(gsub("young", "", sp_no_sp)) # NB: This will cause errors for a species name with 'young' in it
  
  sp_info <- tryCatch(wm_records_name(sp_no_pre)[1,], 
                      error = function(sp_no_pre) {URL_error <<- "Species name doesn't match"})
  if(res_type == "df"){
    if(is.null(URL_error)){
      sp_res <- data.frame(species = sp_name, dplyr::select(sp_info, url, lsid))
    } else {
      sp_res <- data.frame(species = sp_name, url = NA, lsid = NA)
    }
  } else if(res_type == "tax"){
    if(is.null(URL_error)){
      sp_res <- data.frame(species = sp_name, dplyr::select(sp_info, kingdom:genus))
    } else {
      sp_res <- data.frame(species = sp_name, kingdom = NA, phylum = NA, 
                           class = NA, order = NA, family = NA, genus = NA)
    }
  } else if(res_type == "cat"){
    if(is.null(URL_error)){
      # NB: Currently using order as the classifying level for ZOO
      # It may need to be family for some instances
      sp_res <- data.frame(species = sp_name) |> 
        mutate(cat = case_when(sp_info$class == "Teleostei" ~ "|FIS|",
                               sp_info$class == "Mammalia" ~ "|MAM|",
                               sp_info$class == "Aves" ~ "|BIR|",
                               # sp_info$class %in% "" ~ "|PHY|", # NB: Currently looking for this info
                               sp_info$order %in% c("Amphipoda", "Anthoathecata", "Calanoida", "Copelata", "Cyclopoida", "Cydippida", "Euphausiacea", "Harpacticoida", "Isopoda", "Phragmophora", "Pteropoda") ~ "|ZOO|",
                               sp_info$order %in% c("Laminariales") ~ "|ALG|",
                               sp_info$order %in% c("Decapoda") ~ "|BEN|",
                               TRUE ~ "|?|"))
    } else {
      sp_res <- data.frame(species = sp_name, cat = NA)
    }
  }
  return(sp_res)
  # rm(sp_name, sp_no_sp, sp_no_pre, sp_info, sp_res, URL_error); gc()
}

# Convenience function to catch inconsistent column types
# NB: Not used at the moment
refdb_import_BOLD_char <- function(taxon_id, ncbi_tax = FALSE){
  df_res <- refdb_import_BOLD(taxon = taxon_id, ncbi_taxo = ncbi_tax)
  df_res$x <- as.character(df_res$x)
  return(df_res)
}

# Download any number of desired files and save them locally
# TODO: Add a check to see if URL exists
file_URL_save <- function(file_name, base_URL, save_folder){
  
  # Set file info
  file_location <- paste0(base_URL, file_name)
  file_dest <- paste0(save_folder,file_name)
  
  # Check if file already exists and download if needed
  if(file.exists(file_dest)){
    # Intentionally blank
  } else if(RCurl::url.exists(file_location)) {
    download.file(url = file_location, method = "libcurl", destfile = file_dest)
  } else {
    # Intentionally blank
  }
  return()
  # rm(file_name, base_URL, save_folder)
}

# Function for performing a more thorough query of PANGAEA data by bbox
pg_full_search <- function(lookup_table = FALSE, doi_list = TRUE, ...){
  
  # Prep data.frame
  pg_res_all <- data.frame()
  
  # query_min_score <- 100
  query_offset <- 0
  while(query_offset < 10000){
    pg_res_query <- pangaear::pg_search(count = 500, offset = query_offset, ...)
    pg_res_all <- rbind(pg_res_all, pg_res_query)
    query_offset <- query_offset+500
    # query_min_score <- min(pg_EU_cruise_all$score)
  }
  
  # NB: This is a start at integrating lookup tables
  if(lookup_table & nrow(pg_res_all) > 0){
    pg_res_all <- distinct(arrange(pg_res_all, citation)) %>% 
      filter(grepl("station list|master tracks|metadata list|links to file", citation, ignore.case = T))
  } else if(nrow(pg_res_all) > 0) {
    pg_res_all <- distinct(arrange(pg_res_all, citation)) %>% 
      filter(!grepl("video|photograph|photomosaic|image|station list|master tracks|aircraft|flight|
                    |airborne|metadata list|core|links to file|Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% 
      filter(!grepl("sediment|soil", citation)) %>% # Unclear if these files should be filtered as they occasionally have a few useful data
      filter(!grepl("ACLOUD|SOCAT|GLODAP", citation)) %>% # SOCAT and GLODAP are added manually later
      filter(!grepl("Schlegel", citation)) %>% # Prevent downloading the FACE-IT dataset
      filter(!grepl("WOCE", citation)) # The WOCE data have formatting issues and should be downloaded via their own portal
  }
  
  # Filter data if a PANGAEA DOI list is present in the environment
  if(exists("pg_doi_list") & nrow(pg_res_all) > 0 & doi_list){
    pg_res_all <- pg_res_all |> filter(!doi %in% pg_doi_list$doi)
  }
  
  return(pg_res_all)
}

# Function for printing PANGAEA meta-data
pg_meta_print <- function(pg_doi){
  pg_test <- pangaear::pg_data(pg_doi)
}

# Convenience wrapper to abbreviate PANGAEA column names
pg_abb <- function(pg_var_vector){
  if(!is_empty(pg_var_vector)){
    res_abb <- sapply(strsplit(pg_var_vector, " [", fixed = T), "[[", 1)
    res_abb <- sapply(strsplit(res_abb, " (", fixed = T), "[[", 1) # For salinity generally
    return(res_abb)
  } 
}

# Given a list of PG metadata entries this function will extract the goods
pg_meta_extract <- function(pg_item){
  
  # Get the pieces of interest
  # NB: Remove notes from Event title. Not sure that this won't cause merging issues later...
  meta_event <- pg_abb(pg_item[1])
  meta_lon <- gsub("LONGITUDE: ", "", pg_item[grepl("LONGITUDE", pg_item, ignore.case = TRUE)])
  meta_lat <- gsub("LATITUDE: ", "", pg_item[grepl("LATITUDE", pg_item, ignore.case = TRUE)])
  meta_date <- gsub("DATE/TIME: ", "", pg_item[grepl("DATE/TIME", pg_item, ignore.case = TRUE)])
  meta_depth <- gsub("DEPTH: ", "", pg_item[grepl("DEPTH", pg_item, ignore.case = TRUE)])
  
  # Check for missing values
  if(length(meta_event) == 0) meta_event <- NA
  if(length(meta_lon) == 0) meta_lon <- NA
  if(length(meta_lat) == 0) meta_lat <- NA
  if(length(meta_date) == 0) meta_date <- NA
  if(length(meta_depth) == 0) meta_depth <- NA
  
  # NB: Tibble allows for 'DATE/TIME' column name
  meta_res <- tibble(Event = meta_event, LONGITUDE = meta_lon, LATITUDE = meta_lat,
                     `DATE/TIME` = meta_date, DEPTH = meta_depth)
  return(meta_res)
}

# Function for extracting info from PANGAEA data
# pg_dl <- dl_dat[[1]] # tester...
pg_dl_prep <- function(pg_dl){
  
  # Load PANGAEA variable list if it's missing
  if(!exists("query_ALL")) source("code/key_drivers.R")
  
  # Prep for reporting
  dl_error <- "None"
  
  # Extract data.frame or catch specific errors
  if(is.data.frame(pg_dl$data)){
    
    # NB: This can't be done because columns won't match query_ALL$pg_col_name
    # pg_dl$data <- janitor::clean_names(pg_dl$data)
    
    # Check that all columns are unique
    # NB: There is an error in the pangaer package that does not give enough characters to columns
    # This allows very long column names to have the units etc. cut off the end
    # I created an issue on the GitHub page with no response...
    if(length(unique(colnames(pg_dl$data))) == length(colnames(pg_dl$data))){
      
      # Check for metadata stored in an accompanying event list structure
      if("metadata" %in% names(pg_dl)){
        if("events" %in% names(pg_dl$metadata)){
          
          # If they are available as adata.frame
          if(is.list(pg_dl$metadata$events)){
            pg_meta <- as_tibble(pg_dl$metadata$events)
            if(!"Longitude" %in% pg_abb(colnames(pg_dl$data))){
              if(TRUE %in% c(lon_names %in% colnames(pg_meta))){
                pg_dl$data$Longitude <- as.numeric(pg_meta[which(colnames(pg_meta) %in% lon_names)][1])
              }
            }
            if(!"Latitude" %in% pg_abb(colnames(pg_dl$data))){
              if(TRUE %in% c(lat_names %in% colnames(pg_meta))){
                pg_dl$data$Latitude <- as.numeric(pg_meta[which(colnames(pg_meta) %in% lat_names)][1])
              }
            }
            if(!"Date/Time" %in% pg_abb(colnames(pg_dl$data))){
              if("DATE/TIME" %in% colnames(pg_meta)){
                pg_dl$data$`Date/Time` <- pg_meta$`DATE/TIME`[1]
              }
            }
          }
          
          # If they are available as a vector
          if(is.character(pg_dl$metadata$events)){
            if("Event" %in% pg_abb(colnames(pg_dl$data))){
              event_meta <- data.frame(str_split(pg_dl$metadata$events, "; ")) |> `colnames<-`("val")
              event_list <- str_split(event_meta$val, " \\* ")
              event_df <- plyr::ldply(event_list, pg_meta_extract, .parallel = FALSE)
              # NB: These joins cause issues
              pg_dl$data <- dplyr::left_join(pg_dl$data, event_df, by = "Event", relationship = "many-to-many")
            }
          }
        }
      }
        
      # Get names of desired columns
      # NB: It is necessary to allow partial matches via grepl() because column names
      # may have notes about the data attached to them, preventing exact matches
      col_name <- colnames(pg_dl$data)
      col_abb <- pg_abb(colnames(pg_dl$data))
      col_idx <- col_name[which(col_abb %in% query_ALL$Abbreviation)]
      col_meta <- col_name[which(col_abb %in% c("LONGITUDE", "LATITUDE", "DATE/TIME", "DEPTH", query_meta$Abbreviation))]
      col_lon <- col_name[which(col_abb %in% c("LONGITUDE", query_meta$Abbreviation[query_meta$driver == "lon"]))]
      col_lat <- col_name[which(col_abb %in% c("LATITUDE", query_meta$Abbreviation[query_meta$driver == "lat"]))]
      col_date <- col_name[which(col_abb %in% c("DATE/TIME", query_meta$Abbreviation[query_meta$driver == "date"]))]
      col_depth <- col_name[which(col_abb %in% c("DEPTH", query_meta$Abbreviation[query_meta$driver == "depth"]))]
      col_base <- col_idx[!col_idx %in% col_meta]
      col_spp <- col_name[which(col_abb %in% query_species$Abbreviation)]
      
      # Determine if there are too many columns
      # NB: This intentionally does not account for metadata columns or species columns because they will be melted
      if(length(col_base) <= 20){
        
        # Set the preferred metadata column abbreviation hierarchy
        col_lon_order <- unique(c("Longitude", "LONGITUDE", "lon", query_meta$Abbreviation[query_meta$driver == "lon"]))
        col_lat_order <- unique(c("Latitude", "LATITUDE", "lat", query_meta$Abbreviation[query_meta$driver == "lat"]))
        col_date_order <- unique(c("Date/Time", "DATE/TIME", "Date", "date", "Date/time start", "Date/time end", "Sampling date",
                                   query_meta$Abbreviation[query_meta$driver == "date"]))
        col_depth_order <- unique(c("Depth water", "Depth", "depth", "Press", "Depth top", 
                                    "Elevation", "Elev", "Surf elev", "DEPTH", query_meta$Abbreviation[query_meta$driver == "depth"]))
        
        # Determine single choice metadata column for: lon, lat, date, depth
        col_lon_choice <- col_lon[grepl(col_lon_order[col_lon_order %in% pg_abb(col_lon)][1], col_lon)]
        col_lat_choice <- col_lat[grepl(col_lat_order[col_lat_order %in% pg_abb(col_lat)][1], col_lat)]
        col_date_choice <- col_date[grepl(col_date_order[col_date_order %in% pg_abb(col_date)][1], col_date)]
        col_depth_choice <- col_depth[grepl(col_depth_order[col_depth_order %in% pg_abb(col_depth)][1], col_depth)]
        
        # Can't use [1] above because we need empty character vectors for the ext step
        if(length(col_lon_choice) > 1) col_lon_choice <- col_lon_choice[1]
        if(length(col_lat_choice) > 1) col_lat_choice <- col_lat_choice[1]
        if(length(col_date_choice) > 1) col_date_choice <- col_date_choice[1]
        if(length(col_depth_choice) > 1) col_depth_choice <- col_depth_choice[1]
        
        # Bind together for use below
        col_meta_choice <- c(col_lon_choice, col_lat_choice, col_date_choice, col_depth_choice)
        
        # Process metadata columns
        dl_proc <- pg_dl$data |>
          rename(lon := !!col_lon_choice, lat := !!col_lat_choice,
                 date := !!col_date_choice, depth := !!col_depth_choice)
        col_meta_fin <- c("lon", "lat", "date", "depth")[c("lon", "lat", "date", "depth") %in% colnames(dl_proc)]
        
        # Correct date column
        if("date" %in% col_meta_fin){
          dl_proc <- dl_proc |> 
            mutate(date = case_when(nchar(date) == 9 & pg_abb(col_date_choice) == "Sampling date" ~ sapply(str_split(date, "-"), "[[", 1),
                                    TRUE ~ date),
                   date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                                    nchar(date) == 7 ~ paste0(date,"-01"),
                                    TRUE ~ date),
                   date = ifelse(date == "", NA, date)) |>
            mutate(date = as.Date(gsub("T.*", "", date)))
        }
        
        # Correct depth column
        if("depth" %in% col_meta_fin){
          # NB: Choice is made here to round to even 1 metre units
          dl_proc$depth <- round(as.numeric(dl_proc$depth))
          if(pg_abb(col_depth_choice) == "Press"){
            if(length(col_lat_choice) == 1){
              dl_proc$depth <- seacarb::p2d(dl_proc$depth, lat = dl_proc$lat)
            } else {
              dl_proc$depth <- seacarb::p2d(dl_proc$depth, lat = 70)
            }
          }
          if(pg_abb(col_depth_choice) %in% c("Elevation", "Elev", "Surf elev")) dl_proc$depth <- -dl_proc$depth
        }
        
        # Filter spatially if possible
        if(all(c("lon", "lat") %in% col_meta_fin)){
          if(!is.numeric(dl_proc$lon)) dl_proc$lon <- as.numeric(dl_proc$lon)
          if(!is.numeric(dl_proc$lat)) dl_proc$lat <- as.numeric(dl_proc$lat)
          dl_proc$lon <- ifelse(dl_proc$lon > 180, dl_proc$lon - 360, dl_proc$lon)
          dl_no_coords <- filter(dl_proc, is.na(lon) | is.na(lat))
          dl_coords <- plyr::ddply(bbox_ALL_df, c("region"), filter_bbox, df = dl_proc, .parallel = TRUE) |> 
            dplyr::select(-region)
          dl_spatial <- bind_rows(dl_no_coords, dl_coords) 
        } else {
          dl_spatial <- dl_proc
        }
        
        # Skip these steps if all of the data were filtered out
        if(nrow(dl_spatial) > 0){
          
          # Get data for desired drivers, excluding species due to the width of these data
          dl_driver <- tibble()
          if(length(col_base) > 0){
            suppressWarnings(
              dl_driver <- dl_spatial |> 
                dplyr::select(dplyr::all_of(c(col_meta_fin, col_base))) |> 
                # mutate_all(~na_if(., '')) %>% # This will throw errors from unknown column types
                # NB: This forcibly removes non-numeric values
                dplyr::mutate_at(col_base, as.numeric) |>
                janitor::remove_empty(which = c("rows", "cols")) #|> 
                # NB: Don't pivot long here as it makes the file size much larger
                # pivot_longer(cols = -col_meta, names_to = "name", values_to = "value") |> filter(!is.na(value))
              )
            
            # Average by meta columns
            if(length(col_meta_fin) > 0){
              dl_driver <- dl_driver |> 
                reframe(across(everything(), mean), .by = all_of(col_meta_fin))
            }
          }
          
          # Get species values directly as a melted data.frame
          # NB: These data are forced to numeric during pg_var_melt()
          # But they are kept as character values here in order to allow
          # For potential investigation into the raw data
          dl_spp <- tibble()
          if(length(col_spp) > 0){
            dl_spp <- pg_dl$data |> 
              dplyr::select(dplyr::all_of(c(col_meta_fin, col_spp))) |> 
              janitor::remove_empty(which = c("rows", "cols")) |> 
              # NB: Force everything to characters and sort it out in detail later
              dplyr::mutate_at(col_spp, as.character) |> 
              pivot_longer(cols = col_spp, names_to = "spp_name", values_to = "spp_value")# |>
              # NB: Intentionally not filtering NA as species presence might not have a value
              # filter(!is.na(value))
          }
          
          # Create vector of removed columns
          cols_removed <- paste(colnames(pg_dl$data)[!(colnames(pg_dl$data) %in% c(col_meta_choice, colnames(dl_driver), col_spp))], collapse = "; ")
          
          # Combine
          dl_single <- bind_rows(dl_driver, dl_spp) |> 
            mutate(date_accessed = as.Date(Sys.Date()),
                   Error = "None",
                   meta_cols = paste(col_meta_choice, collapse = "; "),
                   removed_cols = cols_removed,
                   parent_doi = pg_dl$parent_doi,
                   URL = pg_dl$url,
                   citation = pg_dl$citation, 
                   .before = 1)
          
        } else {
          dl_error <- "Spatial data not in any site bbox"
        }
      } else {
        dl_error <- "More than 20 columns with data"
      }
    } else {
      dl_error <- "Multiple columns with same name"
    }
  } else {
    dl_error <- "No data in DOI reference"
  }
  
  # Determine if there are columns with desired data
  if(dl_error == "None"){
    if(all(colnames(dl_single) %in% c("date_accessed", "Error", "meta_cols", "removed_cols", 
                                      "parent_doi", "URL", "citation", "lon", "lat", "date", "depth"))){
      dl_error <- "No columns with drivers"
    }
  }
  
  # Create error report if necessary
  if(dl_error != "None") 
    dl_single <- data.frame(date_accessed = as.Date(Sys.Date()),
                            Error = dl_error,
                            parent_doi = pg_dl$parent_doi,
                            URL = pg_dl$url,
                            citation = pg_dl$citation)
  
  # Exit
  return(dl_single)
  # rm(pg_dl, dl_error, col_idx, col_meta, col_spp, dl_driver, dl_spp, dl_no_coords, dl_coords, dl_spatial, dl_single); gc()
}

# Function for downloading and processing PANGAEA data for merging
# testers...
# pg_doi <- pg_doi_files$doi[38] 
# pg_doi <- doi_dl$doi[1]
# pg_doi <- "10.1594/PANGAEA.733415"
# pg_doi <- "10.1594/PANGAEA.912766" # Complex metadata event vector
# pg_doi <- "10.1594/PANGAEA.950509" # Multiple metadata columns
pg_dl_proc <- function(pg_doi){
  
  # Get data
  dl_error <- NULL
  suppressWarnings(
    dl_dat <- tryCatch(pg_data(pg_doi), error = function(pg_doi) {dl_error <<- "Cannot access data via pangaer"})
  ); gc()
  
  # Extract data from multiple lists as necessary
  if(is.null(dl_error)){
    dl_df <- plyr::ldply(dl_dat, pg_dl_prep, .parallel = TRUE); gc()
  } else {
    dl_df <- data.frame(date_accessed = as.Date(Sys.Date()),
                        Error = dl_error,
                        parent_doi = pg_doi,
                        URL = paste0("https://doi.org/",pg_doi),
                        citation = NA)
  }
  
  # Exit
  return(dl_df)
  # rm(pg_doi, dl_error, dl_dat, dl_df); gc()
}

# Function for automagically downloading, processing, and saving PANGAEA data
# testers...
# doi_dl_list <- pg_doi_list[2675:2680,]; file_name <- doi_dl_list$file[1]
pg_dl_save <- function(file_name, doi_dl_list){
  
  # Start message
  print(paste0("Started on ",file_name," at ",Sys.time()))
  
  # Load PANGAEA variable list if it's missing
  if(!exists("query_ALL")) source("code/key_drivers.R")
  
  # Get list of files to download
  # Small batches to avoid downloading parent duplicates
  doi_dl <- filter(doi_dl_list, file == file_name)[1:20,] |> filter(!is.na(doi))
  if(nrow(doi_dl) == 0) return()
  
  # Download data
  pg_res <- plyr::ldply(doi_dl$doi, pg_dl_proc, .parallel = F)
  # TODO: test explicitly that removing this step doesn't cause issues
  # pg_res_meta_columns <- colnames(pg_res)[colnames(pg_res) %in% query_meta$pg_col_name]
  # pg_res <- pg_res |> 
  #   mutate(across(dplyr::all_of(pg_res_meta_columns), as.character))
  
  # Load existing metadata to join the new metadata to
  if(file.exists(paste0("metadata/",file_name,"_doi.csv"))){
    pg_lookup <- read_csv_arrow(paste0("metadata/",file_name,"_doi.csv"))
    pg_meta_start <- max(pg_lookup$meta_idx, na.rm = T)
  } else {
    pg_lookup <- data.frame()
    pg_meta_start <- 0
  }
  
  # Create and utilise metadata lookup tables
  pg_meta_prep <- pg_res |> 
    dplyr::select(date_accessed, Error, parent_doi, URL, citation, meta_cols, removed_cols) |> 
    distinct() |> dplyr::mutate(meta_idx = 1:n() + pg_meta_start, .before = 1)
  pg_meta <- distinct(bind_rows(pg_lookup, pg_meta_prep)) |> 
    filter(!is.na(parent_doi))
  pg_slim <- pg_res |> 
    left_join(pg_meta_prep, by = c("date_accessed", "Error", "parent_doi", "URL", "citation", "meta_cols", "removed_cols")) |> 
    # NB: Intentionally keeping rows with all NA to link with meta_idx
    dplyr::select(meta_idx, everything(), -date_accessed, -Error, -parent_doi, -URL, -citation, -meta_cols, -removed_cols)
  
  # Load and combine existing data
  if(file.exists(paste0("data/pg_data/",file_name,".csv"))){
      pg_base <- data.table::fread(paste0("data/pg_data/",file_name,".csv"), nThread = 14); gc()
      if("spp_value" %in% colnames(pg_base)) pg_base <- mutate(pg_base, spp_value = as.character(spp_value))
      pg_base_meta_columns <- colnames(pg_base)[colnames(pg_base) %in% query_meta$pg_col_name]
      # TODO: test explicitly that removing this step doesn't cause issues
      # pg_base <- mutate(pg_base, across(dplyr::all_of(pg_base_meta_columns), as.character))
      pg_full <- distinct(bind_rows(pg_base, pg_slim))
  } else {
    pg_full <- pg_slim
  }
  
  # Get folder name
  if(file_name == "pg_kong") file_folder <- "kongsfjorden"
  if(file_name == "pg_is") file_folder <- "isfjorden"
  if(file_name == "pg_stor") file_folder <- "storfjorden"
  if(file_name == "pg_young") file_folder <- "young_sound"
  if(file_name == "pg_disko") file_folder <- "disko_bay"
  if(file_name == "pg_nuup") file_folder <- "nuup_kangerlua"
  if(file_name == "pg_por") file_folder <- "porsangerfjorden"
  
  # Save files
  # NB: Intentionally only saving these files locally
  write_csv(pg_meta, paste0("metadata/",file_name,"_doi.csv"))
  data.table::fwrite(pg_full, paste0("data/pg_data/",file_name,".csv"))
  rm(pg_res, pg_res_meta_columns, pg_base, pg_base_meta_columns, pg_full, pg_slim, pg_lookup, pg_meta_prep, pg_meta, file_folder); gc()
  # rm(file_name, doi_dl_list, doi_dl)
}

# Function for quickly opening up a file based on doi
pg_test_dl <- function(pg_doi){
  dl_dat <- tryCatch(pg_data(pg_doi), error = function(pg_doi) stop("Download failed"))
  dl_single <- tibble(URL = dl_dat[[1]]$url,
                      citation = dl_dat[[1]]$citation,
                      dl_dat[[1]]$data)
  return(dl_single)
}

# Look at how long/wide downloaded files are
pg_size <- function(df){
  res <- df |> 
    janitor::remove_empty(which = c("rows", "cols"))
  res_size <- data.frame(width = ncol(res),
                         length = nrow(res))
  return(res_size)
}

# Find duplicate files
pg_duplicates <- function(pg_doi_df, pg_size_df){
  
  # Find downloads with same length, width, and URL
  pg_doi_duplicates <- pg_doi_df |> 
    filter(Error == "None") |> 
    left_join(pg_size_df, by = c("meta_idx")) |> 
    na.omit() |> 
    mutate(count = n(), .by = c(URL, width, length)) |>  
    filter(count >= 2, Error == "None")
  
  # Determine which duplicates to keep
  pg_doi_keep <- pg_doi_duplicates |> 
    mutate(doi = str_remove(URL, "https://doi.org/"),
           parent_file = case_when(parent_doi != doi ~ TRUE, TRUE ~ FALSE)) |> 
    filter(parent_file) |> 
    mutate(min_idx = min(meta_idx), .by = c(URL, width, length)) |> 
    filter(meta_idx %in% min_idx)
  
  # Append/extract final column and exit
  pg_res <- pg_doi_duplicates |> 
    mutate(keep_idx = case_when(meta_idx %in% pg_doi_keep$min_idx ~ TRUE, TRUE ~ FALSE))
  pg_vector <- pg_res$meta_idx[!pg_res$keep_idx]
  return(pg_vector)
  # rm(pg_doi_df, pg_size_df, pg_doi_duplicates, pg_doi_keep, pg_res, pg_vector)
}

# Quick filtering function
# Manual tweaks will still be required after running this
# NB: Values with no lon/lat must be removed at this step to prevent data bleed across sites
pg_site_filter <- function(file_name, site_name){
  
  # Load data
  pg_dat <- load_pg(file_name) |>
    dplyr::rename(lon = Longitude, lat = Latitude)
  file_site <- str_remove_all(file_name, "data/pg_data/pg_|.csv")
  bbox <- bbox_from_name(site_name)
  if(file_site == site_name){
    pg_no_coords <- filter(pg_dat, is.na(lon) | is.na(lat))
    pg_coords <- pg_dat |> 
      filter(lon >= bbox[1], lon <= bbox[2],
             lat >= bbox[3], lat <= bbox[4])
    pg_res <- bind_rows(pg_no_coords, pg_coords)
    rm(pg_dat, pg_no_coords, pg_coords); gc()
  } else {
   pg_res <- pg_dat |> 
      filter(lon >= bbox[1], lon <= bbox[2],
             lat >= bbox[3], lat <= bbox[4])
   rm(pg_dat); gc()
  }
  
  if(nrow(pg_res) == 0){
    return()
  } else {
    pg_base_meta_columns <- colnames(pg_res)[colnames(pg_res) %in% query_meta$pg_col_name]
    pg_res <- mutate(pg_res, across(dplyr::all_of(pg_base_meta_columns), as.character))
    pg_res <- pg_res |> 
      # mutate_if(is.character, ~na_if(., '')) |> # NB: Need to test that this works
      janitor::remove_empty("cols") |> distinct()
    # NB: Need to test that this works
    # Finish up
    # left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
    #   dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything()) |> 
    #   dplyr::rename(`Sal [PSU]` = `Sal ([PSU])`) |> 
    #   # NB: This must be changed manually when new data are loaded
    #   mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
    #   janitor::remove_empty("cols") |> 
    #   # NB: Site exists earlier to reflect data from different files for metadata joining
    #   mutate(site = site_name)
    return(pg_res)  
  }
  # rm(file_name, site_name, pg_res, bbox, file_site); gc()
}

# Function for melting columns related to a specific driver
# pg_clean <- pg_disko_clean; query_sub <- query_cryo
pg_var_melt <- function(pg_clean, query_sub){
  
  # Get unique ID info
  query_ID <- query_sub |> dplyr::select(pg_col_name, driver, category) |> distinct()
  
  # Message of which columns were melted
  cols_fix <- gsub("\\] \\(.*", "\\]", colnames(pg_clean))
  pg_fix <- pg_clean; colnames(pg_fix) <- cols_fix; rm(pg_clean); gc()
  sub_cols <- colnames(pg_fix)[colnames(pg_fix) %in% unique(query_ID$pg_col_name)]
  sub_cols <- sub_cols[!sub_cols %in% c("date_accessed", "URL", "citation", "site", "lon", "lat", "date", "depth")]
  print(sub_cols)
  if(length(sub_cols) == 0) return()
  
  # Subset and melt data.frame
  pg_melt <- pg_fix |> 
    dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, all_of(sub_cols)) |> 
    pivot_longer(cols = dplyr::all_of(sub_cols), names_to = "variable", values_to = "value") |> 
    filter(!is.na(value)) |> distinct() |> 
    left_join(query_ID, by = c("variable" = "pg_col_name"))
  
  # Get species data
  if(query_sub$category[1] == "bio"){
    # NB: At this point any non-numeric species values are lost
    # Need to think about how best to approach this
    # NB: May want to use query_species as a right filter
    pg_melt_bio <- pg_fix |> 
      dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, spp_name, spp_value) |>
      dplyr::rename(variable = spp_name, value = spp_value) |> 
      mutate(value = as.numeric(value),
             driver = "biomass", category = "bio") |> 
      filter(!is.na(value)) |> distinct()
    pg_melt <- rbind(pg_melt, pg_melt_bio) |> distinct()
  }
  
  # Mean values and exit
  rm(pg_fix); gc()
  pg_res <- pg_melt |> 
    mutate(type = "in situ") |> # TODO: This needs to be improved
    group_by(date_accessed, URL, citation, type, site, lon, lat, date, depth, category, driver, variable) |> 
    summarise(value = mean(value, na.rm = T), .groups = "drop")
  return(pg_res)
  # rm(pg_clean, query_sub, sub_cols, pg_melt, pg_res)
}

# Load a PG site file and apply the name
load_pg <- function(file_name){
  if(grepl("meta", file_name)) {
    site_name <- str_remove_all(file_name, "metadata/pg_|_doi.csv")
  } else if(grepl("pg_data", file_name)) {
    site_name <- str_remove_all(file_name, "data/pg_data/pg_|.csv")
  } else {
    stop("Wrong file type(s)")
  }
  res <- data.table::fread(file_name) |> # NB: Faster than read_csv_arrow()
    mutate(site = site_name, .before = 1)
  return(res)
}

# Find the nearest grid cells for each site
## NB: Requires two data.frames with lon, lat in that order
grid_match <- function(coords_base, coords_match){
  if(!"lon" %in% colnames(coords_base)) stop("Need lon/lat columns in coords_base")
  if(!"lon" %in% colnames(coords_match)) stop("Need lon/lat columns in coords_match")
  coords_match$idx <- 1:nrow(coords_match)
  grid_index <- data.frame(coords_base,
                           idx = knnx.index(data = as.matrix(coords_match[,1:2]),
                                            query = as.matrix(coords_base[,1:2]), k = 1))
  grid_points <- left_join(grid_index, coords_match, by = c("idx")) %>% 
    mutate(dist = round(distHaversine(cbind(lon.x, lat.x),
                                      cbind(lon.y, lat.y))/1000, 2), idx = NULL)
  return(grid_points)
}

# Function for finding and cleaning up points within a given region polygon
points_in_region <- function(region_in, bbox_df, data_df){
  region_sub <- bbox_df %>% 
    filter(region == region_in)
  distinct_df <- data_df %>%
    dplyr::select(lon, lat) %>%
    distinct()
  coords_in <- distinct_df %>%
    mutate(in_grid = sp::point.in.polygon(point.x = distinct_df[["lon"]], point.y = distinct_df[["lat"]],
                                          pol.x = region_sub[["lon"]], pol.y = region_sub[["lat"]])) %>%
    filter(in_grid >= 1) %>%
    mutate(region = region_in) %>%
    dplyr::select(lon, lat, region)
  return(coords_in)
}

# Convenience function for extracting bathymetry data by bbox
extract_bathy <- function(bbox, site_name, product = "GEBCO"){
  if(product == "GEBCO") bathy_file <- "~/pCloudDrive/FACE-IT_data/maps/GEBCO/GEBCO_2020.nc"
  # if(product == "IBCAO") bathy_file <- "~/pCloudDrive/FACE-IT_data/maps/IBCAO/IBCAO_v4_200m.nc"
  bathy_df <- tidync(bathy_file) %>% 
    hyper_filter(lon = between(lon, bbox[1], bbox[2]),
                 lat = between(lat, bbox[3], bbox[4])) %>% hyper_tibble()
  write_csv(bathy_df, paste0("~/pCloudDrive/FACE-IT_data/maps/bathy_",site_name,".csv"))
}

# New best practice for converting UTM projected points to an even lon/lat grid
# NB: Expects three columns; x, y, and a data column
convert_UTM_deg_grid <- function(df, proj_base, third_col = NULL){
  if(!is.null(third_col)) df <- df[,c("x", "y", third_col)]
  df_utm <- st_as_sf(df, coords = c("x", "y"), crs = proj_base)
  df_rast <- st_rasterize(df_utm)
  df_raster <- as(df_rast, "Raster")
  df_raster_deg <- projectRaster(df_raster, crs = 4326)
  df_df <- as.data.frame(df_raster_deg, xy = TRUE) |> 
    dplyr::rename(lon = x, lat = y) |> 
    filter(!is.na(layer))
  return(df_df)
}

# Function for converting UTM to decimal degrees
# NB: This will likely need to be changed RE spatil package phase out
# NB: Expects an 'Easting' and 'Northing' column
convert_UTM_deg <- function(df, utm_zone){
  
  # Fill NA with 0 for ease of use
  df$Easting <- replace_na(df$Easting, 0)
  df$Northing <- replace_na(df$Northing, 0)
  
  # Convert coordinates to lon/lat
  suppressWarnings(
    utmcoor <- sp::SpatialPoints(cbind(df$Easting, df$Northing), 
                                 proj4string = sp::CRS(paste0("+proj=utm +zone=",utm_zone)))
  )
  suppressWarnings(
    longlatcoor <- sp::spTransform(utmcoor, sp::CRS("+proj=longlat"))
  )
  
  # Attach to data.frame and replace 0 with NA
  df$lon <- sp::coordinates(longlatcoor)[,1]
  df$lon[df$Easting == 0] <- NA
  df$Easting[df$Easting == 0] <- NA
  df$lat <- sp::coordinates(longlatcoor)[,2]
  df$lat[df$Northing == 0] <- NA
  df$Northing[df$Northing == 0] <- NA
  return(df)
}

# Convert from one EPSG to another
# This is the function to convert Polar Stereographic Coordinates to Lat-Lon
# Adapted from a script that Bernard Gentili found here: https://github.com/jenseva/projected-data-demos
# NB: This will likely need to be changed RE sptail package phase out
# x <- sval_Nature_glacier_mass$X_center
# y <- sval_Nature_glacier_mass$Y_center
# epsg1 <- "epsg:32633"
convert_epsg <- function(x, y, epsg1, epsg2 = "epsg:4326") {

  # Create ygrid and xgrid vectors from the data frame columns and remove any padded NaNs
  ygrid <- x
  xgrid <- y
  
  # Use expand to create a points data frame of all possible coordinate combinations
  points.df <- expand.grid(ygrid, xgrid) %>% 
    `colnames<-`(c("y", "x"))
  
  ## Create a spatial dataframe of the coordinates
  # Next we will convert the coordinate points dataframe to a spatial object (spatial dataframe).
  # Create the **coordsinit** variable to verify the initial coordinates of the spatial dataframe.
  dfcoords <- cbind(points.df$y, points.df$x) # coords in y,x order
  sppoints <- SpatialPoints(coords = dfcoords)
  spdf <- SpatialPointsDataFrame(coords = dfcoords, data = points.df)
  coordsinit <- spdf@coords
  
  ## Reproject data from polar sterographic to latitude-longitude
  # Define coordinate reference systems
  crs1 <- CRSargs(CRS(paste0("+init=",epsg1)))
  crs2 <- CRSargs(CRS(paste0("+init=",epsg2)))
  
  # Set CRS of spatial dataframe
  suppressWarnings(proj4string(spdf) <- CRS(crs1)) # Suppress comment warning
  ps_bbox <- spdf@bbox
  print(ps_bbox)
  
  # Check the initial CRS 
  suppressWarnings(crs_set <- proj4string(spdf)) # Suppress comment warning
  
  # Converts from existing crs to latlon (4326)
  spdfProjected <- spTransform(spdf, CRS(crs2))
  suppressWarnings(crs_projected <- proj4string(spdfProjected)) # Suppress comment warning
  
  coordsproj <- spdfProjected@coords
  bbox <- spdfProjected@bbox
  print(bbox)
  
  df_latlon <- as.data.frame(spdfProjected)
  lon <- df_latlon$coords.x1
  lat <- df_latlon$coords.x2
  return(data.frame(x = points.df$y, y = points.df$x, lon = lon, lat = lat)) # Intentionally swapping x and y
}

# Function for converting uneven lon/lat coords to an even grid for plotting
# df <- filter(ice_4km_annual_trends, site == "kong")
# z_col <- "trend"; pixel_res <- 0.04
convert_even_grid <- function(df, z_col, pixel_res){
  
  # Base info
  lon <- df$lon; lat <- df$lat; z <- df[,which(colnames(df) == z_col)]
  
  # Calculate the number of rows and columns of the raster
  dlon <- diff(range(lon))
  dlat <- diff(range(lat))
  dlon_meters <- floor(dlon * 1.e5 * cos(mean(lat) * pi / 180))
  dlat_meters <- floor(dlat * 1.e5)
  dcell_meters <- pixel_res*100000 # I changed this as I am working with rough lon/lat degree values rather than metres
  nrows <- floor(dlat_meters / dcell_meters)
  ncols <- floor(dlon_meters / dcell_meters)
  
  # Get pixels from an even grid - Centres of pixels
  lon_df <- data.frame(x = c(seq(-180, 0-pixel_res/2, by = pixel_res), seq(0+pixel_res/2, 180, by = pixel_res))) %>% 
    filter(between(x, min(lon-pixel_res), max(lon+pixel_res)))
  lat_df <- data.frame(y = c(seq(-90, 0-pixel_res/2, by = pixel_res), seq(0+pixel_res/2, 90, by = pixel_res))) %>% 
    filter(between(y, min(lat-pixel_res), max(lat+pixel_res)))
  full_grid <- expand.grid(lon_df$x, lat_df$y) %>% `colnames<-`(., c("x", "y"))
  
  # Create base raster
  z_rasterized <- raster(nrows = nrows, ncols = ncols, 
                         xmn = min(full_grid$x), xmx = max(full_grid$x), 
                         ymn = min(full_grid$y), ymx = max(full_grid$y))
  z_rasterized <- rasterize(cbind(lon, lat), z_rasterized, z)
  
  # Info
  cat("mean area of cells", mean(values(area(z_rasterized))), "")
  cat("    should be close to", dcell_meters**2 * 1.e-6, "\n")
  v <- values(z_rasterized)
  cat("number of cells", length(v), "NA-values", length(which(is.na(v))), "percentage of NA-values", 100 * length(which(is.na(v))) / length(v), "%\n")
  
  # Convert to data.frame and exit
  z_df <- as.data.frame(z_rasterized, xy = TRUE) %>% 
    `colnames<-`(c("lon", "lat", "z"))
  return(z_df)
}

# Function for creating citations from a px JXON source (i.e. national stats websites)
px_cite <- function(x){
  cit <- paste0(x$metadata[[1]]$source," (",lubridate::year(x$metadata[[1]]$updated),"). ",
                x$metadata[[1]]$label, ". [Date accessed: ",as.Date(Sys.Date()),"]")
  return(cit)
}

# Convenience function for getting bbox from site name
bbox_from_name <- function(site_name){
  # get correct bounding box
  if(site_name %in% c("kong", "Kongsfjorden")) bbox_name <- bbox_kong
  if(site_name %in% c("is", "Isfjorden")) bbox_name <- bbox_is
  if(site_name %in% c("ingle", "Inglefieldbukta")) bbox_name <- bbox_ingle
  if(site_name %in% c("stor", "Storfjorden")) bbox_name <- bbox_stor
  if(site_name %in% c("young", "Young Sound")) bbox_name <- bbox_young
  if(site_name %in% c("disko", "Disko Bay", "Qeqertarsuup Tunua")) bbox_name <- bbox_disko
  if(site_name %in% c("nuup", "Nuup Kangerlua")) bbox_name <- bbox_nuup
  if(site_name %in% c("por", "Porsangerfjorden")) bbox_name <- bbox_por
  if(site_name %in% c("sval", "Svalbard")) bbox_name <- bbox_sval
  if(site_name %in% c("EU")) bbox_name <- bbox_EU
  # This has been coded to allow an error here
  return(bbox_name)
}

# Convenience function for getting wide bbox from site abbreviation
bbox_wide_from_name <- function(site_abv){
  # get correct bounding box
  if(site_abv == "kong") bbox_name <- bbox_kong_wide
  if(site_abv == "is") bbox_name <- bbox_is_wide
  if(site_abv == "ingle") bbox_name <- bbox_ingle
  if(site_abv == "stor") bbox_name <- bbox_stor_wide
  if(site_abv == "young") bbox_name <- bbox_young_wide
  if(site_abv == "disko") bbox_name <- bbox_disko_wide
  if(site_abv == "nuup") bbox_name <- bbox_nuup_wide
  if(site_abv == "por") bbox_name <- bbox_por_wide
  # This has potentially been coded to allow an error here
  return(bbox_name)
}

# Convenience function to convert lon/lat bbox to UTM
bbox_to_UTM <- function(bbox_base, proj_choice = 3996){
  bbox_deg <- st_bbox(c(xmin = bbox_base[1], ymin = bbox_base[3],
                        xmax = bbox_base[2], ymax = bbox_base[4]), crs = st_crs(4326))
  # This should be improved, but will suffice for now
  bbox_utm <- as.data.frame(st_transform(st_as_sfc(bbox_deg), proj_choice))[[1]][[1]][[1]]
}

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
  if(is.na(bathy_file)) bathy_file <- paste0(pCloud_path,"FACE-IT_data/maps/GEBCO/GEBCO_2020.nc")
    
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
## Note that a release of ggOceanMaps broke the backward compatibility of this function
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

# Function for making a site map out of a bounding box
bbox_to_map <- function(coords, bathy_data = NA, lon_pad = 0, lat_pad = 0, add_bbox = F,
                        depths = c(25, 50, 100, 200, 300, 500, 1000, 2000, 10000)){
  
  # Prepare bathymetry data
  if(is.na(bathy_data)){
    bathy_data <- tidync::tidync("~/pCloudDrive/FACE-IT_data/maps/GEBCO/GEBCO_2020.nc") %>% 
      tidync::hyper_filter(lon = dplyr::between(lon, coords[1]-lon_pad, coords[2]+lon_pad), 
                           lat = dplyr::between(lat, coords[3]-lat_pad, coords[4]+lat_pad)) %>% 
      tidync::hyper_tibble() %>% 
      mutate(depth = -elevation) %>% 
      filter(depth > 0)
  }
  
  # Clip coastline polygons for faster plotting
  if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= coords[1]-lon_pad-10, x <= coords[2]+lon_pad+10,
           y >= coords[3]-lat_pad-10, y <= coords[4]+lat_pad+10)
  
  # Get list of contour depths used in figure
  depths_sub <- depths[depths < max(bathy_data$depth)] 
  
  # Map with coast shapefile and bathy contours
  map_res <- ggplot(bathy_data, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = depth)) +
    geom_contour(aes(z = depth, colour = after_stat(level)), breaks = depths_sub, size = 0.3, show.legend = F) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    annotate("rect",  colour = "darkgreen", fill = "darkgreen", alpha = 0.1,
             xmin = coords[1], xmax = coords[2], ymin = coords[3], ymax = coords[4]) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    scale_colour_distiller(palette = "Greys", direction = -1) +
    # scale_fill_viridis_c(option = "E") +
    coord_quickmap(expand = F,
                   xlim = c(coords[1]-lon_pad, coords[2]+lon_pad), 
                   ylim = c(coords[3]-lat_pad, coords[4]+lat_pad)) +
    labs(x = NULL, y = NULL, fill = "depth (m)",
         subtitle = paste0("Contours at: ",paste(c(depths_sub), collapse = ", "), " m")) +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          panel.background = element_rect(fill = "white"),
          legend.position = "bottom")
  # map_res
  
  # Exit
  return(map_res)
}

# Yep
long_to_short_name <- function(long_name){
  if(long_name == "Kongsfjorden") short_name <- "kong"
  if(long_name == "Isfjorden") short_name <- "is"
  if(long_name == "Inglefieldbukta") short_name <- "ingle"
  if(long_name == "Storfjorden") short_name <- "stor"
  if(long_name == "Young Sound") short_name <- "young"
  if(long_name %in% c("Disko Bay", "Qeqertarsuup Tunua")) short_name <- "disko"
  if(long_name == "Nuup Kangerlua") short_name <- "nuup"
  if(long_name == "Porsangerfjorden") short_name <- "por"
  return(short_name)
}

# Filter data by coords and site name
## NB: Meant to be applied directly to PANGAEA data
## This will remove data with missing lon/lat coords
filter_bbox <- function(bbox, df){
  df_bbox <- df %>% 
    filter(lon >= bbox$lon[1], lon <= bbox$lon[2], 
           lat >= bbox$lat[1], lat <= bbox$lat[2])
  return(df_bbox)
}

# Filter data by coords and site name
## Helps to keep site name data without coords
filter_site_bbox <- function(site_name, df){
  bbox <- bbox_from_name(site_name)
  df_site <- df %>% 
    filter(site == site_name)
  df_bbox <- df %>% 
    filter(lon >= bbox[1], lon <= bbox[2], lat >= bbox[3], lat <= bbox[4])
  df_res <- rbind(df_site, df_bbox) %>% distinct()
  rm(df_site, df_bbox); gc()
  return(df_res)
}

# Filter site data based on bbox, variable, and references
filter_site_plural <- function(site_name, base_df = NULL){
  
  # Assign base data if necessary
  if(is.null(base_df)){
    if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")
    base_df <- full_product_sval
  }
  
  # Get basic info
  if(site_name == "sval"){
    long_name <- data.frame(site = "sval", site_long = "Svalbard")
  } else {
    long_name <- filter(long_site_names, site == site_name)
  }
  unique_var <- base_df |> dplyr::select(variable) |> distinct()
  unique_var_site <- unique_var |> filter(grepl(long_name$site_long[1], variable))
  unique_ref <- base_df |> dplyr::select(citation) |> distinct()
  unique_ref_site <- unique_ref |> filter(grepl(long_name$site_long[1], citation))
  
  # Get bbox and filter data accordingly
  bbox <- bbox_from_name(site_name)
  site_bbox <- filter(base_df, lon >= bbox[1], lon <= bbox[2], lat >= bbox[3], lat <= bbox[4])
  
  # Check if there are variables with the long name (i.e. ship AIS data) or citations
  site_var <- filter(base_df, variable %in% unique_var_site$variable)
  site_ref <- filter(base_df, citation %in% unique_ref_site$citation)
  
  # Combine and exit
  site_res <- rbind(site_bbox, site_var, site_ref) |> mutate(site = long_name$site[1]) |> distinct()
  return(site_res)
  # rm(base_df, bbox, long_name, unique_var, unique_var_site, unique_ref, unique_ref_site,
  #    site_bbox, site_var, site_ref, site_res); gc()
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

# Function for loading individual variables from a difficult NetCDF CTD file
CTD_to_long <- function(nc_file, var_id){
  # Get attributes
  nc_TIME <- ncdf4::ncvar_get(nc_file, varid = "TIME")
  nc_PRES <- ncdf4::ncvar_get(nc_file, varid = "PRES")
  nc_LONGITUDE <- ncdf4::ncvar_get(nc_file, varid = "LONGITUDE")
  nc_LATITUDE <- ncdf4::ncvar_get(nc_file, varid = "LATITUDE")
  # Extract one variable and melt it
  nc_val <- data.frame(t(ncdf4::ncvar_get(nc_file, varid = var_id))) |> 
    `colnames<-`(nc_PRES) |> 
    cbind(nc_TIME, nc_LONGITUDE, nc_LATITUDE) |> 
    pivot_longer(min(nc_PRES):max(nc_PRES), values_to = "value", names_to = "depth") |> 
    filter(!is.na(value)) |> 
    mutate(date = as.Date(as.POSIXct((nc_TIME*86400), origin = "1950-01-01 00:00:00"), .keep = "unused"),
           depth = as.numeric(depth)) |> 
    dplyr::rename(lon = nc_LONGITUDE, lat = nc_LATITUDE) %>% 
    summarise(value = mean(value, na.rm = T), .by = c(lon, lat, date, depth)); gc()
  colnames(nc_val)[5] <- tolower(var_id)
  return(nc_val)
}

# Simple wrapper for loading Isfjorden mooring NetCDF files
load_is_mooring <- function(file_name){
  
  # Get NetCDF metadata
  is_dump <- ncdump::NetCDF(file_name)
  is_units <- is_dump$variable %>% 
    filter(!name %in% c("depth", "lon", "lat")) %>% 
    dplyr::select(name, units)
  is_start <- is_dump$attribute$global$time_coverage_start
  # is_citation <- is_dump$attribute$global$references
  file_short <- sapply(strsplit(file_name, "/"), "[[", 8)
  
  # Get correct URL and citation to add to data
  is_ref_info <- data.frame(short_name = c("IN1516.nc", "IN1617.nc", "IN1718.nc", # North moorings
                                           "IS0506.nc", "IS0607.nc", "IS0708.nc", "IS1011.nc", "IS1112.nc", "IS1213.nc", "IS1314.nc", 
                                           "IS1415.nc", "IS1516_ADCP.nc", "IS1516.nc", "IS1617_ADCP.nc", "IS1617.nc", "IS1718.nc"),
                            URL = c("https://data.npolar.no/dataset/111aca43-7f5c-4c15-9f31-dcd3214dbfcb", "https://data.npolar.no/dataset/3078f619-9955-4a7f-9316-fab598fec382",
                                    "https://data.npolar.no/dataset/e9106051-6c44-4849-9d62-04e4a82f1ca9", # North moorings
                                    "https://data.npolar.no/dataset/176eea39-7d99-49d7-a082-b18acf42850c", "https://data.npolar.no/dataset/a1239ca3-79e6-4284-bba5-38028358994a", 
                                    "https://data.npolar.no/dataset/064a09b7-f590-4448-810e-3f287b182dd2", "https://data.npolar.no/dataset/b0e473c4-b5b9-4ebc-96eb-411d47f1d850", 
                                    "https://data.npolar.no/dataset/2be7bdee-c899-45b8-901b-9ec5baa9397a", "https://data.npolar.no/dataset/a247e9a9-4b62-4149-bbf4-83df3576a7c4", 
                                    "https://data.npolar.no/dataset/6813ce6d-bdc9-4375-a310-679e074bee6b", "https://data.npolar.no/dataset/11b7e849-e53d-40d8-909b-13e29c7971a0", 
                                    # Duplicated intentionally
                                    "https://data.npolar.no/dataset/21838303-c9a0-4fc4-aac3-f537b37356df", "https://data.npolar.no/dataset/21838303-c9a0-4fc4-aac3-f537b37356df",
                                    # Duplicated intentionally
                                    "https://data.npolar.no/dataset/cd7a2f7c-abed-4284-b7c5-a9ff43c89afc", "https://data.npolar.no/dataset/cd7a2f7c-abed-4284-b7c5-a9ff43c89afc", 
                                    "https://data.npolar.no/dataset/54dcd0c9-b863-41b1-a72b-0827099ad2b0"), # South moorings
                            citation = c("Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.111aca43",
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 15 Oct 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.3078f619",
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 5 Oct 2017 to 24 Aug 2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.e9106051",
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during September 2005 to September 2006 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.176eea39", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during September 2006 to September 2007 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.a1239ca3", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the outer Isfjorden - South (I-S) during September 2007 to January 2008 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.064a09b7", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 9 Sep 2010 to 3 Sep 2011 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.b0e473c4", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 8 Sep 2011 to 3 Sep 2012 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.2be7bdee", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 6 Sep 2012 to 28 Aug 2013 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.a247e9a9", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 2 Sep 2013 to 26 Aug 2014 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.6813ce6d", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2014 to 24 Aug 2015 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.11b7e849", 
                                         # Duplicated intentionally
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.21838303", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.21838303", 
                                         # Duplicated intentionally
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 19 Aug 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.cd7a2f7c", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 19 Aug 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.cd7a2f7c", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 5 Oct 2017 to 25 Aug 2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.54dcd0c9"))

  # Process data
  res <- hyper_tibble(tidync(file_name)) %>% 
    cbind(hyper_tibble(activate(tidync(file_name), "D2"))) %>%  
    mutate(date = as.Date(as.POSIXct(TIME*86400, origin = "1950-01-01", tz = "UTC")), .keep = "unused") %>% 
    dplyr::rename(lon = LONGITUDE, lat = LATITUDE, depth = MPRES) %>% 
    pivot_longer(cols = c(-"depth", -"STATION", -"lat", -"lon", -"FDEP", -"date"), names_to = "variable", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    left_join(is_units, by = c("variable" = "name")) %>% 
    mutate(units = case_when(units == "degree_Celsius" ~ "°C", TRUE ~ units),
           category = case_when(variable %in% c("OXY", "OXYS") ~ "chem", TRUE ~ "phys"),
           variable = paste0(variable, " [", units,"]"),
           depth = round(depth, 2),
           date_accessed = as.Date("2021-04-15"),
           URL = is_ref_info$URL[is_ref_info$short_name == file_short], 
           citation = is_ref_info$citation[is_ref_info$short_name == file_short]) %>% 
    group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
    summarise(value = round(mean(value, na.rm = T), 5), .groups = "drop")
  return(res)
}

# Simple wrapper for loading GFI mooring NetCDF files
load_GFI <- function(file_name){
  
  # Get NetCDF metadata
  GFI_dump <- ncdump::NetCDF(file_name)
  GFI_units <- GFI_dump$variable %>% 
    filter(!name %in% c("depth", "lon", "lat")) %>% 
    dplyr::select(name, units)
  GFI_lon <- mean(GFI_dump$attribute$global$geospatial_lon_min,
                  GFI_dump$attribute$global$geospatial_lon_max)
  GFI_lat <- mean(GFI_dump$attribute$global$geospatial_lat_min,
                  GFI_dump$attribute$global$geospatial_lat_max)
  GFI_depth <- mean(GFI_dump$attribute$global$geospatial_vertical_min,
                    GFI_dump$attribute$global$geospatial_vertical_max)
  GFI_start <- GFI_dump$attribute$global$instrument_start_time
  GFI_citation <- GFI_dump$attribute$global$citation
  file_short <- sapply(strsplit(file_name, "/"), "[[", 8)
  
  # Get correct FTP link to add to data
  FTP_URL <- as.character(NA)
  if(file_short %in% c("1249_RCM_3148_QC.nc", "1250_RCM_4040_QC.nc", "1251_RCM_6798_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_A/"
  if(file_short %in% c("1252_RCM_9708_QC.nc", "1253_RCM_9993_QC.nc", "1254_RCM_235_QC.nc", "1255_RCM_6197_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_B/"
  if(file_short %in% c("1256_RCM_3160_QC.nc", "1257_RCM_9707_QC.nc", "1258_RCM_2761_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_C/"
  if(file_short %in% c("1259_RCM_8006_QC.nc", "1260_RCM_10007_QC.nc", "1261_RCM_9994_QC.nc", "1262_RCM_2016_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_D/"
  if(file_short %in% c("1263_RCM_9706_QC.nc", "1264_RCM_9130_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_E/"
  if(file_short %in% c("1883_RCM_645_QC.nc", "1884_RCM_8003_QC.nc", "1885_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/KF_201408/"
  if(file_short %in% c("1766_RCM_10907_QC.nc", "1856_RCM_645_QC.nc", "1857_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201308_A/"
  if(file_short %in% c("0712_RCM_12347_QC.nc", "1848_RCM_645_QC.nc", "1849_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_D/"
  if(file_short %in% c("0706_RCM_10907_QC.nc", "1844_RCM_645_QC.nc", "1845_RCM_646_QC.nc", "0686_RCM_8003_QC.nc", "0687_RCM_2761_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201109_A/"
  if(file_short %in% c("1954_RCM_1318_QC.nc", "1955_RCM_645_QC.nc", "1956_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201508_Isfjorden/"
  if(file_short %in% c("0688_RCM_784_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201009_B/"
  if(file_short %in% c("0710_RCM_783_QC.nc", "0711_RCM_784_QC.nc", "1841_RCM_464_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_A/"
  
  # Process data
  res <- hyper_tibble(tidync(file_name)) %>% 
    mutate(date = as.Date(as.POSIXct(time*86400, origin = GFI_start)), 
           lon = GFI_lon, lat = GFI_lat, depth = GFI_depth, .keep = "unused") %>% 
    pivot_longer(c(GFI_units$name), names_to = "variable", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    group_by(lon, lat, date, depth, variable) %>% 
    summarise(value = case_when(variable == "dir" ~ as.numeric(round(mean.circular(circular(value, units = "degrees")))),
                                TRUE ~ round(mean(value, na.rm = T), 3)), .groups = "drop") %>% 
    distinct() %>% 
    mutate(value = case_when(variable == "dir" & value < 0 ~ value + 360, TRUE ~ value)) %>% 
    replace(is.na(.), NA) %>% 
    left_join(GFI_units, by = c("variable" = "name")) %>% 
    mutate(URL = FTP_URL,
           citation = GFI_citation,
           category = case_when(variable %in% c("oxy") ~ "chem", TRUE ~ "phys"),
           units = case_when(units == "Celsius" ~ "°C", units == "degree" ~ "°", TRUE ~ units),
           variable = paste0(variable, " [", units,"]")) %>% 
    dplyr::select(URL, citation, lon, lat, date, depth, category, variable, value) %>% 
    distinct()
  return(res)
}

# Simple wrapper for loading SAMS mooring NetCDF files
# dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_SAMS", full.names = T)
# file_names <- dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_SAMS/", full.names = T)#13-25
# test1 <- load_SAMS(file_names[17])
# file_name <- file_names[1]
# rm(file_names, file_name)
load_SAMS <- function(file_name){
  
  # Get NetCDF metadata
  SAMS_dump <- ncdump::NetCDF(file_name)
  SAMS_units <- SAMS_dump$variable %>% 
    filter(!name %in% c("depth", "ndepth", "bottom_depth", "nominal_depth",
                        "longitude", "latitude", "mooring", "time")) %>% 
    dplyr::select(name, units)
  SAMS_start <- SAMS_dump$attribute$global$time_coverage_start
  file_short <- sapply(strsplit(file_name, "/"), "[[", 9)
  
  # Get correct FTP link to add to data
  SAMS_URL <- as.character(NA)
  SAMS_ref <- as.character(NA)
  if(file_short %in% c("KF_17_18_pro_sbe16p_6067_34m.nc", "KF_17_18_pro_sbe37_7248_33m.nc", "KF_17_18_pro_sbe37_9114_217m.nc",
                       "KF_17_18_pro_sbe56_2444_38m.nc", "KF_17_18_pro_sbe56_2445_48m.nc", "KF_17_18_pro_sbe56_2447_59m.nc",
                       "KF_17_18_pro_sbe56_2650_80m.nc", "KF_17_18_pro_sbe56_2656_103m.nc", "KF_17_18_pro_sbe56_2657_126m.nc",
                       "KF_17_18_pro_sbe56_2658_156m.nc", "KF_17_18_pro_sbe56_2659_186m.nc", "KF_17_18_pro_sbe56_2660_217m.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00065"
    SAMS_ref <- "Cottier, F., Berge, J., Dumont, E., Kopec, T. P., Venables, E. J., Vogedes, D. L., UiT The Arctic University of Norway, Scottish Association for Marine Science (2021).Temperature, salinity, light and fluorescence (CTD) measurements from the Kongsfjorden (Svalbard) marine observatory (mooring) August 2017-August 2018 [Data set]. Norstore. https://doi.org/10.11582/2021.00065"
  } else if(file_short %in% c("KF_16_17_pro_sbe16p_6066_26m.nc", "KF_16_17_pro_sbe37_5509_96m.nc", "KF_16_17_pro_sbe37_5510_208m.nc",
                              "KF_16_17_pro_sbe37_8478_25m.nc", "KF_16_17_pro_sbe56_5207_31m.nc", "KF_16_17_pro_sbe56_5208_41m.nc",
                              "KF_16_17_pro_sbe56_5209_51m.nc", "KF_16_17_pro_sbe56_5210_72m.nc", "KF_16_17_pro_sbe56_5211_95m.nc",
                              "KF_16_17_pro_sbe56_5212_118m.nc", "KF_16_17_pro_sbe56_5213_148m.nc", "KF_16_17_pro_sbe56_5214_178m.nc",
                              "KF_16_17_pro_sbe56_5215_208m.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00062"
    SAMS_ref <- "Cottier, F., Berge, J., Dumont, E., Griffith, C., Beaton, J., Vogedes, D. L., UiT The Arctic University of Norway, Scottish Association for Marine Science (2021).Temperature, salinity, light and fluorescence (CTD) measurements from the Kongsfjorden (Svalbard) marine observatory (mooring) August 2016-August 2017 [Data set]. Norstore. https://doi.org/10.11582/2021.00062"
  } else if(file_short %in% c("KF_15_16_pro_sbe16p_6101_25m.nc", "KF_15_16_pro_sbe37_9112_27m.nc", "KF_15_16_pro_sbe37_9114_209m.nc",
                              "KF_15_16_pro_sbe56_2444_37m.nc", "KF_15_16_pro_sbe56_2445_47m.nc", "KF_15_16_pro_sbe56_2447_57m.nc",
                              "KF_15_16_pro_sbe56_2650_79m.nc", "KF_15_16_pro_sbe56_2656_102m.nc", "KF_15_16_pro_sbe56_2657_125m.nc",
                              "KF_15_16_pro_sbe56_2658_155m.nc", "KF_15_16_pro_sbe56_2659_185m.nc", "KF_15_16_pro_sbe56_2669_215m.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00061"
    SAMS_ref <- "Cottier, F., Berge, J., Griffith, C., Dumont, E., Beaton, J., Vogedes, D. L., UiT The Arctic University of Norway, Scottish Association for Marine Science (2021).Temperature, salinity, light and fluorescence (CTD) measurements from the Kongsfjorden (Svalbard) marine observatory (mooring) September 2015-August 2016 [Data set]. Norstore. https://doi.org/10.11582/2021.00061"
  } else if(file_short %in% c("KF_20_20_pro_avg24h_sbe16p_5181.nc", "KF_20_20_pro_avg24h_sbe56_10012.nc", "KF_20_20_pro_avg24h_sbe56_10040.nc",
                              "KF_20_20_pro_avg24h_sbe56_10055.nc", "KF_20_20_pro_avg24h_sbe56_10059.nc", "KF_20_20_pro_avg24h_sbe56_10060.nc",
                              "KF_20_20_pro_avg24h_sbe56_10061.nc", "KF_20_20_pro_avg24h_sbe56_10062.nc", "KF_20_20_pro_avg24h_sbe56_10066.nc",
                              "KF_20_20_pro_avg24h_sbe56_10067.nc", "KF_20_20_pro_sbe16p_5181.nc", "KF_20_20_pro_sbe56_10012.nc",
                              "KF_20_20_pro_sbe56_10040.nc", "KF_20_20_pro_sbe56_10055.nc", "KF_20_20_pro_sbe56_10059.nc",
                              "KF_20_20_pro_sbe56_10060.nc", "KF_20_20_pro_sbe56_10061.nc", "KF_20_20_pro_sbe56_10062.nc",
                              "KF_20_20_pro_sbe56_10066.nc", "KF_20_20_pro_sbe56_10067.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00010"
    SAMS_ref <- "Berge, J., Cottier, F., Kopec, T., Dumont, E., Venables, E., Vogedes, D. (2021).Temperature, salinity, light and chl a measurements from the Kongsfjorden SIOS marine observatory January-September 2020 [Data set]. Norstore. https://doi.org/10.11582/2021.00010"
  }
  
  # Get base CTD data as there are two different formats
  if(file_short %in% c("KF_20_20_pro_avg24h_sbe16p_5181.nc", "KF_20_20_pro_avg24h_sbe56_10012.nc", "KF_20_20_pro_avg24h_sbe56_10040.nc",
                       "KF_20_20_pro_avg24h_sbe56_10055.nc", "KF_20_20_pro_avg24h_sbe56_10059.nc", "KF_20_20_pro_avg24h_sbe56_10060.nc",
                       "KF_20_20_pro_avg24h_sbe56_10061.nc", "KF_20_20_pro_avg24h_sbe56_10062.nc", "KF_20_20_pro_avg24h_sbe56_10066.nc",
                       "KF_20_20_pro_avg24h_sbe56_10067.nc", "KF_20_20_pro_sbe16p_5181.nc", "KF_20_20_pro_sbe56_10012.nc",
                       "KF_20_20_pro_sbe56_10040.nc", "KF_20_20_pro_sbe56_10055.nc", "KF_20_20_pro_sbe56_10059.nc",
                       "KF_20_20_pro_sbe56_10060.nc", "KF_20_20_pro_sbe56_10061.nc", "KF_20_20_pro_sbe56_10062.nc",
                       "KF_20_20_pro_sbe56_10066.nc", "KF_20_20_pro_sbe56_10067.nc")) {
    SAMS_lon <- mean(as.numeric(SAMS_dump$attribute$global$geospatial_lon_min),
                     as.numeric(SAMS_dump$attribute$global$geospatial_lon_max), na.rm = T)
    SAMS_lat <- mean(as.numeric(SAMS_dump$attribute$global$geospatial_lat_min),
                     as.numeric(SAMS_dump$attribute$global$geospatial_lat_max))
    res_base <- hyper_tibble(tidync(file_name)) %>% 
      mutate(longitude = SAMS_lon, latitude = SAMS_lat)
  } else {
    res_base <- hyper_tibble(tidync(file_name))
  }
  
  # Correct for some missing depth columns
  if("nominal_depth" %in% colnames(res_base) & !"depth" %in% colnames(res_base)){
    res_base <- dplyr::rename(res_base, depth = nominal_depth)
  }
  if("ndepth" %in% colnames(res_base) & !"depth" %in% colnames(res_base)){
    res_base <- dplyr::rename(res_base, depth = ndepth)
  }
  
  # Process data
  res <- res_base %>% 
    mutate(date = as.Date(as.POSIXct(time, origin = "1970-01-01")), .keep = "unused") %>% 
    pivot_longer(c(SAMS_units$name), names_to = "variable", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    dplyr::rename(lon = longitude, lat = latitude) %>% 
    group_by(lon, lat, date, variable) %>% 
    summarise(depth = round(mean(depth, na.rm = T), 2), # The mooring moves very slightly up and down over a day
              value = round(mean(value, na.rm = T), 4), .groups = "drop") %>% 
    distinct() %>% 
    replace(is.na(.), NA) %>% 
    left_join(SAMS_units, by = c("variable" = "name")) %>% 
    mutate(URL = SAMS_URL,
           citation = SAMS_ref,
           units = case_when(units == "degrees_Celsius" ~ "°C", TRUE ~ units),
           category = case_when(variable %in% c("fluo_V", "fluo") ~ "bio", TRUE ~ "phys"),
           variable = paste0(variable, " [", units,"]")) %>% 
    dplyr::select(URL, citation, lon, lat, date, depth, category, variable, value) %>% 
    distinct()
  return(res)
}

# Simple wrapper for loading met station NetCDF data
load_met_NetCDF <- function(file_name){
  
  # Get NetCDF metadata
  file_short <- sapply(strsplit(file_name, "/"), "[[", 5)
  
  # Determine URL
  met_URL <- paste0("https://thredds.met.no/thredds/catalog/met.no/observations/stations/catalog.html?dataset=met.no/observations/stations/",file_short)
  
  # Citation info
  nc_file <- ncdf4::nc_open(file_name)
  ref_text <- paste0(ncdf4::ncatt_get(nc_file, varid = 0, "title")$value, 
                     " (", lubridate::year(ncdf4::ncatt_get(nc_file, varid = 0, "date_created")$value), "). ",
                     ncdf4::ncatt_get(nc_file, varid = 0, "institution")$value, ". ",
                     ncdf4::ncatt_get(nc_file, varid = 0, "source")$value, ". ",
                     ncdf4::ncatt_get(nc_file, varid = 0, "metadata_link")$value)
  
  # Coords
  # TODO: Figure out how to extract coordinates from messy NetCDF format
  # NCdump doesn't work and can't access coord values via ncatt_get
  # ncdump::NetCDF(file_name)
  # ncdf4::ncatt_get(nc_file, varid = 0, attname = "latitude")
  
  # Process data
  suppressWarnings(
  res <- hyper_tibble(tidync(file_name)) %>% 
    # mutate(across(everything(), ~replace(., . == 9969209968386869046778552952102584320, NA)),
    mutate(date = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>% 
    # NB: Column order differs between files so need to rather state which columns not to melt
    pivot_longer(cols = c(-date, -time), names_to = "variable", values_to = "value") %>% 
    mutate(value = case_when(variable == "air_temperature_2m" & value > 500 ~ 1000001,
                             variable == "air_temperature_2m" & value < 10 ~ 1000001, TRUE ~ value)) %>% 
    filter(value <= 1000000, value >= -100000) %>% # Remove dodgy value
    pivot_wider(names_from = variable, values_from = value) %>% 
    group_by(date) %>% 
    summarise(air_temperature_2m = mean(air_temperature_2m, na.rm = T),
              air_pressure_at_sea_level = mean(air_pressure_at_sea_level, na.rm = T),
              surface_air_pressure_2m = mean(surface_air_pressure_2m, na.rm = T),
              wind_speed_10m = mean(wind_speed_10m, na.rm = T),
              relative_humidity = mean(relative_humidity, na.rm = T),
              air_pressure_at_sea_level_qnh = mean(air_pressure_at_sea_level_qnh, na.rm = T),
              wind_from_direction_10m = as.numeric(round(mean.circular(circular(wind_from_direction_10m, units = "degrees"), na.rm = T)))) %>% 
    # cbind(hyper_tibble(activate(tidync(file_name), "S"))) %>%  # This line throws an unneeded warning
    # dplyr::rename(lon = longitude, lat = latitude) %>% 
    mutate(lon = NA, lat = NA) %>%
    pivot_longer(air_temperature_2m:air_pressure_at_sea_level_qnh, names_to = "variable", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    mutate(URL = met_URL,
           citation = ref_text, depth = NA,
           units = case_when(variable == "relative_humidity" ~ "1",
                             variable == "surface_air_pressure_2m" ~ "Pa",
                             variable == "air_temperature_2m" ~ "K",
                             variable == "wind_from_direction_10m" ~ "°",
                             variable == "wind_speed_10m" ~ "m s-1",
                             variable == "air_pressure_at_sea_level" ~ "Pa",
                             variable == "air_pressure_at_sea_level_qnh" ~ "hPa"),
           depth = case_when(variable == "surface_air_pressure_2m" ~ -2,
                             variable == "air_temperature_2m" ~ -2,
                             variable == "wind_from_direction_10m" ~ -10,
                             variable == "wind_speed_10m" ~ -10,
                             variable == "air_pressure_at_sea_level" ~ 0,
                             variable == "air_pressure_at_sea_level_qnh" ~ 0),
           variable = paste0(variable," [", units,"]"),
           category = "phys") %>% 
    # Convert K to C
    mutate(value = case_when(grepl("air_temperature_2m", variable) ~ value - 273.15, TRUE ~ value),
           variable = case_when(grepl("air_temperature_2m", variable) ~ "TTT [°C]", TRUE ~ variable)) %>% 
    dplyr::select(URL, citation, lon, lat, date, depth, category, variable, value)
  )
  return(res)
}

# Function for loading Norwegian hydrographic data
# year_choice <- 2009 # tester...
load_nor_hydro <- function(year_choice, date_accessed){
  
  # Get file names
  nor_hydro_files <- dir("~/pCloudDrive/FACE-IT_data/porsangerfjorden/UiT", full.names = T)
  
  # Prepare URL and citation info
  cite_info <- data.frame(year = c(1953, 1955, 1957, 1958, 1959, 1966, 1979, 1980, 1981, 1982, 1983, 
                                   1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 
                                   1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                                   2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013),
                          URL = c("https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/KOBJ5W",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/J1BEYR",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/P916SW",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/AHXUIC",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/VQHYNU",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/JFNJXM",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/JTOHCC",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/EGG0AF",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/18AWWA",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/OJBUB9",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/E8YQPO",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/4LDOM0",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/DQHZYY",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/BOBHPL",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/SATEDJ",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/Y7QF58",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/ABONWP",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/LYOY9A",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/D6WLUE",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/M2IWUU",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/QY9WNB",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/G6INDC",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/BYDJU6",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/RYFBFS",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/7CNDUJ",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/XOHNT7",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/PFJWSN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/FD6BVN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/0NGBMI",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/RKCFIP",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/H3NZSX",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/FXHQYN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/9IR4ML",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/JEZXYF",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/KSHAH5",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/CT6XYN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/DFQSRS",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/PPIVEO",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/20VUXR",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/EZPFBU",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/HDUPX3"),
                          citation = c('Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1953", https://doi.org/10.18710/KOBJ5W, DataverseNO, V1, UNF:6:U+/7OuDYgkpYfywsAA4hcg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1955", https://doi.org/10.18710/J1BEYR, DataverseNO, V1, UNF:6:4kJ+l/55yWZvA2Sig7j1/Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1957", https://doi.org/10.18710/P916SW, DataverseNO, V1, UNF:6:DCfYcMk77aH4awCjob44aQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1958", https://doi.org/10.18710/AHXUIC, DataverseNO, V1, UNF:6:ZT2z1HQ5AEh1ElIohNbAAA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1959", https://doi.org/10.18710/VQHYNU, DataverseNO, V1, UNF:6:Ga6QZneh2eYPjYui9OlD0Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1966", https://doi.org/10.18710/JFNJXM, DataverseNO, V1, UNF:6:nrDwQaaH0qbImYgo+ZsbCA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf, (2019), "Hydrographic data from Northern Norwegian fjords - 1979", https://doi.org/10.18710/JTOHCC, DataverseNO, V1, UNF:6:mnZkBDq8vSnj4ZHNXzu0Ow== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1980", https://doi.org/10.18710/EGG0AF, DataverseNO, V1, UNF:6:LgKTqfUGRvwLiaMD1+AunQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1981", https://doi.org/10.18710/18AWWA, DataverseNO, V1, UNF:6:qtDFSir+lAFFcK5NinXMSg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1982", https://doi.org/10.18710/OJBUB9, DataverseNO, V1, UNF:6:0MyG3WJe0WTvqQMEw81dSQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1983", https://doi.org/10.18710/E8YQPO, DataverseNO, V1, UNF:6:Nz6wtToO689GyTSm7lSteA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1984", https://doi.org/10.18710/4LDOM0, DataverseNO, V1, UNF:6:pnZJNguSfyRl92dhNq8Iig== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1985", https://doi.org/10.18710/DQHZYY, DataverseNO, V1, UNF:6:rwZj9dKm4dCOXdzCWjmYCw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1986", https://doi.org/10.18710/BOBHPL, DataverseNO, V1, UNF:6:FzYW+79TOxhtiLq7opQGBg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1987", https://doi.org/10.18710/SATEDJ, DataverseNO, V1, UNF:6:YLM/LNPpIEl64VjAgflbug== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1988", https://doi.org/10.18710/Y7QF58, DataverseNO, V1, UNF:6:3GX0zJ87HG2hqQvp2ZkbWg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2019), "Hydrographic data from Northern Norwegian fjords - 1989", https://doi.org/10.18710/ABONWP, DataverseNO, V1, UNF:6:+pxpAE3V7Sw/LNhL3ZVA3Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1990", https://doi.org/10.18710/LYOY9A, DataverseNO, V1, UNF:6:tdP+AhP2kKYjoypOmCLYSw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1991", https://doi.org/10.18710/D6WLUE, DataverseNO, V1, UNF:6:3tap+ji0IJP6OZm94bG5Fw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1992", https://doi.org/10.18710/M2IWUU, DataverseNO, V1, UNF:6:HWjN+ptN14Crh9QVC+42Gw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1993", https://doi.org/10.18710/QY9WNB, DataverseNO, V1, UNF:6:sQsN3YOltOxjzg5dotk5zg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1994", https://doi.org/10.18710/G6INDC, DataverseNO, V1, UNF:6:cju+LW3fpKYGvIcvypCjdA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1995", https://doi.org/10.18710/BYDJU6, DataverseNO, V1, UNF:6:l7fIO8PYNU4Mk7Yk42cURA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1996", https://doi.org/10.18710/RYFBFS, DataverseNO, V1, UNF:6:vEmlDRhivki61fHOQ6ijzA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1997", https://doi.org/10.18710/7CNDUJ, DataverseNO, V1, UNF:6:l6GPGt1jCCLNzhKPwm6ZRA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1998", https://doi.org/10.18710/XOHNT7, DataverseNO, V1, UNF:6:U+YggbzuI+fW+9ueqvd3uA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 1999", https://doi.org/10.18710/PFJWSN, DataverseNO, V1, UNF:6:h4mv7XkepqZ0OVIHm9DS6A== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2000", https://doi.org/10.18710/FD6BVN, DataverseNO, V1, UNF:6:R3dstLk3piyR7hjtbXiu8w== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2001", https://doi.org/10.18710/0NGBMI, DataverseNO, V1, UNF:6:Jr2Ism1cRI8wmXkcX4sP2Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2002", https://doi.org/10.18710/RKCFIP, DataverseNO, V1, UNF:6:wpaUwIuvwW7taPbBnTfLEQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2003", https://doi.org/10.18710/H3NZSX, DataverseNO, V1, UNF:6:LR5EjU5pBBLIjThcxBTS1w== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2004", https://doi.org/10.18710/FXHQYN, DataverseNO, V1, UNF:6:XKq/t0WTJIPesFbgdUBuJA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2005", https://doi.org/10.18710/9IR4ML, DataverseNO, V1, UNF:6:InE/gQ/5sZo8A4b1XF33zg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2006", https://doi.org/10.18710/JEZXYF, DataverseNO, V1, UNF:6:XHlv71Z+03e23XnEWrJasQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2007", https://doi.org/10.18710/KSHAH5, DataverseNO, V1, UNF:6:s/c5PNYshJLXmJ8WXxteIg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2008", https://doi.org/10.18710/CT6XYN, DataverseNO, V1, UNF:6:In414SgzxVtks0XOvJAUvw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2009", https://doi.org/10.18710/DFQSRS, DataverseNO, V1, UNF:6:aHfoNHJB2YHbAvj01vzmsQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2010", https://doi.org/10.18710/PPIVEO, DataverseNO, V1, UNF:6:eUOo8mYL1/ZWp+jmLIY7kA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2011", https://doi.org/10.18710/20VUXR, DataverseNO, V1, UNF:6:Ku9NJ9iIJUPTHMsi2X6SQA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2012", https://doi.org/10.18710/EZPFBU, DataverseNO, V1, UNF:6:vAkMKNoql1CS8Z46ZXdH4g== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, (2018), "Hydrographic data from Northern Norwegian fjords - 2013", https://doi.org/10.18710/HDUPX3, DataverseNO, V1, UNF:6:sjeZqtEw7ZmwTEWTkLHpNg== [fileUNF] '))
  
  # Load the historic data if a specific year isn't called
  if(year_choice < 1953){
    suppressMessages(
    res_raw <- read_csv(nor_hydro_files[grep("historic", nor_hydro_files)]) %>% 
      mutate(URL = "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/ZEHCIN",
             citation = 'Mankettikkara, Rahman; Eilertsen, Hans Chr, (2018), "Historic hydrographic data from Northern Norwegian fjords and coasts", https://doi.org/10.18710/ZEHCIN, DataverseNO, V1, UNF:6:m1/JT5Q0HncOrUyhzsY05Q== [fileUNF] ') %>% 
      dplyr::rename(temp = Temp, sal = Salinity, dens = Density, depth = `Depth (m)`, date = Date, lat = `Lat (N)`, lon = `Lon (E)`)
    )
  } else {
    sub_files <- nor_hydro_files[grep(year_choice, nor_hydro_files)]
    if(length(sub_files) == 0) return()
    suppressMessages(res_data <- read_csv(sub_files[1]))
    suppressMessages(res_stations <- read_csv(sub_files[2]))
    if(typeof(res_data$station_number) != typeof(res_stations$`Station Number`)){
      res_data$station_number <- as.numeric(res_data$station_number)
      res_stations$`Station Number` <- as.numeric(res_stations$`Station Number`)
    }
    if(typeof(res_data$ship) != typeof(res_stations$Ship)){
      res_data$ship <- as.numeric(res_data$ship)
      res_stations$Ship <- as.numeric(res_stations$Ship)
    }
    res_raw <- left_join(res_data, res_stations, 
                         by = c("station_number" = "Station Number", "ship" = "Ship")) |> #,
                         # NB: The station list doubles without identifiers in 1958
                         # But all of these data lay outside of the study site....
                         # multiple = "all") %>% 
      mutate(URL = cite_info$URL[cite_info$year == year_choice],
             citation = cite_info$citation[cite_info$year == year_choice]) %>% 
      dplyr::rename(date = Date, lat = `Latitude °N`, lon = `Longitude °E`) |> 
      filter(depth != "****")
  }
  
  # Detect columns in data for pivoting longer
  col_pivot <- colnames(res_raw)[grepl("temp|sal|dens", colnames(res_raw))]
  
  # Process and exit
  res <- res_raw %>% 
    filter(lon >= bbox_por[1], lon <= bbox_por[2],
           lat >= bbox_por[3], lat <= bbox_por[4]) %>% 
    pivot_longer(all_of(col_pivot), names_to = "variable", values_to = "value") %>% 
    mutate(date_accessed = date_accessed, category = "phys",
           value = case_when(variable == "sal" & value %in% c(-1, -9999) ~ NA, TRUE ~ value),
           variable = case_when(variable == "temp" ~ "temp [°C]", 
                                variable == "sal" ~ "sal [PSU]",
                                variable == "dens" ~ "dens [kg/m3]"),
           depth = as.numeric(depth)) %>% 
    distinct() %>% 
    group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
    summarise(value = mean(value, na.rm = T), .groups = "drop")
  # print(paste0("Loaded ",lubridate::year(min(res_raw$date)),": ", nrow(res)," rows"))
  return(res)
}

# Function for loading multiple similar model files
load_model <- function(file_stub, pCloud = F){
  
  # Set file pathway
  if(pCloud){
    file_path <- "~/pCloudDrive/FACE-IT_data/model/"
  } else{
    file_path <- "~/WP1/data/model/"
  }
  
  # Basic model info
  # ncdump::NetCDF(paste0(file_path, file_stub, "2.6.nc"))
  model_vars <- ncdump::NetCDF(paste0(file_path, file_stub, "2.6.nc"))$variable
  model_coords <- tidync::tidync(paste0(file_path, file_stub, "2.6.nc")) %>% activate("D0,D1") %>% hyper_tibble()
  
  # Load individual RCPs
  model_2.6 <- tidync::tidync(paste0(file_path, file_stub, "2.6.nc")) %>% hyper_tibble() %>% mutate(proj = "RCP 2.6")
  model_4.5 <- tidync::tidync(paste0(file_path, file_stub, "4.5.nc")) %>% hyper_tibble() %>% mutate(proj = "RCP 4.5")
  model_8.5 <- tidync::tidync(paste0(file_path, file_stub, "8.5.nc")) %>% hyper_tibble() %>% mutate(proj = "RCP 8.5")
  
  # Combine all RCPs and exit
  model_all <- rbind(model_2.6, model_4.5, model_8.5) %>% 
    left_join(model_coords, by = c("X", "Y")) %>% 
    mutate(date = as.Date(as.character(as.PCICt(T*3600, cal = "noleap", origin = "1950-01-01 01:00:00", tz = "UTC")))) %>% 
    dplyr::rename(lon = Long, lat = Latt, topo = Topo, land = RMask, depth = Z) %>% 
    dplyr::select(proj, land, lon, lat, topo, date, depth, Salt:pco2w)
  return(model_all)
}

# Convenience function for loading only the subset of coords for hi-res ice data
load_ice_coords <- function(site_name, res = "1km"){
  
  # get correct bounding box
  bbox_sub <- bbox_from_name(site_name)
  
  # Load 4km res mask if necessary
  if(!exists("ice_coords_4km")){
    ice_coords_4km <- tidync::tidync("~/pCloudDrive/FACE-IT_data/ice/MASIE_4km/masie_lat_lon_4km.nc") %>% 
      tidync::hyper_tibble() %>% dplyr::rename(lon = longitude, lat = latitude)
  }
  
  # Could potentially use the 4km mask to approximate the xy and then refine
  ice_coords_sub <- ice_coords_4km %>% 
    filter(lon >= bbox_sub[1], lon <= bbox_sub[2],
           lat >= bbox_sub[3], lat <= bbox_sub[4])
  if(res == "1km"){
    x_min <- min(ice_coords_sub$x)-2000; x_max <- max(ice_coords_sub$x)+2000
    y_min <- min(ice_coords_sub$y)-2000; y_max <- max(ice_coords_sub$y)+2000
    ice_coords_res <- tidync::tidync("~/pCloudDrive/FACE-IT_data/ice/MASIE_1km/masie_lat_lon_1km.nc") %>% 
      tidync::hyper_filter(x = dplyr::between(x, x_min, x_max),
                           y = dplyr::between(y, y_min, y_max)) %>%
      tidync::hyper_tibble() %>% 
      dplyr::rename(lon = longitude, lat = latitude) %>% 
      filter(lon >= bbox_sub[1], lon <= bbox_sub[2],
             lat >= bbox_sub[3], lat <= bbox_sub[4])
  } else{
    ice_coords_res <- ice_coords_sub
  }
  
  # Exit
  return(ice_coords_res)
}

# Function for loading ice data within a bounding box
load_ice_gridded <- function(file_name, ice_coords){
  df_sub <- tidync::tidync(file_name) %>% 
    tidync::hyper_filter(x = dplyr::between(x, min(ice_coords$x), max(ice_coords$x)),
                         y = dplyr::between(y, min(ice_coords$y), max(ice_coords$y))) %>%
    tidync::hyper_tibble() %>% 
    left_join(ice_coords, by = c("x", "y")) %>%
    filter(!is.na(lon)) %>% # Some pixels don't seem to be able to convert to lon/lat projection
    mutate(date = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>% 
    dplyr::select(lon, lat, date, sea_ice_extent)
  return(df_sub)
}

# Function for loading old CTD data from AWI
load_CTD_DATEN <- function(file_name){
  # Get coords based on file_name
  if(str_detect(file_name, "LONDON")){
    lon_site = 12.0444
    lat_site = 78.9611
  } else if(str_detect(file_name, "NANSEN")){
    lon_site = 11.9231
    lat_site = 78.9286
  } else if(str_detect(file_name, "STEG")){
    lon_site = 11.9194
    lat_site = 78.9303
  } else if(str_detect(file_name, "TONNE")){
    lon_site = 11.9392
    lat_site = 78.9286
  } else if(str_detect(file_name, "NESET")){
    lon_site = 11.9872
    lat_site = 78.9958
  } else {
    lon_site = NA
    lat_site = NA
  }
  # Load+process file
  df_res <- read.table(file_name, skip = 36, header = F,
                       col.names = c("press", "temp", "cond", "Cvx", "Cvy", "Hx", "Hy", "sal",
                                     "sigma", "sound", "CSPD", "CDIR", "date", "time")) %>% 
    mutate(date = as.POSIXct(paste(date, time), tryFormats = c("%d.%m.%y %H:%M:%S")),
           lon = lon_site, lat = lat_site) %>% 
    dplyr::select(lon, lat, date, press, temp, cond, sal, sigma, CSPD, CDIR) %>% 
    dplyr::rename(`press [dbar]` = press, `temp [°C]` = temp, `cond [mS/cm]` = cond, 
                  `sal [ppt]` = sal, `sigma [kg/m3]` = sigma, `spd [cm/s]` = CSPD, `dir [°]` = CDIR)
  return(df_res)
}

# Load GRDC river discharge files
# NB: This requires a function as they have lon/lat values in the text headers
load_GRDC <- function(file_name){
  file_text <- str_split(read_file(file_name), "\r\n")
  lat <- as.numeric(gsub("[^0-9.-]", "", file_text[[1]][13]))
  lon <- as.numeric(gsub("[^0-9.-]", "", file_text[[1]][14]))
  catch <- str_remove(file_text[[1]][15], "(km\xb2)")
  suppressWarnings(catch <- as.numeric(gsub("[^0-9.-]", "", catch))) # Missing catchment value warning
  depth <- -as.numeric(gsub("[^0-9.-]", "", file_text[[1]][16]))
  if(depth == 999) depth <- NA
  suppressMessages(
  res <- read_delim(file_name, skip = 36, delim = ";") %>% 
    dplyr::rename(date = `YYYY-MM-DD`, `Q [m3/s]` = ` Value`) %>% 
    mutate(lon = lon, lat = lat, depth = depth, catch = catch,
           date = as.Date(date),
           `Q [m3/s]` = as.numeric(`Q [m3/s]`),
           `Q [m3/s]` = na_if(`Q [m3/s]`, -999)) %>% 
    dplyr::select(lon, lat, catch, depth, date, `Q [m3/s]`)# %>% 
    # mutate(file_name = file_name) # For testing
  )
  return(res)
  # rm(file_name, file_text, lat, lon, catch, depth, res); gc()
}

# Load machine csv files from Seabird ascii format
load_Seabird <- function(file_name){
  
  # Get metadata from file header
  file_meta <- read_lines(file_name, skip = 10, n_max = 3)
  file_lat <- gsub(" N", "", sapply(strsplit(file_meta[1], "Latitude = "), "[[", 2)) |> 
    measurements::conv_unit(from = 'deg_dec_min', to = 'dec_deg') |> as.numeric() |> round(4)
  file_lon <- gsub(" E", "", sapply(strsplit(file_meta[2], "Longitude = "), "[[", 2)) |> 
    measurements::conv_unit(from = 'deg_dec_min', to = 'dec_deg') |> as.numeric() |> round(4)
  # NB: This assumes an English language local to get the month abbreviations correctly
  file_time <- as.POSIXct(sapply(strsplit(file_meta[3], "\\(Time\\) = "), "[[", 2), format = "%b %d %Y %H:%M:%S")
  
  # Column names copied from file
  col_Seabird <- c("pressure [db]", "temp 1 [°C]", "temp 2 [°C]", "cond 1 [S m]", "cond 2 [S m]", 
                   "O2 raw [V]", "fluor [µg l]", "beam transmission [%]", "turbidity",
                   "altimetry [m]", "pot temp 1 [°C]", "sal 1 [PSU]", "density 1 [sigma-theta, kg m-3]",
                   "pot temp 2 [°C]", "sal 2 [PSU]", "density 2 [sigma-theta, kg m-3]", 
                   "O2 [ml l]", "O2 [µmol kg]", "flag")
  df_Seabird <- read.table(file_name, sep = "", skip = 345, header = FALSE) |> 
    `colnames<-`(col_Seabird) |> 
    mutate(lon = file_lon, lat = file_lat, time = file_time, depth = seacarb::p2d(`pressure [db]`, lat)) |> 
    rowwise() |> 
    mutate(`temp [°C]` = mean(c(`temp 1 [°C]`, `temp 2 [°C]`)),
           `cond [S m]` = mean(c(`cond 1 [S m]`, `cond 2 [S m]`)),
           `pot temp [°C]` = mean(c(`pot temp 1 [°C]`, `pot temp 2 [°C]`)),
           `sal [PSU]` = mean(c(`sal 1 [PSU]`, `sal 2 [PSU]`)),
           `density [sigma-theta, kg m-3]` = mean(c(`density 1 [sigma-theta, kg m-3]`, 
                                                    `density 2 [sigma-theta, kg m-3]`))) |> 
    dplyr::select(lon:`density [sigma-theta, kg m-3]`, `pressure [db]`, 
                  `O2 raw [V]`:`altimetry [m]`, `O2 [ml l]`, `O2 [µmol kg]`)
  return(df_Seabird)
}

# Investigate optics files
load_optics <- function(file_name){
  
  # Open NetCDF file and get metadata
  nc_file <- nc_open(file_name)
  lon <- as.vector(ncvar_get(nc_file, "Longitude"))
  lat <- as.vector(ncvar_get(nc_file, "Latitude"))
  vars <- names(nc_file$var)
  vars <- vars[-which(vars %in% c("Minimum", "Maximum", "Mean", "Median", "StDev", "NumObs"))]
  var_name <- sapply(strsplit(file_name, "/"), "[[", 6)
  
  # Wrapper for variable extraction
  extract_var <- function(var_id, nc_file, lon, lat){
    var_t <- as.Date(sapply(strsplit(var_id, "_"), "[[", 2), format = "%Y.%m.%d")
    nc_var_1 <- as.data.frame(ncvar_get(nc_file, varid = var_id)) |> 
      `colnames<-`(lat) |> mutate(lon = lon) |> 
      pivot_longer(-lon, names_to = "lat", values_to = "value") |> filter(!is.na(value)) |> 
      mutate(lat = as.numeric(lat), t = var_t, .before = "value")
  }
  
  # Get all variables
  nc_all <- plyr::ldply(vars, extract_var, .parallel = TRUE, nc_file = nc_file, lon = lon, lat = lat) |> 
    mutate(variable = var_name, .before = "value")
  nc_close(nc_file)
  return(nc_all)
}

# Convenience wrapper for creating area averaged SST time series
area_average <- function(df, site_name){
  df_res <- df |> dplyr::summarise(value = mean(temp, na.rm = T), .by = c("t")) |> 
    dplyr::rename(date = t) |> mutate(site = site_name)
}

# Convenience wrapper to bin and average clean data by depth
depth_bin_average <- function(df){
  df_res <- df |> 
    mutate(depth = case_when(depth <= 10 ~ 0,
                             depth <= 20 ~ 20,
                             depth <= 50 ~ 50,
                             depth <= 100 ~ 100,
                             depth <= 200 ~ 200,
                             depth <= 500 ~ 500,
                             depth <= 1000 ~ 1000,
                             depth <= 2000 ~ 2000,
                             TRUE ~ as.numeric(NA))) |>
    # NB: This will remove data with NA for depth
    filter(depth >= 0) |>
    summarise(value = mean(value, na.rm = TRUE), 
              .by = c("date_accessed", "URL", "citation", "type", "site", "lon", "lat", 
                      "date", "depth", "category", "driver", "variable"))
  return(df_res)
}

# Convenience function to save site products as individual files
# This expects the output from save_data()
save_data_one <- function(sub_levels, df){
  sub_split <- strsplit(sub_levels, "_")[[1]]
  if(sub_split[3] == "full"){
    sub_df <- filter(df, category == sub_split[1], site == sub_split[2])
    file_path <- paste0("data/full_data/",sub_split[3],"_", sub_split[1],"_", sub_split[2],".csv")
  } else if(sub_split[4] == "clean") {
    # TODO: have this split the data into the main data and a metadata lookup table and save each
    # Or maybe not considering how small these files tend to be
    sub_df <- filter(df, category == sub_split[1], driver == sub_split[2], site == sub_split[3])
    file_path <- paste0("data/full_data/",sub_split[4],"_", sub_split[1],"_",
                        sub_split[2],"_", sub_split[3],".csv")
  }
  if(nrow(sub_df) == 0) return()
  data.table::fwrite(sub_df, file_path, nThread = 15)
  rm(sub_split, sub_df); gc()
}

# Convenience function to save site products as individual files
save_data <- function(df, data_type = "full"){
  if(data_type == "full"){
    select_cols <- c("category", "site") 
  } else if(data_type == "clean") {
    select_cols <- c("category", "driver", "site")
  }
  unique_levels <- df |> 
    dplyr::select(dplyr::all_of(select_cols)) |> 
    dplyr::filter(site %in% long_site_names$site) |> 
    distinct() |>  
    mutate(type = data_type) |> 
    unite(col = "all", sep = "_")|> 
    as.list()
  purrr::walk(unique_levels[[1]], save_data_one, df = df)
  # rm(unique_levels); gc()
}

# Data summary plotting function
data_summary_plot <- function(full_product, site_name){
  
  # Tweaks for consistent plotting
  full_product <-  full_product %>% 
    mutate(category = as.factor(category),
           year = lubridate::year(date))
  
  # get correct bounding box
  bbox_plot <- bbox_from_name(site_name)
  
  # Pixel size
  if(range(full_product$lat, na.rm = T)[2]-range(full_product$lat, na.rm = T)[1] > 1){
    res_round <- 1
    res_text <- "0.1° (~10 km) resolution"
  } else{
    res_round <- 2
    res_text <- "0.01° (~1 km) resolution"
  }
  
  # Table of meta-stats
  meta_table <- data.frame(table(full_product$category)) %>% 
    pivot_wider(names_from = Var1, values_from = Freq) %>% 
    mutate(files = length(unique(full_product$citation)),
           lon = paste0(round(min(full_product$lon, na.rm = T), 2), " to ", round(max(full_product$lon, na.rm = T), 2)), 
           lat = paste0(round(min(full_product$lat, na.rm = T), 2), " to ", round(max(full_product$lat, na.rm = T), 2)),
           date = paste0(min(full_product$year, na.rm = T), " to ", max(full_product$year, na.rm = T)),
           depth = paste0(min(full_product$depth, na.rm = T), " to ", max(full_product$depth, na.rm = T))) %>% #,
           # site = site_name) %>%
    dplyr::select(files, lon, lat, date, depth, everything())
  
  # Graphic version
  # meta_table_g <- gtable_add_grob(tableGrob(meta_table, rows = NULL),
  #                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
  #                                 t = 2, b = nrow(meta_table), l = 1, r = ncol(meta_table))
  meta_table_g <- tableGrob(meta_table, rows = NULL)# + labs(title = "Meta-data")
  
  # Clip coastline polygons for faster plotting
  if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= min(full_product$lon, na.rm = T)-10,
           x <= max(full_product$lon, na.rm = T)+10,
           y >= min(full_product$lat, na.rm = T)-10,
           y <= max(full_product$lat, na.rm = T)+10)
  
  # Count per grid cell
  plot_spatial <- full_product %>% 
    mutate(lon = round(lon, res_round),
           lat = round(lat, res_round)) %>% 
    group_by(lon, lat) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    geom_tile(aes(fill = log10(count))) +
    scale_fill_viridis_c(option = "E") +
    coord_quickmap(expand = F,
                   xlim = c(bbox_plot[1:2]), 
                   ylim = c(bbox_plot[3:4])) +
    labs(x = NULL, y = NULL, fill = "Count\n(log10)",
         title = paste0("Count of data binned at ", res_text)) +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          legend.position = "bottom")
  # plot_spatial
  
  # Count of data over time
  plot_time <- full_product %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year, category) %>% 
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    ggplot() +
    # geom_col(aes(x = year, y = count, fill = category)) +
    geom_col(aes(x = year, y = log10(count), fill = category), width = 1, show.legend = F) +
    coord_cartesian(expand = F) +
    scale_fill_manual(values = CatColAbr) +
    labs(x = NULL, fill = "Variable", y = "Count\n(log10)",
         title = "Count of data per year") +
    theme(panel.border = element_rect(fill = NA, colour = "black"), legend.position = "bottom")
  # plot_time
  
  # Count of data at depth by var type
  plot_depth <- full_product %>% 
    # filter(depth >= 0) %>%
    # filter(!is.na(depth)) %>% 
    mutate(depth = ifelse(is.na(depth), 0, depth)) %>%
    mutate(depth = round(depth, -1)) %>%
    group_by(depth, category) %>% 
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    ggplot() +
    geom_col(aes(x = depth, y = log10(count), fill = category), show.legend = F) +
    scale_x_reverse() +
    coord_flip(expand = F) +
    scale_fill_manual(values = CatColAbr) +
    # guides(fill = guide_legend(nrow = length(unique(full_product$category)))) +
    labs(x = NULL, fill = "Variable", y = "Count (log10)",
         title = "Count of data at\ndepth (10 m bins)") +
    theme(panel.border = element_rect(fill = NA, colour = "black"), legend.position = "bottom")
  # plot_depth
  
  # Plot legend for categories
  plot_blank <- full_product %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year, category) %>% 
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    ggplot() +
    geom_col(aes(x = year, y = count, fill = category), width = 1) +
    scale_fill_manual(values = CatColAbr) +
    guides(fill = guide_legend(nrow = length(unique(full_product$category)))) +
    labs(fill = "Variable Type")
  plot_legend <- ggpubr::get_legend(plot_blank)
  
  # Full summary plot
  plot_summary_left <- ggpubr::ggarrange(plot_spatial, plot_time, labels = c("B)", "C)"), nrow = 2, heights = c(1, 0.4))
  plot_summary_right <- ggpubr::ggarrange(plot_depth, plot_legend, labels = c("D)", ""), nrow = 2, heights = c(1, 0.4))
  plot_summary_bottom <- ggpubr::ggarrange(plot_summary_left, plot_summary_right, nrow = 1, labels = c("", "D)"), widths = c(1, 0.3))
  # plot_summary <- ggpubr::ggarrange(meta_table_g, plot_summary_bottom, ncol = 1, heights = c(0.1, 1))
  plot_summary <- ggpubr::ggarrange(meta_table_g, plot_summary_bottom, heights = c(0.2, 1), labels = c("A)", ""), ncol = 1) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_summary)
}

# Data climatology plotting function
data_clim_plot <- function(full_product, site_name){
  
  # Create factors for more consistent plotting
  full_product$category <- as.factor(full_product$category)
  
  # get correct bounding box
  bbox_plot <- bbox_from_name(site_name)
  
  # Pixel size
  if(range(full_product$lat, na.rm = T)[2]-range(full_product$lat, na.rm = T)[1] > 1){
    res_round <- 1
    res_text <- "0.1° (~10 km) resolution"
  } else{
    res_round <- 2
    res_text <- "0.01° (~1 km) resolution"
  }
  
  # Calculate monthly depth climatologies per pixel
  depth_monthly_clims_pixel <- full_product %>% 
    filter(depth >= 0) %>% 
    filter(value < 9999) %>% 
    mutate(lon = round(lon, res_round),
           lat = round(lat, res_round),
           month = lubridate::month(date),
           depth = round(depth, -1),
           depth_cat = case_when(depth > 0 & depth <= 10 ~ "0 - 10 m",
                                 depth > 10 & depth <= 50 ~ "10 - 50 m",
                                 depth > 50 & depth <= 200 ~ "50 - 200 m",
                                 depth > 200 & depth <= 1000 ~ "200 - 1000 m",
                                 depth > 1000 & depth <= 2000 ~ "1000 - 2000 m",
                                 depth > 2000 ~ "2000+ m",
                                 TRUE ~ as.character(NA)),
           depth_cat = factor(depth_cat, levels = c("0 - 10 m", "10 - 50 m", "50 - 200 m", 
                                                    "200 - 1000 m", "1000 - 2000 m", "2000+ m")),
           var_cat = case_when(grepl("°C", variable, ignore.case = F) ~ "temp",
                               grepl("sal", variable, ignore.case = F) ~ "sal",
                               grepl("cndc", variable, ignore.case = F) ~ "sal")) %>% 
    filter(!is.na(depth_cat), !is.na(var_cat), !is.na(month)) %>% 
    group_by(lon, lat, month, depth_cat, var_cat) %>% 
    summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
    right_join(expand.grid(month = 1:12, depth_cat = c("0 - 10 m", "10 - 50 m", "50 - 200 m", "200 - 1000 m", 
                                                       "1000 - 2000 m", "2000+ m"), var_cat = c("temp", "sal")),
               by = c("month", "depth_cat", "var_cat"))
  
  # Calculate monthly depth climatologies
  depth_monthly_clims <- depth_monthly_clims_pixel %>%
    group_by(month, depth_cat, var_cat) %>%
    summarise(value = mean(value, na.rm = T), .groups = "drop")
  
  # Get limits for consistent legends
  lims_temp <- filter(depth_monthly_clims_pixel, var_cat == "temp")
  lims_sal <- filter(depth_monthly_clims_pixel, var_cat == "sal")
  
  # Clip coastline polygons for faster plotting
  if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= min(full_product$lon, na.rm = T)-10,
           x <= max(full_product$lon, na.rm = T)+10,
           y >= min(full_product$lat, na.rm = T)-10,
           y <= max(full_product$lat, na.rm = T)+10)
  
  # Plot overall monthly depth temperature clims
  plot_depth_temp_clims <- depth_monthly_clims %>% 
    filter(var_cat == "temp") %>%
    ggplot() +
    geom_tile(aes(x = as.factor(month), y = depth_cat, fill = value), show.legend = F) +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_c(limits = range(lims_temp$value, na.rm = T)) +
    labs(x = "Month", y = "Depth", fill = "Temp. (°C)", title = "Temperature climatologies at depth") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
  # plot_depth_temp_clims
  
  # Plot temperature clims per pixel
  plot_spatial_temp_clim <- depth_monthly_clims_pixel %>% 
    filter(var_cat == "temp", depth_cat == "0 - 10 m") %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(limits = range(lims_temp$value, na.rm = T)) +
    coord_quickmap(xlim = c(bbox_plot[1:2]), 
                   ylim = c(bbox_plot[3:4])) +
    facet_wrap(~month) +
    labs(x = NULL, y = NULL, fill = "Temp. (°C)",
         title = paste0("Surface (0 to 10 m) temperature clims at ", res_text)) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          legend.direction = "horizontal")
  # plot_spatial_temp_clim
  
  # Plot overall monthly depth salinity clims
  plot_depth_sal_clims <- depth_monthly_clims %>% 
    filter(var_cat == "sal") %>%
    ggplot() +
    geom_tile(aes(x = as.factor(month), y = depth_cat, fill = value), show.legend = F) +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_c(option = "A", limits = range(lims_sal$value, na.rm = T)) +
    labs(x = "Month", y = "Depth", fill = "Salinity", title = "Salinity climatologies at depth") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
  # plot_depth_sal_clims
  
  # Plot salinity clims per pixel
  plot_spatial_sal_clim <- depth_monthly_clims_pixel %>% 
    filter(var_cat == "sal", depth_cat == "0 - 10 m") %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option = "A") +
    coord_quickmap(xlim = c(bbox_plot[1:2]), 
                   ylim = c(bbox_plot[3:4])) +
    facet_wrap(~month) +
    labs(x = NULL, y = NULL, fill = "Salinity",
         title = paste0("Surface (0 to 10 m) salinity clims at ", res_text)) +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          legend.direction = "horizontal")
  # plot_spatial_sal_clim
  
  # Put them together
  plot_clim <- ggpubr::ggarrange(plot_depth_temp_clims, ggpubr::get_legend(plot_spatial_temp_clim), plot_spatial_temp_clim + theme(legend.position = "none"), 
                                 plot_depth_sal_clims, ggpubr::get_legend(plot_spatial_sal_clim), plot_spatial_sal_clim + theme(legend.position = "none"),
                                 heights = c(0.3, 0.1, 1, 0.3, 0.1, 1), labels = c("A)", "", "B)", "C)", "", "D)"), ncol = 1, nrow = 6) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_clim)
}

# Function for calculating and plotting the trends in the data from full products
data_trend_plot <- function(full_product, site_name){
  
  # Create factors for more consistent plotting
  full_product$category <- as.factor(full_product$category)
  
  # get correct bounding box
  bbox_plot <- bbox_from_name(site_name)
  
  # Pixel size
  if(range(full_product$lat, na.rm = T)[2]-range(full_product$lat, na.rm = T)[1] > 1){
    res_round <- 1
    res_text <- "0.1° (~10 km) resolution"
  } else{
    res_round <- 2
    res_text <- "0.01° (~1 km) resolution"
  }
  
  # Calculate trends 
  depth_trend_pixel <- full_product %>% 
    filter(depth >= 0) %>% 
    filter(value < 9999) %>% 
    mutate(lon = round(lon, res_round),
           lat = round(lat, res_round),
           year = lubridate::year(date),
           depth = round(depth, -1),
           depth_cat = case_when(depth > 0 & depth <= 10 ~ "0 - 10 m",
                                 depth > 10 & depth <= 50 ~ "10 - 50 m",
                                 depth > 50 & depth <= 200 ~ "50 - 200 m",
                                 depth > 200 & depth <= 1000 ~ "200 - 1000 m",
                                 depth > 1000 & depth <= 2000 ~ "1000 - 2000 m",
                                 depth > 2000 ~ "2000+ m",
                                 TRUE ~ as.character(NA)),
           depth_cat = factor(depth_cat, levels = c("0 - 10 m", "10 - 50 m", "50 - 200 m", 
                                                    "200 - 1000 m", "1000 - 2000 m", "2000+ m")),
           var_cat = case_when(grepl("°C", variable, ignore.case = F) ~ "temp",
                               grepl("sal", variable, ignore.case = F) ~ "sal",
                               grepl("cndc", variable, ignore.case = F) ~ "sal")) %>% 
    filter(!is.na(depth_cat), !is.na(var_cat), !is.na(year)) %>% 
    group_by(lon, lat, year, depth_cat, var_cat) %>% 
    summarise(value = mean(value, na.rm = T), .groups = "drop") %>%
    # mutate(plot_group = as.numeric(as.factor(paste0(lon, lat)))) %>% 
    right_join(expand.grid(year = seq(min(lubridate::year(full_product$date), na.rm = T), 
                                      max(lubridate::year(full_product$date), na.rm = T)),
                           depth_cat = c("0 - 10 m", "10 - 50 m", "50 - 200 m", "200 - 1000 m", 
                                         "1000 - 2000 m", "2000+ m"), var_cat = c("temp", "sal")),
               by = c("year", "depth_cat", "var_cat")) %>%
    filter(!is.na(lon)) %>% 
    mutate(lonlat = paste0(lon, lat))
    # group_by(lon, lat, year, depth_cat, var_cat) %>%
    # do(broom::tidy(lm(value ~ year, .)))
  
  # Calculate depth trends
  depth_trend <- depth_trend_pixel %>%
    group_by(year, depth_cat, var_cat) %>%
    summarise(value = mean(value, na.rm = T), .groups = "drop")
  
  # Get limits for consistent plotting
  lims_temp <- filter(depth_trend_pixel, var_cat == "temp")
  lims_sal <- filter(depth_trend_pixel, var_cat == "sal")
  
  # Plot temperature trends
  # TODO: Remove the raw data points once you figure out what is funny with them
  # TODO: Make all symbols and lines smaller
  plot_trend_temp <- depth_trend_pixel %>% 
    filter(var_cat == "temp") %>%
    ggplot(aes(x = year, y = value)) +
    # geom_point(aes(colour = depth_cat), alpha = 0.5) +
    # geom_line(aes(group = lonlat), stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
    geom_point(data = filter(depth_trend, var_cat == "temp"), aes(group = depth_cat), colour = "black", size = 3, shape = 18, alpha = 0.7) +
    geom_point(data = filter(depth_trend, var_cat == "temp"), aes(colour = depth_cat), size = 2, shape = 18, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "temp"), aes(group = depth_cat), colour = "black", method = "lm", se = F, size = 2, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "temp"), aes(colour = depth_cat), method = "lm", se = F, size = 1, alpha = 0.7) +
    coord_cartesian(expand = F, ylim = range(lims_temp$value)) +
    scale_colour_manual(values = DepthCol) +
    labs(x = NULL, y = "Temp. (°C)", colour = "Depth", title = "Temperature trends at depth") +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          legend.direction = "horizontal", legend.position = "bottom")
  # plot_trend_temp
  
  # Plot salinity trends
  plot_trend_sal <- depth_trend_pixel %>% 
    filter(var_cat == "sal") %>%
    ggplot(aes(x = year, y = value)) +
    # geom_point(aes(colour = depth_cat), alpha = 0.5) +
    # geom_line(aes(group = lonlat), stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
    geom_point(data = filter(depth_trend, var_cat == "sal"), aes(group = depth_cat), colour = "black", size = 3, shape = 18, alpha = 0.7) +
    geom_point(data = filter(depth_trend, var_cat == "sal"), aes(colour = depth_cat), size = 2, shape = 18, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "sal"), aes(group = depth_cat), colour = "black", method = "lm", se = F, size = 2, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "sal"), aes(colour = depth_cat), method = "lm", se = F, size = 1, alpha = 0.7) +
    coord_cartesian(expand = F, ylim = range(lims_sal$value)) +
    scale_colour_manual(values = DepthCol) +
    labs(x = NULL, y = "Salinity", colour = "Depth", title = "Salinity trends at depth") +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
  # plot_trend_sal
  
  # Put them together
  plot_trend <- ggpubr::ggarrange(plot_trend_temp + theme(legend.position = "none"), ggpubr::get_legend(plot_trend_temp),  plot_trend_sal + theme(legend.position = "none"), 
                                  ncol = 1, labels = c("A)", "", "B)"), heights = c(1, 0.2, 1)) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_trend)
}

# Function for plotting a quick summary of a model product
model_summary <- function(model_product, site_name){
  
  # Clip coastline polygons for faster plotting
  if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= min(model_product$lon, na.rm = T)-10,
           x <= max(model_product$lon, na.rm = T)+10,
           y >= min(model_product$lat, na.rm = T)-10,
           y <= max(model_product$lat, na.rm = T)+10)
  
  # Spatial temperature
  ## TODO: Only show one map, no facets
  plot_map <- model_product %>% 
    filter(land == 1) %>% 
    group_by(proj, lon, lat, depth) %>% 
    summarise_all(mean) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, fill = "grey70", colour = "black",
                 aes(x = x, y = y, group = polygon_id)) +  
    geom_point(aes(colour = Temp), size = 3) +
    scale_colour_viridis_c() +
    coord_quickmap(xlim = range(model_product$lon),
                   ylim = range(model_product$lat)) +
    facet_grid(proj ~ depth) +
    labs(x = NULL, y = NULL, colour = "Temp. (°C)",
         title = "Average temperature per pixel")
  
  # Spatial temperature time series
  plot_trend <- model_product %>% 
    filter(land == 1) %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(depth, proj, year) %>% 
    summarise_all(mean) %>% 
    ggplot(aes(x = year, y = Temp)) +
    geom_point(aes(colour = proj), alpha = 0.25, show.legend = F) +
    geom_smooth(method = "lm", se = F, colour = "black", size = 2, show.legend = F) +
    geom_smooth(method = "lm", se = F, aes(colour = proj), show.legend = F) +
    scale_x_continuous(expand = c(0, 0)) +
    facet_grid(proj~depth) +
    labs(x = NULL, y  = "Temperature (°C)", 
         title = "Average temperature across all pixels")
  
  # Combine and exit
  plot_all <- ggpubr::ggarrange(plot_map, plot_trend, ncol = 1, labels = c("A)", "B)"), heights = c(2, 1)) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_all)
}

# Function for creating spatial average of model cells within a bbox
# Produces a range of useful stats
model_bbox_stats <- function(model_product, site_abv){
  
  # Get correct bounding box
  bbox <- bbox_from_name(site_abv)
  
  # Load clean data
  if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")
  clean_all_sub <- clean_all_clean %>% 
    filter(site == site_abv,
           variable %in% c("NO3 [µmol/l]", "pCO2 [µatm]","PO4 [µmol/l]",
                           "SiO4 [µmol/l]", "temp [°C]", "sal")) %>% 
    mutate(depth = case_when(depth <= 10 ~ "0 to 10",
                             depth <= 50 ~ "10 to 50",
                             depth <= 200 ~ "50 to 200"),
           date = lubridate::round_date(date, "month")) %>% 
    filter(!is.na(depth)) %>%  # Remove data deeper than 200 metres
    group_by(site, type, depth, date, variable) %>% 
    summarise(value_dat = mean(value, na.rm = T), .groups = "drop")
  
  # Mean stats by bbox
  model_clean <- model_product %>% 
    mutate(site = site_abv,
           date = lubridate::round_date(date-20, "month"), # Round to start of month to match other data
           depth = case_when(depth <= 10 ~ "0 to 10",
                             depth <= 50 ~ "10 to 50",
                             depth <= 200 ~ "50 to 200")) %>% # Convert to depth categories matching site averages
    filter(land == 1, 
           lon >= bbox[1], lon <= bbox[2],
           lat >= bbox[3], lat <= bbox[4]) %>% # Filter within bbox
    dplyr::select(-Udir, -USpeed, -Oxy) %>% # No longer checking these variables
    pivot_longer(cols = Salt:pco2w, names_to = "variable") %>% 
    group_by(site, proj, date, depth, variable) %>% 
    summarise(value_mod = mean(value, na.rm = T), .groups = "drop") %>% 
    mutate(variable = case_when(variable == "Nit" ~ "NO3 [µmol/l]",
                                variable == "pco2w" ~ "pCO2 [µatm]",
                                variable == "Pho" ~ "PO4 [µmol/l]",
                                variable == "Salt" ~ "sal",
                                variable == "Sil" ~ "SiO4 [µmol/l]",
                                variable == "Temp" ~ "temp [°C]",
                                TRUE ~ variable)) %>% 
    left_join(clean_all_sub, by = c("site", "date", "depth", "variable"))
  
  # Test visuals
  # model_clean %>% 
  #   filter(depth == "10 to 50", proj == "RCP 8.5", variable == "temp [°C]") %>% 
  #   ggplot(aes(x = date, y = value_mod)) +
  #   geom_point() + geom_line() +
  #   geom_point(aes(y = value_dat), colour = "red") + 
  #   geom_line(aes(y = value_dat), colour = "red")
  
  # RMSE
  model_RMSE <- model_clean %>% 
    filter(!is.na(value_dat)) %>% 
    group_by(site, type, proj, depth, variable) %>% 
    summarise(n = n(),
              mean_dat = mean(value_dat, na.rm = T),
              mean_mod = mean(value_mod, na.rm = T),
              rmse = sqrt(mean((value_dat-value_mod)^2)), .groups = "drop")
  
  # Linear models
  model_lm <- model_clean %>% 
    group_by(site, proj, depth, variable) %>% 
    lm_tidy(df = ., x_var = "date", y_var = "value_mod") %>% ungroup() %>% 
    mutate(slope = round(slope*3652.5, 4)) %>%  # From daily to decadal trend)
    dplyr::select(-nobs) # Always 1,200
  
  # Combine stats
  model_stats <- left_join(model_RMSE, model_lm,
                           by = c("site", "proj", "depth", "variable"))
  
  # Return and exit
  rm(bbox, model_clean, model_RMSE, model_lm); gc()
  return(model_stats)
}

# Convenience wrapper for creating hi-res gridded coordinates
grid_MUR <- function(bbox_coords){
  bbox_grid <- expand.grid(seq(bbox_coords[1], bbox_coords[2], by = 0.01),
                     seq(bbox_coords[3], bbox_coords[4], by = 0.01)) %>% 
    dplyr::rename(lon = Var1, lat = Var2) %>% 
    # Raster MUR coords get extracted from largest to smallest Y
    arrange(-lat, lon)
  return(bbox_grid)
}

# Convenience function to account for minor x axis change to MUR pixel extent
extent_MUR <- function(bbox_coords){
  bbox_extent <- extent(c(bbox_coords[1]-0.001, bbox_coords[2],
                          bbox_coords[3]-0.001, bbox_coords[4]))
  return(bbox_extent)
}

# Convenience wrapper used within download_MUR_ALL()
download_MUR_single <- function(site_abv, file_date, MUR_raster){
  if(!file.exists(paste0("~/pCloudDrive/FACE-IT_data/MUR/",site_abv,"/",file_date,".rds"))){
    write_rds(data.frame(grid_MUR(bbox_wide_from_name(site_abv)), t = file_date,
                         temp = as.vector(raster::extract(MUR_raster, extent_MUR(bbox_wide_from_name(site_abv)), 
                                                          method = "simple")-273.15)), 
              paste0("~/pCloudDrive/FACE-IT_data/MUR/",site_abv,"/",file_date,".rds"), compress = "gz")
  }
}

# Function for downloading MUR 1km data
download_MUR_ALL <- function(file_date){
  
  # Check if the data have already been downloaded - this should be deactivated for testing
  if(file.exists(paste0("~/pCloudDrive/FACE-IT_data/MUR/por/",file_date,".rds"))) return()
  
  # Base URL for MUR data
  base_MUR_URL <- "https://podaac-opendap.jpl.nasa.gov/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1"
  
  # Construct file name
  file_name <- paste0(base_MUR_URL,"/",year(file_date),"/",sprintf("%03d", yday(file_date)),"/",
                      gsub("-", "", file_date),"090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")
  
  # Connect to NetCDF file as a raster brick
  # system.time(
  suppressWarnings( # Don't need the warning that says SST layer is being used
    MUR_raster <- brick(file_name)
  )
  # ) # 2 seconds
  
  # Download data per site
  # system.time(
  plyr::l_ply(c("kong", "is", "stor", "young", "disko", "nuup", "por"), 
              download_MUR_single, file_date = file_date, MUR_raster = MUR_raster, .parallel = F)
  # ) # 16 seconds for 7
  
  # Test visuals
  # MUR_df <- read_rds("~/pCloudDrive/FACE-IT_data/MUR/young/2003-01-01.rds")
  # ggplot(MUR_df, aes(x = lon, y = lat)) +
  #   geom_raster(aes(fill = temp))
  
  # exit without returning anything
  # return()
}

# Shadow data that aren't meant to be distributed
shadow <- function(df, URL_key = "g-e-m|GRDC|Received directly from Mikael Sejr"){
  df_shadow <- dplyr::filter(df, grepl(URL_key, URL)) |> 
    mutate(lon = as.numeric(NA), lat = as.numeric(NA), 
           date = as.Date(NA), depth = as.numeric(NA), value = as.numeric(NA)) |> 
    distinct() |> 
    # left_join(full_var_list, by = c("category", "variable")) |> 
    mutate(variable = case_when(driver %in% c("biomass", "spp rich") ~ as.character(NA), TRUE ~ variable)) |> 
    distinct()
}

# Convenience function for filtering variables for analyses
review_filter_var <- function(full_product, var_keep, var_remove = NULL, var_precise = NULL, cit_filter = NULL, atmos = F){
  
  # NB: Repetitive, but much faster depth filtering
  # Disabled for now for clean data pipeline to dataAccess app
  # if(atmos){
  #   df_depth <- full_product %>% filter(depth <= 0) %>%
  #     bind_rows(add_depth(filter(full_product, is.na(lon)))) %>% filter(is.na(depth) | depth <= 0)
  # } else {
  #   df_depth <- full_product %>% filter(depth >= 0 & depth <= 10) %>%
  #     bind_rows(add_depth(filter(full_product, is.na(lon)))) %>%
  #     filter(depth >= 0)# & depth <= 10) # Disable to find missing depth data
  # }
  # res_df <- df_depth %>% #filter(!is.na(date)) %>%
  res_df <- full_product %>%
    filter(grepl(var_keep, variable, ignore.case = T)) %>% 
    mutate(type = "in situ")
  if(!is.null(var_remove)) res_df <- res_df %>% filter(!grepl(var_remove, variable, ignore.case = T))
  if(!is.null(var_precise)) res_df <- res_df %>% filter(!variable %in% var_precise)
  if(!is.null(cit_filter)) res_df <- res_df %>% filter(!grepl(cit_filter, citation, ignore.case = T))
  # res_df <- add_depth(res_df)
  print(unique(res_df$variable))
  return(res_df)
}

# Functions for checking filter_vars output
review_filter_check <- function(filter_object, check_var = NULL, check_cit = NULL){
  if(!is.null(check_var[1])) print(unique(filter_object$citation[filter_object$variable == check_var]))
  if(!is.null(check_cit[1])){
    citation_check <- filter_object[grepl(check_cit, filter_object$citation, ignore.case = T),]
    return(citation_check)
  }
}

# Summary analyses of filtered variables
review_summary <- function(filter_object, trend_dates = c("1982-01-01", "2020-12-31")){
  
  if(filter_object$driver[1] == "biomass"){
    df_base <- filter_object %>% 
      mutate(variable = case_when(str_detect(variable, "ind\\/m3") ~ "spp count [ind/m3]", 
                                  str_detect(variable, "cells\\/l") ~ "spp count [cells/l]",
                                  TRUE ~ variable))
  } else if(filter_object$driver[1] == "spp rich"){
    df_base <- filter_object %>% 
      filter(variable == "spp count [n]")
  } else {
    df_base <- filter_object
  }
  
  # Monthly averages
  df_monthly <- df_base %>%
    filter(!is.na(date),
           !is.na(value)) %>% 
    group_by(variable) %>% 
    mutate(date_round = lubridate::floor_date(date, "month"),
           median_value = median(value, na.rm = T),
           # Filter low salinity values. 24 based on the base data before filtering.
           value = case_when(median_value > 30 & value < 24 ~ as.numeric(NA), TRUE ~ value)) %>% 
    group_by(site, type, driver, date_round) %>%
    mutate(count_days_group = length(unique(date))) %>% 
    group_by(site, type, category, driver, variable, date_round, count_days_group) %>%
    summarise(value_mean = round(mean(value, na.rm = T), 2),
              value_median = round(median(value, na.rm = T), 2),
              value_min = round(min(value, na.rm = T), 2),
              value_max = round(max(value, na.rm = T), 2),
              count = n(), 
              count_days_name = length(unique(date)), .groups = "drop") %>%
    dplyr::rename(date = date_round) %>% 
    group_by(site, type, category, driver, variable) %>% 
    complete(date = seq(min(date), max(date), by = "month")) %>% 
    ungroup()
  
  # Summary data
  df_monthly_summary <- df_monthly %>% 
    group_by(site, type, variable) %>%
    summarise(date_min = min(date),
              date_max = max(date),
              value_mean_min = min(value_mean, na.rm = T),
              value_mean_mean = mean(value_mean, na.rm = T),
              value_mean_max = max(value_mean, na.rm = T), .groups = "drop")
  
  # Trends
  df_monthly_trend <- df_monthly %>% 
    filter(between(date, as.Date(trend_dates[1]), as.Date(trend_dates[2]))) %>%
    group_by(site, type, variable) %>%
    mutate(row_idx = 1:n()) %>%
    filter(!is.na(value_mean)) %>% 
    do(fit_site = broom::tidy(lm(value_mean ~ row_idx, data = .))) %>% 
    unnest(fit_site) %>% 
    filter(term == "row_idx") %>% 
    mutate(dec_trend = round(estimate*120, 4), 
           p.value = round(p.value, 4)) %>% 
    dplyr::select(site, type, variable, dec_trend, p.value) %>% 
    left_join(df_monthly_summary, by = c("site", "type", "variable"))
  
  # Monthly climatologies
  df_monthly_clim <- df_monthly %>% 
    filter(!is.na(value_mean)) %>% 
    mutate(month = lubridate::month(date)) %>% 
    group_by(site, type, variable, month) %>% 
    summarise(value_clim = mean(value_mean, na.rm = T),
              count = n(), .groups = "drop")
 
  # Citations
  df_source <- df_base %>% 
    dplyr::select(type, URL, citation) %>% distinct() %>% 
    mutate(source = case_when(grepl("PANGAEA", URL) ~ "PANGAEA",
                              grepl("data.npolar.no", URL) ~ "NPDC",
                              grepl("/nmdc/", URL) ~ "NMDC",
                              grepl("nmdc.no", URL) ~ "NMDC",
                              grepl("dataverse.no", URL) ~ "NMDC",
                              grepl("glodap.info", URL) ~ "GLODAP",
                              grepl("socat.info", URL) ~ "SOCAT",
                              grepl("zenodo.org", URL) ~ "Zenodo",
                              grepl("g-e-m.dk", URL) ~ "GEM",
                              grepl("mosj.no", URL) ~ "MOSJ",
                              grepl("archive.sigma2.no", URL) ~ "NIRD",
                              grepl("port.kingsbay.no", URL) ~ "Kings Bay",
                              grepl("portlongyear.no", URL) ~ "Port of Longyearbyen",
                              URL == "https://doi.org/10.7265/N5GT5K3K" ~ "NSIDC",
                              grepl("thredds.met.no", URL) ~ "NMI",
                              grepl("noaa.gov", URL) ~ "NOAA",
                              grepl("dap.ceda.ac.uk", URL) ~ "CCI",
                              grepl("Received directly from", URL) ~ "Author",
                              grepl("File provided by", URL) ~ "Author",
                              TRUE ~ "Other")) %>% 
    table(.) %>% data.frame() %>% filter(Freq > 0) %>% pivot_wider(names_from = source, values_from = Freq)
  df_citations <- df_base %>% dplyr::select(type, category, site, URL, citation) %>% distinct() %>% 
    table(.) %>% data.frame() %>% filter(Freq > 0) %>% pivot_wider(names_from = site, values_from = Freq) %>% 
    left_join(df_source, by = c("type", "URL", "citation"))
  
  # Combine and exit
  res_list <- list(monthly = df_monthly,
                   trend = df_monthly_trend,
                   clim = df_monthly_clim,
                   citations = df_citations)
  return(res_list)
  # rm(filter_object, trend_dates, df_monthly, df_monthly_trend, df_monthly_clim, df_source, df_citations, res_list); gc() # testing
}

# Convenience plotting function
review_summary_plot <- function(summary_list, short_var, date_filter = c(as.Date("1982-01-01"), as.Date("2020-12-31"))){
  
  # Create x/y coords for labels
  label_df_full <- summary_list$trend %>% 
    arrange(variable, site, type) %>%
    group_by(variable) %>%
    mutate(x = base::rep(seq(from = min(date_min), to = max(date_max), 
                             length.out = length(unique(site))+1)[2:(length(unique(site))+1)], 
                         each = length(unique(type))),
           y = base::rep(seq(from = max(value_mean_max, na.rm = T),#max(value_mean, na.rm = T)-((max(value_mean, na.rm = T)-min(value_mean, na.rm = T))/4), 
                             by = -((max(value_mean_max, na.rm = T)-min(value_mean_min, na.rm = T))/4),
                             length.out = length(unique(type))),
                         each = length(unique(site)))) %>% 
    ungroup()
  label_df_sub <- summary_list$trend %>% 
    arrange(variable, site, type) %>%
    group_by(variable) %>%
    mutate(x = base::rep(seq(from = date_filter[1], to = date_filter[2],
                             length.out = length(unique(site))+1)[2:(length(unique(site))+1)], 
                         each = length(unique(type))),
           y = base::rep(seq(from = max(value_mean_max, na.rm = T),#max(value_mean, na.rm = T)-((max(value_mean, na.rm = T)-min(value_mean, na.rm = T))/4), 
                             by = -((max(value_mean_max, na.rm = T)-min(value_mean_min, na.rm = T))/4),
                             length.out = length(unique(type))),
                         each = length(unique(site)))) %>% 
    ungroup()
  label_key_full <- label_df_full %>% 
    group_by(variable) %>% 
    summarise(x = min(date_min), y = unique(y),
              label = unique(type), site = "label", type = "in situ", .groups = "drop")
  label_key_sub <- label_df_sub %>% 
    group_by(variable) %>% 
    summarise(x = date_filter[1], y = unique(y),
              label = unique(type), site = "label", type = "in situ", .groups = "drop")
  
  # Plot monthly metadata
  summary_list$monthly %>% 
    filter(!is.na(value_mean)) %>% 
    mutate(month = lubridate::month(date)) %>% 
    ggplot(aes(x = as.factor(month), y = count_days_name)) +
    geom_boxplot(aes(fill = site), position = "dodge", outlier.colour = NA) +
    geom_jitter(aes(colour = log10(count))) +
    scale_colour_viridis_c() + scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
    labs(y = "Unique days with data points", x = "Month", fill = "Site", colour = "Count of\nindividual\ndata points\n[log10(n)]") +
    facet_grid(site+type~variable) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  ggsave(paste0("~/Desktop/analyses_output/",short_var,"_meta_box.png"), width = 20, height = 9)
  summary_list$monthly %>% 
    filter(!is.na(value_mean)) %>% 
    ggplot(aes(x = date, y = log10(count), colour = site)) +
    geom_point(aes(fill = count_days_name), shape = 21) + #geom_line(alpha = 0.1) + 
    stat_smooth(geom = "line", method = "lm", size = 3, linetype = "dashed", alpha = 0.3) +
    geom_smooth(data = filter(summary_list$monthly, date >= date_filter[1], date <= date_filter[2]), method = "lm", se = F) +
    # geom_jitter(aes(colour = log10(count))) +
    scale_fill_viridis_c() + #scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
    labs(y = "Count [log10(n)] of daily data points", x = NULL, 
         fill = "Unique days\nof sampling", colour = "Site") +
    facet_grid(type~variable) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  ggsave(paste0("~/Desktop/analyses_output/",short_var,"_meta_ts.png"), width = 20, height = 9)
  
  # Plot monthly values
  ggplot(summary_list$monthly, aes(x = date, y = value_mean, colour = site, linetype = type)) +
    geom_point(alpha = 0.1) + geom_line(alpha = 0.1) + 
    stat_smooth(geom = "line", method = "lm", size = 3, linetype = "dashed", alpha = 0.3) +
    geom_smooth(data = filter(summary_list$monthly, date >= date_filter[1], date <= date_filter[2]), method = "lm", se = F) +
    geom_label(data = label_df_full, show.legend = F,
               aes(x = x, y = y, colour = site,
                   label = paste0(dec_trend,"/dec\n p = ", p.value))) +
    geom_label(data = label_key_full, aes(x = x, y = y, label = label), colour = "black") +
    labs(y = NULL, x = NULL, colour = "Site", linetype = "Source") +
    facet_wrap(~variable, scales = "free") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  ggsave(paste0("~/Desktop/analyses_output/",short_var,"_ts.png"), width = 20, height = 9)
  
  # Plot monthly values with the given date range filter
  filter(summary_list$monthly, date >= date_filter[1], date <= date_filter[2]) %>%
    ggplot(aes(x = date, y = value_mean, colour = site, linetype = type)) +
    geom_point(alpha = 0.1) + geom_line(alpha = 0.1) + geom_smooth(method = "lm", se = T) +
    geom_label(data = label_df_sub, show.legend = F,
               aes(x = x, y = y, colour = site,
                   label = paste0(dec_trend,"/dec\n p = ", p.value))) +
    geom_label(data = label_key_sub, aes(x = x, y = y, label = label), colour = "black") +
    labs(y = NULL, x = NULL, colour = "Site", linetype = "Source") +
    facet_wrap(~variable, scales = "free") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  ggsave(paste0("~/Desktop/analyses_output/",short_var,"_ts_",year(date_filter[1]),"-",year(date_filter[2]),".png"), width = 20, height = 9)
  
  ## Plot monthly clims
  # ggplot(summary_list$clim, aes(x = as.factor(month), y = value_clim, fill = site, colour = type)) +
  #   geom_col(position = "dodge") + scale_colour_viridis_d() +#scale_colour_viridis_c()
  #   labs(y = NULL, x = "Month", fill = "Site", colour = "Source") +
  #   facet_grid(site~variable, scales = "free_y") +
  #   theme(panel.border = element_rect(colour = "black", fill = NA))#,
  #         # legend.position = c(0.63, 0.12), legend.direction = "horizontal")
  # ggsave(paste0("~/Desktop/",short_var,"_clim_site.png"), width = 16, height = 9)
  # ggplot(summary_list$clim, aes(x = as.factor(month), y = value_clim, fill = site)) +
  #   geom_col(position = "dodge") + scale_colour_viridis_d() +#scale_colour_viridis_c()
  #   labs(y = NULL, x = "Month", fill = "Site") +
  #   facet_grid(variable~type, scales = "free_y") +
  #   theme(panel.border = element_rect(colour = "black", fill = NA))
  # ggsave(paste0("~/Desktop/",short_var,"_clim_type.png"), width = 9, height = 16)
  summary_list$monthly %>% 
    filter(!is.na(value_mean)) %>% 
    mutate(month = lubridate::month(date)) %>% 
    ggplot(aes(x = as.factor(month), y = value_mean, fill = site)) +
    geom_boxplot() +
    labs(y = NULL, x = "Month", fill = "Site", colour = "Source") +
    facet_grid(site+type~variable, scales = "free_y") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  ggsave(paste0("~/Desktop/analyses_output/",short_var,"_clim_box.png"), width = 20, height = 9)
}

# Convenience function for interrogating dataframes
cat_driver_var <- function(df){
  print(unique(df$category))
  print(unique(df$driver))
  print(unique(df$variable))
  table(df$driver, df$variable)
}

# Convenience function for seeing which variables for which categories don't have a driver
cat_driver_miss <- function(df, cat_choice){
  filter(df, category == cat_choice, is.na(driver)) |> 
    dplyr::select(citation, site, category, variable) |> distinct()
  # print(unique(df_res$variable))
}

# Add depth data manually from PANGAEA files where this info is in the meta-data
add_depth <- function(df){
  df_res <- df %>% 
    mutate(depth = case_when(URL == "https://doi.org/10.1594/PANGAEA.857619" ~ 0,
                             URL == "https://doi.org/10.1594/PANGAEA.930028" ~ 310.6,
                             URL == "https://doi.org/10.1594/PANGAEA.873568" ~ -5,
                             URL == "https://doi.org/10.1594/PANGAEA.873568" ~ -5,
                             grepl("Schmithüsen, Holger; Raeke, Andreas", citation) & variable == "TTT [°C]" ~ -25,
                             grepl("Schmithüsen, Holger; Rohleder, Christian", citation) & variable == "TTT [°C]" ~ -25,
                             grepl("Matishov, Gennady G; Zuyev, Aleksey N; Golubev", citation) ~ 0,
                             grepl("Schmithüsen, Holger (*)", citation) & variable == "Temp [°C]" ~ 5,
                             grepl("Schmithüsen, Holger (*)", citation) & variable == "TTT [°C]" ~ -29,
                             grepl("König-Langlo, Gert (*)", citation) & variable == "Temp [°C]" ~ 0,
                             grepl("König-Langlo, Gert (*)", citation) & variable == "TTT [°C]" ~ -25,
                             # Basically anything by Golubev et al. is very old data with no depth values
                             # But I found one entry that has the depth/altitude listed as 0, so I'm going with that
                             grepl("PANGAEA", URL) & grepl("Golubev", citation) & is.na(depth) ~ 0,
                             # Other URLs with no depth data
                             # "https://doi.org/10.1594/PANGAEA.484880"
                             # "https://doi.org/10.1594/PANGAEA.484950"
                             # "https://doi.org/10.1594/PANGAEA.901447"
                             # "https://doi.org/10.1594/PANGAEA.680872" - DWD cruise
                             # "https://doi.org/10.1594/PANGAEA.680907" - DWD cruise
                             # "https://doi.org/10.1594/PANGAEA.899570"
                             # ""
                             TRUE ~ depth))
  return(df_res)
}

# Convenience plot of spatial density of data
quick_plot_density <- function(df, site_name){
  
  bbox_site <- bbox_from_name(site_name)
  if(site_name %in% c("kong", "young")){
    round_num = 0.01
  } else {
    round_num = 0.1
  }
  
  # Process data
  df_proc <- df |> 
    filter(site == site_name) |> 
    mutate(lon = plyr::round_any(lon, round_num),
           lat = plyr::round_any(lat, round_num)) |> 
    summarise(grid_count = n(), .by = c("site", "lon", "lat")) |> 
    left_join(long_site_names, by = "site") |> 
    filter(!is.na(lon))
  
  # Crop map
  if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= bbox_site[1]-10, x <= bbox_site[2]+10,
           y >= bbox_site[3]-10, y <= bbox_site[4]+10)
  
  # Create plot, save, and return the plot for viewing
  dens_plot <-  df_proc |> 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, fill = "grey70", colour = "black",
                 aes(x = x, y = y, group = polygon_id)) +  
    geom_raster(aes(fill = grid_count)) +
    scale_fill_viridis() +
    coord_quickmap(xlim = c(bbox_site[1:2]), 
                   ylim = c(bbox_site[3:4])) +
    labs(x = NULL, y = NULL, fill = "Total data\npoints",
         title = paste0(df_proc$site_long[1]," data density per ",round_num,"° grid")) +
    theme(plot.title = element_text(size = 16),
          legend.background = element_rect(colour = "black", fill  = "white"),
          panel.border = element_rect(colour = "black", fill = NA))
  ggsave(paste0("metadata/fjord_data_density_",site_name,".png"), dens_plot, width = 10, height = 6)
  dens_plot
}

# Convenience plot of available data for a given driver
quick_plot_avail <- function(filter_name, filter_choice = "driver", legend_tweak = c(0.2, 0.8)){
  
  # Load data
  if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")
  
  # Filter data accordingly
  if(filter_choice == "driver"){
    avail_data <- clean_all_clean |> 
      filter(driver == filter_name)
  } else if(filter_choice == "variable") {
    avail_data <- clean_all_clean |> 
      filter(grepl(filter_name, variable))
  } else {
    stop("oops")
  }
  
  # Create plot, save, and return the plot for viewing
  avail_plot <- avail_data |> 
    mutate(year = year(date)) |> 
    summarise(daily_count = n(), .by = c("site", "year")) |> 
    left_join(long_site_names, by = "site") |> 
    ggplot(aes(y = site_long, x = year)) +
    geom_point(aes(size = daily_count, colour = site_long)) +
    scale_colour_manual(values = site_colours, guide = "none") +
    labs(x = NULL, y = NULL, size = "Daily data/year",
         title = paste0("Available ",filter_name," data")) +
    theme(plot.title = element_text(size = 16),
          legend.position = legend_tweak,
          legend.background = element_rect(colour = "black", fill  = "white"),
          panel.border = element_rect(colour = "black", fill = NA))
  ggsave(paste0("metadata/avail_",filter_name,".png"), avail_plot, width = 10, height = 6)
  avail_plot
}

# Snappy little convenience function
quick_plot_ice <- function(df, pixel_size = 5){
  if(length(unique(df$sea_ice_extent)) == 4) factor_labels <- c("ocean", "land", "sea ice", "coast")
  if(length(unique(df$sea_ice_extent)) == 5) factor_labels <- c("ocean", "land", "sea ice", "coast", "exclude")
  df %>% 
    # mutate(sea_ice_extent = factor(sea_ice_extent, labels = c("ocean", "land", "sea ice", "coast"))) %>%
    mutate(sea_ice_extent = factor(sea_ice_extent, labels = factor_labels)) %>%
    filter(date == "2017-08-01") %>% 
    ggplot(aes(x = lon, y = lat)) +
    # geom_tile(aes(fill = sea_ice_extent)) +
    geom_point(aes(colour = sea_ice_extent), size = pixel_size, shape = 15) +
    scale_colour_manual(values = ice_cover_colours, aesthetics = c("colour", "fill")) +
    labs(x = NULL, y = NULL, colour = "Pixel key") +
    theme(panel.background = element_blank())
}

# Function for converting uneven ice trend data to an even grid before plotting
# plot_title <- "Kongsfjorden"; pixel_res <- 0.03; check_conv = FALSE;
# lon_nudge = 0; lat_nudge = 0; lon_pad = 0; lat_pad = 0
ice_trend_grid_plot <- function(plot_title, pixel_res, check_conv = FALSE, 
                                lon_nudge = 0, lat_nudge = 0, lon_pad = 0, lat_pad = 0){

  # Get site short name
  short_name <- long_to_short_name(plot_title)
  
  # Prep data.frame
  if(!exists("ice_4km_annual_trends")) load("data/analyses/ice_4km_annual_trends.RData")
  ice_4km_annual_trends <- filter(ice_4km_annual_trends, trend < 10)
  
  # Get bbox from site name
  coords <- bbox_from_name(plot_title)
  
  # Convert to even grid
  df <- filter(ice_4km_annual_trends, site == short_name)
  df_even <- convert_even_grid(df, z_col = "trend", pixel_res = pixel_res) %>% na.omit() %>% 
    mutate(lon = lon+lon_nudge, lat = lat+lat_nudge)
  
  # get pixels that change very little.. maybe better not to bother
  
  # Crop down
  if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= coords[1]-lon_pad-10, x <= coords[2]+lon_pad+10,
           y >= coords[3]-lat_pad-10, y <= coords[4]+lat_pad+10)
  
  # Plot
  ice_plot <- ggplot(data = df_even, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = z, x = lon, y = lat), show.legend = F) +
    geom_contour(aes(z = z), colour = "purple", breaks = 0, size = 0.3, show.legend = F) +
    geom_polygon(data = coastline_full_df_sub, fill = "grey70", colour = "black",
                 aes(x = x, y = y, group = polygon_id)) +  
    annotate("rect",  colour = "black", fill = NA, alpha = 0.1,
             xmin = coords[1], xmax = coords[2], ymin = coords[3], ymax = coords[4]) +
    scale_fill_gradient2(low = "darkolivegreen", mid = "white", high = "deepskyblue", aesthetics = c("colour", "fill"),
                         limits = c(min(ice_4km_annual_trends$trend), max(ice_4km_annual_trends$trend))) +
    labs(title = plot_title, fill = "Ice cover\ndays/year", x = NULL, y = NULL) +
    coord_quickmap(expand = F,
                   xlim = c(coords[1]-lon_pad, coords[2]+lon_pad), 
                   ylim = c(coords[3]-lat_pad, coords[4]+lat_pad)) +
    # theme_void() +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "white", colour = site_colours[plot_title], size = 3), # NB: requires site_colours in environment
          axis.text = element_blank(), axis.ticks = element_blank(), # Remove coords
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Remove axis lines
          legend.position = "bottom")
  # ice_plot
  if(check_conv){
    ice_plot <- ice_plot +
      geom_point(data = df, size = 4, colour = "black") +
      geom_point(data = df, size = 3, aes(colour = trend))
  }
  return(ice_plot)
  # rm(plot_title, pixel_res, short_name, coords, df_even, coastline_full_df_sub, ice_plot, check_conv, lon_nudge, lat_nudge, lon_pad, lat_pad)
}

# Ice prop function
ice_cover_prop <- function(ice_df){
  
  # Get open water pixel count
  water_pixels <- ice_df |> filter(date == "2017-08-01", sea_ice_extent %in% c(1, 3)) |> nrow()
  
  # Find proportion per month per year that has ice
  ice_prop <- ice_df |> 
    filter(sea_ice_extent == 3,
           date <= "2021-12-31") |> 
    # group_by(date) |> 
    summarise(count = n(),
              prop = count/water_pixels, .by = "date") |> 
    # complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
    tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) |>  # NB: Only works with 4km data time series
    tidyr::replace_na(list(count = 0, prop = 0)) |>  
    mutate(date = lubridate::round_date(date, "month"),
           year = lubridate::year(date),
           month = lubridate::month(date, abbr = TRUE, label = TRUE)) |> 
    group_by(year, month, date) |> 
    summarise(mean_prop = round(mean(prop, na.rm = T), 2), .groups = "drop")
}

# Function for calculating open water days
ice_open_water <- function(ice_df){
  
  # Get open water pixel count
  water_pixels <- ice_df |> filter(date == "2017-08-01", sea_ice_extent %in% c(1, 3)) |> nrow()
  
  # Find proportion per day that has ice
  ice_prop <- ice_df |> 
    filter(sea_ice_extent == 3,
           date <= "2021-12-31") |> 
    # group_by(date) |> 
    summarise(count = n(),
              prop = count/water_pixels, .by = "date") |> 
    # complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
    tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) |>  # NB: Only works with 4km data time series
    tidyr::replace_na(list(count = 0, prop = 0)) |> 
    # 5 day running mean
    mutate(prop_smooth = round(zoo::rollmean(prop, 5, fill = prop), 2),
           open_water = ifelse(prop_smooth <= 0.15, TRUE, FALSE))

  # RLE approach
  open_rle <- heatwaveR:::proto_event(ice_prop, ice_prop$open_water, minDuration = 1, joinAcrossGaps = FALSE, maxGap = 0)
  
  # Get some metrics
  open_starts <- open_rle |> group_by(event_no) |> filter(!is.na(event_no), date == min(date)) |> ungroup()
  open_ends <- open_rle |> group_by(event_no) |> filter(!is.na(event_no), date == max(date)) |> ungroup()
  open_date_range <- left_join(open_starts[,c("date", "event_no")], open_ends[,c("date", "event_no")], by = "event_no") |> 
    dplyr::rename(date_start = date.x, date_end = date.y) |> dplyr::select(event_no, date_start, date_end)
  open_annual_events <- open_rle |> filter(!is.na(event_no)) |>  mutate(year = year(date)) |> 
    dplyr::select(year, event_no) |> distinct() |> summarise(event_n = n(), .by = year)
  open_annual_days <- open_rle |> filter(!is.na(event_no)) |>  mutate(year = year(date)) |> 
    dplyr::select(year, event_no) |> summarise(open_days = n(), .by = year)
  open_annual <- left_join(open_annual_events, open_annual_days, by = "year")
  
  # Exit
  return(open_annual)
}

# Annual trend in values
## NB: This assumes the annual dataframe is temporally complete
## It also assumes the value column is called 'val'
trend_calc <- function(df){
  
  # Annual trends
  trend_res <- broom::tidy(lm(val ~ year, df)) %>% 
    slice(2) %>% 
    mutate(trend = round(estimate, 3),
           p.value = round(p.value, 4)) %>% 
    dplyr::select(trend, p.value)
  
  # Total means
  sum_stats <- df %>% 
    summarise(mean_val = round(mean(val, na.rm = T), 2),
              sd_val = round(sd(val, na.rm = T), 3), .groups = "drop")
  
  # Combine and exit
  res <- cbind(trend_res, sum_stats)
  rm(df, trend_res, sum_stats); gc()
  return(res)
}

# Calculate linear models and output a tidy tibble
lm_tidy <- function(df, x_var, y_var){
  df_lm <- df %>% 
    do(mod = lm(as.formula(paste0(y_var," ~ ",x_var)), data = .)) %>% ungroup() %>% 
    mutate(tidy = map(mod, broom::tidy),
           glance = map(mod, broom::glance),
           # augment = map(mod, broom::augment), # Not needed, but good to know
           # slope = tidy %>% map_dbl(function(x) round(x$estimate[2], 4)),
           slope = tidy %>% map_dbl(function(x) x$estimate[2]), # Better not to round so it is more useful
           rsq = glance %>% map_dbl(function(x) round(x$r.squared[1], 4)),
           pval = tidy %>% map_dbl(function(x) round(x$p.value[2], 4)),
           nobs = glance %>% map_dbl(function(x) round(x$nobs[1], 4))) %>% 
    dplyr::select(-mod, -glance, -tidy) %>%
    filter(!is.na(rsq), !is.na(slope), !is.na(pval))
  return(df_lm)
}

# Calculate linear models on all possible driver comparisons for two given drivers
lm_all <- function(df_idx, df_main){
  df_sub <- df_main %>% right_join(df_idx, by = c("type", "driver", "variable", "depth"))
  if(nrow(df_sub) < 3) return()
  df_filter <- df_main %>% 
    filter(driver != df_sub$driver[1])
  df_join <- df_sub %>%
    left_join(df_filter, by = c("site", "date"), suffix = c("", "_y")) %>% 
    filter(!is.na(value), !is.na(value_y))
  if(nrow(df_join) < 3) return()
  df_sub_lm <- df_sub %>%
    filter(date >= as.Date("1982-01-01"), date <= "2020-12-31") %>% 
    group_by(site, type, category, driver, variable, depth) %>% 
    mutate(min_date = min(date, na.rm = T),
           mean_val = mean(value, na.rm = T),
           max_date = max(date, na.rm = T)) %>% 
    complete(date = seq.Date(as.Date("1982-01-01"), as.Date("2020-12-31"), by = "month")) %>%
    fill(var_index, mean_val, min_date, max_date, .direction = "downup") %>%
    group_by(site, type, category, driver, variable, depth, min_date, mean_val, max_date) %>%
    lm_tidy(df = ., x_var = "date", y_var = "value")
  df_res <- df_join %>% 
    group_by(site, type, type_y, category, category_y, driver, driver_y, variable, variable_y, depth, depth_y) %>% 
    mutate(min_date = min(date, na.rm = T),
           mean_val = mean(value_y, na.rm = T),
           max_date = max(date, na.rm = T)) %>% 
    group_by(site, type, type_y, category, category_y, driver, driver_y, variable, variable_y, depth, depth_y,
             min_date, mean_val, max_date) %>% 
    lm_tidy(df = ., x_var = "value", y_var = "value_y") %>% 
    bind_rows(df_sub_lm)
  rm(df_sub, df_filter, df_join, df_sub_lm); gc()
  return(df_res)
}

# Find all possible linear model comparisons for two given drivers
# driver1 <- "sea temp"; driver2 <- "sea ice" # For testing
driver2_lm <- function(driver1, driver2){
  
  # If no variables exist for driver  (e.g. governance) return nothing
  if(nrow(filter(clean_all_clean, driver == driver1)) == 0) return()
  if(nrow(filter(clean_all_clean, driver == driver2)) == 0) return()
  
  # Filter out only the two drivers in question
  df_driver <- clean_all_clean %>% 
    filter(driver %in% c(driver1, driver2),
           site %in% long_site_names$site)
  
  # Get monthly means by depth across entire site
  df_mean_month_depth <- df_driver %>% 
    dplyr::select(type, site, category, driver, variable, date, depth, value) %>% 
    filter(!is.na(date)) %>% distinct() %>% 
    mutate(date = lubridate::round_date(date, unit = "month"),
           depth = case_when(depth < 0 ~ "land",
                             is.na(depth) & variable == "ablation [m w.e.]" ~ "land",
                             is.na(depth) ~ "surface",
                             depth <= 10 ~ "0 to 10",
                             depth <= 50 ~ "10 to 50",
                             depth <= 200 ~ "50 to 200",
                             depth > 200 ~ "+200")) %>%
    group_by(type, site, category, driver, variable, date, depth) %>%
    summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
    filter(!is.na(value)); gc()
  
  # Filter out annual values
  annual_screen <- df_mean_month_depth %>%
    mutate(year = lubridate::year(date)) %>% 
    group_by(type, site, category, driver, variable, year, depth) %>% 
    summarise(annual_count = n(), .groups = "drop") %>% 
    filter(annual_count > 1) %>% # Remove data that never have more than one count per year
    dplyr::select(type, site, category, driver, variable, depth) %>% 
    distinct()
  df_screen <- right_join(df_mean_month_depth, annual_screen, 
                          by = c("type", "site", "category", "driver", "variable", "depth"))
  
  # Test df
  # df_sub <- df_screen %>% 
  #   filter(site == "kong", depth == "0 to 10", type == "in situ",
  #          between(date, as.Date("1982-01-01"), as.Date("2020-12-31")))
  
  # Test visual
  # ggplot(df_sub, aes(x = date, y = value)) +
  #   geom_line() + geom_smooth(method = "lm")
  
  # The list of possible variables to compare
  df_var <- df_screen %>% filter(driver == driver1) %>% 
    dplyr::select(type, driver, variable, depth) %>% distinct() %>% mutate(var_index = 1:n())
  
  # get all stats
  df_lm <- plyr::ddply(df_var, c("var_index"), lm_all, df_main = df_screen, .parallel = TRUE)
  
  # Clean up and exit
  if(nrow(df_lm) > 0) df_lm <- dplyr::select(df_lm, -var_index)
  rm(annual_screen, df_driver, df_mean_month_depth, df_var); gc()
  return(df_lm)
}

# Network arrows grob
# This changes the arrow heads from the end to the middle
# NB: This overrides the default behaviour
middlePathGrob <- function (x, y, id = NULL, id.lengths = NULL, arrow = NULL, start.cap = NULL,
          start.cap2 = NULL, start.captype = "circle", end.cap = NULL,
          end.cap2 = NULL, end.captype = "circle", default.units = "npc",
          name = NULL, gp = gpar(), vp = NULL, constant = TRUE)
{
  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }
  if (is.null(id)) {
    if (!is.null(id.lengths)) {
      id <- rep(seq_along(id.lengths), id.lengths)
    }
    else {
      id <- rep(1, length(x))
    }
  }
  n <- length(unique(id))
  start.cap <- validate_cap(start.cap, default.units, n)
  if (is.null(start.cap2)) {
    start.cap2 <- start.cap
  }
  else {
    start.cap2 <- validate_cap(start.cap2, default.units,
                               n)
  }
  end.cap <- validate_cap(end.cap, default.units, n)
  if (is.null(end.cap2)) {
    end.cap2 <- end.cap
  }
  else {
    end.cap2 <- validate_cap(end.cap2, default.units, n)
  }
  if (!all(c(start.captype, end.captype) %in% c("circle", "rect"))) {
    stop("captype must be either `circle` or `rect`", call. = FALSE)
  }
  start.captype <- rep(start.captype, length.out = n)
  end.captype <- rep(end.captype, length.out = n)
  n_points <- length(x)
  group_diff <- id[-1] != id[-n_points]
  start <- c(TRUE, group_diff)
  end <- c(group_diff, TRUE)
  if (constant) {
    gp <- lapply(gp, function(par) {
      if (length(par) == length(end)) {
        par[start]
      }
      else {
        par
      }
    })
  }
  else {
    gp <- lapply(gp, function(par) {
      if (length(par) == length(end)) {
        par[!end]
      }
      else {
        par
      }
    })
  }
  class(gp) <- "gpar"
  if(is.null(arrow)) {
    # same code as before, if no arrow needs to be drawn
    if (is.null(start.cap) && is.null(end.cap)) {
      if (constant) {
        grob(x = x, y = y, id = id, id.lengths = NULL, arrow = arrow,
             name = name, gp = gp, vp = vp, cl = "polyline")
      }
      else {
        grob(x0 = x[!end], y0 = y[!end],
             x1 = x[!start], y1 = y[!start],
             id = id[!end], arrow = arrow,
             name = name, gp = gp, vp = vp, cl = "segments")
      }
    } else {
      gTree(x = x, y = y, id = id, arrow = arrow, constant = constant,
            start = start, end = end, start.cap = start.cap,
            start.cap2 = start.cap2, start.captype = start.captype,
            end.cap = end.cap, end.cap2 = end.cap2, end.captype = end.captype,
            name = name, gp = gp, vp = vp, cl = "cappedpathgrob")
    }
  } else {
    # split x/y/ID values corresponding to each ID into two halves; first half to
    # end with the specified arrow aesthetics; second half (with a repetition of the
    # last value from first half, so that the two halves join up) has arrow set to NULL.
    id.split = split(id, id)
    id.split = lapply(id.split,
                      function(i) c(rep(TRUE, ceiling(length(i)/2)),
                                    rep(FALSE, length(i) - ceiling(length(i)/2))))
    id.split = unsplit(id.split, id)
    id.first.half = which(id.split == TRUE)
    id.second.half = which(id.split == FALSE |
                             (id.split == TRUE & c(id.split[-1], FALSE) == FALSE))

    if (is.null(start.cap) && is.null(end.cap)) {
      if (constant) {
        gList(grob(x = x[id.first.half], y = y[id.first.half], id = id[id.first.half],
                   id.lengths = NULL, arrow = arrow,
                   name = name, gp = gp, vp = vp, cl = "polyline"),
              grob(x = x[id.second.half], y = y[id.second.half], id = id[id.second.half],
                   id.lengths = NULL, arrow = NULL,
                   name = name, gp = gp, vp = vp, cl = "polyline"))

      }
      else {
        # I haven't modified this chunk as I'm not familiar with ggraph,
        # & haven't managed to trigger constant == FALSE condition yet
        # to test out code modifications here
        grob(x0 = x[!end], y0 = y[!end],
             x1 = x[!start], y1 = y[!start],
             id = id[!end], arrow = arrow,
             name = name, gp = gp, vp = vp, cl = "segments")
      }
    } else {
      gList(gTree(x = x[id.first.half], y = y[id.first.half], id = id[id.first.half],
                  arrow = arrow, constant = constant,
                  start = start, end = end, start.cap = start.cap,
                  start.cap2 = start.cap2, start.captype = start.captype,
                  end.cap = end.cap, end.cap2 = end.cap2, end.captype = end.captype,
                  name = name, gp = gp, vp = vp, cl = "cappedpathgrob"),
            gTree(x = x[id.second.half], y = y[id.second.half], id = id[id.second.half],
                  arrow = NULL, constant = constant,
                  start = start, end = end, start.cap = start.cap,
                  start.cap2 = start.cap2, start.captype = start.captype,
                  end.cap = end.cap, end.cap2 = end.cap2, end.captype = end.captype,
                  name = name, gp = gp, vp = vp, cl = "cappedpathgrob"))
    }
  }
}
R.utils::reassignInPackage("cappedPathGrob", "ggraph", middlePathGrob)

