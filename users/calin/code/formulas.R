# Code/formulas.R
# folders with formulas use for the FACE-IT project

# Set up ------------------------------------------------------------------
library(tidyverse)


# Formulas ----------------------------------------------------------------
# Name to latin + english one
## Use it as -> mutate(nomsp = map(nomSpecies, latin_eng))
latin_eng <- function(nomSpecies){
  # Fish |FIS|
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
  if(nomSpecies == "Falco peregrinus") nom_long <- "|SBI| Falco peregrinus (peregrine falcon)"
  if(nomSpecies == "Fratercula arctica") nom_long <- "|SBI| Fratercula arctica (atlantic puffin)"
  if(nomSpecies == "Fulmarus glacialis") nom_long <- "|SBI| Fulmarus glacialis (northern fulmar)"
  if(nomSpecies == "Gavia immer") nom_long <- "|SBI| Gavia immer (great northern diver)"
  if(nomSpecies == "Haliaeetus albicilla") nom_long <- "|SBI| Haliaeetus albicilla (white-tailed eagle)"
  if(nomSpecies == "Larus argentatus") nom_long <- "|SBI| Larus argentatus (herring gull)"
  if(nomSpecies == "Larus canus") nom_long <- "|SBI| Larus canus (common gull)"
  if(nomSpecies == "Larus delawarensis") nom_long <- "|SBI| Larus delawarensis (ring-billed gull)"
  if(nomSpecies == "Larus fuscus") nom_long <- "|SBI| Larus fuscus (lesser black-backed gull)"
  if(nomSpecies == "Larus glaucoides") nom_long <- "|SBI| Larus glaucoides (iceland gull)"
  if(nomSpecies %in% c("Larus hyperboreus", "Larus hypeboreus", "GLGU")) nom_long <- "|SBI| Larus hyperboreus (glaucous gull)"
  if(nomSpecies == "Larus marinus") nom_long <- "|SBI| Larus marinus (great black-backed gull)"
  if(nomSpecies == "Larus smithonianus") nom_long <- "|SBI| Larus smithonianus (american herring gull)"
  if(nomSpecies %in% c("Larus sp.", "Larus glaucoides/hyperboreus")) nom_long <- "|SBI| Larus sp. (gull unidentified)"
  if(nomSpecies == "Morus bassanus") nom_long <- "|SBI| Morus bassanus (Northern gannet)"
  if(nomSpecies == "Pagophila eburnea") nom_long <- "|SBI| Pagophila eburnea (ivory gull)"
  if(nomSpecies == "Phalacrocorax carbo") nom_long <- "|SBI| Phalacrocorax carbo (great cormorant)"
  if(nomSpecies == "Phalaropus fulicarius") nom_long <- "|SBI| Phalaropus fulicarius (red phalarope)"
  if(nomSpecies == "Rissa tridactyla") nom_long <- "|SBI| Rissa tridactyla (black-legged kittiwake)"
  if(nomSpecies %in% c("Somateria mollissima", "Somateria mollissima borealis", "Sommateria mollissima")) nom_long <- "|SBI| Somateria mollissima (common eider)"
  if(nomSpecies == "Somateria spectabilis") nom_long <- "|SBI| Somateria spectabilis (king eider)"
  if(nomSpecies == "Stercorarius longicaudus") nom_long <- "|SBI| Stercorarius longicaudus (long-tailed skua)"
  if(nomSpecies == "Stercorarius parasiticus") nom_long <- "|SBI| Stercorarius parasiticus (arctic skua)"
  if(nomSpecies == "Sterna paradisaea") nom_long <- "|SBI| Sterna paradisaea (arctic tern)"
  if(nomSpecies == "Uria aalge") nom_long <- "|SBI| Uria aalge (common guillemot)"
  if(nomSpecies == "Uria lomvia") nom_long <- "|SBI| Uria lomvia (brünnich’s guillemot)"
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








# Function for converting UTM to decimal degrees
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

