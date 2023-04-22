# Code/species_names.R
# function to automaticly change species latin name into its latin plus english one

# Set up ------------------------------------------------------------------

# source('code/functions.R')
library(tidyverse)


# Function ----------------------------------------------------------------

nom_latin_com2 <- function(nomSpecies){
  # Fish
  if(nomSpecies == "Boreogadus saida") {"Boreogadus saida (polar cod)"}
  if(nomSpecies == "Clupea harengus") {"Clupea harengus (herring)"}
  if(nomSpecies == "Gadus morhua") {"Gadus morhua (northeast arctic cod)"}
  if(nomSpecies == "Mallotus villosus") {"Mallotus villosus (capelin)"}
  if(nomSpecies == "Sebastes mentella") {"Sebastes mentella (beaked redfish)"}
  if(nomSpecies == "Sebastes norvegicus") {"Sebastes norvegicus (golden redfish)"}
  #Marine Mammal
  if(nomSpecies == "Cystophora cristata") {"Cystophora cristata (hooded seal)"}
  if(nomSpecies == "Odobenus marinus") {"Odobenus marinus (walrus)"}
  if(nomSpecies == "Pagophilus groenlandicus") {"Pagophilus groenlandicus (harp seal)"}
  if(nomSpecies == "Ursus maritimus") {"Ursus maritimus (polar bear)"}
  #Seabird
  if(nomSpecies == "Alca torda") {"Alca torda (razorbill)"}
  if(nomSpecies == "Anas platyrhynchos") {"Anas platyrhynchos (mallard)"}
  if(nomSpecies == "Branta Canadensis") {"Branta Canadensis (canada goose)"}
  if(nomSpecies == "Calidris maritima") {"Calidris maritima (purple sandpiper)"}
  if(nomSpecies == "Cepphus grylle") {"Cepphus grylle (black guillemot)"}
  if(nomSpecies == "Clangula hyemalis") {"Clangula hyemalis (long-tailed duck)"}
  if(nomSpecies == "Corvus corax") {"Corvus corax (raven)"}
  if(nomSpecies == "Falco peregrinus") {"Falco peregrinus (peregrine falcon)"}
  if(nomSpecies == "Fratercula arctica") {"Fratercula arctica (atlantic puffin)"}
  if(nomSpecies == "Fulmarus glacialis") {"Fulmarus glacialis (northern fulmar)"}
  if(nomSpecies == "Gavia stallata") {"Gavia stallata (red-throated loon)"}
  if(nomSpecies == "Histrionicus histrionicus") {"Histrionicus histrionicus (harlequin duck)"}
  if(nomSpecies == "Larus argentatus") {"Larus argentatus (herring gull)"}
  if(nomSpecies == "Larus canus") {"Larus canus (common gull)"}
  if(nomSpecies == "Larus delawarensis") {"Larus delawarensis (ring-billed gull)"}
  if(nomSpecies == "Larus fuscus") {"Larus fuscus (lesser black-backed gull)"}
  if(nomSpecies == "Larus glaucoides") {"Larus glaucoides (iceland gull)"}
  if(nomSpecies == "Larus hyperboreus") {"Larus hyperboreus (glaucous gull)"}
  if(nomSpecies == "Larus marinus") {"Larus marinus (great black-backed gull)"}
  if(nomSpecies == "Lagopus mutus") {"Lagopus mutus (rock ptarmigan)"}
  if(nomSpecies == "Larus smithonianus") {"Larus smithonianus (american herring gull)"}
  if(nomSpecies == "Larus sp.") {"Larus sp. (gull unidentified)"}
  if(nomSpecies == "Mergus serratus") {"Mergus serratus (red-breasted merganser)"}
  if(nomSpecies == "Pagophila eburnea") {"Pagophila eburnea (ivory gull)"}
  if(nomSpecies == "Phalacrocorax carbo") {"Phalacrocorax carbo (great cormorant)"}
  if(nomSpecies == "Rissa tridactyla") {"Rissa tridactyla (black-legged kittiwake)"}
  if(nomSpecies == "Somateria mollissima borealis") {"Somateria mollissima borealis (common eider)"}
  if(nomSpecies == "Somateria spectabilis") {"Somateria spectabilis (king eider)"}
  if(nomSpecies == "Uria aalge") {"Uria aalge (common guillemot)"}
  if(nomSpecies == "Uria lomvia") {"Uria lomvia (brünnich’s guillemot)"}
  if(nomSpecies == "Sterna paradisaea") {"Sterna paradisaea (arctic tern)"}
  if(nomSpecies == "Stercorarius parasiticus") {"Stercorarius parasiticus (parasitic jaeger)"}
  if(nomSpecies == "") {" ()"}
  if(nomSpecies == "Arenaria interpres") {"Arenaria interpres (ruddy turnstone)"}
  if(nomSpecies == "Calidris alba") {"Calidris alba (sanderling)"}
  if(nomSpecies == "Charadrius hiaticula") {"Charadrius hiaticula (common ringed plover)"}
  if(nomSpecies == "Gavia stellata") {"Gavia stellata (red-throated loon)"}
  if(nomSpecies == "Calidris alpina") {"Calidris alpina (dunlin)"}
  if(nomSpecies == "Stercorarius longicaudus") {"Stercorarius longicaudus (long-tailed jaeger)"}
  if(nomSpecies == "Anser brachyrhynchus") {"Anser brachyrhynchus (pink-footed goose)"}
  if(nomSpecies == "Larus hypeboreus") {"Larus hypeboreus (glaucous gull)"}
  if(nomSpecies == "Calidris canutus") {"Calidris canutus (red knot)"}
  if(nomSpecies == "Lagopus muta") {"Lagopus muta (rock ptarmigan)"}
  if(nomSpecies == "Phalaropus lobatus") {"Phalaropus lobatus (red-necked phalarope)"}
  if(nomSpecies == "Somateria mollissima") {"Somateria mollissima (common eider)"}
  if(nomSpecies == "Bubo scandiacus") {"Bubo scandiacus (snowy owl)"}
  if(nomSpecies == "Phalaropus fulicarius") {"Phalaropus fulicarius (red phalarope)"}
  if(nomSpecies == "Calcarius lapponicus") {"Calcarius lapponicus (lapland longspur)"}
  if(nomSpecies == "Plectrophenax nivalis") {"Plectrophenax nivalis (snow bunting)"}
  if(nomSpecies == "Calidris melanotos") {"Calidris melanotos (pectoral sandpiper)"}
  #Zooplankton
  if(nomSpecies == "Calanus finmarchicus") {"Calanus finmarchicus (atlantic calanus (zooplankton))"}
  if(nomSpecies == "Calanus glacialis") {"Calanus glacialis (arctic calanus (zooplankton))"}
  return(nomLong)
}

# 

nom_latin_com2("Boreogadus saida")
nom_latin_com2("Alca torda")



#


nom_lgeufhdjskcl <- function(nomSpecies){
  if(nomSpecies == "Boreogadus saida") nom_long <- "Boreogadus saida (polar cod)"
  if(nomSpecies == "Clupea harengus") nom_long <- "Clupea harengus (herring)"
  if(nomSpecies == "Gadus morhua") nom_long <- "Gadus morhua (northeast arctic cod)"
  if(nomSpecies == "Mallotus villosus") nom_long <- "Mallotus villosus (capelin)"
  if(nomSpecies == "Sebastes mentella") nom_long <- "Sebastes mentella (beaked redfish)"
  if(nomSpecies == "Sebastes norvegicus") nom_long <- "Sebastes norvegicus (golden redfish)"
  #Marine Mammal
  if(nomSpecies == "Cystophora cristata") nom_long <- "Cystophora cristata (hooded seal)"
  if(nomSpecies == "Odobenus marinus") nom_long <- "Odobenus marinus (walrus)"
  if(nomSpecies == "Pagophilus groenlandicus") nom_long <- "Pagophilus groenlandicus (harp seal)"
  if(nomSpecies == "Ursus maritimus") nom_long <- "Ursus maritimus (polar bear)"
  #Seabird
  if(nomSpecies == "Alca torda") nom_long <- "Alca torda (razorbill)"
  if(nomSpecies == "Anas platyrhynchos") nom_long <- "Anas platyrhynchos (mallard)"
  if(nomSpecies == "Anser brachyrhynchus") nom_long <- "Anser brachyrhynchus (pink-footed goose)"
  if(nomSpecies == "Arenaria interpres") nom_long <- "Arenaria interpres (ruddy turnstone)"
  if(nomSpecies == "Branta Canadensis") nom_long <- "Branta Canadensis (canada goose)"
  if(nomSpecies == "Bubo scandiacus") nom_long <- "Bubo scandiacus (snowy owl)"
  if(nomSpecies == "Calcarius lapponicus") nom_long <- "Calcarius lapponicus (lapland longspur)"
  if(nomSpecies == "Calidris alba") nom_long <- "Calidris alba (sanderling)"
  if(nomSpecies == "Calidris alpina") nom_long <- "Calidris alpina (dunlin)"
  if(nomSpecies == "Calidris canutus") nom_long <- "Calidris canutus (red knot)"
  if(nomSpecies == "Calidris maritima") nom_long <- "Calidris maritima (purple sandpiper)"
  if(nomSpecies == "Calidris melanotos") nom_long <- "Calidris melanotos (pectoral sandpiper)"
  if(nomSpecies == "Cepphus grylle") nom_long <- "Cepphus grylle (black guillemot)"
  if(nomSpecies == "Charadrius hiaticula") nom_long <- "Charadrius hiaticula (common ringed plover)"
  if(nomSpecies == "Clangula hyemalis") nom_long <- "Clangula hyemalis (long-tailed duck)"
  if(nomSpecies == "Corvus corax") nom_long <- "Corvus corax (raven)"
  if(nomSpecies == "Falco peregrinus") nom_long <- "Falco peregrinus (peregrine falcon)"
  if(nomSpecies == "Fratercula arctica") nom_long <- "Fratercula arctica (atlantic puffin)"
  if(nomSpecies == "Fulmarus glacialis") nom_long <- "Fulmarus glacialis (northern fulmar)"
  if(nomSpecies %in% c("Gavia stallata", "Gavia stellata")) nom_long <- "Gavia stallata (red-throated loon)"
  if(nomSpecies == "Histrionicus histrionicus") nom_long <- "Histrionicus histrionicus (harlequin duck)"
  if(nomSpecies %in% c("Lagopus muta", "Lagopus mutus")) nom_long <- "Lagopus mutus (rock ptarmigan)"
  if(nomSpecies == "Larus argentatus") nom_long <- "Larus argentatus (herring gull)"
  if(nomSpecies == "Larus canus") nom_long <- "Larus canus (common gull)"
  if(nomSpecies == "Larus delawarensis") nom_long <- "Larus delawarensis (ring-billed gull)"
  if(nomSpecies == "Larus fuscus") nom_long <- "Larus fuscus (lesser black-backed gull)"
  if(nomSpecies == "Larus glaucoides") nom_long <- "Larus glaucoides (iceland gull)"
  if(nomSpecies %in% c("Larus hyperboreus", "Larus hypeboreus")) nom_long <- "Larus hyperboreus (glaucous gull)"
  if(nomSpecies == "Larus marinus") nom_long <- "Larus marinus (great black-backed gull)"
  if(nomSpecies == "Larus smithonianus") nom_long <- "Larus smithonianus (american herring gull)"
  if(nomSpecies == "Larus sp.") nom_long <- "Larus sp. (gull unidentified)"
  if(nomSpecies == "Mergus serratus") nom_long <- "Mergus serratus (red-breasted merganser)"
  if(nomSpecies == "Pagophila eburnea") nom_long <- "Pagophila eburnea (ivory gull)"
  if(nomSpecies == "Phalacrocorax carbo") nom_long <- "Phalacrocorax carbo (great cormorant)"
  if(nomSpecies == "Phalaropus fulicarius") nom_long <- "Phalaropus fulicarius (red phalarope)"
  if(nomSpecies == "Phalaropus lobatus") nom_long <- "Phalaropus lobatus (red-necked phalarope)"
  if(nomSpecies == "Plectrophenax nivalis") nom_long <- "Plectrophenax nivalis (snow bunting)"
  if(nomSpecies == "Rissa tridactyla") nom_long <- "Rissa tridactyla (black-legged kittiwake)"
  if(nomSpecies %in% c("Somateria mollissima", "Somateria mollissima borealis")) nom_long <- "Somateria mollissima (common eider)"
  if(nomSpecies == "Somateria spectabilis") nom_long <- "Somateria spectabilis (king eider)"
  if(nomSpecies == "Stercorarius longicaudus") nom_long <- "Stercorarius longicaudus (long-tailed jaeger)"
  if(nomSpecies == "Stercorarius parasiticus") nom_long <- "Stercorarius parasiticus (parasitic jaeger)"
  if(nomSpecies == "Sterna paradisaea") nom_long <- "Sterna paradisaea (arctic tern)"
  if(nomSpecies == "Uria aalge") nom_long <- "Uria aalge (common guillemot)"
  if(nomSpecies == "Uria lomvia") nom_long <- "Uria lomvia (brünnich’s guillemot)"
  #Zooplankton
  if(nomSpecies == "Calanus finmarchicus") nom_long <- "Calanus finmarchicus (atlantic calanus (zooplankton))"
  if(nomSpecies == "Calanus glacialis") nom_long <- "Calanus glacialis (arctic calanus (zooplankton))"
  # Other
  if(nomSpecies == "") nom_long <- NA
  return(nom_long)
}


nom_lgeufhdjskcl("Uria aalge")


#fhusjk











































# UTM function ------------------------------------------------------------
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


# UTM ---------------------------------------------------------------------

# Function for converting UTM to decimal degrees
# NB: Expects an 'Easting' and 'Northing' column
convert_UTM_deg <- function(df, utm_zone){
  utmcoor <- sp::SpatialPoints(cbind(df$Easting, df$Northing), 
                               proj4string = CRS(paste0("+proj=utm +zone=",utm_zone)))
  longlatcoor <- spTransform(utmcoor, CRS("+proj=longlat"))
  df$lon <- coordinates(longlatcoor)[,1]
  df$lat <- coordinates(longlatcoor)[,2]
  return(df)
}




# Species -----------------------------------------------------------------


