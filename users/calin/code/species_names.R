# Code/species_names.R
# function to automaticly change species latin name into its latin plus english one

source('code/functions.R')
# Set up ------------------------------------------------------------------

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
}



testif <- function(x){ mutate(
  
)
  if (x == 1) {y =="YES"} else {
    if (x == 2) {y =="YES2"} else {
    NA}}
  return(y)
}






testif(2)





















































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


