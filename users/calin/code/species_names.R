# Code/species_names.R
# function to automaticly change species latin name into its latin plus english one

source('code/functions.R')
# Set up ------------------------------------------------------------------

library(tidyverse)


# Function ----------------------------------------------------------------
nom_latin_com <- function(Species){
  mutate(variable = case_when(Species == "Alca torda"~"Alca torda (razorbill)",
                              Species == "Anas platyrhynchos"~"Anas platyrhynchos (mallard)",
                              Species == "Boreogadus saida"~"Boreogadus saida (polar cod)",
                              Species == "Branta Canadensis"~"Branta Canadensis (canada goose)",
                              Species == "Calanus finmarchicus"~"Calanus finmarchicus (atlantic calanus (zooplankton))",
                              Species == "Calanus glacialis"~"Calanus glacialis (arctic calanus (zooplankton))",
                              Species == "Calidris maritima"~"Calidris maritima (purple sandpiper)",
                              Species == "Cepphus grylle"~"Cepphus grylle (black guillemot)",
                              Species == "Clupea harengus"~"Clupea harengus (herring)",
                              Species == "Clangula hyemalis"~"Clangula hyemalis (long-tailed duck)",
                              Species == "Corvus corax"~"Corvus corax (raven)",
                              Species == "Cystophora cristata"~"Cystophora cristata (hooded seal)",
                              Species == "Falco peregrinus"~"Falco peregrinus (peregrine falcon)",
                              Species == "Fratercula arctica"~"Fratercula arctica (atlantic puffin)",
                              Species == "Fulmarus glacialis"~"Fulmarus glacialis (northern fulmar)",
                              Species == "Gadus morhua"~"Gadus morhua (northeast arctic cod)",
                              Species == "Gavia stallata"~"Gavia stallata (red-throated diver/red-throated loon)",
                              Species == "Histrionicus histrionicus"~"Histrionicus histrionicus (harlequin duck)",
                              Species == "Larus argentatus"~"Larus argentatus (herring gull)",
                              Species == "Larus canus"~"Larus canus (common gull/mew gull)",
                              Species == "Larus delawarensis"~"Larus delawarensis (ring-billed gull)",
                              Species == "Larus fuscus"~"Larus fuscus (lesser black-backed gull)",
                              Species == "Larus glaucoides"~"Larus glaucoides (iceland gull)",
                              Species == "Larus hyperboreus"~"Larus hyperboreus (glaucous gull)",
                              Species == "Larus marinus"~"Larus marinus (great black-backed gull)",
                              Species == "Lagopus mutus"~"Lagopus mutus (rock ptarmigan)",
                              Species == "Larus smithonianus"~"Larus smithonianus (american herring gull)",
                              Species == "Larus sp."~"Larus sp. (gull unidentified)",
                              Species == "Mallotus villosus"~"Mallotus villosus (capelin)",
                              Species == "Mergus serratus"~"Mergus serratus (red-breasted merganser)",
                              Species == "Odobenus marinus"~"Odobenus marinus (walrus)",
                              Species == "Pagophila eburnea"~"Pagophila eburnea (ivory gull)",
                              Species == "Pagophilus groenlandicus"~"Pagophilus groenlandicus (harp seal)",
                              Species == "Phalacrocorax carbo"~"Phalacrocorax carbo (great cormorant)",
                              Species == "Rissa tridactyla"~"Rissa tridactyla (black-legged kittiwake)",
                              Species == "Sebastes mentella"~"Sebastes mentella (beaked redfish)",
                              Species == "Sebastes norvegicus"~"Sebastes norvegicus (golden redfish)",
                              Species == "Somateria mollissima borealis"~"Somateria mollissima borealis (common eider)",
                              Species == "Somateria spectabilis"~"Somateria spectabilis (king eider)",
                              Species == "Uria aalge"~"Uria aalge (common guillemot)",
                              Species == "Uria lomvia"~"Uria lomvia (brünnich’s guillemot)",
                              Species == "Ursus maritimus"~"Ursus maritimus (polar bear)",
                              Species == "Sterna paradisaea"~"Sterna paradisaea (arctic tern)",
                              Species == "Stercorarius parasiticus"~"Stercorarius parasiticus (arctic skua/parasitic jaeger)",
                              Species == "Arenaria interpres"~"Arenaria interpres (ruddy turnstone)",
                              Species == "Calidris alba"~"Calidris alba (sanderling)",
                              Species == "Charadrius hiaticula"~"Charadrius hiaticula (common ringed plover)",
                              Species == "Gavia stellata"~"Gavia stellata (red-throated loon)",
                              Species == "Calidris alpina"~"Calidris alpina (dunlin)",
                              Species == "Stercorarius longicaudus"~"Stercorarius longicaudus (long-tailed jaeger)",
                              Species == "Anser brachyrhynchus"~"Anser brachyrhynchus (pink-footed goose)",
                              Species == "Larus hypeboreus"~"Larus hypeboreus (glaucous gull)",
                              Species == "Calidris canutus"~"Calidris canutus (red knot)",
                              Species == "Lagopus muta"~"Lagopus muta (rock ptarmigan)",
                              Species == "Phalaropus lobatus"~"Phalaropus lobatus (red-necked phalarope)",
                              Species == "Somateria mollissima"~"Somateria mollissima (common eider)",
                              Species == "Bubo scandiacus"~"Bubo scandiacus (snowy owl)",
                              Species == "Phalaropus fulicarius"~"Phalaropus fulicarius (red phalarope)",
                              Species == "Calcarius lapponicus"~"Calcarius lapponicus (lapland longspur)",
                              Species == "Plectrophenax nivalis"~"Plectrophenax nivalis (snow bunting)",
                              Species == "Calidris melanotos"~"Calidris melanotos (pectoral sandpiper)"
  ))
}








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

nom_latin_com <- function(nomSpecies){
  if(nomSpecies == "Alca torda") la_specie <- "Alca torda (razorbill)" else
    if(nomSpecies == "Anas platyrhynchos") la_specie <- "Anas platyrhynchos (mallard)" else
      if(nomSpecies == "Boreogadus saida") la_specie <- "Boreogadus saida (polar cod)" else
        if(nomSpecies == "Branta Canadensis") la_specie <- "Branta Canadensis (canada goose)" else
          if(nomSpecies == "Calanus finmarchicus") la_specie <- "Calanus finmarchicus (atlantic calanus (zooplankton))" else
            if(nomSpecies == "Calanus glacialis") la_specie <- "Calanus glacialis (arctic calanus (zooplankton))" else
              if(nomSpecies == "Calidris maritima") la_specie <- "Calidris maritima (purple sandpiper)" else
                if(nomSpecies == "Cepphus grylle") la_specie <- "Cepphus grylle (black guillemot)" else
                  if(nomSpecies == "Clupea harengus") la_specie <- "Clupea harengus (herring)" else
                    if(nomSpecies == "Clangula hyemalis") la_specie <- "Clangula hyemalis (long-tailed duck)" else
                      if(nomSpecies == "Corvus corax") la_specie <- "Corvus corax (raven)" else
                        if(nomSpecies == "Cystophora cristata") la_specie <- "Cystophora cristata (hooded seal)" else
                          if(nomSpecies == "Falco peregrinus") la_specie <- "Falco peregrinus (peregrine falcon)" else
                            if(nomSpecies == "Fratercula arctica") la_specie <- "Fratercula arctica (atlantic puffin)" else
                              if(nomSpecies == "Fulmarus glacialis") la_specie <- "Fulmarus glacialis (northern fulmar)" else
                                if(nomSpecies == "Gadus morhua") la_specie <- "Gadus morhua (northeast arctic cod)" else
                                  if(nomSpecies == "Gavia stallata") la_specie <- "Gavia stallata (red-throated loon)" else
                                    if(nomSpecies == "Histrionicus histrionicus") la_specie <- "Histrionicus histrionicus (harlequin duck)" else
                                      if(nomSpecies == "Larus argentatus") la_specie <- "Larus argentatus (herring gull)" else
                                        if(nomSpecies == "Larus canus") la_specie <- "Larus canus (common gull/mew gull)" else
                                          if(nomSpecies == "Larus delawarensis") la_specie <- "Larus delawarensis (ring-billed gull)" else
                                            if(nomSpecies == "Larus fuscus") la_specie <- "Larus fuscus (lesser black-backed gull)" else
                                              if(nomSpecies == "Larus glaucoides") la_specie <- "Larus glaucoides (iceland gull)" else
                                                if(nomSpecies == "Larus hyperboreus") la_specie <- "Larus hyperboreus (glaucous gull)" else
                                                  if(nomSpecies == "Larus marinus") la_specie <- "Larus marinus (great black-backed gull)" else
                                                    if(nomSpecies == "Lagopus mutus") la_specie <- "Lagopus mutus (rock ptarmigan)" else
                                                      if(nomSpecies == "Larus smithonianus") la_specie <- "Larus smithonianus (american herring gull)" else
                                                        if(nomSpecies == "Larus sp.") la_specie <- "Larus sp. (gull unidentified)" else
                                                          if(nomSpecies == "Mallotus villosus") la_specie <- "Mallotus villosus (capelin)" else
                                                            if(nomSpecies == "Mergus serratus") la_specie <- "Mergus serratus (red-breasted merganser)" else
                                                              if(nomSpecies == "Odobenus marinus") la_specie <- "Odobenus marinus (walrus)" else
                                                                if(nomSpecies == "Pagophila eburnea") la_specie <- "Pagophila eburnea (ivory gull)" else
                                                                  if(nomSpecies == "Pagophilus groenlandicus") la_specie <- "Pagophilus groenlandicus (harp seal)" else
                                                                    if(nomSpecies == "Phalacrocorax carbo") la_specie <- "Phalacrocorax carbo (great cormorant)" else
                                                                      if(nomSpecies == "Rissa tridactyla") la_specie <- "Rissa tridactyla (black-legged kittiwake)" else
                                                                        if(nomSpecies == "Sebastes mentella") la_specie <- "Sebastes mentella (beaked redfish)" else
                                                                          if(nomSpecies == "Sebastes norvegicus") la_specie <- "Sebastes norvegicus (golden redfish)" else
                                                                            if(nomSpecies == "Somateria mollissima borealis") la_specie <- "Somateria mollissima borealis (common eider)" else
                                                                              if(nomSpecies == "Somateria spectabilis") la_specie <- "Somateria spectabilis (king eider)" else
                                                                                if(nomSpecies == "Uria aalge") la_specie <- "Uria aalge (common guillemot)" else
                                                                                  if(nomSpecies == "Uria lomvia") la_specie <- "Uria lomvia (brünnich’s guillemot)" else
                                                                                    if(nomSpecies == "Ursus maritimus") la_specie <- "Ursus maritimus (polar bear)" else
                                                                                      if(nomSpecies == "Sterna paradisaea") la_specie <- "Sterna paradisaea (arctic tern)" else
                                                                                        if(nomSpecies == "Stercorarius parasiticus") la_specie <- "Stercorarius parasiticus (arctic skua/parasitic jaeger)" else
                                                                                          if(nomSpecies == "") la_specie <- " ()" else
                                                                                            if(nomSpecies == "Arenaria interpres") la_specie <- "Arenaria interpres (ruddy turnstone)" else
                                                                                              if(nomSpecies == "Calidris alba") la_specie <- "Calidris alba (sanderling)" else
                                                                                                if(nomSpecies == "Charadrius hiaticula") la_specie <- "Charadrius hiaticula (common ringed plover)" else
                                                                                                  if(nomSpecies == "Gavia stellata") la_specie <- "Gavia stellata (red-throated loon)" else
                                                                                                    if(nomSpecies == "Calidris alpina") la_specie <- "Calidris alpina (dunlin)" else
                                                                                                      if(nomSpecies == "Stercorarius longicaudus") la_specie <- "Stercorarius longicaudus (long-tailed jaeger)" else
                                                                                                        if(nomSpecies == "Anser brachyrhynchus") la_specie <- "Anser brachyrhynchus (pink-footed goose)" else
                                                                                                          if(nomSpecies == "Larus hypeboreus") la_specie <- "Larus hypeboreus (glaucous gull)" else
                                                                                                            if(nomSpecies == "Calidris canutus") la_specie <- "Calidris canutus (red knot)" else
                                                                                                              if(nomSpecies == "Lagopus muta") la_specie <- "Lagopus muta (rock ptarmigan)" else
                                                                                                                if(nomSpecies == "Phalaropus lobatus") la_specie <- "Phalaropus lobatus (red-necked phalarope)" else
                                                                                                                  if(nomSpecies == "Somateria mollissima") la_specie <- "Somateria mollissima (common eider)" else
                                                                                                                    if(nomSpecies == "Bubo scandiacus") la_specie <- "Bubo scandiacus (snowy owl)" else
                                                                                                                      if(nomSpecies == "Phalaropus fulicarius") la_specie <- "Phalaropus fulicarius (red phalarope)" else
                                                                                                                        if(nomSpecies == "Calcarius lapponicus") la_specie <- "Calcarius lapponicus (lapland longspur)" else
                                                                                                                          if(nomSpecies == "Plectrophenax nivalis") la_specie <- "Plectrophenax nivalis (snow bunting)" else
                                                                                                                            if(nomSpecies == "Calidris melanotos") la_specie <- "Calidris melanotos (pectoral sandpiper)"
                                                                                                                            
                                                                                                                            return(la_specie)
}


