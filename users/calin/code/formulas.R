# Code/formulas.R
# folders with formulas use for the FACE-IT project

# Set up ------------------------------------------------------------------
library(tidyverse)


# Formulas ----------------------------------------------------------------
# Name to latin + english one
## Use it as -> mutate(nomsp = map(nomSpecies, latin_eng))
latin_eng <- function(nomSpecies){
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
  if(nomSpecies == "Acanthis hornemanni") nom_long <- "Acanthis hornemanni(arctic redpoll)"
  if(nomSpecies == "Alca torda") nom_long <- "Alca torda (razorbill)"
  if(nomSpecies == "Anas platyrhynchos") nom_long <- "Anas platyrhynchos (mallard)"
  if(nomSpecies == "Anser brachyrhynchus") nom_long <- "Anser brachyrhynchus (pink-footed goose)"
  if(nomSpecies == "Arenaria interpres") nom_long <- "Arenaria interpres (ruddy turnstone)"
  if(nomSpecies == "Branta Canadensis") nom_long <- "Branta Canadensis (canada goose)"
  if(nomSpecies %in% c("Bubo scandiacus", "Bubo scandiaca")) nom_long <- "Bubo scandiacus (snowy owl)"
  if(nomSpecies %in% c("Calcarius lapponicus","LB")) nom_long <- "Calcarius lapponicus (lapland longspur)"
  if(nomSpecies == "Calidris alba") nom_long <- "Calidris alba (sanderling)"
  if(nomSpecies == "Calidris alpina") nom_long <- "Calidris alpina (dunlin)"
  if(nomSpecies == "Calidris canutus") nom_long <- "Calidris canutus (red knot)"
  if(nomSpecies == "Calidris maritima") nom_long <- "Calidris maritima (purple sandpiper)"
  if(nomSpecies == "Calidris melanotos") nom_long <- "Calidris melanotos (pectoral sandpiper)"
  if(nomSpecies %in% c("Carduelis flammea","RP")) nom_long <- "Carduelis flammea (common redpoll)"
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
  if(nomSpecies %in% c("Oenanthe oenanthe","NW")) nom_long <- "Oenanthe oenanthe (northern wheatear)"
  if(nomSpecies == "Pagophila eburnea") nom_long <- "Pagophila eburnea (ivory gull)"
  if(nomSpecies == "Phalacrocorax carbo") nom_long <- "Phalacrocorax carbo (great cormorant)"
  if(nomSpecies == "Phalaropus fulicarius") nom_long <- "Phalaropus fulicarius (red phalarope)"
  if(nomSpecies == "Phalaropus lobatus") nom_long <- "Phalaropus lobatus (red-necked phalarope)"
  if(nomSpecies %in% c("Plectrophenax nivalis","SB")) nom_long <- "Plectrophenax nivalis (snow bunting)"
  if(nomSpecies == "Pluvialis apricaria") nom_long <- "Pluvialis apricaria (european golden plover)"
  if(nomSpecies == "Rissa tridactyla") nom_long <- "Rissa tridactyla (black-legged kittiwake)"
  if(nomSpecies %in% c("Somateria mollissima", "Somateria mollissima borealis", "sommateria mollissima")) nom_long <- "Somateria mollissima (common eider)"
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
  if(nomSpecies == "") nom_long <- " "
  return(nom_long)
}




