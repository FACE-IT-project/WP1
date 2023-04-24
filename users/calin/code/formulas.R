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
  if(nomSpecies == "Odobenus marinus") nom_long <- "|MAM| Odobenus marinus (walrus)"
  if(nomSpecies == "Pagophilus groenlandicus") nom_long <- "|MAM| Pagophilus groenlandicus (harp seal)"
  if(nomSpecies == "Ursus maritimus") nom_long <- "|MAM| Ursus maritimus (polar bear)"
  # Bird |BIR|
  if(nomSpecies == "Acanthis hornemanni") nom_long <- "|BIR| Acanthis hornemanni(arctic redpoll)"
  if(nomSpecies == "Alca torda") nom_long <- "|BIR| Alca torda (razorbill)"
  if(nomSpecies == "Anas platyrhynchos") nom_long <- "|BIR| Anas platyrhynchos (mallard)"
  if(nomSpecies == "Anser brachyrhynchus") nom_long <- "|BIR| Anser brachyrhynchus (pink-footed goose)"
  if(nomSpecies == "Arenaria interpres") nom_long <- "|BIR| Arenaria interpres (ruddy turnstone)"
  if(nomSpecies == "Branta Canadensis") nom_long <- "|BIR| Branta Canadensis (canada goose)"
  if(nomSpecies %in% c("Bubo scandiacus", "Bubo scandiaca")) nom_long <- "|BIR| Bubo scandiacus (snowy owl)"
  if(nomSpecies %in% c("Calcarius lapponicus","LB")) nom_long <- "|BIR| Calcarius lapponicus (lapland longspur)"
  if(nomSpecies == "Calidris alba") nom_long <- "|BIR| Calidris alba (sanderling)"
  if(nomSpecies == "Calidris alpina") nom_long <- "|BIR| Calidris alpina (dunlin)"
  if(nomSpecies == "Calidris canutus") nom_long <- "|BIR| Calidris canutus (red knot)"
  if(nomSpecies == "Calidris maritima") nom_long <- "|BIR| Calidris maritima (purple sandpiper)"
  if(nomSpecies == "Calidris melanotos") nom_long <- "|BIR| Calidris melanotos (pectoral sandpiper)"
  if(nomSpecies %in% c("Carduelis flammea","RP")) nom_long <- "|BIR| Carduelis flammea (common redpoll)"
  if(nomSpecies == "Cepphus grylle") nom_long <- "|BIR| Cepphus grylle (black guillemot)"
  if(nomSpecies == "Charadrius hiaticula") nom_long <- "|BIR| Charadrius hiaticula (common ringed plover)"
  if(nomSpecies == "Clangula hyemalis") nom_long <- "|BIR| Clangula hyemalis (long-tailed duck)"
  if(nomSpecies == "Corvus corax") nom_long <- "|BIR| Corvus corax (raven)"
  if(nomSpecies == "Falco peregrinus") nom_long <- "|BIR| Falco peregrinus (peregrine falcon)"
  if(nomSpecies == "Fratercula arctica") nom_long <- "|BIR| Fratercula arctica (atlantic puffin)"
  if(nomSpecies == "Fulmarus glacialis") nom_long <- "|BIR| Fulmarus glacialis (northern fulmar)"
  if(nomSpecies %in% c("Gavia stallata", "Gavia stellata")) nom_long <- "|BIR| Gavia stallata (red-throated loon)"
  if(nomSpecies == "Histrionicus histrionicus") nom_long <- "|BIR| Histrionicus histrionicus (harlequin duck)"
  if(nomSpecies %in% c("Lagopus muta", "Lagopus mutus")) nom_long <- "|BIR| Lagopus mutus (rock ptarmigan)"
  if(nomSpecies == "Larus argentatus") nom_long <- "|BIR| Larus argentatus (herring gull)"
  if(nomSpecies == "Larus canus") nom_long <- "|BIR| Larus canus (common gull)"
  if(nomSpecies == "Larus delawarensis") nom_long <- "|BIR| Larus delawarensis (ring-billed gull)"
  if(nomSpecies == "Larus fuscus") nom_long <- "|BIR| Larus fuscus (lesser black-backed gull)"
  if(nomSpecies == "Larus glaucoides") nom_long <- "|BIR| Larus glaucoides (iceland gull)"
  if(nomSpecies %in% c("Larus hyperboreus", "Larus hypeboreus", "GLGU")) nom_long <- "|BIR| Larus hyperboreus (glaucous gull)"
  if(nomSpecies == "Larus marinus") nom_long <- "|BIR| Larus marinus (great black-backed gull)"
  if(nomSpecies == "Larus smithonianus") nom_long <- "|BIR| Larus smithonianus (american herring gull)"
  if(nomSpecies == "Larus sp.") nom_long <- "|BIR| Larus sp. (gull unidentified)"
  if(nomSpecies == "Mergus serratus") nom_long <- "|BIR| Mergus serratus (red-breasted merganser)"
  if(nomSpecies %in% c("Oenanthe oenanthe","NW")) nom_long <- "|BIR| Oenanthe oenanthe (northern wheatear)"
  if(nomSpecies == "Pagophila eburnea") nom_long <- "|BIR| Pagophila eburnea (ivory gull)"
  if(nomSpecies == "Phalacrocorax carbo") nom_long <- "|BIR| Phalacrocorax carbo (great cormorant)"
  if(nomSpecies == "Phalaropus fulicarius") nom_long <- "|BIR| Phalaropus fulicarius (red phalarope)"
  if(nomSpecies == "Phalaropus lobatus") nom_long <- "|BIR| Phalaropus lobatus (red-necked phalarope)"
  if(nomSpecies %in% c("Plectrophenax nivalis","SB")) nom_long <- "|BIR| Plectrophenax nivalis (snow bunting)"
  if(nomSpecies == "Pluvialis apricaria") nom_long <- "|BIR| Pluvialis apricaria (european golden plover)"
  if(nomSpecies == "Rissa tridactyla") nom_long <- "|BIR| Rissa tridactyla (black-legged kittiwake)"
  if(nomSpecies %in% c("Somateria mollissima", "Somateria mollissima borealis", "Sommateria mollissima")) nom_long <- "|BIR| Somateria mollissima (common eider)"
  if(nomSpecies == "Somateria spectabilis") nom_long <- "|BIR| Somateria spectabilis (king eider)"
  if(nomSpecies == "Stercorarius longicaudus") nom_long <- "|BIR| Stercorarius longicaudus (long-tailed jaeger)"
  if(nomSpecies == "Stercorarius parasiticus") nom_long <- "|BIR| Stercorarius parasiticus (parasitic jaeger)"
  if(nomSpecies == "Sterna paradisaea") nom_long <- "|BIR| Sterna paradisaea (arctic tern)"
  if(nomSpecies == "Uria aalge") nom_long <- "|BIR| Uria aalge (common guillemot)"
  if(nomSpecies == "Uria lomvia") nom_long <- "|BIR| Uria lomvia (brünnich’s guillemot)"
  # Zooplankton |ZOO|
  if(nomSpecies %in% c("Calanus finmarchicus","atlantic species")) nom_long <- "|ZOO| Calanus finmarchicus (atlantic calanus)"
  if(nomSpecies %in% c("Calanus glacialis","arctic species")) nom_long <- "|ZOO| Calanus glacialis (arctic calanus)"
  # Other
  if(nomSpecies == "") nom_long <- " "
  return(nom_long)
}




