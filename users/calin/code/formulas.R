# Code/formulas.R
# folders with formulas use for the FACE-IT project

# Set up ------------------------------------------------------------------
library(tidyverse)


# Formulas ----------------------------------------------------------------
# Name to latin + english one
latin_eng <- function(nomSpecies){
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
  if(nomSpecies == "Anser brachyrhynchus") {"Anser brachyrhynchus (pink-footed goose)"}
  if(nomSpecies == "Arenaria interpres") {"Arenaria interpres (ruddy turnstone)"}
  if(nomSpecies == "Branta Canadensis") {"Branta Canadensis (canada goose)"}
  if(nomSpecies == "Bubo scandiacus") {"Bubo scandiacus (snowy owl)"}
  if(nomSpecies == "Calcarius lapponicus") {"Calcarius lapponicus (lapland longspur)"}
  if(nomSpecies == "Calidris alba") {"Calidris alba (sanderling)"}
  if(nomSpecies == "Calidris alpina") {"Calidris alpina (dunlin)"}
  if(nomSpecies == "Calidris canutus") {"Calidris canutus (red knot)"}
  if(nomSpecies == "Calidris maritima") {"Calidris maritima (purple sandpiper)"}
  if(nomSpecies == "Calidris melanotos") {"Calidris melanotos (pectoral sandpiper)"}
  if(nomSpecies == "Cepphus grylle") {"Cepphus grylle (black guillemot)"}
  if(nomSpecies == "Charadrius hiaticula") {"Charadrius hiaticula (common ringed plover)"}
  if(nomSpecies == "Clangula hyemalis") {"Clangula hyemalis (long-tailed duck)"}
  if(nomSpecies == "Corvus corax") {"Corvus corax (raven)"}
  if(nomSpecies == "Falco peregrinus") {"Falco peregrinus (peregrine falcon)"}
  if(nomSpecies == "Fratercula arctica") {"Fratercula arctica (atlantic puffin)"}
  if(nomSpecies == "Fulmarus glacialis") {"Fulmarus glacialis (northern fulmar)"}
  if(nomSpecies %int% c("Gavia stallata", "Gavia stellata")) {"Gavia stallata (red-throated loon)"}
  if(nomSpecies == "Histrionicus histrionicus") {"Histrionicus histrionicus (harlequin duck)"}
  if(nomSpecies %int% c("Lagopus muta", "Lagopus mutus")) {"Lagopus mutus (rock ptarmigan)"}
  if(nomSpecies == "Larus argentatus") {"Larus argentatus (herring gull)"}
  if(nomSpecies == "Larus canus") {"Larus canus (common gull)"}
  if(nomSpecies == "Larus delawarensis") {"Larus delawarensis (ring-billed gull)"}
  if(nomSpecies == "Larus fuscus") {"Larus fuscus (lesser black-backed gull)"}
  if(nomSpecies == "Larus glaucoides") {"Larus glaucoides (iceland gull)"}
  if(nomSpecies %int% c("Larus hyperboreus", "Larus hypeboreus")) {"Larus hyperboreus (glaucous gull)"}
  if(nomSpecies == "Larus marinus") {"Larus marinus (great black-backed gull)"}
  if(nomSpecies == "Larus smithonianus") {"Larus smithonianus (american herring gull)"}
  if(nomSpecies == "Larus sp.") {"Larus sp. (gull unidentified)"}
  if(nomSpecies == "Mergus serratus") {"Mergus serratus (red-breasted merganser)"}
  if(nomSpecies == "Pagophila eburnea") {"Pagophila eburnea (ivory gull)"}
  if(nomSpecies == "Phalacrocorax carbo") {"Phalacrocorax carbo (great cormorant)"}
  if(nomSpecies == "Phalaropus fulicarius") {"Phalaropus fulicarius (red phalarope)"}
  if(nomSpecies == "Phalaropus lobatus") {"Phalaropus lobatus (red-necked phalarope)"}
  if(nomSpecies == "Plectrophenax nivalis") {"Plectrophenax nivalis (snow bunting)"}
  if(nomSpecies == "Rissa tridactyla") {"Rissa tridactyla (black-legged kittiwake)"}
  if(nomSpecies %int% c("Somateria mollissima", "Somateria mollissima borealis")) {"Somateria mollissima (common eider)"}
  if(nomSpecies == "Somateria spectabilis") {"Somateria spectabilis (king eider)"}
  if(nomSpecies == "Stercorarius longicaudus") {"Stercorarius longicaudus (long-tailed jaeger)"}
  if(nomSpecies == "Stercorarius parasiticus") {"Stercorarius parasiticus (parasitic jaeger)"}
  if(nomSpecies == "Sterna paradisaea") {"Sterna paradisaea (arctic tern)"}
  if(nomSpecies == "Uria aalge") {"Uria aalge (common guillemot)"}
  if(nomSpecies == "Uria lomvia") {"Uria lomvia (brünnich’s guillemot)"}
  #Zooplankton
  if(nomSpecies == "Calanus finmarchicus") {"Calanus finmarchicus (atlantic calanus (zooplankton))"}
  if(nomSpecies == "Calanus glacialis") {"Calanus glacialis (arctic calanus (zooplankton))"}
  # Other
  if(nomSpecies == "") {NA}
}








latin <- function(nomSpecies){
  # Fish
  if(nomSpecies == "Boreogadus saida") {
    "Boreogadus saida (polar cod)"
  } else if(nomSpecies == "Clupea harengus") {
    "Clupea harengus (herring)"
  } else if(nomSpecies == "Gadus morhua") {
    "Gadus morhua (northeast arctic cod)"
  } else if(nomSpecies == "Mallotus villosus") {
    "Mallotus villosus (capelin)"
  } else if(nomSpecies == "Sebastes mentella") {
    "Sebastes mentella (beaked redfish)"
  } else if(nomSpecies == "Sebastes norvegicus") {
    "Sebastes norvegicus (golden redfish)"
  } else {"NA"}
}


test53 <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv", 
                     na = c("9999-01-01","-9999"), 
                     col_types = "iccnnDDiiicc") %>% 
  mutate (test966 <- latin(Species))
