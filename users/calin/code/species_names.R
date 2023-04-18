# Code/species_names.R
# function to automaticly change species latin name into its latin plus english one


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


