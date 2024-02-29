# code/data_query.R
# Code used to query the PANGAEA database for available and relevant data
# Saves the output as a spreadsheet for use in code/data_collection.R


# Setup -------------------------------------------------------------------

# Libraries used in this script
source("code/functions.R")


# PANGAEA test queries ----------------------------------------------------

# Specific author queries
# pg_Riebesell <- pg_full_search(query = "Riebesell", bbox = c(-60, 60, 60, 90), doi_list = F)
# pg_Fransson <- pg_full_search(query = "Fransson", bbox = c(-60, 60, 60, 90), doi_list = F)
# pg_Chierici <- pg_full_search(query = "Chierici", bbox = c(-60, 60, 60, 90), doi_list = F)
# pg_Fischer <- pg_full_search(query = "Fischer", bbox = c(-60, 60, 60, 90), doi_list = F)
# pg_Bouman <- pg_full_search(query = "Bouman", bbox = c(-60, 60, 60, 90), doi_list = F)

# Test specific files
# pg_test_1 <- pg_data(doi = "10.1594/PANGAEA.857405")[[2]]$data
# pg_test_2 <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.774421")
# pg_test_3 <- pg_test_dl("10.1594/PANGAEA.774421")
# pg_test_4 <- pg_dl_prep(pg_data("10.1594/PANGAEA.896828")[[1]])
# pg_test_5 <- pg_dl_prep(pg_data("10.1594/PANGAEA.56580")[[1]])
# pg_test_6 <- pg_dl_prep(pg_data("10.1594/PANGAEA.150007")[[1]]) # Appears to work fine
# pg_test_7 <- pg_dl_prep(pg_data("10.1594/PANGAEA.400884")[[1]]) # Appears to work fine
# pg_test_8 <- pg_dl_prep(pg_data("10.1594/PANGAEA.943130")[[1]]) # NB: Still need to fix


# PANGAEA site queries ----------------------------------------------------

## All Young Sound data files - 707
# NB: The query for 'Olsen Ice Cap' returns 12 items, but none of them are for the Young Sound feature 
pg_young_bbox <- pg_full_search(query = "", bbox = c(bbox_young[1], bbox_young[3], bbox_young[2], bbox_young[4])) # 701 files
pg_young_name_1 <- pg_full_search(query = "zackenberg") # 51 files - 13 if "river" is added
pg_young_name_2 <- pg_full_search(query = "tyroler") # 0 files
pg_young_name_3 <- pg_full_search(query = "lerbugt") # 0 files
pg_young_name_4 <- pg_full_search(query = "Freya Glacier") # 35 files
pg_young_all <- rbind(pg_young_bbox, pg_young_name_1, pg_young_name_2, pg_young_name_3, pg_young_name_4) |> 
  filter(!doi %in% c("10.1594/PANGAEA.786674", # This file causes weird date issues and doesn't have any key drivers
                     "10.1594/PANGAEA.831056" # Geospatial data of mass balance for Freya Glacier
                     # "10.1594/PANGAEA.842709" # OASIS seamount database
                     )) |> 
  mutate(count = n(), file = "pg_young") |> 
  dplyr::select(doi, file, count) |> distinct()
pg_doi_list <- distinct(pg_young_all)

## All Disko Bay data files - 630
pg_disko_bbox <- pg_full_search(query = "", bbox = c(bbox_disko[1], bbox_disko[3], bbox_disko[2], bbox_disko[4])) # 622 files
pg_disko_name_1 <- pg_full_search(query = "qeqertarsuup") # 0 files
pg_disko_name_2 <- pg_full_search(query = "disko bay") # 67 files
pg_disko_name_3 <- pg_full_search(query = "disko_bay") # 57 files
pg_disko_name_4 <- pg_full_search(query = "qeqertalik") # 0 files
pg_disko_all <- rbind(pg_disko_bbox, pg_disko_name_1, pg_disko_name_2, pg_disko_name_3, pg_disko_name_4) |>
  filter(!grepl("sea level", citation)) |> 
  filter(!doi %in% c("10.1594/PANGAEA.770250", "10.1594/PANGAEA.770249")) |> # These two files are too massive
  filter(!doi %in% c("10.1594/PANGAEA.770247", "10.1594/PANGAEA.770248")) |> # These are simple bathy files
  # "10.1594/PANGAEA.847501", "10.1594/PANGAEA.905012" # These two have broken date columns that can't be reconciled
  mutate(count = n(), file = "pg_disko") |> 
  dplyr::select(doi, file, count) |> distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_disko_all))

## All Nuup Kangerlua data files - 628
pg_nuup_bbox <- pg_full_search(query = "", bbox = c(bbox_nuup[1], bbox_nuup[3], bbox_nuup[2], bbox_nuup[4])) # 504 files
pg_nuup_name_1 <- pg_full_search(query = "kangerlua") # 0 files
pg_nuup_name_2 <- pg_full_search(query = "nuuk")  # 204 files
pg_nuup_name_3 <- pg_full_search(query = "sermersooq") # 0 files
pg_nuup_all <- rbind(pg_nuup_bbox, pg_nuup_name_2, pg_nuup_name_3) |>  
  mutate(count = n(), file = "pg_nuup") |> 
  dplyr::select(doi, file, count) |> distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_nuup_all))

## All Porsangerfjorden data files - 1694
pg_por_bbox <- pg_full_search(query = "", bbox = c(bbox_por[1], bbox_por[3], bbox_por[2], bbox_por[4])) # 1693 files
pg_por_name_1 <- pg_full_search(query = "porsangerfjord") # 1 files
pg_por_name_2 <- pg_full_search(query = "porsangerfjorden") # 0 files
pg_por_all <- rbind(pg_por_bbox, pg_por_name_1, pg_por_name_2) |> 
  mutate(count = n(), file = "pg_por") |> 
  dplyr::select(doi, file, count) |> distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_por_all))

## All Storfjorden data files - 701
# Check: https://toposvalbard.npolar.no/
# Names with no hits: sonklarbreen, inglefieldbukta, inglefieldbreen, strongbreen
pg_stor_bbox <- pg_full_search(query = "", bbox = c(bbox_stor[1], bbox_stor[3], bbox_stor[2], bbox_stor[4])) # 696 files
pg_stor_name_1 <- pg_full_search(query = "storfjord") # 9 files
pg_stor_name_2 <- pg_full_search(query = "storfjorden") # 5 files
# pg_stor_name_3 <- pg_full_search(query = "") # 0 files
pg_stor_all <- rbind(pg_stor_bbox, pg_stor_name_1, pg_stor_name_2) |> 
  mutate(count = n(), file = "pg_stor") |> 
  dplyr::select(doi, file, count) |> distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_stor_all))

## All Isfjorden data files - 1835
# Check: https://toposvalbard.npolar.no/
# Names with no hits: postbreen, tunabreen
pg_is_bbox <- pg_full_search(query = "", bbox = c(bbox_is[1], bbox_is[3], bbox_is[2], bbox_is[4])) # 472 files
pg_is_name_1 <- pg_full_search(query = "isfjord") # 1 files
pg_is_name_2 <- pg_full_search(query = "isfjorden") # 8 files
pg_is_name_3 <- pg_full_search(query = "longyearbyen") # 1521 files
pg_is_name_4 <- pg_full_search(query = "adventfjorden") # 4 files
pg_is_name_5 <- pg_full_search(query = "tempelfjorden") # 2 files
pg_is_name_6 <- pg_full_search(query = "billefjorden") # 92 files
pg_is_all <- rbind(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3,
                   pg_is_name_4, pg_is_name_5, pg_is_name_6) |> 
  filter(!doi %in% c("10.1594/PANGAEA.909130", # Wide file with no date values
                     "10.1594/PANGAEA.56770", # Moss snow line data
                     "10.1594/PANGAEA.847626", "10.1594/PANGAEA.847627", # Lichen experiment datasets
                     "10.1594/PANGAEA.56770")) |> # Fern line elevation data
  mutate(count = n(), file = "pg_is") |> 
  dplyr::select(doi, file, count) |> distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_is_all[c("doi", "count", "file")]))

## All Kongsfjorden bbox data files - 2275
# Check: https://toposvalbard.npolar.no/
# Names with no hits: blomstrandbreen, kongsbreen, kongsvegen, isachsenfonna
pg_kong_bbox <- pg_full_search(query = "", bbox = c(bbox_kong[1], bbox_kong[3], bbox_kong[2], bbox_kong[4])) # 2243 files
pg_kong_name_1 <- pg_full_search(query = "kongsfjord") # 18 files
pg_kong_name_2 <- pg_full_search(query = "kongsfjorden") # 223 files
pg_kong_name_3 <- pg_full_search(query = "ny-alesund") # 2039 files # NB: same return as "ny alesund"
pg_kong_name_4 <- pg_full_search(query = "conwaybreen") # 1 files
pg_kong_name_5 <- pg_full_search(query = "kronebreen") # 2 files
pg_kong_name_6 <- pg_full_search(query = "holtedahlfonna") # 1 files
pg_kong_all <- rbind(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, 
                     pg_kong_name_4, pg_kong_name_5, pg_kong_name_6) |> 
  filter(!doi %in% c("10.1594/PANGAEA.909130")) |> # Wide file with no date values
  filter(!grepl("946961", citation)) |> # 3.7 million rows of second resolution air temperature
  mutate(count = n(), file = "pg_kong") |> 
  dplyr::select(doi, file, count) |> distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_kong_all[c("doi", "count", "file")]))

## Save DOI list - 8470
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")
rm(list = ls()[grep("pg_kong|pg_is|pg_stor|pg_young|pg_disko|pg_nuup|pg_por", ls())]); gc()


# Unused queries ----------------------------------------------------------

## All Svalbard data files - 287
# NB: These files were searched for after the specific sites intentionally
# This was so that site specific files would be allocated to the appropriate folders
# And the files not attributed to a given site would be downloaded in this chunk
# However, I'm currently thinking we don't want these data...
# pg_sval_all <- pg_full_search(query = "", bbox = c(9, 76, 30, 81)) %>%
#   filter(!doi %in% pg_doi_list$doi) %>% arrange(citation) %>% distinct()
# Svalbard team files - 10 datasets
# rm(pg_sval_all); gc()

## All Tromso data files - 6351
# pg_trom_bbox <- pg_full_search(query = "", bbox = c(bbox_trom[1], bbox_trom[3], bbox_trom[2], bbox_trom[4]))# %>% # 2775 files
# # filter(!doi %in% pg_doi_list$doi)
# pg_trom_name_1 <- pg_full_search(query = "Tromsø") %>% # 3504 files
#   filter(!doi %in% pg_trom_bbox$doi)
# pg_trom_name_2 <- pg_full_search(query = "Tromso") %>% # 72 files
#   filter(!doi %in% pg_trom_bbox$doi, !doi %in% pg_trom_name_1$doi)
# pg_trom_all <- rbind(pg_trom_bbox, pg_trom_name_1, pg_trom_name_2) %>% 
#   filter(!grepl("Multibeam survey", citation, ignore.case = T)) %>% # This removes ~7 million rows of bathy data
#   filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
#   arrange(citation) %>% distinct()
# rm(pg_trom_bbox, pg_trom_name_1, pg_trom_name_2); gc()

# Download files
# system.time(
#   pg_trom_dl <- plyr::ldply(pg_trom_all$doi, pg_dl_proc) %>% 
#     janitor::remove_empty(which = "cols")
# ) # 44 seconds
# colnames(pg_trom_dl)
# test1 <- data.frame(table(pg_tromso_dl$citation)) # Investigate which files contribute the most size
# data.table::fwrite(pg_trom_dl, "~/pCloudDrive/FACE-IT_data/tromso/pg_trom.csv")
# data.table::fwrite(pg_trom_dl, "data/pg_data/pg_por.csv")
# rm(pg_tromso_dl); gc()

