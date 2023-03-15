# code/data_query.R
# Code used to query the PANGAEA database for available and relevant data
# Saves the output as a spreadsheet for use in code/data_collection.R


# Setup -------------------------------------------------------------------

# Libraries used in this script
source("code/functions.R")


# PANGAEA queries ---------------------------------------------------------

### NB: The order of these queries is designed to break up their size where possible

## EU Arctic cruise Oceans data on PANGAEA - 321
pg_EU_cruise_oceans <- pg_full_search(query = "cruise", topic = "Oceans", bbox = c(-60, 60, 60, 90)) |> 
  mutate(count = n())
pg_doi_list <- distinct(data.frame(doi = pg_EU_cruise_oceans$doi, count = pg_EU_cruise_oceans$count, file = "pg_EU_cruise_oceans"))

## EU Arctic cruise Atmosphere data on PANGAEA - 271
pg_EU_cruise_atmosphere <- pg_full_search(query = "cruise", topic = "Atmosphere", bbox = c(-60, 60, 60, 90)) |> 
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_cruise_atmosphere")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_atmosphere[c("doi", "count", "file")]))

## EU Arctic cruise Cryosphere data on PANGAEA - 6
pg_EU_cruise_cryosphere <- pg_full_search(query = "cruise", topic = "Cryosphere", bbox = c(-60, 60, 60, 90)) |> 
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_cruise_cryosphere")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_cryosphere[c("doi", "count", "file")]))

## EU Arctic cruise Biological Classification data on PANGAEA - 304
pg_EU_cruise_bio_class <- pg_full_search(query = "cruise", topic = "Biological Classification", bbox = c(-60, 60, 60, 90)) |> 
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_cruise_bio_class")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_bio_class[c("doi", "count", "file")]))

## EU Arctic cruise Biosphere data on PANGAEA - 5
pg_EU_cruise_biosphere <- pg_full_search(query = "cruise", topic = "Biosphere", bbox = c(-60, 60, 60, 90)) |> 
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_cruise_biosphere")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_biosphere[c("doi", "count", "file")]))

## EU Arctic cruise Ecology data on PANGAEA - 578
pg_EU_cruise_ecology <- pg_full_search(query = "cruise", topic = "Ecology", bbox = c(-60, 60, 60, 90)) |>  
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_cruise_ecology")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_ecology[c("doi", "count", "file")]))

## EU Arctic cruise Human Dimensions data on PANGAEA - 0
pg_EU_cruise_human <- pg_full_search(query = "cruise", topic = "Human Dimensions", bbox = c(-60, 60, 60, 90)) |> 
  filter(!doi %in% pg_doi_list$doi)|> mutate(count = n(), file = "pg_EU_human")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_human[c("doi", "count", "file")]))

## EU Arctic cruise Chemistry data on PANGAEA west - 1432
pg_EU_cruise_chemistry_west <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(-60, 60, 0, 90)) |> 
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_cruise_chemistry_west")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_chemistry_west[c("doi", "count", "file")]))

## EU Arctic cruise Chemistry data on PANGAEA east - 979
pg_EU_cruise_chemistry_east <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(0, 60, 60, 90)) |> 
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_cruise_chemistry_east")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_chemistry_east[c("doi", "count", "file")]))

## EU Arctic CTD data on PANGAEA - 3040
pg_EU_CTD <- pg_full_search(query = "CTD", bbox = c(-60, 60, 60, 90)) |> 
  filter(!doi %in% pg_doi_list$doi) |> mutate(count = n(), file = "pg_EU_CTD")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_CTD[c("doi", "count", "file")]))

## All Kongsfjorden bbox data files - 3095
pg_kong_bbox <- pg_full_search(query = "", bbox = c(bbox_kong[1], bbox_kong[3], bbox_kong[2], bbox_kong[4])) |> # 3053 files
  filter(!doi %in% pg_doi_list$doi)
pg_kong_name_1 <- pg_full_search(query = "kongsfjord") |> # 7 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi)
pg_kong_name_2 <- pg_full_search(query = "kongsfjorden") |> # 23 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi)
pg_kong_name_3 <- pg_full_search(query = "ny alesund") |> # 13 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, !doi %in% pg_kong_name_2$doi)
pg_kong_name_4 <- pg_full_search(query = "ny-alesund") |> # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, 
         !doi %in% pg_kong_name_2$doi, !doi %in% pg_kong_name_3$doi)
pg_kong_all <- rbind(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4) |> 
  filter(!doi %in% c("10.1594/PANGAEA.909130")) |> # Wide file with no date values
  filter(!grepl("946961", citation)) |> # 3.7 million rows of second resolution air temperature
  mutate(count = n(), file = "pg_kong_all") |> 
  arrange(citation) |> distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_kong_all[c("doi", "count", "file")]))

## All Isfjorden data files - 1468
pg_is_bbox <- pg_full_search(query = "", bbox = c(bbox_is[1], bbox_is[3], bbox_is[2], bbox_is[4])) %>% # 383 files
  filter(!doi %in% pg_doi_list$doi)
pg_is_name_1 <- pg_full_search(query = "isfjord") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi)
pg_is_name_2 <- pg_full_search(query = "isfjorden") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi)
pg_is_name_3 <- pg_full_search(query = "longyearbyen") %>% # 1088 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi, !doi %in% pg_is_name_2$doi)
pg_is_all <- rbind(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3) %>% 
  filter(!doi %in% c("10.1594/PANGAEA.909130", # Wide file with no date values
                     "10.1594/PANGAEA.847626", "10.1594/PANGAEA.847627")) %>% # Lichen experiment datasets
  mutate(count = n(), file = "pg_is_all") |> 
  arrange(citation) %>% distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_is_all[c("doi", "count", "file")]))

## All Storfjorden data files - 200
pg_stor_bbox <- pg_full_search(query = "", bbox = c(bbox_stor[1], bbox_stor[3], bbox_stor[2], bbox_stor[4])) %>% # 181 files
  filter(!doi %in% pg_doi_list$doi)
pg_stor_name_1 <- pg_full_search(query = "storfjorden") # 9 files
pg_stor_name_2 <- pg_full_search(query = "storfjord") # 10 files
pg_stor_all <- rbind(pg_stor_bbox, pg_stor_name_1, pg_stor_name_2) %>% 
  mutate(count = n(), file = "pg_stor_all") |> 
  arrange(citation) %>% distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_stor_all[c("doi", "count", "file")]))

## All Young Sound data files - 185
pg_young_bbox <- pg_full_search(query = "", bbox = c(bbox_young[1], bbox_young[3], bbox_young[2], bbox_young[4])) %>% # 184 files
  filter(!doi %in% pg_doi_list$doi)
pg_young_name_1 <- pg_full_search(query = "zackenberg") %>% # 3 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_young_bbox$doi)
pg_young_all <- rbind(pg_young_bbox, pg_young_name_1) %>% 
  filter(!doi %in% c("10.1594/PANGAEA.786674", # This file causes weird date issues and doesn't have any key drivers
                     "10.1594/PANGAEA.831056")) %>% # Geospatial data of mass balance for Freya Glacier
  mutate(count = n(), file = "pg_young_all") |> 
  arrange(citation) %>% distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_young_all[c("doi", "count", "file")]))

## All Disko Bay data files - 236
pg_disko_bbox <- pg_full_search(query = "", bbox = c(bbox_disko[1], bbox_disko[3], bbox_disko[2], bbox_disko[4])) %>% # 320 files
  filter(!doi %in% pg_doi_list$doi)
pg_disko_name_1 <- pg_full_search(query = "Qeqertarsuup") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi)
pg_disko_name_2 <- pg_full_search(query = "disko bay") %>% # 13 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi)
pg_disko_name_3 <- pg_full_search(query = "disko_bay") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi, !doi %in% pg_disko_name_2$doi)
# pg_disko_name_4 <- pg_full_search(query = "Qeqertalik") %>% # ? files - throws an error
#   filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi, !doi %in% pg_disko_name_2$doi, !doi %in% pg_disko_name_3$doi)
pg_disko_all <- rbind(pg_disko_bbox, pg_disko_name_1, pg_disko_name_2, pg_disko_name_3) |># , pg_disko_name_4) %>%
  filter(!grepl("sea level", citation)) %>% 
  filter(!doi %in% c("10.1594/PANGAEA.770250", "10.1594/PANGAEA.770249")) %>% # These two files are too massive
  filter(!doi %in% c("10.1594/PANGAEA.770247", "10.1594/PANGAEA.770248")) %>% # These are simple bathy files
  # "10.1594/PANGAEA.847501", "10.1594/PANGAEA.905012" # These two have broken date columns that can't be reconciled
  mutate(count = n(), file = "pg_disko_all") |> 
  arrange(citation) %>% distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_disko_all[c("doi", "count", "file")]))

## All Nuup Kangerlua data files - 185
pg_nuup_bbox <- pg_full_search(query = "", bbox = c(bbox_nuup[1], bbox_nuup[3], bbox_nuup[2], bbox_nuup[4])) %>% # 142 files
  filter(!doi %in% pg_doi_list$doi)
pg_nuup_name_1 <- pg_full_search(query = "kangerlua") # 0 files
pg_nuup_name_2 <- pg_full_search(query = "nuuk") %>% # 43 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_nuup_bbox$doi)
# pg_nuup_name_3 <- pg_full_search(query = "sermersooq") %>% # ? files - throws an error
  # filter(!doi %in% pg_doi_list$doi, !doi %in% pg_nuup_bbox$doi)
pg_nuup_all <- rbind(pg_nuup_bbox, pg_nuup_name_2) %>% 
  mutate(count = n(), file = "pg_nuup_all") |> 
  arrange(citation) %>% distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_nuup_all[c("doi", "count", "file")]))

## All Porsangerfjorden data files - 56
pg_por_bbox <- pg_full_search(query = "", bbox = c(bbox_por[1], bbox_por[3], bbox_por[2], bbox_por[4])) %>% # 877 files
  filter(!doi %in% pg_doi_list$doi)
pg_por_name_1 <- pg_full_search(query = "Porsangerfjord") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_por_bbox$doi)
pg_por_name_2 <- pg_full_search(query = "Porsangerfjorden")# 0 files
pg_por_all <- rbind(pg_por_bbox, pg_por_name_1) %>% 
  mutate(count = n(), file = "pg_por_all") |> 
  arrange(citation) %>% distinct()
pg_doi_list <- distinct(rbind(pg_doi_list, pg_por_all[c("doi", "count", "file")]))

## Save DOI list - 13191
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")
rm(list = ls()[grep("pg_EU|pg_kong|pg_is|pg_stor|pg_young|pg_disko|pg_nuup|pg_por", ls())]); gc()


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

