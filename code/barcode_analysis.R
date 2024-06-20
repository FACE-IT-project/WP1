# code/barcode_analysis.R
# Download and analyse BOLD and GenBank macroalgae records


# Setup -------------------------------------------------------------------

library(tidyverse) # Data manipulation
library(rgbif) # Access GBIF
library(rentrez) # Access GenBank
library(bold) # Access BOLD
library(refdb) # Access both GenBank and BOLD, but a bit wonky 

# https://cran.r-project.org/web/packages/refdb/vignettes/ncbi_bold.html

# Taxon of interest
taxon_list <- c("Bangiophyceae", "Florideophyceae", "Phaeophyceae", "Prasiolaceae", "Ulvophyceae", "Xanthophyceae")


# Download BOLD records ---------------------------------------------------

# Get all BOLD data in one go
bold_algae <- plyr::ldply(taxon_list, refdb_import_BOLD, .parallel = FALSE, ncbi_taxo = FALSE, full = TRUE)

# Save in three formats
write_csv(bold_algae, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.csv")
write_excel_csv2(bold_algae, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.xlsx")
save(bold_algae, file = "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.RData")

# Filter to rough Arctic circle
bold_algae_Arctic <- filter(bold_algae, lat >= 60)

# Save
save(bold_algae_Arctic, file = "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.RData")
write_csv(bold_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.csv")
write_excel_csv2(bold_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.xlsx")


# Download GenBank records ------------------------------------------------

# Running all of these at once is too time consuming/error prone
# Better to run and save each one individually
# ncbi_algae <- plyr::ldply(taxon_list, refdb_import_NCBI, .parallel = FALSE, full = TRUE, seq_bin = 1000)
ncbi_ban <- refdb_import_NCBI_fast(taxon_list[1])
ncbi_flo <- refdb_import_NCBI_fast(taxon_list[2])
ncbi_pha <- refdb_import_NCBI_fast(taxon_list[3])
ncbi_pra <- refdb_import_NCBI_fast(taxon_list[4])
ncbi_ulv <- refdb_import_NCBI_fast(taxon_list[5])
ncbi_xan <- refdb_import_NCBI_fast(taxon_list[6])

# Save individual files due to difficulty in downloading them
write_csv(ncbi_ban, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_ban.csv")
write_csv(ncbi_flo, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_flo.csv")
write_csv(ncbi_pha, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_pha.csv")
write_csv(ncbi_pra, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_pra.csv")
write_csv(ncbi_ulv, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_ulv.csv")
write_csv(ncbi_xan, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_xan.csv")

# Combine and save
ncbi_ban <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/ncbi_ban.csv")
ncbi_flo <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/ncbi_flo.csv")
ncbi_pha <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/ncbi_pha.csv")
ncbi_pra <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/ncbi_pra.csv")
ncbi_ulv <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/ncbi_ulv.csv")
ncbi_xan <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/ncbi_xan.csv")
ncbi_algae <- rbind(ncbi_ban, ncbi_flo, ncbi_pha, ncbi_pra, ncbi_ulv, ncbi_xan)
rm(ncbi_ban, ncbi_flo, ncbi_pha, ncbi_pra, ncbi_ulv, ncbi_xan); gc()

# Save in three formats
write_csv(ncbi_algae, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae.csv")
write_excel_csv2(ncbi_algae, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae.xlsx")
save(ncbi_algae, file = "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae.RData")

# Filter to rough Arctic circle
# NB: Roughly 80% of GenBank records have no lon/lat
# But they do have a country record, so we could use that
ncbi_algae_Arctic <- filter(ncbi_algae, latitude >= 60)

# Save
save(ncbi_algae_Arctic, file = "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.RData")
write_csv(ncbi_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.csv")
write_excel_csv2(ncbi_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.xlsx")


# Analyse -----------------------------------------------------------------

# Load files for analysis below if needed
if(!exists("bold_algae")) load("~/pCloudDrive/FACE-IT_data/barcode/bold_algae.RData")
if(!exists("ncbi_algae")) load("~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae.RData")

# Check number of records with no lon/lat coords
filter(bold_algae, is.na(lon) | is.na(lat)) |> nrow()
filter(ncbi_algae, is.na(longitude) | is.na(latitude)) |> nrow()

# Proportion of rows with no lon/lat
(filter(bold_algae, is.na(lon) | is.na(lat)) |> nrow()) / nrow(bold_algae)
(filter(ncbi_algae, is.na(longitude) | is.na(latitude)) |> nrow()) / nrow(ncbi_algae)
(filter(ncbi_algae, is.na(country_location)) |> nrow()) / nrow(ncbi_algae)

# Function for summarising records
barcode_Arctic_summary <- function(df_bar, lat_filter = 60, table_out = TRUE, unq_records = TRUE){
  
  # Remove "_name" from column names
  colnames(df_bar) <- gsub("_name", "", colnames(df_bar))
  
  # Convert to common column names
  if("longitude" %in% colnames(df_bar)){
    df_bar <- df_bar |> 
      dplyr::rename(lon = longitude, lat = latitude, country = country_location)
  }
  
  # Extract Arctic records
  df_Arctic <- df_bar |> filter(lat >= lat_filter) |>  
    filter(class %in% c("Bangiophyceae", "Florideophyceae", "Phaeophyceae", "Trebouxiophyceae", "Ulvophyceae")) |> 
    dplyr::select(phylum, class, order, family, genus, species, lon, lat, country) |> filter(!is.na(class)) |> 
    mutate(class = factor(class, levels = c("Bangiophyceae", "Florideophyceae", "Phaeophyceae", "Trebouxiophyceae", "Ulvophyceae")))
  
  # Get unique records
  df_Arctic_unq <- df_Arctic |> distinct()
  
  # Output table if desired
  if(table_out){
    df_Arctic_unq_species <- df_Arctic_unq |> 
      dplyr::select(class, species) |> distinct() |> 
      summarise(spp_count_unq = n(), .by = "class")
    df_Arctic_unq_records <- df_Arctic_unq |> distinct() |> 
      summarise(record_count_unq = n(), .by = "class")
    df_Arctic_species <- df_Arctic |> 
      dplyr::select(class, species) |>
      summarise(spp_count = n(), .by = "class")
    df_Arctic_records <- df_Arctic |> 
      dplyr::select(class) |>
      summarise(record_count = n(), .by = "class") |> 
      left_join(df_Arctic_unq_records, by = "class") |>
      left_join(df_Arctic_species, by = "class") |>
      left_join(df_Arctic_unq_species, by = "class")
    
    # Add total row to bottom of table
    df_Arctic_records_sum <- df_Arctic_records |> 
      dplyr::select(-class) |> summarise_all(~sum(., na.rm = T)) |> 
      mutate(class = "Total", .before = "record_count")
    df_Arctic_records_total <- bind_rows(df_Arctic_records, df_Arctic_records_sum)
    return(df_Arctic_records_total)
  }
  
  # Or return the data
  if(unq_records){
    return(df_Arctic_unq)
  } else {
    return(df_Arctic)
  }
  # rm(df_bar, lat_filter, table_out, df_Arctic, df_Arctic_unq, df_Arctic_unq_species, 
  # df_Arctic_unq_records, df_Arctic_species, df_Arctic_records, df_Arctic_records_sum, df_Arctic_records_total)
}

# Extract records by latitude
bold_Arctic <- barcode_Arctic_summary(bold_algae, table_out = FALSE, unq_records = FALSE)
ncbi_Arctic <- barcode_Arctic_summary(ncbi_algae, table_out = FALSE, unq_records = FALSE)
bold_high_Arctic <- barcode_Arctic_summary(bold_algae, lat_filter = 66, table_out = FALSE, unq_records = FALSE)
ncbi_high_Arctic <- barcode_Arctic_summary(ncbi_algae, lat_filter = 66, table_out = FALSE, unq_records = FALSE)

# Create tables of unique records
## Bold Arctic
bold_Arctic_records <- barcode_Arctic_summary(bold_algae, table_out = TRUE)
write_csv(bold_Arctic_records, "~/pCloudDrive/FACE-IT_data/barcode/bold_Arctic_records.csv")

## Bold high Arctic
bold_high_Arctic_records <- barcode_Arctic_summary(bold_algae, lat_filter = 66, table_out = TRUE)
write_csv(bold_high_Arctic_records, "~/pCloudDrive/FACE-IT_data/barcode/bold_high_Arctic_records.csv")

## GenBank Arctic
ncbi_Arctic_records <- barcode_Arctic_summary(ncbi_algae, table_out = TRUE)
write_csv(ncbi_Arctic_records, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_Arctic_records.csv")

## GenBank high Arctic
ncbi_high_Arctic_records <- barcode_Arctic_summary(ncbi_algae, lat_filter = 66, table_out = TRUE)
write_csv(ncbi_high_Arctic_records, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_high_Arctic_records.csv")


# Plotting ----------------------------------------------------------------

## Bold
barcode_Arctic_summary(bold_algae, table_out = FALSE, unq_records = TRUE) |> 
  ggplot(aes(x = lon, y = lat)) + borders(fill = "grey70") +
  geom_point(aes(fill = class), size = 5, shape = 21, alpha = 0.7,
             position = position_dodge(width = 5.0)) +
  annotate(geom = "label", x = -170, y = 80, label = "BOLD") +
  scale_fill_manual(values = c("red", "darkred", "chocolate", "palegreen", "forestgreen")) +
  coord_quickmap(ylim = c(59, 81), xlim = c(-172, 30)) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)", fill = NULL) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black"))
ggsave("~/pCloudDrive/FACE-IT_data/barcode/bold_Arctic_unq_map.png", height = 4, width = 9)

## GenBank
barcode_Arctic_summary(ncbi_algae, table_out = FALSE, unq_records = TRUE) |> 
  ggplot(aes(x = lon, y = lat)) + borders(fill = "grey70") +
  geom_point(aes(fill = class), size = 5, shape = 21, alpha = 0.7,
             position = position_dodge(width = 5.0)) +
  annotate(geom = "label", x = -170, y = 80, label = "GenBank") +
  scale_fill_manual(values = c("red", "darkred", "chocolate", "palegreen", "forestgreen")) +
  coord_quickmap(ylim = c(59, 81), xlim = c(-172, 30)) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)", fill = NULL) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black"))
ggsave("~/pCloudDrive/FACE-IT_data/barcode/ncbi_Arctic_unq_map.png", height = 4, width = 9)

