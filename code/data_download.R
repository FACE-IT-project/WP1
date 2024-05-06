# code/data_download.R
# The code used to download data directly
# This is generally for very large datasets or those with many small files


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(rgbif) # Access GBIF
library(rentrez) # Access GenBank
library(bold) # Access BOLD
library(refdb) # Access both GenBank and BOLD, but a bit wonky 


# ERSST -------------------------------------------------------------------

# Create vector of URLs for downloading
ERSST_files <- expand_grid(year = 1854:2023, 
                           month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) |> 
  mutate(file_name = paste0("ersst.v5.",year,month,".nc"))

doParallel::registerDoParallel(cores = 15)
plyr::l_ply(ERSST_files$file_name,
            file_URL_save, .parallel = T,
            base_URL = "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/", 
            save_folder = "~/pCloudDrive/FACE-IT_data/ERSST/")


# HadCRUT.5 ---------------------------------------------------------------

# NB: As of 2024-01-04, 2023-12 data are not yet available
download.file("https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc",
              "~/pCloudDrive/FACE-IT_data/HadCRUT.5/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc")


# Barcode databases -------------------------------------------------------

# https://cran.r-project.org/web/packages/refdb/vignettes/ncbi_bold.html

# Taxon of interest
taxon_list <- c("Bangiophyceae", "Florideophyceae", "Phaeophyceae", "Prasiolaceae", "Ulvophyceae", "Xanthophyceae")


## Download --------------------------------------------------------------

# Get all BOLD data in one go
bold_algae <- plyr::ldply(taxon_list, refdb_import_BOLD, .parallel = FALSE, ncbi_taxo = FALSE, full = TRUE)

# Save in three formats
write_csv(bold_algae, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.csv")
write_excel_csv2(bold_algae, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.xlsx")
save(bold_algae, file = "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.RData")

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

# Can download by location, but not that interesting for this use case
bold_arctic <- refdb_import_BOLD(geo = "Arctic", ncbi_taxo = FALSE)
ncbi_arctic <- refdb_import_NCBI("Arctic", full = TRUE)

# Filter to rough Arctic circle
# NB: Roughly 80% of GenBank records have no lon/lat
# But they do have a country record, so we could use that
bold_algae_Arctic <- filter(bold_algae, lat >= 60)
ncbi_algae_Arctic <- filter(ncbi_algae, latitude >= 60)

# Save
save(bold_algae_Arctic, file = "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.RData")
write_csv(bold_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.csv")
write_excel_csv2(bold_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.xlsx")
save(ncbi_algae_Arctic, file = "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.RData")
write_csv(ncbi_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.csv")
write_excel_csv2(ncbi_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.xlsx")


## Analyses ---------------------------------------------------------------

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

# Records in the Arctic
filter(bold_algae, lat >= 60) |> nrow()
filter(ncbi_algae, latitude >= 60) |> nrow()

# Records in the high Arctic
filter(bold_algae, lat >= 66) |> nrow()
filter(ncbi_algae, latitude >= 66) |> nrow()

# Extract just unique records per location
# Not currently interested in quantifying the multiples
bold_Arctic_unq <- bold_algae |> filter(lat >= 60) |> 
  dplyr::select(phylum_name, class_name, order_name, family_name, genus_name, species_name, lon, lat, country) |> 
  distinct()
bold_high_Arctic_unq <- bold_Arctic_unq |> filter(lat >= 66)
ncbi_Arctic_unq <- ncbi_algae |> filter(latitude >= 60) |>  
  dplyr::select(phylum, class, order, family, genus, species, longitude, latitude, country_location) |> 
  distinct() |> filter(!is.na(class))
ncbi_high_Arctic_unq <- ncbi_Arctic_unq |> filter(latitude >= 66)

# Create map of records
ggplot(bold_Arctic_unq, aes(x = lon, y = lat)) + borders() +
  geom_point(aes(colour = class_name), size = 5, alpha = 0.7,
             position = position_dodge(width = 5.0)) +
  coord_quickmap(ylim = c(59, 81), xlim = c(-172, 30)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black"))
ggsave("~/pCloudDrive/FACE-IT_data/barcode/bold_Arctic_unq_map.png", height = 3, width = 7)

ggplot(ncbi_Arctic_unq, aes(x = longitude, y = latitude)) + borders() +
  geom_point(aes(colour = class), size = 5, alpha = 0.7,
             position = position_dodge(width = 5.0)) +
  coord_quickmap(ylim = c(59, 81), xlim = c(-172, 30)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black"))
ggsave("~/pCloudDrive/FACE-IT_data/barcode/ncbi_Arctic_unq_map.png", height = 3, width = 7)

