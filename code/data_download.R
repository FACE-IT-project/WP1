# code/data_download.R
# The code used to download data directly
# This is generally for very large datasets or those with many small files


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(refdb)


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
taxon_list <- c("Bangiophyceae", "Florideophyceae", "Phaeophyceae", "Ulvophyceae", "Xanthophyceae")

# Get them all in one go
# NB: The download is extremely slow...but don't run them in parallel
bold1 <- refdb_import_BOLD("Xanthophyceae", ncbi_taxo = FALSE, full = TRUE)
ncbi1 <- refdb_import_NCBI(taxon_list[5], full = TRUE)
# NB: One of these BOLD taxa calls has an error in a column that prevents the internal rbind from working
# Running all of these at once is too time consuming/error prone
# Better to run and save each one individually
## That being said, on 2024-02-12 the full BOLD query managed to run
bold_algae <- plyr::ldply(taxon_list, refdb_import_BOLD, .parallel = FALSE, ncbi_taxo = FALSE, full = TRUE)

# The same goes for GenBank
ncbi_algae <- plyr::ldply(taxon_list, refdb_import_NCBI, .parallel = FALSE, full = TRUE, seq_bin = 1000)
ncbi_ban <- refdb_import_NCBI(taxon_list[1], full = TRUE, seq_bin = 2000)
ncbi_flo <- refdb_import_NCBI(taxon_list[1], full = TRUE, seq_bin = 2000)
ncbi_pha <- refdb_import_NCBI(taxon_list[1], full = TRUE, seq_bin = 2000)
ncbi_ulv <- refdb_import_NCBI(taxon_list[1], full = TRUE, seq_bin = 2000)
ncbi_xan <- refdb_import_NCBI(taxon_list[1], full = TRUE, seq_bin = 2000)

# Save
write_csv(bold_algae, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.csv")
write_csv(ncbi_algae, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae.csv")

# Or try a location
bold_arctic <- refdb_import_BOLD(geo = "Arctic", ncbi_taxo = FALSE)
ncbi_arctic <- refdb_import_NCBI("Arctic", full = TRUE)

# Filter to rough Arctic circle
# NB: Roughly 80% of GenBank records have no lon/lat
# But they do have a country record, so we could use that
bold_algae_Arctic <- filter(bold_algae, lat >= 60)
ncbi_algae_Arctic <- filter(ncbi_algae, latitude >= 60)

# Save
write_csv(bold_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.csv")
write_csv(ncbi_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.csv")

# Load files for analysis below if needed
if(!exists("bold_algae")) bold_algae <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/bold_algae.csv")
if(!exists("bold_algae_Arctic")) bold_algae_Arctic <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.csv")

# Check number of BOLD records with no lon/lat coords
filter(bold_algae, is.na(lon) | is.na(lat)) |> nrow()
filter(bold_algae_Arctic, lat >= 66) |> nrow()

# Extract just unique records per location
# Not currently interested in quantifying the multiples
bold_Arctic_unq <- bold_algae_Arctic |> 
  dplyr::select(phylum_name, class_name, order_name, family_name, genus_name, species_name, lon, lat, country) |> 
  distinct()
bold_high_Arctic_unq <- bold_Arctic_unq |> filter(lat >= 66)

# Create map of records
ggplot(bold_Arctic_unq, aes(x = lon, y = lat)) + 
  borders() +
  geom_point(aes(colour = class_name), size = 5, alpha = 0.7,
             position = position_dodge(width = 5.0)) +
  coord_quickmap(ylim = c(59, 81), xlim = c(-172, 30)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black"))
ggsave("~/pCloudDrive/FACE-IT_data/barcode/bold_Arctic_unq_map.png", height = 3, width = 7)
