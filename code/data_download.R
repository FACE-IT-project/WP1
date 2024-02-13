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
bold1 <- refdb_import_BOLD("Xanthophyceae", ncbi_taxo = FALSE)
# NB: One of these BOLD taxa calls has an error in a column that prevents the internal rbind from working
# Running all of these at once is too time consuming/error prone
# Better to run and save each one individually
bold_algae <- plyr::ldply(taxon_list, refdb_import_BOLD, .parallel = FALSE, ncbi_taxo = FALSE)
ncbi_algae <- plyr::ldply(taxon_list, refdb_import_NCBI, .parallel = FALSE)

# Filter to rough Arctic circle
# NB: Roughly 80% of GenBank records have no lon/lat
# But they do have a country record, so we could use that
bold_algae_Arctic <- filter(bold_algae, lat >= 60)
ncbi_algae_Arctic <- filter(ncbi_algae, latitude >= 60)

# Save
write_csv(bold_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.csv")
write_csv(ncbi_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.csv")

