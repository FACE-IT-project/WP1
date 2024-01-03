# code/data_download.R
# The code used to download data directly
# This is generally for very large datasets or those with many small files


# Setup -------------------------------------------------------------------

source("code/functions.R")


# ERSST -------------------------------------------------------------------

# Create vector of URLs for downloading
ERSST_files <- expand_grid(year = 1854:2023, 
                           month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) |> 
  mutate(file_name = paste0("ersst.v5.",year,month,".nc"))

doParallel::registerDoParallel(cores = 15)
plyr::l_ply(ERSST_files$file_name, # NB: AS of 2024-01-03 December 2023 data were not yet available 
            file_URL_save, .parallel = T,
            base_URL = "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/", 
            save_folder = "~/pCloudDrive/FACE-IT_data/ERSST/")

