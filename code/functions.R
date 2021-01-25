# functions.R
# This script houses functions and other preparatory things used throughout the project


# Setup -------------------------------------------------------------------

# Libraries used in all other scripts
library(tidyverse)

# Find who is the user and define the pCloud path
if (Sys.getenv("LOGNAME") == "gattuso"){
  pCloud_path = "~/pCloud\ Drive/"
} else {
  pCloud_path = "~/pCloudDrive/" 
}


# Meta-data ---------------------------------------------------------------

# The base global map
map_base <- readRDS("metadata/map_base.Rda")

