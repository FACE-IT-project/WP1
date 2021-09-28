# talks/FACE_IT_2021.R
# This script contains the code used for the analyses shown in 
# the talk given for the FACE-IT 2021 annual meeting

# TODO:
# Ask Janne about cruise ship arrival/presence data for Isfjorden
# Ask someone in Kings Bay (Geir as a first point of contact) for cruise ships in Kongsfjorden
  # Ship size and number of tourists where possible
  # This could be tracked down by getting the ship names then doing a bit of digging
# Question for Grete: Is the disappearance of sea ice promoting tourism in the two Svalbard fjords
# What may the triad relationship be between sea ice, cruise ships, and plankton biomass


# Setup -------------------------------------------------------------------

source("code/functions.R")



# Data --------------------------------------------------------------------



# Analyses ----------------------------------------------------------------

# Show what the relationship has been between ship mileage and temperature or ice change
# Then show what the different model RCP projections are and what the future may hold
# Also use these relationship projections for any sort of biomass

# TODO:
# Create statistics about how many data files there are. 
# Who has been contacted. 
# How many we have and what has been added
# Solicit audience for new datasets that have not been identified and that are available
# Read the Kongsfjorden ecosystem book chapter about mooring data
# Have a slide at the end of the science talk about co-authorship for the review article
  # It requires contribution of data, text, figures, ideas etc.
  # The obvious contributors are the site coordinators and who they think are specialists in certain aspects at there site
# Think of a timeline for the review article


# Figures -----------------------------------------------------------------

## Inglefieldbukta expanded bbox figures


## Kongsfjorden data summary figure
# Load full product
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
# Summary plot
kong_anal_fig <- data_summary_plot(full_product_kong, "Kongsfjorden")
# ggsave("docs/assets/kong_fig_1.png", kong_anal_fig, width = 8, height = 8) # Not needed


## Kongsfjorden data summary figure
# Load full product
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
# Summary plot
is_anal_fig <- data_summary_plot(full_product_is, "Isfjorden")
ggsave("docs/assets/is_fig_1.png", is_anal_fig, width = 8, height = 8)

