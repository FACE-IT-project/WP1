# data_investigation.R
# This script investigates what is in the final clean dataset


# Setup -------------------------------------------------------------------

# Libraries used in this script
source("code/functions.R")


# Load data ---------------------------------------------------------------

# Load all clean data
if(!exists("clean_all")) load("data/analyses/clean_all.RData")


# Visuals -----------------------------------------------------------------

# Legacy summaries
# NB: There are isues in this code that prevent it for running for everything
# doParallel::registerDoParallel(cores = 12)
# plyr::l_ply(long_driver_names$driver, review_summary_plot, .parallel = TRUE, summary_list = clean_all, analyse = TRUE)

# Scatterplot with lm per site per driver

# Horizontal boxplots of temperature etc. by depth with box colour showing annual trend

# List of variable names. Perhaps time series or scatter plot.

# Barplot time series of complete monthly daily data. But how for depth? 

