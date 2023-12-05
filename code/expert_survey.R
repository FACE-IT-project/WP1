# expert_survey.R
# This script wrangles the feedback from the expert survey,
# then creates the necessary summary figures before
# automagically compiling the reports


# Setup -------------------------------------------------------------------

source("code/functions.R")

# Unique species names as a vector
spp_unique <- unique(fish_dist_coords$Species.Name[order(fish_dist_coords$Species.Name)])


# Survey results ----------------------------------------------------------

survey_res <- read_csv("survey/Expert input.csv")


# Figures -----------------------------------------------------------------

# NB: These files intentionally do not get pushed to GitHub because of .gitignore
# Rather run this code to generate the figures locally.
doParallel::registerDoParallel(cores = 7)
plyr::l_ply(unique(fish_dist_coords$Species.Name), range_map_func, .parallel = T)


# Reports -----------------------------------------------------------------

for (spp_name in spp_unique) {
  rmarkdown::render(
    'reports/input.Rmd', output_file = paste0(spp_name, '.html')
  )
}

