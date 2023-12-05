# expert_survey.R
# This script wrangles the feedback from the expert survey,
# then creates the necessary summary figures before
# automagically compiling the reports


# TODO: Create a report on which institutes/WPs contributed the most
# I.e. a report on the report


# Setup -------------------------------------------------------------------

source("code/functions.R")

# General sections
sections <- c("tourism", "fishery", "environment", "conclusion")

# Unique species names as a vector
# NB: These orders are very important and must not be changed
tour_unique <- c("Little Auk", "Puffin", "Walrus", "Whales", "Polar bears", "Kelp")
fish_unique <- c("Cod", "Shrimp", "Flounder + Halibut", "Pollock", "Pink Salmon", "King crab", "Snow crab", "Catfish", "Seal")
env_unique <- c("Sea ice", "Glaciers", "Pollution")


# Survey results ----------------------------------------------------------

# NB: Results from Luisa are from Luisa+Inka

# Load base results in wide format
survey_res <- read_csv("survey/Expert input.csv", name_repair = "minimal")

# Manually create sub/section columns
section_col <- rep(c(base::rep("tourism", 114), base::rep("fishery", 180), 
                     base::rep("environment", 48), base::rep("conclusion", 3)), times = 24)
sub_section_col <- rep(c(base::rep(tour_unique[1:5], each = 18), base::rep("Kelp", 24), 
                         base::rep(fish_unique, each = 20), base::rep(env_unique, each = 16),
                         base::rep("conclusion", 3)), times = 24)

# Melt
survey_long <- survey_res |> 
  pivot_longer(`Main biome`:Challenges, names_to = "question", values_to = "response") |> 
  mutate(section = section_col,
         sub_section = sub_section_col)

# Double up results from Luisa as they were also from Inka
survey_double <- filter(survey_long, Name == "Luisa Düsedau")

# Tidy it up
survey_tidy <- rbind(survey_long, survey_double) |> 
  dplyr::select(section, sub_section, question, response) |> 
  filter(!is.na(response)) |> arrange(section, sub_section)


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

