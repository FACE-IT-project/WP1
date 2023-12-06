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
all_unique <- c(tour_unique, fish_unique, env_unique)

# Create data.frame
all_df <- data.frame(index_no = 1:length(all_unique),
                     cat_name = c(rep(sections[1], 6), rep(sections[2], 9), rep(sections[3], 3)),
                     item_name = all_unique)


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

setwd("survey/reports/")

quarto::quarto_render(input = "input.qmd")

quarto::quarto_render(input = "input.qmd",
                      execute_params = list("cat_name" = 2021,
                                            "item_name" = "Alabama"))

## Just do the item name, can subset the other things within the document
## Rather give the title as a text body, not the YAML title
## Determine the zones within which things will be placed
## Add place holders and see how it looks
## Ned to Google how to put document in landscape and add items with exact spacing
## Once a basic outline is good then start adding pictures for each item
## Then start on the widgets
## Send a demo to Greta as soon as possible via slack

quarto::quarto_render(
  input = "input.qmd",
  execute_params = list("item_name" = all_unique[1]),
  output_file = paste0(all_unique[1], '.html')
  )

purrr::walk(all_unique[1], ~quarto::quarto_render(
  input = "survey/reports/input.qmd",
  execute_params = list("set_item" = .x),
  output_file = paste0(.x, '.html')
))

# for (item_name in all_unique[1]) {
#   quarto::quarto_render(
#     "survey/reports/input.Rmd",
#     execute_params = list("set_item" = item_name),
#     output_file = paste0(item_name, '.html')
#     )
#   # rmarkdown::render(
#   #   "survey/reports/input.Rmd", output_file = paste0(item_name, '.html')
#   # )
# }

