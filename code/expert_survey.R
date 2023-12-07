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
tour_code <- c("little_auk", "puffin", "walrus", "whales", "polar_bears", "kelp")
fish_unique <- c("Cod", "Shrimp", "Flounder + Halibut", "Pollock", "Pink Salmon", "King crab", "Snow crab", "Catfish", "Seal")
fish_code <- c("cod", "shrimp", "flounder_halibut", "pollock", "pink_salmon", "king_crab", "snow_crab", "catfish", "seal")
env_unique <- c("Sea ice", "Glaciers", "Pollution")
env_code <- c("sea_ice", "glaciers", "pollution")
all_unique <- c(tour_unique, fish_unique, env_unique)
all_code <- c(tour_code, fish_code, env_code)

# Create data.frame
all_df <- data.frame(index_no = 1:length(all_unique),
                     cat_name = c(rep(sections[1], 6), rep(sections[2], 9), rep(sections[3], 3)),
                     item_name = all_unique,
                     item_code = all_code)
save(all_df, file = "survey/reports/all_df.RData")


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


# Text --------------------------------------------------------------------

# text_vec <- survey_text_final$response[8]
unique_vec <- function(text_vec){
  # split_vec <- str_split(text_vec, ";")
  # unique_vec <- unique(unlist(split_vec))
  # res_vec <- paste0(unique_vec, collapse = ", ")
  res_vec <- paste0(unique(unlist(str_split(text_vec, ";"))), collapse = ", ")
  # return(res_vec)
  # rm(text_vec, split_vec, unique_vec, res_vec)
}

# Save condensed text
survey_text <- survey_tidy |> 
  filter(section != "conclusion") |> 
  group_by(section, sub_section, question) |> 
  summarise(response = paste0(unique(response), collapse = ";"), .groups = "drop") |> 
save(survey_text, file = "survey/reports/survey_text.RData")
load("survey/reports/survey_text.RData")

# Manually edit final text
survey_text_final <- survey_text |> 
  mutate(question = case_when(question == "Comment on main drivers (optional)" ~ "Comment on main drivers",
                              grepl("drivers affecting", question) ~ "Main drivers",
                              question == "Impact from tourism at sea (i.e. ships)" ~ "Impact from tourism at sea",
                              question == "Impact from tourism on shore (i.e. humans)" ~ "Impact from tourism on land",
                              TRUE ~ question),
         response = case_when(question %in% c("Glacier types present in Disko Bay",
                                              "Glacier types present in Isfjorden",
                                              "Glacier types present in Nuup Kangerlua") ~ "Land+marine terminating",
                              sub_section == "Glaciers" & question == "Impact from tourism at sea" ~ "Low",
                              sub_section == "Glaciers" & question == "Impact from tourism on land" ~ "Low",
                              sub_section == "Glaciers" & question == "Main drivers" ~ "Sea ice;Terrestrial runoff;Seawater temperature",
                              sub_section == "Glaciers" & question == "Other..." ~ as.character(NA),
                              sub_section == "Pollution" & question == "Impact from tourism at sea" ~ "High + increasing",
                              sub_section == "Pollution" & question == "Impact from tourism on land" ~ "Low + increasing",
                              sub_section == "Pollution" & question == "Impact on tourism" ~ "High",
                              TRUE ~ response)) |> 
  group_by(section, sub_section, question) |> 
  # mutate(response = unique_vec(response)) |>
  mutate(response = paste0(unique(unlist(str_split(response, ";"))), collapse = ", ")) |> 
  ungroup()# |> 
  filter(!grepl("only for tidewater", response)) |> 
  filter(!is.na(response))

# Save quotes
survey_quotes <- survey_tidy |> 
  filter(section == "conclusion")
save(survey_quotes, file = "survey/reports/survey_quotes.RData")
load("survey/reports/survey_quotes.RData")

## Fishery

# Main biome (include after name)

# Effect of retreating sea ice
# Effect of retreating glaciers
# Effect of Atlantification

# Main drivers

# Presence in which fjord

# Catch trends per fjord

## Tourism

# Main drivers


## Environment


# Figures -----------------------------------------------------------------

# Extract main drivers per item
# Get counts/votes; determine top 3
# Get data for relevant drivers by site
# Create small time series plots

# NB: These files intentionally do not get pushed to GitHub because of .gitignore
# Rather run this code to generate the figures locally.
doParallel::registerDoParallel(cores = 7)
plyr::l_ply(unique(fish_dist_coords$Species.Name), range_map_func, .parallel = T)


# Reports -----------------------------------------------------------------

setwd("survey/reports/")

quarto::quarto_render(input = "input.qmd")

quarto::quarto_render(
  input = "input.qmd",
  execute_params = list("item_code" = all_code[1]),
  output_file = paste0(all_code[1],".pdf")
)
#
purrr::walk(all_code, ~quarto::quarto_render(
  input = "input.qmd",
  execute_params = list("item_code" = .x),
  output_file = paste0(.x,".pdf")
))
#

