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
tour_unique <- c("Little auk", "Puffin", "Walrus", "Whales", "Polar bears", "Kelp")
tour_code <- c("little_auk", "puffin", "walrus", "whales", "polar_bears", "kelp")
fish_unique <- c("Cod", "Shrimp", "Flounder + Halibut", "Pollock", "Pink salmon", "King crab", "Snow crab", "Catfish", "Seal")
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

# Save condensed text
survey_text <- survey_tidy |> 
  filter(section != "conclusion") |> 
  group_by(section, sub_section, question) |> 
  summarise(response = paste0(unique(response), collapse = ";"), .groups = "drop")
save(survey_text, file = "survey/reports/survey_text.RData")
load("survey/reports/survey_text.RData")

# Manually edit final text
survey_text_final <- survey_text |> 
  mutate(question = case_when(question == "Comment on main drivers (optional)" ~ "Comment on main drivers",
                              grepl("drivers affecting", question) ~ "Main drivers",
                              question == "Impact from tourism at sea (i.e. ships)" ~ "Impact from tourism at sea",
                              question == "Impact from tourism on shore (i.e. humans)" ~ "Impact from tourism on land",
                              TRUE ~ question),
                              ## Environmental category
         response = case_when(question %in% c("Glacier types present in Disko Bay",
                                              "Glacier types present in Isfjorden",
                                              "Glacier types present in Nuup Kangerlua") ~ "Land+marine terminating",
                              sub_section == "Glaciers" & question %in% c("Impact from tourism at sea",
                                                                          "Impact from tourism on land") ~ "Low",
                              sub_section == "Glaciers" & question == "Main drivers" ~ "Sea ice;Terrestrial runoff;Seawater temperature",
                              sub_section %in% c("Glaciers", "Pollution", "Sea ice", "Cod") & question == "Other..." ~ as.character(NA),
                              sub_section == "Pollution" & question == "Impact from tourism at sea" ~ "Medium + increasing",
                              sub_section == "Pollution" & question == "Impact from tourism on land" ~ "High + increasing",
                              sub_section == "Pollution" & question == "Impact on tourism" ~ "High",
                              sub_section == "Pollution" & question == "Main drivers" ~ "Tourism;Fisheries;Terrestrial runoff",
                              sub_section == "Sea ice" & question == "Impact from tourism at sea" ~ "Low / High from ships",
                              sub_section == "Sea ice" & question == "Impact from tourism on land" ~ "Low",
                              sub_section == "Sea ice" & question == "Main drivers" ~ "Seawater temperature",
                              sub_section == "Sea ice" & question == "Extent in Isfjorden" ~ "20 and 40%",
                              sub_section == "Sea ice" & question == "Touristic value" ~ "High",
                              ## Fishery category
                              sub_section == "Cod" & question == "Catch trend in Isfjorden" ~ "Increasing",
                              sub_section %in% c("Cod", "Flounder + Halibut", "Pollock", "Shrimp", "Snow crab") & 
                                question == "Landing amount coming from Isfjorden" ~ as.character(NA),
                              sub_section == "Cod" & question == "Main drivers" ~ "Seawater temperature;Fisheries;Sea ice",
                              sub_section == "Flounder + Halibut" & question == "Catch trend in Isfjorden" ~ "Stable",
                              sub_section == "King crab" & question == "Catch trend in Isfjorden" ~ "Unknown",
                              sub_section == "Pink salmon" & question == "Catch trend in Isfjorden" ~ "Increasing",
                              sub_section == "Pink salmon" & question %in% c("Effect of atlantification",
                                                                             "Effect of retreating sea ice") ~ "Positive",
                              sub_section == "Pink salmon" & question %in% c("Effect of retreating glaciers") ~ "Neutral",
                              sub_section == "Pink salmon" & question %in% c("Main biome") ~ "North Pacific/Atlantic",
                              sub_section == "Seal" & question %in% c("Effect of atlantification",
                                                                      "Effect of retreating glaciers",
                                                                      "Effect of retreating sea ice") ~ "Negative",
                              sub_section == "Seal" & question %in% c("Main biome") ~ "Arctic",
                              sub_section == "Seal" & question == "Main drivers" ~ "Sea ice;Seawater temperature",
                              sub_section == "Seal" & question == "Source of knowledge" ~ "Expert knowledge",
                              sub_section == "Shrimp" & question == "Catch trend in Isfjorden" ~ "Stable",
                              sub_section == "Snow crab" & question == "Catch trend in Isfjorden" ~ "Increasing",
                              sub_section == "Snow crab" & question == "Main drivers" ~ "Fisheries;Seawater temperature",
                              sub_section == "Kelp" & question %in% c("Effect of atlantification",
                                                                      "Effect of retreating glaciers",
                                                                      "Effect of retreating sea ice") ~ "Positive + Negative",
                              sub_section == "Kelp" & question %in% c("Impact from tourism at sea",
                                                                      "Impact from tourism on land") ~ "Low",
                              sub_section == "Kelp" & question %in% c("Main biome") ~ "Temperate to Arctic",
                              sub_section == "Kelp" & question == "Main drivers" ~ "Seawater temperature;Sea ice;Light",
                              sub_section == "Kelp" & grepl("Population size in ", question) ~ as.character(NA),
                              sub_section == "Kelp" & grepl("Population trend in ", question) ~ "Increasing",
                              sub_section == "Kelp" & question == "Touristic value" ~ "Low (but could be high)",
                              sub_section %in% c("Little auk", "Puffin") & question %in% c("Impact from tourism at sea",
                                                                                           "Impact from tourism on land") ~ "Medium",
                              sub_section == "Little auk" & question %in% c("Main biome") ~ "Arctic",
                              sub_section == "Little auk" & question == "Main drivers" ~ "Sea ice;Seawater temperature;Nutrients",
                              sub_section %in% c("Little auk", "Puffin", "Walrus", "Whales") & 
                                question %in% c("Population size in Disko Bay",
                                                "Population trend in Disko Bay",
                                                "Population trend in Nuup Kangerlua",
                                                "Population trend in Porsangerfjorden") ~ as.character(NA),
                              sub_section == "Polar bears" & question %in% c("Impact from tourism at sea",
                                                                            "Impact from tourism on land") ~ "Medium",
                              sub_section == "Polar bears" & question == "Main drivers" ~ "Sea ice;Seawater temperature;Governance",
                              sub_section == "Puffin" & question == "Main drivers" ~ "Seawater temperature;Sea ice;Light",
                              sub_section == "Puffin" & question == "Touristic value" ~ "High",
                              sub_section == "Walrus" & question == "Main drivers" ~ "Sea ice;Seawater temperature;Nutrients",
                              sub_section == "Whales" & question == "Impact from tourism at sea" ~ "Medium",
                              sub_section == "Whales" & question == "Impact from tourism on land" ~ "Low",
                              sub_section == "Whales" & question %in% c("Main biome") ~ "North Atlantic - Arctic",
                              sub_section == "Whales" & question == "Main drivers" ~ "Sea ice;Primary production;Fisheries",
                              TRUE ~ response)) |> 
  group_by(section, sub_section, question) |>
  mutate(response = paste0(unique(unlist(str_split(response, ";"))), collapse = ", ")) |>
  ungroup() |>
  filter(!grepl("only for tidewater|Fjord settings are relevant|Fishery is probably the main|
                |and hence terrestrial runoff; sea ice|expertise for regarding the fishery|
                |via the food chain|define are NOT relevant|the areas you define are", response)) |> 
  filter(!is.na(response), response != "NA")
save(survey_text_final, file = "survey/reports/survey_text_final.RData")
write_csv(survey_text_final, "survey/reports/survey_text_final.csv")
load("survey/reports/survey_text_final.RData")

# Save quotes
survey_quotes <- survey_tidy |> 
  filter(section == "conclusion")
save(survey_quotes, file = "survey/reports/survey_quotes.RData")
load("survey/reports/survey_quotes.RData")


# Figures -----------------------------------------------------------------

# Map
base_map <- basemap(limits = c(-50, 35, 60, 80), bathymetry = TRUE, bathy.style = "raster_binned_grays") +
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = c(0.221, 0.07),
        legend.direction = "horizontal",
        # legend.position = "bottom",
        axis.text = element_text(colour = "black"),
        # legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(10, 10, 10, 10),
        legend.box.background = element_rect(fill = "white", colour = "black"))
base_map
ggsave("survey/reports/figures/base_map.png", plot = base_map, height = 8, width = 12)

# Extract main drivers per item
# Get counts/votes; determine top 3
# Get data for relevant drivers by site
# Create small time series plots

# NB: These files intentionally do not get pushed to GitHub because of .gitignore
# Rather run this code to generate the figures locally.
doParallel::registerDoParallel(cores = 7)
plyr::l_ply(unique(fish_dist_coords$Species.Name), range_map_func, .parallel = T)


# Reports -----------------------------------------------------------------

# Reports compiled manually via endNote

