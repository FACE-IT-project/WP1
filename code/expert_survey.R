# expert_survey.R
# This script wrangles the feedback from the expert survey,
# then creates the necessary summary figures before
# automagically compiling the reports


# TODO: Create a report on which institutes/WPs contributed the most
# I.e. a report on the report


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(ggspatial)

# Load pre-processed PAR data
if(!exists("PAR_annual_summary")) load("survey/reports/PAR_annual_summary.RData")
PAR_sub <- PAR_annual_summary |> 
  filter(variable == "YearlyPAR0m", site %in% c("por", "is", "nuup", "disko")) |> 
  left_join(long_site_names, by = "site") |> 
  mutate(driver = "light", variable = "Surface PAR [mol/m^2/d]", 
         date = as.Date(paste0(year,"-01-01")), value = `mean`) |> 
  dplyr::select(site_long, driver, variable, date, value)

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
                              sub_section %in% c("Little auk", "Puffin", "Polar bears", "Walrus", "Whales") & 
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
write_csv(survey_quotes, "survey/reports/survey_quotes.csv")
load("survey/reports/survey_quotes.RData")


# Figures -----------------------------------------------------------------

# Map
load("data/analyses/sst_EU_arctic_annual_trends.RData")
base_map <- basemap(limits = c(-50, 35, 60, 80), bathymetry = FALSE) +
  # geom_spatial_tile(data = filter(sst_EU_arctic_annual_trends,
  #                                 lat >= 60, lat <= 61, lon >= 1, lon <= 2),
  geom_spatial_tile(data = sst_EU_arctic_annual_trends,
                    crs = 4326, colour = NA,
                    aes(fill = trend*10, x = lon, y = lat)) +
  scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
  labs(fill = "Trend\n[°C/dec]", x  = NULL, y = NULL) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = c(0.207, 0.135),
        legend.direction = "horizontal",
        # legend.position = "bottom",
        axis.text = element_text(colour = "black"),
        # legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(10, 10, 10, 10),
        legend.box.background = element_rect(fill = "white", colour = "black"))
base_map$layers <- base_map$layers[c(2,1)] # Reorder land shape and SST rasters
# base_map
ggsave("survey/reports/figures/base_map.png", plot = base_map, height = 4, width = 6)

# Extract main drivers per item
# Get counts/votes; determine top 3
# Get data for relevant drivers by site
# Create small time series plots
# item_choice <- "little_auk"
report_driver_plot <- function(item_choice){
  
  # Prep data
  if(!exists("survey_text_final")) load("survey/reports/survey_text_final.RData")
  item_df <- filter(all_df, item_code == item_choice)
  item_text <- filter(survey_text_final, sub_section == item_df$item_name)
  
  # Get sites listed for given item
  item_presence <- filter(item_text, grepl("presence in...|Present in...|breeding in...", question))
  item_sites <- c()
  if(any(grepl("Isfjorden", item_text$question))) item_sites <- c(item_sites, "Isfjorden")
  if(any(grepl("Porsangerfjorden", item_text$question))) item_sites <- c(item_sites, "Porsangerfjorden")
  if(any(grepl("Disko Bay", item_text$question))) item_sites <- c(item_sites, "Disko Bay")
  if(any(grepl("Nuup Kangerlua", item_text$question))) item_sites <- c(item_sites, "Nuup Kangerlua")
  if(nrow(item_presence) > 0) item_sites <- c(item_sites, item_presence$response)
  item_sites <- unlist(unique(str_split(tools::toTitleCase(item_sites), ", ")))
  item_sites_short <- long_site_names[which(long_site_names$site_long %in% item_sites),]
  
  # Get main drivers
  ## "sea ice"   "runoff"    "sea temp"  "light"     "nutrients" "prim prod" "gov"       "tourism"   "fisheries"
  item_drivers <- filter(item_text, question == "Main drivers")
  item_drivers <- unlist(unique(str_split(tolower(item_drivers$response), ", ")))
  item_drivers_short <- long_driver_names[which(long_driver_names$driver_long %in% item_drivers),]
  
  # Filter clean data accordingly
  # item_sites_short$site
  if(!exists("clean_all")) load("~/pCloudDrive/FACE-IT_data/clean_all.RData")
  clean_sub <- filter(clean_all, site %in% c("por", "is", "nuup", "disko"),
                      driver %in% item_drivers_short$driver) |>
                      # driver %in% c("sea temp", "sea ice", "runoff", "light", "nutrients", "prim prod", "tourism", "fisheries", "gov")) |> 
    mutate(value = case_when(driver == "sea ice" & variable != "sea ice cover [proportion]" ~ NA,
                             driver == "sea temp" & type != "OISST" ~ NA,
                             driver == "runoff" & variable == "Q day mean [m**3/s]" ~ NA,
                             driver == "light" ~ NA, # Rather use FjordLight data
                             driver == "nutrients" & variable %in% c("PO4 biog [%]", "[NO3]- [mg/l]", 
                                                                     "N-[NO3]- [mg/l]", "[NO3]- [µg/kg]") ~ NA,
                             driver == "prim prod" & !(variable %in% c("Chla [µg/l]", "Chl a [mg/m**3]", "")) ~ NA,
                             TRUE ~ value)) |> 
    mutate(variable = case_when(variable %in% c("Chla [µg/l]", "Chl a [mg/m**3]") ~ "Chl a [µg/l]", TRUE ~ variable)) |> 
    filter(!is.na(value), !is.na(date)) |> 
    left_join(long_site_names, by = "site") |> 
    mutate(date = round_date(date, unit = "year")) |> 
    summarise(value = mean(value, na.rm = TRUE), .by = c("site_long", "driver", "variable", "date")) |> 
    rbind(PAR_sub)
    
  # Create static site colour palette
  # brewer.pal(brewer.pal.info["Set1", "maxcolors"], "Set1")
  site_colours <- c(
    "Porsangerfjorden" = "#4DAF4A",
    "Isfjorden" = "#377EB8", 
    "Qeqertarsuup Tunua" = "#984EA3", 
    "Nuup Kangerlua" = "#FF7F00"
  )
  
  # Create theme
  theme_trip <- function(){
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14), 
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          # axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "bottom",
          panel.border = element_rect(fill = NA, colour = "black")) 
  }
  
  # The primary plots
  var_plot_line <- function(var_short, y_lab){
    plot_var <- filter(clean_sub, driver == var_short)  |> 
      ggplot(aes(x = date, y = value, colour = site_long)) +
      geom_line(size = 1.5) + 
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", show.legend = FALSE) + 
      scale_colour_manual("Site", values = site_colours) +
      labs(y = y_lab) + theme_trip()
  }
  var_plot_point <- function(var_short, y_lab){
    plot_var <- filter(clean_sub, driver == var_short)  |> 
      ggplot(aes(x = date, y = value, colour = site_long)) +
      geom_point(size = 3) + 
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", show.legend = FALSE) + 
      scale_colour_manual("Site", values = site_colours) +
      labs(y = y_lab) + theme_trip()
  }
  var_plot_point_many <- function(var_short, y_lab){
    plot_var <- filter(clean_sub, driver == var_short)  |> 
      ggplot(aes(x = date, y = value, colour = site_long)) +
      geom_point(aes(shape = variable), size = 2) + 
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", show.legend = FALSE) + 
      scale_colour_manual("Site", values = site_colours) +
      labs(y = y_lab) + theme_trip()
  }
  var_plot_point_facet <- function(var_short, y_lab){
    plot_var <- filter(clean_sub, driver == var_short)  |> 
      ggplot(aes(x = date, y = value, colour = site_long)) +
      geom_point(size = 2) + 
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", show.legend = FALSE) + 
      scale_colour_manual("Site", values = site_colours) +
      facet_wrap(~variable, scales = "free_y") + labs(y = y_lab) + theme_trip()
  }
  
  # Get specific variables
  # list_vars <- clean_sub |> dplyr::select(driver, variable) |> distinct()
  ## "sea temp", "sea ice", "runoff", "light", "nutrients", "prim prod", "tourism", "fisheries", "gov"
  plot_list <- list()
  if("sea temp" %in% item_drivers_short$driver){
    plot_temp <- var_plot_line("sea temp", "Seawater temperature [°C]")
    plot_list[[length(plot_list)+1]] <- plot_temp
  }
  if("sea ice" %in% item_drivers_short$driver){
    plot_ice <- var_plot_line("sea ice", "Sea ice cover [proportion]")
    plot_list[[length(plot_list)+1]] <- plot_ice
  }
  if("runoff" %in% item_drivers_short$driver){
    plot_runoff <- var_plot_line("runoff", "Terrestrial runoff [m^3/s]")
    plot_list[[length(plot_list)+1]] <- plot_runoff
  }
  if("light" %in% item_drivers_short$driver){
    plot_light <- var_plot_line("light", "Surface PAR [mol/m^2/d]")
    plot_list[[length(plot_list)+1]] <- plot_light
  }
  if("nutrients" %in% item_drivers_short$driver){
    plot_nutr <- var_plot_point_facet("nutrients", "Nutrients [µmol/l]")
    plot_list[[length(plot_list)+1]] <- plot_nutr
  }
  if("prim prod" %in% item_drivers_short$driver){
    plot_pp <- var_plot_point("prim prod", "Chlorophyl A [µg/l]")
    plot_list[[length(plot_list)+1]] <- plot_pp
  }
  if("tourism" %in% item_drivers_short$driver){
    plot_tour <- var_plot_point_facet("tourism", "Various")
    plot_list[[length(plot_list)+1]] <- plot_tour
  }
  if("fisheries" %in% item_drivers_short$driver){
    plot_fish <- ggplot() + geom_blank() + annotate(geom = "label", x = 0, y = 0, label = "Data within fjords is rare") +
      labs(y = paste0(item_df$item_name," fishery"), x = "date") + theme_trip() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
    plot_list[[length(plot_list)+1]] <- plot_fish
  }
  if("governance" %in% item_drivers_short$driver){
    plot_gov <- ggplot() + geom_blank() + annotate(geom = "label", x = 0, y = 0, label = "No numeric data") +
      labs(y = "Governance", x = "date") + theme_trip() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
    plot_list[[length(plot_list)+1]] <- plot_gov
  }
  plot_combine <- ggpubr::ggarrange(plotlist = plot_list, ncol = 3, nrow = 1,
                                    common.legend = TRUE, legend = "bottom")
  ggsave(paste0("survey/reports/figures/dp_",item_choice,".png"),
         plot = plot_combine, width = 12, height = 4)
  # rm(item_choice, item_df, item_text, item_presence, item_sites, item_sites_short,
  #    item_drivers, item_drivers_short, clean_sub, site_colours, theme_trip, var_plot_line,
  #    var_plot_point, var_plot_point_facet, var_plot_point_many, plot_list, plot_temp,
  #    plot_ice, plot_runoff, plot_light, plot_nutr, plot_pp, plot_tour, plot_fish, plot_gov, plot_combine)
}

# Run the figures
doParallel::registerDoParallel(cores = 7)
plyr::l_ply(all_code, report_driver_plot, .parallel = TRUE)


# Reports -----------------------------------------------------------------

# Reports compiled manually via endNote

