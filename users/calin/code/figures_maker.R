# Code/figures_maker.R
# script to creat figures with WP1 data


# Set up ------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(ggridges)
library(ggpubr)
library(stringi)
library(ggdist)
library(tidyquant)


# Load data  set ---------------------------------------------------------------

load('users/calin/data/EU_arctic_data.RData') # firstsetdata data -> svalbard data
load('users/calin/data/young_species_GEM.RData') # young_species_gem data -> young GEM data
load('users/calin/data/nuup_species_GEM.RData') # nuup_species_gem data -> nuup GEM data


# Combined data set -------------------------------------------------------

# Greenland GEM data
green_GEM_data <- rbind(young_species_GEM, nuup_species_GEM)


# Greenland GEM and Svalbard data
## Merge site
ECC_data <- rbind(green_GEM_data, EU_arctic_data) %>% 
  mutate(lieu = case_when(site == "barents sea"~"barents sea",
                          site == "east ice"~"barents sea",
                          site == "is"~"is",
                          site == "kong"~"kong",
                          site == "nuup"~"nuup",
                          site == "svalbard"~"svalbard",
                          site == "west ice"~"barents sea",
                          site == "young"~"young"))


# Data summarise by year and place
ECC_data_annual <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year, lieu))

ECC_type  <- ECC_data %>% 
  mutate(fish = case_when(grepl("|FIS|", variable, fixed = TRUE) == TRUE~"Fish"),
         mammal = case_when(grepl("|MAM|", variable, fixed = TRUE) == TRUE~"Mammal"),
         sbird = case_when(grepl("|SBI|", variable, fixed = TRUE) == TRUE~"Sea Bird"),
         nbird = case_when(grepl("|NBI|", variable, fixed = TRUE) == TRUE~"Non-sea Bird"),
         bird = case_when(grepl("|BIR|", variable, fixed = TRUE) == TRUE~"Bird"),
         zoo = case_when(grepl("|ZOO|", variable, fixed = TRUE) == TRUE~"Zooplankton"),
         phyto = case_when(grepl("phyto", variable, fixed = TRUE) == TRUE~"Phytoplankton"),
         classification = case_when(fish == "Fish"~"fish",
                                    mammal == "Mammal" ~"mammal",
                                    sbird == "Sea Bird"~"sea bird",
                                    nbird == "Non-sea Bird"~"non-sea bird",
                                    bird == "Bird"~"bird",
                                    zoo == "Zooplankton"~"zooplankton",
                                    phyto == "Phytoplankton"~"phytoplankton"))

ECC_data_type <- ECC_data %>% 
  mutate(fish = case_when(grepl("|FIS|", variable, fixed = TRUE) == TRUE~"Fish"),
         mammal = case_when(grepl("|MAM|", variable, fixed = TRUE) == TRUE~"Mammal"),
         sbird = case_when(grepl("|SBI|", variable, fixed = TRUE) == TRUE~"Sea Bird"),
         nbird = case_when(grepl("|NBI|", variable, fixed = TRUE) == TRUE~"Non-sea Bird"),
         bird = case_when(grepl("|BIR|", variable, fixed = TRUE) == TRUE~"Bird"),
         zoo = case_when(grepl("|ZOO|", variable, fixed = TRUE) == TRUE~"Zooplankton"),
         phyto = case_when(grepl("phyto", variable, fixed = TRUE) == TRUE~"Phytoplankton"),
         classification = case_when(fish == "Fish"~"fish",
                                    mammal == "Mammal" ~"mammal",
                                    sbird == "Sea Bird"~"sea bird",
                                    nbird == "Non-sea Bird"~"non-sea bird",
                                    bird == "Bird"~"bird",
                                    zoo == "Zooplankton"~"zooplankton",
                                    phyto == "Phytoplankton"~"phytoplankton")) %>%
  dplyr::select( classification, lieu) %>% 
  summarise(type_count = n(), .by =  classification)

# Data merge by species groups and summarise by year and place
ECC_data_annual_type <- ECC_data %>% 
  mutate(year = year(date),
         fish = case_when(grepl("|FIS|", variable, fixed = TRUE) == TRUE~"Fish"),
         mammal = case_when(grepl("|MAM|", variable, fixed = TRUE) == TRUE~"Mammal"),
         sbird = case_when(grepl("|SBI|", variable, fixed = TRUE) == TRUE~"Sea Bird"),
         nbird = case_when(grepl("|NBI|", variable, fixed = TRUE) == TRUE~"Non-sea Bird"),
         bird = case_when(grepl("|BIR|", variable, fixed = TRUE) == TRUE~"Bird"),
         zoo = case_when(grepl("|ZOO|", variable, fixed = TRUE) == TRUE~"Zooplankton"),
         phyto = case_when(grepl("phyto", variable, fixed = TRUE) == TRUE~"Phytoplankton"),
         classification = case_when(fish == "Fish"~"fish",
                                    mammal == "Mammal" ~"mammal",
                                    sbird == "Sea Bird"~"sea bird",
                                    nbird == "Non-sea Bird"~"non-sea bird",
                                    bird == "Bird"~"bird",
                                    zoo == "Zooplankton"~"zooplankton",
                                    phyto == "Phytoplankton"~"phytoplankton")) %>%
  dplyr::select(year, classification, lieu) %>% 
  summarise(annual_type_count = n(), .by = c(year, classification))


# Data summarise by year and variable
ECC_data_annual_variable <- ECC_data %>% 
  mutate(year = year(date)) %>%
  dplyr::select(year, variable, lieu) %>% 
  summarise(annual_type_count = n(), .by = variable)


# Data summarise by year, place and species/variables
ECC_data_species_summury <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, variable, lieu) %>% 
  distinct() %>% 
  summarise(annual_species_count = n(), .by = c(year, lieu))


# Data summarise by set of data over years and places
ECC_data_set_summury <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, URL, lieu) %>% 
  distinct() %>% 
  summarise(annual_set_count = n(), .by = c(year, lieu))



# Figures -----------------------------------------------------------------
# Population of glaucous gull infunction of the date/year
kong_glaucous1 <- ggplot(data = kong_glaucous_gull_population, aes(x = date, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)
kong_glaucous1


# Population of caplin histogram of mature/immature stock over years
barents_capelin1 <- ggplot(data = barents_capelin_stock, aes(x = date,  y = value)) + 
  geom_bar(stat = "identity", aes(fill = variable)) +
  theme_bw() + 
  theme(legend.position = c(0.75,0.75), legend.background = element_rect(color = "black")) +
  labs(x = NULL, fill = NULL )
barents_capelin1


# Density plot of variable over years
arctic_data1 <- ggplot(EU_arctic_data, aes(x = date, y = variable, fill = variable)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL)
arctic_data1

# Point plot of value over years
arctic_data2 <- ggplot(data = EU_arctic_data, aes(x = date, y = value)) +
  geom_point(aes(color = variable)) + 
  facet_wrap(~variable, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)
arctic_data2

# Mix of two different figures and cleaning of the final
arctic_data3 <- ggarrange(arctic_data1, arctic_data2,ncol = 1, nrow = 2, labels = c("a)", "b)"),align = "hv")
arctic_data4 <- annotate_figure(arctic_data3, top = text_grob("Summary arctic data set"))
arctic_data4


# Bar nb data by site over the years
ECC_data01 <- ggplot(ECC_data_annual, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = lieu), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Data by site and by year", x = NULL, fill = "site") +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set3")
ECC_data01
ggsave(ECC_data01, file = 'users/calin/figures/ECC_data01.png') # Save figure


# Bar nb species by site over the years
ECC_data02 <- ggplot(ECC_data_species_summury, aes(x = year, y = annual_species_count)) + 
  geom_bar(aes(fill = lieu), stat = 'Identity', position = 'dodge') +
  labs(y = "species [n]", x = NULL) +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set3")
ECC_data02
ggsave(ECC_data02, file = 'users/calin/figures/ECC_data02.png') # Save figure


# Bar nb set by site over the years
ECC_data03 <- ggplot(ECC_data_set_summury, aes(x = year, y = annual_set_count)) + 
  geom_bar(aes(fill = lieu), stat = 'Identity', position = 'dodge') +
  labs(y = "set [n]", title = "Data by set and by year", x = NULL) +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set2")
ECC_data03


# Bar nb data by species group over the years
ECC_data04 <- ggplot(ECC_data_annual_type, aes(x = year, y = annual_type_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Data by classification and by year", x = NULL) +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set2")
ECC_data04


# Bar nb species by site over the years
ECC_data05 <- ggplot(ECC_data_type, aes(x="", y = type_count, fill=classification)) +
  geom_bar(stat="identity", width=1, color="white") +
  labs(fill = "species groups") +
  coord_polar("y", start=0)+
  theme_void() + # remove background, grid, numeric labels
  scale_fill_brewer(palette="Set2")
ECC_data05
ggsave(ECC_data05, file = 'users/calin/figures/ECC_data05.png') # Save figure


# Density data by site over the years
ECC_data06 <- ggplot(ECC_data, aes(x = year(date), y = lieu, fill = lieu)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette="Set3")
ECC_data06
ggsave(ECC_data06, file = 'users/calin/figures/ECC_data06.png') # Save figure


# Density data by site over the years
ECC_data07 <- ggplot(ECC_type, aes(x = year(date), y = classification, fill = classification)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette="Set3")
ECC_data07
ggsave(ECC_data07, file = 'users/calin/figures/ECC_data07.png') # Save figure


# Density [new function] data by site over the years
ECC_data08 <- ggplot(ECC_type, aes(x = year(date), y = classification, fill = classification)) +
  ggdist::stat_halfeye(.width = 0, point_color = NA) +
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Set3")
ECC_data08
ggsave(ECC_data08, file = 'users/calin/figures/ECC_data08.png') # Save figure



# Density of data over years for GEM data by site
gem_data01 <- ggplot(green_GEM_data, aes(x = date, y = site, fill = site)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL)
gem_data01


# Density [new function] of data over years for GEM data by site
gem_data02 <- ggplot(green_GEM_data, aes(x = year(date), y = site, fill = site)) +
  ggdist::stat_halfeye(.width = 0, point_color = NA) +
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL)
gem_data02
