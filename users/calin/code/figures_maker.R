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
                          site == "young"~"young")) %>% 
  dplyr::select(date_accessed, URL, citation, type, lieu, category, driver, variable, lon, lat, date, depth, value) %>% 
  dplyr::rename(site = lieu)


# Data summarise by year and place
ECC_data_annual <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year, site))






ECC_type  <- ECC_data %>% 
  mutate(classification = case_when(grepl("|FIS|", variable, fixed = TRUE) == TRUE ~ "Fish",
                                    grepl("|MAM|", variable, fixed = TRUE) == TRUE ~ "Mammal",
                                    grepl("|SBI|", variable, fixed = TRUE) == TRUE~"Bird (Sea)",
                                    grepl("|NBI|", variable, fixed = TRUE) == TRUE ~ "Bird (Non-sea)",
                                    grepl("|BIR|", variable, fixed = TRUE) == TRUE ~ "Bird",
                                    grepl("|ZOO|", variable, fixed = TRUE) == TRUE ~ "Zooplankton",
                                    (grepl("phyto", variable, fixed = TRUE) == TRUE ~ "Phytoplankton")))



ECC_annual_barents <- ECC_type %>% 
  filter(site == "barents sea") %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year,classification))

ECC_annual_barents[nrow(ECC_annual_barents)+1,] <- list(1946, "Bird (Sea)", 0)
ECC_annual_barents[nrow(ECC_annual_barents)+1,] <- list(1946, "Bird (Non-sea)", 0)
ECC_annual_barents[nrow(ECC_annual_barents)+1,] <- list(1946, "Bird", 0)
ECC_annual_barents[nrow(ECC_annual_barents)+1,] <- list(1946, "Zooplankton", 0)
ECC_annual_barents



ECC_annual_is <- ECC_type %>% 
  filter(site == "is") %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year,classification))

ECC_annual_is[nrow(ECC_annual_is)+1,] <- list(1988, "Bird", 0)
ECC_annual_is[nrow(ECC_annual_is)+1,] <- list(1988, "Bird (Non-sea)", 0)
ECC_annual_is[nrow(ECC_annual_is)+1,] <- list(1988, "Mammal", 0)
ECC_annual_is[nrow(ECC_annual_is)+1,] <- list(1988, "Fish", 0)
ECC_annual_is[nrow(ECC_annual_is)+1,] <- list(1988, "Zooplankton", 0)
ECC_annual_is

ECC_annual_kong <- ECC_type %>% 
  filter(site == "kong") %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year,classification))

ECC_annual_kong[nrow(ECC_annual_kong)+1,] <- list(2005, "Bird", 0)
ECC_annual_kong[nrow(ECC_annual_kong)+1,] <- list(2005, "Bird (Non-sea)", 0)
ECC_annual_kong[nrow(ECC_annual_kong)+1,] <- list(2005, "Mammal", 0)
ECC_annual_kong[nrow(ECC_annual_kong)+1,] <- list(2005, "Fish", 0)
ECC_annual_kong


ECC_annual_nuup <- ECC_type %>% 
  filter(site == "nuup") %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year,classification))

ECC_annual_nuup[nrow(ECC_annual_nuup)+1,] <- list(2006, "Zooplankton", 0)
ECC_annual_nuup[nrow(ECC_annual_nuup)+1,] <- list(2006, "Fish", 0)
ECC_annual_nuup


ECC_annual_svalbard <- ECC_type %>% 
  filter(site == "svalbard") %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year,classification))

ECC_annual_svalbard[nrow(ECC_annual_svalbard)+1,] <- list(1980, "Bird", 0)
ECC_annual_svalbard[nrow(ECC_annual_svalbard)+1,] <- list(1980, "Bird (Non-sea)", 0)
ECC_annual_svalbard[nrow(ECC_annual_svalbard)+1,] <- list(1980, "Fish", 0)
ECC_annual_svalbard



ECC_annual_young <- ECC_type %>% 
  filter(site == "young") %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year,classification))

ECC_annual_young[nrow(ECC_annual_young)+1,] <- list(1995, "Fish", 0)
ECC_annual_young[nrow(ECC_annual_young)+1,] <- list(1995, "Mammal", 0)
ECC_annual_young[nrow(ECC_annual_young)+1,] <- list(1995, "Zooplankton", 0)
ECC_annual_young












ECC_data_type <- ECC_type %>% 
  summarise(type_count = n(), .by = classification)

ECC_data_type_spe <- ECC_type %>% 
  summarise(type_count = n(), .by = c(classification, site))

# Data merge by species groups and summarise by year and place
ECC_data_annual_type <- ECC_type %>% 
  mutate(year = year(date)) %>%
  summarise(annual_type_count = n(), .by = c(year, classification))


# Data summarise by year and variable
ECC_data_annual_variable <- ECC_data %>% 
  mutate(year = year(date)) %>%
  # dplyr::select(year, variable, lieu) %>% 
  summarise(annual_type_count = n(), .by = c(year, variable))


# Data summarise by year, place and species/variables
ECC_data_species_summury <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, variable, site) %>% 
  distinct() %>% 
  summarise(annual_species_count = n(), .by = c(year, site))


# Data summarise by set of data over years and places
ECC_data_set_summury <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, URL, site) %>% 
  distinct() %>% 
  summarise(annual_set_count = n(), .by = c(year, site))



# Figures -----------------------------------------------------------------
# Population of glaucous gull infunction of the date/year
# kong_glaucous1 <- ggplot(data = kong_glaucous_gull_population, aes(x = date, y = value)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE)
# kong_glaucous1


# Population of caplin histogram of mature/immature stock over years
# barents_capelin1 <- ggplot(data = barents_capelin_stock, aes(x = date,  y = value)) + 
#   geom_bar(stat = "identity", aes(fill = variable)) +
#   theme_bw() + 
#   theme(legend.position = c(0.75,0.75), legend.background = element_rect(color = "black")) +
#   labs(x = NULL, fill = NULL )
# barents_capelin1


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
  geom_bar(aes(fill = site), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Data by site and by year", x = NULL, fill = "site") +
  theme(legend.position = c(0.20,0.80), panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="Set3")
ECC_data01
ggsave(ECC_data01, file = 'users/calin/figures/ECC_data01.png') # Save figure






ECC_data01_barents <- ggplot(ECC_annual_barents, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'stack') +
  labs(y = "data [n]", title = "Barents sea data by year", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="Accent",)
ECC_data01_barents
ggsave(ECC_data01_barents, file = 'users/calin/figures/ECC_data01_barents.png') # Save figure


ECC_data01_is <- ggplot(ECC_annual_is, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'stack') +
  labs(y = "data [n]", title = "Isfjorden data by year", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="Accent",)
ECC_data01_is
ggsave(ECC_data01_is, file = 'users/calin/figures/ECC_data01_is.png') # Save figure



ECC_data01_kong <- ggplot(ECC_annual_kong, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'stack') +
  labs(y = "data [n]", title = "Kongsfjorden data by year", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="Accent",)
ECC_data01_kong
ggsave(ECC_data01_kong, file = 'users/calin/figures/ECC_data01_kong.png') # Save figure




ECC_data01_nuup <- ggplot(ECC_annual_nuup, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Nuup Kangerlua data by year", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="Accent",)
ECC_data01_nuup
ggsave(ECC_data01_nuup, file = 'users/calin/figures/ECC_data01_nuup.png') # Save figure


ECC_data01_svalbard <- ggplot(ECC_annual_svalbard, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Svalbard data by year", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="Accent",)
ECC_data01_svalbard
ggsave(ECC_data01_svalbard, file = 'users/calin/figures/ECC_data01_svalbard.png') # Save figure




ECC_data01_young <- ggplot(ECC_annual_young, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'stack') +
  labs(y = "data [n]", title = "Young Sound data by year", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="Accent",)
ECC_data01_young
ggsave(ECC_data01_young, file = 'users/calin/figures/ECC_data01_young.png') # Save figure
















ECC_data01_young <- ggplot(ECC_data_annual, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = "young"), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Young data by year", x = NULL, fill = "site") +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA)) +
  scale_fill_brewer(palette="BuGn")
ECC_data01_young
ggsave(ECC_data01, file = 'users/calin/figures/ECC_data01.png') # Save figure


# Bar nb species by site over the years
ECC_data02 <- ggplot(ECC_data_species_summury, aes(x = year, y = annual_species_count)) + 
  geom_bar(aes(fill = site), stat = 'Identity', position = 'dodge') +
  labs(y = "species [n]", x = NULL, fill = "site") +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set3")
ECC_data02
ggsave(ECC_data02, file = 'users/calin/figures/ECC_data02.png') # Save figure


# Bar nb set by site over the years
ECC_data03 <- ggplot(ECC_data_set_summury, aes(x = year, y = annual_set_count)) + 
  geom_bar(aes(fill = site), stat = 'Identity', position = 'stack') +
  # geom_line(aes(colour = lieu)) +
  labs(y = "set [n]", title = "Data by set and by year", x = NULL, fill = "site") +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set2")
ECC_data03
ggsave(ECC_data03, file = 'users/calin/figures/ECC_data03.png') # Save figure


# Bar nb data by species group over the years
ECC_data04 <- ggplot(ECC_data_annual_type, aes(x = year, y = annual_type_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Data by classification and by year", x = NULL) +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set2")
ECC_data04


# Bar nb species by site over the years
ECC_data05 <- ggplot(ECC_data_type, aes(x="", y = type_count, fill=classification)) +
  geom_bar(stat="identity", width = 1)+#, color="white") +
  labs(fill = "species groups") +
  coord_polar("y", start=0)+
  theme_void() + # remove background, grid, numeric labels
  scale_fill_brewer(palette="Set2")
ECC_data05
ggsave(ECC_data05, file = 'users/calin/figures/ECC_data05.png', width = 8) # Save figure


# Density data by site over the years
ECC_data06 <- ggplot(ECC_data, aes(x = year(date), y = site, fill = site)) +
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

mypalette <- 
# Density [new function] data by site over the years
ECC_data08 <- ggplot(ECC_type, aes(x = year(date), y = classification, fill = classification)) +
  # ggdist::geom_slabinterval() +
  ggdist::stat_halfeye(.width = 0, point_color = NA) +
  # theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 15)) +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette = "BuGn", direction = -1)
ECC_data08
ggsave(ECC_data08, file = 'users/calin/figures/ECC_data08.png') # Save figure



ECC_data09 <- ggplot(ECC_type, aes(x = year(date), y = classification, fill = classification)) +
  geom_violin(
    fill = "grey72", 
    ## remove outline
    color = NA, 
    ## width of violins mapped to # observations
    ## custom bandwidth (smoothing)
    bw = 1
  )
ECC_data09


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

