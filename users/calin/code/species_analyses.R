# code/species_analyses
# Species analyses script


# Code from : https://github.com/Biodiversity-trends-in-Europe-ILTER/R-code/blob/master/Script_01#L49

# Set up ------------------------------------------------------------------
source('users/calin/code/formulas.R')

library(vegan)
library(codyn)
library(reshape2)
library(metafor)
library(R.utils)
library(sp)
library(raster)
library(ncdf4)
library(rgdal)







# Analyses ----------------------------------------------------------------

# svalbard (east) harp seal population
svalbard_cycr_population <- read.csv("P:/FACE-IT_data/svalbard/population-size-of-hoode.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/hooded-seal/",
         citation = "Institute of Marine Research (2022). Population size of hooded seals in the West Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/hooded-seal.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Cystophora cristata",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," ", str_replace_all(tolower(name),"\\."," ")," [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Site = site name, TimeSeries_id = unique identifier for the time series, Year = survey year, Taxon = taxon name, Density = total density or biomass or number of individual of that taxon for that year. 
clean_sva_cycr_pop <- svalbard_cycr_population %>% 
  filter(variable == "|MAM| Cystophora cristata (hooded seal) modelled total stock size [n]") %>% 
  dplyr::rename(Site = site, Density = value) %>% 
  mutate(TimeSeries_id = 1,
         Year = year(date),
         Taxon = "Cystophora cristata") %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)

data_n_for_analysis
data_100_for_analysis
data_gm_for_analysis
data_6kg_for_analysis
data_3kg_for_analysis





# (1) Compute biodiversity metrics -----------------------------------------------------------------

DATA1 <- dcast(data_n_for_analysis, Year ~ Taxon, sum, value.var = "Density") # create cross table
lastTaxon <- length(DATA1)
firstTaxon <- 2

DATA1_Taxa <- as.matrix(DATA1[,c(firstTaxon:lastTaxon)])
DATA1_Taxa <- subset(DATA1[,c(firstTaxon:lastTaxon)]) 

DATA1$NTaxa <- specnumber(DATA1_Taxa)  # taxonomic richness
DATA1$Simp <- diversity(DATA1_Taxa, index = "simpson") # Simpson´s taxonomic diversity
DATA1$Abund <- rowSums (DATA1_Taxa) # Total abundance
DATA1_Turnover <- turnover(data_n_for_analysis, time.var = "Year", species.var = "Taxon", abundance.var = "Density" , metric = "total")
DATA1$Turnover <- c(0, DATA1_Turnover$total) # Turnover

# Prepare data for next steps: 

start.year <- min(DATA1$Year) # set the starting year of the time series
end.year <- max(DATA1$Year) # set the end year of the time series

# In case of missing data for one or more years, add a row with NAs:
# new.row <- head(DATA1[NA,], 1)    # creat a new row with NAs for all columns
# new.row["Year"] <- 1992   # assign the year without data
# DATA1 <- rbind(DATA1,new.row)    # attach the newly created row to the main table
# DATA1 <- DATA1[order(DATA1$Year),]   # order the years chronologically





# (2) Compute the monotonic trends for each biodiversity metric ---------------

DATA1.NTaxa <- ts(DATA1$NTaxa, start = start.year, frequency = 1) # define data as time series
acf(DATA1.NTaxa, na.action = na.pass) # check for temporal autocorrelation
pacf(DATA1.NTaxa, na.action = na.pass) # check for temporal autocorrelation
DATA1.trend.NTaxa <- My.mmkh(DATA1$NTaxa) # Modified Mann-Kendall Test For Serially Correlated Data Using Hamed and Rao (1998) Variance Correction Approach

DATA1.Simp <- ts(DATA1$Simp, start=start.year, frequency=1) # define data as time series
acf(DATA1.Simp, na.action = na.pass) # check for temporal autocorrelation
pacf(DATA1.Simp, na.action = na.pass) # check for temporal autocorrelation
DATA1.trend.Simp <- My.mmkh(DATA1$Simp) # Modified Mann-Kendall Test For Serially Correlated Data Using Hamed and Rao (1998) Variance Correction Approach

DATA1.Abund<- ts(DATA1$Abund, start=start.year, frequency=1) # define data as time series
acf(DATA1.Abund, na.action = na.pass) # check for temporal autocorrelation
pacf(DATA1.Abund, na.action = na.pass) # check for temporal autocorrelation
DATA1.trend.Abund <- My.mmkh(DATA1$Abund) # Modified Mann-Kendall Test For Serially Correlated Data Using Hamed and Rao (1998) Variance Correction Approach

DATA1.Turnover <- ts(DATA1$Turnover, start=start.year+1, frequency=1)  # define data as time series
acf(DATA1.Turnover, na.action = na.pass) # check if there is temporal autocorrelation
pacf(DATA1.Turnover, na.action = na.pass) # check if there is temporal autocorrelation
DATA1.trend.Turnover <- My.mmkh(DATA1$Turnover[-1]) # Modified Mann-Kendall Test For Serially Correlated Data Using Hamed and Rao (1998) Variance Correction Approach

# Combine results in a data frame: 

EffectSizes_biodiv_DATA1 <- data.frame(TimeSeries = "DATA1", Site = "RMO", Country = "DE", Lat = 50.187,	Lon = 9.100,	Alt = 122, # fill in with site information 
                                       TaxonomicGroup = "AquaticInv", Realm = "FW", Naturalness = 3, startYear = start.year, endYear = end.year,    # fill in with site information
                                       NTaxa_S = DATA1.trend.NTaxa[10], 
                                       NTaxa_var = DATA1.trend.NTaxa[8], # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]
                                       Abund_S = DATA1.trend.Abund[10], 
                                       Abund_var = DATA1.trend.Abund[8],  # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]
                                       Simp_S = DATA1.trend.Simp[10],  
                                       Simp_var = DATA1.trend.Simp[9],  # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]
                                       Turn_S = DATA1.trend.Turnover[10],  
                                       Turn_var = DATA1.trend.Turnover[8])  # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]

