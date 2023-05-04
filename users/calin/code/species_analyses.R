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

library(metafor)
library(rgeos)
library(RColorBrewer)
library(glmulti)





# Analyses ----------------------------------------------------------------
# 
# # svalbard (east) harp seal population
# svalbard_cycr_population <- read.csv("P:/FACE-IT_data/svalbard/population-size-of-hoode.csv", sep = ";", dec = ",") %>%
#   pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
#   mutate(date_accessed = as.Date("2023-04-14"),
#          URL = "https://mosj.no/en/indikator/fauna/marine-fauna/hooded-seal/",
#          citation = "Institute of Marine Research (2022). Population size of hooded seals in the West Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/hooded-seal.html",
#          lon = NA, lat = NA, depth = NA,
#          Species = "Cystophora cristata",
#          nomsp = map(Species, latin_eng),
#          variable = paste0(nomsp," ", str_replace_all(tolower(name),"\\."," ")," [n]"),
#          category = "bio",
#          driver ="biomass",
#          type = "in situ",
#          site = "svalbard",
#          date = as.Date(paste0(Category,"-12-31"))) %>%
#   dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
#   filter(!is.na(value))
# 
# # Site = site name, TimeSeries_id = unique identifier for the time series, Year = survey year, Taxon = taxon name, Density = total density or biomass or number of individual of that taxon for that year. 
# clean_sva_cycr_pop <- svalbard_cycr_population %>% 
#   filter(variable == "|MAM| Cystophora cristata (hooded seal) modelled total stock size [n]") %>% 
#   dplyr::rename(Site = site, Density = value) %>% 
#   mutate(TimeSeries_id = 1,
#          Year = year(date),
#          Taxon = "Cystophora cristata") %>% 
#   dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)

nuup_bird_nb
DATA1 <- species_analysis_step1(NUUP_n, "DATA1")


# 
# 
# DATA2 <- species_analysis_step1(data_6kg_for_analysis, "DATA5")
# DATA3 <- species_analysis_step1(data_n_for_analysis, "DATA3")
# 
# DATA4 <- plyr::ddply(data_n_for_analysis,.variables = c("Site"), .fun = species_analysis_step1, tsname = "DATA3")
# 
# 
# DATA5 <- plyr::ddply(nuup_bird_nb,.variables = c("Site"), .fun = species_analysis_step1, tsname = "DATA5")
# 
# 


# (1) Compute biodiversity metrics -----------------------------------------------------------------

DATA1 <- dcast(data_nuup_analysis, Year ~ Taxon, sum, value.var = "Density") # create cross table
lastTaxon <- length(DATA1)
firstTaxon <- 2

DATA1_Taxa <- as.matrix(DATA1[,c(firstTaxon:lastTaxon)])
DATA1_Taxa <- subset(DATA1[,c(firstTaxon:lastTaxon)]) 

DATA1$NTaxa <- specnumber(DATA1_Taxa)  # taxonomic richness
DATA1$Simp <- diversity(DATA1_Taxa, index = "simpson") # Simpson´s taxonomic diversity
DATA1$Abund <- rowSums (DATA1_Taxa) # Total abundance
DATA1_Turnover <- turnover(data_nuup_analysis, time.var = "Year", species.var = "Taxon", abundance.var = "Density" , metric = "total")
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


DATA <- rbind(EffectSizes_biodiv_DATA1)












# Transform variables (standardize continuous variables, and log- or sqrt- transform the ones that are not normally distributed)

DATA$StudyLength <- 1+(DATA$endYear-DATA$startYear)
DATA$Lat.s <- decostand(DATA$Lat, "standardize")
DATA$StudyLength.s <- decostand(DATA$StudyLength, "standardize")
DATA$Alt.Log.s <- decostand(log(DATA$Alt+2), "standardize")
DATA$TMean_S.s <- decostand(sqrt(DATA$TMean_S+154), "standardize")
DATA$Lon.s <- decostand(sqrt(DATA$Lon+9) , "standardize")
DATA$Naturalness.s <- decostand(DATA$Naturalness, "standardize")
DATA$PTot_S.s <- decostand(DATA$PTot_S, "standardize")

# Meta-analysis -----------------------------------------------------------


# Abundance:
res.rma.Abund.TaxonomicGroup <- rma(yi = Abund_S, vi = Abund_var, method="REML", mods= ~ TaxonomicGroup-1, data= DATA)
res.rma.Abund.Realm <- rma(yi = Abund_S, vi = Abund_var, method="REML", mods= ~ Realm-1, data= DATA)
res.rma.Abund.Biogeoregion <- rma(yi = Abund_S, vi = Abund_var, method="REML", mods= ~ Biogeoregion-1, data= DATA)
# export results
RMA.Abund.TaxonomicGroup <- data.frame(estimate = res.rma.Abund.TaxonomicGroup$beta,  pval = res.rma.Abund.TaxonomicGroup$pval, zval = res.rma.Abund.TaxonomicGroup$zval, ci.lb = res.rma.Abund.TaxonomicGroup$ci.lb, ci.ub = res.rma.Abund.TaxonomicGroup$ci.ub)
RMA.Abund.Realm <- data.frame(estimate = res.rma.Abund.Realm$beta,  pval = res.rma.Abund.Realm$pval, zval = res.rma.Abund.Realm$zval, ci.lb = res.rma.Abund.Realm$ci.lb, ci.ub = res.rma.Abund.Realm$ci.ub)
RMA.Abund.Biogeoregion <- data.frame(estimate = res.rma.Abund.Biogeoregion$beta,  pval = res.rma.Abund.Biogeoregion$pval, zval = res.rma.Abund.Biogeoregion$zval, ci.lb = res.rma.Abund.Biogeoregion$ci.lb, ci.ub = res.rma.Abund.Biogeoregion$ci.ub)
write.table(RMA.Abund.TaxonomicGroup, "RMA.Abund.TaxonomicGroup.csv", sep =";")
write.table(RMA.Abund.Realm, "RMA.Abund.Realm.csv", sep =";")
write.table(RMA.Abund.Biogeoregion, "RMA.Abund.Biogeoregion.csv", sep =";")

# Taxonomic richness:
res.rma.NTaxa.TaxonomicGroup <- rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods= ~ TaxonomicGroup-1, data= DATA)
res.rma.NTaxa.Realm <- rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods= ~ Realm-1, data= DATA)
res.rma.NTaxa.Biogeoregion <- rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods= ~ Biogeoregion-1, data= DATA)
# export results
RMA.NTaxa.TaxonomicGroup <- data.frame(estimate = res.rma.NTaxa.TaxonomicGroup$beta,  pval = res.rma.NTaxa.TaxonomicGroup$pval, zval = res.rma.NTaxa.TaxonomicGroup$zval, ci.lb = res.rma.NTaxa.TaxonomicGroup$ci.lb, ci.ub = res.rma.NTaxa.TaxonomicGroup$ci.ub)
RMA.NTaxa.Realm <- data.frame(estimate = res.rma.NTaxa.Realm$beta,  pval = res.rma.NTaxa.Realm$pval, zval = res.rma.NTaxa.Realm$zval, ci.lb = res.rma.NTaxa.Realm$ci.lb, ci.ub = res.rma.NTaxa.Realm$ci.ub)
RMA.NTaxa.Biogeoregion <- data.frame(estimate = res.rma.NTaxa.Biogeoregion$beta,  pval = res.rma.NTaxa.Biogeoregion$pval, zval = res.rma.NTaxa.Biogeoregion$zval, ci.lb = res.rma.NTaxa.Biogeoregion$ci.lb, ci.ub = res.rma.NTaxa.Biogeoregion$ci.ub)
write.table(RMA.NTaxa.TaxonomicGroup, "RMA.NTaxa.TaxonomicGroup.csv", sep =";")
write.table(RMA.NTaxa.Realm, "RMA.NTaxa.Realm.csv", sep =";")
write.table(RMA.NTaxa.Biogeoregion, "RMA.NTaxa.Biogeoregion.csv", sep =";")

# Simpson´s diversity:
res.rma.Simp.TaxonomicGroup <- rma(yi = Simp_S, vi = Simp_var, method="REML", mods= ~ TaxonomicGroup-1, data= DATA)
res.rma.Simp.Realm <- rma(yi = Simp_S, vi = Simp_var, method="REML", mods= ~ Realm-1, data= DATA)
res.rma.Simp.Biogeoregion <- rma(yi = Simp_S, vi = Simp_var, method="REML", mods= ~ Biogeoregion-1, data= DATA)
# export results
RMA.Simp.TaxonomicGroup <- data.frame(estimate = res.rma.Simp.TaxonomicGroup$beta,  pval = res.rma.Simp.TaxonomicGroup$pval, zval = res.rma.Simp.TaxonomicGroup$zval, ci.lb = res.rma.Simp.TaxonomicGroup$ci.lb, ci.ub = res.rma.Simp.TaxonomicGroup$ci.ub)
RMA.Simp.Realm <- data.frame(estimate = res.rma.Simp.Realm$beta,  pval = res.rma.Simp.Realm$pval, zval = res.rma.Simp.Realm$zval, ci.lb = res.rma.Simp.Realm$ci.lb, ci.ub = res.rma.Simp.Realm$ci.ub)
RMA.Simp.Biogeoregion <- data.frame(estimate = res.rma.Simp.Biogeoregion$beta,  pval = res.rma.Simp.Biogeoregion$pval, zval = res.rma.Simp.Biogeoregion$zval, ci.lb = res.rma.Simp.Biogeoregion$ci.lb, ci.ub = res.rma.Simp.Biogeoregion$ci.ub)
write.table(RMA.Simp.TaxonomicGroup, "RMA.Simp.TaxonomicGroup.csv", sep =";")
write.table(RMA.Simp.Realm, "RMA.Simp.Realm.csv", sep =";")
write.table(RMA.Simp.Biogeoregion, "RMA.Simp.Biogeoregion.csv", sep =";")

# Turnover:
res.rma.Turn.TaxonomicGroup <- rma(yi = Turn_S, vi = Turn_var, method="REML", mods= ~ TaxonomicGroup-1, data= DATA)
res.rma.Turn.Realm <- rma(yi = Turn_S, vi = Turn_var, method="REML", mods= ~ Realm-1, data= DATA)
res.rma.Turn.Biogeoregion <- rma(yi = Turn_S, vi = Turn_var, method="REML", mods= ~ Biogeoregion-1, data= DATA)
# export results
RMA.Turn.TaxonomicGroup <- data.frame(estimate = res.rma.Turn.TaxonomicGroup$beta,  pval = res.rma.Turn.TaxonomicGroup$pval, zval = res.rma.Turn.TaxonomicGroup$zval, ci.lb = res.rma.Turn.TaxonomicGroup$ci.lb, ci.ub = res.rma.Turn.TaxonomicGroup$ci.ub)
RMA.Turn.Realm <- data.frame(estimate = res.rma.Turn.Realm$beta,  pval = res.rma.Turn.Realm$pval, zval = res.rma.Turn.Realm$zval, ci.lb = res.rma.Turn.Realm$ci.lb, ci.ub = res.rma.Turn.Realm$ci.ub)
RMA.Turn.Biogeoregion <- data.frame(estimate = res.rma.Turn.Biogeoregion$beta,  pval = res.rma.Turn.Biogeoregion$pval, zval = res.rma.Turn.Biogeoregion$zval, ci.lb = res.rma.Turn.Biogeoregion$ci.lb, ci.ub = res.rma.Turn.Biogeoregion$ci.ub)
write.table(RMA.Turn.TaxonomicGroup, "RMA.Turn.TaxonomicGroup.csv", sep =";")
write.table(RMA.Turn.Realm, "RMA.Turn.Realm.csv", sep =";")
write.table(RMA.Turn.Biogeoregion, "RMA.Turn.Biogeoregion.csv", sep =";")


# Dataset descriptors -----------------------------------------------------
# This section allows to export data tables that will be used to create the figures: 

# Site coordinates:
Sites <- DATA[,c("Lat","Lon")]
write.table(Sites, "Sites.csv", sep =";")

# Study period:
Start_end_year = DATA[, c("TaxonomicGroup", "startYear", "endYear")]
write.table(Start_end_year, "Start_end_year.csv", sep =";")

# Data tables for pie charts of Figure 1: 
mycolors <- brewer.pal(8, name = 'Dark2')
mycolors2 <- data.frame(cbind(colB=mycolors, TaxonomicGroup= unique(as.character(DATA$TaxonomicGroup))))
TaxGr_Biog_pivotTable <- dcast(DATA, TaxonomicGroup ~ Biogeoregion, value.var="Realm", fun = length)
row.names(TaxGr_Biog_pivotTable) <- TaxGr_Biog_pivotTable$TaxonomicGroup
TaxGr_Biog_pivotTable <- merge(mycolors2, TaxGr_Biog_pivotTable ,by="TaxonomicGroup")
write.table(TaxGr_Biog_pivotTable, "TaxGr_Biog_pivotTable.csv", sep =";")

mycolors2 <-  data.frame(cbind(colB=mycolors, TaxonomicGroup=unique(as.character(DATA$TaxonomicGroup))))
TaxGr_Real_pivotTable <- dcast(DATA,TaxonomicGroup ~ Realm, value.var="Realm", fun = length)
row.names(TaxGr_Real_pivotTable) <- TaxGr_Real_pivotTable$TaxonomicGroup
TaxGr_Real_pivotTable <- merge(mycolors2, TaxGr_Real_pivotTable, by="TaxonomicGroup")
write.table(TaxGr_Real_pivotTable, "TaxGr_Real_pivotTable.csv", sep =";")

mycolors3 <-  data.frame(cbind(colB=mycolors, Biogeoregion=unique(as.character(DATA$Biogeoregion))))
Biog_Real_pivotTable <- dcast(DATA, Biogeoregion ~ Realm, value.var="Biogeoregion", fun = length)
row.names(Biog_Real_pivotTable) <- Biog_Real_pivotTable$Biogeoregion
Biog_Real_pivotTable <- merge(mycolors3, Biog_Real_pivotTable, by="Biogeoregion")
write.table(Biog_Real_pivotTable, "Biog_Real_pivotTable.csv", sep =";")

mycolors3= data.frame(cbind(colB=mycolors,Biogeoregion= unique(as.character(DATA$Biogeoregion))))
Biog_TaxGr_pivotTable=dcast(DATA,Biogeoregion~TaxonomicGroup, value.var="Biogeoregion", fun = length)
row.names(Biog_TaxGr_pivotTable)=Biog_TaxGr_pivotTable$Biogeoregion
Biog_TaxGr_pivotTable=merge(mycolors3,Biog_TaxGr_pivotTable,by="Biogeoregion")
write.table(Biog_TaxGr_pivotTable, "Biog_TaxGr_pivotTable.csv", sep =";")


# Information-theoretic approach for meta-analysis mixed model selection and multi-model inference ------------------------------------------------------------------

# This code is based on these two sources: https://github.com/nlkinlock/LDGmeta-analysis/blob/master/4-analysis.R
# and: http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti

# define function:
rma.glmulti <- function(formula, data, ...){
  rma(formula, vi, data=data, method="ML", ...)}
setOldClass("rma.uni")
setMethod('getfit', 'rma.uni', function(object, ...) {
  if (object$test=="z") {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=Inf)
  } else {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=object$k-object$p)
  }
})

# Check correlations among explanatory variables: 
cor(DATA[,c("TMean_S.s","PTot_S.s","Naturalness.s","Alt.Log.s","Lat.s","Lon.s")], use="pairwise.complete.obs")

#Remove timeseries with NAs in the target variables:
DATA.Abund.m <- DATA[!apply(DATA[,c("Abund_S","Abund_var","TMean_S.s","PTot_S.s","Naturalness.s","Lat.s","Lon.s","Alt.Log.s")], 1, anyNA),]
DATA.NTaxa.m <- DATA[!apply(DATA[,c("NTaxa_S","NTaxa_var","TMean_S.s","PTot_S.s","Naturalness.s","Lat.s","Lon.s","Alt.Log.s")], 1, anyNA),]
DATA_Simp.m <- DATA[!apply(DATA[,c("Simp_S","Simp_var","TMean_S.s","PTot_S.s","Naturalness.s","Lat.s","Lon.s","Alt.Log.s")], 1, anyNA),]
DATA.Turn.m <- DATA[!apply(DATA[,c("Turn_S","Turn_var","TMean_S.s","PTot_S.s","Naturalness.s","Lat.s","Lon.s","Alt.Log.s")], 1, anyNA),]

# Abundance:

vi <- DATA.Abund.m$Abund_var
modelsFULL.Abund <- glmulti(Abund_S ~ TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s,
                            data = DATA.Abund.m, level = 1,
                            fitfunction = rma.glmulti, crit = "aicc", confsetsize = 500)
rank.modelsFULL.Abund <- weightable(modelsFULL.Abund)
rank.modelsFULL.Abund <- rank.modelsFULL.Abund[rank.modelsFULL.Abund$aicc <= min(rank.modelsFULL.Abund$aicc) + 2, ]

# Model-averaged coefficients (estimate and 95% C.I.) and the relative importance (sum of Akaike weights, see main text for explanation) of each explanatory 
weights <- round(coef.glmulti(modelsFULL.Abund, select = nrow(rank.modelsFULL.Abund), icmethod = "Burnham"), 7)
weights  # Supplementary table 2

# average variation explained:
R2.Abund.sel <- numeric()
for (i in 1:nrow(rank.modelsFULL.Abund)) {
  R2.Abund.sel <- append(R2.Abund.sel,modelsFULL.Abund@objects[[i]]$R2)}
Averaged.R2.Abund <- sum(R2.Abund.sel*rank.modelsFULL.Abund[,3])/sum(rank.modelsFULL.Abund[,3])


# Richness:

vi <- DATA.NTaxa.m$NTaxa_var
modelsFULL.NTaxa <- glmulti(NTaxa_S ~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s,
                            data = DATA.NTaxa.m, level = 1,
                            fitfunction = rma.glmulti, crit = "aicc", confsetsize = 500)
rank.modelsFULL.NTaxa <- weightable(modelsFULL.NTaxa)
rank.modelsFULL.NTaxa <- rank.modelsFULL.NTaxa[rank.modelsFULL.NTaxa$aicc <= min(rank.modelsFULL.NTaxa$aicc) + 2, ]

# Model-averaged coefficients (estimate and 95% C.I.) and the relative importance (sum of Akaike weights, see main text for explanation) of each explanatory 
weights <- round(coef.glmulti(modelsFULL.NTaxa, select = nrow(rank.modelsFULL.NTaxa), icmethod = "Burnham"), 7)
weights # Supplementary table 2

# average variation explained:
R2.NTaxa.sel <- numeric()
for (i in 1:nrow(rank.modelsFULL.NTaxa)) {
  R2.NTaxa.sel <- append(R2.NTaxa.sel,modelsFULL.NTaxa@objects[[i]]$R2)}
Averaged.R2.NTaxa <- sum(R2.NTaxa.sel*rank.modelsFULL.NTaxa[,3])/sum(rank.modelsFULL.NTaxa[,3])


# Simpson´s diversity:

vi <- DATA_Simp.m$Simp_var
modelsFULL_Simp <- glmulti(Simp_S ~ TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s,
                           data = DATA_Simp.m, level = 1,
                           fitfunction = rma.glmulti, crit = "aicc", confsetsize = 500)
rank.modelsFULL_Simp <- weightable(modelsFULL_Simp)
rank.modelsFULL_Simp <- rank.modelsFULL_Simp[rank.modelsFULL_Simp$aicc <= min(rank.modelsFULL_Simp$aicc) + 2, ]
rank.modelsFULL_Simp

# Model-averaged coefficients (estimate and 95% C.I.) and the relative importance (sum of Akaike weights, see main text for explanation) of each explanatory 
weights <- round(coef.glmulti(modelsFULL_Simp, select = nrow(rank.modelsFULL_Simp), icmethod = "Burnham"), 7)
weights # Supplementary table 2

# average variation explained:
R2_Simp.sel <- numeric()
for (i in 1:nrow(rank.modelsFULL_Simp)) {
  R2_Simp.sel <- append(R2_Simp.sel,modelsFULL_Simp@objects[[i]]$R2)}
Averaged.R2_Simp <- sum(R2_Simp.sel*rank.modelsFULL_Simp[,3])/sum(rank.modelsFULL_Simp[,3])


# Turnover:

vi=DATA.Turn.m$Turn_var
modelsFULL.Turn <- glmulti(Turn_S ~ TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s,
                           data = DATA.Turn.m, level = 1,
                           fitfunction = rma.glmulti, crit = "aicc", confsetsize = 500)
rank.modelsFULL.Turn <- weightable(modelsFULL.Turn)
rank.modelsFULL.Turn <- rank.modelsFULL.Turn[rank.modelsFULL.Turn$aicc <= min(rank.modelsFULL.Turn$aicc) + 2, ]

# Model-averaged coefficients (estimate and 95% C.I.) and the relative importance (sum of Akaike weights, see main text for explanation) of each explanatory 
weights <- round(coef.glmulti(modelsFULL.Turn, select = nrow(rank.modelsFULL.Turn), icmethod = "Burnham"), 7)
weights  # Supplementary table 2

# average variation explained:
R2.Turn.sel <- numeric()
for (i in 1:nrow(rank.modelsFULL.Turn)) {
  R2.Turn.sel <- append(R2.Turn.sel,modelsFULL.Turn@objects[[i]]$R2)}
Averaged.R2.Turn <- sum(R2.Turn.sel*rank.modelsFULL.Turn[,3])/sum(rank.modelsFULL.Turn[,3])



# Test interactions between site Naturalness and Temperature and Precipitation--------------------

# Abundance:

# Fit the model as resulting from the model selection above
res.rma.Abund.Continuous0 <- rma(yi = Abund_S, vi = Abund_var, method="ML", 
                                 mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+StudyLength.s, data= DATA.Abund.m)
# Add interaction between site naturalness and temperature and precipitation trends:
res.rma.Abund.Continuous_1 <- rma(yi = Abund_S, vi = Abund_var, method="ML", 
                                  mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+StudyLength.s+TMean_S.s:Naturalness.s+PTot_S.s:Naturalness.s, data= DATA.Abund.m)
# Add interaction between site naturalness and temperature trend:
res.rma.Abund.Continuous_2 <- rma(yi = Abund_S, vi = Abund_var, method="ML", 
                                  mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+StudyLength.s+TMean_S.s:Naturalness.s, data= DATA.Abund.m)
# Add interaction between site naturalness and precipitation trends:
res.rma.Abund.Continuous_3 <- rma(yi = Abund_S, vi = Abund_var, method="ML", 
                                  mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+StudyLength.s+PTot_S.s:Naturalness.s, data= DATA.Abund.m)
# Compare the models and select the one with lowest AIC: 
AIC(res.rma.Abund.Continuous0, res.rma.Abund.Continuous_1, res.rma.Abund.Continuous_2, res.rma.Abund.Continuous_3)
res.rma.Abund.Continuous_selected <- rma(yi = Abund_S, vi = Abund_var, method="REML", 
                                         mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+StudyLength.s+TMean_S.s:Naturalness.s, data= DATA.Abund.m)


# Richness: 

res.rma.NTaxa.Continuous0<-rma(yi = NTaxa_S, vi = NTaxa_var, method="ML", 
                               mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s, data= DATA.NTaxa.m)
res.rma.NTaxa.Continuous_1<-rma(yi = NTaxa_S, vi = NTaxa_var, method="ML", 
                                mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+TMean_S.s:Naturalness.s+PTot_S.s:Naturalness.s, data= DATA.NTaxa.m)
res.rma.NTaxa.Continuous_2<-rma(yi = NTaxa_S, vi = NTaxa_var, method="ML", 
                                mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+TMean_S.s:Naturalness.s, data= DATA.NTaxa.m)
res.rma.NTaxa.Continuous_3<-rma(yi = NTaxa_S, vi = NTaxa_var, method="ML", 
                                mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+PTot_S.s:Naturalness.s, data= DATA.NTaxa.m)
# Compare the models and select the one with lowest AIC: 
AIC(res.rma.NTaxa.Continuous0, res.rma.NTaxa.Continuous_1, res.rma.NTaxa.Continuous_2, res.rma.NTaxa.Continuous_3)
res.rma.NTaxa.Continuous_selected<-rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", 
                                       mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+TMean_S.s:Naturalness.s, data= DATA.NTaxa.m)


# Simpson´s diversity:
# Naturalness is not in the selected model. 


# Turnover: 

res.rma.Turn.Continuous0<-rma(yi = Turn_S, vi = Turn_var, method="ML", 
                              mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s, data= DATA.Turn.m)
res.rma.Turn.Continuous_1<-rma(yi = Turn_S, vi = Turn_var, method="ML", 
                               mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+TMean_S.s:Naturalness.s+PTot_S.s:Naturalness.s, data= DATA.Turn.m)
res.rma.Turn.Continuous_2<-rma(yi = Turn_S, vi = Turn_var, method="ML", 
                               mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+TMean_S.s:Naturalness.s, data= DATA.Turn.m)
res.rma.Turn.Continuous_3<-rma(yi = Turn_S, vi = Turn_var, method="ML", 
                               mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+PTot_S.s:Naturalness.s, data= DATA.Turn.m)

AIC(res.rma.Turn.Continuous0, res.rma.Turn.Continuous_1, res.rma.Turn.Continuous_2, res.rma.Turn.Continuous_3)
res.rma.Turn.Continuous_selected<-rma(yi = Turn_S, vi = Turn_var, method="REML", 
                                      mods=~TMean_S.s+PTot_S.s+Naturalness.s+Lat.s+Lon.s+Alt.Log.s+StudyLength.s+TMean_S.s:Naturalness.s, data= DATA.Turn.m)



# Prepare and export data tables for Supplementary figure 1: 
# predict the values of the response variables using the selected models above, with minimum, mean and maximum level of naturalness, and the full range of temperature S-statistics, 
# the other variables are set to their mean values. Note that all variables are standardized.

# Abundance:

res.rma.Abund.Continuous_selected_minNat <- data.frame(predict(res.rma.Abund.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.Abund.m$PTot_S.s),-1.9755,  mean(DATA.Abund.m$Lat.s),
                                                                                                                mean(DATA.Abund.m$StudyLength.s), -3.0897:4.1233* -1.9755)))
res.rma.Abund.Continuous_selected_meanNat <- data.frame(predict(res.rma.Abund.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.Abund.m$PTot_S.s),0,  mean(DATA.Abund.m$Lat.s),
                                                                                                                 mean(DATA.Abund.m$StudyLength.s), -3.0897:4.1233* 0)))
res.rma.Abund.Continuous_selected_maxNat <- data.frame(predict(res.rma.Abund.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.Abund.m$PTot_S.s),0.9877 ,  mean(DATA.Abund.m$Lat.s),
                                                                                                                mean(DATA.Abund.m$StudyLength.s), -3.0897:4.1233* 0.9877 )))
res.rma.Abund.Continuous_selected_minNat$Temp <- c(-3.0897:4.1233)
res.rma.Abund.Continuous_selected_minNat$Nat <- "min"
res.rma.Abund.Continuous_selected_meanNat$Temp <- c(-3.0897:4.1233)
res.rma.Abund.Continuous_selected_meanNat$Nat <-"mean"
res.rma.Abund.Continuous_selected_maxNat$Temp <- c(-3.0897:4.1233)
res.rma.Abund.Continuous_selected_maxNat$Nat <- "max"

Table_interaction_plot_Ab <- rbind(data.frame(res.rma.Abund.Continuous_selected_minNat), data.frame(res.rma.Abund.Continuous_selected_meanNat), data.frame(res.rma.Abund.Continuous_selected_maxNat))
write.table(Table_interaction_plot_Ab, "Table_interaction_plot_Ab.csv", sep =";")


# Richness:

res.rma.NTaxa.Continuous_selected_minNat <- data.frame(predict(res.rma.NTaxa.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.NTaxa.m$PTot_S.s),-1.9755,  mean(DATA.NTaxa.m$Lat.s),  mean(DATA.NTaxa.m$Lon.s),  mean(DATA.NTaxa.m$Alt.Log.s),
                                                                                                                mean(DATA.NTaxa.m$StudyLength.s), -3.0897:4.1233* -1.9755)))
res.rma.NTaxa.Continuous_selected_meanNat <- data.frame(predict(res.rma.NTaxa.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.NTaxa.m$PTot_S.s),0,  mean(DATA.NTaxa.m$Lat.s),  mean(DATA.NTaxa.m$Lon.s),  mean(DATA.NTaxa.m$Alt.Log.s),
                                                                                                                 mean(DATA.NTaxa.m$StudyLength.s), -3.0897:4.1233* 0)))
res.rma.NTaxa.Continuous_selected_maxNat <- data.frame(predict(res.rma.NTaxa.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.NTaxa.m$PTot_S.s),0.9877 ,  mean(DATA.NTaxa.m$Lat.s),  mean(DATA.NTaxa.m$Lon.s),  mean(DATA.NTaxa.m$Alt.Log.s),
                                                                                                                mean(DATA.NTaxa.m$StudyLength.s), -3.0897:4.1233* 0.9877 )))
res.rma.NTaxa.Continuous_selected_minNat$Temp <- c(-3.0897:4.1233)
res.rma.NTaxa.Continuous_selected_minNat$Nat <- "min"
res.rma.NTaxa.Continuous_selected_meanNat$Temp <- c(-3.0897:4.1233)
res.rma.NTaxa.Continuous_selected_meanNat$Nat <-"mean"
res.rma.NTaxa.Continuous_selected_maxNat$Temp <- c(-3.0897:4.1233)
res.rma.NTaxa.Continuous_selected_maxNat$Nat <- "max"

Table_interaction_plot_NTaxa <- rbind(data.frame(res.rma.NTaxa.Continuous_selected_minNat), data.frame(res.rma.NTaxa.Continuous_selected_meanNat), data.frame(res.rma.NTaxa.Continuous_selected_maxNat))
write.table(Table_interaction_plot_NTaxa, "Table_interaction_plot_NTaxa.csv", sep =";")


# Turnover:

res.rma.Turn.Continuous_selected_minNat <- data.frame(predict(res.rma.Turn.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.Turn.m$PTot_S.s),-1.9755,  mean(DATA.Turn.m$Lat.s),  mean(DATA.Turn.m$Lon.s),  mean(DATA.Turn.m$Alt.Log.s),
                                                                                                              mean(DATA.Turn.m$StudyLength.s), -3.0897:4.1233* -1.9755)))
res.rma.Turn.Continuous_selected_meanNat <- data.frame(predict(res.rma.Turn.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.Turn.m$PTot_S.s),0,  mean(DATA.Turn.m$Lat.s),  mean(DATA.Turn.m$Lon.s),  mean(DATA.Turn.m$Alt.Log.s),
                                                                                                               mean(DATA.Turn.m$StudyLength.s), -3.0897:4.1233* 0)))
res.rma.Turn.Continuous_selected_maxNat <- data.frame(predict(res.rma.Turn.Continuous_selected, newmods=cbind(-3.0897:4.1233, mean(DATA.Turn.m$PTot_S.s),0.9877 ,  mean(DATA.Turn.m$Lat.s),  mean(DATA.Turn.m$Lon.s),  mean(DATA.Turn.m$Alt.Log.s),
                                                                                                              mean(DATA.Turn.m$StudyLength.s), -3.0897:4.1233* 0.9877 )))
res.rma.Turn.Continuous_selected_minNat$Temp <- c(-3.0897:4.1233)
res.rma.Turn.Continuous_selected_minNat$Nat <- "min"
res.rma.Turn.Continuous_selected_meanNat$Temp <- c(-3.0897:4.1233)
res.rma.Turn.Continuous_selected_meanNat$Nat <-"mean"
res.rma.Turn.Continuous_selected_maxNat$Temp <- c(-3.0897:4.1233)
res.rma.Turn.Continuous_selected_maxNat$Nat <- "max"

Table_interaction_plot_Turn <- rbind(data.frame(res.rma.Turn.Continuous_selected_minNat), data.frame(res.rma.Turn.Continuous_selected_meanNat), data.frame(res.rma.Turn.Continuous_selected_maxNat))
write.table(Table_interaction_plot_Turn, "Table_interaction_plot_Turn.csv", sep =";")


# Sensitivity analysis ----------------------------------------------------

# Randomly select datasets for overepresented groups and re-run models (repeat 5 times): 

#Subset only 8 Aquatic invertebrates from Boreal region:
set.seed(100)
DATA_Boreal_AqInv_Sel1 <- sample(DATA[DATA$Biogeoregion=="Boreal" & DATA$TaxonomicGroup == "InvertebratesA","TimeSeries"], size=8, replace=F)

set.seed(111)
DATA_Boreal_AqInv_Sel2 <- sample(DATA[DATA$Biogeoregion=="Boreal" & DATA$TaxonomicGroup == "InvertebratesA","TimeSeries"], size=8, replace=F)

set.seed(122)
DATA_Boreal_AqInv_Sel3 <- sample(DATA[DATA$Biogeoregion=="Boreal" & DATA$TaxonomicGroup == "InvertebratesA","TimeSeries"], size=8, replace=F)

set.seed(133)
DATA_Boreal_AqInv_Sel4 <- sample(DATA[DATA$Biogeoregion=="Boreal" & DATA$TaxonomicGroup == "InvertebratesA","TimeSeries"], size=8, replace=F)

set.seed(144)
DATA_Boreal_AqInv_Sel5 <- sample(DATA[DATA$Biogeoregion=="Boreal" & DATA$TaxonomicGroup == "InvertebratesA","TimeSeries"], size=8, replace=F)



#Subset only 14 Terrestrial invertebrates from Atlantic region:
set.seed(100)
DATA_Atlantic_TerrInv_Sel1 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=14, replace=F)

set.seed(111)
DATA_Atlantic_TerrInv_Sel2 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=14, replace=F)

set.seed(122)
DATA_Atlantic_TerrInv_Sel3 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=14, replace=F)

set.seed(133)
DATA_Atlantic_TerrInv_Sel4 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=14, replace=F)

set.seed(144)
DATA_Atlantic_TerrInv_Sel5 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=14, replace=F)


#Subset only 5 plants from Alpine region:
set.seed(100)
DATA_Alpine_Vegetation_Sel1 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=5, replace=F)

set.seed(111)
DATA_Alpine_Vegetation_Sel2 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=5, replace=F)

set.seed(122)
DATA_Alpine_Vegetation_Sel3 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=5, replace=F)

set.seed(133)
DATA_Alpine_Vegetation_Sel4 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=5, replace=F)

set.seed(144)
DATA_Alpine_Vegetation_Sel5 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=5, replace=F)


# Combine original DATA datasets  with only randomly  selected data for the over-represented groups:

DATA_Boreal_AqInv <- DATA[DATA$Biogeoregion=="Boreal" & DATA$TaxonomicGroup == "InvertebratesA",]
DATA_without_Boreal_AqInv <- DATA[!DATA$TimeSeries %in% DATA_Boreal_AqInv$TimeSeries,]
DATA_Boreal_AqInv_Sel_1 <- rbind(DATA_without_Boreal_AqInv,DATA[DATA$TimeSeries %in% DATA_Boreal_AqInv_Sel1,] )
DATA_Boreal_AqInv_Sel_2 <- rbind(DATA_without_Boreal_AqInv,DATA[DATA$TimeSeries %in% DATA_Boreal_AqInv_Sel2,] )
DATA_Boreal_AqInv_Sel_3 <- rbind(DATA_without_Boreal_AqInv,DATA[DATA$TimeSeries %in% DATA_Boreal_AqInv_Sel3,] )
DATA_Boreal_AqInv_Sel_4 <- rbind(DATA_without_Boreal_AqInv,DATA[DATA$TimeSeries %in% DATA_Boreal_AqInv_Sel4,] )
DATA_Boreal_AqInv_Sel_5 <- rbind(DATA_without_Boreal_AqInv,DATA[DATA$TimeSeries %in% DATA_Boreal_AqInv_Sel5,] )

DATA_Atlantic_TerrInv <- DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT",]
DATA_without_Atlantic_TerrInv <- DATA[!DATA$TimeSeries %in% DATA_Atlantic_TerrInv$TimeSeries,]
DATA_Atlantic_TerrInv_Sel_1 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel1,] )
DATA_Atlantic_TerrInv_Sel_2 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel2,] )
DATA_Atlantic_TerrInv_Sel_3 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel3,] )
DATA_Atlantic_TerrInv_Sel_4 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel4,] )
DATA_Atlantic_TerrInv_Sel_5 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel5,] )

DATA_Alpine_Vegetation <- DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation",]
DATA_without_Alpine_Vegetation <- DATA[!DATA$TimeSeries %in% DATA_Alpine_Vegetation$TimeSeries,]
DATA_Alpine_Vegetation_Sel_1 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel1,] )
DATA_Alpine_Vegetation_Sel_2 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel2,] )
DATA_Alpine_Vegetation_Sel_3 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel3,] )
DATA_Alpine_Vegetation_Sel_4 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel4,] )
DATA_Alpine_Vegetation_Sel_5 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel5,] )


# Re-run models:
# Run the models below 3 times, after selecting the data referring to (1) Boreal_AqInv, (2) Alpine_Vegetation and (3) Atlantic_TerrInv:
DATASET_SA1 <- DATA_Alpine_Vegetation_Sel_1  #  DATA_Atlantic_TerrInv_Sel_1 # DATA_Boreal_AqInv_Sel_1
DATASET_SA2 <- DATA_Alpine_Vegetation_Sel_2  #  DATA_Atlantic_TerrInv_Sel_2 # DATA_Boreal_AqInv_Sel_2
DATASET_SA3 <- DATA_Alpine_Vegetation_Sel_3  #  DATA_Atlantic_TerrInv_Sel_3 # DATA_Boreal_AqInv_Sel_3
DATASET_SA4 <- DATA_Alpine_Vegetation_Sel_4  #  DATA_Atlantic_TerrInv_Sel_4 # DATA_Boreal_AqInv_Sel_4
DATASET_SA5 <- DATA_Alpine_Vegetation_Sel_5  #  DATA_Atlantic_TerrInv_Sel_5 # DATA_Boreal_AqInv_Sel_5


# Abundance:
res.rma.Abund.Biota_SA1=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA1)
res.rma.Abund.Biota_SA2=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA2)
res.rma.Abund.Biota_SA3=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA3)
res.rma.Abund.Biota_SA4=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA4)
res.rma.Abund.Biota_SA5=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA5)

# Richness:
res.rma.NTaxa.Biota_SA1=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA1)
res.rma.NTaxa.Biota_SA2=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA2)
res.rma.NTaxa.Biota_SA3=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA3)
res.rma.NTaxa.Biota_SA4=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA4)
res.rma.NTaxa.Biota_SA5=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA5)

# Simpson´s diversity:
res.rma_Simp.Biota_SA1=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA1)
res.rma_Simp.Biota_SA2=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA2)
res.rma_Simp.Biota_SA3=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA3)
res.rma_Simp.Biota_SA4=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA4)
res.rma_Simp.Biota_SA5=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA5)

# Turnover:
res.rma.Turn.Biota_SA1=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA1)
res.rma.Turn.Biota_SA2=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA2)
res.rma.Turn.Biota_SA3=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA3)
res.rma.Turn.Biota_SA4=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA4)
res.rma.Turn.Biota_SA5=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~TaxonomicGroup-1, data= DATASET_SA5)



## Biogeoregions:
# sample 6 Alpine plants, and 8 Atlantic terrestrial invertebrates

#Subset only 8 Terrestrial invertebrates from Atlantic region:
set.seed(200)
DATA_Atlantic_TerrInv_Sel1 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=8, replace=F)

set.seed(211)
DATA_Atlantic_TerrInv_Sel2 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=8, replace=F)

set.seed(222)
DATA_Atlantic_TerrInv_Sel3 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=8, replace=F)

set.seed(233)
DATA_Atlantic_TerrInv_Sel4 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=8, replace=F)

set.seed(244)
DATA_Atlantic_TerrInv_Sel5 <- sample(DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT","TimeSeries"], size=8, replace=F)


#Subset only 6 plants from Alpine region:
set.seed(200)
DATA_Alpine_Vegetation_Sel1 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=6, replace=F)

set.seed(211)
DATA_Alpine_Vegetation_Sel2 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=6, replace=F)

set.seed(222)
DATA_Alpine_Vegetation_Sel3 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=6, replace=F)

set.seed(233)
DATA_Alpine_Vegetation_Sel4 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=6, replace=F)

set.seed(244)
DATA_Alpine_Vegetation_Sel5 <- sample(DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation","TimeSeries"], size=6, replace=F)


# Combine DATA datasets  with only selected sites for the over-represented groups:

DATA_Atlantic_TerrInv <- DATA[DATA$Biogeoregion=="Atlantic" & DATA$TaxonomicGroup == "InvertebratesT",]
DATA_without_Atlantic_TerrInv <- DATA[!DATA$TimeSeries %in% DATA_Atlantic_TerrInv$TimeSeries,]
DATA_Atlantic_TerrInv_Sel_1 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel1,] )
DATA_Atlantic_TerrInv_Sel_2 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel2,] )
DATA_Atlantic_TerrInv_Sel_3 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel3,] )
DATA_Atlantic_TerrInv_Sel_4 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel4,] )
DATA_Atlantic_TerrInv_Sel_5 <- rbind(DATA_without_Atlantic_TerrInv,DATA[DATA$TimeSeries %in% DATA_Atlantic_TerrInv_Sel5,] )

DATA_Alpine_Vegetation <- DATA[DATA$Biogeoregion=="Alpine" & DATA$TaxonomicGroup == "Vegetation",]
DATA_without_Alpine_Vegetation <- DATA[!DATA$TimeSeries %in% DATA_Alpine_Vegetation$TimeSeries,]
DATA_Alpine_Vegetation_Sel_1 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel1,] )
DATA_Alpine_Vegetation_Sel_2 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel2,] )
DATA_Alpine_Vegetation_Sel_3 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel3,] )
DATA_Alpine_Vegetation_Sel_4 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel4,] )
DATA_Alpine_Vegetation_Sel_5 <- rbind(DATA_without_Alpine_Vegetation,DATA[DATA$TimeSeries %in% DATA_Alpine_Vegetation_Sel5,] )


# Re-run models
# Run the models below 2 times, after selecting the data referring to (1) Atlantic_TerrInv and (2) Alpine_Vegetation:

DATASET_SA1 <- DATA_Atlantic_TerrInv_Sel_1 # DATA_Alpine_Vegetation_Sel_1
DATASET_SA2 <- DATA_Atlantic_TerrInv_Sel_2 # DATA_Alpine_Vegetation_Sel_2
DATASET_SA3 <- DATA_Atlantic_TerrInv_Sel_3 # DATA_Alpine_Vegetation_Sel_3
DATASET_SA4 <- DATA_Atlantic_TerrInv_Sel_4 # DATA_Alpine_Vegetation_Sel_4
DATASET_SA5 <- DATA_Atlantic_TerrInv_Sel_5 # DATA_Alpine_Vegetation_Sel_5

# Abundance:
res.rma.Abund.Biota_SA1=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA1)
res.rma.Abund.Biota_SA2=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA2)
res.rma.Abund.Biota_SA3=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA3)
res.rma.Abund.Biota_SA4=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA4)
res.rma.Abund.Biota_SA5=rma(yi = Abund_S, vi = Abund_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA5)

# Richness:
res.rma.NTaxa.Biota_SA1=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA1)
res.rma.NTaxa.Biota_SA2=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA2)
res.rma.NTaxa.Biota_SA3=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA3)
res.rma.NTaxa.Biota_SA4=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA4)
res.rma.NTaxa.Biota_SA5=rma(yi = NTaxa_S, vi = NTaxa_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA5)

# Simpson´s diversity:
res.rma_Simp.Biota_SA1=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA1)
res.rma_Simp.Biota_SA2=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA2)
res.rma_Simp.Biota_SA3=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA3)
res.rma_Simp.Biota_SA4=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA4)
res.rma_Simp.Biota_SA5=rma(yi = Simp_S, vi = Simp_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA5)

# Turnover:
res.rma.Turn.Biota_SA1=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA1)
res.rma.Turn.Biota_SA2=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA2)
res.rma.Turn.Biota_SA3=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA3)
res.rma.Turn.Biota_SA4=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA4)
res.rma.Turn.Biota_SA5=rma.uni(yi = Turn_S, vi = Turn_var, method="REML", mods=~Biogeoregion-1, data= DATASET_SA5)
