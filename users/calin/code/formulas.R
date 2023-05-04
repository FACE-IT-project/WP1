# Code/formulas.R
# folders with formulas use for the FACE-IT project

# Set up ------------------------------------------------------------------
library(tidyverse)


# Formulas ----------------------------------------------------------------
# Name to latin + english one
## Use it as -> mutate(nomsp = map(nomSpecies, latin_eng))
latin_eng <- function(nomSpecies){
  # Fish |FIS|
  if(nomSpecies == "Benthosema glaciale") nom_long <- "|FIS| Benthosema glaciale (glacier lantern fish)"
  if(nomSpecies == "Boreogadus saida") nom_long <- "|FIS| Boreogadus saida (polar cod)"
  if(nomSpecies == "Clupea harengus") nom_long <- "|FIS| Clupea harengus (herring)"
  if(nomSpecies == "Gadus morhua") nom_long <- "|FIS| Gadus morhua (northeast arctic cod)"
  if(nomSpecies == "Mallotus villosus") nom_long <- "|FIS| Mallotus villosus (capelin)"
  if(nomSpecies == "Sebastes mentella") nom_long <- "|FIS| Sebastes mentella (beaked redfish)"
  if(nomSpecies == "Sebastes norvegicus") nom_long <- "|FIS| Sebastes norvegicus (golden redfish)"
  # Mammal |MAM|
  if(nomSpecies == "Cystophora cristata") nom_long <- "|MAM| Cystophora cristata (hooded seal)"
  if(nomSpecies == "Megaptera novaeangliae") nom_long <- "|MAM| Megaptera novaeangliae (humpback whale)"
  if(nomSpecies == "Odobenus marinus") nom_long <- "|MAM| Odobenus marinus (walrus)"
  if(nomSpecies == "Pagophilus groenlandicus") nom_long <- "|MAM| Pagophilus groenlandicus (harp seal)"
  if(nomSpecies == "Ursus maritimus") nom_long <- "|MAM| Ursus maritimus (polar bear)"
  # Sea Bird |SBI|
  if(nomSpecies == "Alca torda") nom_long <- "|SBI| Alca torda (razorbill)"
  if(nomSpecies == "Alle alle") nom_long <- "|SBI| Alle alle (little auk)"
  if(nomSpecies == "Cepphus grylle") nom_long <- "|SBI| Cepphus grylle (black guillemot)"
  if(nomSpecies == "Chroicocephalus ridibundus") nom_long <- "|SBI| Chroicocephalus ridibundus (black-headed gull)"
  if(nomSpecies == "Falco peregrinus") nom_long <- "|SBI| Falco peregrinus (peregrine falcon)"
  if(nomSpecies == "Fratercula arctica") nom_long <- "|SBI| Fratercula arctica (atlantic puffin)"
  if(nomSpecies == "Fulmarus glacialis") nom_long <- "|SBI| Fulmarus glacialis (northern fulmar)"
  if(nomSpecies == "Gavia adamsii") nom_long <- "|SBI| Gavia adamsii (white-billed diver)"
  if(nomSpecies == "Gavia immer") nom_long <- "|SBI| Gavia immer (great northern diver)"
  if(nomSpecies == "Gulosus aristotelis") nom_long <- "|SBI| Gulosus aristotelis (european shag)"
  if(nomSpecies == "Haliaeetus albicilla") nom_long <- "|SBI| Haliaeetus albicilla (white-tailed eagle)"
  if(nomSpecies == "Larus argentatus") nom_long <- "|SBI| Larus argentatus (herring gull)"
  if(nomSpecies == "Larus canus") nom_long <- "|SBI| Larus canus (common gull)"
  if(nomSpecies == "Larus delawarensis") nom_long <- "|SBI| Larus delawarensis (ring-billed gull)"
  if(nomSpecies == "Larus fuscus") nom_long <- "|SBI| Larus fuscus (lesser black-backed gull)"
  if(nomSpecies == "Larus glaucoides") nom_long <- "|SBI| Larus glaucoides (iceland gull)"
  if(nomSpecies %in% c("Larus hyperboreus", "Larus hypeboreus", "GLGU")) nom_long <- "|SBI| Larus hyperboreus (glaucous gull)"
  if(nomSpecies == "Larus marinus") nom_long <- "|SBI| Larus marinus (great black-backed gull)"
  if(nomSpecies == "Larus philadelphia") nom_long <- "|SBI| Larus philadelphia (bonaparte’s gulls)"
  if(nomSpecies == "Larus smithonianus") nom_long <- "|SBI| Larus smithonianus (american herring gull)"
  if(nomSpecies %in% c("Larus sp.", "Larus glaucoides/hyperboreus")) nom_long <- "|SBI| Larus sp. (gull unidentified)"
  if(nomSpecies == "Melanitta fusca") nom_long <- "|SBI| Melanitta fusca (velvet scoter)"
  if(nomSpecies == "Melanitta nigra") nom_long <- "|SBI| Melanitta nigra (black scoter)"
  if(nomSpecies == "Morus bassanus") nom_long <- "|SBI| Morus bassanus (northern gannet)"
  if(nomSpecies == "Oceanodroma leucorhoa") nom_long <- "|SBI| Oceanodroma leucorhoa (leach’s storm-petrels)"
  if(nomSpecies == "Pagophila eburnea") nom_long <- "|SBI| Pagophila eburnea (ivory gull)"
  if(nomSpecies == "Phalacrocorax auritus") nom_long <- "|SBI| Phalacrocorax auritus (double-crested cormorant)"
  if(nomSpecies == "Phalacrocorax carbo") nom_long <- "|SBI| Phalacrocorax carbo (great cormorant)"
  if(nomSpecies == "Phalaropus fulicarius") nom_long <- "|SBI| Phalaropus fulicarius (red phalarope)"
  if(nomSpecies == "Podiceps grisegena") nom_long <- "|SBI| Podiceps grisegena (red-necked grebe)"
  if(nomSpecies == "Polysticta stelleri") nom_long <- "|SBI| Polysticta stelleri (steller’s duck)"
  if(nomSpecies == "Puffinus gravisn") nom_long <- "|SBI| Puffinus gravisn (greater shearwater)"
  if(nomSpecies == "Puffinus puffinus") nom_long <- "|SBI| Puffinus puffinus (manx shearwater)"
  if(nomSpecies == "Rissa tridactyla") nom_long <- "|SBI| Rissa tridactyla (black-legged kittiwake)"
  if(nomSpecies %in% c("Somateria mollissima", "Somateria mollissima borealis", "Sommateria mollissima")) nom_long <- "|SBI| Somateria mollissima (common eider)"
  if(nomSpecies == "Somateria spectabilis") nom_long <- "|SBI| Somateria spectabilis (king eider)"
  if(nomSpecies == "Stercorarius longicaudus") nom_long <- "|SBI| Stercorarius longicaudus (long-tailed skua)"
  if(nomSpecies == "Stercorarius parasiticus") nom_long <- "|SBI| Stercorarius parasiticus (arctic skua)"
  if(nomSpecies == "Stercorarius skua") nom_long <- "|SBI| Stercorarius skua (great skua)"
  if(nomSpecies == "Sterna hirundo") nom_long <- "|SBI| Sterna hirundo (common tern)"
  if(nomSpecies == "Sterna paradisaea") nom_long <- "|SBI| Sterna paradisaea (arctic tern)"
  if(nomSpecies == "Uria aalge") nom_long <- "|SBI| Uria aalge (common guillemot)"
  if(nomSpecies == "Uria lomvia") nom_long <- "|SBI| Uria lomvia (brünnich’s guillemot)"
  if(nomSpecies == "Xema sabini") nom_long <- "|SBI| Xema sabini (sabine’s gull)"
  # Non-sea Bird |NBI|
  if(nomSpecies == "Acanthis hornemanni") nom_long <- "|NBI| Acanthis hornemanni(arctic redpoll)"
  if(nomSpecies %in% c("Carduelis flammea","RP")) nom_long <- "|NBI| Carduelis flammea (common redpoll)"
  if(nomSpecies %in% c("Lagopus muta", "Lagopus mutus")) nom_long <- "|NBI| Lagopus mutus (rock ptarmigan)"
  if(nomSpecies %in% c("Plectrophenax nivalis","SB")) nom_long <- "|NBI| Plectrophenax nivalis (snow bunting)"
  # Bird |BIR
  if(nomSpecies == "Anas platyrhynchos") nom_long <- "|BIR| Anas platyrhynchos (mallard)"
  if(nomSpecies == "Anser brachyrhynchus") nom_long <- "|BIR| Anser brachyrhynchus (pink-footed goose)"
  if(nomSpecies == "Arenaria interpres") nom_long <- "|BIR| Arenaria interpres (ruddy turnstone)"
  if(nomSpecies == "Branta Canadensis") nom_long <- "|BIR| Branta canadensis (canada goose)"
  if(nomSpecies == "Branta bernicla") nom_long <- "|BIR| Branta bernicla (brant goose)"
  if(nomSpecies == "Branta leucopsis") nom_long <- "|BIR| Branta leucopsis (barnacle goose)"
  if(nomSpecies %in% c("Bubo scandiacus", "Bubo scandiaca")) nom_long <- "|BIR| Bubo scandiacus (snowy owl)"
  if(nomSpecies %in% c("Calcarius lapponicus","LB")) nom_long <- "|BIR| Calcarius lapponicus (lapland longspur)"
  if(nomSpecies == "Calidris alba") nom_long <- "|BIR| Calidris alba (sanderling)"
  if(nomSpecies == "Calidris alpina") nom_long <- "|BIR| Calidris alpina (dunlin)"
  if(nomSpecies == "Calidris canutus") nom_long <- "|BIR| Calidris canutus (red knot)"
  if(nomSpecies == "Calidris maritima") nom_long <- "|BIR| Calidris maritima (purple sandpiper)"
  if(nomSpecies == "Calidris melanotos") nom_long <- "|BIR| Calidris melanotos (pectoral sandpiper)"
  if(nomSpecies == "Charadrius hiaticula") nom_long <- "|BIR| Charadrius hiaticula (common ringed plover)"
  if(nomSpecies == "Clangula hyemalis") nom_long <- "|BIR| Clangula hyemalis (long-tailed duck)"
  if(nomSpecies == "Corvus corax") nom_long <- "|BIR| Corvus corax (raven)"
  if(nomSpecies %in% c("Gavia stallata", "Gavia stellata")) nom_long <- "|BIR| Gavia stallata (red-throated loon)"
  if(nomSpecies == "Histrionicus histrionicus") nom_long <- "|BIR| Histrionicus histrionicus (harlequin duck)"
  if(nomSpecies == "Mergus serratus") nom_long <- "|BIR| Mergus serratus (red-breasted merganser)"
  if(nomSpecies %in% c("Oenanthe oenanthe","NW")) nom_long <- "|BIR| Oenanthe oenanthe (northern wheatear)"
  if(nomSpecies == "Phalaropus lobatus") nom_long <- "|BIR| Phalaropus lobatus (red-necked phalarope)"
  if(nomSpecies == "Pluvialis apricaria") nom_long <- "|BIR| Pluvialis apricaria (european golden plover)"
  # Zooplankton |ZOO|
  if(nomSpecies %in% c("Calanus finmarchicus","atlantic species")) nom_long <- "|ZOO| Calanus finmarchicus (atlantic calanus)"
  if(nomSpecies %in% c("Calanus glacialis","arctic species")) nom_long <- "|ZOO| Calanus glacialis (arctic calanus)"
  if(nomSpecies == "Zooplankton") nom_long <- "|ZOO| Zooplankton (Zooplankton)"
  # Other
  if(nomSpecies %in% c("", "NA", "NULL")) nom_long <- NA
  return(nom_long)
}








# Function for converting UTM to decimal degrees
# NB: Expects an 'Easting' and 'Northing' column
convert_UTM_deg <- function(df, utm_zone){
  
  # Fill NA with 0 for ease of use
  df$Easting <- replace_na(df$Easting, 0)
  df$Northing <- replace_na(df$Northing, 0)
  
  # Convert coordinates to lon/lat
  suppressWarnings(
    utmcoor <- sp::SpatialPoints(cbind(df$Easting, df$Northing), 
                                 proj4string = sp::CRS(paste0("+proj=utm +zone=",utm_zone)))
  )
  suppressWarnings(
    longlatcoor <- sp::spTransform(utmcoor, sp::CRS("+proj=longlat"))
  )
  
  # Attach to data.frame and replace 0 with NA
  df$lon <- sp::coordinates(longlatcoor)[,1]
  df$lon[df$Easting == 0] <- NA
  df$Easting[df$Easting == 0] <- NA
  df$lat <- sp::coordinates(longlatcoor)[,2]
  df$lat[df$Northing == 0] <- NA
  df$Northing[df$Northing == 0] <- NA
  return(df)
}






#####################################################################################################
# This code came from : https://github.com/Biodiversity-trends-in-Europe-ILTER/R-code/blob/master/functions/My_mmkh
# Modified version of the function mmkh (R package: modifiedmk; Patakamuri & O’Brien, 2019. Modified versions of Mann Kendall and Spearman’s Rho trend tests.)
# The only difference from the original function is at line 87, to include S and n in the output
#####################################################################################################

My.mmkh=function (x, ci = 0.95) 
{
  x = x
  z = NULL
  z0 = NULL
  pval = NULL
  pval0 = NULL
  S = 0
  Tau = NULL
  essf = NULL
  ci = ci
  if (is.vector(x) == FALSE) {
    stop("Input data must be a vector")
  }
  if (any(is.finite(x) == FALSE)) {
    x <- x[-c(which(is.finite(x) == FALSE))]
    warning("The input vector contains non-finite numbers. An attempt was made to remove them")
  }
  n <- length(x)
  V <- rep(NA, n * (n - 1)/2)
  k = 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      k = k + 1
      V[k] = (x[j] - x[i])/(j - i)
    }
  }
  slp <- median(V, na.rm = TRUE)
  t = 1:length(x)
  xn <- (x[1:n]) - ((slp) * (t))
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      S = S + sign(x[j] - x[i])
    }
  }
  ro <- acf(rank(xn), lag.max = (n - 1), plot = FALSE)$acf[-1]
  sig <- qnorm((1 + ci)/2)/sqrt(n)
  rof <- rep(NA, length(ro))
  for (i in 1:(length(ro))) {
    if (ro[i] > sig || ro[i] < -sig) {
      rof[i] <- ro[i]
    }
    else {
      rof[i] = 0
    }
  }
  cte <- 2/(n * (n - 1) * (n - 2))
  ess = 0
  for (i in 1:(n - 1)) {
    ess = ess + (n - i) * (n - i - 1) * (n - i - 2) * rof[i]
  }
  essf = 1 + ess * cte
  var.S = n * (n - 1) * (2 * n + 5) * (1/18)
  if (length(unique(x)) < n) {
    aux <- unique(x)
    for (i in 1:length(aux)) {
      tie <- length(which(x == aux[i]))
      if (tie > 1) {
        var.S = var.S - tie * (tie - 1) * (2 * tie + 
                                             5) * (1/18)
      }
    }
  }
  VS = var.S * essf
  if (S == 0) {
    z = 0
    z0 = 0
  }
  if (S > 0) {
    z = (S - 1)/sqrt(VS)
    z0 = (S - 1)/sqrt(var.S)
  }
  else {
    z = (S + 1)/sqrt(VS)
    z0 = (S + 1)/sqrt(var.S)
  }
  pval = 2 * pnorm(-abs(z))
  pval0 = 2 * pnorm(-abs(z0))
  Tau = S/(0.5 * n * (n - 1))
  return(c("Corrected Zc" = z, "new P-value" = pval, "N/N*" = essf, 
           "Original Z" = z0, "old P.value" = pval0, "Tau" = Tau, 
           "Sen s slope" = slp, "old.variance" = var.S, "new.variance" = VS, "S statistic" = S, "n" = n))
}



species_analysis_step1 <- function(df, tsname){
  DATA1 <- dcast(df, Year ~ Taxon, sum, value.var = "Density") # create cross table
  lastTaxon <- length(DATA1)
  firstTaxon <- 2
  
  DATA1_Taxa <- as.matrix(DATA1[,c(firstTaxon:lastTaxon)])
  DATA1_Taxa <- subset(DATA1[,c(firstTaxon:lastTaxon)]) 
  
  DATA1$NTaxa <- specnumber(DATA1_Taxa)  # taxonomic richness
  DATA1$Simp <- diversity(DATA1_Taxa, index = "simpson") # Simpson´s taxonomic diversity
  DATA1$Abund <- rowSums (DATA1_Taxa) # Total abundance
  DATA1_Turnover <- turnover(df, time.var = "Year", species.var = "Taxon", abundance.var = "Density" , metric = "total")
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
  
  EffectSizes_biodiv_DATA1 <- data.frame(TimeSeries = tsname, Site = df$Site[1] , Country = "DE", Lat = 50.187,	Lon = 9.100,	Alt = 122, # fill in with site information 
                                         TaxonomicGroup = "AquaticInv", Realm = "FW", Naturalness = 3, startYear = start.year, endYear = end.year,    # fill in with site information
                                         NTaxa_S = DATA1.trend.NTaxa[10], 
                                         NTaxa_var = DATA1.trend.NTaxa[8], # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]
                                         Abund_S = DATA1.trend.Abund[10], 
                                         Abund_var = DATA1.trend.Abund[8],  # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]
                                         Simp_S = DATA1.trend.Simp[10],  
                                         Simp_var = DATA1.trend.Simp[9],  # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]
                                         Turn_S = DATA1.trend.Turnover[10],  
                                         Turn_var = DATA1.trend.Turnover[8])  # if there is no temporal autocorrelation choose: [8]; if there is temporal autocorrelation choose [9]
  
}



