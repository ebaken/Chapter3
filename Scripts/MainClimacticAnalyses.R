##########################################################
##########################################################
######## USING DATA TO OBTAIN CLIMATE VARIABLES ##########
##########################################################
##########################################################

# Packages
library(sp);library(tidyr);library(dplyr);library(maps);library(ape)
library(stringr);library(ggmap);library(data.table);library(jpeg)
library(tiff); library(utils);
library(rgl);library(utils)

library(geomorph) ; library(RRPP)
# Making colors ####
  Cols <- c("C" = "#FFFFBF", "S" = "#FEE08B", "F" = "#FDAE61", "A" = "#66C2A5", "W" = "deepskyblue4", "T" = "#4D004B")

# Setup and reading data for Lauren ####
setwd("/Users/Laur/Desktop/Dean Adams/Code/Dissertation/Analysis_Scripts/Chapter3/") # Lauren - working in projects means that the working directory is alreayd set to your home /Dissertation/ folder 
BB <- read.tree("/BBPruned.tre") # upload BBPruned
Env <- read.csv("AllRecordsEle.csv") # upload all environmental measures
Microhabitats <- read.csv("Lauren/Data/Raw/HabitatsPruned copy.csv") ##### NOT UPDATED CLASSIFICATIONS ##### 

# Setup and reading data for Erica ####
BB <- read.tree("Data/Pruned/BB.PrunedToPleths.tre") # upload BBPruned
Env <- read.csv("Analysis_Scripts/Chapter4_Nick/AllRecordsEle.csv") # upload all environmental measures
Microhabitats <- read.csv("Data/Pruned/MicrohabitatsPruned.csv")

# Making files appropriately formatted ####
BB$tip.label <- str_replace_all(BB$tip.label, "_", " ") # taking out underscores from binomial
Microhabitats$Species <- as.character(Microhabitats$Species) # changing class of species name to character
Env$binomial <- as.character(Env$binomial) # changing class of binomial to character
Env <- Env[,-1] # deleting the first column that doesn't mean anything
colnames(Env)
colnames(Env)[2:7] <- c("ElevMin", "Elev1Q", "ElevMed", "ElevMean", "Elev3Q", "ElevMax") # renaming elevation variables
colnames(Env)[8:13] <- c("TempMin", "Temp1Q", "TempMed", "TempMean", "Temp3Q", "TempMax") # renaming WC1 variables [Annual Mean Temperature]
colnames(Env)[14:19] <- c("DiurnalMin", "Diurnal1Q", "DiurnalMed", "DiurnalMean", "Diurnal3Q", "DiurnalMax") # renaming WC2 variables [Mean Diurnal Range (Mean of monthly (max temp - min temp))]
colnames(Env)[20:25] <- c("IsoMin", "Iso1Q", "IsoMed", "IsoMean", "Iso3Q", "IsoMax") # renaming WC3 variables [Isothermality (BIO2/BIO7) (* 100)]
colnames(Env)[26:31] <- c("TempSeasMin", "TempSeas1Q", "TempSeasMed", "TempSeasMean", "TempSeas3Q", "TempSeasMax") # renaming WC4 variables [Temperature Seasonality (standard deviation *100)]
colnames(Env)[32:37] <- c("MaxTempMin", "MaxTemp1Q", "MaxTempMed", "MaxTempMean", "MaxTemp3Q", "MaxTempMax") # renaming WC5 variables [Max Temperature of Warmest Month]
colnames(Env)[38:43] <- c("MinTempMin", "MinTemp1Q", "MinTempMed", "MinTempMean", "MinTemp3Q", "MinTempMax") # renaming WC6 variables [Min Temperature of Coldest Month]
colnames(Env)[44:49] <- c("TempRangeMin", "TempRange1Q", "TempRangeMed", "TempRangeMean", "TempRange3Q", "TempRangeMax") # renaming WC7 variables [Temperature Annual Range (BIO5-BIO6)]
colnames(Env)[50:55] <- c("TempWetMin", "TempWet1Q", "TempWetMed", "TempWetMean", "TempWet3Q", "TempWetMax") # renaming WC8 variables [Mean Temperature of Wettest Quarter]
colnames(Env)[56:61] <- c("TempDryMin", "TempDry1Q", "TempDryMed", "TempDryMean", "TempDry3Q", "TempDryMax") # renaming WC9 variables [Mean Temperature of Driest Quarter]
colnames(Env)[62:67] <- c("WarmQTempMin", "WarmQTemp1Q", "WarmQTempMed", "WarmQTempMean", "WarmQTemp3Q", "WarmQTempMax") # renaming WC10 variables [Mean Temperature of Warmest Quarter]
colnames(Env)[68:73] <- c("ColdQTempMin", "ColdQTemp1Q", "ColdQTempMed", "ColdQTempMean", "ColdQTemp3Q", "ColdQTempMax") # renaming WC11 variables [Mean Temperature of Coldest Quarter]
colnames(Env)[74:79] <- c("PrecipMin", "Precip1Q", "PrecipMed", "PrecipMean", "Precip3Q", "PrecipMax") # renaming WC12 variables [Annual Precipitation]
colnames(Env)[80:85] <- c("PrecipWetMin", "PrecipWet1Q", "PrecipWetMed", "PrecipWetMean", "PrecipWet3Q", "PrecipWetMax") # renaming WC13 variables [Precipitation of Wettest Month]
colnames(Env)[86:91] <- c("PrecipDryMin", "PrecipDry1Q", "PrecipDryMed", "PrecipDryMean", "PrecipDry3Q", "PrecipDryMax") # renaming WC14 variables [Precipitation of Driest Month]
colnames(Env)[92:97] <- c("PrecipSeasMin", "PrecipSeas1Q", "PrecipSeasMed", "PrecipSeasMean", "PrecipSeas3Q", "PrecipSeasMax") # renaming WC15 variables [Precipitation Seasonality (Coefficient of Variation)]
colnames(Env)[98:103] <- c("WetQPrecipMin", "WetQPrecip1Q", "WetQPrecipMed", "WetQPrecipMean", "WetQPrecip3Q", "WetQPrecipMax") # renaming WC16 variables [Precipitation of Wettest Quarter]
colnames(Env)[104:109] <- c("DryQPrecipMin", "DryQPrecip1Q", "DryQPrecipMed", "DryQPrecipMean", "DryQPrecip3Q", "DryQPrecipMax") # renaming WC17 variables [Precipitation of Driest Quarter]
colnames(Env)[110:115] <- c("WarmQPrecipMin", "WarmQPrecip1Q", "WarmQPrecipMed", "WarmQPrecipMean", "WarmQPrecip3Q", "WarmQPrecipMax") # renaming WC18 variables [Precipitation of Warmest Quarter]
colnames(Env)[116:121] <- c("ColdQPrecipMin", "ColdQPrecip1Q", "ColdQPrecipMed", "ColdQPrecipMean", "ColdQPrecip3Q", "ColdQPrecipMax") # renaming WC19 variables [Precipitation of Coldest Quarter]

# which variables do other amphibian climate studies use?
      # Gomez-Mestre et al 2012 uses 1, 2, 6, and 7
      # Liedtke et al 2018 used 1
      # Fisher-Reid et al 2012 used all and did a PCA of them
      # Clay and Gifford 2018 used yearly and monthly temperature averages from each known point locality
      # Moen and Wiens 2017 used variables based on previous analyses in amphibians (e.g. Quintero and Wiens 2013 and Bonetti and Wiens 2014): 
          # 1, 5, 6, 12, 16, and 17
      # Sandoval-Comte et al 2012 did all 19 - not sure if this was good statistically, but didn't use PCA
      # Kozak and Wiens 2010 did a PCA of all of them
      # Adams and Church did annual mean tempterature
      # Dugo-Cota et al 2015 did "temperature and precipitation data"


# Matching Tree, EnvironmentalVariables, and Microhabitats ####
Drop <- BB$tip.label[which(is.na(match(BB$tip.label, Env$binomial)))]
BBPruned <- drop.tip(BB, Drop)
MicrohabitatsPruned <- Microhabitats[match(Env$binomial, Microhabitats$Species),]

# Checking all data files are in the right order and size ####
length(BBPruned$tip.label) # should be 318
nrow(MicrohabitatsPruned) # should be 318
identical(MicrohabitatsPruned$Species, Env$binomial) # should be true

# Lenient Phylogenetic ANOVA of Annual Mean Temp, Annual Mean Precip, Min Temperature of Coldest Month, and Total Precip during Dryest Month ####
Temp <- Env[,c("TempMin","TempMean", "TempMax")]
Precip <- Env[,c("PrecipMin", "PrecipMean", "PrecipMax")]
MinTemp <- Env[,c("MinTempMin","MinTempMean","MinTempMax")]
DryPrecip <- Env[,c("PrecipDryMin", "PrecipDryMean", "PrecipDryMax")]
rownames(Temp) <- Env$binomial
rownames(Precip) <- Env$binomial
rownames(MinTemp) <- Env$binomial
rownames(DryPrecip) <- Env$binomial
Habitats <- MicrohabitatsPruned$Lenient
names(Habitats) <- MicrohabitatsPruned$Species

source("Analysis_Scripts/summary.pairwise.EKB.Function.R")
TreeCov <- vcv.phylo(BBPruned)
DF <- rrpp.data.frame(habitat = Habitats, temp = Temp, precip = Precip, mintemp = MinTemp, dryprecip = DryPrecip, cov = TreeCov) 

Fit.Temp <- lm.rrpp(Temp ~ habitat, iter = 9999, Cov = TreeCov, data = DF)
ANOVA.Temp <- anova.lm.rrpp(Fit.Temp)
summary(ANOVA.Temp) # significant 0.006 - this was almost significant with Strict
Fit.Pair.Temp <- pairwise(Fit.Temp, groups = Habitats)
Fit.Pair.Temp.Summary <- summary.pairwise.EKB(Fit.Pair.Temp) # AvsT (0.0008) significant. AvsS almost significant (0.0158)
boxplot(Temp[,1] ~ Habitats)
boxplot(Temp[,2] ~ Habitats)
boxplot(Temp[,3] ~ Habitats)

Fit.MinTemp <- lm.rrpp(MinTemp ~ habitat, iter = 9999, Cov = TreeCov, data=DF)
ANOVA.MinTemp <- anova.lm.rrpp(Fit.MinTemp)
summary(ANOVA.MinTemp) # significant: 0.0019 - significant with Strict
Fit.Pair.MinTemp <- pairwise(Fit.MinTemp, groups = Habitats)
Fit.Pair.MinTemp.Summary <- summary.pairwise.EKB(Fit.Pair.MinTemp) # AvsS (0.0016) and AvsT (0.0007) significant, AvsW almost significant (0.0192) [AvsT 0.0034 with Strict]
boxplot(MinTemp[,1] ~ Habitats)
boxplot(MinTemp[,2] ~ Habitats)
boxplot(MinTemp[,3] ~ Habitats)

Fit.Precip <- lm.rrpp(Precip ~ Habitats, iter = 9999, Cov = TreeCov)
ANOVA.Precip <- anova.lm.rrpp(Fit.Precip)
summary(ANOVA.Precip) # significant: 0.0001 - this was significant with Strict
Fit.Pair.Precip <- pairwise(Fit.Precip, groups = Habitats)
Fit.Pair.Precip.Summary <- summary.pairwise.EKB(Fit.Pair.Precip)  # TONS of significant pairwise comparisons for arboreal species. Strict said AvsF is 0.0147 and AvsT is 0.0125
boxplot(Precip[,1] ~ Habitats)
boxplot(Precip[,2] ~ Habitats)
boxplot(Precip[,3] ~ Habitats)

Fit.DryPrecip <- lm.rrpp(DryPrecip ~ Habitats, iter = 9999, Cov = TreeCov)
ANOVA.DryPrecip <- anova.lm.rrpp(Fit.DryPrecip)
summary(ANOVA.DryPrecip) # significant: 0.0296 - significant with Strict
Fit.Pair.DryPrecip <- pairwise(Fit.DryPrecip, groups = Habitats)
Fit.Pair.DryPrecip.Summary <- summary.pairwise.EKB(Fit.Pair.DryPrecip)  # AvsT significant 0.0045 [AvsF 0.0016 with Strict]
boxplot(MinTemp[,1] ~ Habitats)
boxplot(MinTemp[,2] ~ Habitats)
boxplot(MinTemp[,3] ~ Habitats)

ANOVA.PVal <- rep(NA, 5)
names(ANOVA.PVal) <- c("Temp", "MinTemp", "Precip", "DryPrecip", "Elev")
ANOVA.PVal["Temp"] <- ANOVA.Temp[[1]][1,7]
ANOVA.PVal["MinTemp"] <- ANOVA.MinTemp[[1]][1,7]
ANOVA.PVal["Precip"] <- ANOVA.Precip[[1]][1,7]
ANOVA.PVal["DryPrecip"] <- ANOVA.DryPrecip[[1]][1,7]
ANOVA.PVal["Elev"] <- ANOVA.Elev[[1]][1,7]

PairwiseResults <- data.frame(rep(NA,5), rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA,5))
colnames(PairwiseResults) <- c("Temp", "MinTemp", "Precip", "DryPrecip", "Elev")
rownames(PairwiseResults) <- c("A:C", "A:F", "A:S", "A:T", "A:W")
PairwiseResults[,"Temp"] <- Fit.Pair.Temp.Summary$`Pr > d`[1:5]
PairwiseResults[,"MinTemp"] <- Fit.Pair.MinTemp.Summary$`Pr > d`[1:5]
PairwiseResults[,"Precip"] <- Fit.Pair.Precip.Summary$`Pr > d`[1:5]
PairwiseResults[,"DryPrecip"] <- Fit.Pair.DryPrecip.Summary$`Pr > d`[1:5]
PairwiseResults[,"Elev"] <- Fit.Pair.Elev.Summary$`Pr > d`[1:5]
PairwiseResults

write.csv(ANOVA.PVal, "Data/Pruned/Chapter4/ANOVA.PVal.Lenient.csv", row.names = T)
write.csv(PairwiseResults, "Data/Pruned/Chapter4/PairwisePvals.Lenient.csv", row.names = T)


# overall conclusions of these 4 bioclim variables: arboreal species need warmer, wetter environments.

# 5 pairwise tests [A compared to all C, F, S, T, and W], so bonferroni would be 0.05^5
pairwisecutoff <- 0.05/5
pairwisecutoff # bonferroni
   
# Lauren's test on elevation ####
Elev <- Env[,"ElevMean"]
names(Elev) <- Env$binomial
Habitats <- MicrohabitatsPruned$Lenient
names(Habitats) <- MicrohabitatsPruned$Species

TreeCov <- vcv.phylo(BBPruned)
DF <- rrpp.data.frame(habitat = Habitats, elev = Elev, cov = TreeCov) 

Fit.Elev <- lm.rrpp(Elev ~ Habitats, iter = 9999, Cov = TreeCov)
ANOVA.Elev <- anova.lm.rrpp(Fit.Elev)
summary(ANOVA.Elev) # significant = 0.0001 [significant:0.0056 with Strict]
Fit.Pair.Elev <- pairwise(Fit.Elev, groups = Habitats)
summary(Fit.Pair.Elev) # AvsT (p=0.0001) and AvsS (p=0.005) signfiicant  [AvsT (0.0001) significant with STrict]
boxplot(Elev ~ Habitats)
























