#############################################
### exploring WVP and PET ###################
#############################################

library(raster); library(rgdal); library(maptools)
library(png)

# load in new files
EnvVar <- stack(list.files('./Analysis_Scripts/Chapter3/Climate Data/NewWorld_current_2', full.names = T, 
                           pattern = '.tif'))
EnvVarCrop <- stack(EnvVar$current_2.5arcmin_annualPET, EnvVar$current_2.5arcmin_climaticMoistureIndex,
                    EnvVar$current_2.5arcmin_PETDriestQuarter, EnvVar$current_2.5arcmin_PETWettestQuarter)

WVP.raw <- stack(list.files('./Analysis_Scripts/Chapter3/Climate Data/wc2_WVP', full.names = T, 
                                       pattern = '.tif'))

# get average of monthly values
WVP.average <- calc(WVP.raw, fun = mean)
#plot(WVP.average)

# cant get cloud cover data out of the file that it is in
# wierd png from google idk?

#load polys
Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp")

#crop all by the same extent
WVP.cropp <- crop(WVP.average, EnvVarCrop)
#plot(WVP.cropp)
names(WVP.cropp) <- "WVP"
newdata <- stack(WVP.cropp, EnvVarCrop)

# 5 new variables
# WVP Water Vapor Pressure
# PET Potential Evapotranspiration
# CM Climate moisture index
# PETD Potential Evapotranspiration driest quarter
# PETW Potential Evapotranpiration wettest quarter

# drop spp outside extent like korea and europe
# 187, 189, 190, 191, 192, 195, 196, 200
# Hydromantes ambrosii, Hydromantes flavus, Hydromantes genei       
# Hydromantes imperialis, Hydromantes italicus, Hydromantes strinatii  
# Hydromantes supramontis, Karsenia koreana  

# 
arg <- raster::extract(newdata, Polygons[214,])
arg

drop <- c(187,189,190,191,192,195,196,200)
drop
Polygons1 <- Polygons[-drop,]
Polygons1

RecordsN <- matrix(NA, nrow = 303, ncol = 31)
colnames(RecordsN) <- c("binomial","WVPMin","WVP1Q","WVPMed","WVPMea","WVP3Q","WVPMax",
                       "PETMin","PET1Q","PETMed","PETMea","PET3Q","PETMax",
                       "CMMin","CM1Q","CMMed","CMMea","CM3Q","CMMax",
                       "PETDMin","PETD1Q","PETDMed","PETDMea","PETD3Q","PETDMax",
                       "PETWMin","PETW1Q","PETWMed","PETWMea","PETW3Q","PETWMax")

RecordsNDF <- as.data.frame(RecordsN)

for (i in 1:303) {
  #i <- 4
  envSpecies <- data.frame(raster::extract(newdata, Polygons1[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  RecordsNDF[i,] <-  c(as.character(Polygons1[i,]$binomial),summary(envSpecies$WVP),summary(envSpecies$current_2.5arcmin_annualPET),
                       summary(envSpecies$current_2.5arcmin_climaticMoistureIndex),summary(envSpecies$current_2.5arcmin_PETDriestQuarter),
                       summary(envSpecies$current_2.5arcmin_PETWettestQuarter))
  print(i)
}

RecordsNDF
length(RecordsNDF$binomial)
write.csv(RecordsNDF, file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/NewData_IUCNLMRecords.csv")

NDF <- read.csv("Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/NewData_IUCNLMRecords.csv")
NDF
NDF[3:31]


# either match names only or cut old names and then combine

Old <- read.csv("Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Bioclim_IUCNLMRecords.csv")
Old

# drop spp outside extent like korea and europe
# 187, 189, 190, 191, 192, 195, 196, 200
# Hydromantes ambrosii, Hydromantes flavus, Hydromantes genei       
# Hydromantes imperialis, Hydromantes italicus, Hydromantes strinatii  
# Hydromantes supramontis, Karsenia koreana  
Old$binomial[200]
Old[187, ]

drop <- c(187,189,190,191,192,195,196,200)
drop
Old1 <- Old[-drop,]
Old1$binomial

Old1$binomial[300]
NDF$binomial[300]
NDF

match(NDF$binomial,Old$binomial)

NewOldData <- c(Old1, NDF[3:32])
NewOldData <- as.data.frame(NewOldData)
NewOldData
write.csv(NewOldData, file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Bioclim_addition_all_poly_Records.csv")


## cc
Cloudd <- read.csv(file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Cloud_cover.csv")
drop <- c(187,189,190,191,192,195,196,200)
drop
Cloud1 <- Cloudd[-drop, ]
Keep <- Cloud1[,3:8]
Cloud1$binomial
OldData$binomial

OldData <- read.csv(file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Bioclim_addition_all_poly_Records.csv")
OldData

maybee <- c(OldData,Keep)
maybee
wahoo <- as.data.frame(maybee)
wahoo
anyNA(wahoo)
write.csv(wahoo, file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Cloud_addition_all_poly_Records.csv")



