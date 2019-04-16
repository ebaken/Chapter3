#############################################
### AUTO VS THOUGHTFUL SELECTION ###########
##############################################

# DID NOT USE THIS IN ANALYSIS, FOR EXPLORATION MAINLY
# SET UP TO RUN ALL CLIMATIC VARIABLES COMPARED TO ONLY SOME
# BUT WE TESTED THIS INSTEAD WITH STANDARD NORMAL DEVIATES
# SO THIS STEP WAS NOT NECESSARY

library(dismo); library(raster); library(maxnet)
library(ggplot2); library(data.table); library(rgeos)
library(rgbif); library(geosphere); library(rgdal)
library(scales)

# polygons
Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

# predictor stack of variables
alldata <- stack(c('Analysis_Scripts/Chapter3/Climate Data/USETHISelevation.tif',
                   list.files('Analysis_Scripts/Chapter3/Climate Data/Salamander Range', full.names = T, pattern = '.tif')))

# crop all data by species to get robustness of predictability
BA <- Polygons[1,] # Batrahoseps altasierrae
Balta <- crop(alldata, BA)
Balta <- raster::stack(Balta)

# randomly locate background sites
randomBgSites <- randomPoints(Balta, 10000)

#extract environment at sites
randomBgEnv <- raster::extract(Balta, randomBgSites)
randomBgEnv <- as.data.frame(randomBgEnv)

PolyBgEnv <- raster::extract(alldata, BA)
PolyBgEnv <- as.data.frame(PolyBgEnv)

# remove any sites with NA for at least one variable
isNa <- is.na(rowSums(randomBgEnv))
if (any(isNa)) {
  randomBgSites <- randomBgSites[-which(isNa), ]
  randomBgEnv <- randomBgEnv[-which(isNa), ]
}

# combine with coordinates and rename coordinate fields
randomBg <- cbind(randomBgSites, randomBgEnv)
names(randomBg)[1:2] <- c('longitude', 'latitude')
head(randomBg)

#auto variable
trainDataAuto <- rbind(
  PolyBgEnv[ , c(paste0('WC20', 1:9), paste0('WC2', 10:19))],
  randomBgEnv[ , c(paste0('WC20', 1:9), paste0('WC2', 10:19))]
)

presBg <- c(rep(1, nrow(PolyBgEnv)), rep(0, nrow(randomBgEnv)))
autoSelectModel <- maxnet(p=presBg, data=trainDataAuto)
dir.create('./Models/Model 01 Predictors - Automated Selection', recursive=TRUE,
           showWarnings=FALSE)
# save model
save(autoSelectModel,
     file='./Models/Model 01 Predictors - Automated Selection/Model.Rdata',
     compress=TRUE)

autoSelectMap <- predict(Balta, autoSelectModel,
                         filename='./Models/Model 01 Predictors - Automated Selection/maxentPrediction1970to2000',
                         format='GTiff', overwrite=TRUE, type='cloglog')

plot(BA, main='Model with Auto-selected Predictors')
plot(autoSelectMap, add=TRUE)
#sp::plot(countries, add=TRUE, border='gray45')
plot(BA, add=TRUE)

candidates <- c('WC202', 'WC203', 'WC204', 'WC208', 'WC209', 'WC210', 'WC211', 'WC215', 'WC216', 'WC217', 'WC218', 'WC219')
pairs(PolyBgEnv[ , candidates], col=alpha('blue', 0.5), pch=16)
correl <- cor(PolyBgEnv[ , candidates], method='spearman')
print(correl, digits=2)

predictors <- c('WC201', 'WC212')
trainDataManual <- rbind(PolyBgEnv[ , predictors], randomBg[ , predictors])
presBg <- c(rep(1, nrow(PolyBgEnv)), rep(0, nrow(randomBg)))
dir.create('./Models/Model 02 Predictors - Manual Selection',
           recursive=TRUE, showWarnings=FALSE)
manualSelectModel <- maxnet(p=presBg, data=trainDataManual)
save(manualSelectModel,
     file='./Models/Model 02 Predictors - Manual Selection/Model.Rdata',
     compress=TRUE)
climateSelect <- subset(Balta, c('WC201', 'WC212')) 
manualSelectMap <- predict(climateSelect, manualSelectModel,
                           filename='./Models/Model 02 Predictors - Manual Selection/maxentPrediction1970to2000',
                           format='GTiff', overwrite=TRUE, type='cloglog')
par(mfrow=c(1, 2))
plot(BA, main='Model-Selected Predictors') 
plot(autoSelectMap, add=TRUE)
plot(BA, add=TRUE)

plot(BA, main='Ecologist-Chosen Predictors')
plot(manualSelectMap, add=TRUE)
plot(BA, add=TRUE)

getwd()

