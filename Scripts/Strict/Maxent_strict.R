###############################
### MAXENT SCRIPT DRAFT for strict microhabitats only #######
###############################

# MAXENT MODELS AND EVALUATIONS

library(dismo); library(rJava); library(maptools)
library(raster)

###### maxent pipeline #########
# load variables
# READ IN DATA #
ClimateData <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogCGTiff.gri')

# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors.crop <- crop(x = ClimateData, y = geographic.extent)
predictors <- predictors.crop

# load file with presence points
ArbPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Arb_Points_strict/chull.shp")
TerrPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Terr_Points_strict/chull.shp")
AquaPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Aqua_Points_strict/chull.shp")
CavePointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Cave_Points_strict/chull.shp")
FossPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Foss_Points_strict/chull.shp")
SaxPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Sax_Points_strict/chull.shp")

# make into data frame for maxent
ArbDF <- data.frame(ArbPointsS)
ArbDF <- ArbDF[,1:2]
TerrDF <- data.frame(TerrPointsS)
TerrDF <- TerrDF[,1:2]
AquaDF <- data.frame(AquaPointsS)
AquaDF <- AquaDF[,1:2]
CaveDF <- data.frame(CavePointsS)
CaveDF <- CaveDF[,1:2]
FossDF <- data.frame(FossPointsS)
FossDF <- FossDF[,1:2]
SaxDF <- data.frame(SaxPointsS)
SaxDF <- SaxDF[,1:2]

## take out points outside of the extent area
# like korea, italy, and water points
outliers <- extract(predictors, TerrPointsS)
out <- which(is.na(outliers))
out
View(outliers)

# arb outliers
dropA <- c(34)
ArbNew <- ArbDF[-dropA,]

#terr outliers
dropT <- c(16,37,71,98,114,135,145,164,165)
TerrNew <- TerrDF[-dropT,]

# aqua outliers
dropW <- c(20,47,87)
AquaNew <- AquaDF[-dropW,]

# cave outliers
dropC <- c(1,2,3,4,8,9,17)
CaveNew <- CaveDF[-dropC,]

# sax outliers
dropS <- c(6)
SaxNew <- SaxDF[-dropS,]


# assign occurrence points
occA <- ArbNew
occA <- as.matrix(occA)
foldA <- kfold(occA, k=5)
occtestA <- occA[foldA == 1, ]
occtrainA <- occA[foldA != 1, ]

# assign occurrence points
occT <- TerrNew
occT <- as.matrix(occT)
foldT <- kfold(occT, k=5)
occtestT <- occT[foldT == 1, ]
occtrainT <- occT[foldT != 1, ]

# assign occurrence points
occW <- AquaNew
occW <- as.matrix(occW)
foldW <- kfold(occW, k=5)
occtestW <- occW[foldW == 1, ]
occtrainW <- occW[foldW != 1, ]

# assign occurrence points
occC <- CaveNew
occC <- as.matrix(occC)
foldC <- kfold(occC, k=5)
occtestC <- occC[foldC == 1, ]
occtrainC <- occC[foldC != 1, ]

# assign occurrence points
occF <- FossDF
occF <- as.matrix(occF)
foldF <- kfold(occF, k=5)
occtestF <- occF[foldF == 1, ]
occtrainF <- occF[foldF != 1, ]

# assign occurrence points
occS <- SaxNew
occS <- as.matrix(occS)
foldS <- kfold(occS, k=5)
occtestS <- occS[foldS == 1, ]
occtrainS <- occS[foldS != 1, ]


# maxent model
# maxent model with 5 replicates
ArbMod <- maxent(predictors, occtrainA, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict")

TerrMod <- maxent(predictors, occtrainT, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_strict")

AquaMod <- maxent(predictors, occtrainW, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/AquaMod_strict")

CaveMod <- maxent(predictors, occtrainC, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/CaveMod_strict")

FossMod <- maxent(predictors, occtrainF, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/FossMod_strict")

SaxMod <- maxent(predictors, occtrainS, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/SaxMod_strict")

# see the maxent results in a browser - doesnt work for replications
ArbMod
#variable importance plot
plot(ArbMod)
# response curves
response(ArbMod)

#predict to entire dataset
# arboreal prediction
ArbPrediction <- predict(ArbMod, predictors, progress="text",
                  filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_strict_prediction.grd',
                  overwrite=T)
ArbPredictionAverage <- mean(ArbPrediction) 
writeRaster(ArbPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_strict'))


# terrestrial prediction
TerrPrediction <- predict(TerrMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_strict_prediction.grd',
                         overwrite=T)
TerrPredictionAverage <- mean(TerrPrediction) 
writeRaster(TerrPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_strict'))


# Aquatic prediction
AquaPrediction <- predict(AquaMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_strict_prediction.grd',
                         overwrite=T)
AquaPredictionAverage <- mean(AquaPrediction) 
writeRaster(AquaPredictionAverage, overwrite=T, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_strict'))


# cave prediction
CavePrediction <- predict(CaveMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_strict_prediction.grd',
                         overwrite=T)
CavePredictionAverage <- mean(CavePrediction) 
writeRaster(CavePredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_strict'))


# Fossorial prediction
FossPrediction <- predict(FossMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_strict_prediction.grd',
                         overwrite=T)
FossPredictionAverage <- mean(FossPrediction) 
writeRaster(FossPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_strict'))


# Saxicolous prediction
SaxPrediction <- predict(SaxMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_strict_prediction.grd',
                         overwrite=T)
SaxPredictionAverage <- mean(SaxPrediction) 
writeRaster(SaxPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_strict'))


# plot occurrence points on top of the niche map for specific reason, just checking
# points(occ)

#############
#testing points
#############

#background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'

# evaluate every model from the replication in arb
AS1 <- evaluate(ArbMod@models[[1]], p=occtestA, a=bg, x=predictors)
AS1 # 
AS2 <- evaluate(ArbMod@models[[2]], p=occtestA, a=bg, x=predictors)
AS2 # 
AS3 <- evaluate(ArbMod@models[[3]], p=occtestA, a=bg, x=predictors)
AS3 # 
AS4 <- evaluate(ArbMod@models[[4]], p=occtestA, a=bg, x=predictors)
AS4 # 
AS5 <- evaluate(ArbMod@models[[5]], p=occtestA, a=bg, x=predictors)
AS5 # 
# evaluate every model from the replication in terr
TS1 <- evaluate(TerrMod@models[[1]], p=occtestT, a=bg, x=predictors)
TS1 # 
TS2 <- evaluate(TerrMod@models[[2]], p=occtestT, a=bg, x=predictors)
TS2 # 
TS3 <- evaluate(TerrMod@models[[3]], p=occtestT, a=bg, x=predictors)
TS3 # 
TS4 <- evaluate(TerrMod@models[[4]], p=occtestT, a=bg, x=predictors)
TS4 # 
TS5 <- evaluate(TerrMod@models[[5]], p=occtestT, a=bg, x=predictors)
TS5 # 
# evaluate every model from the replication in aquatic
WS1 <- evaluate(AquaMod@models[[1]], p=occtestW, a=bg, x=predictors)
WS1 # 0.92
WS2 <- evaluate(AquaMod@models[[2]], p=occtestW, a=bg, x=predictors)
WS2 # 0.92
WS3 <- evaluate(AquaMod@models[[3]], p=occtestW, a=bg, x=predictors)
WS3 # 0.91
WS4 <- evaluate(AquaMod@models[[4]], p=occtestW, a=bg, x=predictors)
WS4 # 0.93
WS5 <- evaluate(AquaMod@models[[5]], p=occtestW, a=bg, x=predictors)
WS5 # 0.92
# evaluate every model from the replication in cave
CS1 <- evaluate(CaveMod@models[[1]], p=occtestC, a=bg, x=predictors)
CS1 # 0.98
CS2 <- evaluate(CaveMod@models[[2]], p=occtestC, a=bg, x=predictors)
CS2 # 0.98
CS3 <- evaluate(CaveMod@models[[3]], p=occtestC, a=bg, x=predictors)
CS3 # 0.98
CS4 <- evaluate(CaveMod@models[[4]], p=occtestC, a=bg, x=predictors)
CS4 # 0.98
CS5 <- evaluate(CaveMod@models[[5]], p=occtestC, a=bg, x=predictors)
CS5 # 0.98
# evaluate every model from the replication in fossorial
FS1 <- evaluate(FossMod@models[[1]], p=occtestF, a=bg, x=predictors)
FS1 # 0.89
FS2 <- evaluate(FossMod@models[[2]], p=occtestF, a=bg, x=predictors)
FS2 # 0.82
FS3 <- evaluate(FossMod@models[[3]], p=occtestF, a=bg, x=predictors)
FS3 # 0.92
FS4 <- evaluate(FossMod@models[[4]], p=occtestF, a=bg, x=predictors)
FS4 # 0.91
FS5 <- evaluate(FossMod@models[[5]], p=occtestF, a=bg, x=predictors)
FS5 # 0.87
# evaluate every model from the replication in sax
SS1 <- evaluate(SaxMod@models[[1]], p=occtestS, a=bg, x=predictors)
SS1 # 0.68
SS2 <- evaluate(SaxMod@models[[2]], p=occtestS, a=bg, x=predictors)
SS2 # 0.88
SS3 <- evaluate(SaxMod@models[[3]], p=occtestS, a=bg, x=predictors)
SS3 # 0.86
SS4 <- evaluate(SaxMod@models[[4]], p=occtestS, a=bg, x=predictors)
SS4 # 0.85
SS5 <- evaluate(SaxMod@models[[5]], p=occtestS, a=bg, x=predictors)
SS5 # 0.89



# THESE ARE OTHER WAYS TO EVALUATE, BUT GIVE THE SAME RESULT
# alternative 1
# extract values
pvtest <- data.frame(extract(predictors, occtestV))
avtest <- data.frame(extract(predictors, bg))
e2 <- evaluate(p=pvtest, a=avtest)
e2
# alternative 2
# predict to testing points
testp <- predict(me, pvtest)
head(testp)
testa <- predict(me, avtest)
e3 <- evaluate(p=testp, a=testa)
e3
threshold(e3)
plot(e3, 'ROC')
# look into more...
v <- extract(predictors, VegNew)
mess <-mess(predictors, v, full=FALSE)
plot(mess)
mess
