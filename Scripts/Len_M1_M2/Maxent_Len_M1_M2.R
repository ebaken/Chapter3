###############################
### MAXENT SCRIPT DRAFT for lenient,M1,M2 microhabitats only #######
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
ArbPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_lenient/chull.shp")
TerrPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_lenient/chull.shp")
AquaPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Aqua_Points_lenient/chull.shp")
CavePointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Cave_Points_lenient/chull.shp")
FossPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Foss_Points_lenient/chull.shp")
SaxPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Sax_Points_lenient/chull.shp")
#################################################################################################################################
ArbPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_M1/chull.shp")
TerrPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_M1/chull.shp")
AquaPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Aqua_Points_M1/chull.shp")
CavePointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Cave_Points_M1/chull.shp")
FossPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Foss_Points_M1/chull.shp")
SaxPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Sax_Points_M1/chull.shp")
#################################################################################################################################
ArbPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_M2/chull.shp")
TerrPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_M2/chull.shp")
AquaPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Aqua_Points_M2/chull.shp")
CavePointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Cave_Points_M2/chull.shp")
FossPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Foss_Points_M2/chull.shp")
SaxPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Sax_Points_M2/chull.shp")

# make into data frame for maxent
ArbDFL <- data.frame(ArbPointsL)
ArbDFL <- ArbDFL[,1:2]
TerrDFL <- data.frame(TerrPointsL)
TerrDFL <- TerrDFL[,1:2]
AquaDFL <- data.frame(AquaPointsL)
AquaDFL <- AquaDFL[,1:2]
CaveDFL <- data.frame(CavePointsL)
CaveDFL <- CaveDFL[,1:2]
FossDFL <- data.frame(FossPointsL)
FossDFL <- FossDFL[,1:2]
SaxDFL <- data.frame(SaxPointsL)
SaxDFL <- SaxDFL[,1:2]
#################################################################################################################################
ArbDFM1 <- data.frame(ArbPointsM1)
ArbDFM1 <- ArbDFM1[,1:2]
TerrDFM1 <- data.frame(TerrPointsM1)
TerrDFM1 <- TerrDFM1[,1:2]
AquaDFM1 <- data.frame(AquaPointsM1)
AquaDFM1 <- AquaDFM1[,1:2]
CaveDFM1 <- data.frame(CavePointsM1)
CaveDFM1 <- CaveDFM1[,1:2]
FossDFM1 <- data.frame(FossPointsM1)
FossDFM1 <- FossDFM1[,1:2]
SaxDFM1 <- data.frame(SaxPointsM1)
SaxDFM1 <- SaxDFM1[,1:2]
#################################################################################################################################
ArbDFM2 <- data.frame(ArbPointsM2)
ArbDFM2 <- ArbDFM2[,1:2]
TerrDFM2 <- data.frame(TerrPointsM2)
TerrDFM2 <- TerrDFM2[,1:2]
AquaDFM2 <- data.frame(AquaPointsM2)
AquaDFM2 <- AquaDFM2[,1:2]
CaveDFM2 <- data.frame(CavePointsM2)
CaveDFM2 <- CaveDFM2[,1:2]
FossDFM2 <- data.frame(FossPointsM2)
FossDFM2 <- FossDFM2[,1:2]
SaxDFM2 <- data.frame(SaxPointsM2)
SaxDFM2 <- SaxDFM2[,1:2]

## take out points outside of the extent area
# like korea, italy, and water points
outliers <- extract(predictors, SaxPointsM2)
out <- which(is.na(outliers))
out
View(outliers)

# arb outliers lenient
dropA <- c(41)
ArbNewL <- ArbDFL[-dropA,]

#terr outliers lenient
dropT <- c(14,32,60,133)
TerrNewL <- TerrDFL[-dropT,]

# aqua outliers lenient
dropW <- c(20,47,87)
AquaNewL <- AquaDFL[-dropW,]

# cave outliers leient
dropC <- c(4,5,6,7,17,18,27,53)
CaveNewL <- CaveDFL[-dropC,]

# Foss outliers lenient
dropF <- c(18,51,96)
FossNewL <- FossDFL[-dropF,]

# sax outliers lenient
dropS <- c(18,41)
SaxNewL <- SaxDFL[-dropS,]

#################################################################################################################################

# arb outliers M1
dropA <- c(34)
ArbNewM1 <- ArbDFM1[-dropA,]

#terr outliers M1
dropT <- c(16,37,71,98,114,135,145,164,165)
TerrNewM1 <- TerrDFM1[-dropT,]

# aqua outliers M1
dropW <- c(20,47,87)
AquaNewM1 <- AquaDFM1[-dropW,]

# cave outliers M1
dropC <- c(1,2,3,4,8,9,17)
CaveNewM1 <- CaveDFM1[-dropC,]

# Foss outliers M1
FossNewM1 <- FossDFM1

# sax outliers M1
dropS <- c(6)
SaxNewM1 <- SaxDFM1[-dropS,]

#################################################################################################################################

# arb outliers M2
dropA <- c(17,72,73,86,87,99,100,147)
ArbNewM2 <- ArbDFM2[-dropA,]

#terr outliers M2
TerrNewM2 <- TerrDFM2

# aqua outliers M2
dropW <- c(58)
AquaNewM2 <- AquaDFM2[-dropW,]

# cave outliers M2
dropC <- c(1,2,3,7,15)
CaveNewM2 <- CaveDFM2[-dropC,]

# Foss outliers M2
dropF <- c(47)
FossNewM2 <- FossDFM2[-dropF,]

# sax outliers M2
dropS <- c(2,18)
SaxNewM2 <- SaxDFM2[-dropS,]




# assign occurrence points
occAL <- ArbNewL
occAL <- as.matrix(occAL)
foldAL <- kfold(occAL, k=5)
occtestAL <- occAL[foldAL == 1, ]
occtrainAL <- occAL[foldAL != 1, ]

# assign occurrence points
occTL <- TerrNewL
occTL <- as.matrix(occTL)
foldTL <- kfold(occTL, k=5)
occtestTL <- occTL[foldTL == 1, ]
occtrainTL <- occTL[foldTL != 1, ]

# assign occurrence points
occWL <- AquaNewL
occWL <- as.matrix(occWL)
foldWL <- kfold(occWL, k=5)
occtestWL <- occWL[foldWL == 1, ]
occtrainWL <- occWL[foldWL != 1, ]

# assign occurrence points
occCL <- CaveNewL
occCL <- as.matrix(occCL)
foldCL <- kfold(occCL, k=5)
occtestCL <- occCL[foldCL == 1, ]
occtrainCL <- occCL[foldCL != 1, ]

# assign occurrence points
occFL <- FossDFL
occFL <- as.matrix(occFL)
foldFL <- kfold(occFL, k=5)
occtestFL <- occFL[foldFL == 1, ]
occtrainFL <- occFL[foldFL != 1, ]

# assign occurrence points
occSL <- SaxNewL
occSL <- as.matrix(occSL)
foldSL <- kfold(occSL, k=5)
occtestSL <- occSL[foldSL == 1, ]
occtrainSL <- occSL[foldSL != 1, ]

#################################################################################################################################


# assign occurrence points
occAM1 <- ArbNewM1
occAM1 <- as.matrix(occAM1)
foldAM1 <- kfold(occAM1, k=5)
occtestAM1 <- occAM1[foldAM1 == 1, ]
occtrainAM1 <- occAM1[foldAM1 != 1, ]

# assign occurrence points
occTM1 <- TerrNewM1
occTM1 <- as.matrix(occTM1)
foldTM1 <- kfold(occTM1, k=5)
occtestTM1 <- occTM1[foldTM1 == 1, ]
occtrainTM1 <- occTM1[foldTM1 != 1, ]

# assign occurrence points
occWM1 <- AquaNewM1
occWM1 <- as.matrix(occWM1)
foldWM1 <- kfold(occWM1, k=5)
occtestWM1 <- occWM1[foldWM1 == 1, ]
occtrainWM1 <- occWM1[foldWM1 != 1, ]

# assign occurrence points
occCM1 <- CaveNewM1
occCM1 <- as.matrix(occCM1)
foldCM1 <- kfold(occCM1, k=5)
occtestCM1 <- occCM1[foldCM1 == 1, ]
occtrainCM1 <- occCM1[foldCM1 != 1, ]

# assign occurrence points
occFM1 <- FossDFM1
occFM1 <- as.matrix(occFM1)
foldFM1 <- kfold(occFM1, k=5)
occtestFM1 <- occFM1[foldFM1 == 1, ]
occtrainFM1 <- occFM1[foldFM1 != 1, ]

# assign occurrence points
occSM1 <- SaxNewM1
occSM1 <- as.matrix(occSM1)
foldSM1 <- kfold(occSM1, k=5)
occtestSM1 <- occSM1[foldSM1 == 1, ]
occtrainSM1 <- occSM1[foldSM1 != 1, ]


#################################################################################################################################

# assign occurrence points
occAM2 <- ArbNewM2
occAM2 <- as.matrix(occAM2)
foldAM2 <- kfold(occAM2, k=5)
occtestAM2 <- occAM2[foldAM2 == 1, ]
occtrainAM2 <- occAM2[foldAM2 != 1, ]

# assign occurrence points
occTM2 <- TerrNewM2
occTM2 <- as.matrix(occTM2)
foldTM2 <- kfold(occTM2, k=5)
occtestTM2 <- occTM2[foldTM2 == 1, ]
occtrainTM2 <- occTM2[foldTM2 != 1, ]

# assign occurrence points
occWM2 <- AquaNewM2
occWM2 <- as.matrix(occWM2)
foldWM2 <- kfold(occWM2, k=5)
occtestWM2 <- occWM2[foldWM2 == 1, ]
occtrainWM2 <- occWM2[foldWM2 != 1, ]

# assign occurrence points
occCM2 <- CaveNewM2
occCM2 <- as.matrix(occCM2)
foldCM2 <- kfold(occCM2, k=5)
occtestCM2 <- occCM2[foldCM2 == 1, ]
occtrainCM2 <- occCM2[foldCM2 != 1, ]

# assign occurrence points
occFM2 <- FossDFM2
occFM2 <- as.matrix(occFM2)
foldFM2 <- kfold(occFM2, k=5)
occtestFM2 <- occFM2[foldFM2 == 1, ]
occtrainFM2 <- occFM2[foldFM2 != 1, ]

# assign occurrence points
occSM2 <- SaxNewM2
occSM2 <- as.matrix(occSM2)
foldSM2 <- kfold(occSM2, k=5)
occtestSM2 <- occSM2[foldSM2 == 1, ]
occtrainSM2 <- occSM2[foldSM2 != 1, ]


# maxent model
# maxent model with 5 replicates
ArbModL <- maxent(predictors, occtrainAL, args=c("-J","-P",'replicates=5'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_lenient")

TerrModL <- maxent(predictors, occtrainTL, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_lenient")

AquaModL <- maxent(predictors, occtrainWL, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/AquaMod_lenient")

CaveModL <- maxent(predictors, occtrainCL, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/CaveMod_lenient")

FossModL <- maxent(predictors, occtrainFL, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/FossMod_lenient")

SaxModL <- maxent(predictors, occtrainSL, args=c("-J","-P",'replicates=5'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/SaxMod_lenient")

#################################################################################################################################

ArbModM1 <- maxent(predictors, occtrainAM1, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_M1")

TerrModM1 <- maxent(predictors, occtrainTM1, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_M1")

AquaModM1 <- maxent(predictors, occtrainWM1, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/AquaMod_M1")

CaveModM1 <- maxent(predictors, occtrainCM1, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/CaveMod_M1")

FossModM1 <- maxent(predictors, occtrainFM1, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/FossMod_M1")

SaxModM1 <- maxent(predictors, occtrainSM1, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/SaxMod_M1")

#################################################################################################################################

ArbModM2 <- maxent(predictors, occtrainAM2, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_M2")

TerrModM2 <- maxent(predictors, occtrainTM2, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_M2")

AquaModM2 <- maxent(predictors, occtrainWM2, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/AquaMod_M2")

CaveModM2 <- maxent(predictors, occtrainCM2, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/CaveMod_M2")

FossModM2 <- maxent(predictors, occtrainFM2, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/FossMod_M2")

SaxModM2 <- maxent(predictors, occtrainSM2, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/SaxMod_M2")

# see the maxent results in a browser - doesnt work for replications
ArbMod

#variable importance plot
plot(ArbMod)

# response curves
response(ArbMod)

#predict to entire dataset

# arboreal prediction
ArbPredictionL <- predict(ArbModL, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_lenient_prediction.grd',
                         overwrite=T)
ArbPredictionLAverage <- mean(ArbPredictionL) 
#plot(ArbPredictionLAverage) 
writeRaster(ArbPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_Lenient'))


# terrestrial prediction
TerrPredictionL <- predict(TerrModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_lenient_prediction.grd',
                          overwrite=T)
TerrPredictionLAverage <- mean(TerrPredictionL) 
#plot(TerrPredictionLAverage) 
writeRaster(TerrPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_lenient'))


# Aquatic prediction
AquaPredictionL <- predict(AquaModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_lenient_prediction.grd',
                          overwrite=T)
AquaPredictionLAverage <- mean(AquaPredictionL) 
#plot(AquaPredictionLAverage) 
writeRaster(AquaPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_lenient'))


# cave prediction
CavePredictionL <- predict(CaveModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_lenient_prediction.grd',
                          overwrite=T)
CavePredictionLAverage <- mean(CavePredictionL) 
#plot(CavePredictionLAverage) 
writeRaster(CavePredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_lenient'))


# Fossorial prediction
FossPredictionL <- predict(FossModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_lenient_prediction.grd',
                          overwrite=T)
FossPredictionLAverage <- mean(FossPredictionL) 
#plot(FossPredictionLAverage) 
writeRaster(FossPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_lenient'))


# Saxicolous prediction
SaxPredictionL <- predict(SaxModL, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_lenient_prediction.grd',
                         overwrite=T)
SaxPredictionLAverage <- mean(SaxPredictionL) 
#plot(SaxPredictionLAverage) 
writeRaster(SaxPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_lenient'))





#################################################################################################################################




# arboreal prediction
ArbPredictionM1 <- predict(ArbModM1, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_M1_prediction.grd',
                         overwrite=T)
ArbPredictionM1Average <- mean(ArbPredictionM1) 
#plot(ArbPredictionM1Average) 
writeRaster(ArbPredictionM1Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_M1'))


# terrestrial prediction
TerrPredictionM1 <- predict(TerrModM1, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_M1_prediction.grd',
                          overwrite=T)
TerrPredictionM1Average <- mean(TerrPredictionM1) 
#plot(TerrPredictionM1Average) 
writeRaster(TerrPredictionM1Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_M1'))


# Aquatic prediction
AquaPredictionM1 <- predict(AquaModM1, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_M1_prediction.grd',
                          overwrite=T)
AquaPredictionM1Average <- mean(AquaPredictionM1) 
#plot(AquaPredictionM1Average) 
writeRaster(AquaPredictionM1Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_M1'))


# cave prediction
CavePredictionM1 <- predict(CaveModM1, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_M1_prediction.grd',
                          overwrite=T)
CavePredictionM1Average <- mean(CavePredictionM1) 
#plot(CavePredictionM1Average) 
writeRaster(CavePredictionM1Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_M1'))


# Fossorial prediction
FossPredictionM1 <- predict(FossModM1, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_M1_prediction.grd',
                          overwrite=T)
FossPredictionM1Average <- mean(FossPredictionM1) 
#plot(FossPredictionM1Average) 
writeRaster(FossPredictionM1Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_M1'))


# Saxicolous prediction
SaxPredictionM1 <- predict(SaxModM1, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_M1_prediction.grd',
                         overwrite=T)
SaxPredictionM1Average <- mean(SaxPredictionM1) 
#plot(SaxPredictionM1Average) 
writeRaster(SaxPredictionM1Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_M1'))



#################################################################################################################################




# arboreal prediction
ArbPredictionM2 <- predict(ArbModM2, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_M2_prediction.grd',
                         overwrite=T)
ArbPredictionM2Average <- mean(ArbPredictionM2) 
#plot(ArbPredictionM2Average) 
writeRaster(ArbPredictionM2Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_M2'))


# terrestrial prediction
TerrPredictionM2 <- predict(TerrModM2, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_M2_prediction.grd',
                          overwrite=T)
TerrPredictionM2Average <- mean(TerrPredictionM2) 
#plot(TerrPredictionM2Average) 
writeRaster(TerrPredictionM2Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_M2'))


# Aquatic prediction
AquaPredictionM2 <- predict(AquaModM2, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_M2_prediction.grd',
                          overwrite=T)
AquaPredictionM2Average <- mean(AquaPredictionM2) 
#plot(AquaPredictionM2Average) 
writeRaster(AquaPredictionM2Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_M2'))


# cave prediction
CavePredictionM2 <- predict(CaveModM2, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_M2_prediction.grd',
                          overwrite=T)
CavePredictionM2Average <- mean(CavePredictionM2) 
#plot(CavePredictionM2Average) 
writeRaster(CavePredictionM2Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_M2'))


# Fossorial prediction
FossPredictionM2 <- predict(FossModM2, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_M2_prediction.grd',
                          overwrite=T)
FossPredictionM2Average <- mean(FossPredictionM2) 
#plot(FossPredictionM2Average) 
writeRaster(FossPredictionM2Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_M2'))


# Saxicolous prediction
SaxPredictionM2 <- predict(SaxModM2, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_M2_prediction.grd',
                         overwrite=T)
SaxPredictionM2Average <- mean(SaxPredictionM2) 
#plot(SaxPredictionM2Average) 
writeRaster(SaxPredictionM2Average, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_M2'))





# plot occurrence points on top of the niche map for specific reason, just checking
# points(occ)

#############
#testing points
#############

#background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'

# evaluate every model from the replication in arb
AL1 <- evaluate(ArbModL@models[[1]], p=occtestAL, a=bg, x=predictors)
AL1 # 
AL2 <- evaluate(ArbModL@models[[2]], p=occtestAL, a=bg, x=predictors)
AL2 # 
AL3 <- evaluate(ArbModL@models[[3]], p=occtestAL, a=bg, x=predictors)
AL3 # 
AL4 <- evaluate(ArbModL@models[[4]], p=occtestAL, a=bg, x=predictors)
AL4 # 
AL5 <- evaluate(ArbModL@models[[5]], p=occtestAL, a=bg, x=predictors)
AL5 # 
# evaluate every model from the replication in terr
TL1 <- evaluate(TerrModL@models[[1]], p=occtestTL, a=bg, x=predictors)
TL1 # 
TL2 <- evaluate(TerrModL@models[[2]], p=occtestTL, a=bg, x=predictors)
TL2 # 
TL3 <- evaluate(TerrModL@models[[3]], p=occtestTL, a=bg, x=predictors)
TL3 # 
TL4 <- evaluate(TerrModL@models[[4]], p=occtestTL, a=bg, x=predictors)
TL4 # 
TL5 <- evaluate(TerrModL@models[[5]], p=occtestTL, a=bg, x=predictors)
TL5 # 
# evaluate every model from the replication in aquatic
WL1 <- evaluate(AquaModL@models[[1]], p=occtestWL, a=bg, x=predictors)
WL1 # 
WL2 <- evaluate(AquaModL@models[[2]], p=occtestWL, a=bg, x=predictors)
WL2 # 
WL3 <- evaluate(AquaModL@models[[3]], p=occtestWL, a=bg, x=predictors)
WL3 #
WL4 <- evaluate(AquaModL@models[[4]], p=occtestWL, a=bg, x=predictors)
WL4 # 
WL5 <- evaluate(AquaModL@models[[5]], p=occtestWL, a=bg, x=predictors)
WL5 # 
# evaluate every model from the replication in cave
CL1 <- evaluate(CaveModL@models[[1]], p=occtestCL, a=bg, x=predictors)
CL1 # 
CL2 <- evaluate(CaveModL@models[[2]], p=occtestCL, a=bg, x=predictors)
CL2 # 
CL3 <- evaluate(CaveModL@models[[3]], p=occtestCL, a=bg, x=predictors)
CL3 # 
CL4 <- evaluate(CaveModL@models[[4]], p=occtestCL, a=bg, x=predictors)
CL4 # 
CL5 <- evaluate(CaveModL@models[[5]], p=occtestCL, a=bg, x=predictors)
CL5 # 
# evaluate every model from the replication in fossorial
FL1 <- evaluate(FossModL@models[[1]], p=occtestFL, a=bg, x=predictors)
FL1 # 
FL2 <- evaluate(FossModL@models[[2]], p=occtestFL, a=bg, x=predictors)
FL2 # 
FL3 <- evaluate(FossModL@models[[3]], p=occtestFL, a=bg, x=predictors)
FL3 # 
FL4 <- evaluate(FossModL@models[[4]], p=occtestFL, a=bg, x=predictors)
FL4 # 
FL5 <- evaluate(FossModL@models[[5]], p=occtestFL, a=bg, x=predictors)
FL5 # 
# evaluate every model from the replication in sax
SL1 <- evaluate(SaxModL@models[[1]], p=occtestSL, a=bg, x=predictors)
SL1 # 
SL2 <- evaluate(SaxModL@models[[2]], p=occtestSL, a=bg, x=predictors)
SL2 # 
SL3 <- evaluate(SaxModL@models[[3]], p=occtestSL, a=bg, x=predictors)
SL3 # 
SL4 <- evaluate(SaxModL@models[[4]], p=occtestSL, a=bg, x=predictors)
SL4 # 
SL5 <- evaluate(SaxModL@models[[5]], p=occtestSL, a=bg, x=predictors)
SL5 # 

#################################################################################################################################


# evaluate every model from the replication in arb
AM11 <- evaluate(ArbModM1@models[[1]], p=occtestAM1, a=bg, x=predictors)
AM11 # 
AM12 <- evaluate(ArbModM1@models[[2]], p=occtestAM1, a=bg, x=predictors)
AM12 # 
AM13 <- evaluate(ArbModM1@models[[3]], p=occtestAM1, a=bg, x=predictors)
AM13 # 
AM14 <- evaluate(ArbModM1@models[[4]], p=occtestAM1, a=bg, x=predictors)
AM14 # 
AM15 <- evaluate(ArbModM1@models[[5]], p=occtestAM1, a=bg, x=predictors)
AM15 # 
# evaluate every model from the replication in terr
TM11 <- evaluate(TerrModM1@models[[1]], p=occtestTM1, a=bg, x=predictors)
TM11 # 
TM12 <- evaluate(TerrModM1@models[[2]], p=occtestTM1, a=bg, x=predictors)
TM12 # 
TM13 <- evaluate(TerrModM1@models[[3]], p=occtestTM1, a=bg, x=predictors)
TM13 # 
TM14 <- evaluate(TerrModM1@models[[4]], p=occtestTM1, a=bg, x=predictors)
TM14 # 
TM15 <- evaluate(TerrModM1@models[[5]], p=occtestTM1, a=bg, x=predictors)
TM15 # 
# evaluate every model from the replication in aquatic
WM11 <- evaluate(AquaModM1@models[[1]], p=occtestWM1, a=bg, x=predictors)
WM11 # 
WM12 <- evaluate(AquaModM1@models[[2]], p=occtestWM1, a=bg, x=predictors)
WM12 # 
WM13 <- evaluate(AquaModM1@models[[3]], p=occtestWM1, a=bg, x=predictors)
WM13 #
WM14 <- evaluate(AquaModM1@models[[4]], p=occtestWM1, a=bg, x=predictors)
WM14 # 
WM15 <- evaluate(AquaModM1@models[[5]], p=occtestWM1, a=bg, x=predictors)
WM15 # 
# evaluate every model from the replication in cave
CM11 <- evaluate(CaveModM1@models[[1]], p=occtestCM1, a=bg, x=predictors)
CM11 # 
CM12 <- evaluate(CaveModM1@models[[2]], p=occtestCM1, a=bg, x=predictors)
CM12 # 
CM13 <- evaluate(CaveModM1@models[[3]], p=occtestCM1, a=bg, x=predictors)
CM13 # 
CM14 <- evaluate(CaveModM1@models[[4]], p=occtestCM1, a=bg, x=predictors)
CM14 # 
CM15 <- evaluate(CaveModM1@models[[5]], p=occtestCM1, a=bg, x=predictors)
CM15 # 
# evaluate every model from the replication in fossorial
FM11 <- evaluate(FossModM1@models[[1]], p=occtestFM1, a=bg, x=predictors)
FM11 # 
FM12 <- evaluate(FossModM1@models[[2]], p=occtestFM1, a=bg, x=predictors)
FM12 # 
FM13 <- evaluate(FossModM1@models[[3]], p=occtestFM1, a=bg, x=predictors)
FM13 # 
FM14 <- evaluate(FossModM1@models[[4]], p=occtestFM1, a=bg, x=predictors)
FM14 # 
FM15 <- evaluate(FossModM1@models[[5]], p=occtestFM1, a=bg, x=predictors)
FM15 # 
# evaluate every model from the replication in sax
SM11 <- evaluate(SaxModM1@models[[1]], p=occtestSM1, a=bg, x=predictors)
SM11 # 
SM12 <- evaluate(SaxModM1@models[[2]], p=occtestSM1, a=bg, x=predictors)
SM12 # 
SM13 <- evaluate(SaxModM1@models[[3]], p=occtestSM1, a=bg, x=predictors)
SM13 # 
SM14 <- evaluate(SaxModM1@models[[4]], p=occtestSM1, a=bg, x=predictors)
SM14 # 
SM15 <- evaluate(SaxModM1@models[[5]], p=occtestSM1, a=bg, x=predictors)
SM15 # 






#################################################################################################################################


# evaluate every model from the replication in arb
AM21 <- evaluate(ArbModM2@models[[1]], p=occtestAM2, a=bg, x=predictors)
AM21 # 
AM22 <- evaluate(ArbModM2@models[[2]], p=occtestAM2, a=bg, x=predictors)
AM22 # 
AM23 <- evaluate(ArbModM2@models[[3]], p=occtestAM2, a=bg, x=predictors)
AM23 # 
AM24 <- evaluate(ArbModM2@models[[4]], p=occtestAM2, a=bg, x=predictors)
AM24 # 
AM25 <- evaluate(ArbModM2@models[[5]], p=occtestAM2, a=bg, x=predictors)
AM25 # 
# evaluate every model from the replication in terr
TM21 <- evaluate(TerrModM2@models[[1]], p=occtestTM2, a=bg, x=predictors)
TM21 # 
TM22 <- evaluate(TerrModM2@models[[2]], p=occtestTM2, a=bg, x=predictors)
TM22 # 
TM23 <- evaluate(TerrModM2@models[[3]], p=occtestTM2, a=bg, x=predictors)
TM23 # 
TM24 <- evaluate(TerrModM2@models[[4]], p=occtestTM2, a=bg, x=predictors)
TM24 # 
TM25 <- evaluate(TerrModM2@models[[5]], p=occtestTM2, a=bg, x=predictors)
TM25 # 
# evaluate every model from the replication in aquatic
WM21 <- evaluate(AquaModM2@models[[1]], p=occtestWM2, a=bg, x=predictors)
WM21 # 
WM22 <- evaluate(AquaModM2@models[[2]], p=occtestWM2, a=bg, x=predictors)
WM22 # 
WM23 <- evaluate(AquaModM2@models[[3]], p=occtestWM2, a=bg, x=predictors)
WM23 #
WM24 <- evaluate(AquaModM2@models[[4]], p=occtestWM2, a=bg, x=predictors)
WM24 # 
WM25 <- evaluate(AquaModM2@models[[5]], p=occtestWM2, a=bg, x=predictors)
WM25 # 
# evaluate every model from the replication in cave
CM21 <- evaluate(CaveModM2@models[[1]], p=occtestCM2, a=bg, x=predictors)
CM21 # 
CM22 <- evaluate(CaveModM2@models[[2]], p=occtestCM2, a=bg, x=predictors)
CM22 # 
CM23 <- evaluate(CaveModM2@models[[3]], p=occtestCM2, a=bg, x=predictors)
CM23 # 
CM24 <- evaluate(CaveModM2@models[[4]], p=occtestCM2, a=bg, x=predictors)
CM24 # 
CM25 <- evaluate(CaveModM2@models[[5]], p=occtestCM2, a=bg, x=predictors)
CM25 # 
# evaluate every model from the replication in fossorial
FM21 <- evaluate(FossModM2@models[[1]], p=occtestFM2, a=bg, x=predictors)
FM21 # 
FM22 <- evaluate(FossModM2@models[[2]], p=occtestFM2, a=bg, x=predictors)
FM22 # 
FM23 <- evaluate(FossModM2@models[[3]], p=occtestFM2, a=bg, x=predictors)
FM23 # 
FM24 <- evaluate(FossModM2@models[[4]], p=occtestFM2, a=bg, x=predictors)
FM24 # 
FM25 <- evaluate(FossModM2@models[[5]], p=occtestFM2, a=bg, x=predictors)
FM25 # 
# evaluate every model from the replication in sax
SM21 <- evaluate(SaxModM2@models[[1]], p=occtestSM2, a=bg, x=predictors)
SM21 # 
SM22 <- evaluate(SaxModM2@models[[2]], p=occtestSM2, a=bg, x=predictors)
SM22 # 
SM23 <- evaluate(SaxModM2@models[[3]], p=occtestSM2, a=bg, x=predictors)
SM23 # 
SM24 <- evaluate(SaxModM2@models[[4]], p=occtestSM2, a=bg, x=predictors)
SM24 # 
SM25 <- evaluate(SaxModM2@models[[5]], p=occtestSM2, a=bg, x=predictors)
SM25 # 


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
