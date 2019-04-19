###############################
### MAXENT SCRIPT DRAFT for strict and lenient substratess only #######
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
DirtPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Dirt_Points_strict/chull.shp")
RockPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Rock_Points_strict/chull.shp")
VegPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Veg_Points_strict/chull.shp")
WaterPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Water_Points_strict/chull.shp")
#################################################################################################################################
DirtPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Dirt_Points_lenient/chull.shp")
RockPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Rock_Points_lenient/chull.shp")
VegPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Veg_Points_lenient/chull.shp")
WaterPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Water_Points_lenient/chull.shp")
#################################################################################################################################


# make into data frame for maxent
DirtDFS <- data.frame(DirtPointsS)
DirtDFS <- DirtDFS[,1:2]
RockDFS <- data.frame(RockPointsS)
RockDFS <- RockDFS[,1:2]
VegDFS <- data.frame(VegPointsS)
VegDFS <- VegDFS[,1:2]
WaterDFS <- data.frame(WaterPointsS)
WaterDFS <- WaterDFS[,1:2]
#################################################################################################################################
DirtDFL <- data.frame(DirtPointsL)
DirtDFL <- DirtDFL[,1:2]
RockDFL <- data.frame(RockPointsL)
RockDFL <- RockDFL[,1:2]
VegDFL <- data.frame(VegPointsL)
VegDFL <- VegDFL[,1:2]
WaterDFL <- data.frame(WaterPointsL)
WaterDFL <- WaterDFL[,1:2]


## take out points outside of the extent area
# like korea, italy, and water points
outliers <- extract(predictors, WaterPointsL)
out <- which(is.na(outliers))
out
View(outliers)

# dirt outliers strict
dropD <- c(16,37,71,98,114,147,166,167)
DirtNewS <- DirtDFS[-dropD,]

#veg outliers strict
dropV <- c(34)
VegNewS <- VegDFS[-dropV,]

# rock outliers strict
dropR <- c(1,2,3,4,10,11,20)
RockNewS <- RockDFS[-dropR,]

# water outliers strict
dropWa <- c(20,47,87)
WaterNewS <- WaterDFS[-dropWa,]

#################################################################################################################################

# dirt outliers lenient
dropD <- c(16,37,71,90,114,137,147,166,167)
DirtNewL <- DirtDFL[-dropD,]

#veg outliers lenient
dropV <- c(34)
VegNewL <- VegDFL[-dropV,]

# rock outliers lenient
dropR <- c(7,8,9,10,20,21,34,47,73)
RockNewL <- RockDFL[-dropR,]

# water outliers lenient
dropWa <- c(20,47,87)
WaterNewL <- WaterDFL[-dropWa,]



# assign occurrence points
occDS <- DirtNewS
occDS <- as.matrix(occDS)
foldDS <- kfold(occDS, k=5)
occtestDS <- occDS[foldDS == 1, ]
occtrainDS <- occDS[foldDS != 1, ]

# assign occurrence points
occVS <- VegNewS
occVS <- as.matrix(occVS)
foldVS <- kfold(occVS, k=5)
occtestVS <- occVS[foldVS == 1, ]
occtrainVS <- occVS[foldVS != 1, ]

# assign occurrence points
occRS <- RockNewS
occRS <- as.matrix(occRS)
foldRS <- kfold(occRS, k=5)
occtestRS <- occRS[foldRS == 1, ]
occtrainRS <- occRS[foldRS != 1, ]

# assign occurrence points
occWS <- WaterNewS
occWS <- as.matrix(occWS)
foldWS <- kfold(occWS, k=5)
occtestWS <- occWS[foldWS == 1, ]
occtrainWS <- occWS[foldWS != 1, ]


#################################################################################################################################

# assign occurrence points
occDL <- DirtNewL
occDL <- as.matrix(occDL)
foldDL <- kfold(occDL, k=5)
occtestDL <- occDL[foldDL == 1, ]
occtrainDL <- occDL[foldDL != 1, ]

# assign occurrence points
occVL <- VegNewL
occVL <- as.matrix(occVL)
foldVL <- kfold(occVL, k=5)
occtestVL <- occVL[foldVL == 1, ]
occtrainVL <- occVL[foldVL != 1, ]

# assign occurrence points
occRL <- RockNewL
occRL <- as.matrix(occRL)
foldRL <- kfold(occRL, k=5)
occtestRL <- occRL[foldRL == 1, ]
occtrainRL <- occRL[foldRL != 1, ]

# assign occurrence points
occWL <- WaterNewL
occWL <- as.matrix(occWL)
foldWL <- kfold(occWL, k=5)
occtestWL <- occWL[foldWL == 1, ]
occtrainWL <- occWL[foldWL != 1, ]


# maxent model
# maxent model with 5 replicates
DirtModS <- maxent(predictors, occtrainDS, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/DirtMod_strict")

VegModS <- maxent(predictors, occtrainVS, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/VegMod_strict")

RockModS <- maxent(predictors, occtrainRS, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/RockMod_strict")

WaterModS <- maxent(predictors, occtrainWS, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/WaterMod_strict")

#################################################################################################################################

DirtModL <- maxent(predictors, occtrainDL, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/DirtMod_lenient")

VegModL <- maxent(predictors, occtrainVL, args=c("-J","-P",'replicates=5'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/VegMod_lenient")

RockModL <- maxent(predictors, occtrainRL, args=c("-J","-P",'replicates=5'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/RockMod_lenient")

WaterModL <- maxent(predictors, occtrainWL, args=c("-J","-P",'replicates=5'), 
                    path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/WaterMod_lenient")


# see the maxent results in a browser - doesnt work for replications
VegMod

#variable importance plot
plot(VegMod)

# response curves
response(VegMod)

#predict to entire dataset

# dirt strict prediction
DirtPredictionS <- predict(DirtModS, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/DirtMod_strict_prediction.grd',
                          overwrite=T)
DirtPredictionSAverage <- mean(DirtPredictionS) 
writeRaster(DirtPredictionSAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/DirtMod_prediction_strict'))


# veg strict prediction
VegPredictionS <- predict(VegModS, predictors, progress="text",
                           filename='./Analysis_Scripts/Chapter3/ENM/Prediction/VegMod_strict_prediction.grd',
                           overwrite=T)
VegPredictionSAverage <- mean(VegPredictionS) 
writeRaster(VegPredictionSAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/VegMod_prediction_strict'))


# rock strict prediction
RockPredictionS <- predict(RockModS, predictors, progress="text",
                           filename='./Analysis_Scripts/Chapter3/ENM/Prediction/RockMod_strict_prediction.grd',
                           overwrite=T)
RockPredictionSAverage <- mean(RockPredictionS) 
writeRaster(RockPredictionSAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/RockMod_prediction_strict'))


# water strict prediction
WaterPredictionS <- predict(WaterModS, predictors, progress="text",
                           filename='./Analysis_Scripts/Chapter3/ENM/Prediction/WaterMod_strict_prediction.grd',
                           overwrite=T)
WaterPredictionSAverage <- mean(WaterPredictionS) 
writeRaster(WaterPredictionSAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/WaterMod_prediction_strict'))


#################################################################################################################################

# dirt lenient prediction
DirtPredictionL <- predict(DirtModL, predictors, progress="text",
                           filename='./Analysis_Scripts/Chapter3/ENM/Prediction/DirtMod_lenient_prediction.grd',
                           overwrite=T)
DirtPredictionLAverage <- mean(DirtPredictionL) 
writeRaster(DirtPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/DirtMod_prediction_lenient'))


# veg lenient prediction
VegPredictionL <- predict(VegModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/VegMod_lenient_prediction.grd',
                          overwrite=T)
VegPredictionLAverage <- mean(VegPredictionL) 
writeRaster(VegPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/VegMod_prediction_lenient'))


# rock lenient prediction
RockPredictionL <- predict(RockModL, predictors, progress="text",
                           filename='./Analysis_Scripts/Chapter3/ENM/Prediction/RockMod_lenient_prediction.grd',
                           overwrite=T)
RockPredictionLAverage <- mean(RockPredictionL) 
writeRaster(RockPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/RockMod_prediction_lenient'))


# water lenient prediction
WaterPredictionL <- predict(WaterModL, predictors, progress="text",
                            filename='./Analysis_Scripts/Chapter3/ENM/Prediction/WaterMod_lenient_prediction.grd',
                            overwrite=T)
WaterPredictionLAverage <- mean(WaterPredictionL) 
writeRaster(WaterPredictionLAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/WaterMod_prediction_lenient'))



# plot occurrence points on top of the niche map for specific reason, just checking
# points(occ)

#############
#testing points
#############

#background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'

# evaluate every model from the replication in dirt
DS1 <- evaluate(DirtModS@models[[1]], p=occtestDS, a=bg, x=predictors)
DS1 # 
DS2 <- evaluate(DirtbModS@models[[2]], p=occtestDS, a=bg, x=predictors)
DS2 # 
DS3 <- evaluate(DirtModS@models[[3]], p=occtestDS, a=bg, x=predictors)
DS3 # 
DS4 <- evaluate(DirtModS@models[[4]], p=occtestDS, a=bg, x=predictors)
DS4 # 
DS5 <- evaluate(DirtModS@models[[5]], p=occtestDS, a=bg, x=predictors)
DS5 # 
# evaluate every model from the replication in veg
VS1 <- evaluate(VegModS@models[[1]], p=occtestVS, a=bg, x=predictors)
VS1 # 
VS2 <- evaluate(VegModS@models[[2]], p=occtestVS, a=bg, x=predictors)
VS2 # 
VS3 <- evaluate(VegModS@models[[3]], p=occtestVS, a=bg, x=predictors)
VS3 # 
VS4 <- evaluate(VegModS@models[[4]], p=occtestVS, a=bg, x=predictors)
VS4 # 
VS5 <- evaluate(VegModS@models[[5]], p=occtestVS, a=bg, x=predictors)
VS5 # 
# evaluate every model from the replication in rock
RS1 <- evaluate(RockModS@models[[1]], p=occtestRS, a=bg, x=predictors)
RS1 # 
RS2 <- evaluate(RockModS@models[[2]], p=occtestRS, a=bg, x=predictors)
RS2 # 
RS3 <- evaluate(RockModS@models[[3]], p=occtestRS, a=bg, x=predictors)
RS3 #
RS4 <- evaluate(RockModS@models[[4]], p=occtestRS, a=bg, x=predictors)
RS4 # 
RS5 <- evaluate(RockModS@models[[5]], p=occtestRS, a=bg, x=predictors)
RS5 # 
# evaluate every model from the replication in WATER
WS1 <- evaluate(WaterModS@models[[1]], p=occtestWS, a=bg, x=predictors)
WS1 # 
WS2 <- evaluate(WaterModS@models[[2]], p=occtestWS, a=bg, x=predictors)
WS2 # 
WS3 <- evaluate(WaterModS@models[[3]], p=occtestWS, a=bg, x=predictors)
WS3 # 
WS4 <- evaluate(WaterModS@models[[4]], p=occtestWS, a=bg, x=predictors)
WS4 # 
WS5 <- evaluate(WaterModS@models[[5]], p=occtestWS, a=bg, x=predictors)
WS5 # 

#################################################################################################################################

# evaluate every model from the replication in dirt
DL1 <- evaluate(DirtModL@models[[1]], p=occtestDL, a=bg, x=predictors)
DL1 # 
DL2 <- evaluate(DirtbModL@models[[2]], p=occtestDL, a=bg, x=predictors)
DL2 # 
DL3 <- evaluate(DirtModL@models[[3]], p=occtestDL, a=bg, x=predictors)
DL3 # 
DL4 <- evaluate(DirtModL@models[[4]], p=occtestDL, a=bg, x=predictors)
DL4 # 
DL5 <- evaluate(DirtModL@models[[5]], p=occtestDL, a=bg, x=predictors)
DL5 # 
# evaluate every model from the replication in veg
VL1 <- evaluate(VegModL@models[[1]], p=occtestVL, a=bg, x=predictors)
VL1 # 
VL2 <- evaluate(VegModL@models[[2]], p=occtestVL, a=bg, x=predictors)
VL2 # 
VL3 <- evaluate(VegModL@models[[3]], p=occtestVL, a=bg, x=predictors)
VL3 # 
VL4 <- evaluate(VegModL@models[[4]], p=occtestVL, a=bg, x=predictors)
VL4 # 
VL5 <- evaluate(VegModL@models[[5]], p=occtestVL, a=bg, x=predictors)
VL5 # 
# evaluate every model from the replication in rock
RL1 <- evaluate(RockModL@models[[1]], p=occtestRL, a=bg, x=predictors)
RL1 # 
RL2 <- evaluate(RockModL@models[[2]], p=occtestRL, a=bg, x=predictors)
RL2 # 
RL3 <- evaluate(RockModL@models[[3]], p=occtestRL, a=bg, x=predictors)
RL3 #
RL4 <- evaluate(RockModL@models[[4]], p=occtestRL, a=bg, x=predictors)
RL4 # 
RL5 <- evaluate(RockModL@models[[5]], p=occtestRL, a=bg, x=predictors)
RL5 # 
# evaluate every model from the replication in WATER
WL1 <- evaluate(WaterModL@models[[1]], p=occtestWL, a=bg, x=predictors)
WL1 # 
WL2 <- evaluate(WaterModL@models[[2]], p=occtestWL, a=bg, x=predictors)
WL2 # 
WL3 <- evaluate(WaterModL@models[[3]], p=occtestWL, a=bg, x=predictors)
WL3 # 
WL4 <- evaluate(WaterModL@models[[4]], p=occtestWL, a=bg, x=predictors)
WL4 # 
WL5 <- evaluate(WaterModL@models[[5]], p=occtestWL, a=bg, x=predictors)
WL5 # 


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