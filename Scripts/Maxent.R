###############################
### MAXENT SCRIPT DRAFT #######
###############################

# MAXENT MODELS AND EVALUATIONS

library(dismo); library(rJava); library(maptools)
library(raster)

# for maxent if you do not have it 
# library(devtools)
# install_github("johnbaums/rmaxent")
# library(rmaxent)
# get_maxent(version = "latest", quiet = FALSE)

###### maxent pipeline #########
# load variables
elevation <- stack('./Analysis_Scripts/Chapter3/Climate Data/alt_2-5m_bil/alt.bil')
bioclim_vars <- stack(c(list.files('./Analysis_Scripts/Chapter3/Climate Data/wc2', full.names = T, pattern = '.tif')))
bioclim_keep <- stack(bioclim_vars$wc2.0_bio_2.5m_1, bioclim_vars$wc2.0_bio_2.5m_5, bioclim_vars$wc2.0_bio_2.5m_6,
                      bioclim_vars$wc2.0_bio_2.5m_16, bioclim_vars$wc2.0_bio_2.5m_17)
extra <- stack(c(list.files('./Analysis_Scripts/Chapter3/Climate Data/NewWorld_current_2', full.names = T, pattern = '.tif')))
extra_keep <- stack(extra$current_2.5arcmin_climaticMoistureIndex, extra$current_2.5arcmin_PETDriestQuarter,
                    extra$current_2.5arcmin_PETWettestQuarter)
cloud <- stack("./Analysis_Scripts/Chapter3/Climate Data/Cropped_Cloud.tif")

# # make each at the same extent
elevation1 <- crop(elevation, extra_keep)
bioclim_keep1 <- crop(bioclim_keep, extra_keep)
cloud1 <- crop(cloud, bioclim_keep1)
dim(cloud1) <- c(3435,4028,1)
cloud1@extent <- extra_keep@extent

#resample cloud because different resolution
r1 <- elevation1
r2 <- bioclim_keep1
r3 <- extra_keep
r4 <- cloud
r1 <- resample(r1,r3)
r2 <- resample(r2,r3,method='ngb')
r4 <- resample(r4,r3,method='ngb')
rs <- stack(r1,r2,r3,r4)

# #combine into stack
alldata_togetherC <- stack(elevation1, bioclim_keep1, extra_keep, cloud1)
writeRaster(alldata_togetherC, paste0('./Analysis_Scripts/Chapter3/Climate Data/ENM/AllDataTogC',
            format = 'GTiff'))
writeRaster(rs, paste0('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogC',
                                      format = 'GTiff'))

# READ IN DATA #
alldata_withC <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogCGTiff.gri')

# get polygons for checking
Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors.crop <- crop(x = alldata_withC, y = geographic.extent)

# load predictor files in as a raster stack
predictors <- predictors.crop

# load file with presence points
VegPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Veg_Points/chull.shp")
DirtPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Dirt_Points/chull.shp")
ArbPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points/chull.shp")
TerrPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points/chull.shp")

# make into data frame for maxent
VegDF <- data.frame(VegPoints)
VegDF <- VegDF[,1:2]
DirtDF <- data.frame(DirtPoints)
DirtDF <- DirtDF[,1:2]
ArbDF <- data.frame(ArbPoints)
ArbDF <- ArbDF[,1:2]
TerrDF <- data.frame(TerrPoints)
TerrDF <- TerrDF[,1:2]

## take out points outside of the extent area
# like korea, italy, and water points
outliers <- extract(predictors, ArbPoints)
out <- which(is.na(outliers))
out
View(outliers)

# veg outliers
dropV <- c(34)
VegNew <- VegDF[-dropV,]

# dirt outliers
dropD <- c(16,37,71,98,114,137,147,166,167)
DirtNew <- DirtDF[-dropD,]

# arb outliers
dropA <- c(34)
ArbNew <- ArbDF[-dropA,]

#terr outliers
dropT <- c(16,37,71,98,114,135,145,164,165)
TerrNew <- TerrDF[-dropT,]

# assign occurrence points
occV <- VegNew
occV <- as.matrix(occV)
foldV <- kfold(occV, k=5)
occtestV <- occV[foldV == 1, ]
occtrainV <- occV[foldV != 1, ]

# assign occurrence points
occD <- DirtNew
occD <- as.matrix(occD)
foldD <- kfold(occD, k=5)
occtestD <- occD[foldD == 1, ]
occtrainD <- occD[foldD != 1, ]

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

# maxent model
VegME <- maxent(predictors, occtrainV, args=c("-J","-P"), 
                path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/VegMEC")

DirtME <- maxent(predictors, occtrainD, args=c("-J","-P"), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/DirtMEC")

# maxent model with replicates
VegMEC5 <- maxent(predictors, occtrainV, args=c("-J","-P",'replicates=5'), 
                path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/VegMEC5")

DirtMEC5 <- maxent(predictors, occtrainD, args=c("-J","-P",'replicates=5'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/DirtMEC5")

ArbMEC5 <- maxent(predictors, occtrainA, args=c("-J","-P",'replicates=5'), 
                path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMEC5")

TerrMEC5 <- maxent(predictors, occtrainT, args=c("-J","-P",'replicates=5'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMEC5")

# see the maxent results in a browser - doesnt work for replications
me

#variable importance plot
plot(me)
plot(DirtME)
plot(VegME)

# response curves
response(me)
response(DirtME)
response(VegME)

#predict to entire dataset
# Dirt prediction
DirtRC5 <- predict(DirtMEC5, predictors, progress="text",
                 filename='./Analysis_Scripts/Chapter3/ENM/Prediction/DirtRC5_prediction.grd',
                 overwrite=T)
final_mapDC5 <- mean(DirtRC5) 
plot(final_mapDC5)
writeRaster(final_mapDC5, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapDC5'))

# vegetation prediction
VegRC5 <- predict(VegMEC5, predictors, progress="text",
                filename='./Analysis_Scripts/Chapter3/ENM/Prediction/VegRC5_prediction.grd',
                overwrite=T)
final_mapVC5 <- mean(VegRC5) 
plot(final_mapVC5)
writeRaster(final_mapVC5, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapVC5'))

# terrestrial prediction
TerrRC5 <- predict(TerrMEC5, predictors, progress="text",
                  filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrRC5_prediction.grd',
                  overwrite=T)
final_mapTC5 <- mean(TerrRC5)
plot(final_mapTC5)
writeRaster(final_mapTC5, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapTC5'))

# arboreal prediction
ArbRC5 <- predict(ArbMEC5, predictors, progress="text",
                 filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbRC5_prediction.grd',
                 overwrite=T)
final_mapAC5 <- mean(ArbRC5) 
plot(final_mapAC5) 
writeRaster(final_mapAC5, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapAC5'))

# Veg prediction for 1 run
VegRC <- predict(VegME, predictors, progress="text",
                 filename='./Analysis_Scripts/Chapter3/ENM/Prediction/VegRC_prediction.grd',
                 overwrite=T)
# plot(VegRC) 
# writeRaster(VegRC, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapVC'))

# plot occurrence points on top of the niche map for specific reason, just checking
points(occ)

#############
#testing points
#############

#background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'
e1 <- evaluate(me, p=occtest, a=bg, x=predictors)
e1

Ev <- evaluate(VegME, p=occtestV, a=bg, x=predictors)
Ev

# evaluate every model from the replication in veg
EV51 <- evaluate(VegME5@models[[1]], p=occtestV, a=bg, x=predictors)
EV51 # 0.90
EV52 <- evaluate(VegME5@models[[2]], p=occtestV, a=bg, x=predictors)
EV52 # 0.91
EV53 <- evaluate(VegME5@models[[3]], p=occtestV, a=bg, x=predictors)
EV53 # 0.91
EV54 <- evaluate(VegME5@models[[4]], p=occtestV, a=bg, x=predictors)
EV54 # 0.91
EV55 <- evaluate(VegME5@models[[5]], p=occtestV, a=bg, x=predictors)
EV55 # 0.89

# evaluate every model from the replication in arboreal
aV51 <- evaluate(ArbMEC5@models[[1]], p=occtestA, a=bg, x=predictors)
aV51 # 0.85
aV52 <- evaluate(ArbMEC5@models[[2]], p=occtestA, a=bg, x=predictors)
aV52 # 0.85
aV53 <- evaluate(ArbMEC5@models[[3]], p=occtestA, a=bg, x=predictors)
aV53 # 0.89
aV54 <- evaluate(ArbMEC5@models[[4]], p=occtestA, a=bg, x=predictors)
aV54 # 0.90
aV55 <- evaluate(ArbMEC5@models[[5]], p=occtestA, a=bg, x=predictors)
aV55 # 0.89

# evaluate every model from the replication in terrestrial
tV51 <- evaluate(TerrMEC5@models[[1]], p=occtestT, a=bg, x=predictors)
tV51 # 0.84
tV52 <- evaluate(TerrMEC5@models[[2]], p=occtestT, a=bg, x=predictors)
tV52 # 0.84
tV53 <- evaluate(TerrMEC5@models[[3]], p=occtestT, a=bg, x=predictors)
tV53 # 0.85
tV54 <- evaluate(TerrMEC5@models[[4]], p=occtestT, a=bg, x=predictors)
tV54 # 0.85
tV55 <- evaluate(TerrMEC5@models[[5]], p=occtestT, a=bg, x=predictors)
tV55 # 0.84

# evaluate every model from veg with cloud cover replicate
vV51 <- evaluate(VegMEC5@models[[1]], p=occtestV, a=bg, x=predictors)
vV51 # 0.94
vV52 <- evaluate(VegMEC5@models[[2]], p=occtestV, a=bg, x=predictors)
vV52 # 0.91
vV53 <- evaluate(VegMEC5@models[[3]], p=occtestV, a=bg, x=predictors)
vV53 # 0.92
vV54 <- evaluate(VegMEC5@models[[4]], p=occtestV, a=bg, x=predictors)
vV54 # 0.92
vV55 <- evaluate(VegMEC5@models[[5]], p=occtestV, a=bg, x=predictors)
vV55 # 0.94

# evaluate every model from dirt replications
dV51 <- evaluate(DirtMEC5@models[[1]], p=occtestD, a=bg, x=predictors)
dV51 # 0.89
dV52 <- evaluate(DirtMEC5@models[[2]], p=occtestD, a=bg, x=predictors)
dV52 # 0.87
dV53 <- evaluate(DirtMEC5@models[[3]], p=occtestD, a=bg, x=predictors)
dV53 # 0.87
dV54 <- evaluate(DirtMEC5@models[[4]], p=occtestD, a=bg, x=predictors)
dV54 # 0.88
dV55 <- evaluate(DirtMEC5@models[[5]], p=occtestD, a=bg, x=predictors)
dV55 # 0.87




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





