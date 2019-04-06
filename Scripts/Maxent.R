###############################
### MAXENT SCRIPT DRAFT #######
###############################

library(dismo); library(rJava); library(maptools)
library(raster)

# for maxent
# library(devtools)
# install_github("johnbaums/rmaxent")
# library(rmaxent)
# get_maxent(version = "latest", quiet = FALSE)

###### maxent pipeline #########
# load variables
# elevation <- stack('./Analysis_Scripts/Chapter3/Climate Data/alt_2-5m_bil/alt.bil')
# 
# bioclim_vars <- stack(c(list.files('./Analysis_Scripts/Chapter3/Climate Data/wc2', full.names = T, pattern = '.tif')))
# bioclim_keep <- stack(bioclim_vars$wc2.0_bio_2.5m_1, bioclim_vars$wc2.0_bio_2.5m_5, bioclim_vars$wc2.0_bio_2.5m_6,
#                       bioclim_vars$wc2.0_bio_2.5m_16, bioclim_vars$wc2.0_bio_2.5m_17)
# 
# extra <- stack(c(list.files('./Analysis_Scripts/Chapter3/Climate Data/NewWorld_current_2', full.names = T, pattern = '.tif')))
# extra_keep <- stack(extra$current_2.5arcmin_climaticMoistureIndex, extra$current_2.5arcmin_PETDriestQuarter,
#                     extra$current_2.5arcmin_PETWettestQuarter)
# 
# # make each at the same extent
# elevation1 <- crop(elevation, extra_keep)
# bioclim_keep1 <- crop(bioclim_keep, extra_keep)
# 
# #combine into stack
# alldata_together <- stack(elevation1, bioclim_keep1, extra_keep)
# writeRaster(alldata_together, paste0('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTog',
#             format = 'GTiff'))

# READ IN DATA #
alldata_tog <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogGTiff.gri')

# get polygons for checking
#Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors.crop <- crop(x = alldata_tog, y = geographic.extent)

# check the extent
#plot(predictors.crop$alt)
#plot(Polygons, add=T)

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
                path="./Analysis_Scripts/Chapter3/SDM/Maxent_Files/VegME")

DirtME <- maxent(predictors, occtrainD, args=c("-J","-P"), 
                 path="./Analysis_Scripts/Chapter3/SDM/Maxent_Files/DirtME")


# maxent model with replicates
VegME5 <- maxent(predictors, occtrainV, args=c("-J","-P",'replicates=5'), 
                path="./Analysis_Scripts/Chapter3/SDM/Maxent_Files/VegME5", silent=F)

DirtME5 <- maxent(predictors, occtrainD, args=c("-J","-P",'replicates=5'), 
                 path="./Analysis_Scripts/Chapter3/SDM/Maxent_Files/DirtME5",silent=T)

ArbME5 <- maxent(predictors, occtrainA, args=c("-J","-P",'replicates=5'), 
                path="./Analysis_Scripts/Chapter3/SDM/Maxent_Files/ArbME5",silent=F)

TerrME5 <- maxent(predictors, occtrainT, args=c("-J","-P",'replicates=5'), 
                 path="./Analysis_Scripts/Chapter3/SDM/Maxent_Files/TerrME5")


# see the maxent results in a browser
me
DirtME

#variable importance plot
plot(me)
plot(DirtME)
plot(VegME)

# response curves
response(me)
response(DirtME)
response(VegME)

#predict to entire dataset
DirtR5 <- predict(DirtME5, predictors, progress="text",
                 filename='./Analysis_Scripts/Chapter3/SDM/Prediction/DirtR5_prediction.grd',
                 overwrite=T)
final_mapD5 <- mean(DirtR5) 
plot(final_mapD5)
writeRaster(final_mapD5, paste0('./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapD5'))

VegR5 <- predict(VegME5, predictors, progress="text",
                filename='./Analysis_Scripts/Chapter3/SDM/Prediction/VegR5_prediction.grd',
                overwrite=T)
final_mapV5 <- mean(VegR5) 
plot(final_mapV5)
writeRaster(final_mapV5, paste0('./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapV5'))

TerrR5 <- predict(TerrME5, predictors, progress="text",
                  filename='./Analysis_Scripts/Chapter3/SDM/Prediction/TerrR5_prediction.grd',
                  overwrite=T)
final_mapT5 <- mean(TerrR5)
plot(final_mapT5)
writeRaster(final_mapT5, paste0('./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapT5'))

ArbR5 <- predict(ArbME5, predictors, progress="text",
                 filename='./Analysis_Scripts/Chapter3/SDM/Prediction/ArbR5_prediction.grd',
                 overwrite=T)
final_mapA5 <- mean(ArbR5) 
plot(final_mapA5) 
writeRaster(final_mapA5, paste0('./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapA5'))

# plot occurrence points on top of the niche map
points(occ)

#testing

#background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'
e1 <- evaluate(me, p=occtest, a=bg, x=predictors)
e1

Ev <- evaluate(VegME, p=occtestV, a=bg, x=predictors)
Ev

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


# alternative 1
# extract values
pvtest <- data.frame(extract(predictors, occtestV))
avtest <- data.frame(extract(predictors, bg))
e2 <- evaluate(, p=pvtest, a=avtest)
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





