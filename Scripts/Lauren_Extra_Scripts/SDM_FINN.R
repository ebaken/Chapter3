# ============================================================================== #
# Script attempting to project Ficus petiolaris distribution
# following sdm.pdf
# ============================================================================== #

setwd("C:/Finn/Doctorat/Analyzes/SDM/")

# install.packages(c('raster', 'rgdal', 'dismo', 'rJava', 'SDMTools'))
library(raster)
library(rgdal)
library(dismo)
library(rJava)
library(SDMTools)
library(maptools)
library(rgbif)
library(spocc)
library(rinat)
library(biomod2)

# ============================================================================== #
# Part 0 Getting occurence data with GBIF and INaturalist
# ============================================================================== #

# rgif
# fpetocc <- occ_download('taxonKey = ???', 'hasCoordinate = TRUE') # Not done yet, what is the taxon key for F. petiolaris? API key needed?
# occ_download_meta(fpetocc)
# fpetocc_data <- occ_download_get("???")

# inat
# out <- occ(query = "Ficus petiolaris", from = "inat")
# inat_out <- get_inat_obs(taxon = "Ficus petiolaris")
# Which doesn't seem to work.

# Let's give up for now, I will come back to this to make sure I have all the occurence data. My dataset has a lot already but I don't think it has all


# ============================================================================== #
# Part I Data preparation
# ============================================================================== #

# ------------------------------------------------------------------------------ #
# 1: Importing occurrence data
# ------------------------------------------------------------------------------ #

df_occurence <- read.csv("C:/Finn/Doctorat/Data Base/Occurence Data/Combined Coordinates for F. petiolaris.csv", h=T) # This combine GPS coordiantes + data obtained by Nico, José and Kevin
colnames(df_occurence) <- c("species", "ID", "lat", "lon")
df_coordinates  <- df_occurence[,c(4,3)]
head(df_coordinates)
dim(df_coordinates)

# ------------------------------------------------------------------------------ #
# 2: Data cleaning
# ------------------------------------------------------------------------------ #

# Check for duplicates
duplicates <- duplicated(df_coordinates[, c('lon', 'lat')])
# number of duplicates
sum(duplicates)

# keep the records that are not duplicated
df_occ_dup_removed <- df_coordinates[!duplicates, ]
dim(df_occ_dup_removed)

# Simple plot: 
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-115,-85), ylim=c(10,35), axes=TRUE, col="light yellow") # Here center arund Mexico
# restore the box around the map
box()
# plot points
points(df_occ_dup_removed$lon, df_occ_dup_removed$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(df_occ_dup_removed$lon, df_occ_dup_removed$lat, col='red', cex=0.75)

#Better boudaries? 
# getData('gadm', country='BOL', level=0) to get
# getData('countries')

# Cross Checking
sp_occ <- SpatialPointsDataFrame(coords=cbind(df_occ_dup_removed$lon, df_occ_dup_removed$lat), data=df_occ_dup_removed, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
ovr <- over(sp_occ, wrld_simpl)
head(ovr)
cntr <- ovr$NAME
outofmap <- which(is.na(cntr))
 
# Here it tells me that some points are in the Ocean (easily seen on the map plotted around Baja and islands).
# It seems that it is mostly the trees from 179 (looks like it but not sure)

# j <- which(cntr != df_occ_cleaned$country)
# for the mismatches, bind the country names of the polygons and points
# cbind(cntr, df_occ_cleaned$country)[j,]
# plot(df_occ_cleaned)
# plot(wrld_simpl, add=T, border='blue', lwd=2)
# points(df_occ_cleaned[j, ], col='red', pch=20, cex=2)
# Not important for me since everything is in Mexico

# Remove points in the ocean
tmp <- as.data.frame(sp_occ@coords)
colnames(tmp) <- c("lon", "lat")
df_occ_cleaned <- tmp[-outofmap,]

#What points are out of map?
df_outofmap <- tmp[outofmap,]
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-115,-85), ylim=c(12,31), axes=TRUE, col="light yellow")
box()
points(df_outofmap$lon, df_outofmap$lat, col='orange', pch=20, cex=1.5)
points(df_outofmap$lon, df_outofmap$lat, col='red', cex=1.5)

#Add one occurence at 179
df_occ_cleaned <- rbind(df_occ_cleaned, (c(-111.349716, 25.913455)))

# Clear memory
rm(tmp, outofmap, ovr, df_coordinates, df_occurence, df_occ_dup_removed, df_outofmap)

# Replot on map
head(df_occ_cleaned)
plot(wrld_simpl, xlim=c(-115,-105), ylim=c(20,30), axes=TRUE, col="light yellow") # This is centered around Baja where I had most of datapoints in the water
box()
points(df_occ_cleaned$lon, df_occ_cleaned$lat, col='orange', pch=20, cex=0.75)
points(df_occ_cleaned$lon, df_occ_cleaned$lat, col='red', cex=0.75)
sp_occ_cleaned <- SpatialPointsDataFrame(coords=cbind(df_occ_cleaned$lon, df_occ_cleaned$lat), data=df_occ_cleaned, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

# Sample bias
# create a RasterLayer 
occ_raster_tmp <- raster(sp_occ_cleaned)
# set the resolution of the cells to (for example) 0.5 degree
res(occ_raster_tmp) <- 0.5
# expand (extend) the extent of the RasterLayer a little
occ_raster_tmp <- extend(occ_raster_tmp, extent(occ_raster_tmp)+1)
# sample:
sp_occ_resamp <- gridSample(sp_occ_cleaned, occ_raster_tmp, n=1)
# to illustrate the method and show the result
occ_polygon <- rasterToPolygons(occ_raster_tmp)
plot(occ_polygon, border='gray')
points(sp_occ_cleaned)
# selected points in red
points(sp_occ_resamp, cex=1, col='red', pch='x')

# Check the re-sampled data
head(sp_occ_resamp)
plot(wrld_simpl, xlim=c(-110,-90), ylim=c(10,35), axes=TRUE, col="light yellow")
box()
points(sp_occ_resamp, col='orange', pch=20, cex=0.75)
points(sp_occ_resamp, col='red', cex=0.75)
# locator() # This is to select a point on the map. Select with mouse then escape

# Change into a dataframe
str(sp_occ_resamp)
df_occ_resamp<- as.data.frame(sp_occ_resamp)
colnames(df_occ_resamp) <- c("lon", "lat")

# ------------------------------------------------------------------------------ #
head(df_occ_resamp) # This is the final occurence data set. Think about reducing the grid resolution for more data points
# ------------------------------------------------------------------------------ #

# Save cleaned and re-sampled (to reduce bias)
# write.csv(df_occ_resamp, "Combined Coordinates for F. petiolaris - Cleaned and Re-sampled - res0.5.csv") # Not done yet


# ------------------------------------------------------------------------------ #
# 3: Import environmental Data
# ------------------------------------------------------------------------------ #

# Worldcim data
bioclim.data <- getData(name = "worldclim",
                        download=TRUE,
                        var = "bio",
                        res = 2.5,
                        path = "data/")

elevation.data <- getData(name = "worldclim",
                        download=TRUE,
                        var = "alt",
                        res = 2.5,
                        path = "data/")


plot(bioclim.data) # Already a raster stack
bioclim.data@extent # looks good
plot(elevation.data)
elevation.data@extent # Same

# Rasterize occurence data (can't be plotted as there is no data associated with the occurences)
sp_final_occ <- SpatialPointsDataFrame(coords=cbind(df_occ_resamp$lon, df_occ_resamp$lat), data=df_occ_resamp, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
occ_raster <- raster(sp_final_occ)
res(occ_raster)

# Plot occurence data on one climate layer
plot(bioclim.data, 1)
plot(wrld_simpl, add=TRUE)
points(sp_final_occ, col='blue')

# Stack elevation and bioclimatic variables
predictors <- stack(bioclim.data, elevation.data)

# ------------------------------------------------------------------------------ #
# Crop the bioclim data
max.lat = ceiling(max(df_occ_resamp$lat) + 2)
min.lat = floor(min(df_occ_resamp$lat) - 2)
max.lon = ceiling(max(df_occ_resamp$lon) + 2)
min.lon = floor(min(df_occ_resamp$lon) - 2)
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))
geographic.extent
predictors.crop <- crop(x = predictors, y = geographic.extent)

# Replot to visualize cropped predictors
plot(predictors.crop, 1)
plot(wrld_simpl, add=TRUE)
points(sp_final_occ, col='blue')

# Future climate will be obtained later

# ------------------------------------------------------------------------------ #
# Explore the environmental data






# ------------------------------------------------------------------------------ #
# 4: Absence and background points
# ------------------------------------------------------------------------------ #

# Background data: Here I create background data in the cropped layer (done before in sdm.pdf)
mask <- predictors.crop
plot(mask[[3]]) # I'm using only one layer here (this problem took me a while to understand)
ncell(mask[[3]])
set.seed(Sys.time())
background <- randomPoints(mask[[3]], 5000 ) # Takes for ever, large number almost crashed my computer. Is 5000 too high?
plot(!is.na(mask[[3]]), legend=FALSE) # Somthing wrong here
points(background, cex=0.5)
points(sp_final_occ)


# All the following is from sdm.pdf and apparently I can have similar results with the function rasterize.
# These seem to be to get something with the same purpose as the background data but with another method.
# It works but not sure if I have to do it
# x <- circles(sp_final_occ, d=50000, lonlat=TRUE)
# pol <- polygons(x)
# samp1 <- spsample(pol, 250, type='random', iter=25)
# cells <- cellFromXY(mask[[3]], samp1)
# length(cells)
# cells <- unique(cells)
# length(cells)
# xy <- xyFromCell(mask[[3]], cells)
# plot(pol, axes=TRUE)
# points(xy, cex=0.75, pch=20, col='blue')
# spxy <- SpatialPoints(xy, proj4string=CRS('+proj=longlat +datum=WGS84'))
# o <- over(spxy, geometry(x))
# xyInside <- xy[!is.na(o), ]
# v <- extract(mask[[3]], x@polygons, cellnumbers=T)
# v <- do.call(rbind, v)
# v <- unique(v[,1])
# head(v)
# m <- mask[[3]]
# m[] <- NA
# m[v] <- 1
# plot(m, ext=extent(x@polygons)+1)
# plot(x@polygons, add=T)

# ============================================================================== #
# Part II Model fitting, prediction, and evaluation
# ============================================================================== #

# Extracting values from rasters (This part is from the earthskysea workshop)
presvals <- extract(predictors.crop, sp_final_occ)
absvals <- extract(predictors.crop, background)
head(presvals)
head(absvals)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals))) # This tells which data is associated with presence (1) and abscence (2)
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
# sdmdata[,'biome'] <- as.factor(sdmdata[,'biome']) # I don't have this variable in my data set. Note for later: I haven't checked high correlation between variables.
# head(sdmdata)
dim(sdmdata)
head(sdmdata)

# Look at variable correlation
pairs(sdmdata[,2:21], cex=0.1, fig=TRUE) # change collums, we can see some variable look highly correlated, this takes some time
library(legendary)
correl <- cor(sdmdata[,2:21], method='spearman')
print(correl, digits=2)
pos <- correl > 0.7
neg <- correl < -0.7
spoke(
  pos=pos,
  neg=neg,
  lwdPos=2,
  lwdNeg=2,
  colPos='black',
  colNeg='red',
  pty='s'
)

# Look at the variables above, select ones not correlated to many others (bio2 good candidate, bio1 probably not)
# Chose if model select the variables to use itself, or remove correlated variable manually 


# ------------------------------------------------------------------------------ #
# Model fitting and prediction
m1 <- glm(pb ~ bio1 + bio5 + bio12, data=sdmdata)
prediction1 <- predict(predictors.crop, m1) 
plot(prediction1)


m2 <- glm(pb ~ ., data=sdmdata) # I didn't remove correlated variables, I also get a warning message
prediction2 <- predict(predictors.crop, m2) # I didn't remove correlated variables, I also get a warning message
plot(prediction2)

library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 8, name = "Blues")
plot(prediction1, col=my.palette) 
plot(prediction1, col=brewer.pal(11,"Spectral"))

# Here I'm trying to use the maxent command from earthskysea's exercise and chose variables more carefully 
library(maxnet)
sdmdata_formated <- na.omit(sdmdata)
autoSelectModel <- maxnet(sdmdata_formated[,1], data=sdmdata_formated[,2:length(sdmdata_formated)]) 
autoSelectMap <- predict(predictors.crop, autoSelectModel, type = "logistic")
plot(autoSelectMap) # Looks ok but something seems wrong: maximum prediction is close to 0??
points(sp_final_occ$lon, sp_final_occ$lat, pch=21, bg="blue")
# Works but sucks!! Why negative values?

# Apparently no need to remove NAS
# autoSelectModel <- maxnet(sdmdata[,1], data=sdmdata[,2:length(sdmdata)]) 
# autoSelectMap <- predict(predictors.crop, autoSelectModel)
# plot(autoSelectMap)
# points(sp_final_occ$lon, sp_final_occ$lat, pch=21, bg="blue")

manualSelectModel <- maxnet(sdmdata[,1], data=sdmdata[,c("bio2", "bio3", "bio4", "bio5", "bio6", "bio12", "bio14", "bio15", "alt")])
manualSelectMap <- predict(predictors.crop, manualSelectModel, type = "logistic")
plot(manualSelectMap)
# Fucking worst!



# ------------------------------------------------------------------------------ #
# Model evaluation









# ============================================================================== #
# Projecting future:
# ============================================================================== #

fut1 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi701.tif")
fut2 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi702.tif")
fut3 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi703.tif")
fut4 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi704.tif")
fut5 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi705.tif")
fut6 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi706.tif")
fut7 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi707.tif")
fut8 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi708.tif")
fut9 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi709.tif")
fut10 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7010.tif")
fut11 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7011.tif")
fut12 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7012.tif")
fut13 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7013.tif")
fut14 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7014.tif")
fut15 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7015.tif")
fut16 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7016.tif")
fut17 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7017.tif")
fut18 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7018.tif")
fut19 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Projection/Worldclim/CCSM4_rcp85/bc85bi70/bc85bi7019.tif")

# Code to combine raster layers of different extent:

a<-extent(-45.85728, -43.76855, -2.388705, -0.5181549)
b<-extent(-45.87077, -43.78204, -2.388727, -0.5208711) 
c<-extent(-45.81952 ,-43.7173 , -2.405129 ,-0.5154312)
extent_list<-list(a, b, c)

# make a matrix out of it, each column represents a raster, rows the values
extent_list<-lapply(extent_list, as.matrix)
matrix_extent<-matrix(unlist(extent_list), ncol=length(extent_list))
rownames(matrix_extent)<-c("xmin", "ymin", "xmax", "ymax")
# create an extent with the extrem values of your extent
best_extent<-extent(min(matrix_extent[1,]), max(matrix_extent[3,]),        min(matrix_extent[2,]), max(matrix_extent[4,]))
# the range of your extent in degrees
ranges<-apply(as.matrix(best_extent), 1, diff)
# the resolution of your raster (pick one) or add a desired resolution
reso<-res(r1)
# deviding the range by your desired resolution gives you the number of rows and columns
nrow_ncol<-ranges/reso
# create your raster with the following
s<-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=r1@crs)
# use this raster to reproject your original raster (since your using the same crs,
# resample should work fine
r1<-resample(r1, s, method="ngb")


# Or ... more simply try this:
##Resample r1 to extent/resolution of r2 
r1rsmp <- resample(r1,r2, resample='bilinear') 

##Create stack 'r1r2stk' 
r1r2stk <- stack(r1rsmp, r2) 

##Plot them to make sure it looks right... 
#plot(r1r2stk) 


# ============================================================================== #
# BETTER DATA
# ============================================================================== #

# This code was initially above but I will first do stuff without high resolution data, then do it


# Here I do not follow the manual but will later
# Climate data from ClimateNA
# Global averages: C:\Finn\Doctorat\Analyzes\Climate Data\Current Data\Global averages

# In DATABASE not: change directon
bio1 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/AHM.asc")
bio2 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/bFFP.asc")
bio3 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/CMD.asc")
bio4 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/DD_0.asc")
bio5 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/DD_18.asc")
bio6 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/DD18.asc")
bio7 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/DD5.asc")
bio8 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/eFFP.asc")
bio9 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/EMT.asc")
bio10 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/Eref.asc")
bio11 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/EXT.asc")
bio12 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/FFP.asc")
bio13 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/MAP.asc")
bio14 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/MAR.asc")
bio15 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/MAT.asc")
bio16 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/MCMT.asc")
bio17 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/MSP.asc")
bio18 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/MWMT.asc")
bio19 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/NFFD.asc")
bio20 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/PAS.asc")
bio21 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/PPT_sm.asc")
bio22 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/PPT_wt.asc")
bio23 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/RH.asc")
bio24 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/SHM.asc")
bio25 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/Tave_sm.asc")
bio26 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/Tave_wt.asc")
bio27 <- raster("C:/Finn/Doctorat/Analyzes/Climate Data/Current Data/Global averages/TD.asc")
# EDIT: I need to use projectRaster to change to extent, check how to do later

# predictors <- stack(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19, bio20, bio21, bio22, bio23, bio24, bio25, bio26, bio27)
# predictors
# names(predictors)
# plot(predictors) # Just plotting 16? 

# predictors@extent # I don't understand why these exent are off that way, I can transform them? I will deal with these later, Chelsa might be better but more heavy

# Soil thickness characteristcis
# From https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1304
# C:\Finn\Doctorat\Analyzes\Soil Data\Gridded Thickness

dev.off()
geo1 <- raster("C:/Finn/Doctorat/Analyzes/Soil Data/Gridded Thickness/average_soil_and_sedimentary-deposit_thickness.tif")
plot(geo1) # This one is not at the same resolution and extent, will cause problem to combine with other layers
geo1

# Soil classification
# From
# C:\Finn\Doctorat\Analyzes\Soil Data\SoilGrid\TAXNWRB

geo2 <- raster("C:/Finn/Doctorat/Analyzes/Soil Data/SoilGrid/TAXNWRB/TAXNWRB_250m_ll.tif")
plot(geo2) # This looks weird, as if soil type became a continuous variable
plot(geo2, xlim=c(-125,-100), ylim=c(15,35))
geo2
as.factor(geo2) # Seems to work but I might have to specify what are the factors, takes a lot of time
levels(geo2)
plot(geo2)

geo1@extent # This one looks good
geo2@extent # This one not really

# Worldcim data
bioclim.data <- getData(name = "worldclim",
                        download=TRUE,
                        var = "bio",
                        res = 2.5,
                        path = "data/")

plot(bioclim.data)
bioclim.data@extent # looks good

# Stack try 1
predictors <- stack(bioclim.data, geo1, geo2) # Doesn't work, some difference in extent (doesn't it matter here?) and different resolutions

# Crop predictors (not is the sdm.pdf file)
# Determine geographic extent of our data
max.lat = ceiling(max(df_occ_resamp$lat))
min.lat = floor(min(df_occ_resamp$lat))
max.lon = ceiling(max(df_occ_resamp$lon))
min.lon = floor(min(df_occ_resamp$lon))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))
geographic.extent

crop1 <- crop(x = geo1, y = geographic.extent)
crop2 <- crop(x = geo2, y = geographic.extent)
crop3 <- crop(x = bioclim.data, y = geographic.extent)

# Stack try 2
predictors_crop <- stack(crop1, crop2, crop3) 
# As expected it doesn't work: they don't have the same resolution. So I will have to resample in some raster layers.

plot(crop1)
plot(crop2)
plot(crop3)

# Resample
crop1
crop2 # Crop 2 has the higher resolution
crop3
crop1_res <- projectRaster(crop1, crop2, method='bilinear') # from crop1 to the extent (here the same) and resolution of crop2
crop3_res <- projectRaster(crop3, crop2, method='bilinear') # Takes a while
# If the raster layer is categorical we need to use 'ngb' method, here crop2 is but is not changed.

crop1_res[crop1_res > 200] <- NA # This is to remove
plot(crop1_res)
plot(crop2)
plot(crop3_res)
predictors_crop <- stack(crop1_res, crop2, crop3_res) # Now it works! Now we have bioclim data from Woldclim and 2 soil data layer
predictors_crop

# Save this raster in case the computers crashes (almost happened)
writeRaster(predictors_crop, filename = "Stacked raster soil and worldclim croped for F.petiolaris.tif", format = "GTiff") # Takes a while
# predictors_crop <- raster("Stacked raster soil and worldclim croped for F.petiolaris.tif")
# I had "restarting interrupted promise evaluation" when using projectRaster in crop 3 though, if weir I should rerun this command

# Because of the deadline, I will try to do something with a lower resolution
sp_final_occ <- SpatialPointsDataFrame(coords=cbind(df_occ_resamp$lon, df_occ_resamp$lat), data=df_occ_resamp, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
occ_raster <- raster(sp_final_occ)
res(occ_raster)
crop1_res_2 <- projectRaster(crop1, crop3, method='bilinear')
crop2_res_2 <- projectRaster(crop2, crop3, method='bilinear')
as.factor(crop2_res_2)
crop1_res_2[crop1_res_2 > 200] <- NA
predictors_crop_reduced <- stack(crop1_res_2, crop2_res_2, crop3) 
predictors_crop_reduced
plot(predictors_crop_reduced)

plot(predictors_crop_reduced, 1)
plot(wrld_simpl, add=TRUE)
points(sp_final_occ, col='blue')

writeRaster(predictors_crop_reduced, filename = "Stacked raster soil and worldclim croped for F.petiolaris - Reduced.tif", format = "GTiff") # Takes a while


