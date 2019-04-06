######################################
###   Species  Niche   Modeling   ####
######################################

# load packages
library(raster); library(rgdal); library(dismo); library(rJava)
library(sdm); library(maptools); library(maxnet)
library(rgeos); library(mapdata); library(SDMTools)
#install.packages("SSDM")

# library(maxent)
# archived 3/5/19

library(biomod2)
library(SSDM)


Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

# note crs is in decimal degrees but you can project to meters as well
# paper stating that presence only data are reliable
  # Brotons, Thuiller 2004 Ecography Presence-absence vs presence-only modelling methods for predicting bird habitat suitability

#check class
class(Polygons)

# check crs
crs(Polygons)
projection(Polygons) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
crs(Polygons)

#all
plot(eco)
class(eco)
class(alldata)
# this takes literally forever and crashes my computer ugh
#Alleco <- raster::extract(alldata, eco)
write.csv(Alleco, file = "./Projections/Alleco.csv")

#only polys
plot(ecoStudyRegion)

#testing
meh <- raster::extract(alldata, mah)

## the goal
# grid each polygon by the coords of the climate data
# make points in center of each grid cell of polygon
# get coords and extract them and save them
# rasterize points for each species polygon

# get one species polygon as practice
mah<- Polygons[1,]
plot(mah)
crs(mah)
map.axes()

#crop climate data by poly bbox for ease
croppy <- crop(alldata_together, DirtPoly)
plot(DirtPoly)
map.axes()

#see it
plot(croppy)
plot(croppy$alt)
plot(DirtPoly, add = T)
plot(mah, add = T)

#creating the grid to put the polygons in
grid <- raster(extent(DirtPoly))
res(grid) <- 2.5
proj4string(grid) <- proj4string(DirtPoly)
gridpolygon <- rasterToPolygons(grid)
plot(gridpolygon)
plot(DirtPoly, add = T)
plot(mah, add=T)

#subset the bbox with the polygon

#pc <- spTransform(mah, CRS( "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ) )
DirtPolyy <- rgeos::gBuffer(DirtPoly, width = 0, byid=F)
plot(DirtPoly)
plot(DirtPolyy)

dry.grid <- raster::intersect(DirtPolyy, gridpolygon)
plot(dry.grid)

# looking at them all
eck <- crop(alldata, dry.grid)
plot(eck$USETHISelevation)
plot(dry.grid, add = T)
plot(yes, add=T)

#try to subset the centroids based on gridoverlap
#the gCentroids per bbox
agh <- gCentroid(gridpolygon, byid= T)
plot(agh)
#the gCentroids per grid in cropped poly grid
agg <- gCentroid(dry.grid, byid=T)
plot(agg)
agg@coords

testtest <- as.data.frame(agg)

yes <- SpatialPoints(agg, proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')),
                     bbox=NULL)
plot(yes)
class(yes)
coords <- agg@coords
DirtPolyy <- SpatialPointsDataFrame(coords = coords, data = testtest, coords.nrs = numeric(0),
                                   proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))

VegPolyy
plot(VegPolyy)
DirtPolyy
plot(DirtPolyy)
writeOGR(DirtPolyy, "./Analysis_Scripts/Chapter3/Points/DirtPoly_Points", layer= "chull", driver = "ESRI Shapefile")
DirtPoints <- readOGR("./Analysis_Scripts/Chapter3/Points/DirtPoly_Points")
plot(VegPoints, add=T)

# i can click on the map on a coord and it will tell me which one
locator()

# have alldata ready
alldata <- stack(c('./Analysis_Scripts/Chapter3/Climate Data/elevation/USETHISelevation.tif',
                   list.files('./Analysis_Scripts/Chapter3/Climate Data/Salamander Range', full.names = T, pattern = '.tif')))


# loop for extracting points from gridded polygons
PointRec <- NA
DirtPoly 
crs(DirtPoly) <- crs(VegPoly)
#190

CRS.new <- CRS("+proj=tmerc +lat_0=BLAHBLAH+lon_0=-BLAHBLAH +k=0.9999375 +x_0=250000 +y_0=0 
               +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")

#CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#311
for (i in 1:190) {
 i = 10
  Poly <- DirtPoly[i,]
  grid <- raster(extent(Poly))
  res(grid) <- 2.5
  proj4string(grid) <- proj4string(Poly)
  gridpolygon <- rasterToPolygons(grid)
  gridpolygon <- spTransform(gridpolygon, CRS.new)
  gridpolygon <- rgeos::gBuffer(gridpolygon, width = 0, byid = F)
  Poly <- spTransform(Poly, CRS.new)
  Poly <- gBuffer(Poly, width = 0, byid=T)
  dry.grid <- intersect(Poly, gridpolygon)
  centerpoints <- gCentroid(dry.grid, byid=T)
  yes <- SpatialPoints(centerpoints, proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')),
                       bbox=NULL)
  yes <- as.data.frame(yes)
  #species loop
  if (i == 1) {
    PointRec <- list(yes)
  } else {
   PointRec <- c(PointRec,  list(yes))
  }
}

PointRec

#write.csv(PointRecords, file = file = "Analysis_Scripts/Chapter3/Climate Data/Points/PointRecords.csv")
nope <- do.call(rbind.data.frame, PointRec)
nopee <- as.data.frame(nope)
names(nopee)[1] <- "lon"
names(nopee)[2] <- "lat"
write.csv(nopee, file = "./Analysis_Scripts/Chapter3/Points/check.csv")
testy <- read.csv("./Analysis_Scripts/Chapter3/Points/check.csv", header = T)
testy$binomial <- as.character(testy$binomial)
testy$binomial <- removeNumbers(testy$binomial)
testy$binomial <- removePunctuation(testy$binomial)
length(unique(testy$binomial))
coords <- testy[,2:3]

All_Poly_Points <- SpatialPointsDataFrame(coords = coords, data = testy, coords.nrs = numeric(0), match.ID = T,
                                          proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
plot(All_Poly_Points, add = T)
writeOGR(All_Poly_Points, "./Analysis_Scripts/Chapter3/Points/All_Poly_Points.csv", layer = "chull", driver = "ESRI Shapefile")
sort(table(All_Poly_Points$binomial)) # some bias from only one sample point, could change if 30 sec resolution though
# either change to 30 sec and see if it has gotten better or group the arb together now
# or try to get the point data, but then you have to simplify anyway, but how much?

# simple plot
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-140,-50), ylim=c(-10,60), axes=TRUE, col="light yellow")
points(All_Poly_Points, col='red', cex=0.75)
crs(wrld_simpl)
crs(All_Poly_Points)
eco <- rgdal::readOGR('./Analysis_Scripts/Chapter3/Borders', 'provinces', verbose = F)
crs(eco)

## for overlapping species but I already know they are where they are supposed to be 
plot(eco)
ovr <- over(All_Poly_Points, eco)
head(ovr)
cntr <- ovr$NAME
i <- which(is.na(cntr))

## start to make background points
alldata
alldata@extent
plot(alldata)

# you can change this is the predictor variables change
mask <- raster(alldata[1])
alldata[1]
crs(mask) <- crs(eco)
randomSites <- randomPoints(mask, 100)

# lauren seeing if we can group veg/rock/dirt/water instead
# dont have to use

VegPoly
 # load in the point file
Vegclim <- crop(alldata, VegPoly)
plot(Vegclim$USETHISelevation)
plot(VegPolyy, add= T)
Vegraster <- raster(VegPolyy)
res(Vegraster)
VegPolyy
VegPolyy$y = lat

#crop veg clim further
max.lat = ceiling(max(VegPoints$y) + 5)
min.lat = floor(min(VegPoints$y) - 5)
max.lon = ceiling(max(VegPoints$x) + 5)
min.lon = floor(min(VegPoints$x) - 5)
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))
geographic.extent
predictors.crop <- crop(x = alldata, y = geographic.extent)
Vegclim <- predictors.crop

#background data
mask <- Vegclim
plot(mask[[3]]) # I'm using only one layer here (this problem took me a while to understand)
ncell(mask[[3]])
set.seed(Sys.time())
background <- randomPoints(mask[[3]], 500 )
anyNA(background)
#background <- na.omit(background)
plot(!is.na(mask[[3]]), legend=FALSE) # Somthing wrong here
points(background, cex=0.5)
background
points(VegPolyy)

coords <- background[,1:2]
background <- as.data.frame(background)
background <- SpatialPointsDataFrame(coords = coords, data=background, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))

presvals <- extract(alldata, VegPolyy)
presvals <- na.omit(presvals)
absvals <- extract(Vegclim, background)
absvals <- na.omit(absvals)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals))) # This tells which data is associated with presence (1) and abscence (2)
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmcoords <- data.frame(cbind(pb, rbind(VegPolyy@coords, background@coords)))
# sdmdata[,'biome'] <- as.factor(sdmdata[,'biome']) # I don't have this variable in my data set. Note for later: I haven't checked high correlation between variables.
head(sdmdata)
dim(sdmdata)
head(sdmdata)
anyNA(sdmdata)
sdmdata

# Look at variable correlation
pairs(sdmdata[,2:21], cex=0.1, fig=TRUE) # change collums, we can see some variable look highly correlated, this takes some time


### doesnt work im moving on with my life ugh ##
#######
# install by devtool
#devtools::install_github("adamlilith/legendary")
library(legendary)
library(stats)
class(sdmdata)
#sdmdata <- na.omit(sdmdata)
correl <- cor(sdmdata[,2:21], method = "pearson")
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

dimnames(sdmdata)

m1 <- glm(pb ~ WC201 + WC206 + WC212 + WC214, data=sdmdata)
prediction1 <- predict(Vegclim, m1, type = "response") 
plot(prediction1)

autoSelectModel <- maxnet(sdmdata[,1], data=sdmdata[,2:length(sdmdata)]) 
autoSelectMap <- predict(Vegclim, autoSelectModel, type = "logistic")
plot(autoSelectMap)
points(VegPolyy, pch=21, bg="blue")

manualSelectModel <- maxnet(sdmdata[,1], data=sdmdata[,c("USETHISelevation","WC201", "WC206", "WC212", "WC214")])
manualSelectMap <- predict(Vegclim, manualSelectModel, type = "logistic")
plot(manualSelectMap)

## modeling?
Vegclim <- stack(Vegclim)

myBiomodData <- BIOMOD_FormatingData(resp.var = sdmdata$pb,
                                     resp.xy = sdmcoords,
                                     expl.var = Vegclim)

######

DirtPoly
RockPoly
WaterPoly

# agh trying ssdm LETS GOOOO

# algorithms = GLM, MARS, MAXENT, RF (BRT)
# Occurrences = data.frame
# Env = rasterstack of variables
# Xcol = char. column name w/x var in coords
# Ycol = char. column name w/y var in coords
# Pcol = NULL
# rep = 10 fold? maybe try 1
# name = Test.Ensemble
# save? not yet
# cv = k-fold       unsure why people chose this
# cv.param = 10?
# thresh = 0.7?
# metric = ROC?
# axes.metric = AUC?
# uncertainty = makes uncertainty map = T?
# tmp = T
# ensemble.metric = AUC
# ensemble.thresh = 0.7?
# weight = try later?
# verbose = T ?
# GUI = F

#VegPoints@coords[,1] = lon
class(VegPoints)
VegDF <- data.frame(VegPoints)
class(Vegclim)
Vegclim <- stack(Vegclim)

ensemble_modelling(algorithms=c("GLM","MARS","RF","MAXENT"), Occurrences=VegDF, Env = Vegclim, Xcol = "coords.x1",
                   Ycol = "coords.x2", Pcol = NULL, rep = 1, name = "Test.Ensemble", save = T,
                   PA = NULL, cv = "k-fold", cv.param = c(10,1),
                   thresh = 1001, metric = "ROC", axes.metric = "AUC",
                   uncertainty = TRUE, tmp = T, ensemble.metric = c("AUC"),
                   ensemble.thresh = c(0.7), weight = TRUE, verbose = TRUE, GUI = FALSE)
#library(raster)
str_name <-'Test.Ensemble/Rasters/Probability.tif' 
imported_raster=raster(str_name)
plot(imported_raster)
par(mar=c(1,1,1,1))












