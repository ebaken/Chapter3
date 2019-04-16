###################################################
## MAKING OCC POINTS FOR SUBSTRATE/MICROHABITAT ###
###################################################

# MAKING POINTS FROM POLYGONS TO USE IN MAXENT ANALYSIS

# Packages
library(raster); library(rgdal); library(dismo); library(rJava)
library(sdm); library(maptools); library(maxnet)
library(rgeos); library(mapdata); library(SDMTools)

# Polygons
Polygons <- readOGR("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

#check class
class(Polygons)

# check crs
crs(Polygons)
projection(Polygons) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
crs(Polygons)

# Check polygon labels
Polygons$binomial <- as.character(Polygons$binomial)
Polygons$binomial[which(Polygons$binomial == "Eurycea longicauda melanopleura")] <- "Eurycea longicauda" # combine polys of subspecies together
Polygons <- aggregate(Polygons, by = "binomial")

### load clasifications
Class <- read.csv("./Data/Pruned/MicrohabitatsPruned.csv")
Class$Species <- as.character(Class$Species)
Class$Species[which(Class$Species == "Pseudoeurycea lineolus")] <- "Pseudoeurycea lineola" #match the species names
Class1 <- Class[match(Polygons$binomial, Class$Species), ]
anyNA(Class1$Species) # want FALSE

# prune the polygons by substrate and micro habitat
#
###### substrate ##########
#
### prune dirt
Dirt <- Class1[Class1$ArbVegStrict == "D", ]
Dirt$Species #189
DirtPoly <- Polygons[match(Dirt$Species,Polygons$binomial), ]
DirtPolyy <- rgeos::gBuffer(DirtPoly, width = 0, byid=F)
DirtPolyAll <- aggregate(DirtPolyy, dissolve=T)
DirtPolyAll.SP <- SpatialPolygonsDataFrame(DirtPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(DirtPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/DirtPolyAll", layer= "chull", driver = "ESRI Shapefile")

# prune veg
Veg <- Class1[Class1$ArbVegStrict == "V", ]
Veg$Species #71
VegPoly <- Polygons[match(Veg$Species,Polygons$binomial), ]
VegPolyAll <- aggregate(VegPoly, dissolve=T)
VegPolyAll.SP <- SpatialPolygonsDataFrame(VegPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(VegPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/VegPolyAll", layer= "chull", driver = "ESRI Shapefile")

# prune rock
Rock <- Class1[Class1$ArbVegStrict == "R", ]
Rock$Species #18
RockPoly <- Polygons[match(Rock$Species,Polygons$binomial), ]
RockPolyAll <- aggregate(RockPoly, dissolve=T)

# prune water
Water <- Class1[Class1$ArbVegStrict == "W", ]
Water$Species #21
WaterPoly <- Polygons[match(Water$Species,Polygons$binomial), ]
WaterPolyAll <- aggregate(WaterPoly, dissolve=T)
#
##################
#
##### microhabitat #########
#
### prune arboreal polys - A ##
Arb <- Class1[Class1$Strict == "A", ]
Arb$Species #60 is around the right number
ArbPoly <- Polygons[match(Arb$Species,Polygons$binomial), ]  
ArbPolyAll <- aggregate(ArbPoly, dissolve=T)
ArbPolyAll.SP <- SpatialPolygonsDataFrame(ArbPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(ArbPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/ArbPolyAll", layer= "chull", driver = "ESRI Shapefile")
### prune terrestrial polys - T ###
Terr <- Class1[Class1$Strict == "T", ]
Terr$Species #207
TerrPoly <- Polygons[match(Terr$Species, Polygons$binomial), ] 
TerrPolyy <- rgeos::gBuffer(TerrPoly, width = 0, byid=F)
TerrPolyAll <- aggregate(TerrPolyy, dissolve=T)
TerrPolyAll.SP <- SpatialPolygonsDataFrame(TerrPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(TerrPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/TerrPolyAll", layer= "chull", driver = "ESRI Shapefile")

### prune aquatic polys - W##
Aqua <- Class1[Class1$Strict == "W", ]
Aqua$Species # 32
AquaPoly <- Polygons[match(Aqua$Species, Polygons$binomial), ]
AquaPolyAll <- aggregate(AquaPoly, dissolve=T)

### prune aquatic polys - C##
Cave <- Class1[Class1$Strict == "C", ]
Cave$Species # 11
CavePoly <- Polygons[match(Cave$Species, Polygons$binomial), ]
CavePolyAll <- aggregate(CavePoly, dissolve=T)

### prune aquatic polys - S##
Sax <- Class1[Class1$Strict == "S", ]
Sax$Species # 7
SaxPoly <- Polygons[match(Sax$Species, Polygons$binomial), ]
SaxPolyAll <- aggregate(SaxPoly, dissolve=T)

### prune aquatic polys - F##
Foss <- Class1[Class1$Strict == "F", ]
Foss$Species  # 3
FossPoly <- Polygons[match(Foss$Species, Polygons$binomial), ]
FossPolyAll <- aggregate(FossPoly, dissolve=T)
#
##############


# make points and point files with all of the PolyAll files

# FOR SUBSTRATE - DIRT - 187 points
#creating the grid to put the polygons in
gridSD <- raster(extent(DirtPolyAll))
res(gridSD) <- 2.5
proj4string(gridSD) <- proj4string(DirtPolyAll)
gridpolygonSD <- rasterToPolygons(gridSD)
dry.gridSD <- raster::intersect(DirtPolyAll, gridpolygonSD)
SD <- gCentroid(dry.gridSD, byid=T)
SDDF <- as.data.frame(SD)
coords <- SD@coords
DirtPoints <- SpatialPointsDataFrame(coords = coords, data = SDDF, coords.nrs = numeric(0),
                                    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(DirtPoints, "./Analysis_Scripts/Chapter3/Points/Dirt_Points", layer= "chull", driver = "ESRI Shapefile")


# FOR SUBSTRATE - VEG - 60 points
#creating the grid to put the polygons in
gridSV <- raster(extent(VegPolyAll))
res(gridSV) <- 2.5
proj4string(gridSV) <- proj4string(VegPolyAll)
gridpolygonSV <- rasterToPolygons(gridSV)
dry.gridSV <- raster::intersect(VegPolyAll, gridpolygonSV)
SV <- gCentroid(dry.gridSV, byid=T)
SVDF <- as.data.frame(SV)
coords <- SV@coords
VegPoints <- SpatialPointsDataFrame(coords = coords, data = SVDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(VegPoints, "./Analysis_Scripts/Chapter3/Points/Veg_Points", layer= "chull", driver = "ESRI Shapefile")

# FOR SUBSTRATE - ROCK - 39 points
#creating the grid to put the polygons in
gridSR <- raster(extent(RockPolyAll))
res(gridSR) <- 2.5
proj4string(gridSR) <- proj4string(RockPolyAll)
gridpolygonSR <- rasterToPolygons(gridSR)
dry.gridSR <- raster::intersect(RockPolyAll, gridpolygonSR)
SR <- gCentroid(dry.gridSR, byid=T)
SRDF <- as.data.frame(SR)
coords <- SR@coords
RockPoints <- SpatialPointsDataFrame(coords = coords, data = SRDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(RockPoints, "./Analysis_Scripts/Chapter3/Points/Rock_Points", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)

# FOR SUBSTRATE - WATER - 33 points
#creating the grid to put the polygons in
gridSW <- raster(extent(WaterPolyAll))
res(gridSW) <- 2.5
proj4string(gridSW) <- proj4string(WaterPolyAll)
gridpolygonSW <- rasterToPolygons(gridSW)
dry.gridSW <- raster::intersect(WaterPolyAll, gridpolygonSW)
SW <- gCentroid(dry.gridSW, byid=T)
SWDF <- as.data.frame(SW)
coords <- SW@coords
WaterPoints <- SpatialPointsDataFrame(coords = coords, data = SWDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(WaterPoints, "./Analysis_Scripts/Chapter3/Points/Water_Points", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)

# FOR MICROHABITAT - ARB - 54 points
#creating the grid to put the polygons in
gridMA <- raster(extent(ArbPolyAll))
res(gridMA) <- 2.5
proj4string(gridMA) <- proj4string(ArbPolyAll)
gridpolygonMA <- rasterToPolygons(gridMA)
dry.gridMA <- raster::intersect(ArbPolyAll, gridpolygonMA)
MA <- gCentroid(dry.gridMA, byid=T)
MADF <- as.data.frame(MA)
coords <- MA@coords
ArbPoints <- SpatialPointsDataFrame(coords = coords, data = MADF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(ArbPoints, "./Analysis_Scripts/Chapter3/Points/Arb_Points", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - TERR - 186 points
#creating the grid to put the polygons in
gridMT <- raster(extent(TerrPolyAll))
res(gridMT) <- 2.5
proj4string(gridMT) <- proj4string(TerrPolyAll)
gridpolygonMT <- rasterToPolygons(gridMT)
dry.gridMT <- raster::intersect(TerrPolyAll, gridpolygonMT)
MT <- gCentroid(dry.gridMT, byid=T)
MTDF <- as.data.frame(MT)
coords <- MT@coords
TerrPoints <- SpatialPointsDataFrame(coords = coords, data = MTDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(TerrPoints, "./Analysis_Scripts/Chapter3/Points/Terr_Points", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - AQUA - 92 points
#creating the grid to put the polygons in
gridMAQ <- raster(extent(AquaPolyAll))
res(gridMAQ) <- 2.5
proj4string(gridMAQ) <- proj4string(AquaPolyAll)
gridpolygonMAQ <- rasterToPolygons(gridMAQ)
dry.gridMAQ <- raster::intersect(AquaPolyAll, gridpolygonMAQ)
MAQ <- gCentroid(dry.gridMAQ, byid=T)
MAQDF <- as.data.frame(MAQ)
coords <- MAQ@coords
AquaPoints <- SpatialPointsDataFrame(coords = coords, data = MAQDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(AquaPoints, "./Analysis_Scripts/Chapter3/Points/Aqua_Points", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - CAVE - 28 points
#creating the grid to put the polygons in
gridMC <- raster(extent(CavePolyAll))
res(gridMC) <- 2.5
proj4string(gridMC) <- proj4string(CavePolyAll)
gridpolygonMC <- rasterToPolygons(gridMC)
dry.gridMC <- raster::intersect(CavePolyAll, gridpolygonMC)
MC <- gCentroid(dry.gridMC, byid=T)
MCDF <- as.data.frame(MC)
coords <- MC@coords
CavePoints <- SpatialPointsDataFrame(coords = coords, data = MCDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(CavePoints, "./Analysis_Scripts/Chapter3/Points/Cave_Points", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - SAX - 18 points
#creating the grid to put the polygons in
gridMS <- raster(extent(SaxPolyAll))
res(gridMS) <- 2.5
proj4string(gridMS) <- proj4string(SaxPolyAll)
gridpolygonMS <- rasterToPolygons(gridMS)
dry.gridMS <- raster::intersect(SaxPolyAll, gridpolygonMS)
MS <- gCentroid(dry.gridMS, byid=T)
MSDF <- as.data.frame(MS)
coords <- MS@coords
SaxPoints <- SpatialPointsDataFrame(coords = coords, data = MSDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(SaxPoints, "./Analysis_Scripts/Chapter3/Points/Sax_Points", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - FOSS - 11 points
#creating the grid to put the polygons in
gridMF <- raster(extent(FossPolyAll))
res(gridMF) <- 2.5
proj4string(gridMF) <- proj4string(FossPolyAll)
gridpolygonMF <- rasterToPolygons(gridMF)
dry.gridMF <- raster::intersect(FossPolyAll, gridpolygonMF)
MF <- gCentroid(dry.gridMF, byid=T)
MFDF <- as.data.frame(MF)
coords <- MF@coords
FossPoints <- SpatialPointsDataFrame(coords = coords, data = MFDF, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(FossPoints, "./Analysis_Scripts/Chapter3/Points/Foss_Points", layer= "chull", driver = "ESRI Shapefile")




