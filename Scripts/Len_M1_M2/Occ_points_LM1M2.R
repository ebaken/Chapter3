###################################################
## MAKING OCC POINTS FOR MICROHABITAT using Lenient,M1,M2 ###
###################################################

# MAKING POINTS FROM microhabitat POLYGONS TO USE IN MAXENT ANALYSIS with Lenient,M1,M2 classification

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


##########################################################################################


# prune the polygons micro habitat LENIENT
##### microhabitat #########
#
### prune arboreal polys - A ##
Arb <- Class1[Class1$Lenient == "A", ]
Arb$Species #72
ArbPoly <- Polygons[match(Arb$Species,Polygons$binomial), ]  
ArbPolyAll <- aggregate(ArbPoly, dissolve=T)
ArbPolyAll.SP <- SpatialPolygonsDataFrame(ArbPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(ArbPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/ArbPoly_lenient", layer= "chull", driver = "ESRI Shapefile")

### prune terrestrial polys - T ###
Terr <- Class1[Class1$Lenient == "T", ]
Terr$Species #134
TerrPolygon <- Polygons[match(Terr$Species, Polygons$binomial), ] 
TerrPolybuffer <- rgeos::gBuffer(TerrPolygon, width = 0, byid=F)
TerrPolyAll <- aggregate(TerrPolybuffer, dissolve=T)
TerrPolyAll.SP <- SpatialPolygonsDataFrame(TerrPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(TerrPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/TerrPoly_lenient", layer= "chull", driver = "ESRI Shapefile")

### prune aquatic polys - W##
Aqua <- Class1[Class1$Lenient == "W", ]
Aqua$Species # 37
AquaPoly <- Polygons[match(Aqua$Species, Polygons$binomial), ]
AquaPolybuffer <- rgeos::gBuffer(AquaPoly, width = 0, byid=F)
AquaPolyAll <- aggregate(AquaPolybuffer, dissolve=T)
AquaPolyAll.SP <- SpatialPolygonsDataFrame(AquaPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(AquaPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/AquaPoly_lenient", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)

### prune cave polys - C##
Cave <- Class1[Class1$Lenient == "C", ]
Cave$Species # 13
CavePoly <- Polygons[match(Cave$Species, Polygons$binomial), ]
CavePolyAll <- aggregate(CavePoly, dissolve=T)
CavePolyAll.SP <- SpatialPolygonsDataFrame(CavePolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(CavePolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/CavePoly_lenient", layer= "chull", driver = "ESRI Shapefile")

### prune saxicolous polys - S##
Sax <- Class1[Class1$Lenient == "S", ]
Sax$Species # 24
SaxPoly <- Polygons[match(Sax$Species, Polygons$binomial), ]
SaxPolybuffer <- rgeos::gBuffer(SaxPoly, width = 0, byid=F)
SaxPolyAll <- aggregate(SaxPolybuffer, dissolve=T)
SaxPolyAll.SP <- SpatialPolygonsDataFrame(SaxPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(SaxPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/SaxPoly_lenient", layer= "chull", driver = "ESRI Shapefile",overwrite_layer = T)

### prune fossorial polys - F##
Foss <- Class1[Class1$Lenient == "F", ]
Foss$Species  # 30
FossPoly <- Polygons[match(Foss$Species, Polygons$binomial), ]
FossPolyAll <- aggregate(FossPoly, dissolve=T)
FossPolyAll.SP <- SpatialPolygonsDataFrame(FossPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(FossPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/FossPoly_lenient", layer= "chull", driver = "ESRI Shapefile")

#
##############

# make points and point files with all of the PolyAll files

# FOR MICROHABITAT - ARB -
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
writeOGR(ArbPoints, "./Analysis_Scripts/Chapter3/Points/Arb_Points_lenient", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - TERR 
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
writeOGR(TerrPoints, "./Analysis_Scripts/Chapter3/Points/Terr_Points_lenient", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - AQUA 
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
writeOGR(AquaPoints, "./Analysis_Scripts/Chapter3/Points/Aqua_Points_lenient", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - CAVE 
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
writeOGR(CavePoints, "./Analysis_Scripts/Chapter3/Points/Cave_Points_lenient", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - SAX -
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
writeOGR(SaxPoints, "./Analysis_Scripts/Chapter3/Points/Sax_Points_lenient", layer= "chull", driver = "ESRI Shapefile")

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
writeOGR(FossPoints, "./Analysis_Scripts/Chapter3/Points/Foss_Points_lenient", layer= "chull", driver = "ESRI Shapefile")



##########################################################################################

# prune the polygons micro habitat M1
##### microhabitat #########
#
### prune arboreal polys - A ##
Arb <- Class1[Class1$McE1 == "A", ]
Arb$Species #62
ArbPoly <- Polygons[match(Arb$Species,Polygons$binomial), ]  
ArbPolyAll <- aggregate(ArbPoly, dissolve=T)
ArbPolyAll.SP <- SpatialPolygonsDataFrame(ArbPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(ArbPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/ArbPoly_M1", layer= "chull", driver = "ESRI Shapefile")

### prune terrestrial polys - T ###
Terr <- Class1[Class1$McE1 == "T", ]
Terr$Species #195
TerrPolygon <- Polygons[match(Terr$Species, Polygons$binomial), ] 
TerrPolybuffer <- rgeos::gBuffer(TerrPolygon, width = 0, byid=F)
TerrPolyAll <- aggregate(TerrPolybuffer, dissolve=T)
TerrPolyAll.SP <- SpatialPolygonsDataFrame(TerrPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(TerrPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/TerrPoly_M1", layer= "chull", driver = "ESRI Shapefile")

### prune aquatic polys - W##
Aqua <- Class1[Class1$McE1 == "W", ]
Aqua$Species # 32
AquaPoly <- Polygons[match(Aqua$Species, Polygons$binomial), ]
AquaPolyAll <- aggregate(AquaPoly, dissolve=T)
AquaPolyAll.SP <- SpatialPolygonsDataFrame(AquaPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(AquaPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/AquaPoly_M1", layer= "chull", driver = "ESRI Shapefile")

### prune cave polys - C##
Cave <- Class1[Class1$McE1 == "C", ]
Cave$Species # 11
CavePoly <- Polygons[match(Cave$Species, Polygons$binomial), ]
CavePolyAll <- aggregate(CavePoly, dissolve=T)
CavePolyAll.SP <- SpatialPolygonsDataFrame(CavePolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(CavePolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/CavePoly_M1", layer= "chull", driver = "ESRI Shapefile")

### prune saxicolous polys - S##
Sax <- Class1[Class1$McE1 == "S", ]
Sax$Species # 7
SaxPoly <- Polygons[match(Sax$Species, Polygons$binomial), ]
SaxPolyAll <- aggregate(SaxPoly, dissolve=T)
SaxPolyAll.SP <- SpatialPolygonsDataFrame(SaxPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(SaxPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/SaxPoly_M1", layer= "chull", driver = "ESRI Shapefile")

### prune fossorial polys - F##
Foss <- Class1[Class1$McE1 == "F", ]
Foss$Species  # 3
FossPoly <- Polygons[match(Foss$Species, Polygons$binomial), ]
FossPolyAll <- aggregate(FossPoly, dissolve=T)
FossPolyAll.SP <- SpatialPolygonsDataFrame(FossPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(FossPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/FossPoly_M1", layer= "chull", driver = "ESRI Shapefile")

#
##############

# make points and point files with all of the PolyAll files

# FOR MICROHABITAT - ARB - 
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
writeOGR(ArbPoints, "./Analysis_Scripts/Chapter3/Points/Arb_Points_M1", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - TERR - 
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
writeOGR(TerrPoints, "./Analysis_Scripts/Chapter3/Points/Terr_Points_M1", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - AQUA -
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
writeOGR(AquaPoints, "./Analysis_Scripts/Chapter3/Points/Aqua_Points_M1", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - CAVE
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
writeOGR(CavePoints, "./Analysis_Scripts/Chapter3/Points/Cave_Points_M1", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - SAX -
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
writeOGR(SaxPoints, "./Analysis_Scripts/Chapter3/Points/Sax_Points_M1", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - FOSS -
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
writeOGR(FossPoints, "./Analysis_Scripts/Chapter3/Points/Foss_Points_M1", layer= "chull", driver = "ESRI Shapefile")


##########################################################################################



# prune the polygons micro habitat M2
##### microhabitat #########
#
### prune arboreal polys - A ##
Arb <- Class1[Class1$McE2 == "A", ]
Arb$Species #147
ArbPoly <- Polygons[match(Arb$Species,Polygons$binomial), ]  
ArbPolyAll <- aggregate(ArbPoly, dissolve=T)
ArbPolyAll.SP <- SpatialPolygonsDataFrame(ArbPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(ArbPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/ArbPoly_M2", layer= "chull", driver = "ESRI Shapefile")

### prune terrestrial polys - T ###
Terr <- Class1[Class1$McE2 == "T", ]
Terr$Species #88
TerrPolygon <- Polygons[match(Terr$Species, Polygons$binomial), ] 
TerrPolybuffer <- rgeos::gBuffer(TerrPolygon, width = 0, byid=F)
TerrPolyAll <- aggregate(TerrPolybuffer, dissolve=T)
TerrPolyAll.SP <- SpatialPolygonsDataFrame(TerrPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(TerrPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/TerrPoly_M2", layer= "chull", driver = "ESRI Shapefile")

### prune aquatic polys - W##
Aqua <- Class1[Class1$McE2 == "W", ]
Aqua$Species # 29
AquaPoly <- Polygons[match(Aqua$Species, Polygons$binomial), ]
AquaPolyAll <- aggregate(AquaPoly, dissolve=T)
AquaPolyAll.SP <- SpatialPolygonsDataFrame(AquaPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(AquaPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/AquaPoly_M2", layer= "chull", driver = "ESRI Shapefile")

### prune cave polys - C##
Cave <- Class1[Class1$McE2 == "C", ]
Cave$Species # 11
CavePoly <- Polygons[match(Cave$Species, Polygons$binomial), ]
CavePolyAll <- aggregate(CavePoly, dissolve=T)
CavePolyAll.SP <- SpatialPolygonsDataFrame(CavePolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(CavePolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/CavePoly_M2", layer= "chull", driver = "ESRI Shapefile")

### prune saxicolous polys - S##
Sax <- Class1[Class1$McE2 == "S", ]
Sax$Species # 15
SaxPoly <- Polygons[match(Sax$Species, Polygons$binomial), ]
SaxPolybuffer <- rgeos::gBuffer(SaxPoly, width = 0, byid=F)
SaxPolyAll <- aggregate(SaxPolybuffer, dissolve=T)
SaxPolyAll.SP <- SpatialPolygonsDataFrame(SaxPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(SaxPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/SaxPoly_M2", layer= "chull", driver = "ESRI Shapefile",overwrite_layer = T)

### prune fossorial polys - F##
Foss <- Class1[Class1$McE2 == "F", ]
Foss$Species  # 20
FossPoly <- Polygons[match(Foss$Species, Polygons$binomial), ]
FossPolyAll <- aggregate(FossPoly, dissolve=T)
FossPolyAll.SP <- SpatialPolygonsDataFrame(FossPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(FossPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/FossPoly_M2", layer= "chull", driver = "ESRI Shapefile")

#
##############

# make points and point files with all of the PolyAll files

# FOR MICROHABITAT - ARB -
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
writeOGR(ArbPoints, "./Analysis_Scripts/Chapter3/Points/Arb_Points_M2", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - TERR -
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
writeOGR(TerrPoints, "./Analysis_Scripts/Chapter3/Points/Terr_Points_M2", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - AQUA -
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
writeOGR(AquaPoints, "./Analysis_Scripts/Chapter3/Points/Aqua_Points_M2", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - CAVE -
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
writeOGR(CavePoints, "./Analysis_Scripts/Chapter3/Points/Cave_Points_M2", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - SAX -
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
writeOGR(SaxPoints, "./Analysis_Scripts/Chapter3/Points/Sax_Points_M2", layer= "chull", driver = "ESRI Shapefile")

# FOR MICROHABITAT - FOSS 
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
writeOGR(FossPoints, "./Analysis_Scripts/Chapter3/Points/Foss_Points_M2", layer= "chull", driver = "ESRI Shapefile")



