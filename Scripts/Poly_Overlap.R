############################################
#### Code to find % overlap in polys #######
#### for Veg and Dirt polys          #######
############################################

# using polygons only
# distribution overlaps

# Load Libs
library(spatstat); library(rgeos); library(geosphere)
library(maps); library(magrittr); library(maptools)
library(scales); library(mapproj); library(mapdata)
library(raster)

# NOTE FOR LAUREN
# hidden functions in function
# function:::
# package:::function

# Load polygon file
Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

# Check poly labels
Polygons$binomial <- as.character(Polygons$binomial)
Polygons$binomial[which(Polygons$binomial == "Eurycea longicauda melanopleura")] <- "Eurycea longicauda" # combine polys of subspecies together

### Load clasifications
Class <- read.csv("./Data/Pruned/MicrohabitatsPruned.csv")
Class$Species <- as.character(Class$Species)
Class$Species[which(Class$Species == "Pseudoeurycea lineolus")] <- "Pseudoeurycea lineola" #match the species names
Class1 <- Class[match(Polygons1$binomial, Class$Species), ]
anyNA(Class1$Species) # want FALSE
Polygons1 <- raster::aggregate(Polygons, by = "binomial")
length(unique(Polygons1$binomial))
length(Polygons1$binomial)
sort(table(Polygons1$binomial))

### prune dirt polygons 
Dirt <- Class1[Class1$ArbVegStrict == "D", ]
Dirt$Species #190
DirtPoly <- Polygons1[match(Dirt$Species,Polygons1$binomial), ]
DirtPoly$binomial <- as.character(DirtPoly$binomial)
DirtPoly$binomial
sort(table(DirtPoly$binomial))
DirtPoly <- raster::aggregate(DirtPoly, by = "binomial")
  # we have to aggregate because we changed Eurycea longicauda earlier

# check plot and names
plot(DirtPoly)
DirtPoly$binomial

### prune veg polygons
Veg <- Class1[Class1$ArbVegStrict == "V", ]
Veg$Species #71
VegPoly <- Polygons[match(Veg$Species,Polygons$binomial), ]

# check plot and names
plot(VegPoly)
VegPoly$binomial

### prune rock polygons
Rock <- Class1[Class1$ArbVegStrict == "R", ]
Rock$Species
RockPoly <- Polygons1[match(Rock$Species,Polygons1$binomial), ]

### prune water polygons
Water <- Class1[Class1$ArbVegStrict == "W", ]
Water$Species
WaterPoly <- Polygons1[match(Water$Species,Polygons1$binomial), ]

###
# making single file for area of dirt
###

# create common ID to combine all into
DirtPoly$home <- "Dirt"
DirtPoly$home <- as.character(DirtPoly$home)

# check projection and assign if NULL
crs(DirtPoly) 
projection(DirtPoly) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

# aggregate Dirt sallys by common ID
DirtPoly1 <- raster::aggregate(DirtPoly, by = 'home', dissolve = F)

# check plot
plot(DirtPoly1)

# add width 0 buffer because rasters suck and won't work if I dont define boundaries
DirtPoly1 <- gBuffer(DirtPoly1, byid=TRUE, width=0)

# Dissolve inner boundaries
DirtPoly2 <- aggregate(DirtPoly1, dissolve = T)
plot(DirtPoly2)

###
# making single file for area of veg
###

# create common ID to aggregate with
VegPoly$home <- "veg"
VegPoly$home

# check projection and assign if NULL
crs(VegPoly)
projection(VegPoly) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

# combine all polys of veg
VegPoly1 <- raster::aggregate(VegPoly, by = "home")

# check projection
plot(VegPoly1) #good

# add width 0 buffer to defind boundaries
VegPoly1 <- gBuffer(VegPoly1, byid = T, width = 0)

# dissolve inner boundaries
VegPoly2 <- aggregate(VegPoly1, dissolve = T)

###
# making single file for area of rock
###

# create common ID to aggregate with
RockPoly$home <- "rock"
RockPoly$home

# check projection and assign if NULL
crs(RockPoly)
#projection(VegPoly) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

# combine all polys of veg
RockPoly1 <- raster::aggregate(RockPoly, by = "home")

# check projection
plot(RockPoly1) #good

# add width 0 buffer to defind boundaries
RockPoly1 <- gBuffer(RockPoly1, byid = T, width = 0)

# dissolve inner boundaries
RockPoly2 <- aggregate(RockPoly1, dissolve = T)

###
# making single file for area of water
###

# create common ID to aggregate with
WaterPoly$home <- "water"
WaterPoly$home

# check projection and assign if NULL
crs(WaterPoly)
#projection(VegPoly) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

# combine all polys of veg
WaterPoly1 <- raster::aggregate(WaterPoly, by = "home")

# check projection
plot(WaterPoly1) #good

# add width 0 buffer to defind boundaries
WaterPoly1 <- gBuffer(WaterPoly1, byid = T, width = 0)

# dissolve inner boundaries
WaterPoly2 <- aggregate(WaterPoly1, dissolve = T)

# check
plot(WaterPoly2)

###
# making single file for area of all salamanders
###
Polygons1$binomial
sort(table(Polygons1$binomial))
length(Polygons1$binomial) #311
length(unique(Polygons1$binomial))

# create common ID to aggregate with
Polygons1$home <- "all"
Polygons1$home <- as.character(Polygons1$home)

# check projection and assign if NULL
crs(Polygons1)
projection(Polygons1) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
crs(Polygons1)

# combine all polys of veg
length(unique(getSpPPolygonsIDSlots(Polygons1)))
sort(table(getSpPPolygonsIDSlots(Polygons1)))

# combine all by common name
Polygons2 <- raster::aggregate(Polygons1, by = "home", dissolve = F)

# check projection
plot(Polygons2) #good

# add width 0 buffer to defind boundaries
Polygons2 <- gBuffer(Polygons2, byid = T, width = 0)

# dissolve inner boundaries
Polygons3 <- aggregate(Polygons2, dissolve = T)
plot(Polygons3)


###
# intersect the two polygons to find overlap
###

# intersect and check
# this inter can change based on inputs
inter <- raster::intersect(DirtPoly2, WaterPoly2)
plot(inter)

## area of intersection
areaPolygon(inter) / 1e6

#inter dirt - water
# 2564698

#inter rock - water
# 464001.2

# inter rock - dirt
# 554859.5

#inter veg - rock
# 1042.569

# inter veg - dirt
# 200636.6 in square meters

# get full area for all sallys
areaPolygon(Polygons3) / 1e6
# 5,178,265

ArbPolyy <- aggregate(ArbPoly, dissolve=T)
plot(ArbPolyy)
areaPolygon(ArbPolyy) / 1e6
# 593188.5
areaPolygon(ArbPolyAll) / 1e6

crs(TerrPoly)
TerrPoly <- gBuffer(TerrPoly, byid=T, width=0)
TerrPolyy <- aggregate(TerrPoly, dissolve=T)
plot(TerrPoly)
areaPolygon(TerrPolyAll) / 1e6
# 4547682

#Arb of all
(593188.5/5178265)*100
# Terr of all
(4547682/5178265)*100

interATTT <- raster::intersect(ArbPolyAll, TerrPolyAll)
areaPolygon(interATTT) / 1e6
(174936/5178265)*100

# get full area of dirt
areaPolygon(DirtPoly2) / 1e6
# 4,549,033 in square meters

#get full area of veg
areaPolygon(VegPoly2) / 1e6
# 671,261.5 in square meters

#get full area of rock
areaPolygon(RockPoly2) / 1e6
#592,799

#get full area of water
areaPolygon(WaterPoly2) / 1e6
# 2,829,823 

# get percent overlap in dirt
(200636.6 / 4549033) * 100
# 4.44 percent overlap in Dirt

# get percent overlap in veg
(200636.6 / 671261.5) * 100
#29.89 percent overlap in Veg

# get percent of area described by Veg
(671261.5 / 5178265) * 100
# 12.96

#get percent of area described by dirt
(4549033 / 5178265) * 100
# 87.85

#get percent of area described by rock
(592799 / 5178265) * 100
# 11.44

#get percent of area described by water
(2829823 / 5178265) * 100
#54.65

# percent inter veg-dirt described
(200636.6 / 5178265) * 100
# 3.87

#percent inter veg - rock described
(1042.569 / 5178265) * 100
# 0.02

#percent inter rock - dirt described
(554859.5 / 5178265) * 100
# 10.72

#percent inter rock - water described
(464001.2 / 5178265) * 100
# 8.96

#percent inter dirt - water described
(2564698 / 5178265) * 100
# 49.53


