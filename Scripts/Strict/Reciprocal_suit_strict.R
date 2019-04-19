###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###  for the microhabitat STRICT    ##############
###################################################

# USES OUTPUT OF THE MAXENT MODEL
# MAKES A POLYGON OF A CERTAIN SUITABILITY THRESHOLD
# ASKS HOW MUCH OF THE POLYGON OVERLAP IS IN THE SUITABILITY
# BY AREA

###############################

# packages
library(phyloclim)
library(geosphere)
library(raster)
library(rgdal)
library(rgeos)

# load the maxent predictions
ArbModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapA5.grd")
TerrModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapT5.grd")
AquaModS
CaveModS
FossModS
SaxModS

# load the polygons
ArbPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/ArbPolyAll/chull.shp")
TerrPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/TerrPolyAll/chull.shp")
AquaPolyS
CavePolyS
FossPolyS
SaxPolyS

# load the points
ArbPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points/chull.shp")
TerrPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points/chull.shp")

# check what they look like
plot(ArbR)

# give them a threshold suitability score
ArbModSSS <- ArbModS > 0.5

# turn it into a polygon
ArbModpolS <- rasterToPolygons(ArbModSSS,function(x) x == 1,dissolve=T)

#
maybe <- extract(ArbSS, ArbPoly)
summary(maybe[[1]])

# get the area
areaPolygon(Arbpol) / 1e6


# give them a threshold suitability score
TerrSS <- Terr5 > 0.5

# turn it into a polygon
TerrPol <- rasterToPolygons(TerrSS,function(x) x == 1,dissolve=T)
TerrCPol <- rasterToPolygons(TerrCSS,function(x) x == 1,dissolve=T)
#
?dismo::nicheOverlap
dismo::nicheOverlap(ArbC5, TerrC5, stat='I', mask=T, checkNegatives = T)
# 0.6919516
dismo::nicheOverlap(ArbC5, TerrC5, stat='D', mask=T, checkNegatives = T)
# 0.415737

dismo::nicheOverlap(ArbCSS, TerrCSS, stat='I', mask=T, checkNegatives = T)
# 0.2881969
dismo::nicheOverlap(ArbCSS, TerrCSS, stat='D', mask=T, checkNegatives = T)
# 0.1470364

# get the area
areaPolygon(TerrPol) / 1e6
areaPolygon(TerrCPol) / 1e6

# intersection area 
inter <- raster::intersect(TerrPol, Arbpol)
interC <- raster::intersect(TerrCPol,ArbpolC)
#plot(predictors$alt)
#plot(inter, add=T)
areaPolygon(inter) / 1e6
#inter bet Arb and Terr at 0.5 cutoff
areaPolygon(interC) / 1e6

# inter/terr
(918784.4/6614631)*100
# 13.89 with 0.5 suitability

# inter/arb
(918784.4/1957924)*100
# 46.93 with 0.5 suitability

# intersect terr polygon with arb niche model
interTA <- raster::intersect(TerrPoly, Arbpol)
interTAC <- raster::intersect(TerrPoly, ArbpolC)
areaPolygon(interTA) / 1e6
# 716507.5  with 0.5 suitability
areaPolygon(interTAC) / 1e6
# this is dirt spp present where veg can live

# bottom left plot
plot(ArbCSS)
plot(TerrPoly, add=T)

# intersect arb polygon with terr niche model
interAT <- raster::intersect(ArbPoly, TerrPol)
interATC <- raster::intersect(ArbPoly, TerrCPol)
areaPolygon(interAT) / 1e6
# 247933.1  with 0.5 suitability
areaPolygon(interATC) / 1e6
# this is veg spp present where dirt can live

# TA divided by terr niche
(716507.5/6614631)*100
# 10.83216 with 0.5 suitability

# TAC divided by terr niche
(389641.9/4034541)*100
# 9.657651 with 0.7 suitability

# AT divided by arb niche
(247933.1/1957924)*100
# 12.66306 with 0.5 suitability

# ATC divided by arb niche
(186611.7/875402.1)*100
# 21.31726 with 0.7 

# arb polygons divided by arb niche
areaPolygon(ArbPoly) / 1e6
# 593,188.5 with 0.5 suitability

#divide area of arb polygon by arb niche
(539188.5/1957924)*100
# 27.54 with 0.5 suitability

#divide area of arb polygon by arb niche cloud
(539188.5/875402.1)*100
# 61.59 with 0.7 suitability

#overlap of arbpolygon with arbniche
APAN <- raster::intersect(ArbPoly, Arbpol)
APANC <- raster::intersect(ArbPoly, ArbpolC)
areaPolygon(APANC) / 1e6
(526672.6/593188.5)*100


areaPolygon(APAN) / 1e6

(493484.4/593188.5)*100


# terr polygons divided by terr niche
areaPolygon(TerrPoly) / 1e6
# 4547682 with 0.5 suitability
(4547682/6614631)*100
# 68.75 with 0.5 suitability

TPTN <- raster::intersect(TerrPoly, TerrPol)
TPTNC <- raster::intersect(TerrPoly, TerrCPol)
areaPolygon(TPTNC) / 1e6

(4214758/4547682)*100


areaPolygon(TPTN) / 1e6
# 3934204 with 0.5 suitability
(3934204/4547682)*100
# 86.51 with 0.5 suitability
(3934204/6614631)*100
# 59.48 with 0.5 suitability




