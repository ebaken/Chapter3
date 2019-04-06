###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###  for the microhabitat (Arb, Terr)##############
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

# load the maxent predictions
Arb5 <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapA5.grd")
Terr5 <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapT5.grd")

# load the polygons
ArbPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/ArbPolyAll/chull.shp")
TerrPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/TerrPolyAll/chull.shp")
  
# load the points
ArbPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points/chull.shp")
TerrPoints <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points/chull.shp")

# check what they look like
plot(ArbR)
plot(TerrR)

# give them a threshold suitability score
ArbSS <- Arb5 > 0.2

# turn it into a polygon
Arbpol <- rasterToPolygons(ArbSS,function(x) x == 1,dissolve=T)

#
maybe <- extract(ArbSS, ArbPoly)
summary(maybe[[1]])

# get the area
areaPolygon(Arbpol) / 1e6
# 1,275,883 for .6
# 1,957,924 for .5
# 3,079,622 for .4
# 5,420,456 for .3
# 

# give them a threshold suitability score
TerrSS <- Terr5 > 0.5

# turn it into a polygon
TerrPol <- rasterToPolygons(TerrSS,function(x) x == 1,dissolve=T)

# get the area
areaPolygon(TerrPol) / 1e6
# 5,394,934 for .6, 6,614,631 for .5

# intersection area 
inter <- raster::intersect(TerrPol, Arbpol)
#plot(predictors$alt)
#plot(inter, add=T)
areaPolygon(inter) / 1e6
#inter bet Arb and Terr at 0.5 cutoff
# 918784.4

# inter/terr
(918784.4/6614631)*100
# 13.89 with 0.5 suitability

# inter/arb
(918784.4/1957924)*100
# 46.93 with 0.5 suitability

# intersect terr polygon with arb niche model
interTA <- raster::intersect(TerrPoly, Arbpol)
areaPolygon(interTA) / 1e6
# 716507.5  with 0.5 suitability
# this is dirt spp present where veg can live

# intersect arb polygon with terr niche model
interAT <- raster::intersect(ArbPoly, TerrPol)
areaPolygon(interAT) / 1e6
# 247933.1  with 0.5 suitability
# this is veg spp present where dirt can live

# TA divided by terr niche
(716507.5/6614631)*100
# 10.83216 with 0.5 suitability

# AT divided by arb niche
(247933.1/1957924)*100
# 12.66306 with 0.5 suitability

# arb polygons divided by arb niche
areaPolygon(ArbPoly) / 1e6
# 593,188.5 with 0.5 suitability
#divide area of arb polygon by arb niche
(539188.5/1957924)*100
# 27.54 with 0.5 suitability
#overlap of arbpolygon with arbniche
APAN <- raster::intersect(ArbPoly, Arbpol)
areaPolygon(APAN) / 1e6
# 368095.8 with 0.5 suitability
# 421808.2 with 0.4 suitability
# 493484.4 with 0.3 suitabilty
(493484.4/593188.5)*100
# 62.05 with 0.5 suitability
# 71.108 with 0.4 suitability
# 83.19184 with 0.3 suitability

(368095.8/1957924)*100
# 18.80 with 0.5 suitability

# terr polygons divided by terr niche
areaPolygon(TerrPoly) / 1e6
# 4547682 with 0.5 suitability
(4547682/6614631)*100
# 68.75 with 0.5 suitability
TPTN <- raster::intersect(TerrPoly, TerrPol)
areaPolygon(TPTN) / 1e6
# 3934204 with 0.5 suitability
(3934204/4547682)*100
# 86.51 with 0.5 suitability
(3934204/6614631)*100
# 59.48 with 0.5 suitability




