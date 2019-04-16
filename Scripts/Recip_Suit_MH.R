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
library(rgeos)

# load the maxent predictions
Arb5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapA5.grd")
Terr5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapT5.grd")

ArbC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapAC5.grd")
TerrC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapTC5.grd")

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
ArbCSS <- ArbC5 > 0.5

# turn it into a polygon
Arbpol <- rasterToPolygons(ArbSS,function(x) x == 1,dissolve=T)
ArbpolC <- rasterToPolygons(ArbCSS,function(x) x == 1,dissolve=T)

#
maybe <- extract(ArbSS, ArbPoly)
summary(maybe[[1]])

# get the area
areaPolygon(Arbpol) / 1e6
# 1,275,883 for .6
# 1,957,924 for .5
# 3,079,622 for .4
# 5,420,456 for .3
areaPolygon(ArbpolC) / 1e6
# 875,402.1 for .7
# 1,223,588 for .6
# 1,815,267 for .5
# 2,787,041 for .4
# 4,755,532 for .3
# 8,373,377 for .2

# give them a threshold suitability score
TerrSS <- Terr5 > 0.5
TerrCSS <- TerrC5 > 0.5

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
# 5,394,934 for .6
# 6,614,631 for .5
areaPolygon(TerrCPol) / 1e6
# 4,034,541 for .7
# 5,058,505 for .6
# 6,294,555 for .5
# 7,677,826 for .4
# 9,402,052 for .3

# intersection area 
inter <- raster::intersect(TerrPol, Arbpol)
interC <- raster::intersect(TerrCPol,ArbpolC)
#plot(predictors$alt)
#plot(inter, add=T)
areaPolygon(inter) / 1e6
#inter bet Arb and Terr at 0.5 cutoff
# 918784.4
areaPolygon(interC) / 1e6
# 402,043 for .7
# 597,876.6 for .6
# 944910.8 for .5
# 1396346 for .4
# 2060638 for .3

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
# 389641.9 with 0.7
# 466977.2 with 0.6
# 636105.2 with 0.5
# 849359.2 with 0.4
# 1057335 with 0.3
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
# 186611.7 with 0.7
# 218896.3 with 0.6
# 263894.4 with 0.5
# 323924.7 with 0.4
# 390611.3 with 0.3
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
# 275956.4 with 0.7 suitabiltiy
# 311615.9 with 0.6
# 351771.2 with 0.5
# 402962.9 with 0.4
# 465561.3 with 0.3
# 526672.6 with 0.2
(526672.6/593188.5)*100
# 46.52 with 0.7 suitabilt
# 52.53 with 0.6 suit
# 59.30176 with 0.5 suit
# 67.93 with 0.4 suit
# 78.48 with 0.3 suit
# 88.78 with 0.2 suit

areaPolygon(APAN) / 1e6
# 368095.8 with 0.5 suitability
# 421808.2 with 0.4 suitability
# 493484.4 with 0.3 suitabilty
(493484.4/593188.5)*100
# 62.05 with 0.5 suitability
# 71.108 with 0.4 suitability
# 83.19184 with 0.3 suitability

# terr polygons divided by terr niche
areaPolygon(TerrPoly) / 1e6
# 4547682 with 0.5 suitability
(4547682/6614631)*100
# 68.75 with 0.5 suitability

TPTN <- raster::intersect(TerrPoly, TerrPol)
TPTNC <- raster::intersect(TerrPoly, TerrCPol)
areaPolygon(TPTNC) / 1e6
# 3023032 with 0.7 suitability
# 3473731 with 0.6 
# 3837957 with 0.5
# 4045513 with 0.4 
# 4214758 with 0.3
(4214758/4547682)*100
# 66.47 with 0.7 suitabiltiy
# 76.38 with 0.6 suitability
# 84.39 with 0.5
# 88.95 with 0.4
# 92.679

areaPolygon(TPTN) / 1e6
# 3934204 with 0.5 suitability
(3934204/4547682)*100
# 86.51 with 0.5 suitability
(3934204/6614631)*100
# 59.48 with 0.5 suitability




