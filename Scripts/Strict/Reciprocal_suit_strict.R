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
ArbModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_strict.grd")
TerrModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_strict.grd")
AquaModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_strict.grd")
CaveModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_strict.grd")
FossModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_strict.grd")
SaxModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_strict.grd")

# load the polygons
ArbPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/ArbPoly_strict/chull.shp")
TerrPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/TerrPoly_strict/chull.shp")
AquaPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/AquaPoly_strict/chull.shp")
CavePolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/CavePoly_strict/chull.shp")
FossPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/FossPoly_strict/chull.shp")
SaxPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/SaxPoly_strict/chull.shp")

# load the points
ArbPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Arb_Points_strict/chull.shp")
TerrPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Terr_Points_strict/chull.shp")
AquaPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Aqua_Points_strict/chull.shp")
CavePointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Cave_Points_strict/chull.shp")
FossPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Foss_Points_strict/chull.shp")
SaxPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Sax_Points_strict/chull.shp")


# check what they look like
plot(ArbR)

# give them a threshold suitability score
ArbModSSS <- ArbModS > 0.5
TerrModSSS <- TerrModS > 0.5
AquaModSSS <- AquaModS > 0.5
CaveModSSS <- CaveModS > 0.5
FossModSSS <- FossModS > 0.5
SaxModSSS <- SaxModS > 0.5

# turn it into a polygon
ArbModpolS <- rasterToPolygons(ArbModSSS,function(x) x == 1,dissolve=T)
TerrModpolS <- rasterToPolygons(TerrModSSS,function(x) x == 1,dissolve=T)
AquaModpolS <- rasterToPolygons(AquaModSSS,function(x) x == 1,dissolve=T)
CaveModpolS <- rasterToPolygons(CaveModSSS,function(x) x == 1,dissolve=T)
FossModpolS <- rasterToPolygons(FossModSSS,function(x) x == 1,dissolve=T)
SaxModpolS <- rasterToPolygons(SaxModSSS,function(x) x == 1,dissolve=T)

#
maybe <- extract(ArbSS, ArbPoly)
summary(maybe[[1]])

# get the area of the niche polygon
areaPolygon(ArbModpolS) / 1e6
# 1652808
areaPolygon(TerrModpolS) / 1e6
# 5894229
areaPolygon(AquaModpolS) / 1e6
# 3459333
areaPolygon(CaveModpolS) / 1e6
# 1390241
areaPolygon(FossModpolS) / 1e6
# 8027235
areaPolygon(SaxModpolS) / 1e6
# 2506735


# niche overlap 
# arb and terr
dismo::nicheOverlap(ArbModSSS, TerrModSSS, stat='I', mask=T, checkNegatives = T)
# 0.2400875
dismo::nicheOverlap(ArbModSSS, TerrModSSS, stat='D', mask=T, checkNegatives = T)
# 0.1204878

# arb and water
dismo::nicheOverlap(ArbModSSS, AquaModSSS, stat='I', mask=T, checkNegatives = T)
# 0.0436299
dismo::nicheOverlap(ArbModSSS, AquaModSSS, stat='D', mask=T, checkNegatives = T)
# 0.0280168

# arb and cave
dismo::nicheOverlap(ArbModSSS, CaveModSSS, stat='I', mask=T, checkNegatives = T)
# 0.03980642
dismo::nicheOverlap(ArbModSSS, CaveModSSS, stat='D', mask=T, checkNegatives = T)
# 0.03779222

# arb and foss
dismo::nicheOverlap(ArbModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 0.3746644
dismo::nicheOverlap(ArbModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 0.1747053

# arb and sax
dismo::nicheOverlap(ArbModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.1903572
dismo::nicheOverlap(ArbModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.149132

# terr and water
dismo::nicheOverlap(TerrModSSS, AquaModSSS, stat='I', mask=T, checkNegatives = T)
# 0.6406319
dismo::nicheOverlap(TerrModSSS, AquaModSSS, stat='D', mask=T, checkNegatives = T)
# 0.5006657

# terr and cave
dismo::nicheOverlap(TerrModSSS, CaveModSSS, stat='I', mask=T, checkNegatives = T)
# 0.475957
dismo::nicheOverlap(TerrModSSS, CaveModSSS, stat='D', mask=T, checkNegatives = T)
# 0.2267726

# terr and foos
dismo::nicheOverlap(TerrModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 0.2395885
dismo::nicheOverlap(TerrModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 0.2226158

# terr and sax
dismo::nicheOverlap(TerrModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.617572
dismo::nicheOverlap(TerrModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.3956032

# water and cave
dismo::nicheOverlap(AquaModSSS, CaveModSSS, stat='I', mask=T, checkNegatives = T)
# 0.4557815
dismo::nicheOverlap(AquaModSSS, CaveModSSS, stat='D', mask=T, checkNegatives = T)
# 0.2778691

# water and foss
dismo::nicheOverlap(AquaModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 0.1148412
dismo::nicheOverlap(AquaModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 0.08339249

# water and sax
dismo::nicheOverlap(AquaModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.5280969
dismo::nicheOverlap(AquaModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.4328591

# cave and foss
dismo::nicheOverlap(CaveModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 0.07523933
dismo::nicheOverlap(CaveModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 0.0333087

# cave and sax
dismo::nicheOverlap(CaveModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.6844893
dismo::nicheOverlap(CaveModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.5091167

# foss and sax
dismo::nicheOverlap(FossModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.171238
dismo::nicheOverlap(FossModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.1019207

###
# arb poly intersect with arb niche
# ArbPolyS area
areaPolygon(ArbPolyS) / 1e6 # 593188.5
# ArbModpolS area
areaPolygon(ArbModpolS) / 1e6 # 1652808
# intersection between them
AAS <- raster::intersect(ArbPolyS,ArbModpolS)
areaPolygon(AAS) / 1e6 # 346230.2
# percent of the polygon explained by the niche
(346230.2/593188.5)*100 # 58.37

###
# terr poly intersect with terr niche
# TerrPolyS area
areaPolygon(TerrPolyS) / 1e6 # 4547682
# TerrModpolS area
areaPolygon(TerrModpolS) / 1e6 # 5894229
# intersection between them
TTS <- raster::intersect(TerrPolyS,TerrModpolS)
areaPolygon(TTS) / 1e6 # 3740433 
# percent of the polygon explained by the niche
(3740433/4547682)*100 # 82.25

###
# water poly intersect with water niche
# AquaPolyS area
areaPolygon(AquaPolyS) / 1e6 # 2829823 
# AquaModpolS area
areaPolygon(AquaModpolS) / 1e6 # 3459333 
# intersection between them
WWS <- raster::intersect(AquaPolyS,AquaModpolS)
areaPolygon(WWS) / 1e6 # 2338716
# percent of the polygon explained by the niche
(2338716/2829823)*100 # 82.64531

###
# cave poly intersect with cave niche
# CavePolyS area
areaPolygon(CavePolyS) / 1e6 # 492224
# CaveModpolS area
areaPolygon(CaveModpolS) / 1e6 # 1390241
# intersection between them
CCS <- raster::intersect(CavePolyS,CaveModpolS)
areaPolygon(CCS) / 1e6 # 450375.3
# percent of the polygon explained by the niche
(450375.3/492224)*100 # 91.49804

###
# foss poly intersect with foss niche
# FossPolyS area
areaPolygon(FossPolyS) / 1e6 # 110435
# FossModpolS area
areaPolygon(FossModpolS) / 1e6 # 8027235
# intersection between them
FFS <- raster::intersect(FossPolyS,FossModpolS)
areaPolygon(FFS) / 1e6 # 108256 
# percent of the polygon explained by the niche
(108256/110435)*100 # 98.02

###
# sax poly intersect with sax niche
# SaxPolyS area
areaPolygon(SaxPolyS) / 1e6 # 180106.9
# SaxModpolS area
areaPolygon(SaxModpolS) / 1e6 # 2506735
# intersection between them
SSS <- raster::intersect(SaxPolyS,SaxModpolS)
areaPolygon(SSS) / 1e6 # 177146.2  
# percent of the polygon explained by the niche
(177146.2/180106.9)*100 # 98.36

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1652808
areaPolygon(TerrModpolS) / 1e6 # 5894229
#inter bet Arb and Terr at 0.5 cutoff
interATSN <- raster::intersect(ArbModpolS, TerrModpolS)
areaPolygon(interATSN) / 1e6 # 728048.5
# percent of each niche that overlaps with the other by area
# inter/arb niche
(728048.5/1652808)*100 # 44.05
# inter/terr niche
(728048.5/5894229)*100 # 12.35

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1652808
areaPolygon(AquaModpolS) / 1e6 # 3459333
#inter bet Arb and water at 0.5 cutoff
interAWSN <- raster::intersect(ArbModpolS, AquaModpolS)
areaPolygon(interAWSN) / 1e6 # 111058.8
# percent of each niche that overlaps with the other by area
# inter/arb niche
(111058.8/1652808)*100 # 6.7
# inter/water niche
(111058.8/3459333)*100 # 3.21

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1652808
areaPolygon(CaveModpolS) / 1e6 # 1390241
#inter bet Arb and cave at 0.5 cutoff
interACSN <- raster::intersect(ArbModpolS, CaveModpolS)
areaPolygon(interACSN) / 1e6 # 56593.81
# percent of each niche that overlaps with the other by area
# inter/arb niche
(56593.81/1652808)*100 # 3.424
# inter/cave niche
(56593.81/1390241)*100 # 4.07

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1652808
areaPolygon(FossModpolS) / 1e6 # 8027235
#inter bet Arb and foss at 0.5 cutoff
interAFSN <- raster::intersect(ArbModpolS, FossModpolS)
areaPolygon(interAFSN) / 1e6 # 1291780
# percent of each niche that overlaps with the other by area
# inter/arb niche
(1291780/1652808)*100 # 78.15669
# inter/foss niche
(1291780/8027235)*100 # 16.09

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1652808
areaPolygon(SaxModpolS) / 1e6 # 2506735
#inter bet Arb and sax at 0.5 cutoff
interASSN <- raster::intersect(ArbModpolS, SaxModpolS)
areaPolygon(interASSN) / 1e6 # 363601
# percent of each niche that overlaps with the other by area
# inter/arb niche
(363601/1652808)*100 # 21.99
# inter/sax niche
(363601/2506735)*100 # 14.50

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5894229
areaPolygon(AquaModpolS) / 1e6 # 3459333
#inter bet Terr and Water at 0.5 cutoff
interTWSN <- raster::intersect(TerrModpolS, AquaModpolS)
areaPolygon(interTWSN) / 1e6 # 2902812
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2902812/5894229)*100 # 49.24
# inter/water niche
(2902812/3459333)*100 # 83.91

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5894229
areaPolygon(CaveModpolS) / 1e6 # 1390241
#inter bet Terr and Cave at 0.5 cutoff
interTCSN <- raster::intersect(TerrModpolS, CaveModpolS)
areaPolygon(interTCSN) / 1e6 # 1388815
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1388815/5894229)*100 # 23.56
# inter/cave niche
(1388815/1390241)*100 # 99.89

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5894229
areaPolygon(FossModpolS) / 1e6 # 8027235
#inter bet Terr and Foss at 0.5 cutoff
interTFSN <- raster::intersect(TerrModpolS, FossModpolS)
areaPolygon(interTFSN) / 1e6 # 1450720
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1450720/5894229)*100 # 24.61
# inter/foss niche
(1450720/8027235)*100 # 18.07

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5894229
areaPolygon(SaxModpolS) / 1e6 # 2506735
#inter bet Terr and Sax at 0.5 cutoff
interTSSN <- raster::intersect(TerrModpolS, SaxModpolS)
areaPolygon(interTSSN) / 1e6 # 2406366
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2406366/5894229)*100 # 40.82
# inter/sax niche
(2406366/2506735)*100 # 95.99

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolS) / 1e6 # 3459333
areaPolygon(CaveModpolS) / 1e6 # 1390241
#inter bet Water and Cave at 0.5 cutoff
interWCSN <- raster::intersect(AquaModpolS, CaveModpolS)
areaPolygon(interWCSN) / 1e6 # 1040115
# percent of each niche that overlaps with the other by area
# inter/water niche
(1040115/3459333)*100 # 30.06
# inter/cave niche
(1040115/1390241)*100 # 74.82

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolS) / 1e6 # 3459333
areaPolygon(FossModpolS) / 1e6 # 8027235
#inter bet Water and Foss at 0.5 cutoff
interWFSN <- raster::intersect(AquaModpolS, FossModpolS)
areaPolygon(interWFSN) / 1e6 # 572186.4
# percent of each niche that overlaps with the other by area
# inter/water niche
(572186.4/3459333)*100 # 16.54
# inter/foss niche
(572186.4/8027235)*100 # 7.12

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolS) / 1e6 # 3459333
areaPolygon(SaxModpolS) / 1e6 # 2506735
#inter bet Water and Sax at 0.5 cutoff
interWSSN <- raster::intersect(AquaModpolS, SaxModpolS)
areaPolygon(interWSSN) / 1e6 # 1636002
# percent of each niche that overlaps with the other by area
# inter/water niche
(1636002/3459333)*100 # 47.29
# inter/Sax niche
(1636002/2506735)*100 # 65.26

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolS) / 1e6 # 1390241
areaPolygon(FossModpolS) / 1e6 # 8027235
#inter bet cave and foss at 0.5 cutoff
interCFSN <- raster::intersect(CaveModpolS, FossModpolS)
areaPolygon(interCFSN) / 1e6 # 240783.9
# percent of each niche that overlaps with the other by area
# inter/cave niche
(240783.9/1390241)*100 # 17.32
# inter/foss niche
(240783.9/8027235)*100 # 2.99

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolS) / 1e6 # 1390241
areaPolygon(SaxModpolS) / 1e6 # 2506735
#inter bet cave and sax at 0.5 cutoff
interCSSN <- raster::intersect(CaveModpolS, SaxModpolS)
areaPolygon(interCSSN) / 1e6 # 1282485
# percent of each niche that overlaps with the other by area
# inter/cave niche
(1282485/1390241)*100 # 92.25
# inter/sax niche
(1282485/2506735)*100 # 51.16

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolS) / 1e6 # 8027235
areaPolygon(SaxModpolS) / 1e6 # 2506735
#inter bet foss and sax at 0.5 cutoff
interFSSN <- raster::intersect(FossModpolS, SaxModpolS)
areaPolygon(interFSSN) / 1e6 # 702563.4
# percent of each niche that overlaps with the other by area
# inter/foss niche
(702563.4/8027235)*100 # 8.75
# inter/sax niche
(702563.4/2506735)*100 # 28.02


#######################################
#### distribution inter with niche ###
#######################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5894229
interADTS <- raster::intersect(ArbPolyS, TerrModpolS)
areaPolygon(interADTS) / 1e6 # 227228.3
(227228.3/5894229)*100 # 3.855
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolS) / 1e6 # 1390241
interADCS <- raster::intersect(ArbPolyS, CaveModpolS)
areaPolygon(interADCS) / 1e6 # 20004.74
(20004.74/1390241)*100 # 1.43894
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 2506735
interADSS <- raster::intersect(ArbPolyS, SaxModpolS)
areaPolygon(interADSS) / 1e6 # 113164.4
(113164.4/2506735)*100 # 4.514
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolS) / 1e6 # 3459333
interADWS <- raster::intersect(ArbPolyS, AquaModpolS)
# this is zero
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 8027235
interADFS <- raster::intersect(ArbPolyS, FossModpolS)
areaPolygon(interADFS) / 1e6 # 421011.7
(421011.7/8027235)*100 # 5.244
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1652808
interTDAS <- raster::intersect(TerrPolyS, ArbModpolS)
areaPolygon(interTDAS) / 1e6 # 600311.9
(600311.9/1652808)*100 # 36.32
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolS) / 1e6 # 1390241
interTDCS <- raster::intersect(TerrPolyS, CaveModpolS)
areaPolygon(interTDCS) / 1e6 # 1242523
(1242523/1390241)*100 # 89.37
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 2506735
interTDSS <- raster::intersect(TerrPolyS, SaxModpolS)
areaPolygon(interTDSS) / 1e6 # 2015086
(2015086/2506735)*100 # 80.38
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolS) / 1e6 # 3459333
interTDWS <- raster::intersect(TerrPolyS, AquaModpolS)
areaPolygon(interTDWS) / 1e6 # 2526147
(2526147/3459333)*100 # 73.02
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 8027235
interTDFS <- raster::intersect(TerrPolyS, FossModpolS)
areaPolygon(interTDFS) / 1e6 # 1234392
(1234392/8027235)*100 # 15.37
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1652808
interWDAS <- raster::intersect(AquaPolyS, ArbModpolS)
areaPolygon(interWDAS) / 1e6 # 109613.7
(109613.7/1652808)*100 # 6.63
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolS) / 1e6 # 1390241
interWDCS <- raster::intersect(AquaPolyS, CaveModpolS)
areaPolygon(interWDCS) / 1e6 # 1085880
(1085880/1390241)*100 # 78.10
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 2506735
interWDSS <- raster::intersect(AquaPolyS, SaxModpolS)
areaPolygon(interWDSS) / 1e6 # 1709486
(1709486/2506735)*100 # 68.19
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5894229
interWDTS <- raster::intersect(AquaPolyS, TerrModpolS)
areaPolygon(interWDTS) / 1e6 # 2590994
(2590994/5894229)*100 # 43.95
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 8027235
interWDFS <- raster::intersect(AquaPolyS, FossModpolS)
areaPolygon(interWDFS) / 1e6 # 493619.5
(493619.5/8027235)*100 # 6.14
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1652808
interCDAS <- raster::intersect(CavePolyS, ArbModpolS)
areaPolygon(interCDAS) / 1e6 # 508.8025
(508.8025/1652808)*100 # 0.03
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolS) / 1e6 # 3459333
interCDWS <- raster::intersect(CavePolyS, AquaModpolS)
areaPolygon(interCDWS) / 1e6 # 317372.5
(317372.5/3459333)*100 # 9.17
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 2506735
interCDSS <- raster::intersect(CavePolyS, SaxModpolS)
areaPolygon(interCDSS) / 1e6 # 440251.5
(440251.5/2506735)*100 # 17.56
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5894229
interCDTS <- raster::intersect(CavePolyS, TerrModpolS)
areaPolygon(interCDTS) / 1e6 # 459341.1
(459341.1/5894229)*100 # 7.79
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolS) / 1e6 # 8027235
interCDFS <- raster::intersect(CavePolyS, FossModpolS)
areaPolygon(interCDFS) / 1e6 # 59022.81
(59022.81/8027235)*100 # 0.74
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1652808
interFDAS <- raster::intersect(FossPolyS, ArbModpolS)
areaPolygon(interFDAS) / 1e6 # 81773.19
(81773.19/1652808)*100 # 4.95
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolS) / 1e6 # 3459333
interFDWS <- raster::intersect(FossPolyS, AquaModpolS)
# this is zero
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 2506735
interFDSS <- raster::intersect(FossPolyS, SaxModpolS)
# this is zero
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5894229
interFDTS <- raster::intersect(FossPolyS, TerrModpolS)
areaPolygon(interFDTS) / 1e6 # 15357.55
(15357.55/5894229)*100 # 0.26
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolS) / 1e6 # 1390241
interFDCS <- raster::intersect(FossPolyS, CaveModpolS)
# this is zero
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1652808
interSDAS <- raster::intersect(SaxPolyS, ArbModpolS)
areaPolygon(interSDAS) / 1e6 # 3825.708
(3825.708/1652808)*100 # 0.23
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolS) / 1e6 # 3459333
interSDWS <- raster::intersect(SaxPolyS, AquaModpolS)
areaPolygon(interSDWS) / 1e6 # 91127.75
(91127.75/3459333)*100 # 2.63
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 8027235
interSDFS <- raster::intersect(SaxPolyS, FossModpolS)
areaPolygon(interSDFS) / 1e6 # 48537.69
(48537.69/8027235)*100 # 0.60
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5894229
interSDTS <- raster::intersect(SaxPolyS, TerrModpolS)
areaPolygon(interSDTS) / 1e6 # 178575.8
(178575.8/5894229)*100 # 3.02
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolS) / 1e6 # 1390241
interSDCS <- raster::intersect(SaxPolyS, CaveModpolS)
areaPolygon(interSDCS) / 1e6 # 154855.8
(154855.8/1390241)*100 # 11.13
# this is sax spp present where cave can live

##################
# niche equivalency
##################

# climate data
ClimateData <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogCGTiff.gri')
# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors.crop <- crop(x = ClimateData, y = geographic.extent)
predictors <- predictors.crop

# get the points from the Maxent_strict script and make sure its the *New versions

# takes a shit ton of time and crashes computer :(
dismo::nicheEquivalency(sp1=ArbNew, sp2=TerrNew, predictors = predictors,
                        n=2, model=maxent, verbose=T)

# also note there is another niche.equivalency in ENM with breadth but i cant load it 
# also in phyloclim that requires one file of occ points and not defined two





