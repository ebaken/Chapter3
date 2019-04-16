###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###   for the substrate (Veg, Dirt)  ##############
###################################################

# USES OUTPUT OF THE MAXENT MODEL
# MAKES A POLYGON OF A CERTAIN SUITABILITY THRESHOLD
# ASKS HOW MUCH OF THE POLYGON OVERLAP IS IN THE SUITABILITY
# BY AREA in square meters or divide and get km squared

###############################

# packages
library(phyloclim)
library(geosphere)
library(raster)
library(rgdal)

# load the maxent predictions
VegR <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/VegR_prediction.grd")
DirtR <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/DirtR_prediction.grd")

VegRC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapVC5.grd")
DirtRC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapDC5.grd")

# check what they look like
plot(VegR)
plot(DirtR)

# give them a threshold suitability score
VegRR <- VegR > 0.5
VegRCR <- VegRC5 > 0.7

# turn it into a polygon
VegpolC <- rasterToPolygons(VegRCR,function(x) x == 1,dissolve=T)
      cropRveg <- crop(Vegpol, VegPolyAll)
      maybe <- extract(VegR, VegPolyAll)
      min(maybe[[1]])
      summary(maybe[[1]])
      plot(cropRveg)

# get the area
areaPolygon(Vegpol) / 1e6
# 1,098,593 for .6, 1478542 for .5
areaPolygon(VegpolC) / 1e6
# 1,219,537 for .7

# give them a threshold suitability score
DirtRR <- DirtR > 0.5
DirtRCR <- DirtRC5 > 0.7

# turn it into a polygon
DirtPolC <- rasterToPolygons(DirtRCR,function(x) x == 1,dissolve=T)

# get the area
areaPolygon(DirtPol) / 1e6
# 5,520,928 for .6, 6720540 for .5
areaPolygon(DirtPolC) / 1e6
# 4,054,280 for .7

# intersection area 
inter <- raster::intersect(DirtPol, Vegpol)
#plot(predictors$alt)
#plot(inter, add=T)
areaPolygon(inter) / 1e6
#inter bet Veg and Dirt
#575572.5

# inter/dirt
(575572.5/5520928)*100
# 10.42529

# inter/veg
(575572.5/1098593)*100
# 52.39179

# load polygons
DirtPolyAll <- readOGR("./Analysis_Scripts/Chapter3/Polygons/DirtPolyAll/chull.shp")
VegPolyAll <- readOGR("./Analysis_Scripts/Chapter3/Polygons/VegPolyAll/chull.shp")

# intersect dirt polygon with veg niche model
interDV <- raster::intersect(DirtPolyAll, Vegpol)
interDVC <- raster::intersect(DirtPolyAll, VegpolC)
areaPolygon(interDV) / 1e6
# 391,185
areaPolygon(interDVC) / 1e6
# 477,738.9 
# this is dirt spp present where veg can live

# intersect veg polygon with dirt niche model
interVD <- raster::intersect(VegPolyAll, DirtPol)
interVDC <- raster::intersect(VegPolyAll, DirtPolC)
areaPolygon(interVD) / 1e6
# 267,698.1
areaPolygon(interVDC) / 1e6
# 221,279.3
# this is veg spp present where dirt can live

# DV divided by Dirt niche
(391185/5520928)*100
# 7.085494

# DVC divided by Dirt niche
(477738.9/4054280)*100
# 11.78

# VD divided by Veg niche
(267698.1/1098593)*100
# 24.36736

#VDC divided by Veg niche
(221279.3/1219537)*100
# 18.14453

# use output from MAXENT, not what we want
#nicheOverlap(DirtR, VegR, stat='I', mask=T,checkNegatives = T)
# D - 0.3760527
# I - 0.6711939
#nicheOverlap(DirtRR, VegRR, stat='I', mask=T, checkNegatives = T)
# D - 0.09827291
# I - 0.2352182

# map to see the overlap between polgyons
# for fun and understanding
leaflet:::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet:::addPolygons(data = DirtDirtDirt,
                        color = "black",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 0.5,
                        fillOpacity = 0.5
  ) %>%
  leaflet:::addPolygons(data = VegVeg,
                        color = "green",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 0.5,
                        fillOpacity = 0.5
  ) %>% 
  leaflet:::addPolygons(data = interDV,
                        color = "red",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5
  ) %>%
  leaflet:::addPolygons(data = interVD,
                        color = "blue",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5
  )



