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

# load the maxent predictions
VegR <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/VegR_prediction.grd")
DirtR <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/DirtR_prediction.grd")

# check what they look like
plot(VegR)
plot(DirtR)

# give them a threshold suitability score
VegRR <- VegR > 0.5

# turn it into a polygon
Vegpol <- rasterToPolygons(VegRR,function(x) x == 1,dissolve=T)
      cropRveg <- crop(Vegpol, VegPolyAll)
      maybe <- extract(VegR, VegPolyAll)
      min(maybe[[1]])
      summary(maybe[[1]])
      plot(cropRveg)

# get the area
areaPolygon(Vegpol) / 1e6
# 1,098,593 for .6, 1478542 for .5

# give them a threshold suitability score
DirtRR <- DirtR > 0.5

# turn it into a polygon
DirtPol <- rasterToPolygons(DirtRR,function(x) x == 1,dissolve=T)

# get the area
areaPolygon(DirtPol) / 1e6
# 5,520,928 for .6, 6720540 for .5

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
areaPolygon(interDV) / 1e6
# 391,185
# this is dirt spp present where veg can live

# intersect veg polygon with dirt niche model
interVD <- raster::intersect(VegPolyAll, DirtPol)
areaPolygon(interVD) / 1e6
# 267,698.1
# this is veg spp present where dirt can live

# DV divided by Dirt niche
(391185/5520928)*100
# 7.085494

# VD divided by Veg niche
(267698.1/1098593)*100
# 24.36736

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



