###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###################################################

# for lauren to explore, not the real script used

# USES OUTPUT OF THE MAXENT MODEL
# MAKES A POLYGON OF A CERTAIN SUITABILITY THRESHOLD
# ASKS HOW MUCH OF THE POLYGON OVERLAP IS IN THE SUITABILITY
# BY AREA

# ALSO FUN MAP AT THE END

###############################

# packages
library(phyloclim)

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
      library(geosphere)
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
inter <- raster::intersect(DirtPol, VegPolyAll)
plot(predictors$alt)
plot(inter, add=T)
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
plot(VegPoly)
plot(DirtPoly)

# make into one dirt polygon
DirtDirtPoly <- rgeos::gBuffer(DirtPoly, width = 0, byid=F)
DirtDirtDirt <- aggregate(DirtDirtPoly, dissolve=T)
crs(DirtDirtDirt) <- crs(Vegpol)

# intersect dirt polygon with veg niche model
interDV <- raster::intersect(DirtDirtDirt, Vegpol)
areaPolygon(interDV) / 1e6
# 391,185
# this is dirt spp present where veg can live

VegVeg <- aggregate(VegPoly, dissolve=T)
interVD <- raster::intersect(VegVeg, DirtPol)
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



