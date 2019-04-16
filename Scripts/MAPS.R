 ###################################
#### MAPPING SCRIPT ###############
###################################
 
# THIS IS JUST A SCRIPT TO MAKE MAPS
 # ADDED LEAFLET SCRIPT TO THE BOTTOM
 
library(magrittr); library(maptools); library(rgeos)
library(scales); library(mapproj); library(maps)
library(mapdata)

# AllPolysforanalysis is IUCN + LM Polys 
# UniqueNLMPolys is IUCN only 

## load polygons, can be different
Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

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

### prune vegetation, dirt, rock, water
Dirt <- Class1[Class1$ArbVegStrict == "D", ]
Dirt$Species #190
DirtPoly <- Polygons[match(Dirt$Species,Polygons$binomial), ]
DirtPoly <- aggregate(DirtPoly, by = "binomial")
#plot(DirtPoly)
DirtPoly$binomial

Veg <- Class1[Class1$ArbVegStrict == "V", ]
Veg$Species #71
VegPoly <- Polygons[match(Veg$Species,Polygons$binomial), ]
#plot(VegPoly)
VegPoly$binomial
#plot(VegPoly[3,])
map.axes()

Rock <- Class1[Class1$ArbVegStrict == "R", ]
Rock$Species #18
RockPoly <- Polygons[match(Rock$Species,Polygons$binomial), ]
RockPoly$binomial

Water <- Class1[Class1$SemiAqStrict == "W", ]
Water$Species #21
WaterPoly <- Polygons[match(Water$Species,Polygons$binomial), ]
WaterPoly$binomial

### prune arboreal polys - A ##
Arb <- Class1[Class1$Strict == "A", ]
Arb$Species #60 is around the right number
ArbPoly <- Polygons[match(Arb$Species,Polygons$binomial), ]  
plot(ArbPoly)
ArbPoly$binomial #56

### prune terrestrial polys - T ###
Terr <- Class1[Class1$Strict == "T", ]
Terr$Species #207
TerrPoly <- Polygons[match(Terr$Species, Polygons$binomial), ] 
TerrPoly$binomial #188
plot(TerrPoly)

### prune aquatic polys - W##
Aqua <- Class1[Class1$Strict == "W", ]
Aqua$Species # 32
AquaPoly <- Polygons[match(Aqua$Species, Polygons$binomial), ]
AquaPoly$binomial #28
plot(AquaPoly)

### prune aquatic polys - C##
Cave <- Class1[Class1$Strict == "C", ]
Cave$Species # 11
CavePoly <- Polygons[match(Cave$Species, Polygons$binomial), ]
CavePoly$binomial #11
plot(CavePoly)

### prune aquatic polys - S##
Sax <- Class1[Class1$Strict == "S", ]
Sax$Species # 7
SaxPoly <- Polygons[match(Sax$Species, Polygons$binomial), ]
SaxPoly$binomial #7
plot(SaxPoly)

### prune aquatic polys - F##
Foss <- Class1[Class1$Strict == "F", ]
Foss$Species  # 3
FossPoly <- Polygons[match(Foss$Species, Polygons$binomial), ]
FossPoly$binomial #3
plot(FossPoly)

#PROJECTED MAPS
map(database = "world", fill = TRUE, col = 8, border = F, plot = TRUE, add=F,
    xlim = c(-155,-40), ylim = c(-10,60))
map(database = "world", fill = TRUE, col = 8, border = F, plot = TRUE, add=F,
    xlim = c(-105,-80), ylim = c(10,24))
plot(ArbPoly, add=T)
plot(ArbPoints, add=T)
plot(dry.gridMA,add=T)
plot(AllPointss, add=T)
map.axes()

# plot(ArbPoly, add=TRUE, xlim=c(-200,-40),ylim=c(-10,90), 
#      col=alpha("darkgreen", 0.9), border=F)
# plot(AquaPoly, add=TRUE, xlim=c(-200,-40),ylim=c(-10,90), 
#      col= alpha("blue", 0.9), border=FALSE)
# plot(TerrPoly, add = T, xlim =c(-200,-40), ylim=c(-10,90),
#      col = alpha("red", 0.5), border =F)
# plot(CavePoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("black", 0.5), border = F)
# plot(SaxPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("orange", 0.5), border = F)
# plot(FossPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("purple", 0.5), border = F)
plot(DirtPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("black", 0.5), border = F)
plot(VegPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("green", 0.5), border = F)
plot(RockPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("orange", 0.5), border = F)
plot(WaterPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("blue", 0.5), border = F)
##
plot(ArbPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("black", 0.9), border = F)
plot(ArbC5, add = T, xlim = c(-200, -40), ylim = c(-10,90),border = F)

plot(TerrPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("black", 0.6), border = F)
plot(TerrC5, add = T, xlim = c(-200, -40), ylim = c(-10,90),border = F)



### europe
map(database = "world", fill = TRUE, col = 8, border = F, plot = TRUE, add=F,
    xlim = c(5,20), ylim = c(35,48))
map.axes()

# plot(ArbPoly, add=TRUE, xlim=c(-200,-40),ylim=c(-10,90), 
#      col=alpha("darkgreen", 0.9), border=F)
# plot(AquaPoly, add=TRUE, xlim=c(-200,-40),ylim=c(-10,90), 
#      col= alpha("blue", 0.9), border=FALSE)
# plot(TerrPoly, add = T, xlim =c(-200,-40), ylim=c(-10,90),
#      col = alpha("red", 0.5), border =F)
# plot(CavePoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("black", 0.5), border = F)
# plot(SaxPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("orange", 0.5), border = F)
# plot(FossPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("purple", 0.5), border = F)
plot(DirtPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("black", 0.5), border = F)
plot(VegPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("green", 0.5), border = F)
plot(RockPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("orange", 0.5), border = F)
plot(WaterPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("blue", 0.5), border = F)

### korea
map(database = "world", fill = TRUE, col = 8, border = F, plot = TRUE, add=F,
    xlim = c(123,132), ylim = c(34,42))
map.axes()

# plot(ArbPoly, add=TRUE, xlim=c(-200,-40),ylim=c(-10,90), 
#      col=alpha("darkgreen", 0.9), border=F)
# plot(AquaPoly, add=TRUE, xlim=c(-200,-40),ylim=c(-10,90), 
#      col= alpha("blue", 0.9), border=FALSE)
# plot(TerrPoly, add = T, xlim =c(-200,-40), ylim=c(-10,90),
#      col = alpha("red", 0.5), border =F)
# plot(CavePoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("black", 0.5), border = F)
# plot(SaxPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("orange", 0.5), border = F)
# plot(FossPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
#      col = alpha("purple", 0.5), border = F)
plot(DirtPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("black", 0.5), border = F)
plot(VegPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("green", 0.5), border = F)
plot(RockPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("orange", 0.5), border = F)
plot(WaterPoly, add = T, xlim = c(-200, -40), ylim = c(-10,90),
     col = alpha("blue", 0.5), border = F)

# mapping specific species
i <- which(Polygons$binomial == "Bolitoglossa nympha")
# or
i <- 71

par(mar = c(0,0,0,0))
map(database = "world", fill = TRUE, col = 8, border = F, plot = TRUE, add=F,
    xlim = c(-155,-30), ylim = c(-10,60))
map.axes()
plot(VegPoly[i, ], add = T, xlim =c(-200,-40), ylim=c(-10,90),
     col = alpha("red", 0.5), border =F)

#########################################
32:37
Polygons[32:37,]
AnPoly <- Polygons[32:37,]
AnPoly$binomial
# Leaflet experiment - lol cool stuff

# try to add elevation / temp maps to see cool stuff

library(leaflet)
library(tidyr)

leaflet:::leaflet() %>% leaflet:::addTiles() %>% leaflet:::setView(lng = c(-10,60), lat = c(-155,-30), zoom = 7)

leaflet:::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet:::addPolygons(data = DirtPoly,
                        color = "black",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group = "Dirt",
                        popup = DirtPoly$binomial
                        ) %>%
  leaflet:::addPolygons(data = VegPoly,
                        color = "green",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group = "Veg",
                        popup = VegPoly$binomial
                        ) #%>%
  leaflet:::addPolygons(data = RockPoly,
                        color = "orange",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group = "Rock",
                        popup = RockPoly$binomial
                        ) %>%
  leaflet:::addPolygons(data = WaterPoly,
                        color = "blue",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group = "Water" ,
                        popup = WaterPoly$binomial
  )


  leaflet:::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet:::addPolygons(data = AnPoly,
                          color = c("brown", "green", "black", "black", "green", "green"),
                          weight = .1,
                          smoothFactor = 0.1,
                          opacity = 0.5,
                          fillOpacity = 0.6,
                          group = "An",
                          popup = AnPoly$binomial
    )

#########################################
## THIS IS JUST A FUN INTERACTIVE MAP ###
#########################################
  
  library(leaflet)
  library(tidyr)
  
  leaflet:::leaflet() %>% leaflet:::addTiles() %>% leaflet:::setView(lng = c(-10,60), lat = c(-155,-30), zoom = 7)
  
  leaflet:::leaflet() %>%
    leaflet::addTiles() %>%
    # #leaflet:::addPolygons(data = DirtPoly,
    #                       color = "black", 
    #                       weight = 1, 
    #                       smoothFactor = 0.1,
    #                       opacity = 1.0, 
    #                       fillOpacity = 0.5,
    #                       group = "Dirt",
    #                       popup = DirtPoly$binomial
    #                       ) %>%
    leaflet:::addPolygons(data = VegPoly,
                          color = "green",
                          weight = 1,
                          smoothFactor = 0.1,
                          opacity = 1.0,
                          fillOpacity = 0.5,
                          group = "Veg",
                          popup = VegPoly$binomial
    ) #%>%
  # #leaflet:::addPolygons(data = RockPoly,
  #                       color = "orange",
  #                       weight = 1,
  #                       smoothFactor = 0.1,
  #                       opacity = 1.0,
  #                       fillOpacity = 0.5,
  #                       group = "Rock",
  #                       popup = RockPoly$binomial
  #                       ) %>%
  # #leaflet:::addPolygons(data = WaterPoly,
  #                       color = "blue",
  #                       weight = 1,
  #                       smoothFactor = 0.1,
  #                       opacity = 1.0,
  #                       fillOpacity = 0.5,
  #                       group = "Water" ,
  #                       popup = WaterPoly$binomial
  #) 
  



