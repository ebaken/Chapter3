#################################################
##            Recreate iucn polygons        #####
#################################################
library(alphahull)
library(ggplot2)
library(ConR)
library(data.table)

#########
# TO DO #
#########

# make file of polygons without LM makings for comparison
  #UniqueNLMPolys - done
# make file of polygons with LM makings
  #UniqueLMPolys

###############################################
# species I was not able to make polygons for #
# Bolitoglossa mucuyensis - LM made
# Plethodon savannah
# Bolitoglossa cataguana
# Bolitoglossa chinanteca
# Bolitoglossa kaqchikelorum
# Nototriton picucha

#List of species I have to add polygons to or redo with code
# Batrachoseps regius
    #polygon made from 163 points, this will replace the old one
# Batrachoseps relictus
    #polygon made from 611 points, add to OG
# Bolitoglossa dunni
    #polygon made from 26 points, add to OG
#LM MADE # Bolitoglossa mucuyensis      ###############
    #only one point on vertnet, no new polys
# Bolitoglossa odonnelli
    #polygons made from 36 points, this will be an addition to IUCN polygons to add in honduras
# Bradytriton silus
    #polygons made from 31 points, this will replace the old one
# Desmognathus conanti
    #polygons made from 1,111 points, add to OG
# Desmognathus fuscus
    # polygons made from 6,000 points, this will add to the IUCN polygons
# Eurycea bislineata
    # polygons made from 5,997 points, this will add to the IUCN polygons
# Eurycea longicauda
    # polygons made from 2,578 points, this will add to the IUCN polygons
# Plethodon aureolus
    # polygons made from 762 points, this will add to the IUCN polygons
# LM MADE # Plethodon chlorobryonis   ###############
    # polygons made from 1913 points, this will replace the polygon I made bc it is better
# Plethodon dorsalis
     # polygons made from 1,794 points, this will add to IUCN polygons
# Plethodon jordani
     # polygons made from 1,998 points, this will add to IUCN polygons
# Plethodon richmondi
     # polygons made from 3,043 points, this will add to IUCN polygons
# LM MADE ### Plethodon savannah #############
     # polygon not made, only 2 unique points, costa rica and georgia
# Plethodon wehrlei
     # polygon made from 1,591 points, this will add to the IUCN polygons

########################################
# Batrachoseps regius
Batreg <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Batrachoseps_regius.txt.tsv", quote=""))
# make into the colums needed
Batraregius <-  cbind(Batreg$decimallatitude, Batreg$decimallongitude, Batreg$scientificname)
colnames(Batraregius) <- c("ddlat", "ddlon","tax")
Batraregius <- as.data.frame(Batraregius)
Batraregius[,1] <- as.character(Batraregius[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Batraregius[,1] <- as.numeric(Batraregius[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Batraregius[,2] <- as.character(Batraregius[,2])
Batraregius[,2] <- as.numeric(Batraregius[,2])
Batraregius[,3] <- as.character(Batraregius[,3])
# this is just getting a polygon by connecting the outside dots
EEO.results_BRe <- EOO.computing(Batraregius, file.name = "Batraregius_EOO", export_shp = T)
plot(EEO.results_BRe[[1]][[2]], col="grey")
data(land)
plot(EEO.results_BRe[[1]][[2]], col="grey")
plot(land, add=T)
# This only adds shorelines when the polygon overlaps with it. if nothing happens, the polygon is not touching any water

EOO.results.BR <- EOO.computing(Batraregius, method.range = "alpha.hull", 
                             export_shp = T, write_shp = F,
                             alpha=1, exclude.area = T,  # alpha is analogous to level of confidence in sampling breadth
                             country_map = land) # EOO (extent of occupation outine) IUCN powerpoint in Extras folder
#class(EOO.results.BR$Species1$spatial.polygon) #it is a spatial polygon
Batraregius.SP <- SpatialPolygonsDataFrame(EOO.results.BR$Species1$spatial.polygon, data=data.frame(binomial=1))
writeOGR(Batraregius.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batraregius", driver="ESRI Shapefile", layer = "chull")
Batraregius.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batraregius/chull.shp")
Batraregius.SHP$binomial <- "Batrachoseps regius"
proj4string(Batraregius.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Batraregius.SHP
#plot(EOO.results.BR$Species1$spatial.polygon, col="red")
#plot(land, add=T)

# the alpha value changes the contour of the polygon. closer to 1 is more contoured and higher is less contoured
# right now set the alpha value to 1 for all polygons
# if we find out later that my polys are different and we delineate that to be 
#    because alpha values, we can further assess the alpha value later on

#######################################
# Batrachoseps relictus
Batrel <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Batrarelictus.txt.tsv", quote="")) # not in Erica's folders yet
# make into the colums needed
Batrarel <-  cbind(Batrel$decimallatitude, Batrel$decimallongitude, Batrel$scientificname)
colnames(Batrarel) <- c("ddlat", "ddlon","tax")
Batrarel <- as.data.frame(Batrarel)
Batrarel[,1] <- as.character(Batrarel[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Batrarel[,1] <- as.numeric(Batrarel[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Batrarel[,2] <- as.character(Batrarel[,2])
Batrarel[,2] <- as.numeric(Batrarel[,2])
Batrarel[,3] <- as.character(Batrarel[,3])

EOO.results.BRR <- EOO.computing(Batrarel, method.range = "alpha.hull", 
                             export_shp = T, write_shp = F, alpha=1, exclude.area = T, 
                             country_map = land)

Batrarel.SP <- SpatialPolygonsDataFrame(EOO.results.BRR$Species1$spatial.polygon, data=data.frame(binomial=1))
writeOGR(Batrarel.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batrarel", driver="ESRI Shapefile", layer = "chull")
Batrarel.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batrarel/chull.shp")
Batrarel.SHP$binomial <- "Batrachoseps relictus"
proj4string(Batrarel.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Batrarel.SHP
plot(EOO.results.BRR[[1]][[2]], col="red")
plot(land, add=T)

#######################################
# Bolitoglossa dunni
Bolitodunn <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Bolitoglossa_dunni.txt.tsv", quote=""))
# make into the colums needed
Bolitodunn <-  cbind(Bolitodunn$decimallatitude, Bolitodunn$decimallongitude, Bolitodunn$scientificname)
colnames(Bolitodunn) <- c("ddlat", "ddlon","tax")
Bolitodunn <- as.data.frame(Bolitodunn)
Bolitodunn[,1] <- as.character(Bolitodunn[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Bolitodunn[,1] <- as.numeric(Bolitodunn[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Bolitodunn[,2] <- as.character(Bolitodunn[,2])
Bolitodunn[,2] <- as.numeric(Bolitodunn[,2])
Bolitodunn[,3] <- as.character(Bolitodunn[,3])

EOO.results.BD <- EOO.computing(Bolitodunn, method.range = "alpha.hull", 
                                export_shp = T, write_shp = F, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.BD$Species1$spatial.polygon)
Bolitodunn.SP <- SpatialPolygonsDataFrame(EOO.results.BD$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Bolitodunn.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolitodunn", driver="ESRI Shapefile", layer = "chull")
Bolitodunn.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolitodunn/chull.shp")
Bolitodunn.SHP$binomial <- "Bolitoglossa dunni"
proj4string(Bolitodunn.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Bolitodunn.SHP

plot(EOO.results.BD[[1]][[2]], col="red")
plot(land, add=T)

#######################################
# Bolitoglossa odonnelli
Bolitood <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Bolitoglossa_odonnelli.txt.tsv", quote=""))
# make into the colums needed
Bolitood <-  cbind(Bolitood$decimallatitude, Bolitood$decimallongitude, Bolitood$scientificname)
colnames(Bolitood) <- c("ddlat", "ddlon","tax")
Bolitood <- as.data.frame(Bolitood)
Bolitood[,1] <- as.character(Bolitood[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Bolitood[,1] <- as.numeric(Bolitood[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Bolitood[,2] <- as.character(Bolitood[,2])
Bolitood[,2] <- as.numeric(Bolitood[,2])
Bolitood[,3] <- as.character(Bolitood[,3])

EOO.results.BO <- EOO.computing(Bolitood, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.BO$Species1$spatial.polygon)
Bolitood.SP <- SpatialPolygonsDataFrame(EOO.results.BO$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Bolitood.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolitood", driver="ESRI Shapefile", layer = "chull")
Bolitood.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolitood/chull.shp")
Bolitood.SHP$binomial <- "Bolitoglossa odonnelli"
proj4string(Bolitood.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Bolitood.SHP

plot(EOO.results.BO[[1]][[2]], col="red")
plot(land, add=T)

#######################################
# Bradytriton silus
Bradysil <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Bradytriton_silus.txt.tsv", quote=""))
# make into the colums needed
Bradysil <-  cbind(Bradysil$decimallatitude, Bradysil$decimallongitude, Bradysil$scientificname)
colnames(Bradysil) <- c("ddlat", "ddlon","tax")
Bradysil <- as.data.frame(Bradysil)
Bradysil[,1] <- as.character(Bradysil[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Bradysil[,1] <- as.numeric(Bradysil[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Bradysil[,2] <- as.character(Bradysil[,2])
Bradysil[,2] <- as.numeric(Bradysil[,2])
Bradysil[,3] <- as.character(Bradysil[,3])

EOO.results.BS <- EOO.computing(Bradysil, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.BS$Species1$spatial.polygon)
Bradysil.SP <- SpatialPolygonsDataFrame(EOO.results.BS$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Bradysil.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bradysil", driver="ESRI Shapefile", layer = "chull")
Bradysil.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bradysil/chull.shp")
Bradysil.SHP$binomial <- "Bradytriton silus"
proj4string(Bradysil.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Bradysil.SHP

plot(EOO.results.BS[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Desmognathus conanti
Desmocon <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Desmognathus_conanti.txt.tsv", quote=""))
# make into the colums needed
Desmocon <-  cbind(Desmocon$decimallatitude, Desmocon$decimallongitude, Desmocon$scientificname)
colnames(Desmocon) <- c("ddlat", "ddlon","tax")
Desmocon <- as.data.frame(Desmocon)
Desmocon[,1] <- as.character(Desmocon[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Desmocon[,1] <- as.numeric(Desmocon[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Desmocon[,2] <- as.character(Desmocon[,2])
Desmocon[,2] <- as.numeric(Desmocon[,2])
Desmocon[,3] <- as.character(Desmocon[,3])

EOO.results.DC <- EOO.computing(Desmocon, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.DC$Species1$spatial.polygon)
Desmocon.SP <- SpatialPolygonsDataFrame(EOO.results.DC$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Desmocon.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Desmocon", driver="ESRI Shapefile", layer = "chull")
Desmocon.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Desmocon/chull.shp")
Desmocon.SHP$binomial <- "Desmognathus conanti"
proj4string(Desmocon.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Desmocon.SHP

plot(EOO.results.DC[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Desmognathus fuscus
Desmofus <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons/Polygons_Dec/Desmognathus_fuscus.txt.tsv", quote=""))
# make into the colums needed
Desmofus <-  cbind(Desmofus$decimallatitude, Desmofus$decimallongitude, Desmofus$scientificname)
colnames(Desmofus) <- c("ddlat", "ddlon","tax")
Desmofus <- as.data.frame(Desmofus)
Desmofus[,1] <- as.character(Desmofus[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Desmofus[,1] <- as.numeric(Desmofus[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Desmofus[,2] <- as.character(Desmofus[,2])
Desmofus[,2] <- as.numeric(Desmofus[,2])
Desmofus[,3] <- as.character(Desmofus[,3])

EOO.results.DF <- EOO.computing(Desmofus, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.DF$Species1$spatial.polygon)
Desmofus.SP <- SpatialPolygonsDataFrame(EOO.results.DF$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Desmofus.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Desmofus", driver="ESRI Shapefile", layer = "chull", overwrite_layer = T)
Desmofus.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Desmofus/chull.shp")
Desmofus.SHP$binomial <- "Desmognathus fuscus"
proj4string(Desmofus.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Desmofus.SHP
plot(Desmofus.SHP)
plot(EOO.results.DF[[1]][[2]], col="red")
plot(land, add=T)

#######################################
# Eurycea bislineata
Eurbis <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Eurycea_bislineata.txt.tsv", quote=""))
# make into the colums needed
Eurbis <-  cbind(Eurbis$decimallatitude, Eurbis$decimallongitude, Eurbis$scientificname)
colnames(Eurbis) <- c("ddlat", "ddlon","tax")
Eurbis <- as.data.frame(Eurbis)
Eurbis[,1] <- as.character(Eurbis[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Eurbis[,1] <- as.numeric(Eurbis[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Eurbis[,2] <- as.character(Eurbis[,2])
Eurbis[,2] <- as.numeric(Eurbis[,2])
Eurbis[,3] <- as.character(Eurbis[,3])

EOO.results.EB <- EOO.computing(Eurbis, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.EB$Species1$spatial.polygon)
Eurbis.SP <- SpatialPolygonsDataFrame(EOO.results.EB$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Eurbis.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Eurbis", driver="ESRI Shapefile", layer = "chull")
Eurbis.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Eurbis/chull.shp")
Eurbis.SHP$binomial <- "Eurycea bislineata"
proj4string(Eurbis.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Eurbis.SHP

plot(EOO.results.EB[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Eurycea longicauda
Eurlon <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Eurycea_longicauda.txt.tsv", quote=""))
# make into the colums needed
Eurlon <-  cbind(Eurlon$decimallatitude, Eurlon$decimallongitude, Eurlon$scientificname)
colnames(Eurlon) <- c("ddlat", "ddlon","tax")
Eurlon <- as.data.frame(Eurlon)
Eurlon[,1] <- as.character(Eurlon[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Eurlon[,1] <- as.numeric(Eurlon[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Eurlon[,2] <- as.character(Eurlon[,2])
Eurlon[,2] <- as.numeric(Eurlon[,2])
Eurlon[,3] <- as.character(Eurlon[,3])

EOO.results.EL <- EOO.computing(Eurlon, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.EL$Species1$spatial.polygon)
Eurlon.SP <- SpatialPolygonsDataFrame(EOO.results.EL$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Eurlon.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Eurlon", driver="ESRI Shapefile", layer = "chull")
Eurlon.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Eurlon/chull.shp")
Eurlon.SHP$binomial <- "Eurycea longicauda"
proj4string(Eurlon.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(EOO.results.EL[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon aureolus

Pleaur <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Plethodon_aureolus.txt.tsv", quote=""))
# make into the colums needed
Pleaur <-  cbind(Pleaur$decimallatitude, Pleaur$decimallongitude, Pleaur$scientificname)
colnames(Pleaur) <- c("ddlat", "ddlon","tax")
Pleaur <- as.data.frame(Pleaur)
Pleaur[,1] <- as.character(Pleaur[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Pleaur[,1] <- as.numeric(Pleaur[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Pleaur[,2] <- as.character(Pleaur[,2])
Pleaur[,2] <- as.numeric(Pleaur[,2])
Pleaur[,3] <- as.character(Pleaur[,3])

EOO.results.PA <- EOO.computing(Pleaur, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.PA$Species1$spatial.polygon)
Pleaur.SP <- SpatialPolygonsDataFrame(EOO.results.PA$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Pleaur.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleaur", driver="ESRI Shapefile", layer = "chull")
Pleaur.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleaur/chull.shp")
Pleaur.SHP$binomial <- "Plethodon aureolus"
proj4string(Pleaur.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Pleaur.SHP

plot(EOO.results.PA[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon chlorobryonis

Plechl <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Plethodon_chlorobryonis.txt.tsv", quote=""))
# make into the colums needed
Plechl <-  cbind(Plechl$decimallatitude, Plechl$decimallongitude, Plechl$scientificname)
colnames(Plechl) <- c("ddlat", "ddlon","tax")
Plechl <- as.data.frame(Plechl)
Plechl[,1] <- as.character(Plechl[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Plechl[,1] <- as.numeric(Plechl[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Plechl[,2] <- as.character(Plechl[,2])
Plechl[,2] <- as.numeric(Plechl[,2])
Plechl[,3] <- as.character(Plechl[,3])

EOO.results.PC <- EOO.computing(Plechl, method.range = "alpha.hull", 
                                export_shp = T, alpha
                                =1, exclude.area = T, 
                                country_map = land)
class(EOO.results.PC$Species1$spatial.polygon)
Plechl.SP <- SpatialPolygonsDataFrame(EOO.results.PC$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Plechl.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plechl", driver="ESRI Shapefile", layer = "chull")
Plechl.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plechl/chull.shp")
Plechl.SHP$binomial <- "Plethodon chlorobryonis"
proj4string(Plechl.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

plot(EOO.results.PC[[1]][[2]], col="red")
plot(land, add=T)

#######################################
# Plethodon dorsalis

Pledor <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Plethodon_dorsalis.txt.tsv", quote=""))
# make into the colums needed
Pledor <-  cbind(Pledor$decimallatitude, Pledor$decimallongitude, Pledor$scientificname)
colnames(Pledor) <- c("ddlat", "ddlon","tax")
Pledor <- as.data.frame(Pledor)
Pledor[,1] <- as.character(Pledor[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Pledor[,1] <- as.numeric(Pledor[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Pledor[,2] <- as.character(Pledor[,2])
Pledor[,2] <- as.numeric(Pledor[,2])
Pledor[,3] <- as.character(Pledor[,3])

EOO.results.PD <- EOO.computing(Pledor, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.PD$Species1$spatial.polygon)
Pledor.SP <- SpatialPolygonsDataFrame(EOO.results.PD$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Pledor.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pledor", driver="ESRI Shapefile", layer = "chull")
Pledor.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pledor/chull.shp")
Pledor.SHP$binomial <- "Plethodon dorsalis"
proj4string(Pledor.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Pledor.SHP

plot(EOO.results.PD[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon jordani

Plejor <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Plethodon_jordani.txt.tsv", quote=""))
# make into the colums needed
Plejor <-  cbind(Plejor$decimallatitude, Plejor$decimallongitude, Plejor$scientificname)
colnames(Plejor) <- c("ddlat", "ddlon","tax")
Plejor <- as.data.frame(Plejor)
Plejor[,1] <- as.character(Plejor[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Plejor[,1] <- as.numeric(Plejor[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Plejor[,2] <- as.character(Plejor[,2])
Plejor[,2] <- as.numeric(Plejor[,2])
Plejor[,3] <- as.character(Plejor[,3])

EOO.results.PJ <- EOO.computing(Plejor, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.PJ$Species1$spatial.polygon)
Plejor.SP <- SpatialPolygonsDataFrame(EOO.results.PJ$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Plejor.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plejor", driver="ESRI Shapefile", layer = "chull")
Plejor.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plejor/chull.shp")
Plejor.SHP$binomial <- "Plethodon jordani"
proj4string(Plejor.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Plejor.SHP

plot(EOO.results.PJ[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon richmondi

Pleric <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Plethodon_richmondi.txt.tsv", quote=""))
# make into the colums needed
Pleric <-  cbind(Pleric$decimallatitude, Pleric$decimallongitude, Pleric$scientificname)
colnames(Pleric) <- c("ddlat", "ddlon","tax")
Pleric <- as.data.frame(Pleric)
Pleric[,1] <- as.character(Pleric[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Pleric[,1] <- as.numeric(Pleric[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Pleric[,2] <- as.character(Pleric[,2])
Pleric[,2] <- as.numeric(Pleric[,2])
Pleric[,3] <- as.character(Pleric[,3])

EOO.results.PR <- EOO.computing(Pleric, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.PR$Species1$spatial.polygon)
Pleric.SP <- SpatialPolygonsDataFrame(EOO.results.PR$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Pleric.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleric", driver="ESRI Shapefile", layer = "chull")
Pleric.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleric/chull.shp")
Pleric.SHP$binomial <- "Plethodon richmondi"
proj4string(Pleric.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Pleric.SHP

plot(EOO.results.PR[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon wehrlei

Pleweh <-as.data.frame(fread("./Analysis_Scripts/Chapter3/Polygons_Dec/Plethodon_wehrlei.txt.tsv", quote=""))
# make into the colums needed
Pleweh <-  cbind(Pleweh$decimallatitude, Pleweh$decimallongitude, Pleweh$scientificname)
colnames(Pleweh) <- c("ddlat", "ddlon","tax")
Pleweh <- as.data.frame(Pleweh)
Pleweh[,1] <- as.character(Pleweh[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Pleweh[,1] <- as.numeric(Pleweh[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Pleweh[,2] <- as.character(Pleweh[,2])
Pleweh[,2] <- as.numeric(Pleweh[,2])
Pleweh[,3] <- as.character(Pleweh[,3])

EOO.results.PW <- EOO.computing(Pleweh, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.PW$Species1$spatial.polygon)
Pleweh.SP <- SpatialPolygonsDataFrame(EOO.results.PW$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Pleweh.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleweh", driver="ESRI Shapefile", layer = "chull")
Pleweh.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleweh/chull.shp")
Pleweh.SHP$binomial <- "Plethodon wehrlei"
proj4string(Pleweh.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Pleweh.SHP

plot(EOO.results.PW[[1]][[2]], col="red")
plot(land, add=T)


#########################################
## LM REDO LM POLYGONS OH BOY ##########
########################################

# SPECIES I MADE THAT NEED TO BE REDONE - 18

# Batrachoseps altasierrae
  # polygon redone from 120 points
# Batrachoseps bramei
  # polygon redone from 145 points
# Bolitoglossa cataguana # its a line #
# Bolitoglossa chinanteca # its a line #
# Bolitoglossa kaqchikelorum # its a line #
# Bolitoglossa mucuyensis # only one point no #
# Bolitoglossa nympha
  # polygon redone from 23 points
# Bolitoglossa robinsoni
  # polygon redone from 23 points
# Chiropterotriton miquihuanus
  # polygon redone from 18 points
# Desmognathus organi
  # polygon redone from 188 points
# Desmognathus planiceps
  # polygon redone from 70 points
# Eurycea aquatica
  # polygon redone from 16 points
# Eurycea chamberlaini
  # polygon redone from 60 points
# Eurycea longicauda melanopleura
  # polygon redone from 64 points
# Nototriton picucha # only 2 points #
# Oedipina nica
  # polygon redone from 15 points
# Plethodon chattahoochee
  # polygon redone from 482 points
# Plethodon chlorobryonis
  # polygon redone from 1,913 points
# Plethodon grobmani
  # polygon redone from 1,929 points
# Plethodon mississippi
  # polygon redone from 1,646 points
# Plethodon ocmulgee
  # polygon redone from 257 points
# Plethodon savannah # only two points #
# Plethodon variolatus
  # polygon redone from 1,009 points

#######################################
# Batrachoseps altasierrae

Batalta <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Baltasierrae.xls")) # warnings has to do with xls file, it reads in correctly
A <- Batalta
# make into the colums needed
A <-  cbind(A$decimallatitude, A$decimallongitude, A$scientificname)
colnames(A) <- c("ddlat", "ddlon","tax")
A <- as.data.frame(A)
A[,1] <- as.character(A[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
A[,1] <- as.numeric(A[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
A[,2] <- as.character(A[,2])
A[,2] <- as.numeric(A[,2])
A[,3] <- as.character(A[,3])

EOO.results.A <- EOO.computing(A, method.range = "alpha.hull", 
                                export_shp = T, alpha=1, exclude.area = T, 
                                country_map = land)

class(EOO.results.A$Species1$spatial.polygon)
Batalta.SP <- SpatialPolygonsDataFrame(EOO.results.A$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Batalta.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batalta", driver="ESRI Shapefile", layer = "chull")
Batalta.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batalta/chull.shp")
Batalta.SHP$binomial <- "Batrachoseps altasierrae"
proj4string(Batalta.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Batalta.SHP

plot(EOO.results.A[[1]][[2]], col="red")
plot(land, add=T)

#######################################
# Batrachoseps bramei

Batbram <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Bbramei.xls"))
B <- Batbram
# make into the colums needed
B <-  cbind(B$decimallatitude, B$decimallongitude, B$scientificname)
colnames(B) <- c("ddlat", "ddlon","tax")
B <- as.data.frame(B)
B[,1] <- as.character(B[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
B[,1] <- as.numeric(B[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
B[,2] <- as.character(B[,2])
B[,2] <- as.numeric(B[,2])
B[,3] <- as.character(B[,3])

EOO.results.B <- EOO.computing(B, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.B$Species1$spatial.polygon)
Batbram.SP <- SpatialPolygonsDataFrame(EOO.results.B$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Batbram.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batbram", driver="ESRI Shapefile", layer = "chull")
Batbram.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Batbram/chull.shp")
Batbram.SHP$binomial <- "Batrachoseps bramei"
proj4string(Batbram.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Batbram.SHP

plot(EOO.results.B[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Bolitoglossa nympha

Bolnym <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Bnympha.xls"))
C <- Bolnym
# make into the colums needed
C <-  cbind(C$decimallatitude, C$decimallongitude, C$scientificname)
colnames(C) <- c("ddlat", "ddlon","tax")
C <- as.data.frame(C)
C[,1] <- as.character(C[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
C[,1] <- as.numeric(C[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
C[,2] <- as.character(C[,2])
C[,2] <- as.numeric(C[,2])
C[,3] <- as.character(C[,3])

EOO.results.C <- EOO.computing(C, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.C$Species1$spatial.polygon)
Bolnym.SP <- SpatialPolygonsDataFrame(EOO.results.C$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Bolnym.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolnym", driver="ESRI Shapefile", layer = "chull")
Bolnym.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolnym/chull.shp")
Bolnym.SHP$binomial <- "Bolitoglossa nympha"
proj4string(Bolnym.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Bolnym.SHP

plot(EOO.results.C[[1]][[2]], col="red")
plot(land, add=T)
91-68

#######################################
# Bolitoglossa robinsoni

Bolrob <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Brobinsoni.xls"))
D <- Bolrob
# make into the colums needed
D <-  cbind(D$decimallatitude, D$decimallongitude, D$scientificname)
colnames(D) <- c("ddlat", "ddlon","tax")
D <- as.data.frame(D)
D[,1] <- as.character(D[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
D[,1] <- as.numeric(D[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
D[,2] <- as.character(D[,2])
D[,2] <- as.numeric(D[,2])
D[,3] <- as.character(D[,3])

EOO.results.D <- EOO.computing(D, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.D$Species1$spatial.polygon)
Bolrob.SP <- SpatialPolygonsDataFrame(EOO.results.D$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Bolrob.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolrob", driver="ESRI Shapefile", layer = "chull")
Bolrob.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Bolrob/chull.shp")
Bolrob.SHP$binomial <- "Bolitoglossa robinsoni"
proj4string(Bolrob.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Bolrob.SHP

plot(EOO.results.D[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Chiropterotriton miquihuanus

Chimiq <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Cmiquihuanus.xls"))
E <- Chimiq
# make into the colums needed
E <-  cbind(E$decimallatitude, E$decimallongitude, E$scientificname)
colnames(E) <- c("ddlat", "ddlon","tax")
E <- as.data.frame(E)
E[,1] <- as.character(E[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
E[,1] <- as.numeric(E[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
E[,2] <- as.character(E[,2])
E[,2] <- as.numeric(E[,2])
E[,3] <- as.character(E[,3])

EOO.results.E <- EOO.computing(E, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.E$Species1$spatial.polygon)
Chimiq.SP <- SpatialPolygonsDataFrame(EOO.results.E$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Chimiq.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Chimiq", driver="ESRI Shapefile", layer = "chull")
Chimiq.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Chimiq/chull.shp")
Chimiq.SHP$binomial <- "Chiropterotriton miquihuanus"
proj4string(Chimiq.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Chimiq.SHP

plot(EOO.results.E[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Desmognathus organi

Desor <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Dorgani.xls"))
G <- Desor
# make into the colums needed
G <-  cbind(G$decimallatitude, G$decimallongitude, G$scientificname)
colnames(G) <- c("ddlat", "ddlon","tax")
G <- as.data.frame(G)
G[,1] <- as.character(G[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
G[,1] <- as.numeric(G[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
G[,2] <- as.character(G[,2])
G[,2] <- as.numeric(G[,2])
G[,3] <- as.character(G[,3])

EOO.results.G <- EOO.computing(G, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.G$Species1$spatial.polygon)
Desor.SP <- SpatialPolygonsDataFrame(EOO.results.G$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Desor.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Desor", driver="ESRI Shapefile", layer = "chull")
Desor.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Desor/chull.shp")
Desor.SHP$binomial <- "Desmognathus organi"
proj4string(Desor.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Desor.SHP

plot(EOO.results.G[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Desmognathus planiceps

Despla <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Dplaniceps.xls"))
H <- Despla
# make into the colums needed
H <-  cbind(H$decimallatitude, H$decimallongitude, H$scientificname)
colnames(H) <- c("ddlat", "ddlon","tax")
H <- as.data.frame(H)
H[,1] <- as.character(H[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
H[,1] <- as.numeric(H[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
H[,2] <- as.character(H[,2])
H[,2] <- as.numeric(H[,2])
H[,3] <- as.character(H[,3])

EOO.results.H <- EOO.computing(H, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.H$Species1$spatial.polygon)
Despla.SP <- SpatialPolygonsDataFrame(EOO.results.H$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Despla.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Despla", driver="ESRI Shapefile", layer = "chull")
Despla.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Despla/chull.shp")
Despla.SHP$binomial <- "Desmognathus planiceps"
proj4string(Despla.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Despla.SHP

plot(EOO.results.H[[1]][[2]], col="red")
plot(land, add=T)



#######################################
# Eurycea aquatica

Eaqua <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Eaquatica.xls"))
I <- Eaqua
# make into the colums needed
I <-  cbind(I$decimallatitude, I$decimallongitude, I$scientificname)
colnames(I) <- c("ddlat", "ddlon","tax")
I <- as.data.frame(I)
I[,1] <- as.character(I[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
I[,1] <- as.numeric(I[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
I[,2] <- as.character(I[,2])
I[,2] <- as.numeric(I[,2])
I[,3] <- as.character(I[,3])

EOO.results.I <- EOO.computing(I, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.I$Species1$spatial.polygon)
Eaqua.SP <- SpatialPolygonsDataFrame(EOO.results.I$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Eaqua.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Eaqua", driver="ESRI Shapefile", layer = "chull")
Eaqua.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Eaqua/chull.shp")
Eaqua.SHP$binomial <- "Eurycea aquatica"
proj4string(Eaqua.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Eaqua.SHP

plot(EOO.results.I[[1]][[2]], col="red")
plot(land, add=T)
146-130 # this is needed when the xls file has rows where lat long are missing... this number is the true number of points used to create this polygon


#######################################
# Eurycea chamberlaini

Echam <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Echamberlaini.xls"))
J <- Echam
# make into the colums needed
J <-  cbind(J$decimallatitude, J$decimallongitude, J$scientificname)
colnames(J) <- c("ddlat", "ddlon","tax")
J <- as.data.frame(J)
J[,1] <- as.character(J[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
J[,1] <- as.numeric(J[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
J[,2] <- as.character(J[,2])
J[,2] <- as.numeric(J[,2])
J[,3] <- as.character(J[,3])

EOO.results.J <- EOO.computing(J, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.J$Species1$spatial.polygon)
Echam.SP <- SpatialPolygonsDataFrame(EOO.results.J$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Echam.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Echam", driver="ESRI Shapefile", layer = "chull")
Echam.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Echam/chull.shp")
Echam.SHP$binomial <- "Eurycea chamberlaini"
proj4string(Echam.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Echam.SHP

plot(EOO.results.J[[1]][[2]], col="red")
plot(land, add=T)

#######################################
# Eurycea longicauda melanopleura

Elome <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Elonme.xls"))
K <- Elome
# make into the colums needed
K <-  cbind(K$decimallatitude, K$decimallongitude, K$scientificname)
colnames(K) <- c("ddlat", "ddlon","tax")
K <- as.data.frame(K)
K[,1] <- as.character(K[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
K[,1] <- as.numeric(K[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
K[,2] <- as.character(K[,2])
K[,2] <- as.numeric(K[,2])
K[,3] <- as.character(K[,3])

EOO.results.K <- EOO.computing(K, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.K$Species1$spatial.polygon)
Elome.SP <- SpatialPolygonsDataFrame(EOO.results.K$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Elome.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Elome", driver="ESRI Shapefile", layer = "chull")
Elome.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Elome/chull.shp")
Elome.SHP$binomial <- "Eurycea longicauda melanopleura"
proj4string(Elome.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Elome.SHP

plot(EOO.results.K[[1]][[2]], col="red")
plot(land, add=T)
495+333 # adding rows that were cutoff in printout
828-764



#######################################
# Oedipina nica

Onica <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Onica.xls"))
L <- Onica
# make into the colums needed
L <-  cbind(L$decimallatitude, L$decimallongitude, L$scientificname)
colnames(L) <- c("ddlat", "ddlon","tax")
L <- as.data.frame(L)
L[,1] <- as.character(L[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
L[,1] <- as.numeric(L[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
L[,2] <- as.character(L[,2])
L[,2] <- as.numeric(L[,2])
L[,3] <- as.character(L[,3])

EOO.results.L <- EOO.computing(L, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.L$Species1$spatial.polygon)
Onica.SP <- SpatialPolygonsDataFrame(EOO.results.L$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Onica.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Onica", driver="ESRI Shapefile", layer = "chull")
Onica.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Onica/chull.shp")
Onica.SHP$binomial <- "Oedipina nica"
proj4string(Onica.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Onica.SHP

plot(EOO.results.L[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon chattahoochee

Plechat <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Pchattahoochee.xls"))
M <- Plechat
# make into the colums needed
M <-  cbind(M$decimallatitude, M$decimallongitude, M$scientificname)
colnames(M) <- c("ddlat", "ddlon","tax")
M <- as.data.frame(M)
M[,1] <- as.character(M[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
M[,1] <- as.numeric(M[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
M[,2] <- as.character(M[,2])
M[,2] <- as.numeric(M[,2])
M[,3] <- as.character(M[,3])

EOO.results.M <- EOO.computing(M, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.M$Species1$spatial.polygon)
Plechat.SP <- SpatialPolygonsDataFrame(EOO.results.M$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Plechat.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plechat", driver="ESRI Shapefile", layer = "chull")
Plechat.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plechat/chull.shp")
Plechat.SHP$binomial <- "Plethodon chattahoochee"
proj4string(Plechat.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Plechat.SHP

plot(EOO.results.M[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon chlorobryonis

Plechlo <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Pchlorobryonis.xls"))
N <- Plechlo
# make into the colums needed
N <-  cbind(N$decimallatitude, N$decimallongitude, N$scientificname)
colnames(N) <- c("ddlat", "ddlon","tax")
N <- as.data.frame(N)
N[,1] <- as.character(N[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
N[,1] <- as.numeric(N[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
N[,2] <- as.character(N[,2])
N[,2] <- as.numeric(N[,2])
N[,3] <- as.character(N[,3])

EOO.results.N <- EOO.computing(N, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)
class(EOO.results.N$Species1$spatial.polygon)
Plechlo.SP <- SpatialPolygonsDataFrame(EOO.results.N$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Plechlo.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plechlo", driver="ESRI Shapefile", layer = "chull")
Plechlo.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plechlo/chull.shp")
Plechlo.SHP$binomial <- "Plethodon chlorobryonis"
proj4string(Plechlo.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Plechlo.SHP

plot(EOO.results.N[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon grobmani

Plegrob <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Pgrobmani.xls"))
O <- Plegrob
# make into the colums needed
O <-  cbind(O$decimallatitude, O$decimallongitude, O$scientificname)
colnames(O) <- c("ddlat", "ddlon","tax")
O <- as.data.frame(O)
O[,1] <- as.character(O[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
O[,1] <- as.numeric(O[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
O[,2] <- as.character(O[,2])
O[,2] <- as.numeric(O[,2])
O[,3] <- as.character(O[,3])

EOO.results.O <- EOO.computing(O, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.O$Species1$spatial.polygon)
Plegrob.SP <- SpatialPolygonsDataFrame(EOO.results.O$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Plegrob.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plegrobm", driver="ESRI Shapefile", layer = "chull")
Plegrob.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plegrobm/chull.shp")
Plegrob.SHP$binomial <- "Plethodon grobmani"
proj4string(Plegrob.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Plegrob.SHP

plot(EOO.results.O[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon mississippi

Plemiss <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Pmississippi.xls"))
P <- Plemiss
# make into the colums needed
P <-  cbind(P$decimallatitude, P$decimallongitude, P$scientificname)
colnames(P) <- c("ddlat", "ddlon","tax")
P <- as.data.frame(P)
P[,1] <- as.character(P[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
P[,1] <- as.numeric(P[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
P[,2] <- as.character(P[,2])
P[,2] <- as.numeric(P[,2])
P[,3] <- as.character(P[,3])

EOO.results.P <- EOO.computing(P, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.P$Species1$spatial.polygon)
Plemiss.SP <- SpatialPolygonsDataFrame(EOO.results.P$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Plemiss.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plemiss", driver="ESRI Shapefile", layer = "chull")
Plemiss.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plemiss/chull.shp")
Plemiss.SHP$binomial <- "Plethodon mississippi"
proj4string(Plemiss.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Plemiss.SHP

plot(EOO.results.P[[1]][[2]], col="red")
plot(land, add=T)



#######################################
# Plethodon ocmulgee

Pleocm <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Pocmulgee.xls"))
Q <- Pleocm
# make into the colums needed
Q <-  cbind(Q$decimallatitude, Q$decimallongitude, Q$scientificname)
colnames(Q) <- c("ddlat", "ddlon","tax")
Q <- as.data.frame(Q)
Q[,1] <- as.character(Q[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
Q[,1] <- as.numeric(Q[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
Q[,2] <- as.character(Q[,2])
Q[,2] <- as.numeric(Q[,2])
Q[,3] <- as.character(Q[,3])

EOO.results.Q <- EOO.computing(Q, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.Q$Species1$spatial.polygon)
Pleocm.SP <- SpatialPolygonsDataFrame(EOO.results.Q$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Pleocm.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleocm", driver="ESRI Shapefile", layer = "chull")
Pleocm.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Pleocm/chull.shp")
Pleocm.SHP$binomial <- "Plethodon ocmulgee"
proj4string(Pleocm.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Pleocm.SHP

plot(EOO.results.Q[[1]][[2]], col="red")
plot(land, add=T)


#######################################
# Plethodon variolatus

Plevar <-as.data.frame(readxl::read_xls("./Analysis_Scripts/Chapter3/Polygons/Pvariolatus.xls"))
R <- Plevar
# make into the colums needed
R <-  cbind(R$decimallatitude, R$decimallongitude, R$scientificname)
colnames(R) <- c("ddlat", "ddlon","tax")
R <- as.data.frame(R)
R[,1] <- as.character(R[,1]) # Hi lauren, I changed these lines of code so that you wouldn't lose the actual number part of the lat long
R[,1] <- as.numeric(R[,1]) # when changing a factor directly into a numeric, it reassigns each unique factor a number, starting with 1 and going up to the number of levels.
R[,2] <- as.character(R[,2])
R[,2] <- as.numeric(R[,2])
R[,3] <- as.character(R[,3])

EOO.results.R <- EOO.computing(R, method.range = "alpha.hull", 
                               export_shp = T, alpha=1, exclude.area = T, 
                               country_map = land)

class(EOO.results.R$Species1$spatial.polygon)
Plevar.SP <- SpatialPolygonsDataFrame(EOO.results.R$Species1$spatial.polygon, data=data.frame(binomial=1), match.ID = F)
writeOGR(Plevar.SP, "Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plevar", driver="ESRI Shapefile", layer = "chull")
Plevar.SHP <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/LM_Polys/Plevar/chull.shp")
Plevar.SHP$binomial <- "Plethodon variolatus"
proj4string(Plevar.SHP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Plevar.SHP

plot(EOO.results.R[[1]][[2]], col="red")
plot(land, add=T)



###########################################################
##### combo all of the polygons I made into one object ####
###########################################################

# combine all
AllLMPolys <- c(Batalta.SHP, Batbram.SHP, Batraregius.SHP,
  Batrarel.SHP, Bolitodunn.SHP, Bolitood.SHP, Bolnym.SHP, 
  Bolrob.SHP, Bradysil.SHP,Chimiq.SHP, Desmocon.SHP, Desmofus.SHP,
  Desor.SHP, Despla.SHP, Eaqua.SHP, Echam.SHP, Elome.SHP, Eurbis.SHP,
  Eurlon.SHP, Onica.SHP, Pleaur.SHP, Plechat.SHP, Plechl.SHP,
  Pledor.SHP, Plegrob.SHP, Plejor.SHP, Plemiss.SHP,  Pleocm.SHP, Pleric.SHP,
  Plevar.SHP, Pleweh.SHP)

# bind all and check names
AllLMPolysBinded <- do.call(bind, AllLMPolys)
length(unique(AllLMPolysBinded$binomial)) #31
sort(table(AllLMPolysBinded$binomial))

# check coordinates and write file
crs(AllLMPolysBinded)
writeOGR(obj=AllLMPolysBinded, dsn="Analysis_Scripts/Chapter3/Shapefiles/AllLMPolysBinded", layer="chull", 
         driver="ESRI Shapefile", overwrite_layer = T)

#checks
LMPolyCheck <- readOGR("Analysis_Scripts/Chapter3/Shapefiles/AllLMPolysBinded/chull.shp")
LMPolyCheck$binomial
plot(LMPolyCheck)
plot(LMPolyCheck[31, ])

###################################
### combine all NEW ################
####################################

AllNewLMPolys <- c(Batalta.SHP, Batbram.SHP,
                Bolnym.SHP, 
                Bolrob.SHP,Chimiq.SHP, Desmocon.SHP,
                Desor.SHP, Despla.SHP, Eaqua.SHP, Echam.SHP, Elome.SHP,
                Onica.SHP, Plechat.SHP, Plechl.SHP,
                Plegrob.SHP, Plemiss.SHP,  Pleocm.SHP,
                Plevar.SHP)

# bind all and check names
AllNewLMPolysBinded <- do.call(bind, AllNewLMPolys)
length(unique(AllNewLMPolysBinded$binomial)) #18
sort(table(AllNewLMPolysBinded$binomial))

# check coordinates and write file
crs(AllNewLMPolysBinded)
writeOGR(obj=AllNewLMPolysBinded, dsn="Analysis_Scripts/Chapter3/Shapefiles/AllNewLMPolysBinded", layer="chull", 
         driver="ESRI Shapefile", overwrite_layer = T)

#checks
LMNewPolyCheck <- readOGR("Analysis_Scripts/Chapter3/Shapefiles/AllNewLMPolysBinded/chull.shp")
LMNewPolyCheck$binomial
plot(LMNewPolyCheck)
plot(LMPolyCheck[31, ])


