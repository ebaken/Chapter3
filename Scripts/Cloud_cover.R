########################################
#### cloud cover new dataset ###########
########################################

# THIS IS CLIPPING THE CLOUD COVER DATASET TO ANALYZE THE SIGNIFICANCE ACROSS POLYS WITH CSV
# ALSO THIS PREPS IT FOR THE MAXENT ANALYSIS

# from website
# https://visibleearth.nasa.gov/view.php?id=85843

# load packages
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

# NASA <- raster("./Analysis_Scripts/Chapter3/Climate Data/globalcldfr_amo_200207-201504_geo.tif")
# identicalCRS(NASA, Polygons)
# NASA <- projectRaster(NASA, crs=crs(Polygons))
# NASA.p <- rasterToPoints(NASA)
# NASA.df <- data.frame(NASA.p)
# colnames(NASA.df) <- c("x","y","MAP")
# summary(NASA.df)
# NASA1 <- crop(NASA, alldata_tog)
# writeRaster(NASA1, filename="./Analysis_Scripts/Chapter3/Climate Data/Cropped_Cloud.tif")

NASA <- raster("./Analysis_Scripts/Chapter3/Climate Data/Cropped_Cloud.tif")
AllPolysforanalysis <- readOGR("./Analysis_Scripts/Chapter3/Shapefiles/AllPolysforAnalysis/chull.shp")
Records <- matrix(NA, nrow = 311, ncol = 7)
colnames(Records) <- c("binomial","CloudMin","Cloud1Q","CloudMed","CloudMea","Cloud3Q","CloudMax")

RecordsDF <- as.data.frame(Records)
for (i in 1:311) {
  #i <- 4
  envSpecies <- data.frame(extract(NASA, AllPolysforanalysis[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  RecordsDF[i,] <-  c(as.character(AllPolysforanalysis[i,]$binomial),summary(envSpecies))
  print(i)
}

RecordsDF
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Max.   :", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Mean   :", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Median :", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Min.   :", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="1st Qu.:", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="3rd Qu.:", replacement="")
RecordsDF
write.csv(RecordsDF, file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Cloud_cover.csv")
























