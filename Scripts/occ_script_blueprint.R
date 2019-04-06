setwd("~/Desktop/Dean Adams/Salamander Proj")

##  ---------------------------------------------------------------------------------------------------------------------  ##
                        # Simplifed Species Distribution Modeling Code
##  ---------------------------------------------------------------------------------------------------------------------  ##
library(rgbif); library(rbison); library(ecoengine); library(spocc); library(rgdal);
library(rgeos); library(dismo); library(raster); library(scrubr); library(geosphere);
library(scales); library(rJava); library(mapr); library(ggmap); library(ggplot2);
library(sp)

# lat/lon box from which occurrence records are to be collected
geom <- c(-130, 20, -60, 55)

##  ---------------------------------------------------------------------------------------------------------------------  ##
                              # Data Entry and Preliminary Cleaning
##  ---------------------------------------------------------------------------------------------------------------------  ##
#spnames <- c('Plethodon jordani')
#spnames <- PlethPolyPrunedd$binomial
#spnames <- as.character(spnames)
#spnames[1]

#df_mult <- occ(query = spnames[1], from = c('gbif', 'ecoengine', 'bison', 'vertnet', 'inat', 'amphibiaweb'),
               limit = 100, geometry = geom, has_coords = T)

df_nam <- fixnames(df_mult, how = "query")
df_comb <- occ2df(df_nam)
df_comb

records_geoclean <- df_comb %>% coord_incomplete(drop = T) %>% coord_unlikely(drop = T)
# Want to do coord_impossible too, but some unkown error is happening
# records_geoclean <- df_comb %>% coord_impossible(drop = T) %>% coord_incomplete(drop = T) %>% coord_unlikely(drop = T)

records_dateclean <- records_geoclean
records_dateclean <- date_standardize(records_dateclean, "%Y")
records_dateclean <- date_missing(records_dateclean, format = "%Y", drop = T)
records_dateclean$date <- as.numeric(records_dateclean$date)
records_dateclean

# Set which years for which you want data
records <- subset(records_dateclean, records_dateclean$date >= 1960 & records_dateclean$date <= 2018)
nrow(records)

records$longitude <- as.numeric(records$longitude)
records$latitude <- as.numeric(records$latitude)

recordsSpatial <- SpatialPointsDataFrame(coords = cbind(records$longitude, records$latitude), data = records,
                                proj4string = CRS('+proj=longlat +datumWGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

#saveRDS(records, './ASP/Species Records/ASP V1 - Initial Pull.rds')
# what is RDS

##  ----------------------------------------------------------------------------------------------------------------------  ##
                                      # Ecoregion Selection
##  ----------------------------------------------------------------------------------------------------------------------  ##
# Load in the shape files
eco <- rgdal::readOGR('./Borders', 'provinces', verbose = F)

# Ditch information from oceans
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]

# Make it spatially-explicit
recordsSpatial <- SpatialPointsDataFrame(coords = cbind(records$longitude, records$latitude), data = records,
                          proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

# Must run this, but its specifics are largely irrelevant to you
ecoContain <- eco[recordsSpatial, ]
ecoContain <- eco[PlethPolyPrunedd, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)

# Save (you'll be happy about this when your R crashes)
save(studyExtent, file = './ASP', compress = T)

# Plotting occurrence records (sans climate info)
plot(recordsSpatial, main = 'Plethodon jordani')
#sp::plot(countries, add = T, col = 'gray80')
#sp::plot(states, add = T, col = 'gray80')
points(records$longitude, records$latitude, pch = 21, bg = 'gray45')
#legend('bottomleft', legend = c(paste0('N = ', nrow(records))bty = 'n'))

############# using elevation ... broken? ###########
##  ----------------------------------------------------------------------------------------------------------------------  ##
                                    # Clipping rasters
##  ----------------------------------------------------------------------------------------------------------------------  ##
ele <- getData("SRTM", lon = (-130:-60), lat = (20:55))

elevation <- raster('./WORLDCLIM/Full/Elevation/alt.bil')
elevation <- crop(elevation, studyExtent)
elevation <- setMinMax(elevation)

projection(elevation) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
writeRaster(elevation, './ASP/WORLDCLIM/Study Region - Elevation/elevation',
            format = 'GTiff', datatype = 'INT2S', overwrite = T)

for (i in 1:19) {
  print(paste('Clipping current WORLDCLIM raster ', i))
  flush.console()
  
  rast <- raster(paste0('./WORLDCLIM/Full/1960-1990/bio', i, '.bil'))
  rast <- crop(rast, studyExtent)
  rast <- setMinMax(rast)
  projection(rast) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  
  writeRaster(rast, paste0('./ASP/WORLDCLIM/Study Region - BIOCLIM/WC', ifelse(i < 10, '0', ''), i),
              format = 'GTiff', datatype = 'INT2S', overwrite = T)}

current <- stack(list.files('./ASP/WORLDCLIM/Study Region - BIOCLIM', full.names = T, pattern = '.tif'))

plot(current, main = paste0('BIO', 1:19))

##  ----------------------------------------------------------------------------------------------------------------------  ##
                          # Match Records with Environmental Data
##  ----------------------------------------------------------------------------------------------------------------------  ##

env <- stack(c('./ASP/WORLDCLIM/Study Region - Elevation/elevation.tif',
               list.files('./ASP/WORLDCLIM/Study Region - BIOCLIM', full.names = T)))

envSpecies <- data.frame(extract(env, cbind(records$longitude, records$latitude)))
records <- cbind(records, envSpecies)

if (any(is.na(rowSums(envSpecies)))) records <- records[-which(is.na(rowSums(envSpecies))), ]

saveRDS(records, './ASP/Species Records/ASP V2 - Matched with Env.rds')

plot(records$WC01, records$WC12, xlab = 'MAT (deg C x 10)', ylab = 'MAP (mm)', main = '')
plot(records$elevation, records$WC01, xlab = 'Elevation (m)', ylab = 'MAT (deg C x 10)', main = '')
plot(records$elevation, records$WC12, xlab = 'Elevation (m)', ylab = 'MAP (mm)', main = '')



##  ---------------------------------------------------------------------------------------------------------------------  ##
                        # Histograms of Predictor Variables in each model
##  ---------------------------------------------------------------------------------------------------------------------  ##

# Dataframe for present data
pres.wc08 <- data.frame(records$WC08); pres.wc08$origin <- 'present'; names(pres.wc08) <- c("WC08", "origin")
pres.wc09 <- data.frame(records$WC09); pres.wc09$origin <- 'present'; names(pres.wc09) <- c("WC09", "origin")
pres.wc10 <- data.frame(records$WC10); pres.wc10$origin <- 'present'; names(pres.wc10) <- c("WC10", "origin")
pres.wc11 <- data.frame(records$WC11); pres.wc11$origin <- 'present'; names(pres.wc11) <- c("WC11", "origin")
pres.wc16 <- data.frame(records$WC16); pres.wc16$origin <- 'present'; names(pres.wc16) <- c("WC16", "origin")
pres.wc17 <- data.frame(records$WC17); pres.wc17$origin <- 'present'; names(pres.wc17) <- c("WC17", "origin")
pres.wc19 <- data.frame(records$WC19); pres.wc19$origin <- 'present'; names(pres.wc19) <- c("WC19", "origin")

# Data frames for CCSM4
cc.wc08 <- raster::as.data.frame(futcc.pred$WC08); cc.wc08$origin <- 'CCSM4'
cc.wc09 <- raster::as.data.frame(futcc.pred$WC09); cc.wc09$origin <- 'CCSM4'
cc.wc10 <- raster::as.data.frame(futcc.pred$WC10); cc.wc10$origin <- 'CCSM4'
cc.wc11 <- raster::as.data.frame(futcc.pred$WC11); cc.wc11$origin <- 'CCSM4'
cc.wc16 <- raster::as.data.frame(futcc.pred$WC16); cc.wc16$origin <- 'CCSM4'
cc.wc17 <- raster::as.data.frame(futcc.pred$WC17); cc.wc17$origin <- 'CCSM4'
cc.wc19 <- raster::as.data.frame(futcc.pred$WC19); cc.wc19$origin <- 'CCSM4'

# Data frames for HadGEM2-ES
he.wc08 <- raster::as.data.frame(futhe.pred$WC08); he.wc08$origin <- 'HadGEM2-ES'
he.wc09 <- raster::as.data.frame(futhe.pred$WC09); he.wc09$origin <- 'HadGEM2-ES'
he.wc10 <- raster::as.data.frame(futhe.pred$WC10); he.wc10$origin <- 'HadGEM2-ES'
he.wc11 <- raster::as.data.frame(futhe.pred$WC11); he.wc11$origin <- 'HadGEM2-ES'
he.wc16 <- raster::as.data.frame(futhe.pred$WC16); he.wc16$origin <- 'HadGEM2-ES'
he.wc17 <- raster::as.data.frame(futhe.pred$WC17); he.wc17$origin <- 'HadGEM2-ES'
he.wc19 <- raster::as.data.frame(futhe.pred$WC19); he.wc19$origin <- 'HadGEM2-ES'

# Combine dataframes for ggplot
comp.wc08 <- rbind(pres.wc08, cc.wc08, he.wc08)
comp.wc09 <- rbind(pres.wc09, cc.wc09, he.wc09)
comp.wc10 <- rbind(pres.wc10, cc.wc10, he.wc10)
comp.wc11 <- rbind(pres.wc11, cc.wc11, he.wc11)
comp.wc16 <- rbind(pres.wc16, cc.wc16, he.wc16)
comp.wc17 <- rbind(pres.wc17, cc.wc17, he.wc17)
comp.wc19 <- rbind(pres.wc19, cc.wc19, he.wc19)

# Plot(!)
ggplot(comp.wc08, aes(WC08, fill = origin)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggsave("ASP/_Outputs/Histograms/ASP_wc08.pdf", plot = last_plot())

ggplot(comp.wc09, aes(WC09, fill = origin)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggsave("ASP/_Outputs/Histograms/ASP_wc09.pdf", plot = last_plot())

ggplot(comp.wc10, aes(WC10, fill = origin)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggsave("ASP/_Outputs/Histograms/ASP_wc10.pdf", plot = last_plot())

ggplot(comp.wc11, aes(WC11, fill = origin)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggsave("ASP/_Outputs/Histograms/ASP_wc11.pdf", plot = last_plot())

ggplot(comp.wc16, aes(WC16, fill = origin)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggsave("ASP/_Outputs/Histograms/ASP_wc16.pdf", plot = last_plot())

ggplot(comp.wc17, aes(WC17, fill = origin)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggsave("ASP/_Outputs/Histograms/ASP_wc17.pdf", plot = last_plot())

ggplot(comp.wc19, aes(WC19, fill = origin)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggsave("ASP/_Outputs/Histograms/ASP_wc19.pdf", plot = last_plot())

###########################