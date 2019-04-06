################################################
#### FUTURE CLIMATE MODELS #####################
################################################
### DIFFER IN GCM, RESOLUTION, SCENARIO, YEAR ##
################################################

#Load packages
library(maptools)
library(rgeos)

#Get polygons
Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 
# 311 species

#Give polygons CRS
proj4string(Polygons) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

##### YEAR 2050 ###############################

getwd()
############################# model BB50 #######
#Get climate data for specified projection
#GCM model BC, Scenario 26, Bioclim variables, Year 2050
B26B50Files <- list.files('./Analysis_Scripts/Chapter3/Projections/2.5_Reso/2050/BCC-CSM1-1/bc26bi50', full.names = T, pattern = '.tif')
B26B50Files

#create one stack of climate data
B26B50stack <- stack(B26B50Files)

#Set up Polygons for climate data
# Set study extent for clipping
eco <- rgdal::readOGR('./Analysis_Scripts/Chapter3/Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = './ASP', compress = T)

#Extract Future Climate using polygons

#create matrix to put climate data in
B26B50Records <- matrix(NA, nrow = 311, ncol = 115)

#label column names in matrix
colnames(B26B50Records) <- c("binomial",
                       "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                       "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                       "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                       "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                       "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                       "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                       "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                       "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                       "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                       "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                       "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                       "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                       "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                       "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                       "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                       "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                       "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                       "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                       "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

#make as a data frame
B26B50RecordsDF <- as.data.frame(B26B50Records)

#run loop for each variable
for (i in 1:311) {
  #i = 3 
  envSpecies <- data.frame(extract(B26B50stack, Polygons[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  B26B50RecordsDF[i,] <-  c(Polygons[i,]$binomial,summary(envSpecies$bc26bi501),summary(envSpecies$bc26bi502),
                      summary(envSpecies$bc26bi503),summary(envSpecies$bc26bi504),summary(envSpecies$bc26bi505),
                      summary(envSpecies$bc26bi506),summary(envSpecies$bc26bi507),summary(envSpecies$bc26bi508),
                      summary(envSpecies$bc26bi509),summary(envSpecies$bc26bi5010),summary(envSpecies$bc26bi5011),
                      summary(envSpecies$bc26bi5012),summary(envSpecies$bc26bi5013),summary(envSpecies$bc26bi5014),
                      summary(envSpecies$bc26bi5015),summary(envSpecies$bc26bi5016),summary(envSpecies$bc26bi5017),
                      summary(envSpecies$bc26bi5018),summary(envSpecies$bc26bi5019))
  print(i)
}

#observe
B26B50RecordsDF
write.csv(B26B50RecordsDF, file = "./Analysis_Scripts/Chapter3/Projections/2.5_Reso/2050/Model_Output/B26B50Projected.csv")





#Get climate data for specified projection
#GCM model BC, Scenario 45, Bioclim variables, Year 2050
B45B50Files <- list.files('./Projections/2.5_Reso/2050/BCC-CSM1-1/bc45bi50', full.names = T, pattern = '.tif')
B45B50Files

#create one stack of climate data
B45B50stack <- stack(B45B50Files)

#Set up Polygons for climate data
# Set study extent for clipping
eco <- rgdal::readOGR('./Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = './ASP', compress = T)

#Extract Future Climate using polygons

#create matrix to put climate data in
B45B50Records <- matrix(NA, nrow = 318, ncol = 115)

#label column names in matrix
colnames(B45B50Records) <- c("binomial",
                             "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                             "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                             "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                             "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                             "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                             "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                             "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                             "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                             "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                             "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                             "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                             "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                             "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                             "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                             "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                             "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                             "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                             "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                             "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

#make as a data frame
B45B50RecordsDF <- as.data.frame(B45B50Records)

#run loop for each variable
#very tedious to edit each variable name...maybe find better way to code
for (i in 1:318) {
  #i = 3
  envSpecies <- data.frame(extract(B45B50stack, Polygons[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  B45B50RecordsDF[i,] <-  c(Polygons[i,]$binomial,summary(envSpecies$bc45bi501),summary(envSpecies$bc45bi502),
                            summary(envSpecies$bc45bi503),summary(envSpecies$bc45bi504),summary(envSpecies$bc45bi505),
                            summary(envSpecies$bc45bi506),summary(envSpecies$bc45bi507),summary(envSpecies$bc45bi508),
                            summary(envSpecies$bc45bi509),summary(envSpecies$bc45bi5010),summary(envSpecies$bc45bi5011),
                            summary(envSpecies$bc45bi5012),summary(envSpecies$bc45bi5013),summary(envSpecies$bc45bi5014),
                            summary(envSpecies$bc45bi5015),summary(envSpecies$bc45bi5016),summary(envSpecies$bc45bi5017),
                            summary(envSpecies$bc45bi5018),summary(envSpecies$bc45bi5019))
  print(i)
}

#observe
B45B50RecordsDF

write.csv(B45B50RecordsDF, file = "./Projections/2.5_Reso/2050/Model_Output/B45B50Projected.csv")



#Get climate data for specified projection
#GCM model BC, Scenario 60, Bioclim variables, Year 2050
B60B50Files <- list.files('./Projections/2.5_Reso/2050/BCC-CSM1-1/bc60bi50', full.names = T, pattern = '.tif')

#create one stack of climate data
B60B50stack <- stack(B60B50Files)

#Set up Polygons for climate data
# Set study extent for clipping
eco <- rgdal::readOGR('./Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = './ASP', compress = T)

#Extract Future Climate using polygons

#create matrix to put climate data in
B60B50Records <- matrix(NA, nrow = 318, ncol = 115)

#label column names in matrix
colnames(B60B50Records) <- c("binomial",
                             "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                             "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                             "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                             "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                             "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                             "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                             "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                             "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                             "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                             "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                             "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                             "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                             "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                             "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                             "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                             "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                             "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                             "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                             "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

#make as a data frame
B60B50RecordsDF <- as.data.frame(B60B50Records)

#run loop for each variable
#very tedious to edit each variable name...maybe find better way to code
for (i in 1:318) {
  #i = 3
  envSpecies <- data.frame(extract(B60B50stack, Polygons[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  B60B50RecordsDF[i,] <-  c(Polygons[i,]$binomial,summary(envSpecies$bc60bi501),summary(envSpecies$bc60bi502),
                            summary(envSpecies$bc60bi503),summary(envSpecies$bc60bi504),summary(envSpecies$bc60bi505),
                            summary(envSpecies$bc60bi506),summary(envSpecies$bc60bi507),summary(envSpecies$bc60bi508),
                            summary(envSpecies$bc60bi509),summary(envSpecies$bc60bi5010),summary(envSpecies$bc60bi5011),
                            summary(envSpecies$bc60bi5012),summary(envSpecies$bc60bi5013),summary(envSpecies$bc60bi5014),
                            summary(envSpecies$bc60bi5015),summary(envSpecies$bc60bi5016),summary(envSpecies$bc60bi5017),
                            summary(envSpecies$bc60bi5018),summary(envSpecies$bc60bi5019))
  print(i)
}

#observe
B60B50RecordsDF

write.csv(B60B50RecordsDF, file = "./Projections/2.5_Reso/2050/Model_Output/B60B50Projected.csv")







#Get climate data for specified projection
#GCM model BC, Scenario 85, Bioclim variables, Year 2050
B85B50Files <- list.files('./Projections/2.5_Reso/2050/BCC-CSM1-1/bc85bi50', full.names = T, pattern = '.tif')

#create one stack of climate data
B85B50stack <- stack(B85B50Files)

#Set up Polygons for climate data
# Set study extent for clipping
eco <- rgdal::readOGR('./Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = './ASP', compress = T)

#Extract Future Climate using polygons

#create matrix to put climate data in
B85B50Records <- matrix(NA, nrow = 318, ncol = 115)

#label column names in matrix
colnames(B85B50Records) <- c("binomial",
                             "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                             "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                             "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                             "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                             "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                             "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                             "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                             "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                             "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                             "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                             "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                             "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                             "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                             "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                             "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                             "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                             "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                             "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                             "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

#make as a data frame
B85B50RecordsDF <- as.data.frame(B85B50Records)

#run loop for each variable
#very tedious to edit each variable name...maybe find better way to code
for (i in 1:318) {
  #i = 3
  envSpecies <- data.frame(extract(B85B50stack, Polygons[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  B85B50RecordsDF[i,] <-  c(Polygons[i,]$binomial,summary(envSpecies$bc85bi501),summary(envSpecies$bc85bi502),
                            summary(envSpecies$bc85bi503),summary(envSpecies$bc85bi504),summary(envSpecies$bc85bi505),
                            summary(envSpecies$bc85bi506),summary(envSpecies$bc85bi507),summary(envSpecies$bc85bi508),
                            summary(envSpecies$bc85bi509),summary(envSpecies$bc85bi5010),summary(envSpecies$bc85bi5011),
                            summary(envSpecies$bc85bi5012),summary(envSpecies$bc85bi5013),summary(envSpecies$bc85bi5014),
                            summary(envSpecies$bc85bi5015),summary(envSpecies$bc85bi5016),summary(envSpecies$bc85bi5017),
                            summary(envSpecies$bc85bi5018),summary(envSpecies$bc85bi5019))
  print(i)
}

#observe
B85B50RecordsDF

write.csv(B85B50RecordsDF, file = "./Projections/2.5_Reso/2050/Model_Output/B85B50Projected.csv")
################################################

########################## model CC50
#Get climate data for specified projection
#GCM model CCSM4, Scenario 26, Bioclim variables, Year 2050
CC26B50Files <- list.files('./Projections/2.5_Reso/2050/CCSM4/cc26bi50', full.names = T, pattern = '.tif')

#create one stack of climate data
CC26B50stack <- stack(CC26B50Files)

#Set up Polygons for climate data
# Set study extent for clipping
eco <- rgdal::readOGR('./Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = './ASP', compress = T)

#Extract Future Climate using polygons

#create matrix to put climate data in
CC26B50Records <- matrix(NA, nrow = 318, ncol = 115)

#label column names in matrix
colnames(CC26B50Records) <- c("binomial",
                             "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                             "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                             "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                             "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                             "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                             "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                             "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                             "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                             "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                             "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                             "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                             "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                             "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                             "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                             "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                             "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                             "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                             "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                             "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

#make as a data frame
CC26B50RecordsDF <- as.data.frame(CC26B50Records)

#run loop for each variable
#very tedious to edit each variable name...maybe find better way to code
for (i in 1:318) {
  #i = 3
  envSpecies <- data.frame(extract(CC26B50stack, Polygons[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  CC26B50RecordsDF[i,] <-  c(Polygons[i,]$binomial,summary(envSpecies$cc26bi501),summary(envSpecies$cc26bi502),
                            summary(envSpecies$cc26bi503),summary(envSpecies$cc26bi504),summary(envSpecies$cc26bi505),
                            summary(envSpecies$cc26bi506),summary(envSpecies$cc26bi507),summary(envSpecies$cc26bi508),
                            summary(envSpecies$cc26bi509),summary(envSpecies$cc26bi5010),summary(envSpecies$cc26bi5011),
                            summary(envSpecies$cc26bi5012),summary(envSpecies$cc26bi5013),summary(envSpecies$cc26bi5014),
                            summary(envSpecies$cc26bi5015),summary(envSpecies$cc26bi5016),summary(envSpecies$cc26bi5017),
                            summary(envSpecies$cc26bi5018),summary(envSpecies$cc26bi5019))
  print(i)
}

#observe
CC26B50RecordsDF

write.csv(CC26B50RecordsDF, file = "./Projections/2.5_Reso/2050/Model_Output/CC26B50Projected.csv")




#Get climate data for specified projection
#GCM model CCSM4, Scenario 45, Bioclim variables, Year 2050
CC45B50Files <- list.files('./Projections/2.5_Reso/2050/CCSM4/cc45bi50', full.names = T, pattern = '.tif')

#create one stack of climate data
CC45B50stack <- stack(CC45B50Files)

#Set up Polygons for climate data
# Set study extent for clipping
eco <- rgdal::readOGR('./Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = './ASP', compress = T)

#Extract Future Climate using polygons

#create matrix to put climate data in
CC45B50Records <- matrix(NA, nrow = 318, ncol = 115)

#label column names in matrix
colnames(CC45B50Records) <- c("binomial",
                              "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                              "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                              "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                              "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                              "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                              "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                              "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                              "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                              "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                              "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                              "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                              "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                              "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                              "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                              "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                              "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                              "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                              "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                              "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

#make as a data frame
CC45B50RecordsDF <- as.data.frame(CC45B50Records)

#run loop for each variable
#very tedious to edit each variable name...maybe find better way to code
for (i in 1:318) {
  #i = 3
  envSpecies <- data.frame(extract(CC45B50stack, Polygons[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  CC45B50RecordsDF[i,] <-  c(Polygons[i,]$binomial,summary(envSpecies$cc45bi501),summary(envSpecies$cc45bi502),
                             summary(envSpecies$cc45bi503),summary(envSpecies$cc45bi504),summary(envSpecies$cc45bi505),
                             summary(envSpecies$cc45bi506),summary(envSpecies$cc45bi507),summary(envSpecies$cc45bi508),
                             summary(envSpecies$cc45bi509),summary(envSpecies$cc45bi5010),summary(envSpecies$cc45bi5011),
                             summary(envSpecies$cc45bi5012),summary(envSpecies$cc45bi5013),summary(envSpecies$cc45bi5014),
                             summary(envSpecies$cc45bi5015),summary(envSpecies$cc45bi5016),summary(envSpecies$cc45bi5017),
                             summary(envSpecies$cc45bi5018),summary(envSpecies$cc45bi5019))
  print(i)
}

#observe
CC45B50RecordsDF

write.csv(CC45B50RecordsDF, file = "./Projections/2.5_Reso/2050/Model_Output/CC45B50Projected.csv")



#Get climate data for specified projection
#GCM model CCSM4, Scenario 60, Bioclim variables, Year 2050
CC60B50Files <- list.files('./Projections/2.5_Reso/2050/CCSM4/cc60bi50', full.names = T, pattern = '.tif')

#create one stack of climate data
CC60B50stack <- stack(CC60B50Files)

#Set up Polygons for climate data
# Set study extent for clipping
eco <- rgdal::readOGR('./Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = './ASP', compress = T)

#Extract Future Climate using polygons

#create matrix to put climate data in
CC60B50Records <- matrix(NA, nrow = 318, ncol = 115)

#label column names in matrix
colnames(CC60B50Records) <- c("binomial",
                              "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                              "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                              "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                              "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                              "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                              "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                              "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                              "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                              "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                              "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                              "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                              "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                              "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                              "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                              "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                              "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                              "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                              "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                              "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

#make as a data frame
CC60B50RecordsDF <- as.data.frame(CC60B50Records)

#run loop for each variable
#very tedious to edit each variable name...maybe find better way to code
for (i in 1:318) {
  #i = 3
  envSpecies <- data.frame(extract(CC60B50stack, Polygons[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  CC60B50RecordsDF[i,] <-  c(Polygons[i,]$binomial,summary(envSpecies$cc60bi501),summary(envSpecies$cc60bi502),
                             summary(envSpecies$cc60bi503),summary(envSpecies$cc60bi504),summary(envSpecies$cc60bi505),
                             summary(envSpecies$cc60bi506),summary(envSpecies$cc60bi507),summary(envSpecies$cc60bi508),
                             summary(envSpecies$cc60bi509),summary(envSpecies$cc60bi5010),summary(envSpecies$cc60bi5011),
                             summary(envSpecies$cc60bi5012),summary(envSpecies$cc60bi5013),summary(envSpecies$cc60bi5014),
                             summary(envSpecies$cc60bi5015),summary(envSpecies$cc60bi5016),summary(envSpecies$cc60bi5017),
                             summary(envSpecies$cc60bi5018),summary(envSpecies$cc60bi5019))
  print(i)
}

#observe
CC60B50RecordsDF

write.csv(CC60B50RecordsDF, file = "./Projections/2.5_Reso/2050/Model_Output/CC60B50Projected.csv")

