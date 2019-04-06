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
