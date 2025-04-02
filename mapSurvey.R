# map survey resonses 
# MAC 09/30/22

library(readxl)
library(leaflet)
library(rgdal)
library(ggplot2)
library(dplyr)

fire_climate_lat_long <- read_excel("fire climate lat long_10_20_22.xlsx")

dispatch_bounds <- readOGR("./shapes/National_Dispatch_Current.shp",
                                     layer = "National_Dispatch_Current")


leaflet(data = fire_climate_lat_long) %>% addTiles() %>%
  addPolygons(data=dispatch_bounds, color = "#444444", weight = 1, smoothFactor = 0.5,
                              opacity = 1.0, fillOpacity = 0.5,
                              fillColor = NA) %>%
  addMarkers(~Longitude, ~Latitude)


# map some ggmaps
library(ggmap)

# API key
source('APIkey.R')
# get map and bounding box
where<-geocode("Arizona", source = "google")
baseMap <- get_map(location=where,
                 source="stamen", maptype="toner", crop=FALSE, zoom=6)
ggmap(baseMap)+
  geom_point(data = fire_climate_lat_long, aes(x=Longitude,y=Latitude), alpha=0.5, color="darkred", size=3)+
  geom_polygon(aes(x = long, y = lat, group=group),
               data = dispatch_bounds, color ="black", fill =NA,
               alpha = .4, size = .2)

# intersect points and polys
fire_climate_lat_long <- read_excel("fire climate lat long_10_21_22.xlsx")

fire_climate_lat_long<-fire_climate_lat_long[,-2]
fire_climate_lat_long<-na.omit(fire_climate_lat_long)
fire_climate_lat_long_df<-fire_climate_lat_long

coordinates(fire_climate_lat_long) <- ~ Longitude + Latitude
raster::crs(fire_climate_lat_long)<-raster::crs(dispatch_bounds)

joinsDF<-over(fire_climate_lat_long,dispatch_bounds, returnList = FALSE)

joinsDF<-cbind.data.frame(fire_climate_lat_long_df,joinsDF)

summJoins<-joinsDF %>% group_by(DispName) %>%
                        summarise(allSources=sum(`Total Used Both Seasons`))

ggplot(joinsDF, aes(DispName,`Total Used Both Seasons`))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# export xls
library(xlsx)

write.xlsx(joinsDF, file="fire_climate_lat_long_10_21_22_wZones.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
