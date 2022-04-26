# mapedit testing for FDRA project
# MAC 1/21/22


#https://r-spatial.org/r/2017/01/30/mapedit_intro.html

library(mapview)
library(mapedit)
library(dplyr)

what_we_created <- mapview() %>%
  editMap()

mapview(what_we_created$finished)

# edit existing features
library(sf)

# simplified border for purpose of exercise
border <- st_as_sfc(
  "LINESTRING(-109.050197582692 31.3535554844322, -109.050197582692 31.3535554844322, -111.071681957692 31.3723176640684, -111.071681957692 31.3723176640684, -114.807033520192 32.509681296831, -114.807033520192 32.509681296831, -114.741115551442 32.750242384668, -114.741115551442 32.750242384668, -117.158107738942 32.5652527715121, -117.158107738942 32.5652527715121)"
) %>%
  st_set_crs(4326)

# plot quickly for visual inspection
plot(border)

#####
# try with VPD data
# library(raster)
# 
# rasterOptions(progress = "text")
# vpd<-stack("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_vpd_1979_2020.grd")
# 
#   beginCluster(7)
#     vpdMean <- clusterR(vpd, calc, args=list(mean, na.rm=T))
#   endCluster()
# 
#writeRaster(vpdMean, filename = "vpdMean_SW_PSA.grd")

vpdMean<-raster("vpdMean_SW_PSA.grd")

# reclassify from https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/classify-raster/

# create classification matrix
reclass_df <- c(0, 0.6, 1,
                0.6, Inf, NA)
reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)
vpdCats <- reclassify(vpdMean,
                             reclass_m)
plot(vpdCats)

#vpdCont<-rasterToContour(vpdCats)
vpdPoly<-rasterToPolygons(vpdCats, dissolve=TRUE)
plot(vpdPoly)


#vpdPoly2 <- rasterToPolygons(vpdMean, fun=function(x){x<0.6}, dissolve=TRUE)

#vpdPoly <- as(vpdCats,'SpatialPolygonsDataFrame')
vpdPoly<-st_as_sf(vpdPoly)

#vpdCont<-subset(vpdCont, level=="1")

vpdPoly <- st_cast(vpdPoly, "POLYGON")

# try to edit contour lines
editVPD <- mapview(vpdPoly) %>%
  editMap("vpdPoly")


editVPD <- mapview(editVPD$all) %>%
  editMap("editVPD")
