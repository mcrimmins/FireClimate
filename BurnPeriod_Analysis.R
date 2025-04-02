# RH/Burn Period analysis using RAWS -- Looking at wind/dp interactions
# adapted from RAWS_BurnPeriod_CEFA.R
# MAC 04/25/22
# using CEFA data https://cefa.dri.edu/raws/index.php

library(rgdal)
library(tidyr)
library(dplyr)
library(stringr)

# psa zones
psa<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="National_Predictive_Service_Areas_(PSA)_Boundaries")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")
# get psa centroids for factor order
sw_psaDF<- cbind.data.frame(sw_psa, rgeos::gCentroid(sw_psa,byid=TRUE))


# key raws sites https://fsapps.nwcg.gov/psp/npsg/forecast/home/downloads
raws<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="key_raws")
sw_raws<-subset(raws, GACC=="Southwest")
sw_rawsDF<-sw_raws@data
# delete duplicated station entries
sw_rawsDF<-sw_rawsDF[!duplicated(sw_rawsDF[ , c("STA_NAME")]),]

# station number as character
sw_rawsDF$STATION_NU<-as.character(sw_rawsDF$STATION_NU)
sw_rawsDF$STATION_NU<-str_pad(sw_rawsDF$STATION_NU, 6, pad = "0")

# load in RAWS data --- update data using RAWS_BurnPeriod_CEFA.R
load("~/RProjects/FireClimate/monsoonClimo/allRAWScomb_lists.RData")

#####
# RAWS site stats
statsRAWS<-list()
for(i in 1:length(allRAWS_comb)){
  # count of burn hours
  tempRAWS<-  allRAWS_comb[[i]] 
  statsRAWS[[i]]<-cbind.data.frame(as.character(sw_rawsDF$STA_NAME[i]),min(tempRAWS$year),max(tempRAWS$year))  
}

statsRAWS <- do.call(rbind, statsRAWS)
  colnames(statsRAWS)<-c("station","minYr","maxYr")
statsRAWS$yrs<-statsRAWS$maxYr-statsRAWS$minYr
#####

# focus on station
tempRAWS<-  allRAWS_comb[[39]] 
# add date
tempRAWS$date<-as.Date(paste0(tempRAWS$year,"-",tempRAWS$month,"-",tempRAWS$day))
# fix off 00 hrs
tempRAWS$Time<-plyr::round_any(tempRAWS$Time, 10)
# add in day/night label
tempRAWS$diurnal<-ifelse(tempRAWS$Time<=2000 & tempRAWS$Time>=900, "day","night")
# subset outside of 0 and 100
tempRAWS<-subset(tempRAWS, Moisture<100)

# plots
library(ggplot2)

ggplot(tempRAWS, aes(Time, Moisture, group=Time))+
  geom_boxplot()+
  facet_wrap(.~diurnal)

ggplot(tempRAWS, aes(Temp, ws, color=Temp))+
  geom_point()+
  facet_grid(month~Time)+
  geom_smooth(method = "lm", se = FALSE, color="red")+
  scale_color_gradientn(colours = rev(rainbow(5)))


# burn period monthly stats
# load in RAWS data
load("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/burnList.RData")
temp<-burnList[[39]]


dayQuantiles<- temp %>% group_by(month) %>% summarise(
  q05 = quantile(rh_lt_20,0.05,na.rm='TRUE'),
  q50 = quantile(rh_lt_20,0.50,na.rm='TRUE'),
  q95 = quantile(rh_lt_20,0.95,na.rm='TRUE'),
  min = min(rh_lt_20,na.rm='TRUE'),
  max = max(rh_lt_20,na.rm='TRUE'),
  avg = mean(rh_lt_20,na.rm='TRUE'),
  n = n())



