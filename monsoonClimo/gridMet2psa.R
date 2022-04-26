# process gridmet into PSA time series
# Fire weather/climate analysis
# MAC 05/27/21

library(rgdal)
library(raster)
library(snow)
library(tidyr)
library(dplyr)

# set rasteroptions
rasterOptions(progress = 'text')

# psa zones
psa<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="National_Predictive_Service_Areas_(PSA)_Boundaries")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")

# common date list for POR
dates<-seq(as.Date(paste0(1979,"-01-01"),format="%Y-%m-%d"),as.Date(paste0(2020,"-12-31"),format="%Y-%m-%d"),1)

# load datasets
# gridded: NARR, gridmet, PRISM
# stations: RAWS, mesowest,...

#vars<-c("pr", "pet", "rmax", "rmin", "sph", "erc","bi","srad","tmmn", "tmmx", "vpd", "vs","th", "fm100","fm1000")
# vars<-c("rmin","erc","bi","fm100") # extStack_working.RData
# vars<-c("pr", "vs","rmax","vpd")   # extStack_working2.RData
#vars<-c("pet", "fm1000","tmmn","tmmx")     # extStack_working3.RData
vars<-c("sph", "srad","th")     # extStack_working4.RData

extStack=list()

for(k in 1:length(vars)){
  print(vars[k])
  gridmet<-stack(paste0("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_",vars[k],"_1979_2020.grd"))
  #gridTS <- (t(extract(gridmet, sw_psa, fun=mean, df=TRUE, na.rm=TRUE)))
  # parallel extract
  ptm <- proc.time()
  # Extract
  beginCluster(n=7)
  gridTS<-as.data.frame(t(raster::extract(gridmet, sw_psa, fun=mean, df=TRUE, na.rm=TRUE)))
  endCluster()
  proc.time() - ptm
  gridTS<-gridTS[2:nrow(gridTS),]
  gridTS$var<-vars[k]
  gridTS$date<-dates
  extStack[[k]]<- gridTS
}

save(extStack, file = "/home/crimmins/RProjects/FireClimate/monsoonClimo/extStack_working4.RData")