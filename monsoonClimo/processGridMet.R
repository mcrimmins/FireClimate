# process gridmet data for PSA fire-climate analysis
# adapting from /RangeDrought/summarizeGridMet.R
# mac 5/27/21

library(raster)
library(rgdal)

# set rasteroptions
rasterOptions(progress = 'text')

# psa zones
psa<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="National_Predictive_Service_Areas_(PSA)_Boundaries")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")

# list of variables
# pr, pet, rmax, rmin, sph, srad, th, tmmn, tmmx, vpd, vs
#   erc, bi
vars<-c("pr", "pet", "rmax", "rmin", "sph", "erc","bi","srad","tmmn", "tmmx", "vpd", "vs","th", "fm100","fm1000")

for(k in 14:15){
  yrStack=list()
  i<-1
    for(yr in 1979:2020){
      gridMet<-stack(paste0("/scratch/crimmins/gridmet/update_Aug2019/",vars[k],"_",yr,".nc"))
      # crop to region
      gridMet <- crop(gridMet, extent(sw_psa))
      dates<-seq(as.Date(paste0(yr,"-01-01"),format="%Y-%m-%d"),as.Date(paste0(yr,"-12-31"),format="%Y-%m-%d"),1)
      names(gridMet)<-dates
      yrStack[[i]]<- gridMet
      print(yr)
      i=i+1
    }
    
    # combine into stack
    temp = stack(yrStack)
    
    # save cropped raster
    writeRaster(temp, filename = paste0("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_",vars[k],"_1979_2020.grd"),
                overwrite=TRUE)
    rm(temp)
    gc()
}


# test
# dates<-as.data.frame(seq(as.Date(paste0(1979,"-01-01"),format="%Y-%m-%d"),as.Date(paste0(2020,"-12-31"),format="%Y-%m-%d"),1))

 #test<-stack(paste0("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_",vars[k],"_1979_2020.grd"))