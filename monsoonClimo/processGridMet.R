# process gridmet data for PSA fire-climate analysis
# adapting from /RangeDrought/summarizeGridMet.R
# download new gridmet data using ./ClimPlot/gridmet/downloadGridMet.R
# mac 5/27/21

library(raster)
library(rgdal)

# set rasteroptions
rasterOptions(progress = 'text')
#rasterOptions(maxmemory = 7000000000)
rasterOptions(memfrac = 0.8)

# psa zones
#psa<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="National_Predictive_Service_Areas_(PSA)_Boundaries")
#sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")

# new psa zones
psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/BurnPeriodTracker/shapes", layer="National_PSA_Current")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")

# list of variables
# pr, pet, rmax, rmin, sph, srad, th, tmmn, tmmx, vpd, vs
#   erc, bi
vars<-c("pr", "pet", "rmax", "rmin", "sph", "erc","bi","srad","tmmn", "tmmx", "vpd", "vs","th", "fm100","fm1000")

yr1<-2002
yr2<-2022

for(k in 12:12){
  yrStack=list()
  i<-1
    for(yr in yr1:yr2){
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
    writeRaster(temp, filename = paste0("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_",vars[k],"_",yr1,"_",yr2,".grd"),
                overwrite=TRUE, progress="text")
    # writeRaster(temp, filename = paste0("SW_PSA_gridmet_daily_",vars[k],"_",yr1,"_",yr2,".grd"),
    #             overwrite=TRUE, progress="text")
    rm(temp)
    gc()
}


# try to combine two files
library(raster)

# set rasteroptions
rasterOptions(progress = 'text')
#rasterOptions(maxmemory = 7000000000)
rasterOptions(memfrac = 0.8)

temp<-stack(stack("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_vs_1979_2001.grd"),
            stack("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_vs_2002_2022.grd"))

writeRaster(temp, filename = "/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_vs_1979_2022.grd",
            overwrite=TRUE, progress="text")



# test
# dates<-as.data.frame(seq(as.Date(paste0(1979,"-01-01"),format="%Y-%m-%d"),as.Date(paste0(2020,"-12-31"),format="%Y-%m-%d"),1))

 #test<-stack(paste0("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_",vars[k],"_1979_2020.grd"))