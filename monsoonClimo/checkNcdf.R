
library(ncdf4)

yr="1979"
vars<-c("pr", "pet", "rmax", "rmin", "sph", "erc","bi","srad","tmmn", "tmmx", "vpd", "vs","th", "fm100","fm1000")


ncin <- nc_open(paste0("/scratch/crimmins/gridmet/update_Aug2019/",vars[7],"_",yr,".nc"))
print(ncin)