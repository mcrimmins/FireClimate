# working with Predictive Service Area Zones
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

# key raws sites
#raws<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="key_raws")
#sw_raws<-subset(raws, GACC=="Southwest")

# common date list for POR
#dates<-seq(as.Date(paste0(1979,"-01-01"),format="%Y-%m-%d"),as.Date(paste0(2020,"-12-31"),format="%Y-%m-%d"),1)

# load datasets
# gridded: NARR, gridmet, PRISM
# stations: RAWS, mesowest,...

#vars<-c("pr", "pet", "rmax", "rmin", "sph", "erc","bi","srad","tmmn", "tmmx", "vpd", "vs","th", "fm100","fm1000")
#vars<-c("rmin","erc","bi","fm100")
# vars<-c("pr", "vs","rmax","vpd")
# extStack=list()
# 
# for(k in 1:length(vars)){
#   print(vars[k])
#   gridmet<-stack(paste0("/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA/SW_PSA_gridmet_daily_",vars[k],"_1979_2020.grd"))
#   #gridTS <- (t(extract(gridmet, sw_psa, fun=mean, df=TRUE, na.rm=TRUE)))
#     # parallel extract
#     ptm <- proc.time()
#     # Extract
#     beginCluster(n=7)
#       gridTS<-as.data.frame(t(raster::extract(gridmet, sw_psa, fun=mean, df=TRUE, na.rm=TRUE)))
#     endCluster()
#     proc.time() - ptm
#   gridTS<-gridTS[2:nrow(gridTS),]
#   gridTS$var<-vars[k]
#   gridTS$date<-dates
#   extStack[[k]]<- gridTS
# }
#   
# save(extStack, file = "/home/crimmins/RProjects/FireClimate/monsoonClimo/extStack_working2.RData")

# vars<-c("rmin","erc","bi","fm100") # extStack_working.RData
# vars<-c("pr", "vs","rmax","vpd")   # extStack_working2.RData
# vars<-c("pet", "fm1000","tmmn","tmmx")     # extStack_working3.RData
# vars<-c("sph", "srad","th")     # extStack_working4.RData

# load save dataframes
#load("~/RProjects/FireClimate/monsoonClimo/extStack_working.RData")
load("~/RProjects/FireClimate/monsoonClimo/extStack_working.RData")

# get dataframe
gridTS<-extStack[[3]] 
# clean up colnames
colnames(gridTS)<-c(as.character(sw_psa$PSANAME),"var","date")
varName<-unique(gridTS$var)

# get date fields
gridTS$doy<-as.numeric(format(gridTS$date,"%j"))
gridTS$month<-as.numeric(format(gridTS$date,"%m"))
gridTS$year<-as.numeric(format(gridTS$date,"%Y"))

# summaries by PSA
meltTS<-gather(gridTS,"PSA","var",-date,-doy,-year,-month,-var)


# group summary by month
summTS<- meltTS %>% group_by(month,PSA) %>%
                    summarise(avgVar=mean(var, na.rm=TRUE))

    ### Choropleth maps https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html
    library(broom)
    spdf_fortified <- tidy(sw_psa, region = "PSANAME")
    
    # Now I can plot this shape easily as described before:
    library(ggplot2)
    # ggplot() +
    #   geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
    #   theme_void() +
    #   coord_map()
    
    # Make the merge
    spdf_fortified = spdf_fortified %>%
      left_join(. , summTS, by=c("id"="PSA"))

    ggplot() +
      geom_polygon(data = spdf_fortified, aes(fill = avgVar, x = long, y = lat, group = group)) +
      facet_wrap(.~month)+
      #facet_wrap(.~doy)+
      scale_fill_viridis_c(direction=1, name=varName)+
      theme_void() +
      coord_map()+
      ggtitle(paste0("Avg Monthly ",varName," by PSA (GridMet, 1979-2020)"))

# group summary by doy
    doyDate<-as.data.frame(seq(as.Date("2000/1/1"), as.Date("2000/12/31"), "days"))
      colnames(doyDate)<-"date"
      doyDate$doy<-as.numeric(format(doyDate$date,"%j"))
      doyDate$label<-factor(format(doyDate$date, "%m-%d"))
    
    summTS<- meltTS %>% group_by(doy,PSA) %>%
                        summarise(avgVar=mean(var, na.rm=TRUE))
    summTS<-merge(summTS,doyDate,by="doy")
    summTS<-subset(summTS, doy>=153 & doy <=213)
    
    # create choropleth data
    spdf_fortified <- tidy(sw_psa, region = "PSANAME")
    # Make the merge
    spdf_fortified = spdf_fortified %>%
      left_join(. , summTS, by=c("id"="PSA"))
    
    ggplot() +
      geom_polygon(data = spdf_fortified, aes(fill = avgVar, x = long, y = lat, group = group)) +
      #facet_wrap(.~month)+
      facet_wrap(.~label)+
      scale_fill_viridis_c(direction=1, name=varName)+
      theme_void() +
      coord_map()+
      ggtitle(paste0("Avg Daily ",varName," by PSA (GridMet, 1979-2020)"))
    
# change in var between days
    summTS1<-subset(summTS, doy==166)
    summTS2<-subset(summTS, doy==213)
    summTS2$diff<-summTS2$avgVar-summTS1$avgVar
    
    # create choropleth data
    spdf_fortified <- tidy(sw_psa, region = "PSANAME")
    # Make the merge
    spdf_fortified = spdf_fortified %>%
      left_join(. , summTS2, by=c("id"="PSA"))
    
    ggplot() +
      geom_polygon(data = spdf_fortified, aes(fill = diff, x = long, y = lat, group = group)) +
      #facet_wrap(.~month)+
      scale_fill_viridis_c(direction=1, name=varName)+
      theme_void() +
      coord_map()+
      ggtitle(paste0("Avg Daily Diff for ",varName," by PSA from Jun 15 to Jul 31"))
    
# trends by month?    
    
    
# time series plots
    load("~/RProjects/FireClimate/monsoonClimo/extStack_working.RData")
    #load("~/RProjects/FireClimate/monsoonClimo/extStack_working2.RData")
    
    # get dataframe
    gridTS<-extStack[[3]] 
    # clean up colnames
    colnames(gridTS)<-c(as.character(sw_psa$PSANAME),"var","date")
    varName<-unique(gridTS$var)
    
    # get date fields
    gridTS$doy<-as.numeric(format(gridTS$date,"%j"))
    gridTS$month<-as.numeric(format(gridTS$date,"%m"))
    gridTS$year<-as.numeric(format(gridTS$date,"%Y"))
  
    # select PSA
    psaTS<-gridTS[,c(18,19,20,21,22,8)]
    colnames(psaTS)[ncol(psaTS)]<-"value"
    psaTS$dateLab<-as.Date(paste0(format(psaTS$date,"%m-%d"),"-2000"),format="%m-%d-%Y")
      
    # summary stats
    climoDOY<- psaTS %>% group_by(doy) %>%
                          summarise(q10 = quantile(value,0.1,na.rm='TRUE'),
                           q50 = quantile(value,0.50,na.rm='TRUE'),
                           q90 = quantile(value,0.9,na.rm='TRUE'),
                           min = min(value,na.rm='TRUE'),
                           max = max(value,na.rm='TRUE'),
                           avg = mean(value,na.rm='TRUE'),
                           dateLab = first(dateLab))
    
    library(RColorBrewer)
    colourCount = length(unique(psaTS$year))
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))  
    
   p<-ggplot(psaTS,aes(dateLab,value, color=as.factor(year)))+
      geom_line()+
      scale_color_manual(name="Year",values = getPalette(colourCount))+
      theme_bw()
   p<-p+geom_line(data=climoDOY, aes(dateLab,q50), color="black")+
        geom_line(data=climoDOY, aes(dateLab,q10), color="black", linetype="dotted")+
        geom_line(data=climoDOY, aes(dateLab,q90), color="black", linetype="dotted")+     
        scale_x_date(date_labels = "%b %d")+
        xlab("Day of Year")+
        ylab("Burning Index")+
        ggtitle("Daily BI for PSA Southeast AZ")
   
   
   library(plotly)
   ggplotly(p)
       
#####    median by doy/PSA 
   
   load("~/RProjects/FireClimate/monsoonClimo/extStack_working.RData")
   #load("~/RProjects/FireClimate/monsoonClimo/extStack_working2.RData")
   
   # get dataframe
   gridTS<-extStack[[4]] 
   # clean up colnames
   colnames(gridTS)<-c(as.character(sw_psa$PSANAME),"var","date")
   varName<-unique(gridTS$var)
   
   # get date fields
   gridTS$doy<-as.numeric(format(gridTS$date,"%j"))
   gridTS$month<-as.numeric(format(gridTS$date,"%m"))
   gridTS$year<-as.numeric(format(gridTS$date,"%Y"))
   
   # select PSA
   psaTS<-gridTS
   psaTS$dateLab<-as.Date(paste0(format(psaTS$date,"%m-%d"),"-2000"),format="%m-%d-%Y")
   psaTS<-gather(psaTS,"PSA","value",-date,-doy,-year,-month,-var, -dateLab)
   
   # summary stats
   climoDOY<- psaTS %>% group_by(doy,PSA) %>%
     summarise(
               q50 = quantile(value,0.50,na.rm='TRUE'),  
               dateLab = first(dateLab))
   
   smClimoDOY<-climoDOY %>% group_by(PSA) %>%
     mutate(q50sm = zoo::rollmean(q50, k = 10, fill = NA))
   
   library(RColorBrewer)
   colourCount = length(unique(climoDOY$PSA))
   getPalette = colorRampPalette(brewer.pal(9, "Set1"))  
   
   p<-ggplot(smClimoDOY,aes(dateLab,q50sm, color=as.factor(PSA)))+
     geom_line()+
     scale_color_manual(name="PSA",values = getPalette(colourCount))+
     scale_x_date(date_labels = "%b %d")+
     ylab("median FM100")+
     xlab("Day of year")+
     ggtitle("Median FM100 by day/PSA (10-day smooth)")+
     theme_bw()
 
   library(plotly)
   ggplotly(p)
   
   
# quantile regression of monthly fire wx/clim indices
   library(quantreg)
   
   load("~/RProjects/FireClimate/monsoonClimo/extStack_working.RData")
   #load("~/RProjects/FireClimate/monsoonClimo/extStack_working2.RData")
   
   # get dataframe
   gridTS<-extStack[[1]] 
   # clean up colnames
   colnames(gridTS)<-c(as.character(sw_psa$PSANAME),"var","date")
   varName<-unique(gridTS$var)
   
   # get date fields
   gridTS$doy<-as.numeric(format(gridTS$date,"%j"))
   gridTS$month<-as.numeric(format(gridTS$date,"%m"))
   gridTS$year<-as.numeric(format(gridTS$date,"%Y"))
   
   # select PSA
   psaTS<-gridTS[,c(18,19,20,21,22,8)]
   colnames(psaTS)[ncol(psaTS)]<-"value"
   psaTS$dateLab<-as.Date(paste0(format(psaTS$date,"%m-%d"),"-2000"),format="%m-%d-%Y")
   
   psaTS<-subset(psaTS, month==5)
   plot(psaTS$year,psaTS$value,xlab="Year", ylab="BI", main="July BI PSA-Southeast AZ (GridMet 1979-2020)")
   taus <- c(.05,.1,.25,.75,.90,.95)
   abline(rq((value)~(year),data=psaTS,tau=.5),col="blue")
   abline(lm((value)~(year),data=psaTS),lty = 3,col="red")
   for( i in 1:length(taus)){
      abline(rq((value)~(year),data=psaTS,tau=taus[i]),col="gray")
   }
   
   # ggplot quantile regression
   
   ggplot(psaTS, aes(year,value))+
     geom_point()+
     geom_quantile(quantiles=c(0.05,0.25,0.5,0.75,0.95))+
     facet_wrap(.~month)+
     ylab("Rh-min")+
     xlab("Year")+
     ggtitle("Daily Rh-min by month PSA-SE AZ (GridMet 1979-2020)")+
     theme_bw()
   
   
   # Sharples F index 
   # vars<-c("rmin","erc","bi","fm100") # extStack_working.RData
   # vars<-c("pr", "vs","rmax","vpd")   # extStack_working2.RData
   # vars<-c("pet", "fm1000","tmmn","tmmx")     # extStack_working3.RData
   # vars<-c("sph", "srad","th")     # extStack_working4.RData
   
   # rmin in % Daily Minimum Relative Humidity 
   load("~/RProjects/FireClimate/monsoonClimo/extStack_working.RData")
      # get dataframe
      rmin<-extStack[[1]] 
      # clean up colnames
      colnames(rmin)<-c(as.character(sw_psa$PSANAME),"var","date")
   # wind in m/s Daily Mean Wind Speed
   load("~/RProjects/FireClimate/monsoonClimo/extStack_working2.RData")
      # get dataframe
      wind<-extStack[[2]] 
      # convert from m/s to km/h
      wind[1:length(unique(sw_psa$PSANAME))]<-wind[1:length(unique(sw_psa$PSANAME))]*3.6
      # clean up colnames
      colnames(wind)<-c(as.character(sw_psa$PSANAME),"var","date")
   # tmmx in K Daily Maximum Temperature
   load("~/RProjects/FireClimate/monsoonClimo/extStack_working3.RData")
      # get dataframe
      tmmx<-extStack[[4]] 
      # convert from m/s to km/h
      tmmx[1:length(unique(sw_psa$PSANAME))]<-tmmx[1:length(unique(sw_psa$PSANAME))]-273.15
      # clean up colnames
      colnames(tmmx)<-c(as.character(sw_psa$PSANAME),"var","date")
      
   # calculate F index
   Findex<-(wind[1:length(unique(sw_psa$PSANAME))])/(10-0.25*(tmmx[1:length(unique(sw_psa$PSANAME))]-rmin[1:length(unique(sw_psa$PSANAME))]))
      Findex<-cbind.data.frame(Findex,rmin$date)
      colnames(Findex)[18]<-"date"
      Findex$var<-"F-index"
   
      # get date fields
      Findex$doy<-as.numeric(format(Findex$date,"%j"))
      Findex$month<-as.numeric(format(Findex$date,"%m"))
      Findex$year<-as.numeric(format(Findex$date,"%Y"))
      
      # select PSA
      psaTS<-Findex
      psaTS$dateLab<-as.Date(paste0(format(psaTS$date,"%m-%d"),"-2000"),format="%m-%d-%Y")
      psaTS<-gather(psaTS,"PSA","value",-date,-doy,-year,-month,-var, -dateLab)
      
      # summary stats
      climoDOY<- psaTS %>% group_by(doy,PSA) %>%
         summarise(
            q50 = quantile(value,0.50,na.rm='TRUE'),  
            dateLab = first(dateLab))
      
      smClimoDOY<-climoDOY %>% group_by(PSA) %>%
         mutate(q50sm = zoo::rollmean(q50, k = 10, fill = NA))
      
      library(RColorBrewer)
      colourCount = length(unique(climoDOY$PSA))
      getPalette = colorRampPalette(brewer.pal(9, "Set1"))  
      
      p<-ggplot(smClimoDOY,aes(dateLab,q50sm, color=as.factor(PSA)))+
         geom_line()+
         scale_color_manual(name="PSA",values = getPalette(colourCount))+
         scale_x_date(date_labels = "%b %d")+
         ylab("median F-index")+
         xlab("Day of year")+
         ggtitle("Median F-index by day/PSA (10-day smooth)")+
         theme_bw()
      
      library(plotly)
      ggplotly(p)
      
      
      
      