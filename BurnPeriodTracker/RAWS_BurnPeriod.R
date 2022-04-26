# RH/Burn Period analysis using RAWS
# MAC 06/17/21
# looking here for data https://www.nwcg.gov/publications/pms437/weather/data-resources
# https://fam.nwcg.gov/fam-web/weatherfirecd/state_data.htm
# https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm


library(rgdal)
library(tidyr)
library(dplyr)
library(stringr)
# library(raster)
# library(snow)
# library(tidyr)
# library(dplyr)

# set rasteroptions
#rasterOptions(progress = 'text')

# psa zones
psa<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="National_Predictive_Service_Areas_(PSA)_Boundaries")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")

# key raws sites
raws<-rgdal::readOGR(dsn="~/RProjects/FireClimate/monsoonClimo/shapes", layer="key_raws")
sw_raws<-subset(raws, GACC=="Southwest")
sw_rawsDF<-sw_raws@data
# station number as character
sw_rawsDF$STATION_NU<-as.character(sw_rawsDF$STATION_NU)
sw_rawsDF$STATION_NU<-str_pad(sw_rawsDF$STATION_NU, 6, pad = "0")

# get all data and add into dataframe
allRAWS = list()

for(i in 1:nrow(sw_rawsDF)){

    #URL<-"https://fam.nwcg.gov/fam-web/weatherfirecd/data/az/wx020503.fw13"
    URL<-paste0("https://fam.nwcg.gov/fam-web/weatherfirecd/data/",tolower(sw_rawsDF$STATE_ABBR[i]),"/wx",sw_rawsDF$STATION_NU[i],".fw13")
    
    tryCatch({
        stationData<-read.fwf(URL, widths = c(3,6,8,4,1,1,3,3,3,3,2,3,3,3,3,2,5,1,2,2,1,1,1))
    
    colnames(stationData)<-c("W13",
                             "StationNum",
                             "Date",
                             "Time",
                             "ObType",
                             "WxCode",
                             "Temp",
                             "Moisture",
                             "WDir",
                             "ws",
                             "10-fm",
                             "MaxTemp",
                             "MinTemp",
                             "MaxRH",
                             "MinRH",
                             "PrecipDur",
                             "Precip",
                             "Wet",
                             "HerbGreen",
                             "ShrubGreen",
                             "MoistureCode",
                             "MeasureCode",
                             "SeasCode")
    
    stationData<-tidyr::separate(stationData, Date, c("year","month","day"), sep=c(4,6))
    
    # change value types 
    cols = c(3,4,5);    
    stationData[,cols] = apply(stationData[,cols], 2, function(x) as.numeric(as.character(x)));
    
    # use only RAWS obs
    stationData<-subset(stationData, ObType=="R")
    
    # count of burn hours
    burnHRS<-  stationData %>%
                group_by(year, month, day) %>%
                  summarize(n_hours = n(),
                            rh_lt_20 = sum(Moisture <= 20, na.rm = TRUE),
                            minRH = min(MinRH, na.rm = TRUE),
                            StationNum = first(StationNum))
    # add in date field
    burnHRS$date<-as.Date(paste0(as.numeric(burnHRS$month),"-",as.numeric(burnHRS$day),"-",as.numeric(burnHRS$year)), format="%m-%d-%Y")
    
    # thin out days with fewer than 21 hours ~ 10% missing and create complete dataframe
    burnHRS<-subset(burnHRS, n_hours>=21)
    burnHRS<-burnHRS %>% complete(date = seq.Date(min(date), max(date), by="day"))
    
    # add in station info
    burnHRS$LATITUDE<-sw_rawsDF$LATITUDE[i]
    burnHRS$LONGITUDE<-sw_rawsDF$LONGITUDE[i]
    burnHRS$STA_NAME<-sw_rawsDF$STA_NAME[i]
    burnHRS$PSA_NAME<-sw_rawsDF$PSA_NAME[i]
    burnHRS$StationNum<-sw_rawsDF$STATION_NU[i]
    
    # add in doy
    burnHRS$doy<-as.numeric(format(burnHRS$date, "%j"))
    burnHRS$day<-as.numeric(format(burnHRS$date, "%d"))

    # add df to list
    allRAWS[[i]] <- burnHRS
    print(paste(i, sw_rawsDF$STA_NAME[i]))
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# drop empty entries in list
allRAWSdf = do.call(rbind, allRAWS)

save(allRAWS,allRAWSdf, file = "/home/crimmins/RProjects/FireClimate/monsoonClimo/allRAWS.RData")


# plot some data
library(ggplot2)

ggplot(allRAWSdf, aes(doy, rh_lt_20, group=doy))+
    geom_boxplot(outlier.shape = NA)+
    xlim(150,250)+
    ylim(0,24)+
    ylab("burning hours")+
    xlab("day of year")+
    facet_wrap(PSA_NAME~STA_NAME)

# find stations with longest, complete records
test <-  allRAWSdf %>%
                group_by(STA_NAME) %>%
                summarize(numObs = n(),
                          minYr = min(year, na.rm = TRUE),
                          maxYr = max(year, na.rm = TRUE))




