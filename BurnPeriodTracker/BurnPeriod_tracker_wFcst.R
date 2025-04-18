# RAWS Burn Period Daily Tracker with NDFD forecast data
# adapted from RAWS_BurnPeriod_CEFA.R, BurnPeriod_tracker.R
# MAC 5/26/22

ptm <- proc.time()

library(rgdal)
library(tidyr)
library(dplyr)
library(stringr)
library(XML)
library(RCurl)
library(ggplot2)
library(scales)
library(magick)
library(RColorBrewer)

# DEAL WITH PANDOC ERROR
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

# ggplot inset data
states <- map_data("state")

#####
# load spatial data
# psa zones
psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/FireClimate/monsoonClimo/shapes", layer="National_Predictive_Service_Areas_(PSA)_Boundaries")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")
# get psa centroids for factor order
sw_psaDF<- cbind.data.frame(sw_psa, rgeos::gCentroid(sw_psa,byid=TRUE))


# key raws sites https://fsapps.nwcg.gov/psp/npsg/forecast/home/downloads
raws<-rgdal::readOGR(dsn="/home/crimmins/RProjects/FireClimate/monsoonClimo/shapes", layer="key_raws")
sw_raws<-subset(raws, GACC=="Southwest")
sw_rawsDF<-sw_raws@data
# delete duplicated station entries
sw_rawsDF<-sw_rawsDF[!duplicated(sw_rawsDF[ , c("STA_NAME")]),]

# station number as character
sw_rawsDF$STATION_NU<-as.character(sw_rawsDF$STATION_NU)
sw_rawsDF$STATION_NU<-str_pad(sw_rawsDF$STATION_NU, 6, pad = "0")
#####

#####
# load processed RAWS data from RAWS_BurnPeriod_CEFA.R
# load("~/RProjects/FireClimate/monsoonClimo/allRAWScomb_lists.RData")
# #####
# 
# #####
# # calculate burning hours
# burnList<-list()
# 
# for(i in 1:length(allRAWS_comb)){
#   # count of burn hours
#   burnHRS<-  allRAWS_comb[[i]] %>%
#     group_by(year, month, day) %>%
#     summarize(n_hours = n(),
#               rh_lt_20 = sum(Moisture <= 20, na.rm = TRUE),
#               minRH = min(as.numeric(as.character(MinRH)), na.rm = TRUE),
#               maxRH = max(as.numeric(as.character(MaxRH)), na.rm = TRUE),
#               StationNum = first(StationNum))
#   # add in date field
#   burnHRS$date<-as.Date(paste0(as.numeric(burnHRS$month),"-",as.numeric(burnHRS$day),"-",as.numeric(burnHRS$year)), format="%m-%d-%Y")
#   
#   # thin out days with fewer than 21 hours ~ 10% missing and create complete dataframe
#   #burnHRS<-tidyr::complete(burnHRS, date = seq.Date(min(date), max(date), by="day"), fill = list())
#   burnHRS<-subset(burnHRS, n_hours>=21)
#   # complete dates
#   dates<-  as.data.frame(seq.Date(min(burnHRS$date), max(burnHRS$date), by="day"))
#   colnames(dates)<-"date"
#   # complete list    
#   burnHRS<-merge(burnHRS, dates, by="date", all.y=TRUE)
#   
#   # add in station info
#   burnHRS$LATITUDE<-sw_rawsDF$LATITUDE[i]
#   burnHRS$LONGITUDE<-sw_rawsDF$LONGITUDE[i]
#   burnHRS$STA_NAME<-sw_rawsDF$STA_NAME[i]
#   burnHRS$PSA_NAME<-sw_rawsDF$PSA_NAME[i]
#   burnHRS$StationNum<-sw_rawsDF$STATION_NU[i]
#   
#   # add in doy
#   burnHRS$doy<-as.numeric(format(burnHRS$date, "%j"))
#   burnHRS$day<-as.numeric(format(burnHRS$date, "%d"))
#   burnHRS$year<-as.numeric(format(burnHRS$date, "%Y"))
#   burnHRS$month<-as.numeric(format(burnHRS$date, "%m"))
#   
#   burnList[[i]]<-burnHRS
# }
# 
# rm(allRAWS_CEFA,allRAWS_comb,allRAWS_FAMWEB, burnHRS, dates)
# save(burnList, file = "/home/crimmins/RProjects/FireClimate/burnList.RData")
# #####

# load in RAWS data
load("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/burnList.RData")


#####
# loop through list and make plots
for(i in 1:length(burnList)){
    # develop daily climo for station in list
    temp<-burnList[[i]]
    
    # build plot label
    plotTitle<-paste0(sub("_", " ", temp$STA_NAME[1]), " Daily Burn Period Tracker (Climatology period of record: ",min(temp$year),"-",max(temp$year),")")
    
    dayQuantiles<- temp %>% group_by(doy) %>% summarise(
                     q05 = quantile(rh_lt_20,0.05,na.rm='TRUE'),
                     q50 = quantile(rh_lt_20,0.50,na.rm='TRUE'),
                     q95 = quantile(rh_lt_20,0.95,na.rm='TRUE'),
                     min = min(rh_lt_20,na.rm='TRUE'),
                     max = max(rh_lt_20,na.rm='TRUE'),
                     avg = mean(rh_lt_20,na.rm='TRUE'),
                     n = n())
    
    # moving average
    dayQuantiles$rollAvg<-zoo::rollmean(dayQuantiles$avg,14,fill=NA, align = 'center')
    # pad NAs
    dayQuantiles<-tidyr::fill(dayQuantiles, rollAvg, .direction = "downup")
    
    # get recent RAWS data
    url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-",format(Sys.time(), "%Y"),"&end=31-Dec-",format(Sys.time(), "%Y"),"&time=&sort=&ndays=")
    # past year
    #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
    xData <- getURL(url)
    xmldoc <- xmlParse(xData)
    currYear <- xmlToDataFrame(xData)
    # if no data, skip
    if(nrow(currYear)!=0){
        # correct date field
        currYear$date<-as.Date(currYear$obs_dt,format="%m/%d/%Y")
        currYear$doy<-format(currYear$date,"%j")
        # get current Year numeric
        yr<-((format(currYear$date[1],"%Y")))
        
        # current year burn hours
        currBurnHRS<-  currYear %>%
          group_by(date) %>%
          summarize(n_hours = n(),
                    rh_lt_20 = sum(as.numeric(rh) <= 20, na.rm = TRUE),
                    minRH = min(as.numeric((rh_min)), na.rm = TRUE),
                    maxRH = max(as.numeric((rh_max)), na.rm = TRUE))
        currBurnHRS$doy<-format(currBurnHRS$date,"%j")
        currBurnHRS$doy<-as.numeric(currBurnHRS$doy)
        # set missing days to NA
        currBurnHRS$rh_lt_20<-ifelse(currBurnHRS$n_hours<22,NA,currBurnHRS$rh_lt_20)
        
        # add in dummy dates
        dumYr<-as.numeric(format(Sys.Date(),"%Y"))
        dayQuantiles$date<-as.Date(paste0(dumYr,dayQuantiles$doy),format="%Y %j")
        #currBurnHRS$date<-as.Date(paste0(2016,currBurnHRS$doy),format="%Y %j")
        currBurnHRS$date<-as.Date(paste0(dumYr,format(currBurnHRS$date,"%m"),
                                              format(currBurnHRS$date,"%d")),format="%Y%m%d")
        #temp$dummyDate<-as.Date(paste0(2016,temp$doy),format="%Y %j")
        currBurnHRS$roll_rh_hrs<-zoo::rollmean(currBurnHRS$rh_lt_20,10,fill=NA, align = 'right')
        
        # better names for Plotly labels
        colnames(dayQuantiles)[c(2,4,9)]<-c("q5th_percentile","q90th_percentile","median")
        colnames(currBurnHRS)[c(3,7)]<-c("Burn_hours","avg_10days")
        
        #####
        # NDFD Burn Period forecast
        # url <- "http://forecast.weather.gov/MapClick.php?lat=32&lon=-112&FcstType=digitalDWML"
        url <- paste0("http://forecast.weather.gov/MapClick.php?lat=",temp[1,c("LATITUDE")],"&lon=",temp[1,c("LONGITUDE")],"&FcstType=digitalDWML")
        download.file(url=url,"/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/url.txt" )
        data <- XML::xmlParse("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/url.txt")
        
        xml_data <- XML::xmlToList(data)
        # get location info
        location <- as.list(xml_data[["data"]][["location"]][["point"]])
        # get times
        start_time <- as.data.frame(as.character(unlist(xml_data[["data"]][["time-layout"]][
          names(xml_data[["data"]][["time-layout"]]) == "start-valid-time"])))
        colnames(start_time)<-"date_time"
        start_time$date_time<-as.character(start_time$date_time)
        start_time$date_time<-lubridate::ymd_hms(substring(start_time$date_time, 1,19))
        start_time$doy<-as.numeric(format(start_time$date_time,"%j"))
        
        # get RH
        rhum<-as.data.frame(as.numeric(unlist(xml_data[["data"]][["parameters"]][["humidity"]])))
        colnames(rhum)<-"rh_perc"
        rhum<-as.data.frame(rhum[1:nrow(start_time),1])  
        colnames(rhum)<-"rh_perc"
        # add in times
        rhum<-cbind.data.frame(start_time,rhum)  
        rhum$bhour<-ifelse(rhum$rh_perc<=20, 1, 0)
        
        fcst_bhrs<- rhum %>% group_by(doy) %>% 
          summarise(Burn_hours_forecast=sum(bhour),
                    nhrs =n())
        fcst_bhrs$date<-as.Date(paste0(fcst_bhrs$doy,"-",format(Sys.Date(),"%Y")),format="%j-%Y")
        fcst_bhrs<-subset(fcst_bhrs, nhrs>=22)
        
        #####
        
        
        # make the plot
        barWidth<-1
        p1<-ggplot(dayQuantiles,aes(date,median))+
          theme_bw()+
          #theme(plot.background = element_blank(),
          #      panel.grid.minor = element_blank(),
          #      panel.grid.major = element_blank(),
          #      panel.border = element_blank(),
          #      panel.background = element_blank()) +
          geom_line(colour='grey',size=0.5)+
          geom_linerange(dayQuantiles, mapping=aes(x=date, ymin=q5th_percentile, ymax=q90th_percentile), colour = "grey83",alpha=0.4, size=barWidth, show.legend = NA)
        p<-p1 + geom_line(data=currBurnHRS,aes(date,Burn_hours, color=yr), size=0.5) +
          scale_colour_manual(values=c("red"),name='Year')+
          theme(legend.position=c(0.92,0.75),
                legend.title=element_blank(),
                legend.background = element_rect(fill=alpha('white', 0)))+
          #scale_y_discrete(name ="Burn period (hrs)", 
          #                 limits=c(0,4,8,12,16,20,24))+
          scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
          
          scale_x_date(labels = date_format("%b"), date_breaks = "1 month")+
          ggtitle(plotTitle)
        
        
        p<-p + geom_line(data=currBurnHRS,aes(date,avg_10days), size=0.5,linetype = "dashed", color="darkorange4") +
          #scale_colour_manual(values=c("red"),name='Year')+
          theme(legend.position=c(0.92,0.75),
                legend.title=element_blank(),
                legend.background = element_rect(fill=alpha('white', 0)))+
          #scale_y_discrete(name ="Burn period (hrs)", 
          #                 limits=c(0,4,8,12,16,20,24))+
          scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
          
          scale_x_date(labels = date_format("%b"), date_breaks = "1 month")+
          ggtitle(plotTitle)
        
        # add in forecast line
        p<-p + geom_line(data=fcst_bhrs, aes(date,Burn_hours_forecast), size=0.5, color="forestgreen")
        
        
        # interactive plot
        pLy<-plotly::ggplotly(p)
        htmlwidgets::saveWidget(pLy, paste0("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnTrackerPlots/plotly/",temp$STA_NAME[1],"_BurnPeriod.html"))
        
        # INSET MAP - OPTIONAL
        # point<-as.data.frame(t((out$meta$ll)))
        # # inset map:
        # zoomLev<-5
        sw_psa_df<-fortify(sw_psa)
        stationLatLon<-temp[1,c("LATITUDE","LONGITUDE")]
        insetmap<-ggplot() +
          geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
          geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey", alpha=0.8)  + # get the state border back on top
          #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
          coord_fixed(xlim=c(-115, -99.75), ylim=c(28.75, 37.5), ratio = 1) +
          geom_point(data = stationLatLon, aes(x = LONGITUDE, y = LATITUDE), size=0.75, color='red')+
          theme_bw(base_size=5)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
        
        g <- ggplotGrob(insetmap)
        p<-p + annotation_custom(grob = g, xmin = as.Date(paste0(dumYr,"-11-01")), xmax = Inf, ymin = 20, ymax = Inf)+
          labs(caption=paste0("Updated: ",format(Sys.time(), "%Y-%m-%d")," (current through ",format(currBurnHRS$date[max(which(is.na(currBurnHRS$Burn_hours)==TRUE))-1], "%m-%d"),")",
                              "\nBurn Period is total hours/day with RH<20%\n10-day moving avg(dashed line); 7-day NOAA NDFD forecast(green line)\nClimatology represents daily median and range\n of values between 5th and 95th percentiles\nRAWS Data Source: famprod.nwcg.gov & cefa.dri.edu"))
          
        
        # write out file
        png(paste0("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnTrackerPlots/",temp$STA_NAME[1],"_BurnPeriod.png"), width = 12, height = 6, units = "in", res = 300L)
        #grid.newpage()
        print(p, newpage = FALSE)
        dev.off()
        
        # add logos
        # Call back the plot
        plot <- image_read(paste0("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnTrackerPlots/",temp$STA_NAME[1],"_BurnPeriod.png"))
        # And bring in a logo
        logo_raw <- image_read("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/CLIMAS_UACOOP_SWCC_horiz.png") 
        logo <- image_resize(logo_raw, geometry_size_percent(width=70,height = 70))
        # Stack them on top of each other
        #final_plot <- image_append((c(plot, logo)), stack = TRUE)
        #final_plot <- image_mosaic((c(plot, logo)))
        final_plot <- image_composite(plot, logo, offset = "+210+1560")
        # And overwrite the plot without a logo
        image_write(final_plot, paste0("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnTrackerPlots/",temp$STA_NAME[1],"_BurnPeriod.png"))
        # ----
        
        # # # plotly interactive of all years
        # temp2<-temp
        # temp2$dummyDate<-as.Date(paste0(2016,temp$doy),format="%Y %j")
        # colnames(currBurnHRS)[1]<-"dummyDate"
        # currBurnHRS$year<-as.numeric(yr)
        # temp2<-rbind(currBurnHRS,temp2[,c("dummyDate","n_hours","rh_lt_20","minRH","maxRH","doy","year")])
        # colnames(temp2)<-c("Date","n_hours","Burn Hours","minRH","maxRH","Day of year","Year")
        # temp2$Date<-format(temp2$Date, "%b-%d")
        # temp2$Year<-as.factor(temp2$Year)
        # 
        # # quantiles
        # temp3<-dayQuantiles[,c("doy","q05","q95","rollAvg","date")]
        #   colnames(temp3)<-c("Day of year","5th %tile","95th %tile","Avg","Date")
        #   temp3$Date<-format(temp3$Date, "%b-%d")
        # 
        # # color ramp
        #   colourCount = length(unique(temp2$Year))
        #   getPalette = colorRampPalette(brewer.pal(9, "Set1"))
        # 
        # barWidth<-1
        # p1<-ggplot(temp3,aes(`Day of year`,`Avg`))+
        #   theme_bw()+
        #   #theme(plot.background = element_blank(),
        #   #      panel.grid.minor = element_blank(),
        #   #      panel.grid.major = element_blank(),
        #   #      panel.border = element_blank(),
        #   #      panel.background = element_blank()) +
        #   geom_line(colour='grey',size=0.5)+
        #   geom_linerange(temp3, mapping=aes(x=`Day of year`, ymin=`5th %tile`, ymax=`95th %tile`, group=1, text=Date), colour = "grey83",alpha=0.4, size=barWidth, show.legend = NA)
        # 
        #  pLy<-p1+geom_line(data=temp2,aes(`Day of year`,`Burn Hours`, color=Year, group=1,text=Date), size=0.5)+
        #    scale_x_continuous(breaks=c(1,32,61,93,122,153,183,214,245,275,306,336),labels=temp3$Date[c(1,32,61,93,122,153,183,214,245,275,306,336)])+
        #    scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
        #    scale_color_manual(name="Year",values = getPalette(colourCount))+
        #    xlab("Day of Year")+
        #    ylab("Burn period (hrs)")+
        #    ggtitle(plotTitle)+
        #    theme_bw()
        #  pLy<-plotly::ggplotly(pLy)
        #  htmlwidgets::saveWidget(pLy, paste0("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnTrackerPlots/plotly/",temp$STA_NAME[1],"_BurnPeriod.html"))
        # #######
        print(temp$STA_NAME[1])
    }
    else{
      
    }
}

proc.time() - ptm

# 
# #####
# create map page
library(leaflet)
library(knitr)
library(rmarkdown)
library(rmdformats)
library(DT)
library(leaflet.esri)

# DEAL WITH PANDOC ERROR
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

# remove ROOSEVELT - empty station
sw_rawsDF<-subset(sw_rawsDF, STA_NAME!="ROOSEVELT")

# create links
sw_rawsDF$fileName<-paste0(sw_rawsDF$STA_NAME,"_BurnPeriod.png")
# plotly links
sw_rawsDF$plotlyName<-paste0("./plotly/",sw_rawsDF$STA_NAME,"_BurnPeriod.html")
# create data table
pageTable<-sw_rawsDF[,c(2,3,1,5,6,7,10,11,12,13,14)]
pageTable$dataLink<-paste0('https://www.weather.gov/wrh/timeseries?site=',pageTable$STATION_ID)


# image link for table
pageTable$imageLinks<-paste0('<a href="',sw_rawsDF$fileName,'"><img alt="Thumb" src="',sw_rawsDF$fileName,'"width=150" height="70"></a>')
# data link <a href="https://www.w3schools.com">Visit W3Schools</a>
pageTable$STA_NAME<-paste0('<a href="',pageTable$dataLink,'">',pageTable$STA_NAME,'</a>')
# plotly hyperlink
pageTable$Interactive<-paste0('<a href="',sw_rawsDF$plotlyName,'">',sw_rawsDF$STA_NAME,'</a>')


# pop up labels
stnLabs <- lapply(seq(nrow(pageTable)), function(i) {
  paste0( '<p> <b>', pageTable$imageLinks[i], '</b></p>',
          '<p>',pageTable$STA_NAME[i], '</p>')
})

# adjust table
pageTable<-pageTable[,c("STA_NAME","STATION_ID","STATION_NU","LATITUDE","LONGITUDE","ELEV_FEET","PSA_NAME","imageLinks","Interactive")]
colnames(pageTable)<-c("Name","ID","Number","Latitude","Longitude","Elevation(ft)","PSA Name","Tracker","Interactive")


# create Website with markdown ----
render(paste0('/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnPeriod_Markdown.Rmd'), output_file='index.html',
       output_dir='/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnTrackerPlots/', clean=TRUE)

# #####

source('/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/pushNotify.R')