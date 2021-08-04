# RH/Burn Period analysis using RAWS
# MAC 06/17/21
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



# # get all data and add into dataframe
# allRAWS_FAMWEB = list()
# allRAWS_CEFA = list()
# 
# # fw13 colnames
# ##### 
# fw13cols<-c("W13",
#             "StationNum",
#             "Date",
#             "Time",
#             "ObType",
#             "WxCode",
#             "Temp",
#             "Moisture",
#             "WDir",
#             "ws",
#             "10-fm",
#             "MaxTemp",
#             "MinTemp",
#             "MaxRH",
#             "MinRH",
#             "PrecipDur",
#             "Precip",
#             "Wet",
#             "HerbGreen",
#             "ShrubGreen",
#             "MoistureCode",
#             "MeasureCode",
#             "SeasCode")
# #####
# 
# for(i in 1:nrow(sw_rawsDF)){
#   
#   # get RAWS from CEFA
#   #URL<-paste0("https://fam.nwcg.gov/fam-web/weatherfirecd/data/",tolower(sw_rawsDF$STATE_ABBR[i]),"/wx",sw_rawsDF$STATION_NU[i],".fw13")
#   URL<-paste0("https://cefa.dri.edu/raws/fw13/",sw_rawsDF$STATION_NU[i],".fw13")
#   tryCatch({
#     stationData<-read.fwf(URL, widths = c(3,6,8,4,1,1,3,3,3,3,2,3,3,3,3,2,5,1,2,2,1,1,1))
#       colnames(stationData)<-fw13cols
#     stationData<-tidyr::separate(stationData, Date, c("year","month","day"), sep=c(4,6))
#     
#     # change value types 
#     cols = c(3,4,5);    
#     stationData[,cols] = apply(stationData[,cols], 2, function(x) as.numeric(as.character(x)));
#     
#     # use only RAWS obs
#     stationData<-subset(stationData, ObType=="R")
#     # database code
#     stationData$database<-"CEFA"
#     # add df to list
#     allRAWS_CEFA[[i]] <- stationData
#     print(paste(i, sw_rawsDF$STA_NAME[i], "- CEFA"))
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#     
#   # get RAWS from Fam-web
#   URL<-paste0("https://fam.nwcg.gov/fam-web/weatherfirecd/data/",tolower(sw_rawsDF$STATE_ABBR[i]),"/wx",sw_rawsDF$STATION_NU[i],".fw13")
#   #URL<-paste0("https://cefa.dri.edu/raws/fw13/",sw_rawsDF$STATION_NU[i],".fw13")
#   tryCatch({
#     stationData<-read.fwf(URL, widths = c(3,6,8,4,1,1,3,3,3,3,2,3,3,3,3,2,5,1,2,2,1,1,1))
#     colnames(stationData)<-fw13cols
#     stationData<-tidyr::separate(stationData, Date, c("year","month","day"), sep=c(4,6))
#     
#     # change value types 
#     cols = c(3,4,5);    
#     stationData[,cols] = apply(stationData[,cols], 2, function(x) as.numeric(as.character(x)));
#     
#     # use only RAWS obs
#     stationData<-subset(stationData, ObType=="R")
#     # database code
#     stationData$database<-"FAMWEB"
#     # add df to list
#     allRAWS_FAMWEB[[i]] <- stationData
#     print(paste(i, sw_rawsDF$STA_NAME[i],"- FAMWEB"))
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
#   
# }  
#   
# allRAWS_comb = list()
# # combine databases
#   for(i in 1:length(allRAWS_CEFA)){
#    
#     if (is.null(allRAWS_CEFA[[i]])) {
#       allRAWS_comb[[i]]<-allRAWS_FAMWEB[[i]]
#     } else if (is.null(allRAWS_FAMWEB[[i]])) {
#       allRAWS_comb[[i]]<-allRAWS_CEFA[[i]]
#     } else
#       allRAWS_comb[[i]]<- rbind.data.frame(allRAWS_CEFA[[i]],
#                                         subset(allRAWS_FAMWEB[[i]],
#                                                year>max(allRAWS_CEFA[[i]]$year, na.rm=TRUE)))
#   }
# 
# 
# save(allRAWS_CEFA,allRAWS_FAMWEB, allRAWS_comb,
#      file = "/home/crimmins/RProjects/FireClimate/monsoonClimo/allRAWScomb_lists.RData")
# #####

# load in RAWS data
load("~/RProjects/FireClimate/monsoonClimo/allRAWScomb_lists.RData")

# calculate burning hours
#####
burnList<-list()

for(i in 1:length(allRAWS_comb)){
    # count of burn hours
    burnHRS<-  allRAWS_comb[[i]] %>%
      group_by(year, month, day) %>%
      summarize(n_hours = n(),
                rh_lt_20 = sum(Moisture <= 20, na.rm = TRUE),
                minRH = min(as.numeric(as.character(MinRH)), na.rm = TRUE),
                maxRH = max(as.numeric(as.character(MinRH)), na.rm = TRUE),
                StationNum = first(StationNum))
    # add in date field
    burnHRS$date<-as.Date(paste0(as.numeric(burnHRS$month),"-",as.numeric(burnHRS$day),"-",as.numeric(burnHRS$year)), format="%m-%d-%Y")
    
    # thin out days with fewer than 21 hours ~ 10% missing and create complete dataframe
    #burnHRS<-tidyr::complete(burnHRS, date = seq.Date(min(date), max(date), by="day"), fill = list())
    burnHRS<-subset(burnHRS, n_hours>=21)
    # complete dates
    dates<-  as.data.frame(seq.Date(min(burnHRS$date), max(burnHRS$date), by="day"))
    colnames(dates)<-"date"
    # complete list    
    burnHRS<-merge(burnHRS, dates, by="date", all.y=TRUE)
  
    # add in station info
    burnHRS$LATITUDE<-sw_rawsDF$LATITUDE[i]
    burnHRS$LONGITUDE<-sw_rawsDF$LONGITUDE[i]
    burnHRS$STA_NAME<-sw_rawsDF$STA_NAME[i]
    burnHRS$PSA_NAME<-sw_rawsDF$PSA_NAME[i]
    burnHRS$StationNum<-sw_rawsDF$STATION_NU[i]
    
    # add in doy
    burnHRS$doy<-as.numeric(format(burnHRS$date, "%j"))
    burnHRS$day<-as.numeric(format(burnHRS$date, "%d"))
    burnHRS$year<-as.numeric(format(burnHRS$date, "%Y"))
    burnHRS$month<-as.numeric(format(burnHRS$date, "%m"))
    
    
    burnList[[i]]<-burnHRS
}

#nrowsList<-lapply(burnList, nrow)

# combine into df
allBHRS <- do.call(rbind, burnList)
allBHRS$rhDiff<-allBHRS$maxRH-allBHRS$minRH
  allBHRS$STA_NAME<-as.character(allBHRS$STA_NAME)
  allBHRS$PSA_NAME<-as.character(allBHRS$PSA_NAME)

# find stations with longest, complete records
stationStats <-  allBHRS %>%
            group_by(STA_NAME) %>%
            summarize(numObs = n(),
                      minYr = min(date, na.rm = TRUE),
                      maxYr = max(date, na.rm = TRUE),
                      lat = first(LATITUDE),
                      lon = first(LONGITUDE))
stationStats$days<-stationStats$maxYr-stationStats$minYr
stationStats$numYrs<-as.numeric(format(stationStats$maxYr, "%Y"))-as.numeric(format(stationStats$minYr, "%Y"))
# subset to 15 yrs or more
stationStats<-subset(stationStats, numYrs>=15)
# map stations
  library(ggplot2)
  sw_psa_poly<-fortify(sw_psa)
    ggplot()+
      #geom_polygon(data = states, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
      geom_polygon(data = sw_psa_poly, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
      coord_fixed(1.3)+
      theme_bw()+
      geom_point(data = stationStats, aes(x = lon, y = lat), size=1)+
      geom_text(data = stationStats, aes(x = lon, y = lat, label=STA_NAME,), size = 3, color="red")

# thin out allBHRS
#allBHRS_thin <- allBHRS[allBHRS$STA_NAME %in% unique(stationStats$STA_NAME), ]
allBHRS_thin <- subset(allBHRS, STA_NAME %in% unique(stationStats$STA_NAME))
#allBHRS_thin$STA_NAME<-as.character(allBHRS_thin$STA_NAME)

test<-subset(allBHRS_thin, STA_NAME=="HEBER" & month==6)

# median burnHRS by month
burnHRSmo<-  allBHRS_thin %>%
              group_by(month, STA_NAME) %>%
              summarize(n_obs = n(),
                        medianHRS = median(rh_lt_20, na.rm = TRUE),
                        meanHRS   = mean(rh_lt_20, na.rm = TRUE),
                        sdHRS   = sd(rh_lt_20, na.rm = TRUE),
                        iqrHRS   = IQR(rh_lt_20, na.rm = TRUE),
                        medianMINRH = median(minRH, na.rm = TRUE),
                        meanMINRH   = mean(minRH, na.rm = TRUE),
                        sdMINRH   = sd(minRH, na.rm = TRUE),
                        iqrMINRH  = IQR(minRH, na.rm = TRUE),
                        lat = first(LATITUDE),
                        lon = first(LONGITUDE))
                                            
# plot maps
library(ggplot2)
sw_psa_poly<-fortify(sw_psa)
states <- ggplot2::map_data("state") 

burnHRSmo_sub<-subset(burnHRSmo, month %in% c(6,7,8,9))

ggplot()+
  #geom_polygon(data = states, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
  geom_polygon(data = sw_psa_poly, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
  coord_fixed(1.3)+
  theme_bw()+
  geom_point(data = burnHRSmo_sub, aes(x = lon, y = lat, color = medianHRS, size=iqrHRS))+
  scale_color_continuous(type = "viridis", name="median burning hours", limits=c(0,24))+
  facet_wrap(.~month)+
  ggtitle("Median Monthly Hours<20% RH/day (Key RAWS sites with >=15 years POR)")

ggplot()+
  #geom_polygon(data = states, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
  geom_polygon(data = sw_psa_poly, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
  coord_fixed(1.3)+
  theme_bw()+
  geom_point(data = burnHRSmo_sub, aes(x = lon, y = lat, color = medianMINRH, size=iqrMINRH))+
  scale_color_continuous(type = "viridis", name="median Min-RH", limits=c(5,50), direction=-1)+
  facet_wrap(.~month)+
  ggtitle("Median Min-RH (Key RAWS sites with >=19 years POR)")


# add in PSA centroid for factor levels
# merge in PSA centroid
sw_rawsDF_2<-merge(sw_rawsDF, sw_psaDF, by.x="NAT_CODE", by.y="PSANationa")
allBHRS_thin<-merge(allBHRS_thin, sw_rawsDF_2[,c("STA_NAME","y","x")], by="STA_NAME")
allBHRS_thin$PSA_NAME = factor(allBHRS_thin$PSA_NAME, levels=unique(allBHRS_thin$PSA_NAME[order(allBHRS_thin$x,allBHRS_thin$y)]), ordered=TRUE)
# factor level on RAWS long
allBHRS_thin$STA_NAME = factor(allBHRS_thin$STA_NAME, levels=unique(allBHRS_thin$STA_NAME[order(allBHRS_thin$LONGITUDE)]), ordered=TRUE)


# monthly boxplots
allBHRS_sub<-subset(allBHRS_thin, month %in% c(6,7,8,9))

ggplot(allBHRS_sub, aes(as.factor(month), rh_lt_20, group=month))+
  #geom_boxplot(outlier.shape = NA)+
  geom_boxplot()+
  #xlim(150,250)+
  ylim(0,24)+
  ylab("burning hours")+
  xlab("month")+
  facet_grid(.~PSA_NAME)

# heat map of summer daily BH
ggplot(allBHRS_thin, aes(STA_NAME, date, fill= rh_lt_20)) + 
  geom_tile()+
  #scale_fill_continuous(type = "viridis")+
  scale_fill_distiller(palette = 'Spectral', name="Hours/day RH<20%")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# ggplot(allBHRS_sub, aes(as.factor(doy), rh_lt_20, group=doy))+
#   geom_boxplot(outlier.shape = NA)+
#   #xlim(150,250)+
#   ylim(0,24)+
#   ylab("burning hours")+
#   xlab("day of year")+
#   facet_grid(.~PSA_NAME)

# look at daily transition
# relationship between minRH and BH
# compare to precip, fire danger indices


# stats burnHRS by day
burnHRSdoy<-  allBHRS_thin %>%
  group_by(doy, STA_NAME) %>%
  summarize(n_obs = n(),
            medianHRS = median(rh_lt_20, na.rm = TRUE),
            meanHRS   = mean(rh_lt_20, na.rm = TRUE),
            lat = first(LATITUDE),
            lon = first(LONGITUDE),
            month = first(month),
            day = first(day))
burnHRSdoy$fakeDate<-as.Date(paste0(burnHRSdoy$month,"-",burnHRSdoy$day,"-2000"), format="%m-%d-%Y")
burnHRSdoy$fakeDateLab<-factor(format(burnHRSdoy$fakeDate, "%b-%d"),
                               levels = format(seq.Date(as.Date("2000-01-01"),as.Date("2000-12-31"),by="day"), "%b-%d"), ordered = TRUE)


# plot maps
library(ggplot2)

sw_psa_poly<-fortify(sw_psa)
states <- ggplot2::map_data("state") # we already did this, but we can do it again

burnHRSdoy_sub<-burnHRSdoy[burnHRSdoy$fakeDate >= as.Date("2000-06-15") & burnHRSdoy$fakeDate <= as.Date("2000-07-14"), ]

ggplot()+
  #geom_polygon(data = states, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
  geom_polygon(data = sw_psa_poly, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
  coord_fixed(1.3)+
  theme_bw()+
  geom_point(data = burnHRSdoy_sub, aes(x = lon, y = lat, color = medianHRS), size = 2)+
  scale_color_continuous(type = "viridis", name="avg hours/day RH<20%")+
  facet_wrap(.~fakeDateLab)+
  ggtitle("Average Hours<20 %/day (Key RAWS sites with >15 years POR)")


# establishing regions with RAWS
allBHRS_sub<-subset(allBHRS_thin, month %in% c(6,7,8,9))
allBHRS_sub<-allBHRS_sub[,c("STA_NAME","date","rh_lt_20")]
# create data matrix
allBHRS_sub <- tidyr::spread(allBHRS_sub, STA_NAME, rh_lt_20)
# look at corrs
#corrmtx<-cor(allBHRS_sub[2:ncol(allBHRS_sub)], method = "pearson", use = "complete.obs")
#library(corrplot)
#corrplot(corrmtx, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45)

# PCA of stations
# library(factoextra)
# res.pca<-prcomp(na.omit(allBHRS_sub[2:ncol(allBHRS_sub)]), center = FALSE, scale = FALSE)
#   fviz_eig(res.pca)
#   fviz_pca_var(res.pca,
#                col.var = "contrib", # Color by contributions to the PC
#                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#                repel = TRUE     # Avoid text overlapping
#   )
# rotated PCA
  fit <- psych::principal((allBHRS_sub[2:ncol(allBHRS_sub)]), nfactors=3, rotate="varimax")

  fitLoad<-as.data.frame(unclass(fit$loadings))
  region<-cbind.data.frame(rownames(fitLoad),max.col(fitLoad, ties.method = "first"))
    colnames(region)<-c("STA_NAME","region")
    region<-merge(region, sw_rawsDF[,c("STA_NAME","LATITUDE","LONGITUDE")], by="STA_NAME")
  # plot regions
    ggplot()+
      #geom_polygon(data = states, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
      geom_polygon(data = sw_psa_poly, aes(x=long, y = lat, group = group),fill = NA, color = "black") + 
      coord_fixed(1.3)+
      theme_bw()+
      geom_text(data = region, aes(x = LONGITUDE, y = LATITUDE, label=region, color=as.factor(region)), size = 5)
      #ggtitle("Average Hours<20 %/day (Key RAWS sites with >15 years POR)")
  
# # number of regions
#   library(nFactors)
#   ev <- eigen(cor(allBHRS_sub[2:ncol(allBHRS_sub)], use="complete.obs")) # get eigenvalues
#   ap <- parallel(subject=nrow(allBHRS_sub[2:ncol(allBHRS_sub)]),var=ncol(allBHRS_sub[2:ncol(allBHRS_sub)]),
#                  rep=100,cent=.05)
#   nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
#   plotnScree(nS)
  
# BH ~ minRH relationship
#####
# Assess relationship between burning hours and min/maxRH
allBHRS_sub<-subset(allBHRS_thin, month %in% c(6,7,8,9))
  #cor(allBHRS_sub$rhDiff,allBHRS_sub$rh_lt_20, use="complete.obs")
  #plot(allBHRS_sub$rhDiff,allBHRS_sub$rh_lt_20)
# all stations
model<-lm(rh_lt_20 ~ minRH+rhDiff, data = allBHRS_sub)
  summary(model)
model<-lm(rh_lt_20 ~ minRH, data = allBHRS_sub)
  summary(model)
  model.res <- resid(model)  
hist(model.res)

# by station
library("broom")
groupsLM <- allBHRS_sub %>%
            group_by(STA_NAME)
GDDTrends<-do(groupsLM,glance(lm(rh_lt_20 ~ minRH+rhDiff, data = .)))
GDDSlopes<-do(groupsLM,tidy(lm(rh_lt_20 ~ minRH+rhDiff, data = .)))
#####

# RH daily climatologies
  statBHRS<-subset(allBHRS_thin, STA_NAME=="STANTON")
  statBHRS<-subset(allBHRS_thin, STA_NAME=="COLUMBINE")
  #statBHRS<-subset(allBHRS_thin, STA_NAME=="PELONA_MOUNTAIN")
  
# thin out to summer months
  statBHRS<-subset(statBHRS, month %in% c(6,7,8,9))
  
# ggplot quantile regression
  ggplot(statBHRS, aes(year,minRH))+
    geom_point()+
    geom_quantile(quantiles=c(0.05,0.5,0.95))+
    facet_wrap(.~month)+
    ylab("Burning Hours")+
    xlab("Year")+
    ggtitle("Monthly Burning Hours ~ ALPINE RAWS")+
    theme_bw()
