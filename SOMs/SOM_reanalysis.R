# SOM using Reanalysis data downloaded from ESRL
# adapting code from rncep_som_test.R
# MAC 06/11/2020

# to do: adapt to other near-real time gridded datasets like NARR, CFSR

library(raster)
library(lubridate)
library(reshape2)
library(kohonen)
library(tidyr)
library(ggplot2)
library(PBSmapping)

# set rasteroptions
rasterOptions(progress = 'text')

# functions
leap_every_year <- function(x) {
  ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
}

# map layers
states <- getData('GADM', country='United States', level=1)
us <- getData('GADM', country='United States', level=0)
mx <- getData('GADM', country='Mexico', level=0)
#cn <- getData('GADM', country='Canada', level=0)

# get grid processed from ~/SWMonsoonTracker/NCEPgrids/createNCEPgrids.R, stored in scratch
gh500<-stack("/scratch/crimmins/esrl/processed/HGT700_NCEP_R1_1948_2020.grd")

# crop to NAME region
# e <- extent(360-130, 360-90,15, 50)
# gh500 <- (crop(gh500, e)) # can also add in rotate to convert lon to neg

# Crimmins 2006 region
e <- extent(360-118, 360-102,27, 42)
gh500 <- (crop(gh500, e)) # can also add in rotate to convert lon to neg

# dates - find and remove leap days
nlayers(gh500) 
  dates<-as.data.frame(seq.Date(as.Date("1948-01-01"),as.Date("2020-12-31"),1))
  colnames(dates)<-"date"
  dates$month<-as.numeric(format(dates$date, "%m"))
  dates$day<-as.numeric(format(dates$date, "%d"))
  dates$year<-as.numeric(format(dates$date, "%Y"))
  dates$doy<-as.numeric(format(dates$date, "%j"))
  dates$doy_ly<-leap_every_year(dates$date) # this day of year without leap day shifts

# subset layers to months of interest
  mos<-c(4,5,6)
  subDates<-dates[which(dates$month %in% mos),]
  subLayers<-gh500[[which(dates$month %in% mos)]]
  
# convert layers to dataframe
  layers.df<-(as.data.frame(subLayers, long=TRUE, xy=TRUE))
    colnames(layers.df)<-c("lon","lat","date","value")  
    # long to wide
    df.wide<-dcast(layers.df, formula = date~lat+lon, value.var = "value")

    
    #  SOM screening #####
    source("/home/crimmins/RProjects/SOMs/topo.error.R")
    ncols=c(2,3,4,3,5,4,6,7,5,4,8,9,6,5,10,11,6,8,5,13,7,14)
    nrows=c(2,2,2,3,2,3,2,2,3,4,2,2,3,4,2,2,4,3,5,2,4,2)
    qe<-c()
    te<-c()
    ptm <- proc.time()
    for(i in 1:length(nrows)){
      som.gh500 <- supersom(as.matrix(df.wide[,2:ncol(df.wide)]),
                            #grid = somgrid(ncols[i], nrows[i], "rectangular"),
                            grid = somgrid(ncols[i], nrows[i], topo="rectangular", neighbourhood.fct = c("gaussian")),
                            #alpha = c(0.05, 0.001),
                            radius = c(3,1),
                            mode= "pbatch",
                            cores = 7,
                            rlen = 5000,
                            dist.fcts = "sumofsquares")
      print(paste0(nrows[i],"x",ncols[i],"-round1"))
      som.gh500.2 <- supersom(as.matrix(df.wide[,2:ncol(df.wide)]),
                              #grid = somgrid(ncols, nrows, "rectangular"),
                              grid = somgrid(ncols[i], nrows[i], topo="rectangular", neighbourhood.fct = c("gaussian")),
                              #alpha = c(0.05, 0.001), # for online
                              radius = c(2,1),
                              mode= "pbatch", # "pbatch" or "online"
                              #mode= "online", # "pbatch" or "online"
                              #maxNA.fraction = 0.5,
                              init= som.gh500$codes,
                              cores = 7,
                              rlen = 5000,
                              dist.fcts = "sumofsquares")
      ## quantization error:
      qe[i]<-mean(som.gh500.2$distances)
      ## topographical error measures:
      te[i]<-topo.error(som.gh500.2, "nodedist")
      print(paste0(nrows[i],"x",ncols[i],"-round2"))
    }
    proc.time() - ptm

    diagnostics<-cbind.data.frame(nrows,ncols,qe,te)
    plot(diagnostics$qe, diagnostics$te, xlim=c())
    text(diagnostics$qe, diagnostics$te, labels=paste0(diagnostics$nrows,"-",diagnostics$ncols))
    # save(diagnostics, file = "~/RProjects/SOMs/monsoonPrecip/diagnostics_CP2.RData")
    ######
    diagnostics2<-diagnostics
    
        
# simple kohonen SOM
  # nrows=3
  # ncols=4
  #   som.gh500 <- som(as.matrix(df.wide[,2:ncol(df.wide)]), grid = somgrid(ncols, nrows, "rectangular"))
 
    ##### 
    # CP2 - 2 phase SOM training
     nrows=3
     ncols=4
     ptm <- proc.time()
     #som.gh500 <- som(as.matrix(df.wide[,2:ncol(df.wide)]), grid = somgrid(ncols, nrows, "rectangular"))
     #set.seed(999) #keep set seed 16, 11 upper left/wet, 9,6 upper right dry/UL wet, 8 UL Wet/LR dry, 7 LR Wet/LL Dry, 5 LR wet/LL dry, 4/100 UL wet/UR dry, 101 wet UR/dry LL, 102/104 dry UL/wet UR, 103 UL Wet/LR Dry
     set.seed(29) # 29 for 3x4
     som.gh500 <- supersom(as.matrix(df.wide[,2:ncol(df.wide)]),
                          #grid = somgrid(ncols, nrows, "rectangular"),
                          grid = somgrid(ncols, nrows, topo="rectangular", neighbourhood.fct = c("gaussian")),
                          #alpha = c(0.05, 0.001), # for online
                          radius = c(3,1), #(4,1) for 4x5
                          mode= "pbatch", # "pbatch" or "online"
                          #mode= "online", # "pbatch" or "online"
                          #maxNA.fraction = 0.999,
                          cores = 7,
                          rlen = 25000, #5000
                          dist.fcts = "sumofsquares")
     print("completed phase 1, performing phase 2")
    som.gh500.2 <- supersom(as.matrix(df.wide[,2:ncol(df.wide)]),
                          #grid = somgrid(ncols, nrows, "rectangular"),
                          grid = somgrid(ncols, nrows, topo="rectangular", neighbourhood.fct = c("gaussian")),
                          #alpha = c(0.05, 0.001), # for online
                          radius = c(2,1), # c(3,0.33) for 3x5
                          mode= "pbatch", # "pbatch" or "online"
                          #mode= "online", # "pbatch" or "online"
                          #maxNA.fraction = 0.999,
                          init= som.gh500$codes,
                          cores = 7,
                          rlen = 50000, #7000
                          dist.fcts = "sumofsquares")
        ## quantization error:
        mean(som.gh500.2$distances)
        ## topographical error measures:
        source("/home/crimmins/RProjects/SOMs/topo.error.R")
        topo.error(som.gh500.2, "nodedist")
    #proc.time() - ptm
    som.gh500<-som.gh500.2
    #####    
    
# get codebook    
    codebook<-as.data.frame(som.gh500$codes)
    code_grid<-as.data.frame(som.gh500$grid$pts)
    code_grid$mapUnit<-seq(1,nrow(code_grid))
    code_grid<-code_grid %>%
      unite(y,x, col="codes", sep="_") # add mapunit back in if needed
    
# deal with code book
    codebook<-cbind(code_grid,codebook)
    codebook.long<-melt(codebook, id.vars = 1)
      # separate out mapunits  ----
      mapunits<-codebook.long[1:(nrows*ncols),]
      codebook.long<-codebook.long[-(1:(nrows*ncols)),]
      # ####
    codebook.long<-separate(codebook.long, variable, convert = TRUE, into = c("lat", "lon"), sep="_")
    codebook.long$lat<-as.numeric(gsub("X", "", codebook.long$lat))
    codebook.long$lon<-codebook.long$lon-360
    codebook.long<-separate(codebook.long, codes, convert = FALSE, remove = FALSE, into = c("xCols", "yRows"), sep="_")
    
    # assign days to nodes
    nodes<-map(som.gh500)
    somTime<-as.data.frame(cbind(df.wide$date, nodes$unit.classif, nodes$distances))
    colnames(somTime)<-c("date","mapUnit","errorDist")
      somTime$date<-as.character(somTime$date)
      somTime$date<-as.Date(somTime$date, format="X%Y.%m.%d")
      somTime$month<-as.numeric(format(somTime$date, format="%m"))
      somTime$year <-as.numeric(format(somTime$date, format="%Y"))
      somTime$day  <-as.numeric(format(somTime$date, format="%d"))
      #somTime<-separate(somTime, as.character(date), convert = TRUE, remove = FALSE, into = c("year","month","day"), sep=".")
    #somTime$date<-as.Date(paste(somTime$year,"-",somTime$day,"-",somTime$month,sep=""), format="%Y-%d-%m")
    somTime$doy<-as.numeric(format(somTime$date, "%j"))
    somTime$mapUnit<-as.integer(somTime$mapUnit)
    somTime$errorDist<-as.numeric(as.character(somTime$errorDist))
    # join codes to node table
    somTime<-left_join(somTime, code_grid)
    # get codeList
    codeList<-unique(codebook.long$codes)
    
    
    ##### MAP RESULTS
    # plot map - fix lines http://cameron.bracken.bz/finally-an-easy-way-to-fix-the-horizontal-lines-in-ggplot2-maps
    # plot limits
    #xlim = c(-130,-90)
    #ylim = c(15,50)
  
    
    xlim = c(-(360-extent(gh500)[1]),-(360-extent(gh500)[2]))
    ylim = c(extent(gh500)[3],extent(gh500)[4])
    
    all_states <- map_data("state")
    world<-map_data("world")
    
    colnames(world)<-c("X","Y","PID","POS","region","subregion")
    world = clipPolys(world, xlim=xlim,ylim=ylim, keepExtra=TRUE)
    #colnames(world)<-c("Lon","Lat","PID","POS","region","subregion") 
    
    colnames(all_states)<-c("X","Y","PID","POS","region","subregion")
    all_states = clipPolys(all_states, xlim=xlim,ylim=ylim, keepExtra=TRUE)
    #colnames(all_states)<-c("Lon","Lat","PID","POS","region","subregion")
    
    p <- ggplot()
    p + geom_polygon( data=world, aes(x=X, y=Y, group = PID),colour="black", fill=NA )+
      geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="grey", fill=NA )+
      scale_x_continuous(breaks = c(-120,-140))+
      stat_contour(data=codebook.long, aes(lon, lat, z=value, color=..level..), binwidth = 10,size=1)+
      scale_colour_distiller(palette = "Spectral", name="700mb GPH (m)")+
      #scale_color_continuous(low="blue", high = "red")+ 
      coord_map(xlim = xlim,ylim = ylim)+
      facet_wrap(~codes, nrow = nrows, ncol=ncols)+theme_bw()+
      labs(x="Lon", y="Lat")+
      ggtitle("AMJ 700mb GH Pattern Classification (NCEP R1, 1948-2020)")
    
    # summary plots
    # DIAGNOSTICS
    plot(som.gh500, type="changes") # changes, codes, counts, property, quality, mapping
    
    # summary plots -- appears to plot opposite up/down from SOM plot
    counts <- plot(som.gh500, type="counts", shape = "straight", labels=counts)
    codes <- plot(som.gh500, type="codes", shape = "straight")
    similarities <- plot(som.gh500, type="quality", palette.name = terrain.colors)
    plot(som.gh500, type="dist.neighbours", main = "SOM neighbour distances")
    plot(som.gh500)
    
    # sammon mapping
    library(MASS)
    gh500.codes <- som.gh500$codes
    dis <- dist(as.matrix(som.gh500$codes[[1]]))
    gh500.sam <- sammon(dis)
    plot(gh500.sam$points, type="n")
    text(gh500.sam$points,labels=as.character(1:nrow(code_grid)))
    ##  Polygon version of map ----
    library(sp)
    temp<-list()
    ctr<-1
    for(j in 1:(nrows-1)){
      for(k in 1:(ncols-1)){
        temp[ctr]<-Polygons(list(Polygon(cbind(c(gh500.sam$points[(k+(ncols*j)-ncols),1], gh500.sam$points[(k+(ncols*j)-ncols)+1,1],
                                                 gh500.sam$points[k+(ncols*j)+1,1],gh500.sam$points[k+(ncols*j),1]),
                                               c(gh500.sam$points[(k+(ncols*j)-ncols),2], gh500.sam$points[(k+(ncols*j)-ncols)+1,2],
                                                 gh500.sam$points[k+(ncols*j)+1,2],gh500.sam$points[k+(ncols*j),2])))),paste0(ctr))
        ctr<-ctr+1
      }
    }  
    sr1<-SpatialPolygons(temp)  
    plot(gh500.sam$points, type="n")
    text(gh500.sam$points,labels=as.character(1:nrow(code_grid)))
    plot(sr1, add=TRUE)
    # ----
    
    # GGPLOT versions
    # counts --- plotting incorrectly
    counts<-somTime %>% group_by(codes) %>% count(codes)
    counts <- counts %>% separate(codes, c("row","col"), sep = "_", remove=FALSE)
    counts$row<-as.numeric(counts$row); counts$col<-as.numeric(counts$col); 
    counts$avg<-(counts$n/length(seq(1948,2020,1)))
    pCt<-ggplot(counts, aes(x=col,y=-row))+
      geom_tile(aes(fill = (n/nrow(somTime)*100)))+
      #geom_text(aes(label = codes), size=6)+
      #geom_text(aes(label = n), size=6)+
      geom_text(aes(label = round((n/nrow(somTime)*100),0)), size=6)+
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA, name="% days")+
      #scale_fill_gradient2(low = "lightblue",mid="yellow", midpoint = 10,
      #                     high = "red", na.value = NA, name="% days")+
      ggtitle("Percent of days/node")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    pAvgDays<-ggplot(counts, aes(x=col,y=-row))+
      geom_tile(aes(fill = avg))+
      #geom_text(aes(label = codes), size=6)+
      #geom_text(aes(label = n), size=6)+
      geom_text(aes(label = round(avg,1)), size=6)+
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA, name="# days")+
      #scale_fill_gradient2(low = "lightblue",mid="yellow", midpoint = 10,
      #                     high = "red", na.value = NA, name="% days")+
      ggtitle("Avg # of days/node")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())  
    
    # SOM distances
    ndist <- unit.distances(som.gh500$grid)
    cddist <- as.matrix(object.distances(som.gh500, type = "codes"))
    cddist[abs(ndist - 1) > .001] <- NA
    neigh.dists <- colMeans(cddist, na.rm = TRUE)
    som_grid <- som.gh500[[4]]$pts %>%
      as_tibble %>% 
      mutate(id=row_number())
    som_grid$codes<-paste0(som_grid$y,"_",som_grid$x)
    som_grid <- som_grid %>% mutate(dist=neigh.dists)
    pDist<-ggplot(som_grid, aes(x=x,y=-y))+
      geom_tile(aes(fill = dist))+
      geom_text(aes(label = codes), size=6)+
      scale_fill_gradient(low = "lightblue", high = "red", na.value = NA, name="distance")+
      ggtitle("Neighborhood Distance")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    
    # SOM node quality
    sim<-cbind.data.frame(som.gh500$unit.classif,som.gh500$distances)
    colnames(sim)<-c("node","distance")
    sim <- sim %>% group_by(node) %>% summarise(dist = mean(distance))
    sim$code<-codeList
    sim <- sim %>% separate(code, c("row","col"), sep = "_", remove=FALSE)
    sim$row<-as.numeric(sim$row); sim$col<-as.numeric(sim$col); 
    pQ<-ggplot(sim, aes(x=col,y=-row))+
      geom_tile(aes(fill = dist))+
      geom_text(aes(label = code), size=6)+
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA, name="mean error")+
      ggtitle("SOM Node Quality")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    
    # SOM node counts by year - facet wrap indiv heat maps
    countsYr<-somTime %>% group_by(codes,year) %>% count(codes)
    countsYr <- countsYr %>% separate(codes, c("row","col"), sep = "_", remove=FALSE)
    countsYr$row<-as.numeric(countsYr$row); countsYr$col<-as.numeric(countsYr$col); 
    ggplot(countsYr, aes(x=col, y=-row))+
      geom_tile(aes(fill=n))+
      geom_text(aes(label = n), size=4)+
      facet_wrap(~year)+
      scale_fill_gradient2(low = "lightblue",mid="yellow", midpoint = 25,
                           high = "red", na.value = NA, name="count")+
      ggtitle("Count of days in each node by year")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    
    # time series
    temp<-subset(countsYr, codes=="2_2")
    ggplot(temp, aes(x=year, y=n, color=as.factor(codes)))+
      geom_line()+
      geom_point()
    
    # SOM node anomaly
    library(scales)
    countAnom<-merge(countsYr, counts, by="codes")
    countAnom$anomCT<-countAnom$n.x/countAnom$avg
    ggplot(countAnom, aes(x=col.x, y=-row.x))+
      geom_tile(aes(fill=anomCT))+
      #geom_text(aes(label = anomCT), size=4)+
      facet_wrap(~year)+
      scale_fill_gradient2(low = "purple",mid="white", midpoint = 1,
                           high = "orange", na.value = NA, name="% of avg",limits=c(0, 2), oob=squish)+
      ggtitle("Anom of days in each node by year")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    
    
    # SOM node counts by month
    countsMo<-somTime %>% group_by(codes,month) %>% count(codes)
    countsMo <- countsMo %>% separate(codes, c("row","col"), sep = "_", remove=FALSE)
    countsMo$row<-as.numeric(countsMo$row); countsMo$col<-as.numeric(countsMo$col); 
    ggplot(countsMo, aes(x=col, y=-row))+
      geom_tile(aes(fill=n))+
      geom_text(aes(label = n), size=4)+
      facet_wrap(~month)+
      scale_fill_gradient2(low = "lightblue",mid="yellow", midpoint = 350,
                           high = "red", na.value = NA, name="count")+
      ggtitle("Count of days in each node by month")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    
    # Expected Counts - SOM node counts by month
    temp2<-somTime %>% group_by(month) %>% count(month)
    temp2$prop<-temp2$n/sum(temp2$n)
    temp3<-somTime %>% group_by(codes) %>% count(codes)
    countsMo<-somTime %>% group_by(codes,month) %>% count(codes)
    countsMo <- countsMo %>% separate(codes, c("row","col"), sep = "_", remove=FALSE)
    countsMo$row<-as.numeric(countsMo$row); countsMo$col<-as.numeric(countsMo$col); 
    countsMo<-merge(countsMo,temp2, by="month")
    countsMo$anom<-(countsMo$n.x/countsMo$n.y)*100
    countsMo<-merge(countsMo, temp3, by="codes")
    countsMo$expCt<-countsMo$n*countsMo$prop
    countsMo$anomCt<-countsMo$n.x-countsMo$expCt
    # proportion test prop.test()
    propPval<- lapply(seq_along(countsMo$n.x),
                      function(i) prop.test(countsMo$n.x[i],countsMo$n[i],p=countsMo$prop[i],alternative = "two.sided")$p.value)
    binomPval<- lapply(seq_along(countsMo$n.x),
                       function(i) binom.test(countsMo$n.x[i],countsMo$n[i],p=countsMo$prop[i],alternative = "two.sided")$p.value)
    countsMo$propPval<-unlist(propPval)
    countsMo$binomPval<-unlist(binomPval)
    countsMo$sig<-countsMo$propPval<=0.05
    countsMo$sigBinom<-countsMo$binomPval<=0.05
    countsMo$anomCtLabel<-ifelse(countsMo$sig==TRUE,
                                 paste0(round(countsMo$anomCt,1),"*"),
                                 paste0(round(countsMo$anomCt,1)))
    ggplot(countsMo, aes(x=col, y=-row))+
      geom_tile(aes(fill=anomCt))+
      geom_text(aes(label = anomCtLabel), size=4)+
      facet_wrap(~month)+
      scale_fill_gradient2(low = "orange",mid="grey", midpoint = 0,
                           high = "purple", na.value = NA, name="count", limits=c(-50, 50), oob=squish)+
      ggtitle("Anom of expected days in each node by Month")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    
# Add in MJO/ENSO
    
    #####
    # add in climate indices
    load("~/RProjects/FireClimate/SOMs/climInd.RData")
    climInd<-climInd[,c("date","phase","amplitude","ONI")]
    climInd$date<-climInd$date+1 # align with PRISM doy
    colnames(climInd)[1]<-"date.c"
    # shift to match ONI
   # climInd$ONI<-c(climInd$ONI[-1],NA)
    somTime<-merge(somTime,climInd, by.x="date",by.y="date.c", all.x=TRUE)
    # ENSO phase
    somTime$ENSO<-"Neutral"
    somTime$ENSO<-ifelse(somTime$ONI <= -0.5, "La Nina", somTime$ENSO)   
    somTime$ENSO<-ifelse(somTime$ONI >= 0.5, "El Nino", somTime$ENSO)   
    #####  
    
    # SOM node counts by ENSO phase
    # trim to no NA
    somTrim<-na.omit(somTime)
    temp2<-somTrim %>% group_by(ENSO) %>% count(ENSO)
    temp2$prop<-temp2$n/sum(temp2$n)
    temp3<-somTrim %>% group_by(codes) %>% count(codes)
    countsENSO<-somTrim %>% group_by(codes,ENSO) %>% count(codes)
    countsENSO <- countsENSO %>% separate(codes, c("row","col"), sep = "_", remove=FALSE)
    countsENSO$row<-as.numeric(countsENSO$row); countsENSO$col<-as.numeric(countsENSO$col); 
    countsENSO<-merge(countsENSO,temp2, by="ENSO")
    countsENSO$anom<-(countsENSO$n.x/countsENSO$n.y)*100
    countsENSO<-merge(countsENSO, temp3, by="codes")
    countsENSO$expCt<-countsENSO$n*countsENSO$prop
    countsENSO$anomCt<-countsENSO$n.x-countsENSO$expCt
    # proportion test prop.test()
    propPval<- lapply(seq_along(countsENSO$n.x),
                      function(i) prop.test(countsENSO$n.x[i],countsENSO$n[i],p=countsENSO$prop[i],alternative = "two.sided")$p.value)
    binomPval<- lapply(seq_along(countsENSO$n.x),
                       function(i) binom.test(countsENSO$n.x[i],countsENSO$n[i],p=countsENSO$prop[i],alternative = "two.sided")$p.value)
    countsENSO$propPval<-unlist(propPval)
    countsENSO$binomPval<-unlist(binomPval)
    countsENSO$sig<-countsENSO$propPval<=0.05
    countsENSO$sigBinom<-countsENSO$binomPval<=0.05
    countsENSO$anomCtLabel<-ifelse(countsENSO$sig==TRUE,
                                   paste0(round(countsENSO$anomCt,1),"*"),
                                   paste0(round(countsENSO$anomCt,1)))  
    # ggplot  
    ggplot(countsENSO, aes(x=col, y=-row))+
      geom_tile(aes(fill=anomCt))+
      geom_text(aes(label = anomCtLabel), size=4)+
      facet_wrap(~ENSO)+
      scale_fill_gradient2(low = "lightblue",mid="white", midpoint = 0,
                           high = "red", na.value = NA, name="count")+
      ggtitle("Anom of expected days in each node by ENSO Phase (*pval<0.05)")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    
    
#####    
    
    # classification error by node-year
    errorYr<-somTime %>% 
      group_by(codes,year) %>%
      summarise(error=mean(errorDist))
    errorYr <- errorYr %>% separate(codes, c("row","col"), sep = "_", remove=FALSE)
    errorYr$row<-as.numeric(errorYr$row); errorYr$col<-as.numeric(errorYr$col); 
    ggplot(errorYr, aes(x=col, y=-row))+
      geom_tile(aes(fill=round(error,0)))+
      #geom_text(aes(label = error), size=2)+
      facet_wrap(~year)+
      scale_fill_gradient2(low = "blue",mid="yellow", 
                           high = "red", midpoint = 300000, 
                           na.value = NA, name="class. error")+
      ggtitle("Classification error by node-year")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank())
    # error by year
    errorYr<-somTime %>% 
      group_by(year) %>%
      summarise(error=median(errorDist))
    ggplot(errorYr, aes(x=year, y=error))+
      geom_bar(stat = "identity", fill="red")+
      ggtitle("Median error of all nodes by year")
    
    
    
    # counts of nodes by day of year
    library(dplyr)
    countDOY<-somTime %>% group_by(month,day) %>% count(codes)
    countDOY <- countDOY %>% group_by(month,day) %>% summarize(maxCount=max(n),
                                                               maxNode= codes[which.max(n)])
    countDOY$date<-as.Date(paste0(countDOY$month,"-",countDOY$day,"-2016"), format="%m-%d-%Y")      
    countDOY$wday<-wday(countDOY$date, label = T, week_start = 7)
    countDOY$week<-epiweek(countDOY$date)
    countDOY %>%
      ggplot(aes(wday,-week, fill = maxCount)) +
      geom_tile(colour = "white")  + 
      geom_text(aes(label = maxNode), size = 3) +
      theme(aspect.ratio = 1/2,
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 15),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      scale_fill_gradient(low="lightblue", high="red") +
      facet_wrap(~month, nrow = 3, ncol = 1, scales = "free") +
      labs(title = "Most frequent nodes by day of year")
    
    
    
    
  