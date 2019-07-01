#https://www.ecad.eu//dailydata/predefinedseries.php
#TX: maximum temperatures
#TG: mean temperatures
#TN: minimum temperatures

x=read.table(unzip("ECA_blend_tx.zip", fname), header=TRUE, sep = ',', skip=20)


if (0) {
  input = list(STAID=4588)
  ID = input$STAID#as.numeric(gsub(".txt","", gsub("TG_STAID", "", files[i]),fixed=TRUE))
  fname=paste0("TG_STAID",paste0(rep(0,6-nchar(ID)),collapse=""),ID,".txt") #files[i]
  
  ow=setwd("\\\\user\\home\\loecherm\\DropboxHWR\\TeachingMaterials\\ShinyApps\\Climate")
  x=read.table(unzip("ECA_blend_tg.zip", fname), header=TRUE, sep = ',', skip=20)
  x=x[x$Q_TG==0,]
  x$TG = x$TG/10
  x$date=as.Date(as.character(x$DATE), "%Y%m%d")
  x$yh = as.POSIXlt(x$date)$yday
  setwd("c:/Daten Löcher/Climate")
  load("results.rda")
  load("hotpoints.rda")
  load("x.rda")
  ID = input$STAID#as.numeric(gsub(".txt","", gsub("TG_STAID", "", files[i]),fixed=TRUE))
  fname=paste0("TG_STAID",paste0(rep(0,6-nchar(ID)),collapse=""),ID,".txt") #files[i]
  x=read.table(unzip("ECA_blend_tg.zip", fname), header=TRUE, sep = ',', skip=20)
  x=x[x$Q_TG==0,]
  x$TG = x$TG/10
  x$date=as.Date(as.character(x$DATE), "%Y%m%d")
  x$yh = as.POSIXlt(x$date)$yday
  save(x,file="x.rda")
  library(shiny)
  library(xts)
  library(dygraphs)
  library(leaflet)
  library(RColorBrewer)
  leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%   addTiles()  %>% addCircleMarkers(lng=hotpoints$lon,lat=hotpoints$lat,radius=1,clusterOptions=1, popup=hotpoints$STANAME)
  source('C:/Daten Löcher/Climate/SimpleLeafletShiny.R')
  print(source('C:/Daten Löcher/Climate/SimpleLeafletShiny.R')$value)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%
    addTiles()  %>% addCircleMarkers(lng=hotpoints$lon,lat=hotpoints$lat,radius=1,clusterOptions=1, popup=hotpoints$STANAME)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  print(source('C:/Daten Löcher/Climate/SimpleLeafletShiny.R')$value)
  runApp('H:/DropboxHWR/TeachingMaterials/ShinyApps/ECA_Climate')
  runApp('H:/DropboxHWR/TeachingMaterials/ShinyApps/ECA_Climate')
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  ?addCircleMarkers
  leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%
    addTiles()  %>% addCircleMarkers(lng=hotpoints$lon,lat=hotpoints$lat,radius=1,fillColor = pal( hotpoints$trnd), popup=hotpoints$STANAME)
  pal <- colorNumeric(
    palette = "Spectral",
    domain = hotpoints$trnd
  )
  leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%
    addTiles()  %>% addCircleMarkers(lng=hotpoints$lon,lat=hotpoints$lat,radius=1,fillColor = pal( hotpoints$trnd), popup=hotpoints$STANAME)
  #,clust
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  print(source('C:/Daten Löcher/Climate/LeafletProxy.R')$value)
  print(source('C:/Daten Löcher/Climate/ClickOnMap.R')$value)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  head(pal( hotpoints$trnd))
  plot(hotpoints$trnd,col=pal( hotpoints$trnd),cex=0.5,pch=0.5)
  leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%
    addTiles()  %>% s(lng=hotpoints$lon,lat=hotpoints$lat,radius=1,color = pal( hotpoints$trnd), popup=hotpoints$STANAME)
  leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%
    addTiles()  %>% s(lng=hotpoints$lon,lat=hotpoints$lat,radius=1,color = pal( hotpoints$trnd), popup=hotpoints$STANAME)
  leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%
    addTiles()  %>% addCircleMarkers(lng=hotpoints$lon,lat=hotpoints$lat,radius=1,color = pal( hotpoints$trnd), popup=hotpoints$STANAME)
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  ?mapview::viewRGB
  library(sp)
  library(raster)
  data(poppendorf)
  viewRGB(poppendorf, 4, 3, 2) # true-color
  library(mapview)
  library(sp)
  library(raster)
  data(poppendorf)
  viewRGB(poppendorf, 4, 3, 2) # true-color
  setwd("c:/Daten Löcher/Climate") #This line doen's work until you type a valid path
  ?merge
  ?merge.zoo
  print(source('C:/Daten Löcher/Climate/WeatherMap.R')$value)
  RefDates=zoo(order.by = seq.Date(as.Date("1876-01-01"),as.Date("2016-12-31"),by=1))
  length(RefDates)
  nrow(RefDates)
  head(RefDates)
  x = merge(zoo(x$TG,x$date),RefDates)
  M=51500
  AllStations=matrix(nrow=M,ncol=n)
  n=length(files)
  files = unzip("ECA_blend_tg.zip",list=TRUE)
  files = files[grep("TG_STAID",files[,1]),1]
}

ow=setwd("//user/home/loecherm/DropboxHWR/TeachingMaterials/ShinyApps/Climate/")
files=list.files(pattern="TG_STAID")
library(zoo)  
n=length(files)
M=51500
AllStations=matrix(nrow=M,ncol=n)
rownames(AllStations)=as.character(seq.Date(as.Date("1876-01-01"),as.Date("2016-12-31"),by=1))
colnames(AllStations)=as.character(1:n)
for (i in 1:length(files))
{
  #table=read.table(files[i], header=TRUE, sep = ',', skip=20)
  #x=read.table(unzip("ECA_blend_tg.zip", files[i]), header=TRUE, sep = ',', skip=20)
  x=read.table(files[i], header=TRUE, sep = ',', skip=20)
  x=x[x$Q_TG==0,]
  x$date=as.Date(as.character(x$DATE), "%Y%m%d")
  #ID = as.numeric(gsub(".txt","", gsub("TG_STAID", "", files[i]),fixed=TRUE))
  ID = as.numeric(gsub(".txt.gz","", gsub("TG_STAID", "", files[i]),fixed=TRUE))
  x = merge(zoo(x$TG,x$date),RefDates)
  AllStations[,i]=coredata(x)
  colnames(AllStations)[i]=ID
  cat("done with", i, "out of 3713.\n")
}
ID
i
length(coredata(x))
AllStations[,i]=coredata(x)
colnames(AllStations)[i]=ID
View(AllStations)
