load("SelStations.rda")
load("PreCalcData_trySmaller_woX.rda")

annual_1950 = vector(mode = "list", length = length(plotty))
annual_2016 = vector(mode = "list", length = length(plotty))
trend_fit = vector(mode = "list", length = length(plotty))
trend_se = vector(mode = "list", length = length(plotty))
trend_begin = c() #vector(mode = "list", length = length(plotty))
trend_end = c() #vector(mode = "list", length = length(plotty))

no_cities = which(is.na(Selhotpoints$trnd))

for(i in 1:length(plotty)){
  
  if(i %in% no_cities) next
  
  annual_1950[[i]] = as.numeric(thenY[[i]])
  annual_2016[[i]] = as.numeric(nowY[[i]])
  
  trend_fit[[i]] = plotty[[i]][[2]]$fit
  trend_se[[i]] = plotty[[i]][[2]]$se
  
  trend_begin[i] = as.numeric(substring(index(head(na.omit(SelStations[,colnames(SelStations)[i]]),n=1)),1,4))
  trend_end[i] = as.numeric(substring(index(tail(na.omit(SelStations[,colnames(SelStations)[i]]),n=1)),1,4))
  #ylim(min(unlist(thenY[1]))*0.9, max(unlist(nowY[1]))*1.1)
}

trnd_marker = na.omit(Selhotpoints$trnd)
lon = na.omit(Selhotpoints$lon)
lat = na.omit(Selhotpoints$lat)
station_names = na.omit(as.character(Selhotpoints$STANAME))
trend_begin = na.omit(trend_begin)
trend_end = na.omit(trend_end)


annual_1950 = annual_1950[-no_cities]
annual_2016 = annual_2016[-no_cities]
trend_fit = trend_fit[-no_cities]
trend_se = trend_se[-no_cities]

save(annual_1950, annual_2016, trend_fit, trend_se, 
     trnd_marker, lon, lat, station_names, trend_begin, trend_end,
     file="ShinyData-small.rda", compress = "xz")

