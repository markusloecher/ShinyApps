runTime = function(){
  load("SelStations.rda")
  hotpoints = Selhotpoints
  #hotpoints = Selhotpoints[-(which(is.na(hotpoints$STANAME))), ] # Deletes all NA rows
  
  now=seq.Date(as.Date("2016-01-01"),by=1,length=365)
  then=seq.Date(as.Date("1950-01-01"),by=1,length=365)
  
  
  mods = vector(mode = "list", length = length(colnames(SelStations))) # Initializes the vector
  plotty =  vector(mode = "list", length = length(colnames(SelStations)))
  xticks =  vector(mode = "list", length = length(colnames(SelStations)))
  nowY =  vector(mode = "list", length = length(colnames(SelStations)))
  thenY =  vector(mode = "list", length = length(colnames(SelStations)))
  #xs =  vector(mode = "list", length = length(colnames(SelStations)))
  
  for(i in 1:nrow(hotpoints)){
    
    ## copied from WeatherMapShiny.R
    columnNames = colnames(SelStations)
    x = data.frame(TG = coredata(SelStations)[, columnNames[i]], date = index(SelStations))
    x = na.omit(x)
    x$TG=x$TG/10
    x$yh = as.POSIXlt(x$date)$yday
    ##
    mods[[i]] = gam(TG ~ s(yh,k=40,bs="cc") + s(as.numeric(date),k=9),data=x) # Add the models to a vector
    
    #xs[[i]] = x
    
    plotty[[i]] = plot(mods[[i]], select = 2)
    xticks[[i]] = axTicks(1)
    
    nowY[[i]]=predict(mods[[i]],newdata=cbind.data.frame(date=as.numeric(now),yh=1:365))
    thenY[[i]]=predict(mods[[i]],newdata=cbind.data.frame(date=as.numeric(then),yh=1:365))
  }
  save(plotty, nowY, thenY, xticks, file="PreCalcData_trySmaller_woX.rda") # Saves the vector to a rda.-file
}


runTime()
