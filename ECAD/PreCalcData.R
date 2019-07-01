runTime = function(){
  mods <- vector(mode = "list", length = length(colnames(SelStations))) # Initializes the vector
  for(i in 1:length(mods)){
    
    ## copied from WeatherMapShiny.R
    columnNames = colnames(SelStations)
    x = data.frame(TG = coredata(SelStations)[, columnNames[i]], date = index(SelStations))
    x = na.omit(x)
    x$TG=x$TG/10
    x$yh = as.POSIXlt(x$date)$yday
    ##
    mods[[i]] = gam(TG ~ s(yh,k=40,bs="cc") + s(as.numeric(date),k=9),data=x) # Add the models to a vector
  }
  save(mods, file="PreCalcData.rda") # Saves the vector to a rda.-file
}


runTime()
