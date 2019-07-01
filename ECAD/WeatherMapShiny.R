#addServer("http://zeno.lehre.hwr-berlin.de:3838/", "zeno")
#addConnectServer("http://zeno.lehre.hwr-berlin.de:3838/", "zeno")
#connectUser(server = "http://zeno.lehre.hwr-berlin.de:3838/")


library(shiny)
library(leaflet)
library(zoo)
library(dygraphs)
library(xts)
library(RColorBrewer)
library(ggmap)
library(mgcv)
#library(sparkline)
#library(mapview)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

load("../SelStations.rda")
load("PreCalcData.rda")
hotpoints = Selhotpoints
#STAID = sample(colnames(SelStations),1)
#x = data.frame(TG =coredata(SelStations)[,"4848"], date = index(SelStations))
#x = na.omit(x)
#x$TG=x$TG/10
#x$yh = as.POSIXlt(x$date)$yday

rand = sample.int(427, 1)  # Replaces STAID (the sample)


LevCols = rev(heat.colors(5))[cut(hotpoints$trnd,breaks=quantile(hotpoints$trnd,c(seq(0,1,length=6)),na.rm=TRUE),labels=FALSE)] 


ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  h4(textOutput("cityName"), align = "center"), # Adds the name of the corresponding city below the map
  p(),
  plotOutput("GAMplot"),
  p()#,
  #fluidRow(verbatimTextOutput("Click_text"))
  #dygraphOutput("dygraph")
)

server <- function(input, output, session) {
  
  #reactive({ fib(as.numeric(input$n)) })
  
  # if ("STAID" %in% names(input)) {
  #   STAID = input$STAID
  # }
  
  station_id <- reactive({
    input$mymap_marker_click$id
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  pal <- colorNumeric(
    palette = "Spectral",
    domain = hotpoints$trnd
  )
  #pal( hotpoints$trnd)
  
  # observe({
  #   click<-input$map_marker_click
  #   if(is.null(click))
  #     return()
  #   text2 = paste("Latitude ", click$lat, "Longtitude ", click$lng)
  #   output$Click_text<-renderText({
  #     text2
  #   })
  # })
  # 
  #output$dygraph <- renderDygraph({
   #   dygraph(xts(x$TG, x$date), main =hotpoints[ifelse(!is.null(station_id()), station_id(), STAID),"staname"]) %>%  dyRangeSelector()
  #})
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%   
      addTiles()  %>% addCircleMarkers(lng=hotpoints$lon,
                                       lat=hotpoints$lat,radius=4,color = LevCols, 
                                       fillColor =  LevCols, popup=paste(hotpoints$STANAME), # Remnants of my approach to add sparkline to the popups: "<br>", popupGraph(  parkline( as.vector( dataForSparkline[[1]]$fit ) ) = "html")),# sparkline(mods[[ifelse(!is.null(station_id()), station_id(), 1)]], "line")), #Extended the popup's arugments
                                       layerId=which(rownames(hotpoints) == rownames(hotpoints))) #rownames(hotpoints))
    #,clusterOptions=1
  })
  
  output$cityName <- renderText(
    hotpoints[ifelse(!is.null(station_id()), station_id(), rand), "staname"]) # Creates the city name when new marker is clicked
  
  output$GAMplot <- renderPlot({
    
    ## mods[[ifelse(!is.null(station_id()), station_id(), rand)]] 
    ## mods contains all models and is loaded by load("PreCalcData.rda")
    
    x = data.frame(TG = coredata(SelStations)[, ifelse(!is.null(station_id()), station_id(), rand)], date = index(SelStations))
    x = na.omit(x)
    x$TG=x$TG/10
    x$yh = as.POSIXlt(x$date)$yday
    #YHfit=gam(TG ~ s(yh,k=40,bs="cc") + s(as.numeric(date),k=9),data=x)
    par(mfrow=c(1,2),mar=c(2,2,2.5,1))
    #plot(mods[[ifelse(!is.null(station_id()), station_id(), STAID)]], scale=0,select=1,ylab="", main="Yearly");grid()
    now=seq.Date(as.Date("2016-01-01"),by=1,length=365)
    nowY=predict(mods[[ifelse(!is.null(station_id()), station_id(), rand)]],newdata=cbind.data.frame(date=as.numeric(now),yh=1:365))
    then=seq.Date(as.Date("1950-01-01"),by=1,length=365)
    thenY=predict(mods[[ifelse(!is.null(station_id()), station_id(), rand)]],newdata=cbind.data.frame(date=as.numeric(then),yh=1:365))
    plot(now,nowY,type="l",col="blue",lwd=2,ylab="", xlab="",main="Annual",ylim=range(c(nowY,thenY),na.rm=TRUE));grid()
    lines(now, thenY,col="brown",lwd=2)
    legend("bottom", legend=c("1950","2016"), col=c("brown","blue"),lwd=1.75)
    
    plot(mods[[ifelse(!is.null(station_id()), station_id(), rand)]], scale=0,select=2,ylab="", main="Trend",rug=F,col.axis="white",col.lab="white");grid()
    axis(side=1, at=axTicks(1), labels=as.character(format(x$date[axTicks(1)-min(axTicks(1))+1],"%Y")),col.ticks="darkgreen")
    axis(side=2)
  })

  # Observe mouse clicks and add circles
  # observeEvent(input$mymap_marker_click, {
  #   ## Get the click info like had been doing
  #   click <- input$mymap_marker_click
  #   clat <- click$lat
  #   clng <- click$lng
  #   #cat("marker ID",click$id, "\n")
  #   STAID = as.character(click$id)
  # 
  #   assign("STAID",STAID, parent.env(environment()))
  #   #input$STAID = STAID
  #   #plotOutput("GAMplot")
  # 
  #   #address <- revgeocode(c(clng,clat))
  # 
  #   ## Add the circle to the map proxy
  #   ## so you dont need to re-render the whole thing
  #   ## I also give the circles a group, "circles", so you can
  #   ## then do something like hide all the circles with hideGroup('circles')
  #   # leafletProxy('mymap') %>% # use the proxy to save computation
  #     # addCircles(lng=clng, lat=clat, group='circles',
  #     #            weight=1, radius=3, color='red', fillColor='orange',
  #     #            popup=address, fillOpacity=0.5, opacity=1)
  # })
}

shinyApp(ui, server)

