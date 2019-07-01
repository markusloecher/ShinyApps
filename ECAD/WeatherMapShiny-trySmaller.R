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
library(shinyjs)
library(shinydashboard)
#library(sparkline)
#library(mapview)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

load("../SelStations.rda")
load("PreCalcData_trySmaller_woX.rda")
hotpoints = Selhotpoints
#hotpoints = Selhotpoints[-(which(is.na(hotpoints$STANAME))), ] # Deletes all NA rows
#STAID = sample(colnames(SelStations),1)
x = data.frame(TG =coredata(SelStations)[,"4848"], date = index(SelStations))
x = na.omit(x)
x$TG=x$TG/10
x$yh = as.POSIXlt(x$date)$yday

rand = sample.int(411, 1)  # Replaces STAID (the sample)


LevCols = rev(heat.colors(5))[cut(hotpoints$trnd,breaks=quantile(hotpoints$trnd,c(seq(0,1,length=6)),na.rm=TRUE),labels=FALSE)] 


ui <- fluidPage(
  dashboardBody(
    tags$head(tags$style(HTML('
                              .modal.in .modal-dialog{
                              width:100%;
                              height:185%;
                              margin:0px;
                              }
                              
                              .modal-content{
                              width:100%;
                              height:100%;
                              }
                              ')))),
  useShinyjs(),
  leafletOutput("mymap"),
  p(),
  h4(textOutput("cityName"), align = "center"), # Adds the name of the corresponding city below the map
  p(),
  plotOutput("GAMplot"),
  p(),
  actionButton("do", "Dygraph"),
  actionButton("button", "Clear"),
  p(),
  #fluidRow(verbatimTextOutput("Click_text"))
  dygraphOutput("dygraph"),
  p()
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
  
  
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = rand, { 
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = h3("Weather Map", align = "center"),
      p("Welcome to the Weather Map! This Weather Map visualizes the weather data from the last 100 years and lets you explore it interactively. 
        This landing page guides you through the functionalities of this app."),
      p("If you want to learn more about the weather in Erfurt, just click on the coloured circle in the map. 
        Of course you can also explore Leipzig, Luxembourg, Erlangen, Kassel, and so on! If you want to explore cities beyond Germany, 
        scroll out, or simply use the zoom feature in the upper left corner. "),
      p("As you're curious about Erfurt's weather (or any other city), the 'Annual' plot will visualize the difference between the weather 
        in 1950 and 2016. Additionally, the 'Trend' plot indicates a weather trend from as early as 1900, up to today. "),
      p("If you are still curious, you can further dig into the data, by clicking 'Dygraph'. 
        The Dygraph lets you explore the data used for the plots, in detail. 
        If you have enough of the Dygraph, simply click 'Clear' and it will disappear."),
      p("That's it, simply hit 'Dismiss' and start exploring the Weather App!"),
      #'This landing page guides you through the app's functionalities'
      img(src='shinymap.png', width = '100%', height = '100%'),
      img(src='shinyplotsErfurt.png', width = '100%', height = '100%')#,

    ))
  })
  
  
  
  observeEvent(input$do, {output$dygraph <- renderDygraph({
    dygraph(xts(x$TG, x$date), main =hotpoints[ifelse(!is.null(station_id()), station_id(), rand),"staname"]) %>%  dyRangeSelector()
    })
  })
  
  observeEvent(input$button, {
    hide("dygraph")
    # toggle can reavtivate the plot
  })
  
  
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
    
    indx = ifelse(!is.null(station_id()), station_id(), rand)
    
    par(mfrow=c(1,2),mar=c(2,2,2.5,1))
    
    now=seq.Date(as.Date("2016-01-01"),by=1,length=365)
    then=seq.Date(as.Date("1950-01-01"),by=1,length=365)
    plot(now,nowY[[indx]],type="l",col="blue",lwd=2,ylab="", xlab="",main="Annual",
         ylim=range(c(nowY[[indx]],thenY[[indx]]),na.rm=TRUE));grid()
    lines(now, thenY[[indx]],col="brown",lwd=2)
    legend("bottom", legend=c("1950","2016"), col=c("brown","blue"),lwd=1.75)
    
    plot(plotty[[indx]][[2]]$x, 
         plotty[[indx]][[2]]$fit, 
         ylab = plotty[[indx]][[2]]$ylab, 
         xlab = plotty[[indx]][[2]]$xlab, 
         type = "l", xaxt = "n", main = "Trend",
         ylim = c(min(plotty[[indx]][[2]]$fit)*1.3, max(plotty[[indx]][[2]]$fit)*1.3));grid()
    
    upr = plotty[[indx]][[2]]$fit + (1 * plotty[[indx]][[2]]$se)
    lwr = plotty[[indx]][[2]]$fit - (1 * plotty[[indx]][[2]]$se)
    lines(plotty[[indx]][[2]]$x, upr, lty = 3)
    lines(plotty[[indx]][[2]]$x, lwr, lty = 3)
    axis(side=1, at=xticks[[indx]], labels=as.character(format(x$date[xticks[[indx]]-min(xticks[[indx]])+1],"%Y")),
         col.ticks="darkgreen")
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

