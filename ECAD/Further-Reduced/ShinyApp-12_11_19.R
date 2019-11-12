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

load("ShinyData-small.rda")

rand = sample.int(383, 1)  # Replaces STAID (the sample)

LevCols = rev(heat.colors(5))[cut(trnd_marker,breaks=quantile(trnd_marker,c(seq(0,1,length=6)),na.rm=TRUE),labels=FALSE)] 

ui <- navbarPage("Weather",
                 tabPanel("Map",
                          fluidPage(
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
                            #actionButton("do", "Dygraph"),
                            #actionButton("button", "Clear"),
                            p(),
                            #fluidRow(verbatimTextOutput("Click_text"))
                            #dygraphOutput("dygraph"),
                            p()
                          )), tabPanel("Empty")) # The content for the second panel can be entered right after "Summary"

server <- function(input, output, session) {
  
  station_id <- reactive({
    input$mymap_marker_click$id
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  pal <- colorNumeric(
    palette = "Spectral",
    domain = trnd_marker
  )
  
  
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
  
  
  #observeEvent(input$do, {output$dygraph <- renderDygraph({
   # dygraph(xts(x$TG, x$date), main =hotpoints[ifelse(!is.null(station_id()), station_id(), rand),"staname"]) %>%  dyRangeSelector()
  #})
  #})
  
  #observeEvent(input$button, {
   # hide("dygraph")
    # toggle can reavtivate the plot
  #})
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% setView(lng = 8.68, lat = 50.11, zoom = 7) %>%   
      addTiles()  %>% addCircleMarkers(lng=lon,
                                       lat=lat,radius=4,color = LevCols, 
                                       fillColor =  LevCols, popup=paste(station_names), # Remnants of my approach to add sparkline to the popups: "<br>", popupGraph(  parkline( as.vector( dataForSparkline[[1]]$fit ) ) = "html")),# sparkline(mods[[ifelse(!is.null(station_id()), station_id(), 1)]], "line")), #Extended the popup's arugments
                                       layerId=which(station_names == station_names)
                                       ) #rownames(hotpoints))
    #,clusterOptions=1
  })
  
  output$cityName <- renderText(
    #hotpoints[ifelse(!is.null(station_id()), station_id(), rand), "staname"]) # Creates the city name when new marker is clicked
    station_names[ifelse(!is.null(station_id()), station_id(), rand)]
  )
    
  output$GAMplot <- renderPlot({
    
    par(mfrow=c(1,2),mar=c(2,2,2.5,1))
    
    now=seq.Date(as.Date("2016-01-01"),by=1,length=365)
    then=seq.Date(as.Date("1950-01-01"),by=1,length=365)
    
    plot(annual_2016[[ifelse(!is.null(station_id()), station_id(), rand)]], 
         col='blue', lwd=2,ylab="", xlab="",main="Annual", type='l', xaxt='n',
         ylim = range(c(min(annual_1950[[ifelse(!is.null(station_id()), station_id(), rand)]])*0.9, 
                        max(annual_2016[[ifelse(!is.null(station_id()), station_id(), rand)]])))
         );grid()
    axis(1, at=seq(1, 365, length.out = 13), month.abb[c(seq(1,12),1)])
    
    #plot(now,nowY[[indx]],type="l",col="blue",lwd=2,ylab="", xlab="",main="Annual",
     #    ylim=range(c(nowY[[indx]],thenY[[indx]]),na.rm=TRUE));grid()
    lines(annual_1950[[ifelse(!is.null(station_id()), station_id(), rand)]], col="brown",lwd=2)
    legend("bottom", legend=c("1950","2016"), col=c("brown","blue"),lwd=1.75)
    
    
    plot(trend_fit[[ifelse(!is.null(station_id()), station_id(), rand)]],
         type='l', main = "Trend", xaxt = "n",
         ylim=range(c(min(trend_fit[[ifelse(!is.null(station_id()), station_id(), rand)]]-
                            trend_se[[ifelse(!is.null(station_id()), station_id(), rand)]])*1.1, 
                      max(trend_fit[[ifelse(!is.null(station_id()), station_id(), rand)]]+
                                   trend_se[[ifelse(!is.null(station_id()), station_id(), rand)]])*1.1)));grid()
    lines(trend_fit[[ifelse(!is.null(station_id()), station_id(), rand)]]+
            trend_se[[ifelse(!is.null(station_id()), station_id(), rand)]], lty = 3)
    lines(trend_fit[[ifelse(!is.null(station_id()), station_id(), rand)]]-
            trend_se[[ifelse(!is.null(station_id()), station_id(), rand)]], lty = 3)
    axis(1, at=seq(1, 100, length.out = (trend_end[ifelse(!is.null(station_id()), station_id(), rand)]
                                         -trend_begin[ifelse(!is.null(station_id()), station_id(), rand)])+1), 
         trend_begin[ifelse(!is.null(station_id()), station_id(), rand)]:
           trend_end[ifelse(!is.null(station_id()), station_id(), rand)])
    
  })
  
}

shinyApp(ui, server)

