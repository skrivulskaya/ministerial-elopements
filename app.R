rm(list=ls(all=TRUE)) # clear memory

#Mat's new Todo
    #Impliment the random mover (working for the origins) earlier on so it generates the lines off of the moved points
    #Move the lost /found same city points over
#move the arrows over
#clasify direction
#put in the direction as a classification


# packages<- c("rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
library(rgdal)
library(leaflet)
library(shiny)
# library(ggmap)
# library(leaflet.minicharts)
library (geosphere)
# library(maptools)


# setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements")
# setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")
# setwd("E:\\GIT_Checkouts\\R_Scripts\\ministerial-elopements")

latlong <- "+init=epsg:4326"


#Loading the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)

#Generate new locations for duplicate places 

elop.raw$dup_origin <- elop.raw$Location_Origin %in% elop.raw[duplicated(elop.raw$Location_Origin),]$Location_Origin
elop.raw$dup_found <- elop.raw$Location_Found %in% elop.raw[duplicated(elop.raw$Location_Found),]$Location_Found
elop.raw$Latitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Latitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Origin)
elop.raw$Longtitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Longtitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Origin)
elop.raw$Latitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Latitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Found)
elop.raw$Longtitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Longtitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Found)


#Make the popup

elop.raw$popupw <- paste(sep = "",  "<b>",elop.raw$Full_Name,"</b><br/>",
                         "Denomination: ",elop.raw$Denomination_for_Tableau, "<br/>",
                         "Age: ",elop.raw$Age, "<br/>",
                         "Year Eloped: ",elop.raw$Year,"<br/>",
                         "Origin: ",elop.raw$Location_Origin,"<br/>",
                         "Accusation: ",elop.raw$Accusations,"<br/>",
                         "Age of Female: ",elop.raw$Female_Age, "<br/>",
                         "Found: ",elop.raw$Location_Found,"<br/>",
                         "Year Found: ",elop.raw$Year_Found,"<br/>"
) #A bit of HTML To make the popups on the lines
elop.raw$bearing[(!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)] <- bearingRhumb(elop.raw[((!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)),c("Longtitude_Origin","Latitude_Origin")],elop.raw[((!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)),c("Longtitude_Found","Latitude_Found")])
elop.raw$bearClass[(elop.raw$bearing < 45 ) | (elop.raw$bearing >= 315)] <- "North"
elop.raw$bearClass[(elop.raw$bearing < 135) & (elop.raw$bearing >= 45)] <- "East"
elop.raw$bearClass[(elop.raw$bearing < 225) & (elop.raw$bearing >= 135)] <- "South"
elop.raw$bearClass[(elop.raw$bearing < 315) & (elop.raw$bearing >= 225)] <- "West"
table(elop.raw$bearClass)
elop.raw$popupw <- paste (sep = "", elop.raw$popupw,"bearing: ",elop.raw$bearClass,"<br/>")


#New method for creating lines
elop.comp <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
row.names(elop.comp) <- NULL

a <- (gcIntermediate(elop.comp[,c("Longtitude_Origin","Latitude_Origin")], elop.comp[,c("Longtitude_Found","Latitude_Found")],addStartEnd = T, breakAtDateLine = T, n=150, sp = T) )
complete.lines <- SpatialLinesDataFrame(a,elop.comp)
#Fixing a dateline issue for that one that ran to new zealand: TODO: Still not perfect
negs <- as.matrix(coordinates(complete.lines[94,])[[1]][[2]])
negs[,1] <- (negs[,1])-360
complete.lines@lines[[94]]@Lines[[2]]@coords[] <- negs

#Making the arrows for the lines

markers.df <- elop.comp[which(elop.comp$Location_Origin != elop.comp$Location_Found),]

markers.df$midlong <- apply(markers.df[,c("Longtitude_Origin","Longtitude_Found")], 1, mean) 
markers.df$midlat <- apply(markers.df[,c("Latitude_Origin","Latitude_Found")], 1, mean) 
a <- (gcIntermediate(markers.df[,c("Longtitude_Origin","Latitude_Origin")], markers.df[,c("Longtitude_Found","Latitude_Found")], n=1) )
a <- do.call(rbind.data.frame, a)
markers.df$midlong <- a$lon
markers.df$midlat <- a$lat


# markers.df$bearing <- bearingRhumb(markers.df[,c("Longtitude_Origin","Latitude_Origin")],markers.df[,c("Longtitude_Found","Latitude_Found")])
#Now can calculate the direction of travel



arrow.length <- 60000
arrow.angle <- 30

a <- data.frame(destPoint(markers.df[,c( "midlong","midlat")], d = arrow.length, b = markers.df$bearing + arrow.angle -180 ))
markers.df$arrow1Lat <- a$lat
markers.df$arrow1Lon <- a$lon

a <- data.frame(destPoint(markers.df[,c( "midlong","midlat")], d = arrow.length, b = markers.df$bearing - arrow.angle-180 ))
markers.df$arrow2Lat <- a$lat
markers.df$arrow2Lon <- a$lon


row.names(markers.df) <- NULL
polys <- list()
for (i in 1:nrow(markers.df)) { 
  polys[[i]] = Polygons(list(Polygon(rbind(c(markers.df$midlong[i],markers.df$midlat[i]), c(markers.df$arrow2Lon[i], markers.df$arrow2Lat[i]), c(markers.df$arrow1Lon[i], markers.df$arrow1Lat[i]),c(markers.df$midlong[i],markers.df$midlat[i])))), as.character(i))
}
poly.arrows <- SpatialPolygonsDataFrame(SpatialPolygons(polys),markers.df)
proj4string(poly.arrows) <- CRS(latlong)


#Creating a variable from the bearing

#Mapping Section

#converting to point data frames for mapping

#Randomizing identical points

orig.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Origin)),]
a<- data.frame(table(orig.spdf$Location_Origin))
a$Location_Origin <- as.character(a$Var1)
a$Var1 <- NULL
orig.spdf <- merge(orig.spdf,  a, by="Location_Origin",all=T)
orig.spdf$Latitude_Origin <-ifelse((orig.spdf$Freq > 1),  orig.spdf$Latitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Latitude_Origin)
orig.spdf$Longtitude_Origin <-ifelse((orig.spdf$Freq > 1), orig.spdf$Longtitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Longtitude_Origin)

coordinates(orig.spdf)=~Longtitude_Origin+Latitude_Origin
proj4string(orig.spdf) <- CRS(latlong)

found.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
coordinates(found.spdf)=~Longtitude_Found+Latitude_Found
proj4string(found.spdf) <- CRS(latlong)

same.spdf <- elop.comp[which(elop.comp$Location_Origin == elop.comp$Location_Found),]
coordinates(same.spdf)=~Longtitude_Found+Latitude_Found
proj4string(same.spdf) <- CRS(latlong)


#Building Shiny Interface
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  sliderInput("range", "Range:",
              min = 1870, max = 1915,
              value = c(1870,1915), sep = ""),
 # selectInput("decade", "Decade:",
               #c("all", "1870s","1880s","1890s","1900s","1910-1914")),
  selectizeInput("denomination", "Denomination:",
                 choices = c("All", sort(unique(elop.raw$Denomination_for_Tableau)))),
  checkboxInput("CompCheck","Complete cases", value = FALSE, width = NULL),
 checkboxGroupInput("direction", label = h3("Directions"), 
                    choices = c("Same","North", "East", "West", "South"),
                    selected = c("Same", "North", "East", "West", "South"))
  # selectizeInput("direction","Direction: ", choices = c("None"))
  
)#end fluidpage

server <- function(input, output, session) {
  
  points.orig <- eventReactive(c(input$range, input$denomination, input$CompCheck), {
    #TODO: Should be able to make this pull and generate from one data frame all at the same time
    working.spdf <- orig.spdf
    if(input$CompCheck){
      working.spdf <- orig.spdf[which(!is.na(orig.spdf$Latitude_Found)),]
    }
    if (input$denomination == "All"){
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2]),])
    }else{
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2] & working.spdf$Denomination_for_Tableau == input$denomination),])  
    }
    
    
  }, ignoreNULL = FALSE)#end points.orig
  
  points.found <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "All"){
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2]),]
    }else{
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2] & found.spdf$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)#end points.found
  
  points.connections <- eventReactive(c(input$range, input$denomination, input$direction), {
    # if (input$denomination == "All"){
    #   same.spdf[which(same.spdf$Year >= input$range[1] & same.spdf$Year <= input$range[2]),]
    # }else{
    #   same.spdf[which(same.spdf$Year >= input$range[1] & same.spdf$Year <= input$range[2] & same.spdf$Denomination_for_Tableau == input$denomination),]  
    # }
    if (!"Same" %in% input$direction){
      return(same.spdf [which(same.spdf$bearClass == "Same"),])
    }
    if (input$denomination == "All"){
      temp.same <- same.spdf
    }else{
      temp.same <- same.spdf[which(same.spdf$Denomination_for_Tableau == input$denomination),]
    }
    #filter years
    temp.same <- temp.same[which(temp.same$Year >= input$range[1] & temp.same$Year <= input$range[2]),]
    # #filter bearing
    # temp.same <- temp.same[which(temp.same$bearClass %in% input$direction),]
    # return(complete.lines[
    return(temp.same)
    
  }, ignoreNULL = FALSE)#end points.found
  
  lines.connections <- eventReactive(c(input$range, input$denomination, input$direction), {
    if (input$denomination == "All"){
      temp.lines <- complete.lines
    }else{
      temp.lines <- complete.lines[which(complete.lines$Denomination_for_Tableau == input$denomination),]
          }
    #filter years
    temp.lines <- temp.lines[which(temp.lines$Year >= input$range[1] & temp.lines$Year <= input$range[2]),]
    #filter bearing
    temp.lines <- temp.lines[which(temp.lines$bearClass %in% input$direction),]
    # return(complete.lines[
    return(temp.lines)
    
  }, ignoreNULL = FALSE)#end lines.connections
  
  arrowheads.connections <- eventReactive(c(input$range, input$denomination, input$direction), {

    if (input$denomination == "All"){
      temp.arrows <- poly.arrows
    }else{
      temp.arrows <- poly.arrows[which(poly.arrows$Denomination_for_Tableau == input$denomination),]
    }
    #filter years
    temp.arrows <- temp.arrows[which(temp.arrows$Year >= input$range[1] & temp.arrows$Year <= input$range[2]),]
    #filter bearing
    temp.arrows <- temp.arrows[which(temp.arrows$bearClass %in% input$direction),]
    # return(complete.lines[
    return(temp.arrows)
    
  }, ignoreNULL = FALSE)#end lines.connections
  
  output$value <- renderPrint({ input$direction })
  
 
    
    
    
  output$mymap <- renderLeaflet({
    leaflet() %>%
      fitBounds(-129,24.2,-65.58,50.54)%>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addLayersControl(
        overlayGroups = c("Origin", "Found","Connections"),
        options = layersControlOptions(collapsed = FALSE)
      )       

  })
  observe({
    leafletProxy("mymap") %>%
      clearMarkers() %>% 
      clearMarkerClusters() %>%
      clearShapes()%>%
      addCircleMarkers(data = points.orig(), popup = ~popupw, group = "Origin",color = "brown",radius=3)%>%
      addCircleMarkers(data = points.found(), popup = ~popupw, group = "Found",color = "green",radius=3)%>%#,clusterOptions = markerClusterOptions())
      addCircleMarkers(data = points.connections(), popup = ~popupw, group = "Connections",color = "navy",radius=3, opacity = 1) %>%
      addPolylines(data = lines.connections(), popup = ~popupw, group = "Connections", color = "navy", opacity = 1)  %>%
      addPolygons(data=arrowheads.connections(), group = "Connections",  fillOpacity = .6, opacity = .6, popup = ~popupw, color = "navy", fillColor = "navy", stroke = F )

    
  })
}

shinyApp(ui, server)
