rm(list=ls(all=TRUE)) # clear memory

#Mat's new Todo
    #Impliment the random mover (working for the origins) earlier on so it generates the lines off of the moved points
    #Move the lost /found same city points over
#move the arrows over
#clasify direction
#put in the direction as a classification


packages<- c("rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
library(rgdal)
library(leaflet)
library(shiny)
library(ggmap)
library(leaflet.minicharts)


# setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements")
setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")

latlong <- "+init=epsg:4326"


#Loading the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)

#Generate new locations for duplicate places 

# elop.raw <- elop.raw[,c("Location_Origin","Location_Found", "Longtitude_Origin","Latitude_Origin" ,"Longtitude_Found" ,"Latitude_Found"   )]

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



elop.comp <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
row.names(elop.comp) <- NULL
lines <- list()
for (i in 1:nrow(elop.comp)) { 
  lines[[i]] <- Lines(list(Line(rbind(c(elop.comp$Longtitude_Origin[i],elop.comp$Latitude_Origin[i]), c(elop.comp$Longtitude_Found[i], elop.comp$Latitude_Found[i]) ))), as.character(i)) 
  #print(i)
}
complete.lines <- SpatialLinesDataFrame(SpatialLines(lines),elop.comp)


elop.comp$arrowLat <- elop.comp$Latitude_Origin + (elop.comp$Latitude_Origin - elop.comp$Latitude_Found)/2
elop.comp$arrowLon <- elop.comp$Longtitude_Origin + (elop.comp$Longtitude_Origin - elop.comp$Longtitude_Found)/2

elop.comp$arrowLon <- ifelse(elop.comp$arrowLon < -180, elop.comp$arrowLon + 360,elop.comp$arrowLon)
library(geosphere)
elop.comp$mid <- midPoint(elop.comp[,c("Longtitude_Origin", "Latitude_Origin")],elop.comp[,c("Longtitude_Found", "Latitude_Found")])


markers.spdf <- elop.comp
coordinates(markers.spdf)=midPoint(elop.comp[,c("Longtitude_Origin", "Latitude_Origin")],elop.comp[,c("Longtitude_Found", "Latitude_Found")],f=0)
proj4string(markers.spdf) <- CRS(latlong)



# var diffLat = points[p+1]["lat"] - points[p]["lat"]
# var diffLng = points[p+1]["lng"] - points[p]["lng"]
# var angle = 360 - (Math.atan2(diffLat, diffLng)*57.295779513082)
# 1
# 2
# 3
# var diffLat = points[p+1]["lat"] - points[p]["lat"]
# var diffLng = points[p+1]["lng"] - points[p]["lng"]
# var angle = 360 - (Math.atan2(diffLat, diffLng)*57.295779513082)


#Mapping Section

#converting to point data frames for mapping
orig.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Origin)),]
a<- data.frame(table(orig.spdf$Location_Origin))
a$Location_Origin <- as.character(a$Var1)
a$Var1 <- NULL

# orig.spdf$Latitude_Origin <- orig.spdf$Latitude_Origin - (runif(nrow(orig.spdf))-.5)/20
# orig.spdf$Longtitude_Origin <- orig.spdf$Longtitude_Origin - (runif(nrow(orig.spdf))-.5)/20
orig.spdf <- merge(orig.spdf,  a, by="Location_Origin",all=T)



orig.spdf$Latitude_Origin <-ifelse((orig.spdf$Freq > 1),  orig.spdf$Latitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Latitude_Origin)
orig.spdf$Longtitude_Origin <-ifelse((orig.spdf$Freq > 1), orig.spdf$Longtitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Longtitude_Origin)

coordinates(orig.spdf)=~Longtitude_Origin+Latitude_Origin
proj4string(orig.spdf) <- CRS(latlong)



found.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
coordinates(found.spdf)=~Longtitude_Found+Latitude_Found
proj4string(found.spdf) <- CRS(latlong)

elop.map <-elop.comp[which(elop.comp$Location_Origin != elop.comp$Location_Found),]
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
  checkboxInput("CompCheck","Complete cases", value = FALSE, width = NULL)
)

server <- function(input, output, session) {
  
  points.orig <- eventReactive(c(input$range, input$denomination, input$CompCheck), {
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
  
  points.connections <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "All"){
      same.spdf[which(same.spdf$Year >= input$range[1] & same.spdf$Year <= input$range[2]),]
    }else{
      same.spdf[which(same.spdf$Year >= input$range[1] & same.spdf$Year <= input$range[2] & same.spdf$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)#end points.found
  
  lines.connections <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "All"){
      complete.lines[which(complete.lines$Year >= input$range[1] & complete.lines$Year <= input$range[2]),]
      
    }else{
      complete.lines[which(complete.lines$Year >= input$range[1] & complete.lines$Year <= input$range[2] & complete.lines$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)#end lines.connections
  
  
  
  
 
    
    
    
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
      clearFlows()%>%
      addCircleMarkers(data = points.orig(), popup = ~popupw, group = "Origin",color = "brown",radius=3)%>%
      addCircleMarkers(data = points.found(), popup = ~popupw, group = "Found",color = "green",radius=3)%>%#,clusterOptions = markerClusterOptions())
      addCircleMarkers(data = points.connections(), popup = ~popupw, group = "Connections",color = "navy",radius=3) %>%
      addPolylines(data = lines.connections(), popup = ~popupw, group = "Connections", color = "navy") 
      # addMarkers(data = markers.spdf)
      # plotArrows(lines.connections(), fraction=0.9, length=0.15, first='', add=FALSE)
      # addFlows(
      #   lines.connections()@data$Longtitude_Origin, lines.connections()@data$Latitude_Origin, lines.connections()@data$Longtitude_Found, lines.connections()@data$Latitude_Found,
      #   # flow = .01,
      #   maxThickness = 2,
      #   color = "navy",
      #   popup = popupArgs(noPopup = T),
      #   group = "Connections"
      # ) # end add flows
    
  })
}

shinyApp(ui, server)
